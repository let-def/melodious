(**
 * Melodious, Ocsigen-powered media player
 *
 * Copyright (C) 2012 Frédéric Bour <frederic.bour@lakaban.net>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 **)

open Utils
open Model

(* Hardcoded limit complexity of search queries *)
let max_keywords = 12

module Sig =
struct
  module type DB =
  sig
    exception E of string (* location *) * string (* description *)

    type db_user

    val close_db : unit -> unit

    (* Create user if it doesn't exists *)
    val new_user   : string -> db_user
    (* Throw Not_found if user doesn't exists *)
    val log_user   : string -> db_user

    (* Simple key/value store for users *)
    val user_store : db_user -> string -> string -> unit
    val user_get   : db_user -> string -> string

    (* Manage playlists *)
    val new_list    : db_user -> string -> Playlist.id
    val get_list_id : db_user -> string -> Playlist.id option
    val get_list    : db_user -> Playlist.id -> Playlist.t
    val set_list    : db_user -> Playlist.id -> Resource.uid list -> unit
    val append_list : db_user -> Playlist.id -> Resource.uid list -> unit
    val rename_list : db_user -> Playlist.id -> string -> unit
    val delete_list : db_user -> Playlist.id -> unit
    val get_lists   : db_user -> Playlist.folder

    (* Manage songs *)
    val update_songs : Resource.t list -> unit
    val insert_songs : Repository.name -> Repository.rid list -> Resource.uid list
    val get_song     : Resource.uid -> Resource.t
    val resolve_song : Resource.uid -> (Repository.name * Repository.rid) option

    (* Query songs *)
    val search    : Tag.t list -> Resource.t list
    val aggregate : Tag.key -> Tag.t list -> Tag.value list

    (* Execute queries in a transaction, for bulk insertion *)
    val with_trans : ('a -> 'b) -> ('a -> 'b)

    (* Useful when database got modified outside Melodious *)
    val reindex : unit -> unit
  end
end
type melo_db = (module Sig.DB)

let sqlite_connect db =
  let module SqliteDB =
struct
  module S = struct
    include Sqlite3
    include Sql_utils
  end
  module D = S.Data
  module B = S.Banana
  exception E = S.E

  type table_kind = Normal | Fts4
  let last_row db = Int64.to_string (S.last_insert_rowid db)

  let query = S.fold_query
  let query' stmt args fold seed =
    query stmt args (B.custom fold seed)


  (** Database initialisation **)

  (* Function to drop text content from FTS table *)
  let compress_null_text datum =
    match datum with
    | D.TEXT s -> D.NONE
    | d -> d
  let _ = S.create_fun1 db "null_text" compress_null_text

  let table_exists =
    let stmt = S.prepare db
      "SELECT name FROM sqlite_master WHERE type='table' AND name=?"
    in fun name ->
      query stmt [D.TEXT name] B.exists

  (* Hack for nested transaction *)
  let locked_trans_ = ref false

  let stmt_begin_, stmt_commit_ =
    map2 (S.prepare db) ("BEGIN", "COMMIT")

  let with_trans f x =
    if !locked_trans_ then f x else
    let enter () =
      query stmt_begin_ [] B.void;
      locked_trans_ := true
    and cleanup () =
      query stmt_commit_ [] B.void;
      locked_trans_ := false
    in
    try
      enter ();
      let result = f x in
        cleanup ();
      result
    with
    | e -> cleanup ();
           raise e

  (* FIXME Introduce foreign key constraints *)
  let create_tables () =
    let id_col = "id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL" in
    let foreign name ?(typ="INTEGER NOT NULL") ?(key="id") table =
      Printf.sprintf "%s %s REFERENCES %s(%s) ON DELETE CASCADE" name typ table key
    in
    let tables =
    [ Normal, "users",
      [ id_col
      ; "login    TEXT"
      ]
    ; Normal, "users_store",
      [ foreign "user" "users"
      ; "key      TEXT"
      ; "value    TEXT"
      ]
    ; Normal, "playlists",
      [ id_col
      ; "name     TEXT"
      ; foreign "owner" "users"
      ]
    ; Normal, "playlists_content",
      [ foreign "playlist" "playlists"
      ; foreign "song" "songs"
      ; "pos      INTEGER"
      ]
    ; Normal, "songs",
      [ id_col
      ; "key      TEXT NOT NULL"
      ; "repo     TEXT"
      ]
    ; Normal, "tags",
      [ foreign "song" "songs"
      ; ("key     TEXT")
      ; ("value   TEXT")
      ]
    ; Fts4, "tags_search",
      [ ("content TEXT")
      ; ("tokenize=porter")
      (** 
       * Tags are duplicated in tags_search, though these are never read.
       * Compress functions may be used to get around this, but SQLite
       * documentation specify that they should form a bijection.
       * Not sure that this is really safe.
       * Uncomment lines below to enable compressed database.
       *)
      (*; ("", "compress=null_text")
      ; ("", "uncompress=null_text") *)
      ]
    ]
    in
    let make_table (decl, name, layout) =
      let sql_layout = String.concat ", " layout
      in
      let stmt' = match decl with
        | Normal ->
            Some (Printf.sprintf "CREATE TABLE IF NOT EXISTS %s (%s);" name sql_layout)
        | Fts4 ->
            if (table_exists name)
            then None
            else Some (Printf.sprintf "CREATE VIRTUAL TABLE %s USING fts4 (%s);"
                         name sql_layout)
      in
      match stmt' with
      | None -> ()
      | Some stmt ->
        match S.exec db stmt with
        |  S.Rc.OK -> ()
        |  x -> raise (E ("make_table", S.Rc.to_string x ^ " ~ " ^ stmt))
    in
    List.iter make_table tables;
    let init_stmts =
      [ "CREATE UNIQUE INDEX IF NOT EXISTS 'songs_repo_unique' \
           ON 'songs' ('repo' ASC, 'key' ASC);"
      ; "CREATE INDEX IF NOT EXISTS 'tags_song' \
           ON 'tags' ('song' ASC);"
      ; "CREATE INDEX IF NOT EXISTS 'tags_key'  \
           ON 'tags' ('key' ASC);"
      ; "CREATE UNIQUE INDEX IF NOT EXISTS 'users_store_unique'  \
           ON 'users_store' ('user' ASC, 'key' ASC);"
      ; "PRAGMA SYNCHRONOUS = NORMAL;"
      ]
    in
    flip List.iter init_stmts (fun stmt ->
      match S.exec db stmt with
      |  S.Rc.OK -> ()
      |  x -> raise (E ("init_stmts", S.Rc.to_string x ^ " ~ " ^ stmt))
    )
  let _ = create_tables ()



  (** Melodious DB interface **)

  let close_db () = ignore @$ S.db_close db

  type db_user = D.t


  (* Throw Not_found if user doesn't exists *)
  let log_user =
    let stmt = S.prepare db "SELECT id FROM users WHERE login = ?" in
    fun username ->
    match query stmt [D.TEXT username] B.raw1 with
    | Some id -> id
    | None -> raise Not_found

  (* Create user if it doesn't exists *)
  let new_user =
    let stmt = S.prepare db "INSERT OR IGNORE INTO users (login) VALUES (?)" in
    fun username ->
    query stmt [D.TEXT username] B.void;
    log_user username

  (* Simple key/value store for users *)
  let user_store =
    let stmt = S.prepare db "INSERT OR REPLACE INTO users_store (user,key,value) VALUES (?,?,?)" in
    fun db_user key value ->
    query stmt [db_user; D.TEXT key; D.TEXT value] B.void

  let user_get =
    let stmt = S.prepare db "SELECT value FROM users_store WHERE user = ? AND key = ?" in
    fun db_user key ->
    match query stmt [db_user; D.TEXT key] B.string1 with
    | Some value -> value
    | None -> raise Not_found

  let get_list_content =
    let stmt_song, stmt_tag = map2 (S.prepare db)
     ("SELECT songs.id FROM songs, playlists_content \
      WHERE songs.id = playlists_content.song AND playlists_content.playlist = ? \
      ORDER BY playlists_content.pos",
     "SELECT tags.song, tags.key, tags.value \
      FROM tags WHERE tags.song IN \
      (SELECT song FROM playlists_content WHERE playlist = ?)")
    in
    fun db_user plid ->
    let htags = Hashtbl.create 16 in
    let songs =
    query' stmt_song [S.id_to_data plid]
      (fun songs r ->
        match List.map D.to_string r with
        | id :: _ ->
            Hashtbl.replace htags id ([]);
            id :: songs
        | _ -> songs
      ) []
    in
    query' stmt_tag [S.id_to_data plid]
      (fun () r ->
        match List.map D.to_string r with
        | song :: k :: v :: _ ->
          begin try 
            let tags = Hashtbl.find htags song in
              Hashtbl.replace htags song ((k,v) :: tags)
          with
          | Not_found -> assert false
          end
        | _ -> ()
      ) ();
    List.rev_map (fun song -> song, Hashtbl.find htags song) songs


  let name_of_list =
    let stmt = S.prepare db
      "SELECT name FROM playlists WHERE id = ?"
    in
    fun user id ->
    query stmt [S.id_to_data id] B.string1
      


  let get_list user list_id =
    match name_of_list user list_id with
    | Some name -> (name, list_id), get_list_content user list_id
    | None -> raise Not_found
 

  let get_song =
    let stmt_tag = S.prepare db
      "SELECT key, value FROM tags WHERE song = ?"
    in
    fun songid ->
    songid,
    query stmt_tag [S.id_to_data songid] B.strings2

  let resolve_song =
    let stmt = S.prepare db
       "SELECT songs.repo, songs.key FROM songs WHERE songs.id = ?"
    in
    fun song_id ->
    query stmt [S.id_to_data song_id] B.string2


  let get_lists =
    let stmt = S.prepare db "SELECT id, name FROM playlists WHERE owner = ?" in 
    fun db_user ->
    query stmt [db_user] B.strings2

  let get_list_id =
    let stmt = S.prepare db "SELECT id FROM playlists WHERE owner = ? AND name = ?" in
    fun db_user name ->
    query stmt [db_user; D.TEXT name] B.string1

  let new_list =
    let stmt = S.prepare db "INSERT INTO playlists (owner,name) VALUES (?,?)" in
    fun db_user name ->
    query stmt [db_user; D.TEXT name] B.void;
    last_row db


  let delete_list =
    let stmt = S.prepare db "DELETE FROM playlists WHERE id = ? AND owner = ?" in
    fun db_user list_id ->
    query stmt [S.id_to_data list_id; db_user] B.void


  let rename_list =
    let stmt = S.prepare db "UPDATE playlists SET name = ? WHERE id = ? AND owner = ?" in 
    fun db_user list_id name ->
    query stmt [D.TEXT name; S.id_to_data list_id; db_user] B.void


  let check_owner = 
    let stmt = S.prepare db "SELECT id FROM playlists WHERE owner = ? AND id = ?" in
    fun db_user list_id ->
      query stmt [db_user; S.id_to_data list_id] B.exists

  let append_list =
    let stmt_ins, stmt_pos = map2 (S.prepare db)
      ( "INSERT INTO playlists_content (playlist,song,pos) VALUES (?,?,?)"
      , "SELECT COALESCE (MAX(pos)+1, 1) FROM playlists_content WHERE playlist = ?"
      ) in
    fun db_user list_id content ->
    let list_id' = S.id_to_data list_id in
    let pos = query' stmt_pos [list_id']
                (fun m l ->
                  match l with
                  | D.INT x :: _ -> Int64.to_int x
                  | _ -> m
                ) 0
    in
    let insert_song i song_id = 
      let song_id' = S.id_to_data song_id in
      let rank     = D.INT (Int64.of_int (pos + i))  in
      query stmt_ins [list_id'; song_id'; rank] B.void
    in
    assert (check_owner db_user list_id);
    with_trans (list_iteri insert_song) content
          

  let set_list =
    let stmt = S.prepare db "DELETE FROM playlists_content WHERE playlist = ?" in
    fun db_user list_id content ->
    assert (check_owner db_user list_id);
    query stmt [S.id_to_data list_id] B.void;
    append_list db_user list_id content


  let update_songs =
    (* This strange _ _ _ ... separate different tags to prevent 
     * NEAR operator matching against a wong key *)
    let format_tag    (k,v) = "%%" ^ k ^ " " ^ v in
    let indexable_tag (k,v) = not (String.contains k '_') in
    let prepare = String.concat " _ _ _ _ _ _ _ _ _ _ _ "
                  @% List.map    format_tag
                  @% List.filter indexable_tag
    in
    let stmt_delt, stmt_dels, stmt_inst, stmt_inss = map4 (S.prepare db)
      ( "DELETE FROM tags WHERE song = ?"
      , "DELETE FROM tags_search WHERE docid = ?"
      , "INSERT INTO tags (song,key,value) VALUES (?,?,?)"
      , "INSERT INTO tags_search (docid,content) VALUES (?,?)"
      )
    in
    let update_resource res = 
      let songid = S.id_to_data (Resource.uid res) in
      query stmt_delt [songid] B.void;
      query stmt_dels [songid] B.void;
      flip List.iter (Resource.tags res)
        (fun (k,v) -> query stmt_inst [songid; D.TEXT k; D.TEXT v] B.void);
      query stmt_inss [songid; D.TEXT (prepare (Resource.tags res))] B.void
    in
    with_trans (List.iter update_resource)

  let reindex =
    let stmt = S.prepare db "SELECT id FROM songs" in
    with_trans (query' stmt [] (fun () l ->
      match l with
      | x :: [] -> 
        let song = D.to_string x in
        update_songs [get_song song]
      | _ -> ()
      ))


  let insert_songs =
    let stmt1, stmt2 = map2 (S.prepare db)
      ( "INSERT OR IGNORE INTO songs (repo,key) VALUES (?,?)"
      , "SELECT id FROM songs WHERE repo = ? AND key = ?"
      ) in
    fun repo content ->
    let repo' = D.TEXT repo in
  	let insert_song rid =
      let args = [repo'; D.TEXT rid] in
      query stmt1 args B.void;
      match query stmt2 args B.string1 with
      | Some id -> id
      | None -> raise (E ("playlist_insert_songs:no_id", "No result after insertion"))
  	in
    with_trans (List.map insert_song) content

  let search, aggregate =
    let tags_search, tags_aggreg =
      "SELECT docid FROM tags_search WHERE content MATCH ?",
      "SELECT DISTINCT tags.value FROM tags WHERE tags.key = ?"
    in
    let stmt_search, stmt_aggreg, stmt_aggreg_search = map3 (S.prepare db)
      (tags_search ^ " LIMIT 100", tags_aggreg ^ " LIMIT 100",
       tags_aggreg ^ " AND tags.song IN (" ^ tags_search ^ ") LIMIT 100")
    in
    let preprocess = (list_take max_keywords) @% list_uniq @% List.fast_sort
      (fun ((k1,v1) as t1) ((k2,v2) as t2) ->
        match compare (String.length v1) (String.length v2) with
        | 0 ->
        (match compare (String.length k1) (String.length k2) with
         | 0 -> - (compare t1 t2)
         | n -> - n
        )
        | n -> - n)
    in
    (* FIXME : filter/escape term *)
    let quote str = "\"" ^ str ^ "\"" in
    let prepare tags =
      let neg_tags, pos_tags = flip List.partition tags
        (fun (k,v) -> String.length k > 0 && k.[0] = '-')
      in
      let prepare_query (k,v) =
        if k = ""
          then quote v
          else quote ("%%" ^ k) ^ " NEAR " ^ quote v
      and drop_neg (k,v) =
        String.sub k 1 (String.length k - 1), v
      in
      let pos_query, neg_query =
        List.map prepare_query pos_tags,
        List.map (prepare_query @% drop_neg) neg_tags
      in
      (* Conjunction of positive terms *)
      let pos_request = String.concat " AND " pos_query in
      (* Substract negative ones *)
      let request = String.concat " NOT " (pos_request :: neg_query)
      in
      (* prerr_endline "Searching:"; *)
      (* prerr_endline request; *)
      request
    in
    (fun tags' ->
    let song_ids =
      match preprocess tags' with
      | [] -> []
      | tags ->
         let search_request = prepare tags in
         query stmt_search [D.TEXT search_request] B.strings1
    in
    List.map get_song song_ids),
    (fun key tags' ->
      match preprocess tags' with
      | [] -> query stmt_aggreg [D.TEXT key] B.strings1
      | tags ->
         let search_request = prepare tags in
         query stmt_aggreg_search [D.TEXT key; D.TEXT search_request] B.strings1
    )

end
  in (module SqliteDB : Sig.DB)

let sqlite_db path =
  sqlite_connect (Sqlite3.db_open path)

