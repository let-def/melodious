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
open HTML5.M
open Lwt
module Eref = Eliom_references

{shared{
  open Model
  module P = Eliom_parameters
}}

{client{
  open Utils_client
}}

(* Database (metadata & repository) *)

let repo_name = "main" (* Hardcoded ATM *)

let data_repo, (data_db_path, data_db) =
  let config = (Eliom_config.get_config ()) in
  let repo_msg, db_msg =
    "Set-up repository path: <repository path=\"/var/db/melodious\" />",
    "Set-up database path: <database path=\"db.sqlite\" />"
  in
  let conf_error strs =
    List.iter prerr_endline strs;
    failwith (String.concat ". " strs)
  in
  match 
    let open Config in
      maybe (elem config "repository" attrib "path"),
      maybe (elem config "database" attrib "path")
  with
  | Some repo_path, Some db_path ->
      (Repository.local_repo repo_path repo_name),
      (db_path, Database.sqlite_db db_path)
  | None, None -> conf_error [repo_msg;db_msg]
  | None, _    -> conf_error [repo_msg]
  | _   , None -> conf_error [db_msg]

let _ = prerr_endline ("Melodious starting, using database: " ^ data_db_path)

module DB = (val data_db : Database.Sig.DB)
module Repo = (val data_repo : Repository.Sig)


(* module Authentication = Auth.HeaderHandler (DB) *)
module Authentication = Auth.GuestHandler

let user_ref = Eref.eref ~scope:Eliom_common.session None

let set_user auth_val = 
  match auth_val with
  | Some user ->
    let logged = DB.new_user user in
    lwt () = Eref.set user_ref (Some logged) in
    Lwt.return (Some logged)
  | None -> 
    lwt () = Eref.set user_ref None in
    Lwt.return None

let get_user () =
  match_lwt Eref.get user_ref with
  | Some user -> Lwt.return user
  | None ->
  lwt auth_val = Authentication.get_auth () in
  match_lwt set_user auth_val with
  | Some user -> Lwt.return user
  | None -> raise Auth.Unauthenticated

let with_user m = 
  lwt user = get_user () in
  m user


(* Playlists services *)

let svc_reindex = 
  Eliom_output.Unit.register_service
   ~path:["search"; "reindex"]
   ~get_params:P.unit
   (fun () () -> Lwt.return (DB.reindex ()))

let svc_resolver =
  Eliom_output.Caml.register_service
   ~path:["song";"resolve"]
   ~get_params:(P.string "id")
   (fun songid () ->
      match DB.resolve_song songid with
      | None -> failwith "Invalid song"
      | Some (repo,key) -> (* FIXME: Ignoring repo *)
          Lwt.return [Repo.get key]
   )

let svc_playlist_list =
  Eliom_output.Caml.register_service
    ~path:["playlist";"list"]
    ~get_params:P.unit
    (fun () () -> with_user (Lwt.return @% DB.get_lists))

let svc_playlist_get =
  Eliom_output.Caml.register_service
    ~path:["playlist";"get"]
    ~get_params:(P.string "id")
    (fun id () -> lwt user = get_user () in Lwt.return (DB.get_list user id))

let svc_playlist_search =
  let re = Str.regexp "\\(-\\)?\\(\\([a-zA-Z_]*\\):\\)?\\(\"[^\"]+\"\\|[^\" \t]+\\)" in
  let aggreg_suffix s =
    let l = String.length s in
    if l > 0 && s.[l - 1] = '?'
      then Some (String.sub s 0 (l - 1))
      else None
  in
  let split s =
    let agg = ref [] in
    let rec find idx =
      (try
        let _ = Str.search_forward re s idx in
        let positive =
          try Str.matched_group 1 s
          with Not_found -> ""
        in
        let k = try Str.matched_group 3 s with Not_found -> "" in
        let v = try Str.matched_group 4 s with Not_found -> "" in
        (* prerr_endline ("search : " ^ k ^ " ~ " ^ v); *)
        match k, v, aggreg_suffix v with
        | "", "", _ -> find (Str.match_end ())
        | "", _, (Some a) -> agg := a :: !agg; find (Str.match_end ())
        | k, v, _ -> (positive ^ k, v) :: find (Str.match_end ()) 
      with Not_found -> [])
    in
    let results = find 0 in
    match List.rev !agg with
    | key :: keys -> Right ((key, keys), results)
    | [] -> Left results
  in
  Eliom_output.Caml.register_service
    ~path:["search";"query"]
    ~get_params:(P.string "query")
    (fun query () -> Lwt.return @$
      match split query with
      | Left t      -> Left
        (Resource.sort_by [Tag.album; Tag.number] @$ DB.search t)
      | Right ((k,ks),t) -> Right
        ( DB.aggregate k t,
        { Aggregation.base = t
        ; Aggregation.bound = []
        ; Aggregation.unbound = k::ks 
        }
        )
    )

let svc_playlist_aggregate =
let svc_playlist_aggregate' =
  Eliom_output.Caml.register_service
    ~path:["search";"aggregate"]
    ~get_params:P.unit
    (fun () () -> Lwt.return ())
in
  Eliom_output.Caml.register_post_service
    ~fallback:svc_playlist_aggregate'
    ~post_params:(P.caml "query" Json.t<Tag.t list * Tag.key option>)
    (fun () (tags,key) -> Lwt.return @$
      match key with
      | Some k -> Right
        (DB.aggregate k tags)
      | None   -> Left
        (Resource.sort_by [Tag.album; Tag.number] @$ DB.search tags)
    )

let svc_playlist_delete =
  Eliom_output.Caml.register_service
    ~path:["playlist";"delete"]
    ~get_params:(P.string "id")
    (fun id () -> lwt user = get_user () in DB.delete_list user id; Lwt.return ())

let svc_playlist_upload =
let svc_playlist_upload' =
  Eliom_output.Html5.register_service
    ~path:["upload";"song"]
    ~get_params:P.unit
    (fun () () -> Lwt.return
           (html
             (head (title (pcdata "Melodious - File upload")) [])
             (body [pcdata "Use POST method"])
           ))
in
  Eliom_output.Html5.register_post_service
    ~fallback:svc_playlist_upload'
    ~post_params:(P.prod (P.file "ogg") (P.any))
    (fun () (file,tags) ->
      let key = Repo.insert file in
      match DB.insert_songs repo_name [key]
      with
      | [] -> Lwt.return
           (html
             (head (title (pcdata "Melodious - File upload")) [])
             (body [pcdata "Failed"])
           )
      | uid :: _ ->
          let tags' = List.map (fun (k,v) -> String.lowercase k,v) tags in
          DB.update_songs [uid,tags'];
          Lwt.return
           (html
             (head (title (pcdata "Melodious - File upload")) [])
             (body [pcdata "Done"])
           )
    )

let svc_playlist_check =
  Eliom_output.Text.register_service
    ~path:["upload";"check"]
    ~get_params:(P.string "hash")
    (fun hash () ->
      let s = if Repo.has hash
        then "1"
        else "0"
      in
      Lwt.return (s, "text")
    )

let svc_playlist_save =
let svc_playlist_save' =
  Eliom_output.Caml.register_service
    ~path:["playlist";"save"]
    ~get_params:(P.string "name")
    (fun id () -> Lwt.return ())
in
  Eliom_output.Caml.register_post_service
    ~fallback:svc_playlist_save'
    ~post_params:(P.caml "content" Json.t<Resource.uid list>)
    (fun name plist -> 
      lwt user = get_user () in 
      let id = match DB.get_list_id user name with
               | Some id -> id
               | None -> DB.new_list user name
      in
      DB.set_list user id plist;
      Lwt.return ()
    )

(* Player *)

module Melodious =
  Eliom_output.Eliom_appl (struct
      let application_name = "melodious"
    end)

let player_elt = unique (audio
		       ~a:[a_controls (`Controls)]
		       [pcdata "Your browser does not support audio element"])

let tracks_elt       = unique (table (tr []) [])
let tracks_del_elt   = unique (a [pcdata "Delete tracks"])

let plists_elt       = unique (table (tr []) [])
let plist_name_elt   = unique (input ())
let plist_save_elt   = unique (button [pcdata "Save"])

let results_elt      = unique (table (tr []) [])
let searchentry_elt  = unique (input ())
let searchbutton_elt = unique (button [pcdata "Search"])
let addresults_elt   = unique (button [pcdata "Append"])
let aggregates_elt   = unique (div [])

let player_onload () =
{{

  (* Eliom syntax extension bug with modules *)
  let js_tracks        = Eliom_client.Html5.of_table %tracks_elt in
  let js_tracks_delete = Eliom_client.Html5.of_element %tracks_del_elt in

  let js_playlist_name = Eliom_client.Html5.of_input %plist_name_elt in
  let js_playlist_save = Eliom_client.Html5.of_button %plist_save_elt in
  let js_playlists     = Eliom_client.Html5.of_table %plists_elt in

  let js_search_input  = Eliom_client.Html5.of_input %searchentry_elt in
  let js_search_submit = Eliom_client.Html5.of_button %searchbutton_elt in
  let js_results_add   = Eliom_client.Html5.of_button %addresults_elt in
  let js_results       = Eliom_client.Html5.of_table %results_elt in
  let js_aggregates     = Eliom_client.Html5.of_element %aggregates_elt in

  let js_audio = Dom_media.Client.of_audio %player_elt in

  let svc_resolver = %svc_resolver in
  let svc_playlist_list = %svc_playlist_list in
  let svc_playlist_get = %svc_playlist_get in
  let svc_playlist_save = %svc_playlist_save in
  let svc_playlist_delete = %svc_playlist_delete in
  let svc_search_query = %svc_playlist_search in
  let svc_aggregate_query = %svc_playlist_aggregate in

  let module Doc : Melodious.Sig.Doc =
    struct
      let audio     = js_audio 
      let tracks    = js_tracks
      let tracks_delete = js_tracks_delete

      let playlist_name = js_playlist_name
      let playlist_save = js_playlist_save
      let playlists     = js_playlists

      let search_submit = js_search_submit
      let search_input  = js_search_input
      let results_add   = js_results_add
      let results       = js_results
      let aggregates     = js_aggregates
    end
  in
  let module Services : Melodious.Sig.Services =
    struct
      let song_resolver id = Eliom_client.call_caml_service
        ~service:svc_resolver id ()

      let playlist_list () = Eliom_client.call_caml_service
        ~service:svc_playlist_list () ()

      let playlist_get id = Eliom_client.call_caml_service
        ~service:svc_playlist_get id ()

      let playlist_save name content = Eliom_client.call_caml_service
        ~service:svc_playlist_save name content

      let playlist_delete id = Eliom_client.call_caml_service
        ~service:svc_playlist_delete id ()

      let search_query query = Eliom_client.call_caml_service
        ~service:svc_search_query query ()

      let aggregate_query query = 
        let tags = query.Aggregation.bound @ query.Aggregation.base in
        let svc_query =
          tags,
          match query with
          | { Aggregation.unbound = (k :: _) } -> Some k
          | _ -> None
        in
        lwt result = Eliom_client.call_caml_service
          ~service:svc_aggregate_query () svc_query
        in
        Lwt.return @$ match result with
        | Left x -> Left x 
        | Right x -> Right (x,query)
    end
  in
  let module Melo = Melodious.Make (Doc) (Services) in
    ()
}}

let div' str e =
  div ~a:[a_class [str]] e

let main_service = Eliom_services.service
  ~path:[""] ~get_params:P.unit ()

let _ =
  let open Eliom_output.Html5 in 
  let open Eliom_services in 
  Melodious.register ~service:main_service
    (fun () () ->
       try_lwt 
         lwt user = get_user () in
         Eliom_services.onload (player_onload ());
         lwt log_box = Authentication.logged_box () in
         return
           (html
             (head (title (pcdata "Melodious - Music player"))
                   [css_link ~uri:(make_uri ~service:(static_dir ()) ["style.css"]) ()])
             (body [ div' "header" [h1 [pcdata "FIXME"] ; log_box ]
                   ; div' "player" [player_elt]
                   ; div' "tracklist" [tracks_elt ; tracks_del_elt]
                   ; div' "playlists" [plists_elt ; plist_name_elt; plist_save_elt]
                   ; div' "search"
                     [ div' "input" [searchentry_elt ; searchbutton_elt ; addresults_elt]
                     ; div' "aggregates" [aggregates_elt]
                     ; div' "results" [results_elt]
                     ]
                   ])
           )
       with Auth.Unauthenticated ->
        Authentication.login_page main_service
    )

