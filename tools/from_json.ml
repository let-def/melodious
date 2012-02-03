open Utils_generic
module Json = Yojson.Safe

module S =
struct
  include Sqlite3
  include Sql_utils
end
module B = S.Banana
module D = S.Data

let read_json filename = Json.from_file filename

let dump_json j =
  Json.pretty_to_channel ~std:true stdout j

type 'a effect = Effect of 'a

let update_song db =
  let stmt_del, stmt_ins = map2 (S.prepare db)
    ( "DELETE FROM tags WHERE song = ?"
    , "INSERT INTO tags (song,key,value) VALUES (?,?,?)"
    ) in
  Effect (fun song tags ->
    let _ = prerr_endline ("Tagging " ^ song) in
    let songid = S.id_to_data song in
    S.fold_query stmt_del [songid] B.void;
    flip List.iter tags
      (function
       | (k,`String v) ->
           S.fold_query stmt_ins [songid; D.TEXT k; D.TEXT v] B.void
       | (k, _) -> prerr_endline ("Unknown entry " ^ k)))

let main () =
  if Array.length Sys.argv < 3 then
    prerr_endline "Usage: from_json <file.json> <db.sqlite>"
  else
  begin
    let json_file, db_file = Sys.argv.(1), Sys.argv.(2) in
    let json = read_json json_file in
    let db = S.db_open db_file in
    let _ = prerr_endline "Opened database" in
    let _ = S.exec db "PRAGMA SYNCHRONOUS = OFF" in
    let Effect update = update_song db in
    let _ = match json with
    | `Assoc songs -> flip List.iter songs
      (function
       | (song, `Assoc tags) -> update song tags
       | _ -> prerr_endline "Invalid song schema"
      )
    | _ -> prerr_endline "Invalid json schema"
    in ignore (S.db_close db)
  end

let () = main ()
