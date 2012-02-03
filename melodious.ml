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

(** Melodious.ml : Client-side code **)

open Utils_client
open Model

module Table = Table_view

module Sig =
struct
  (* Server-side services *)
  module type Services =
  sig
    val song_resolver   : Resource.uid -> XML.uri list Lwt.t

    val playlist_list   : unit -> Playlist.folder Lwt.t
    val playlist_get    : Playlist.id -> Playlist.t Lwt.t

    val playlist_save   : string -> Resource.uid list -> unit Lwt.t
    val playlist_delete : Playlist.id -> unit Lwt.t

(*     val playlist_create : string -> Playlist.id Lwt.t *)
(*     val playlist_rename : Playlist.id -> string -> unit Lwt.t *)
(*     val playlist_set    : Playlist.id -> Resource.uid list -> unit Lwt.t *)
(*     val playlist_append : Playlist.id -> Resource.uid list -> unit Lwt.t *)

    val search_query    : string ->
      (Resource.t list, Tag.value list * Aggregation.t) either Lwt.t
    val aggregate_query : Aggregation.t ->
      (Resource.t list, Tag.value list * Aggregation.t) either Lwt.t
  end

  (* HTML Document *)
  module type Doc =
  sig
    val audio            : Dom_media.audioElement Js.t
    val tracks           : Dom_html.tableElement Js.t
    val tracks_delete    : Dom_html.element Js.t
  
    (* val playlist_name : string port *)
    val playlist_name    : Dom_html.inputElement Js.t
    val playlist_save    : Dom_html.buttonElement Js.t
    val playlists        : Dom_html.tableElement Js.t
  
    val search_input     : Dom_html.inputElement Js.t
    val search_submit    : Dom_html.buttonElement Js.t
    val results_add      : Dom_html.buttonElement Js.t
    val results          : Dom_html.tableElement Js.t
    val aggregates       : Dom_html.element Js.t
  end

  (* Audio player *)
  module type Player =
  sig
    type state = Idle | Current | Next 

    val audio : Audio.t

    val tracks : Resource.t Table.Data.t
    val register_table : Resource.t Table.t -> unit

    val column : Resource.t Table.column
  end

  (* Tracks table *)
  module type Tracks =
  sig
    val table   : Resource.t Table.t
    val checked : bool port Table.Data.t
  end

  (* Playlist management *)
  module type Playlist =
  sig
    val table : Playlist.name Table.t
  end

  (* Search Table *)
  module type Search =
  sig
    val results : Resource.t Table.t
  end
end;;

(* Shortcuts for making column *)

let cells_from_tag tag =
  Table.Cells.map 
    (fun (ix,a) -> Inversion.const @$ from_option "<?>" @$ Resource.get_tag tag a)
    Table.Cells.string_cells

let static_header title =
  Table.Cells.map 
    (fun _ -> Inversion.const title)
    Table.Cells.string_cells

let action_header title sink =
  Table.Cells.map 
    (fun _ -> Inversion.const title, sink)
    Table.Cells.click_cells

(* Select/deselect all elements of a table *)

let negate_checks table = Sink.make (fun () ->
  let status =
    not (Table.Data.fold (fun _ p b -> b && Port.get p) table true)
  in
  Table.Data.iter (fun _ p -> Port.set p status) table)

let make_checks () =
  let table, cells = 
    Table.Cells.make_ports false Table.Cells.toggle_cells
  in
  table, Table.Cells.map fst cells

module Parts =
struct
  module Player (Services : Sig.Services) (Doc : Sig.Doc) : Sig.Player =
  struct

    let audio = Audio.make Doc.audio

    let tracks  = Table.Data.create 0

    let tables = ref []
    let register_table tbl = tables := tbl :: !tables

    (* Track scheduling state *)

    type state = Idle | Current | Next 

    let states = Table.Data.create 0

    let set_state state ix = 
      try
        (Table.Data.find states ix)
          (match state with
           | Idle    -> "[--]"
           | Current -> "[>>]"
           | Next    -> "[>-]"
          )
      with
        Not_found -> ()

    (* Track selection *)

    let current_track = Port.make None
    let current_media = Port.make None

    let load_track ?(allow_restart=false) ix_opt =
      let m = option_trymap (Table.Data.find tracks) ix_opt in
      if allow_restart || Port.get current_media <> m then
        Port.set current_media m

    let next_track =
      let filter_next (old_ix,new_ix) =
        let set_current new_ix = 
          load_track ~allow_restart:true new_ix;
          Port.set current_track new_ix;
          Some None
        in
        if Port.get current_track = None then
          set_current new_ix
        else match old_ix, new_ix with
        | Some ix, Some ix' when ix = ix' -> set_current new_ix
        | _ -> Some new_ix
      in
      Port.filter (Port.make None) filter_next

    let _ =
      let switch_track (old_ix,new_ix) =
        option_iter (set_state Idle) old_ix;
        option_iter (set_state Current) new_ix;
        load_track ~allow_restart:false new_ix
      in
      let switch_next (old_ix,new_ix) =
        option_iter (set_state Idle) old_ix;
        option_iter (set_state Next) new_ix;
      in
      let switch_media (_,new_media) =
        match new_media with
        | None -> Audio.exec audio Audio.Stop
        | Some m ->
          Lwt.ignore_result @$
          lwt urls = Services.song_resolver (Resource.uid m) in
          Audio.execs audio
            [ Audio.Open (Some (m, urls))
            ; Audio.Play
            ];
          Lwt.return ()
      in
      Port.register current_track switch_track,
      Port.register current_media switch_media,
      Port.register next_track switch_next

    let track_ended () =
      match Port.get next_track with
      | Some ix when Table.Data.mem tracks ix ->
          Port.set next_track None;
          Port.set current_track (Some ix);
      | _ ->
        flip option_iter (Port.get current_track) (fun ix ->
          flip List.iter !tables (fun table ->
            match Table.rel_index table ix 1 with
            | Some next_ix -> Port.set current_track (Some next_ix)
            | None -> ()
          )
        )

    let _ = Event.register (Audio.on_state audio)
      (function
        | Audio.Ended -> track_ended ()
        | _ -> ())

    (* Player column *)

    let on_click = Event.make ()

    let column = 
      let click_sink =
        let s = Sink.of_event on_click in
        fun ix -> Sink.map (fun () -> Some ix) s
      in
      static_header "[>>]",
      Table.Cells.register_data states fst @$
        Table.Cells.map (fun (ix,media) ->
          (fun f -> 
            let validate ix tbl b = Table.is_index_of tbl ix || b in
            Table.Data.replace states ix f;
            match Port.get current_track with
            | Some oldix when not (List.fold_right (validate oldix) !tables false)
                           && Port.get current_media = Some media -> 
              Port.set current_track (Some ix);
              f "[>>]"
            | _ -> f "[--]";
          ),
          click_sink ix)
        Table.Cells.click_cells

    let _ = Event.register on_click (Port.set next_track)

  end

  module Tracks (Doc : Sig.Doc) (Services : Sig.Services) (Player : Sig.Player) : Sig.Tracks =
  struct
    (* Track list *)

    let table =
      let tbl = Table.make Doc.tracks Player.tracks in
      Player.register_table tbl;
      tbl

    let checked, check_col = make_checks ()

    let _ = Table.add_columns table
      [ Player.column
      ; action_header "[X]" (negate_checks checked), check_col
      ; static_header "Titre", cells_from_tag Tag.title
      ; static_header "Album", cells_from_tag Tag.album
      ; static_header "Artiste", cells_from_tag Tag.artist 
      ]

    (* Tracks Deletion *)

    let _ = Dom_events.listen Doc.tracks_delete Dom_events.Typ.click
      (fun _ _ -> 
       let idxs = Table.fold_right
            (fun (idx,_) idxs ->
              let p = Table.Data.find checked idx in
              if Port.get p
              then idx :: idxs
              else idxs
            ) table []
       in
       ignore @$ Table.rem_rows table idxs)

  end

  module Playlist (Doc : Sig.Doc) (Services : Sig.Services) (Tracks : Sig.Tracks) : Sig.Playlist =
  struct
    let playlists = Table.Data.create 0
    let table : Playlist.name Table.t = Table.make Doc.playlists playlists

    let get_name ()  = Js.to_string (Doc.playlist_name##value)
    let set_name str = Doc.playlist_name##value <- Js.string str

    let reload_playlists () =
      Lwt.ignore_result @$
      lwt playlists = Services.playlist_list () in 
      Lwt.return @$ Table.load_rows table playlists

    let save_playlist () =
      Lwt.ignore_result @$
      lwt _ = Services.playlist_save (get_name ())
        (List.map Resource.uid @$ Table.save_rows Tracks.table) in
      Table.revision_reset Tracks.table;
      reload_playlists ();
      Lwt.return ()

    let (on_delete, on_select) as events =
      map2 Event.make ((),())
    let sink_del, sink_select =
      map2 Sink.of_event_const events

    let _ = Event.register on_delete
      (fun ix ->
        let (plid, name) = Table.Data.find playlists ix in
        Lwt.ignore_result @$ 
        lwt _ = Services.playlist_delete plid in
        Table.rem_rows table [ix];
        Lwt.return ()
      ),
      Event.register on_select
      (fun ix ->
        let (plid,_) = Table.Data.find playlists ix in
        Lwt.ignore_result @$
        lwt ((name,_),tracklist) = Services.playlist_get plid in
        set_name name;
        Lwt.return (Table.load_rows Tracks.table tracklist)
      )

    let _ = Table.add_columns table
      [ static_header "", Table.Cells.map
          (fun (ix,_) -> Inversion.const "[-]", sink_del ix) @$
          Table.Cells.click_cells
      ; static_header "Playlist", Table.Cells.map
          (fun (ix,(_,s))  -> Inversion.const s, sink_select ix) @$
          Table.Cells.click_cells
      ]
    
    let _ = Dom_events.listen Doc.playlist_save Dom_events.Typ.click
      (fun _ _ -> save_playlist ())
      
    let _ = reload_playlists ()
  end

  module Search (Doc : Sig.Doc) (Services : Sig.Services) (Player : Sig.Player) (Tracks : Sig.Tracks) : Sig.Search =
  struct
    let results =
      let tbl = Table.make Doc.results Player.tracks in
      Player.register_table tbl;
      tbl

    let checked, check_col = make_checks ()

    let rec update_search_results = function
      | Left content ->
          Table.clear_rows results;
          ignore @$ Table.add_rows results content
      | Right (values,agg) ->
          clear_children_of Doc.aggregates;
          let elem_from text f =
            let a_elem = 
              Eliom_client.Html5.of_a
              (HTML5.M.a [HTML5.M.pcdata (" [| " ^ text ^ " |] ")])
            in
            ignore @$ Dom_events.listen a_elem Dom_events.Typ.click
              (fun _ _ -> f ());
            Dom.appendChild Doc.aggregates a_elem
          in
          if Aggregation.can_go_up agg then
            elem_from ".." (fun _ -> exec_aggregation (Aggregation.go_up agg));
          flip List.iter values
            (fun text -> elem_from text
                (fun () -> exec_aggregation (Aggregation.apply agg text)))

    and exec_aggregation agg =
      Lwt.ignore_result @$
      lwt result = Services.aggregate_query agg in
      Lwt.return @$ update_search_results result

    let _ = Table.add_columns results 
      [ Player.column 
      ; action_header "[X]" (negate_checks checked), check_col
      ; static_header "Titre", cells_from_tag Tag.title
      ; static_header "Album", cells_from_tag Tag.album
      ; static_header "Artiste", cells_from_tag Tag.artist 
      ]

    let _ = Dom_events.listen Doc.results_add Dom_events.Typ.click
      (fun _ _ -> 
        let tracks = Table.fold_right
            (fun (ti,track) tracks ->
              let p = Table.Data.find checked ti in
              if Port.get p
              then (Port.set p false; track :: tracks)
              else tracks
            ) results []
        in
        ignore @$ Table.add_rows Tracks.table tracks)
  
    let _ = 
      let exec_search query =  
        Lwt.ignore_result @$
        lwt content = Services.search_query query in
        Table.clear_rows results;
        clear_children_of Doc.aggregates;
        update_search_results content;
        Lwt.return ()
      in
      let default_search () =
        exec_search (Js.to_string Doc.search_input##value)
      in
      Dom_events.listen Doc.search_input Dom_events.Typ.keyup
        (fun _ k -> if k##keyCode = 13 then default_search ()),
      Dom_events.listen Doc.search_submit Dom_events.Typ.click
        (fun _ _ -> default_search ())
  end
end

module Make (Doc : Sig.Doc) (Services : Sig.Services) =
struct
  module Player   = Parts.Player (Services) (Doc) 
  module Tracks   = Parts.Tracks (Doc) (Services) (Player)
  module Playlist = Parts.Playlist (Doc) (Services) (Tracks)
  module Search   = Parts.Search (Doc) (Services) (Player) (Tracks)
end

