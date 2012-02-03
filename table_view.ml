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

open Utils_client
open Eliom_client
module H = HTML5.M

module Index =
struct
  type index =
    { sym : Sym.t
    ; mutable pos : int
    }
  type t = index

  let make row =
    { sym = Sym.fresh ()
    ; pos = row
    }
  
  let update idx row =
    idx.pos <- row

  let pos idx = idx.pos
end

module Data = Hashtbl.Make
  (struct
     type t = Index.t
     let equal = (==)
     let hash idx = Hashtbl.hash idx.Index.sym
   end)

type index = Index.t

module Cells =
struct
  type cell = Dom_html.tableCellElement js
  type 'a gen = 'a -> cell * canceller

  let map (f : 'b -> 'a) (col : 'a gen) : 'b gen =
    col @% f

  let on_cancel (col : 'a gen) (c : 'a -> canceller) =
    (fun a -> let cell, cancel = col a in
              (cell, Canceller.compose (c a) cancel))


  let make_cell l = Html5.of_td (H.td l)

  let string_cells : string callback gen = fun cb ->
    let node = make_cell [] in
    cb (fun str ->
          Js.Opt.iter (node##firstChild)
            (fun x -> ignore @$ node##removeChild (x));
          Dom.appendChild node
            (Html5.of_element (H.pcdata str))
       );
    node, Canceller.empty
    

  let toggle_cells : bool port gen = fun p ->
    let box = H.unique (H.a []) in
    let get_elt b =
      H.pcdata (if b then "[x]"
                     else "[ ]")
    in
    let node = Html5.of_a box in
    ignore @$ Dom_events.listen node Dom_events.Typ.click 
      (fun _ _ -> Port.set p (not (Port.get p)));
    let set_box b =
      clear_children_of node;
      Dom.appendChild node
        (Html5.of_element (get_elt b))
    in
    set_box (Port.get p);
    make_cell [box],
    Port.register' p set_box

  let click_cells : (string callback * unit sink) gen = fun (cb, e) ->
    let cell = make_cell [] in
    let content str =
      clear_children_of cell;
      Dom.appendChild cell (Html5.of_element (H.a [H.pcdata str]))
    in
    cb content;
    ignore @$ Dom_events.listen cell
      Dom_events.Typ.click (fun _ _ -> Sink.put e ());
    cell, Canceller.empty

  let register_data (tbl : 't Data.t) (f : 'a -> index) (col : 'a gen) =
    let cancel a = Canceller.make
      (fun () -> Data.remove tbl (f a))
    in
    on_cancel col cancel

  let make_ports (a : 'a) (col : 'a port gen) :
      'a port Data.t * index gen =
    let tbl = Data.create 4 in
    let make_port ix = 
      let port = Port.make a in
        Data.replace tbl ix port;
      port
    in
    tbl, register_data tbl id (map make_port col)

end

exception Invalid_index

type 'd body = (index * 'd) Cells.gen
type 'd header = ((index * 'd), canceller) inversion Cells.gen

type 'd column = 'd header * 'd body

type 'a t =
  { mutable columns : 'a column list
  ; mutable rows : (index * canceller list ref * Dom_html.tableRowElement js) array
  ; data    : 'a Data.t
  ; element : Dom_html.tableElement js
  ; headrow : Dom_html.tableRowElement js
  ; body    : Dom_html.tableSectionElement js
  ; mutable revision : int
  }

let _array_empty () = Array.init 0 (fun _ -> failwith "Should not happen")
let get_index (i,_,_) = i
let _get_node (_,_,n) = n

let make node data : 'a t =
  let trow  = Dom_html.createTr Dom_html.document in
  let tbody = Dom_html.createTbody Dom_html.document in
  node##deleteTHead ();
  Dom.appendChild (node##createTHead ()) trow;
  Dom.appendChild node tbody;
  { columns = []
  ; rows    = _array_empty ()
  ; data    = data
  ; element = node
  ; headrow = trow
  ; body    = tbody
  ; revision = 0
  }

let revision_bump tbl = tbl.revision <- tbl.revision + 1

let revision tbl = tbl.revision
let revision_reset tbl = tbl.revision <- 0
let is_modified tbl = tbl.revision > 0

let is_index_of t index =
  let pos = index.Index.pos in
  if pos >= 0 && pos < Array.length t.rows then
    index == get_index t.rows.(pos)
  else
    false

let get_dom_node t index =
  let (_,_,nd) = t.rows.(index.Index.pos) in
    nd

let rel_index t index inc =
  if is_index_of t index
  then
    (match index.Index.pos + inc with
     | p when p < 0 -> None
     | p when p >= Array.length t.rows -> None
     | p -> Some (get_index t.rows.(p))
    )
  else None

let fold_left f x t =
  Array.fold_left (fun b (i,_,_) -> f b (i, Data.find t.data i)) x t.rows

let fold_right f t x =
  Array.fold_right (fun (i,_,_) b -> f (i, Data.find t.data i) b) t.rows x

let validate t index =
  if not @$ is_index_of t index then
    raise Invalid_index

let add_columns t cs = 
  t.columns <- t.columns @ cs;

  (* FIXME : Header cancellers *)
  ignore @$ flip List.map cs
    (fun (h,b) -> 
      Dom.appendChild t.headrow @$ fst (h (fun _ -> ()))
    );

  flip Array.iter t.rows
    (fun (index, cancellers, e) ->
      let newcancels = flip List.map cs 
         (fun (h,b) ->
           let cell, cancel = b (index, Data.find t.data index) in
           Dom.appendChild e cell;
           cancel)
      in
      cancellers := !cancellers @ newcancels
    )

let add_rows (t : 'a t) (rs : 'a list) =
  let len = Array.length t.rows in
  let ra  = Array.of_list rs in
  let new_rows = flip Array.mapi ra
    (fun i row_a ->
      let index = Index.make (i + len) in
      let row_e = t.body##insertRow (-1) in
      let cancellers = flip List.map t.columns
        (fun (h,b) ->
          let cell, cancel = b (index,row_a) in
          Dom.appendChild row_e cell;
          cancel)
      in
      Data.replace t.data index row_a;
      (index, ref cancellers, row_e)
    )
  in
  t.rows <- Array.append t.rows new_rows;
  revision_bump t;
  Array.to_list @$ Array.map get_index new_rows

let _cancel_row t (ridx, cancellers, node) =
  List.iter Canceller.trigger !cancellers;
  cancellers := [];
  Dom.removeChild t.body node;
  ridx.Index.pos <- -1
  
let rem_rows t rs =
  List.iter (validate t) rs;
  let rs' = List.sort (on Index.pos compare) rs in
  let newrows, _ = Array.fold_left
     (fun (rows, indexes) row ->
         match indexes with
         | (idx :: idxs') when (get_index row) = idx
                 -> _cancel_row t row;
                    (rows, idxs')
         | _     -> (row :: rows, indexes)
     ) ([], rs') t.rows 
  in
  t.rows <- Array.of_list newrows;
  revision_bump t


let clear_rows t =
  Array.iter (_cancel_row t) t.rows;
  t.rows <- _array_empty ();
  revision_bump t

let move_row t r pos =
  validate t r;
  let oldpos = r.Index.pos in
  let oldrow = t.rows.(oldpos) in
  let newpos =
    let len = Array.length t.rows in
    match pos with
    | p when p < 0 -> 0
    | p when p >= len -> len - 1
    | p -> p
  in
  if newpos != oldpos then
  begin
    (* Update node *)
    Dom.insertBefore t.body (_get_node oldrow)
      (Js.Opt.return @$ _get_node t.rows.(newpos));
    (* Update array *)
    if newpos < oldpos then
      Array.blit t.rows newpos t.rows (newpos + 1) (oldpos - newpos)
    else if newpos > oldpos then
      Array.blit t.rows (oldpos + 1) t.rows oldpos (newpos - oldpos);
    t.rows.(newpos) <- oldrow;
    (* Update indices *)
    for i = (min oldpos newpos) to (max oldpos newpos) do
      (get_index t.rows.(i)).Index.pos <- i
    done;
    revision_bump t
  end

let sort_rows t index_comp =
  let rows' = Array.copy t.rows in
  Array.stable_sort (on get_index index_comp) rows';
  t.rows <- rows';
  (* Update indices *)
  for i = 0 to Array.length t.rows - 1 do
    (get_index t.rows.(i)).Index.pos <- i;
    Dom.appendChild t.body (_get_node t.rows.(i))
  done;
  revision_bump t

let save_rows t =
  Array.fold_right (fun row b -> Data.find t.data (get_index row) :: b) t.rows []

let load_rows t l =
  clear_rows t;
  add_rows t l

