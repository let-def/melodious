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

include Utils_generic

type ('a, 'b) either = Left of 'a | Right of 'b
deriving (Json)

module Config =
struct
  open Simplexmlparser 

  let sub node k =
    match node with
    | Element (_,_,nodes) -> k nodes
    | PCData _ -> raise Not_found

  let attrib node name () =
    match node with
    | Element (_,attribs,_) -> List.assoc name attribs
    | PCData _ -> raise Not_found

  let pcdata node () =
    match node with
    | Element _ -> raise Not_found
    | PCData text -> text

  let elem nodes child k =
    let rec find_elem = function
        Element (elem,_,_) as e :: _ when elem = child ->  e
      | _ :: l -> find_elem l
      | [] -> raise Not_found
    in
    k (find_elem nodes)

  let maybe k =
    try Some (k ())
    with Not_found -> None
end

