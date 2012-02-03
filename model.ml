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

(** Model.ml : basic type definition, used throughout the whole project **)
open Utils

module Tag =
struct
  type key   = string
  and  value = string
  and  t     = key * value
  deriving (Json)

  let artist : key = "artist"
  let album  : key = "album"
  let title  : key = "title"
  let number : key = "tracknumber"

  let value_order_bottom = (max_int,"")
  let value_order = function
    | "" -> value_order_bottom
    | s ->
      let num =
        let len = String.length s in
        let zeroa = Char.code '0' in
        let rec nums aux i =
          if i < len 
          then
            match s.[i] with
            | c when c >= '0' && c <= '9' ->
              nums (aux * 10 + Char.code c - zeroa) (i + 1)
            | ' ' -> nums aux (i + 1)
            | _   -> aux
          else aux
        in
          nums 0 0
      in
      (num,s)
end

module Aggregation =
struct
  (* Aggregation is a type used between client and server to describe
   * the state of an aggregation :
   * - base is the standard query emitted by client
   * - bound are the aggregated tags that have been bound to a value by the
   *   client
   * - unbound are the tags waiting to be bound
   *
   * A final query has no unbound keys, and should return the list of resources
   * satisfying the tags.
   *
   * A transient query has some unbound keys, and thus returns the list of
   * values the key can be instantiated to (matching both base and bound tags).
   *)
  type t =
    { base : Tag.t list
    ; bound : Tag.t list
    ; unbound : Tag.key list
    }
  deriving (Json)

  let can_go_up agg =
    match agg with
    | { bound = (_ :: _) } -> true
    | _ -> false

  let go_up agg =
    match agg with
    | { bound = ((k,v) :: bound) ; unbound = unbound } ->
       { agg with bound   = bound
                ; unbound = k :: unbound }
    | _ -> agg

  let apply agg value =
    match agg with
    | { bound = bound ; unbound = (k :: unbound)  } ->
      { agg with bound   = (k,value) :: bound
               ; unbound = unbound }
    | _ -> agg
end

module Resource =
struct
  type uid = string
  and  t   = uid * Tag.t list
  deriving (Json)

  let uid  (u : t) = fst u
  let tags (u : t) = snd u
  
  let get_tag k (u : t) =
    try  Some (List.assoc k (tags u))
    with Not_found -> None

  let sort_by (keys : Tag.key list) (content : t list) : t list =
    (* Give index to each key *)
    let index = Hashtbl.create 4 in
    list_iteri (fun i k -> Hashtbl.replace index k i) keys;
    let count = Hashtbl.length index in
    (* Create an array associating key index to value for each song tags *)
    let data = flip List.map content
      (fun res ->
        let tags' = tags res in
        let values = Array.make count Tag.value_order_bottom in
        flip List.iter tags'
          (fun (k,v) ->
            try values.(Hashtbl.find index k) <- Tag.value_order v
            with Not_found -> ());
        values,res)
    in
    List.map snd @$ List.stable_sort (fun (a1,_) (a2,_) -> compare a1 a2) data
end

module Playlist =
struct
  type id    = string
  and  name = string * id
  and  t       = name * Resource.t list 
  and  folder  = name list
  deriving (Json)
end

