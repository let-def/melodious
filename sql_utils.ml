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

(** Slightly more friendly interface to Sqlite **)
open Utils_generic
module Sql = Sqlite3

exception E of string (* location *) * string (* description *)

let id_to_data id =
  try
    Sql.Data.INT (Int64.of_string id)
  with
    Failure "int_of_string" -> Sql.Data.TEXT id

let stmt_guard f stmt =
  let result =
    try f stmt
    with e -> ignore (Sql.finalize stmt); raise e
  in
  match Sql.finalize stmt with
  | Sql.Rc.OK -> result
  | x -> raise (E ("stmt_guard", Sql.Rc.to_string x))

type 'a banana = ('a -> Sql.Data.t list -> 'a) * 'a

let fold_query stmt args (fold,seed : 'a banana) : 'a =
  ignore @$ Sql.reset stmt;
  list_iteri (fun i a -> Sql.bind stmt (i + 1) a) args;
  let rec loop a =
    match Sql.step stmt with
    | Sql.Rc.ROW -> 
        let row = list_init (Sql.column stmt) (Sql.data_count stmt)
        in loop (fold a row)
    | _ -> a (* FIXME : handle errors *)
  in
  loop seed

module Banana =
struct
  open Sql.Data
  exception Failed_pattern

  let custom fold seed : 'a banana =
    fold, seed

  let void : unit banana = 
    (fun () _ -> ()), ()

  let exists : bool banana =
    (fun _ _ -> true), false

  let raw1_ = function
    | [x] -> x
    | _ -> raise Failed_pattern

  let raw2_ = function
    | [x;y] -> x, y
    | _ -> raise Failed_pattern

  let raw3_ = function
    | [x;y;z] -> x, y, z
    | _ -> raise Failed_pattern

  let raw1 : t option banana =
    (fun m l -> Some (raw1_ l)), None

  let raw2 : (t * t) option banana =
    (fun m l -> Some (raw2_ l)), None

  let raw3 : (t * t * t) option banana =
    (fun m l -> Some (raw3_ l)), None

  let raws1 : t list banana =
    (fun m l -> (raw1_ l) :: m), []

  let raws2 : (t * t) list banana =
    (fun m l -> (raw2_ l) :: m), []

  let raws3 : (t * t * t) list banana =
    (fun m l -> (raw3_ l) :: m), []

  let string1 : string option banana =
    (fun m l -> Some (to_string @$ raw1_ l)), None

  let string2 : (string * string) option banana =
    (fun m l -> Some (map2 to_string @$ raw2_ l)), None

  let string3 : (string * string * string) option banana =
    (fun m l -> Some (map3 to_string @$ raw3_ l)), None

  let strings1 : string list banana =
    (fun m l -> (to_string @$ raw1_ l) :: m), []

  let strings2 : (string * string) list banana =
    (fun m l -> (map2 to_string @$ raw2_ l) :: m), []

  let strings3 : (string * string * string) list banana =
    (fun m l -> (map3 to_string @$ raw3_ l) :: m), []

end
