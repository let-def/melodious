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

type ('a,'b) proto = Proto of ('a -> 'b)

let make f = Proto f

let comap f (Proto p) = Proto (fun a -> p (f a))
let map   f (Proto p) = Proto (fun a -> f (p a))
let iter  f (Proto p) = Proto (fun a -> let x = p a in f x; x)

let run     (Proto p) = p
let direct  (Proto p) = Proto (fun s -> p (Inversion.const s))

let (>>>)   (Proto p) (Proto q) = Proto (fun a -> p (q a))
let ( *** ) (Proto p) (Proto q) = Proto (fun (a,b) -> p a, q b)
let (&&&)   (Proto p) (Proto q) = Proto (fun a -> p a, q a)

let with_arg proto = proto &&& make identity

let pfst () = make fst
let psnd () = make snd

module UI =
struct
  open Eliom_client

  let clickable proto = 
    let catch_click (node,sink) =
      let listener = Dom_events.listen node
        Dom_events.Typ.click (fun _ _ -> Sink.put sink ())
      in
      node, Canceller.make (fun () -> Dom_events.stop_listen listener)
    in
    map catch_click (with_arg proto)

  let text = make @$ fun f ->
    let node = Dom_html.createSpan Dom_html.document in
    let () = f (fun str ->
      clear_children_of node;
      Dom.appendChild node (Html5.of_element (HTML5.M.pcdata str)))
    in
    node
 
  let anchor = make @$ fun f ->
    let node = Dom_html.createA Dom_html.document in
    let () = f (fun str ->
      clear_children_of node;
      Dom.appendChild node (Html5.of_element (HTML5.M.pcdata str)))
    in
    node
end

