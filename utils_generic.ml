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

let (@%) f g x = f (g x)
let (@$) f x = f x
let flip f x y = f y x
let identity x = x

let map2 f (a,b) = (f a, f b)
let map3 f (a,b,c) = (f a, f b, f c)
let map4 f (a,b,c,d) = (f a, f b, f c, f d)

let const (v : 'b) : 'a -> 'b = fun _ -> v

let on f op x y = op (f x) (f y)

let from_option n = function
  | Some s -> s
  | None -> n

let option_map f = function
  | Some a -> Some (f a)
  | None -> None

let option_trymap f = function
  | Some a -> 
      (try
        Some (f a)
       with _ -> None)
  | None -> None

let option_iter f = function
  | Some a -> f a
  | None -> ()

let list_init f len =
  let rec loop = function
    | n when n >= len -> []
    | n -> f n :: loop (n + 1)
  in
  loop 0

let list_iteri f =
  let rec loop i l =
    match l with
    | [] -> ()
    | a :: l' -> f i a; loop (i + 1) l'
  in
  loop 0

let rec list_take n = function
  | _ when n = 0 -> []
  | x :: xs -> x :: list_take (n - 1) xs
  | [] -> []

let rec list_uniq = function
  | x1 :: ((x2 :: _) as xs) when x1 = x2 -> list_uniq xs
  | x1 :: xs -> x1 :: list_uniq xs
  | [] -> []

module Canceller =
struct
  type t = Canceller of (unit -> unit)
           | Empty

  let trigger = function
    | Canceller c -> c ()
    | Empty -> ()

  let empty = Empty
  let make f = Canceller f

  let compose c d = match c, d with
  | Canceller c', Canceller d' -> Canceller (fun () -> c' (); d' ()) 
  | c, Empty | Empty, c -> c
end

type canceller = Canceller.t

module Event =
struct
  type 'a t = (unit ref * ('a -> unit)) list ref
  
  (* Server *)
  let make () : 'a t = ref []
  
  let trigger (e : 'a t) (a : 'a) =
    List.iter (fun (r,f) -> f a) (!e)
  
  (* Clients *)
  let register (e : 'a t) f : Canceller.t =
    let i = (ref (), f) in
    let c () = 
      e := List.remove_assq (fst i) (!e);
    in
    e := i :: !e; Canceller.make c

  let sub (f : 'a -> 'b option) (ea : 'a t) : 'b t * canceller =
    let eb = make () in
    eb,
    register ea (fun a -> match f a with
                 | Some b -> trigger eb b
                 | None   -> ()
                )

  let super (f : 'b -> 'a option) (ea : 'a t) : 'b t * canceller =
    let eb = make () in
    eb,
    register eb (fun b -> match f b with
                 | Some a -> trigger ea a
                 | None   -> ()
                )
end

module Port =
struct
  type 'a t = (unit -> 'a) * ('a * 'a) Event.t
  
  let get   ((f, _) : 'a t)  = f ()
  let set   ((f, ev) : 'a t) v = Event.trigger ev (f (), v)

  let event ((_, ev) : 'a t) = ev
 
  let make (v : 'a) =
    let r = ref v in
    let ev = Event.make () in
    ignore (Event.register ev (fun (_,v') -> r := v'));
    ((fun () -> !r), ev)

  let register p f = Event.register (event p) f
  let register' p f = Event.register (event p) (fun (_, v) -> f v)

  let filter (get,set) f : 'a t =
    let set',_ = flip Event.super set
      (fun (a',a) ->
        option_map (fun a -> (a',a)) (f (a',a)))
    in
    (get,set')
end

module Inversion =
struct
  type ('a,'r) t = ('a -> 'r) -> unit

  let make (v : ('a -> 'r) -> unit) : ('a,'r) t = v
  let const (v : 'a) : ('a,'r) t = make (fun fa -> fa v)

  let trigger (ca : ('a,'r) t) (f : 'a -> 'r) = ca f

  let map (f : 'b -> 'a) (ca : ('a,'r) t) : ('b,'r) t =
    fun fb -> ca (f @% fb)
end

module Sink =
struct
  type 'a t = 'a -> unit

  let make (v : 'a -> unit) : 'a t = v

  let of_event (e : 'a Event.t) : 'a t =
    make (Event.trigger e)

  let put (p : 'a t) (v : 'a) = p v

  let map (f : 'b -> 'a) (s : 'a t) : 'b t =
    s @% f

  let of_event_const (e : 'a Event.t) : 'a -> unit t =
    let s = of_event e in
      fun a -> map (const a) s
end

module Sym : 
sig
  type t
  val fresh : unit -> t
end =
struct
  type t = int
  let fresh =
    let counter = ref 0 in
    fun () ->
      counter := !counter + 1;
      !counter
end

type 'a event = 'a Event.t
type 'a port = 'a Port.t
type 'a sink = 'a Sink.t

type ('a,'r) inversion = ('a,'r) Inversion.t
type 'a callback = ('a,unit) inversion
