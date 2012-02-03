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
module D = Dom_media
module DE = D.MediaEvent

type position = float * float option

type resolved_resource = Model.Resource.t * XML.uri list

type state = 
  | Playing of position
  | Paused of position
  | Stopped
  | Ended

type action = 
  | Play
  | Pause
  | Stop
  | Seek of float
  | Volume of float
  | Open of resolved_resource option

type t =
    { ev_action : action event
    ; ev_state  : state event
    ; mutable resource : resolved_resource option
    ; element   : D.audioElement js
    }

let exec p a =
  begin match a with
  | Play     -> p.element##play ()
  | Pause    -> p.element##pause ()
  | Stop     -> p.element##load () (* Trigger reload *)
  | Seek f   -> p.element##currentTime <- Js.float f
  | Volume v -> p.element##volume <- Js.float v
  | Open res -> p.resource <- res;
                let src = match res with
                  | Some (_,url :: _) -> url
                  | Some (_,_) | None -> ""
                in
                p.element##src <- Js.string src;
                p.element##load ()
  end;
  begin match a with
  | Volume _ | Seek _ -> ()
  | _ -> Event.trigger p.ev_action a 
  end

let execs p = List.iter (exec p)

let on_action p = p.ev_action

let on_state p = p.ev_state

let get_position p =
  let time = Js.to_float p.element##currentTime in
  let duration =
    let f = Js.to_float p.element##duration in
    match classify_float f with
    | FP_normal | FP_subnormal | FP_nan -> Some f
    | _ -> None
  in
  (time, duration)

let get_state p =
  if (Js.to_bool p.element##ended) then
    Ended
  else if (Js.to_bool p.element##paused) &&
          ((fst (get_position p)) == Js.to_float p.element##initialTime) then
    Stopped
  else if (Js.to_bool p.element##paused) then
    Paused (get_position p)
  else
    Playing (get_position p)

let get_volume p =
  Js.to_float (p.element##volume)

let make (e : D.audioElement js) =
  let ea, es = Event.make (), Event.make () in
  let p = 
    { ev_action = ea
    ; ev_state  = es 
    ; resource  = None
    ; element   = e
    }
  in
  let _ = [
    Dom_events.listen e DE.pause
      (fun _ _ -> Event.trigger es (get_state p));
    Dom_events.listen e DE.playing
      (fun _ _ -> Event.trigger es (Playing (get_position p)));
    Dom_events.listen e DE.ended
      (fun _ _ -> Event.trigger es Ended);
    Dom_events.listen e DE.volumechange
      (fun _ _ -> Event.trigger ea (Volume (Js.to_float e##volume)));
    Dom_events.listen e DE.seeked
      (fun _ _ -> Event.trigger ea (Seek (fst (get_position p))))
  ] in
  p

