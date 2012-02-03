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

open Js
module D = Dom_html

(* From dom_html.ml  *)

(* FIXME : Cleanup unsafe code *)
external rawCoerce : _ -> _ = "%identity"

let unsafeCoerce tag (e : #Dom_html.element t) =
  (* FIXME: Stopped working ?! *)
  if Js.to_string (e##tagName##toLowerCase()) = tag then
    Js.some (Js.Unsafe.coerce e)
  else
    Js.null

let createElement (doc : D.document t) name =
  doc##createElement(Js.string name)

let unsafeCreateElement doc name =
  Js.Unsafe.coerce (createElement doc name)

let coerceEvent e : Dom_html.event Js.t Dom_events.Typ.typ =
  rawCoerce (Js.string e)

(****)

class type mediaElement = object
  inherit D.element

  (* Error state *)
  (* FIXME mediaError
  method error : mediaError t readonly_prop
  *)

  (* Network state *)
  method src : js_string t prop
  method currentSrc : js_string t readonly_prop
  method networkState : int readonly_prop
  method preload : js_string t prop
  (* FIXME timeRanges
  method buffered : timeRanges t readonly_prop
  *)
  method load : unit meth
  method canPlayType : js_string t -> js_string t meth

  (* Ready state *)
  method readyState : int readonly_prop
  method seeking : bool t readonly_prop

  (* Playback state *)
  method currentTime : float t prop
  method initialTime : float t readonly_prop
  method duration : float t readonly_prop
  method startOffsetTime : date t readonly_prop
  method paused : bool t readonly_prop
  method defaultPlaybackRate : float t prop
  method playbackRate : float t prop
  (* FIXME timeRanges
  method played : timeRanges t readonly_prop
  method seekable : timeRanges t readonly_prop
  *)
  method ended : bool t readonly_prop
  method autoplay : bool t prop
  method loop : bool t prop

  method play : unit meth
  method pause : unit meth

  (* Media controller *)
  method mediaGroup : js_string t prop
  (* FIXME mediaController 
  method controller : mediaController t prop
  *)

  (* Controls *)
  method controls : bool t prop
  method volume : float t prop
  method muted : bool t prop
  method defaultMuted : bool t prop
  
  (* Tracks *)
  (* FIXME multipleTrackList exclusiveTrackList textTrack mutableTextTrack 
  method audioTracks : multipleTrackList t readonly_prop
  method videoTracks : exclusiveTrackList t readonly_prop
  method textTracks : textTrack t js_array t readonly_prop

  method addTextTrack : js_string t -> mutableTextTrack t meth
  method addTextTrack_withLabel : js_string t -> js_string t ->
      mutableTextTrack t meth
  method addTextTrack_full : js_string t -> js_string t -> js_string t ->
      mutableTextTrack t meth
  *)
end

class type audioElement = object
  inherit mediaElement
end

class type videoElement = object
  inherit mediaElement

  method width : int prop
  method height : int prop
  method videoWidth : int readonly_prop
  method videoHeight : int readonly_prop
end

(****)

let createAudio doc : audioElement t = unsafeCreateElement doc "audio"
let createVideo doc : videoElement t = unsafeCreateElement doc "video"

module CoerceTo =
struct
  let audio e : audioElement t opt = unsafeCoerce "audio" e
  let video e : videoElement t opt = unsafeCoerce "video" e
end

module MediaEvent =
struct
  let loadstart = coerceEvent "loadstart"
  let progress = coerceEvent "progress"
  let suspend = coerceEvent "suspend"
  let abort = coerceEvent "abort"
  let error = coerceEvent "error"
  let emptied = coerceEvent "emptied"
  let stalled = coerceEvent "stalled"
  let loadedmetadata = coerceEvent "loadedmetadata"
  let loadeddata = coerceEvent "loadeddata"
  let canplay = coerceEvent "canplay"
  let canplaythrough = coerceEvent "canplaythrough"
  let playing = coerceEvent "playing"
  let waiting = coerceEvent "waiting"
  let seeking = coerceEvent "seeking"
  let seeked = coerceEvent "seeked"
  let ended = coerceEvent "ended"
  let durationchange = coerceEvent "durationchange"
  let timeupdate = coerceEvent "timeupdate"
  let play = coerceEvent "play"
  let pause = coerceEvent "pause"
  let ratechange = coerceEvent "ratechange"
  let volumechange = coerceEvent "volumechange"

  (* FIXME: Should go somewhere else, hackish there should be a better way to
   * bind js events *)
  let input = coerceEvent "input"
  let submit = coerceEvent "submit"
end

(* Of ... for Eliom_client *)

module Client =
struct
  (* HTML5_types.audio_ *)
  let of_audio (e : 'a HTML5_types.audio Eliom_pervasives.HTML5.M.elt)
               : audioElement t =
      Js.Unsafe.coerce (Eliom_client.Html5.of_element e)

  (* HTML5_types.video_ *)
  let of_video (e : 'a HTML5_types.video Eliom_pervasives.HTML5.M.elt)
               : videoElement t =
      Js.Unsafe.coerce (Eliom_client.Html5.of_element e)
end
