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

type user_id = string
exception Unauthenticated

module Sig =
struct

  module type AuthListener =
  sig
    (* Called when user log in (auth_changed (Some "username"))
     * or log out (auth_changed None)
     *)
    val auth_changed : string option -> unit
  end

  module type AuthHandler   =
  sig
    (* Small box shown when user is logged *)
    val logged_box : unit -> [> HTML5_types.div ] HTML5.M.elt Lwt.t
    (* Page shown when user not logged *)
    val login_page : (unit, 'b, 'c, 'd, unit, 'g, 'h, 'i) Eliom_services.service
                     -> [ `Html] HTML5.M.elt Lwt.t

    val get_auth : unit -> string option Lwt.t
  end
  
end

module GuestHandler : Sig.AuthHandler =
struct
  open HTML5.M

  let get_auth () = Lwt.return (Some "guest")

  let logged_box () = Lwt.return 
  (div [ pcdata "Using guest account" ])

  let login_page _ =
    Lwt.return
      (html (head (title (pcdata "Dummy registration module") ) [])
           (body ( [ pcdata "Dummy registration module" ] ) ))
end

module HeaderHandler (DB : Database.Sig.DB) : Sig.AuthHandler = 
struct
  open HTML5.M

  let login_header = "REMOTE_USER"

  let get_auth () =
    Lwt.return (
    try
      let open Ocsigen_extensions in
      let rinfo = Eliom_request_info.get_ri () in
      let frame = rinfo.ri_http_frame in
      Some (Ocsigen_headers.find login_header frame)
    with
      Not_found -> None
    )

  let logged_box () = 
     lwt auth = get_auth () in
     Lwt.return (div [ pcdata (
             match auth with
             | Some user -> "Welcome " ^ user
             | None -> "Unauthenticated"
           ) 
         ])

    (* Page shown when user not logged *)
  let login_page _ =
    Lwt.return
      (html (head (title (pcdata "Please log-in to continue") ) [])
           (body ( [ pcdata "Please log-in to continue" ] ) ))
end
