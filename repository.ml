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

open Utils
module P = Eliom_parameters

type hash = string
type name = string
type path = string
type rid = string

module type Sig =
sig
  val get_path : hash -> path
  val insert : Ocsigen_extensions.file_info -> rid
  val has : hash -> bool

  val get : rid -> XML.uri
end

let local_repo path name =
  let module Repo =
    struct
      let get_path hash =
        flip String.iter hash (fun c ->
            assert ((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9'))
          ) ;
        Filename.concat path (hash ^ ".ogg")

      let insert file =
        let tmp_path = Eliom_request_info.get_tmp_filename file in
        let hash = Digest.to_hex (Digest.file tmp_path) in
        (* FIXME:
         * Use Lwt_unix ?
         * handle exceptions: try _ with Unix.Unix_error _ -> ()
         * link doesn't cross file-systems
         *)
        Unix.link tmp_path (get_path hash);
        hash

      let has hash =
        Sys.file_exists (get_path hash)

      let service =
        Eliom_output.Files.register_service
        ~path:["repo";name]
          ~get_params:(P.suffix @$ P.string "hash")
          (fun hash () -> Lwt.return (get_path hash))
      
      let get rid =
        Eliom_output.Html5.make_uri
        ~https:false ~absolute:true ~service:service rid
    end
  in
    (module Repo : Sig)
