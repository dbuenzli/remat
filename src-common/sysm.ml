(*
 * Copyright (c) 2014 Daniel C. Bünzli
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let str = Printf.sprintf

(* Command and IO operation results *)

let err_error msg = str "result value is (`Error %S)" msg

type 'a result = [ `Ok of 'a | `Error of string ]

let ret v = `Ok v
let error e = `Error e
let bind v f = match v with `Ok v -> f v | `Error _ as e -> e
let map v f = match v with `Ok v -> `Ok (f v) | `Error _ as e -> e
let get = function `Ok v -> v | `Error msg -> invalid_arg (err_error msg)
let ignore_error ~use r = match r with `Ok v -> v | `Error _ -> use
let on_error ?(level = Log.Error) ~use r = match r with
| `Ok v -> v
| `Error msg -> Log.kmsg (fun () -> use) level "@[%a@]" Fmt.pp_text msg

let reword_error ?(replace = false) msg r = match r with
| `Ok _ as r -> r
| `Error _ when replace -> `Error msg
| `Error old -> `Error (str "%s\n%s" msg old)

let exn_msg bt _ _ = Printexc.raw_backtrace_to_string bt
let exn_error ?(msg = exn_msg) f v = try `Ok (f v) with
| e ->
    let bt = Printexc.get_raw_backtrace () in
    `Error (msg bt e v)

let ( >>= ) = bind
let ( >>| ) = map

module Infix = struct
  let ( >>= ) = ( >>= )
  let ( >>| ) = ( >>| )
end

(* Path operations *)

type path = Path.t (* to avoid Sysm.mli confusion *)

let path_str = Path.to_string

module SPath = struct   (* Renamed at the end of the module. *)
  let exists p = try ret (Sys.file_exists (path_str p))
  with Sys_error e -> error e

  let err_move src dst =
    let src, dst = (path_str src), (path_str dst) in
    error (str "move %s to %s: destination exists" src dst)

  let move ?(force = false) src dst =
    (if force then ret false else exists dst) >>= fun don't ->
    if don't then err_move src dst else
    try ret (Sys.rename (path_str src) (path_str dst)) with
    | Sys_error e -> error e
end

(* File operations *)

module File = struct

  let apply f x ~finally y =
    let result = try f x with
    | e -> try finally y; raise e with _ -> raise e
    in
    finally y;
    result

  let is_dash = Path.is_dash

  (* Files *)

  let dev_null = match Sys.os_type with
  (* Using Sys.os_type, because that's really for the driver. *)
  | "Win32" -> Path.file "NUL"
  |  _ -> Path.(root /"dev"/"null")

  let exists file =
    let file = path_str file in
    try ret (Sys.file_exists file && not (Sys.is_directory file)) with
    | Sys_error e -> error e

  let delete ?(maybe = false) file =
    exists file >>= fun exists ->
    if maybe && not exists then ret () else
    try ret (Sys.remove (path_str file)) with Sys_error e -> error e

  let temp suff =
    try
      let f = Path.of_string (Filename.temp_file "assemblage" suff) in
      at_exit (fun () -> ignore (delete f));
      ret f
    with Sys_error e -> error e

  (* Input *)

  let with_inf f file v =
    try
      let ic = if is_dash file then stdin else open_in_bin (path_str file) in
      let close ic = if is_dash file then () else close_in ic in
      apply (f ic) v ~finally:close ic
    with
    | Sys_error e -> error e

  let read file =
    let input ic () =
      let len = in_channel_length ic in
      let s = String.create len in
      really_input ic s 0 len; ret s
    in
    with_inf input file ()

  let read_lines file = read file >>| (Prelude.String.split ~sep:"\n")

  (* Output *)

  let with_outf f file v =
    try
      let oc = if is_dash file then stdout else open_out_bin (path_str file) in
      let close oc = if is_dash file then () else close_out oc in
      apply (f oc) v ~finally:close oc
    with
    | Sys_error e -> error e

  let write file contents =
    let write oc contents = output_string oc contents; ret () in
    if is_dash file then with_outf write file contents else
    temp "write"
    >>= fun tmpf -> with_outf write tmpf contents
    >>= fun () -> SPath.move ~force:true tmpf file

  let write_lines file lines = write file (String.concat "\n" lines)

  let write_subst vars file contents =
    let write_subst oc contents =                     (* man that's ugly. *)
      let s = contents in
      let start = ref 0 in
      let last = ref 0 in
      let len = String.length s in
      while (!last < len - 4) do
        if not (s.[!last] = '%' && s.[!last + 1] = '%') then incr last else
        begin
          let start_subst = !last in
          let last_id = ref (!last + 2) in
          let stop = ref false in
          while (!last_id < len - 1 && not !stop) do
            if not (s.[!last_id] = '%' && s.[!last_id + 1] = '%') then begin
              if s.[!last_id] <> ' '
              then (incr last_id)
              else (stop := true; last := !last_id)
            end else begin
              let id_start = start_subst + 2 in
              let id = String.sub s (id_start) (!last_id - id_start) in
              try
                let subst = List.assoc id vars in
                Pervasives.output oc s !start (start_subst - !start);
                output_string oc subst;
                stop := true;
                start := !last_id + 2;
                last := !last_id + 2;
              with Not_found ->
                stop := true;
                last := !last_id
            end
          done
        end
      done;
      Pervasives.output oc s !start (len - !start); ret ()
    in
    if is_dash file then with_outf write_subst file contents else
    temp "write"
    >>= fun tmpf -> with_outf write_subst tmpf contents
    >>= fun () -> SPath.move ~force:true tmpf file
end

(* Directory operations *)

module Dir = struct
  let exists dir =
    let dir = path_str dir in
    try ret (Sys.file_exists dir && Sys.is_directory dir)
    with Sys_error e -> error e

  let getcwd () =
    try ret (Path.of_string (Sys.getcwd ())) with Sys_error e -> error e

  let chdir dir =
    try ret (Sys.chdir (path_str dir)) with Sys_error e -> error e

  let fold_files_rec ?(skip = []) f acc paths =
    let is_dir d = try Sys.is_directory d with Sys_error _ -> false in
    let readdir d = try Array.to_list (Sys.readdir d) with Sys_error _ -> [] in
    let keep p = not (List.exists (fun s -> Filename.check_suffix p s) skip) in
    let process acc file = acc >>= fun acc -> f file acc in
    let rec aux f acc = function
    | (d :: ds) :: up ->
        let paths = List.rev_map (Filename.concat d) (readdir d) in
        let paths = List.find_all keep paths in
        let dirs, files = List.partition is_dir paths in
        begin match List.fold_left process acc files with
        | `Error _ as e -> e
        | `Ok _ as acc -> aux f acc (dirs :: ds :: up)
        end
    | [] :: [] -> acc
    | [] :: up -> aux f acc up
    | _ -> assert false
    in
    let paths = List.find_all keep paths in
    let dirs, files = List.partition is_dir paths in
    let acc = List.fold_left process (`Ok acc) files in
    aux f acc (dirs :: [])
end

(* Environment variables lookup *)

let env var = try Some (Sys.getenv var) with Not_found -> None
let get_env var = try `Ok (Sys.getenv var) with
| Not_found -> `Error (str "environment variable `%s' undefined" var)

(* Commands *)

(* FIXME in these functions should [cmd] and [args] be quoted ? *)

let exists cmd =
  let null = path_str File.dev_null in
  (* Using Sys.os_type, because that's really for the driver. *)
  let test = match Sys.os_type with "Win32" -> "where" | _ -> "type" in
  try ret (Sys.command (str "%s %s 1>%s 2>%s" test cmd null null) = 0)
  with Sys_error e -> error e

let trace cmd = Log.info ~header:"EXEC" "@[<2>%a@]" Fmt.pp_text cmd
let mk_cmd cmd args = String.concat " " (cmd :: args)

let execute cmd = trace cmd; Sys.command cmd
let exec_ret cmd args = execute (mk_cmd cmd args)
let handle_ret cmd = match execute cmd with
| 0 -> ret ()
| c -> error (str "Exited with code: %d `%s'" c cmd)

let exec cmd args = handle_ret (mk_cmd cmd args)
let exec_read ?(trim = true) cmd args =
  let cmd = mk_cmd cmd args in
  File.temp "cmd-read"
  >>= fun file -> handle_ret (str "%s > %s" cmd (path_str file))
  >>= fun () -> File.read file
  >>= fun v -> ret (if trim then Prelude.String.trim v else v)

let exec_read_lines cmd args =
  exec_read cmd args >>| Prelude.String.split ~sep:"\n"

let exec_write cmd args file =
  let cmd = mk_cmd cmd args in
  File.temp "cmd-write"
  >>= fun tmpf -> handle_ret (str "%s > %s" cmd (path_str tmpf))
  >>= fun () -> SPath.move ~force:true tmpf file

(* Give the SPath module its correct name. *)

module Path = SPath

(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
