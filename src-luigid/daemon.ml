(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let err_setsid = "setsid (): unable to detach from terminal"
let err_parse_pid f = Printf.sprintf "unable to parse PID file %s" f
let err_unix e f v = Printf.sprintf "%s%s: %s" f v (Unix.error_message e)

let rec no_EINTR f x = try f x with
| Unix.Unix_error (Unix.EINTR, _, _) -> no_EINTR f x

let apply f x ~finally y =
  let result = try f x with exn -> finally y; raise exn in
  finally y;
  result

(* Daemonization *)

let daemonize ~dir ~umask =
  flush stdout; flush stderr;
  try
    if Unix.fork () <> 0 then (`Parent `Ok) else
    try
      if Unix.setsid () = -1 then `Daemon (`Error err_setsid) else
      (Unix.chdir dir; ignore (Unix.umask umask); `Daemon `Ok)
    with
    | Unix.Unix_error (e, f, v) -> `Daemon (`Error (err_unix e f v))
  with
  | Unix.Unix_error (e, f, v) -> `Parent (`Not_forked (err_unix e f v))

(* Daemon status *)

let output_pid oc pid = Printf.fprintf oc "%d" pid
let input_pid ic = try Scanf.fscanf ic "%d" (fun x -> `Pid x) with
| Scanf.Scan_failure _ | Failure _ | End_of_file -> `Error

let status pid_file =
  try
    let fd = no_EINTR (Unix.openfile pid_file [Unix.O_RDONLY;]) 0o644 in
    let ic = Unix.in_channel_of_descr fd in
    match apply input_pid ic ~finally:close_in ic with
    | `Error -> `Error (err_parse_pid pid_file)
    | `Pid pid ->
        try (Unix.kill pid 0; `Alive pid) with
        | Unix.Unix_error (Unix.ESRCH, _, _) -> `Not_found pid
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) (* openfile *) -> `Dead
  | Unix.Unix_error (e, f, v) -> `Error (err_unix e f v)

let set_status pid_file s =
  try match s with
  | `Alive ->
      let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL;] in
      let fd = no_EINTR (Unix.openfile pid_file flags) 0o644 in
      let oc = Unix.out_channel_of_descr fd in
      apply (output_pid oc) (Unix.getpid ()) ~finally:close_out oc; `Ok
  | `Dead ->
      Unix.unlink pid_file; `Ok
  with
  | Unix.Unix_error (e, f, v) -> `Error (err_unix e f v)

(* Daemon commands

   TODO Check that recv_command () is race free w.r.t. to a blocking
   infinitely blocking select. *)

type command =
  | Other | Reopen_logs | Restart | Stop | Quick_stop

let cmd_sig_rel = [
  Other, Sys.sigusr2;
  Reopen_logs, Sys.sigusr1;
  Restart, Sys.sighup;
  Stop, Sys.sigquit;
  Quick_stop, Sys.sigterm;
  Quick_stop, Sys.sigint; ]

let last_received = ref None
let receive c _ = if c > !last_received then last_received := c
let enable_command_reception () =
  let recv (c, s) = Sys.set_signal s (Sys.Signal_handle (receive (Some c))) in
  List.iter recv cmd_sig_rel

let recv_command () = let l = !last_received in last_received := None; l
let send_command pid c =
  let s = try List.assoc c cmd_sig_rel with Not_found -> assert false in
  try (Unix.kill pid s; `Ok) with
  | Unix.Unix_error (Unix.ESRCH, _, _) -> `Not_found
  | Unix.Unix_error (e, f, v) -> `Error (err_unix e f v)

(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli
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
