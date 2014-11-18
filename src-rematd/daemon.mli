(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Daemon functions. 

    The {!daemonize} function does all what is needed to fork off the
    deamon from the controlling terminal. 

    The {{!section:status}status functions} handle the daemon's
    running status via a PID file and the 
    {{!section:commands}command functions} give basic external 
    daemon control via UNIX signals.
*)

(** {1 Daemonization} *)

val daemonize : dir:string -> umask:int->
  [ `Daemon of [`Ok | `Error of string] 
  | `Parent of [`Ok | `Not_forked of string] ]
(** [daemonize dir umask] daemonizes the process. It forks the parent, 
    detaches the child from the controlling terminal, sets its working
    directory to [dir] and its umask to [umask]. The function returns 
    {ul 
    {- [`Parent `Ok] to the parent process if the daemon could be forked.}
    {- [`Parent (`Not_forked reason)] to the parent process if the daemon 
    could not be forked.}
    {- [`Daemon `Ok] to the daemon if the daemonization succeded.}
    {- [`Daemon (`Error _)] to the daemon if the daemonization failed.}} *)

(** {1:status Daemon status} *)

val status : string -> [ `Alive of int | `Dead | `Not_found of int | 
  `Error of string ]
(** [status pid_file] is the daemon status as given by [pid_file]. The value is:
    {ul 
    {- [`Alive pid] if the daemon is running as the process with ID [pid].} 
    {- [`Dead] if [pid_file] does not exist.}
    {- [`Not_found pid] if [pid_file] exists but a process with ID [pid] 
       does not exist.} 
    {- [`Error _] in case of error.}} *)

val set_status : string -> [`Alive | `Dead ] -> 
  [`Ok | `Error of string ]
(** [set_status pid_file s] sets the daemon's status to [s] using
    [pid_file]. If [s] is :
    {ul 
    {- [`Alive] then [status pid_file] should be [`Dead] before. This creates
     the [pid_file] and writes the current process ID to it.}
    {- [`Dead] then [status pid_file] should be [`Alive] or [`Not_found]. 
       This unlinks the [pid_file].}}
    [`Error _] is returned in case of unix errors (which also occur
    if the conditions on the current status are not satisfied.) *)

(** {1 Daemon stdio} *)

(** {1 Deamon logs} *)

(* val dev_null_to_stdin : unit -> [ `Ok | `Error of string ] *)
(** [dev_null_to_stdin ()] redirects [/dev/null] to [stdin]. *)

(** {1:commands Daemon commands} 

    Daemon commands are handled via UNIX signals.
*)

type command = 
  | Other (** {!Sys.sigusr2} *)
  | Reopen_logs (** {!Sys.sigusr1} *)
  | Restart (** {!Sys.sighup} *)
  | Stop (** {!Sys.sigquit} *)
  | Quick_stop (** {!Sys.sigterm} or {!Sys.sigint} *)
(** The type for daemon commands with their corresponding UNIX signal, ordered
    by increasing priority. *)

val enable_command_reception : unit -> unit
(** [enable_command_reception ()] setups the daemon process to receive commands 
    via UNIX signals. *)

val recv_command : unit -> command option
(** [recv_command ()] checks if a command was received. If multiple commands
    where received, only the one with highest priority is kept. 
    
    {b Note.} This function call doesn't block. It is typically called
    after blocking functions that may be interrupted (e.g. {!Unix.select}).

    {b TODO.} In OCaml because of checkpoints, are we immune to race conditions
    between {!recv_command} and infinitely blocking [select()] ? 
*)

val send_command : int -> command -> [ `Ok | `Error of string | `Not_found ]
(** [send_command pid c] sends the command [c] to the daemon with process 
    ID [pid]. [`Error _] is returned in case of UNIX errors, `Not_found is 
    returned if no process with ID [pid] exists. *)


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
