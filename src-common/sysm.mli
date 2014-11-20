(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Executing commands and IO operations. *)

(** {1:command_results Command and IO results} *)

type 'a result = [ `Ok of 'a | `Error of string ]
(** The type for command and IO results. *)

val ret : 'a -> 'a result
(** [ret v] is [`Ok v]. *)

val error : string -> 'a result
(** [error e] is [`Error e]. *)

val bind : 'a result -> ('a -> 'b result) -> 'b result
(** [bind r f] is [f v] if [r = `Ok v] and [r] if [r = `Error _]. *)

val map : 'a result -> ('a -> 'b) -> 'b result
(** [map r f] is [bind r (fun v -> ret (f v))]. *)

val get : 'a result -> 'a
(** [get r] is [v] if [r = `Ok v] and @raise Invalid_argument otherwise. *)

val on_error : ?level:Log.level -> use:'a -> 'a result -> 'a
(** [on_error ~level ~use r] is:
    {ul
    {- [v] if [r = `Ok v]}
    {- [use] if [r = `Error msg]. As a side effect [msg] is
       {{!Log}logged} with level [level] (defaults to {!Log.Error})}} *)

val ignore_error : use:'a -> 'a result -> 'a
(** [ignore_error ~use r] is like {!on_error} but the error
    is not logged. *)

val reword_error : ?replace:bool -> string -> 'a result -> 'a result
(** [reword_error msg r] uses [msg] for the error message in case of
    [`Error]. If replace is [false] (default), [msg] is stacked on
    top of the old message. *)

val exn_error : ?msg:(Printexc.raw_backtrace -> exn -> 'a -> string) ->
  (('a -> 'b) -> ('a -> 'b result))

val ( >>= ) : 'a result -> ('a -> 'b result) -> 'b result
(** [r >>= f] is [bind r f]. *)

val ( >>| ) : 'a result -> ('a -> 'b) -> 'b result
(** [r >>| f] is [map r f]. *)

(** Infix operators.

    Gathers {!Sysm}'s infix operators. *)
module Infix : sig

  (** {1 Infix operators} *)

  val ( >>= ) : 'a result -> ('a -> 'b result) -> 'b result
  (** [(>>=)] is {!Cmd.( >>= )}. *)

  val ( >>| ) : 'a result -> ('a -> 'b) -> 'b result
  (** [(>>|)] is {!Cmd.( >>| )}. *)
end

(** {1:io IO and file system operations} *)

type path = Path.t

(** Path operations. *)
module Path : sig

  (** {1:pathops Path operations} *)

  val exists : ?err:bool -> path -> bool result
  (** [exists path] is [true] iff [path] exists.
      If [err] is [true] (defaults to [false]) an error is returned
      when the file doesn't exist. *)

  val move : ?force:bool -> path -> path -> unit result
  (** [move ~force src dst] moves path [src] to [dst]. If [force] is
      [false] (default) the operation fails if [dst] exists. *)
end

(** File operations. *)
module File : sig

  (** {1:fileops File operations}

      {b Note.} When paths are {{!Path.rel}relative} they are expressed
      relative to the {{!Dir.getcwd}current working directory}. *)

  val exists : ?err:bool -> path -> bool result
  (** [exists file] is [true] iff [file] exists and is not a directory.
      If [err] is [true] (defaults to [false]) an error is returned
      when the file doesn't exist. *)

  val dev_null : path
  (** [dev_null] represents a file that discards all writes. *)

  val delete : ?maybe:bool -> path -> unit result
  (** [delete ~maybe file] deletes file [file]. If [maybe] is [false]
      (default) no error is returned if the file doesn't exit. *)

  val temp : string -> path result
  (** [temp suffix] creates a temporary file with suffix [suffix] and returns
      its name. The file is destroyed at the end of program execution. *)

  (** {1:input Input} *)

  val with_inf : (in_channel -> 'a -> 'b result) -> path -> 'a ->
    'b result
  (** [with_inf f inf v] opens [inf] as a channel [ic] and returns [f
      ic v] if no error occurs. In case of error the channel is closed
      and the error is returned. If [inf] is {!Path.dash}, [ic] is
      {!Pervasives.stdin} and not closed. *)

  val read : path -> string result
  (** [read file] is [file]'s content. If [file] is {!Path.dash} reads
      from {!Pervasives.stdin}. *)

  val read_lines : path -> string list result
  (** [read_lines file] is [file]'s content splitted at ['\n']. If
      [file] is {!Path.dash} reads from {!Pervasives.stdin}. *)

  (** {1:output Output} *)

  val with_outf : (out_channel -> 'a -> 'b result) -> path -> 'a ->
    'b result
  (** [with_inf f outf v] opens [outf] as a channel [oc] and returns
      [f oc v] if no error occurs. In case of error the channel is
      closed and the error is returned. If [outf] is {!Path.dash}, [oc] is
      {!Pervasives.stdout} and not closed. *)

  val write : path -> string -> unit result
  (** [write file content] outputs [content] to [file]. If [file]
      is {!Path.dash}, writes to {!Pervasives.stdout}. If an error is
      returned [file] is left untouched except if {!Pervasives.stdout}
      is written.*)

  val write_lines : path -> string list -> unit result
  (** [write_lines file lines] outputs [lines] separated by ['\n'] to
      [file]. If [file] is {!Path.dash}, writes to {!Pervasives.stdout}.
      If an error is returned [file] is left untouched except if
      {!Pervasives.stdout} is written.*)

  val write_subst : (string * string) list -> path -> string -> unit result
  (** [write_subst vars file content] outputs [content] to [file]. In
      [content] patterns of the form ["%%ID%%"] are replaced by the value
      of [List.assoc "ID" vars] (if any). If [file] is {!Path.dash}, writes
      to {!Pervasives.stdout}. If an error is returned [file] is left
      untouched except if {!Pervasives.stdout} is written. *)
end

(** Directory operations. *)
module Dir : sig

  (** {1:dirops Directory operations}

  {b Note.} When paths are {{!Path.rel}relative} they are expressed
  relative to the {{!Dir.getcwd}current working directory}. *)

  val exists : ?err:bool -> path -> bool result
  (** [exists dir] is [true] if directory [dir] exists.
      If [err] is [true] (defaults to [false]) an error is returned
      when the file doesn't exist. *)

  val getcwd : unit -> path result
  (** [getcwd ()] is the current working directory. *)

  val chdir : path -> unit result
  (** [chdir dir] changes the current working directory to [dir]. *)

  val fold_files_rec : ?skip:string list -> (string -> 'a -> 'a result) ->
    'a -> string list -> 'a result
  (** [fold_files_rec skip f acc paths] folds [f] over the files
      found in [paths]. Files and directories whose suffix matches an
      element of [skip] are skipped. {b FIXME} this should be using
      {!Path.t} and {!Path.ext}. *)
end

(** {1:env_lookup Environment variables lookup} *)

val env : string -> string option
(** [env var] is the value if the environment variable [var], if
    defined. *)

val get_env : string -> string result
(** [get_env var] is like {!env} but returns an error if [var] is
    undefined. *)

(** {1:executing_commands Executing commands} *)

val exists : ?err:bool -> string -> bool result
(** [exists cmd] is [true] if [cmd] exists and can be invoked.
    If [err] is [true] (defaults to [false]) an error is returned
    when the command doesn't exist. *)

val exec_ret : string -> string list -> int
(** [exec_ret cmd args] executes [cmd] with arguments [args] and
    returns the exit code of the invocation. *)

val exec : string -> string list -> unit result
(** [exec cmd args] executes [cmd] with arguments [args]. On exit
    code [0] returns [`Ok ()]. Otherwise an error message with
    the failed invocation and its exit code is returned in [`Error]. *)

val exec_read : ?trim:bool -> string -> string list -> string result
(** [exec_read cmd args] execute [cmd] with arguments [args] and returns
    its standard output. If [cmd]'s return code is non zero returns
    an error message. If [trim] is [true] (default) the contents is
    passed to {!String.trim} before being returned. *)

val exec_read_lines : string -> string list -> string list result
(** [exec_readl_lines cmd args] is like [input ~trim:false cmd args] but
    the input is splitted at ['\n']. *)

val exec_write : string -> string list -> path -> unit result
(** [exec_write cmd args file] execute [cmd] with arguments [args] and writes
    the invocation's [stdout] to [file]. In [cmd]'s return code is non
    zero ryeturns an error message and [file] is left intact. *)


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
