(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Logging module

    {b Warning.} Not thread safe. *)

(** {1 Logging} *)

type level = [ `Debug | `Error | `Info | `Warning ]
(** The type for logging levels. *)

type 'a level_log = ('a, Format.formatter, unit, unit) Pervasives.format4 -> 'a
(** The type for a log level. *)

type 'a log = level -> 'a level_log
(** The basic type for logs. *)

(** Module to open.

    Defines a type for first class value logs. It's also a module
    that can be opened in order to be able to access the fields without
    prefixing. *)
module Log : sig
  type t =
    { log : 'a. 'a log;
      err : 'a. 'a level_log;
      warn : 'a. 'a level_log;
      info : 'a. 'a level_log;
      debug : 'a. 'a level_log; }
    (** The type for first class logs. *)
end

type t = Log.t
(** The type for first class logs. *)

val log : 'a log
(** [log l fmt a1 a2 ...] logs a level [l] message with the format string
    [fmt] and its arguments [a1 a2 ...] *)

val err : 'a level_log
(** [err] is [log `Error]. *)

val warn : 'a level_log
(** [warn] is [log `Warn]. *)

val info : 'a level_log
(** [info] is [log `Info]. *)

val debug : 'a level_log
(** [debug] is [log `Debug]. *)

val default_log : t
(** [default_log] is the first-class log corresponding to {!log}. *)

(** {1 Reporting} *)

type verbosity = [ `Verbose | `Normal | `Error | `Quiet ]
(** The type for log reporting verbosity.
    {ul
    {- [`Verbose] logs all levels.},
    {- [`Normal] logs the levels [`Error], [`Warning] and [`Info].}
    {- [`Error] logs the levels [`Error].}
    {- [`Quiet] logs nothing.}}
*)

type reporter = { report : 'a. 'a log }
(** The type for log message reporters. *)

val set_reporter : reporter -> unit
(** [set_reporter r] sets the log reporter to [r]. Initially, defaults
    to {!nil_reporter}. *)

val nil_reporter : reporter
(** [nil_reporter] discards all entries. *)

val formatter_reporter : verbosity -> Format.formatter -> reporter
(** [formatter_reporter verbosity ppf] reports on [ppf]
    according to [verbosity]. *)

val timed_formatter_reporter : verbosity -> Format.formatter -> reporter
(** [timed_formatter_reporter verbosity ppf] reports on [ppf]
    according to [verbosity] with timing information. *)


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
