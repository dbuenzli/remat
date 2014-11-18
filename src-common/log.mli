(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Logging. *)

(** {1 Log level} *)

(** The type for log levels. *)
type level = Show | Error | Warning | Info | Debug

val msg : ?header:string -> level ->
  ('a, Format.formatter, unit, unit) format4 -> 'a
(** [msg header l fmt ...] logs a message with level [l]. [header] is
      the message header, default depends on [l]. *)

val kmsg : ?header:string ->
  (unit -> 'a) -> level -> ('b, Format.formatter, unit, 'a) format4 -> 'b
(** [kmsg header k l fmt ...] is like [msg header l fmt] but calls [k ()]
    before returning. *)

val show : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** [show fmt ...] logs a message with level [Show]. [header] defaults
    to [None]. *)

val err : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** [err fmt ...] logs a message with level [Error]. [header] defaults
    to ["ERROR"]. *)

val warn : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** [warn fmt ...] logs a message with level [Warning]. [header] defaults
    to ["WARNING"]. *)

val info : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** [info fmt ...] logs a message with level [Info]. [header] defaults
    to ["INFO"]. *)

val debug : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** [debug info ...] logs a message with level [Debug]. [header] defaults
    to ["DEBUG"]. *)

(** {1 Log level and output} *)

val level : unit -> level option
(** [level ()] is the log level (if any). If the log level is [(Some l)]
    any message whose level is [<= l] is logged. If level is [None]
    no message is ever logged. Initially the level is [(Some Warning)]. *)

val set_level : level option -> unit
(** [set_level l] sets the log level to [l]. See {!level}. *)

val set_formatter : [`All | `Level of level ] -> Format.formatter -> unit
(** [set_formatter spec ppf] sets the formatter for a given level or
    for all the levels according to [spec]. Initially the formatter
    of level [Show] is {!Format.std_formatter} and all the other level
    formatters are {!Format.err_formatter}. *)

(** {1 Log monitoring} *)

val err_count : unit -> int
(** [err_count ()] is the number of messages logged with level [Error]. *)

val warn_count : unit -> int
(** [warn_count ()] is the number of messages logged with level
    [Warning]. *)

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
