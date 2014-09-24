(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** TTY user interface.

    [Ui] handles a TTY user interface with a bottom status part and
    an upper window for logging messages. *)

(** {1 TTY UI} *)

val set_formatter : Format.formatter -> unit
(** [set_formatter] sets the formatter on which the ui is displayed
    (defaults to [Format.std_formatter]). *)

val set_verbosity : [`Quiet | `Error | `Normal ] -> unit
(** [set_verbosity v] sets the UI verbosity. With  [`Quiet] nothing
    is ever displayed on the formatter. With [`Error], only calls
    to {!log_err} are displayed. With [`Normal] the full UI is
    displayed. *)

val log : ('a, Format.formatter, unit, unit) format4 -> 'a
(** [log fmt], logs a message on the UI. *)

val log_err : ('a, Format.formatter, unit, unit) format4 -> 'a
(** [log_err fmt], logs an error message on the UI. *)

val show_mapf : (Format.formatter -> (string * 'a) -> unit) ->
  ((string * 'a) -> unit) -> 'a Mapf.t -> unit
(** [show_mapf pp action m] shows the progress of [m] on the status line
    with [pp], action is also with each result. *)

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
