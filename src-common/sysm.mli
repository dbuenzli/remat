(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%NAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

(** System abstraction.

    All [Unix] module usage is confined to this module. *)

(** {1 Environment} *)

val getenv : string -> string option
(** [getenv key] is the value of [key] in the process environment (if any). *)

(** {1 File system operations} *)

val mkdir : string -> bool
(** [mkdir d] creates the directory [d] and returns [true]. If an error
    occurs, it is logged and [false] is returned. Intermediary directories
    are created as required. *)

val rename : string -> string -> bool
(** [rename oldf newf] renames [oldf] to [newf] and returns [true]. If an error
    occurs, it is logged and [false] is returned. *)

val tmp_file : string -> string option
(** [tmp_file f] is a temporary file in the same location as [f] and
    named after [f]. If an error occurs it is logged and [None] is returned. *)

(** {1 Finding files} *)

val make_relative : string -> string -> string
(** [make_relative p f] is [f] without the prefix [p] or [f] if
    [p] is not a prefix of [f]. *)

val find_file : string list -> string -> string option
(** [find_file dirs file] is the full path to [file] in the first
    directory of [dirs] in which it can be found or None otherwise.  *)

val find_file_rec : string list -> string -> string option
(** [find_file_rec dirs file] is like {!find_file} but sub directories
    are also searched in depth first order. *)

val fold_files_rec : string list -> ('a -> string -> 'a) -> 'a -> 'a
(** [fold_files_rec dirs f acc] lists the files in [dirs], recursively
    in depth first order and folds the function [f] over the file names. *)

(** {1 IO brackets} *)

val with_inf : string -> 'a -> (in_channel -> 'a) -> 'a
(** [with_inf inf err f] is [f ic] where [ic] is an input channel
    opened and closed on [inf] ([stdin] and not closed if [f] is ["-"]).
    If a [Sys_error] occurs it is logged and [err] is returned. *)

val with_outf : string -> 'a -> (out_channel -> 'a) -> 'a
(** [with_outf outf err f] is [f oc] where [oc] is an output channel
    opened and closed on [outf] ([stdout] and not closed if [f] is ["-"]).
    If a [Sys_error] occurs it is logged and [err] is returned. *)

val with_outf_pp : string ->
  ('a, Format.formatter, unit, unit) Pervasives.format4 -> 'a
(** [with_outf_pp outf fmt a0 a1 ...] formats [a0], [a1], ...
    according to [fmt] on a formatter opened and closed on [outf]
    ([stdout] and not closed if [f] is ["-"]).
    If a [Sys_error] occurs it is logged. *)

val with_out_path : string -> 'a -> (out_channel -> 'a) -> 'a
(** [with_out_path] is like {!with_outf} except the path is created if it
    doesn't exist. An error is logged if the path cannot be created. *)

(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli.
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

   3. Neither the name of the Daniel C. Bünzli nor the names of
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
