(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Formatters. *)

(** {1 Formatters} *)

type 'a formatter = Format.formatter -> 'a -> unit
(** The type for formatters of values of type ['a]. *)

val pp : Format.formatter -> ('a, Format.formatter, unit) Pervasives.format ->
  'a
(** [pp] is {!Format.fprintf} *)

val rpp : ('a, Format.formatter, unit) Pervasives.format ->
  Format.formatter -> 'a
(** [rpp] is [pp fmt ppf] *)

val nop : 'a formatter
(** [nop] does nothing. *)

val pp_cut : unit formatter
(** [pp_cut] is {!Format.pp_print_cut}. *)

val pp_sp : unit formatter
(** [pp_sp] is {!Format.pp_print_space}. *)

val pp_str : string formatter
(** [pp_str] is {!Format.pp_print_string}. *)

val pp_int : int formatter
(** [pp_int] is {!Format.pp_print_int}. *)

val pp_bool : bool formatter
(** [pp_bool] is {!Format.pp_print_bool}. *)

val pp_larrow : unit formatter
  (** [pp_larrow] formats a left arrow. *)

val pp_rarrow : unit formatter
(** [pp_rarrow] formats a right arrow. *)

val pp_opt : ?pp_none:unit formatter -> 'a formatter -> 'a option formatter
(** [pp_opt pp_none pp_v] formats value of type ['a option]. The default
    value of [pp_none] prints nothing. *)

val pp_list : ?pp_sep:unit formatter -> 'a formatter -> 'a list formatter
(** [pp_list pp_sep pp_v] formats lists of type ['a]. Each value
    is printed with [pp_v] followed by [pp_sep] (defaults to {!pp_cut}).
    Empty lists never print anything. *)

val pp_text : string formatter
(** [pp_text] formats text by replacing spaces and newlines in the string
    with calls to {!Format.pp_print_space} and {!Format.pp_force_newline}. *)

val pp_lines : string formatter
(** [pp_lines] formats lines by replacing newlines in the string
    with calls to {!Format.pp_force_newline}. *)

val pp_range : ((int * int) * (int * int)) formatter
(** [pp_range] formats a range. *)

val pp_doomed : string formatter
(** [pp_doomed] should be used for printing a message when reasonable
    assumptions are being violated. The string should be a short
    description of what is going on. *)

(** {1:utf8_cond Conditional UTF-8 formatting}

    {b Note.} Since {!Format} is not UTF-8 aware using UTF-8 output
    may derail the pretty printing process. *)

val pp_if_utf_8 : 'a formatter -> 'a formatter -> 'a formatter
(** [pp_if_utf_8 pp_u pp] is a formatter that will use [pp_u] if UTF-8
    output is {{!utf_8_enabled}enabled} and [pp] otherwise. *)

(** {2:utf8_cond Conditional UTF-8 formatting control} *)

val utf_8_enabled : unit -> bool
(** [utf_8_enabled ()] is [true] if UTF-8 pretty-printing is enabled. *)

val set_utf_8_enabled : bool -> unit
(** [set_utf_8_enabled b] sets UTF-8 pretty-printing to [b]. *)

(** {1:styled Styled formatting} *)

type style =
  [ `Bold
  | `Underline
  | `Black
  | `Red
  | `Green
  | `Yellow
  | `Blue
  | `Magenta
  | `Cyan
  | `White
  | `None ]
(** The type for styles. *)

val pp_styled : style -> 'a formatter -> 'a formatter
(** [pp_styled style pp] formats according to [pp] but styled with [style]. *)

val pp_styled_str : style -> string formatter
(** [pp_styled_str style] is [pp_styled_str style pp_str]. *)

(** {2 Styled formatting control} *)

type style_tags = [ `Ansi | `None ]
(** The type for style tags.
    {ul
    {- [`Ansi], tags the text with
       {{:http://www.ecma-international.org/publications/standards/Ecma-048.htm}
           ANSI escape sequences}.}
    {- [`None], text remains untagged.}} *)

val style_tags : unit -> style_tags
(** [style_tags ()] is the current tag style used by {!Fmt.pp_styled}.
    Initial value is [`None]. *)

val set_style_tags : style_tags -> unit
(** [set_style_tags s] sets the current tag style used by
    {!Fmt.pp_styled}. *)

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
