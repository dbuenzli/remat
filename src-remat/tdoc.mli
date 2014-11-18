(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Newspaper Textual document model.

    In the textual document model we start from the physical document
    model and try to collect related text blocks into indexable
    {{!doc_unit}units} of semantically contiguous text.
    These units may span more than one page.

    All styling information is lost but we keep the physical location
    of the words and words blocks on the pages for hilighting search
    results and identifiying document units on pages in the IR
    system. *)

(** {1 Textual model} *)

(*
type word =
    { w_rect : Rect.t;
      w_hyphenated : (int * Rect.t) option;
      w_font : font;
      w_lang : lang option;
      w_confidence : float option;
      w_chars_confidence : char_confidence option;
      w_chars : string;
      w_chars_alternatives : string list; }


    If the word is hyphenated, the index of the first char on the next
    line and the remaining rectangle is in [w_hyphenated] (these rectangles
    may not be on the same page). For [w_confidence]
    we use ALTO style from [0.] (unsure) to [1.] (sure). *)


type word =
    { w_page : int;
      w_rect : Rect.t;
      w_hyphenated : (int * Rect.t) option;
      w_confidence : float option;
      w_chars : string;
      w_chars_alternatives : string list; }
(** The type for words. *)

type text_block =
    { tb_page : int;
      tb_rect : Rect.t;
      tb_words : word list }
(** The type for rectangular blocks of words. *)

type article =
    { a_title : text_block list;
      a_content : text_block list;
      a_langs  : string list }
(** The type for articles. *)

type picture =
    { p_page : int;
      p_rect : Rect.t }
(** The type for pictures. *)

type doc_unit = [
  | `Header of text_block list
  | `Footer of text_block list
  | `Unknown of text_block list
  | `Picture of Rect.t * int
  | `Article of article ]
(** The type for textual model units. *)

type t = doc_unit list
(** The type for the textual model. *)

(** {1 IO} *)

val input : in_channel -> [ `Ok of t | `Error ]
(** [input ic] inputs a textual document model. *)

val output : out_channel -> t -> unit
(** [output oc d] outputs [d] on [oc]. *)

(** {1 Conversions} *)

val of_pdoc : ?log:('a, Format.formatter, unit, unit) Pervasives.format4 -> 'a
  -> Pdoc.t -> t
(** [of_pdoc log pd] is the textual model of the physical model [pd].
    The [log] function reports found problems. *)


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
