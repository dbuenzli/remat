(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Physical document model.

    This physical model is influenced by what FineReader
    {{!FineReader}gives us} and the
    {{:http://www.loc.gov/standards/alto/}ALTO} standard.

    Font information is only attached to chars. We use a coordinate
    system whose origin is at the bottom left of the page (this
    convention is more intuitive when working with what humans think
    of the bottom of a rectangle).

    At the line level we group characters into character blocks to
    which OCR textual level word meta data may be attached. Character
    blocks are separated by space which is not recorded (since it can
    be inferred).

    Implementation-wise we systematically use hash-consing to reduce
    memory requirements (in IO aswell).  *)

(** {1 Hash-consed strings, attributes and styles}

    Strings, attributes and styles are hash-consed to reduce memory
    requirements. *)

val string : string -> string
(** [string s] is [s] hash-consed.

    {b Important.} We assume strings are never mutated. *)

type color = float * float * float
(** The type for colors. *)

type lang = string
(** The type for languages
    ({{:http://tools.ietf.org/html/bcp47}BCP 47} language tags). N.B.
    the standard tag for undefined is ["und"]. *)

type font = private
      { ts_font_family : string option;
        ts_font_size : float option;
        ts_font_type : [ `Serif | `Sans_serif ] option;
        ts_font_width : [ `Fixed | `Proportional ] option;
        ts_font_color : color option;
        ts_font_styles :
          [ `Bold | `Italics | `Subscript | `SuperScript | `Smallcaps |
          `Underline | `Strikeout] list; }
(** The type for fonts (modeled after ALTO's TextStyle). *)

type text_block_style = private
      { tbs_align : [`Left | `Right | `Center | `Justify] option;
        tbs_left_indent : float option;
        tbs_right_indent : float option;
        tbs_first_line_indent : float option;
        tbs_line_space : float option; }
(** The type for text block styles (modeled after ALTO's ParagraphStyle). *)

(** {1 Structure} *)

type confidence = float
(** The type for character or word confidence. From [0.], unsure to
    [1.], sure. *)

type char =
    { c_rect : Rect.t;
      c_font : font;
      c_confidence : confidence option;
      c_char : string;
      c_alternatives : string list }
(** The type for characters. *)

type char_block =
    { cb_rect : Rect.t;
      cb_word_lang : lang;
      cb_word_confidence : confidence option;
      cb_word_in_dict : bool option;
      cb_word_alternatives : string list;
      cb_chars : char list }
(** The type for character blocks.

    A character block is a sequence of characters not separated
    by white space. OCR processes may attach textual, word level,
    meta information to a character block, these informations are
    available under the [cb_word_*] fields. *)

type line =
    { l_rect : Rect.t;
      l_baseline : float option;
      l_hyphen : Rect.t option;
      l_char_blocks : char_block list; }
(** The type for lines.

    A line is a sequence of character blocks. If the line is hyphenated
    l_hypen holds the hypen rectangle. *)

type text_block =
    { tb_rect : Rect.t;
      tb_style : text_block_style;
      tb_lines : line list; }
(** The type for rectangular blocks of text.

    A text block is a sequence of lines. *)

type block = [
  | `Text of text_block
  | `Blocks of Rect.t * block list
  | `Picture of Rect.t ]
(** The type for blocks. *)

type page =
    { p_width : float;
      p_height : float;
      p_resolution : int; (** dpi *)
      p_blocks : block list }
(** The type for pages. *)

type t = page list
(** The type for documents. *)

(** {1 Input / Output} *)

val input : in_channel -> [ `Ok of t | `Error ]
(** [input ic] inputs a physical document model.

    {b Raises} [End_of_file] is raised if an end of file was reached.
    {b Note.} TODO. This function use OCaml's type unsafe
    marshal functions. A versioned magic number is first read to make
    sure we try to read a document model. But still, if the remaining
    data is corrupted this may lead to a segfault. *)

val output : out_channel -> t -> unit
(** [output oc d] outputs [d] on [oc]. *)

(** {1 Recognized Text}

    Pretty printers to output the OCR textual data in UTF-8. Pages are
    separated by the form feed character ([U+000C]), text blocks are
    separated by blank lines ([U+000A], [U+000A]), lines are separated
    by a new line [U+000A], character blocks are separated by spaces
    ([U+0020]). Hyphenated lines have a hyphen character ([U+2010]) at
    the end.*)

val pp_txt_char : Format.formatter -> char -> unit
val pp_txt_char_block : Format.formatter -> char_block -> unit
val pp_txt_line : Format.formatter -> line -> unit
val pp_txt_text_block : Format.formatter -> text_block -> unit
val pp_txt_block : Format.formatter -> block -> unit
val pp_txt_page : Format.formatter -> page -> unit
val pp_txt : Format.formatter -> t -> unit

(** {1 Converting from FineReader}

    Some points to take into account.
    {ul
    {- block_region is lost ? Is it useful ? }
    {- Except for hyphenated words we assume that a word never spans two
       FineReader [formatting] elements. If that happens for a non-hyphenated
       a message is logged.}} *)

type fineReader_conversion_msg = [ `Unknown_lang of string ]

(** The type for FineReader conversion messages. *)

val pp_fineReader_conversion_msg : Format.formatter ->
  fineReader_conversion_msg -> unit
(** [pp_fineReader_conversion_msg ff m] prints a textual representation
    of [m] on [ff]. *)

val of_fineReader : ?log:(fineReader_conversion_msg -> unit) ->
  FineReader.document -> t
(** [of_fineReader log fr] is the physical model of FineReader's document
    [fr]. [log] reports messages about the translation. *)

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
