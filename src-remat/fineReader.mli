(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Abbyy FineReader XML OCR data.

    [FineReader] parses the {{:http://www.abbyy.com}ABBYY} FineReader
    XML OCR data structure. The data structure closely models the XML
    structure but it's rather inefficient.

  {b References.}
  {ul
    {- {{:http://www.abbyy.com/FineReader_xml/FineReader6-schema-v1.xml}
    The FineReader 6v1 schema}}} *)

(** {1 FineReader types} *)

type rect = { l : int; t : int; r : int; b : int }
(** The type for FineReader rectangles. *)

val pp_rect : Format.formatter -> rect -> unit
(** [pp_rect ppf r] prints an unspecified representation of [r] on
    [ppf]. *)

type char_params =
    { char_rect : rect;
      suspicious : bool;
      proofed : bool;
      word_start : bool option;
      word_from_dictionary : bool option;
      word_normal : bool option;
      word_numeric : bool option;
      word_identifier : bool option;
      char_confidence : int option;
      serif_probability : int option;
      word_penalty : int option;
      mean_stroke_width : int option;
      char : string }
(** The type for FineReader character parameters. *)

val pp_char_params : Format.formatter -> char_params -> unit
(** [pp_char_params ppf cp] prints an unspecified representation of [cp] on
    [ppf]. *)


type formatting =
    { lang : string;
      font_family : string option;
      font_size : float option;
      bold : bool;
      italic : bool;
      subscript : bool;
      superscript : bool;
      smallcaps : bool;
      underline : bool;
      strikeout : bool;
      color : int;
      scaling : int;
      spacing : int;
      chars : char_params list }
(** The type for FineReader formatting. *)

val pp_formatting : Format.formatter -> formatting -> unit
(** [pp_formatting ppf f] prints an unspecified representation of [f] on
    [ppf]. *)

type line =
    { baseline : int;
      line_rect : rect;
      formattings : formatting list }
(** The type for FineReader lines *)

val pp_line : Format.formatter -> line -> unit
(** [pp_line ppf l] prints an unspecified representation of [l] on
    [ppf]. *)

type par =
    { drop_cap_chars_count : int;
      drop_cap_rect : rect option;
      align : [`Left | `Center | `Right | `Justify];
      left_indent : int;
      right_indent : int;
      start_indent : int;
      line_spacing : int;
      lines : line list }
(** The type for FineReader paragraphs. *)

val pp_par : Format.formatter -> par -> unit
(** [pp_par ppf p] prints an unspecified representation of [p] on
    [ppf]. *)

type text =
    { orientation : [`Normal | `Rotated_cw | `Rotated_upsidedown |
                     `Rotated_ccw ];
      background_color : int;
      mirrored : bool;
      inverted : bool;
      paragraphs : par list; }
(** The type for FineReader text. *)

val pp_text : Format.formatter -> text -> unit
(** [pp_text ppf t] prints an unspecified representation of [t] on
    [ppf]. *)


type cell_border_type = [`Absent |`Unknown | `White | `Black]
(** The type for FineReader table cell border types. *)

type cell =
    { cell_text : text list;
      col_span : int;
      row_span : int;
      cell_align : [ `Top | `Center | `Bottom ];
      picture : bool;
      left_border : cell_border_type;
      top_border : cell_border_type;
      right_border : cell_border_type;
      bottom_border : cell_border_type;
      cell_width : int;
      cell_height : int; }
(** The type for FineReader table cells. *)

(*
val pp_cell : Format.formatter -> cell -> unit
*)

type row = cell list
(** The type for FineReader table rows. *)

(*
val pp_row : Format.formatter -> row -> unit
*)

type block =
    { block_type : [`Text of text | `Table of row list | `Picture | `Barcode];
      block_rect : rect;
      block_region : rect list; }
(** The type for a FineReader block. *)

val pp_block : Format.formatter -> block -> unit
(** [pp_block ppf b] prints an unspecified representation of [b] on
    [ppf]. *)

type page =
    { width : int;
      height : int;
      resolution : int;
      original_coords : bool;
      blocks : block list }
(** The type for a FineReader page. *)

val pp_page : Format.formatter -> page -> unit
(** [pp_page ppf p] prints an unspecified representation of [p] on
    [ppf]. *)

type document =
    { version : string;
      producer : string;
      pages_count : int;
      main_language : string option;
      languages : string option;
      pages : page list }
(** The type for a FineReader document. *)

val pp_document : Format.formatter -> document -> unit
(** [pp_document ppf d] prints an unspecified representation of [d] on
    [ppf]. *)

(** {1 Input} *)

val input : ?err:(Xmlm.pos -> string -> unit) -> Xmlm.source -> document
(** [input err src] reads a FineReader document from [src]. Errors
    are reported on [err].

    {b Raises} [Xmlm.Error] on XML well formedness errors. *)

(** {1 Data dumps} *)

val ascii_art : Format.formatter -> document -> unit
(** [ascii_art ppf d] prints selected part of [d] as ASCII art on [ppf]. *)


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
