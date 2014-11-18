(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Hash-conses the given type. *)

module HashCons (T : sig type t end) : sig
  val create : T.t -> T.t
end = struct
  module Hashed = struct
    type t = T.t
    let equal : t -> t -> bool = ( = )
    let hash : t -> int = Hashtbl.hash
  end
  module H = Weak.Make (Hashed)
  let index = H.create 89
  let create v = H.merge index v
end

module HString = HashCons (struct type t = string end)
let string = HString.create

(* Attributes and styles. *)

type color = float * float * float
type lang = string
let lang = string

type font =
    { ts_font_family : string option;
      ts_font_size : float option;
      ts_font_type : [ `Serif | `Sans_serif ] option;
      ts_font_width : [ `Fixed | `Proportional ] option;
      ts_font_color : color option;
      ts_font_styles :
        [ `Bold | `Italics | `Subscript | `SuperScript | `Smallcaps |
        `Underline | `Strikeout ] list; }

module Font = HashCons (struct type t = font end)
let font                                          (* hash-consed constructor. *)
    ts_font_family ts_font_size ts_font_type ts_font_width ts_font_color
    ts_font_styles =
  Font.create { ts_font_family; ts_font_size; ts_font_type; ts_font_width;
                ts_font_color; ts_font_styles; }

type text_block_style =
    { tbs_align : [`Left | `Right | `Center | `Justify] option;
      tbs_left_indent : float option;
      tbs_right_indent : float option;
      tbs_first_line_indent : float option;
      tbs_line_space : float option; }

module Text_block_style = HashCons (struct type t = text_block_style end)
let text_block_style                              (* hash-consed constructor. *)
    tbs_align tbs_left_indent tbs_right_indent tbs_first_line_indent
    tbs_line_space =
  Text_block_style.create { tbs_align; tbs_left_indent; tbs_right_indent;
                            tbs_first_line_indent; tbs_line_space; }

(** Structure *)

type confidence = float
type char =
    { c_rect : Rect.t;
      c_font : font;
      c_confidence : confidence option;
      c_char : string;
      c_alternatives : string list }

type char_block =
    { cb_rect : Rect.t;
      cb_word_lang : lang;
      cb_word_confidence : confidence option;
      cb_word_in_dict : bool option;
      cb_word_alternatives : string list;
      cb_chars : char list }

type line =
    { l_rect : Rect.t;
      l_baseline : float option;
      l_hyphen : Rect.t option;
      l_char_blocks : char_block list; }

type text_block =
    { tb_rect : Rect.t;
      tb_style : text_block_style;
      tb_lines : line list; }

type block = [
  | `Text of text_block
  | `Blocks of Rect.t * block list
  | `Picture of Rect.t ]

type page =
    { p_width : float;
      p_height : float;
      p_resolution : int; (** dpi *)
      p_blocks : block list }

type t = page list

(* Input / Output *)

let magic = "lpdoc00"
let output o d = output_string o magic; output_char o '\n'; output_value o d
let input i = if input_line i = magic then (`Ok (input_value i)) else `Error

(* Recognized text *)

let pp = Format.fprintf
let pp_char = Format.pp_print_char
let pp_float = Format.pp_print_float
let pp_cut = Format.pp_print_cut
let pp_sp = Format.pp_print_space
let pp_str = Format.pp_print_string
let pp_blank_ln ff () = pp_cut ff (); pp_cut ff ()
let pp_opt ?(pp_none = fun ppf () -> ()) pp_v ppf = function
  | None -> pp_none ppf () | Some v -> pp_v ppf v

let rec pp_list ?(pp_sep = pp_cut) pp_v ff = function [] -> ()
  | v :: vs ->
      pp_v ff v; if vs <> [] then (pp_sep ff (); pp_list ~pp_sep pp_v ff vs)

let pp_txt_char ff c = pp_str ff c.c_char
let pp_txt_char_block ff cb =
  pp_list ~pp_sep:(fun _ () -> ()) pp_txt_char ff cb.cb_chars

let pp_txt_line ff l =
  let pp_sep ff () = pp_char ff ' ' in
  pp_list ~pp_sep pp_txt_char_block ff l.l_char_blocks;
  if l.l_hyphen <> None then pp_str ff "\xE2\x80\x90" (* U+2010 *)

let pp_txt_text_block ff tb =
  let pp_sep = Format.pp_force_newline in
  pp_list ~pp_sep pp_txt_line ff tb.tb_lines

let rec pp_txt_block ff = function                               (* not t.r. *)
  | `Text tb -> pp_txt_text_block ff tb
  | `Blocks (_, bs) -> pp_list ~pp_sep:pp_blank_ln pp_txt_block ff bs
  | `Picture _ -> ()

let pp_txt_page ff p =
  let pp_sep ff () = pp_cut ff (); pp_cut ff () in              (* blank line *)
  pp ff "@[<v>%a@]" (pp_list ~pp_sep pp_txt_block) p.p_blocks

let pp_txt ff pages =
  let pp_sep ppf () = pp_cut ff (); pp_char ff '\x0C'; pp_cut ff () in
  pp ff "@[<v>%a@]" (pp_list ~pp_sep pp_txt_page) pages

(* FineReader *)

type fineReader_conversion_msg = [ `Unknown_lang of string ]

let pp_fineReader_conversion_msg ff = function
  | `Unknown_lang l -> pp ff "@[unknown language: `%s'@]" l

let lang_of_fr_language log = function
  | "FrenchStandard" -> (lang "fr")
  | "ItalianStandard" -> (lang "en")
  | "EnglishUnitedStates" -> (lang "en-us")
  | l -> log (`Unknown_lang l); (lang "und")

let font_of_fr_formatting f =
  let open FineReader in
  let ts_font_family = match f.font_family with
  | Some ff -> Some (string ff) | None -> None
  in
  let ts_font_size = f.font_size in
  let ts_font_type = None in
  let ts_font_width = None in
  let ts_font_color = let c = float f.color in Some (c, c, c) in
  let ts_font_styles =
    let bold acc = if f.bold then `Bold :: acc else acc in
    let italic acc = if f.italic then `Italics :: acc else acc in
    let subscript acc = if f.subscript then `Subscript :: acc else acc in
    let superscript acc = if f.superscript then `SuperScript :: acc else acc in
    let smallcaps acc = if f.smallcaps then `Smallcaps :: acc else acc in
    let underline acc = if f.underline then `Underline :: acc else acc in
    let strikeout acc = if f.strikeout then `Strikeout :: acc else acc in
    bold (italic (subscript (superscript (smallcaps (underline
                                                       (strikeout []))))))
  in
  font ts_font_family ts_font_size ts_font_type ts_font_width ts_font_color
    ts_font_styles

let fr_is_space c =
  try
    for i = 0 to String.length c.FineReader.char - 1 do
      if c.FineReader.char.[i] <> ' ' then raise Exit
    done;
    true
  with Exit -> false

let rec fr_skip_spaces = function [] -> []
  | c :: cs as l -> if fr_is_space c then fr_skip_spaces cs else l

let char_blocks_of_fr_chars log rconv c_font cb_word_lang acc chars =
  let open FineReader in
  let next_block cs =
    let cb_word_in_dict = (List.hd chars).word_from_dictionary in
    let rec aux chars cb_rect = function
      | c :: cs when not (fr_is_space c) ->
          let char =
            let c_rect = rconv c.FineReader.char_rect in
            let c_confidence = match c.FineReader.char_confidence with
            | None -> None | Some 255 -> None | Some c -> Some (float c /. 100.)
            in
            let c_char = string c.FineReader.char in
            { c_rect; c_font; c_confidence; c_char; c_alternatives = [] }
          in
          let cb_rect' = Rect.union cb_rect char.c_rect in
          aux (char :: chars) cb_rect' cs
      | cs ->
          { cb_rect; cb_word_lang; cb_word_confidence = None;
            cb_word_in_dict; cb_word_alternatives = [];
            cb_chars = List.rev chars }, cs
    in
    aux [] Rect.empty cs
  in
  let rec char_blocks acc chars = match fr_skip_spaces chars with
  | [] -> acc
  | cs -> let cb, cs' = next_block cs in char_blocks (cb :: acc) cs'
  in
  char_blocks acc chars

let char_blocks_of_fr_formatting log rconv acc f =
  let c_font = font_of_fr_formatting f in
  let cb_word_lang = lang_of_fr_language log f.FineReader.lang in
  char_blocks_of_fr_chars log rconv c_font cb_word_lang acc f.FineReader.chars

let char_block_across_formattings last f = match f.FineReader.chars with
| [] -> false
| c :: _ when fr_is_space c -> false
| _ -> match List.rev (last.FineReader.chars) with
  | c :: _ when fr_is_space c -> false
  | _ -> true

let char_blocks_of_fr_formattings log rconv fs = match List.rev fs with
| [] -> None, []
| f :: rfs ->
    let hyphen, fs = match List.rev f.FineReader.chars with
    | [] -> None, fs
    | c :: cs ->                                (* extract a possible hyphen *)
        if c.FineReader.char <> "¬" then None, fs else
        (Some (rconv c.FineReader.char_rect)),
        List.rev ({ f with FineReader.chars = List.rev cs } :: rfs)
    in
    let add (last, acc) f =
      if acc <> [] && char_block_across_formattings last f then
        let cb1 = List.hd acc in
        let cbs = List.rev (char_blocks_of_fr_formatting log rconv [] f) in
        let cb2 = List.hd cbs in
        let cb = { cb_rect = Rect.union cb1.cb_rect cb2.cb_rect;
                   cb_word_lang = cb1.cb_word_lang;
                   cb_word_confidence = cb1.cb_word_confidence;
                   cb_word_in_dict = cb1.cb_word_in_dict;
                   cb_word_alternatives = [];
                   cb_chars =
                   List.rev_append (List.rev cb1.cb_chars) cb2.cb_chars }
        in
        f, List.rev_append (List.tl cbs) (cb :: (List.tl acc))
      else
        f, (char_blocks_of_fr_formatting log rconv acc f)
    in
    hyphen, List.rev (snd (List.fold_left add (f (* ignored *), []) fs))

let line_of_fr_line log rconv l =
  let open FineReader in
  let l_rect = rconv l.line_rect in
  let l_baseline = Some (float l.baseline) in
  let l_hyphen, l_char_blocks =
    char_blocks_of_fr_formattings log rconv l.formattings;
  in
  { l_rect; l_baseline; l_hyphen; l_char_blocks }

let lines_of_fr_lines log rconv ls =
  let rec aux r acc = function
    | [] -> r, List.rev acc
    | l :: ls ->
        let l = line_of_fr_line log rconv l in
        aux (Rect.union l.l_rect r) (l :: acc) ls
  in
  aux Rect.empty [] ls

let text_block_of_fr_par log rconv p =
  let open FineReader in
  let sf i = Some (float i) in
  let tb_style =
    text_block_style (Some p.align) (sf p.left_indent) (sf p.right_indent)
      (sf p.start_indent) (sf p.line_spacing)
  in
  let tb_rect, tb_lines = lines_of_fr_lines log rconv p.lines in
  `Text { tb_rect; tb_style; tb_lines }

let block_list_of_fr_text log rconv t =
  let add acc p = (text_block_of_fr_par log rconv p) :: acc in
  List.rev (List.fold_left add [] t.FineReader.paragraphs)

let block_list_of_fr_table log rconv rows =
  let open FineReader in
  let add_pars acc p = (text_block_of_fr_par log rconv p) :: acc in
  let add_text acc t = List.fold_left add_pars acc t.paragraphs in
  let add_cell acc c = List.fold_left add_text acc c.cell_text in
  let add_row acc r = List.fold_left add_cell acc r in
  List.rev (List.fold_left add_row [] rows)

let block_of_fr_block log rconv b =
  let r = rconv b.FineReader.block_rect in
  match b.FineReader.block_type with
  | `Text t ->
      let bs = block_list_of_fr_text log rconv t in
      begin match bs with
      | [`Text tb] -> (* r may be more regular with other blocks (?) *)
          `Text { tb with tb_rect = r }
      | bs -> `Blocks (r, bs)
      end;
  | `Table t -> `Blocks (r, block_list_of_fr_table log rconv t)
  | `Picture | `Barcode -> `Picture r

let page_of_fr_page log p =
  let open FineReader in
  let rconv r = Rect.of_ltrb_i r.l (p.height - r.t) r.r (p.height - r.b) in
  { p_width = float (p.width);
    p_height = float (p.height);
    p_resolution = p.resolution;
    p_blocks = List.rev (List.rev_map (block_of_fr_block log rconv) p.blocks)}

let of_fineReader ?(log = fun _ -> ()) d =
  List.rev (List.rev_map (page_of_fr_page log) d.FineReader.pages)

(* Pretty printers *)


let pp_att ?(sp = true) n pp_v ff v =
  if sp then pp_sp ff (); pp ff "@[<1>(%s%a)@]" n pp_v v

let pp_oatt n pp_v ff v = match v with None -> ()
| Some v -> pp_att n pp_v ff v

let pp_align ff = function
  | `Center -> pp ff "center"
  | `Justify -> pp ff "justify"
  | `Left -> pp ff "left"
  | `Right -> pp ff "right"

let pp_text_block_style ff s =
  let pp_atts ff s =
    pp ff "%a%a%a%a%a"
      (pp_oatt "align" pp_align) s.tbs_align
      (pp_oatt "left-indent" pp_float) s.tbs_left_indent
      (pp_oatt "right-indent" pp_float) s.tbs_right_indent
      (pp_oatt "first-line-indent" pp_float) s.tbs_first_line_indent
      (pp_oatt "line-space" pp_float) s.tbs_line_space
  in
  (pp_att "text-block-style" pp_atts) ff s

let pp_text_block ff tb = failwith ""
(*
  let pp_atts ff b =
    pp ff "%a%a%a" Rect.print b.tb_rect pp_text_block_style b.tb_style
      (pp_list pp_line) t.tb_lines
  in
  (pp_att "text" pp_atts) ff s

*)
let rec pp_block ff = function                                   (* not t.r. *)
  | `Picture r -> (pp_att "picture" Rect.print) ff r
  | `Text t -> pp_text_block ff t
  | `Blocks (r, bs) ->
      pp ff "@[<1>(blocks@ %a@ %a)@]"
        Rect.print r (pp_list ~pp_sep:pp_sp pp_block) bs

let pp_page ff p =
  pp ff "@[<1>(page@ @[<1>(size@ %g@ %g)@]@ \
         @[<1>(resolution@ %d)@]@ @[<1>(blocks@ %a@)])@]"
    p.p_width p.p_height p.p_resolution
    (pp_list pp_block) p.p_blocks

let pp ff d = pp_att ~sp:false "pages" (pp_list pp_page) ff d


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
