(*---------------------------------------------------------------------------
  Copyright 2012 Daniel C. Bünzli. All rights reserved.
  Distributed under the BSD3 license, see license at the end of the file.
  %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf
let str_name (ns, n) = if ns <> "" then str "%s:%s" ns n else n
let err_int v = str "'%s' not an integer" v
let err_bool v = str "'%s' not a boolean" v
let err_float v = str "'%s' not a float" v
let err_req_att att = str "missing required attribute %s" (str_name (fst att))
let err_exp_el ?found e =
  str "expected element %s" (str_name e) ^
  (match found with None -> "" | Some e' -> str "found %s" (str_name e))

let err_exp_el_end e = str "expected end of element %s" (str_name e)
let err_data d = str "unexpected data '%s" d
let err_dtd d = str "unexpected dtd '%s" d

let err_enum_att v enum =
  let enum = String.concat ", " (List.map fst enum) in
  str "illegal enum value '%s' should be one of %s" v enum

(* Parsing element attributes *)

module Name = struct
  type t = string * string
  let compare = compare
end

module Att = struct
  module NameMap = Map.Make (Name)
  type 'a value_spec = [`Required | `Optional of 'a option | `Parsed of 'a ]
  type att_spec =
    [ `String of string value_spec
    | `Int of int value_spec
    | `Bool of bool value_spec
    | `Float of float value_spec ]

  type t = Name.t * att_spec
  type parser = att_spec NameMap.t

  let parser atts =
    let add acc (name, spec) = NameMap.add name spec acc in
    List.fold_left add NameMap.empty atts

  let p_string _ v = `String (`Parsed v)
  let p_float err v =
    let f = try float_of_string v with Failure _ -> err (err_float v); 0. in
    `Float (`Parsed f)

  let p_int err v =
    let i = try int_of_string v with Failure _ -> err (err_int v); 0 in
    `Int (`Parsed i)

  let p_bool err v =
    let b = try bool_of_string v with Invalid_argument _ ->
      let i = try int_of_string v with Failure _ -> err (err_bool v); 1 in
      if i = 1 then true else if i = 0 then false else
      (err (err_bool v); true)
    in
    `Bool (`Parsed b)

  let parse err p atts =
    let rec aux err p = function
    | [] -> p
    | (name, v) :: atts' ->
        try
          let result = match NameMap.find name p with
          | `String _ -> p_string err v
          | `Float _ -> p_float err v
          | `Int _ -> p_int err v
          | `Bool _ -> p_bool err v
          in
          aux err (NameMap.add name result p) atts'
        with Not_found -> aux err p atts'
    in
    aux err p atts

  let get_value err ~on_error a = function
  | `Required -> err (err_req_att a); on_error
  | `Optional Some v | `Parsed v -> v
  | `Optional None -> assert false

  let get_opt_value err a = function
  | `Optional None -> None
  | `Parsed v -> (Some v)
  | `Optional Some _ | `Required -> assert false

  let att_spec p a = try NameMap.find (fst a) p with
  | Not_found -> assert false

  let bool err p a = match att_spec p a with
  | `Bool v -> get_value err ~on_error:true a v | _ -> assert false

  let bool_opt err p a = match att_spec p a with
  | `Bool v -> get_opt_value err a v | _ -> assert false

  let int err p a = match att_spec p a with
  | `Int v -> get_value err ~on_error:0 a v | _ -> assert false

  let int_opt err p a = match att_spec p a with
  | `Int v -> get_opt_value err a v | _ -> assert false

  let float err p a = match att_spec p a with
  | `Float v -> get_value err ~on_error:0 a v | _ -> assert false

  let float_opt err p a = match att_spec p a with
  | `Float v -> get_opt_value err a v | _ -> assert false

  let str err p a = match att_spec p a with
  | `String v -> get_value err ~on_error:"" a v | _ -> assert false

  let str_opt err p a = match att_spec p a with
  | `String v -> get_opt_value err a v | _ -> assert false

  let str_enum err p (enum : (string * 'a) list) a = match att_spec p a with
  | `String v ->
      let v = get_value err ~on_error:(fst (List.hd enum)) a v in
      (try List.assoc v enum
      with Not_found -> err (err_enum_att v enum); snd (List.hd enum))
  | _ -> assert false
end

(* Parsing elements. *)

module El = struct
  type t = Name.t * Att.parser
  type parser = t *
                ((Xmlm.pos -> string -> unit) -> Xmlm.input -> Att.parser ->
                 Xmlm.attribute list)

  let spec n atts = n, Att.parser atts
  let skip i =
    let rec skip i d = match Xmlm.input i with
    | `El_start (n, _) -> skip i (d + 1)
    | `El_end -> if d <= 1 then () else skip i (d - 1)
    | s -> skip i d
    in
    skip i 0

  let _parse_el err i atts atts_p el_p =
    let pos = Xmlm.pos i in
    let atts = Att.parse (err pos) atts_p atts in
    ignore (Xmlm.input i);
    let r = el_p err i pos atts in
    match Xmlm.peek i with
    | `El_end -> ignore (Xmlm.input i); r
    | l -> skip i; r

  let rec parse ~on_error err i ((n, atts_p), el_p as e) =
    match Xmlm.peek i with
    | `El_end -> err (Xmlm.pos i) (err_exp_el n); on_error
    | `El_start (n', atts) when n' = n -> _parse_el err i atts atts_p el_p
    | `El_start _ -> skip i; parse ~on_error err i e
    | `Data _ -> ignore (Xmlm.input i); parse ~on_error err i e
    | `Dtd _ -> assert false

  let rec parse_opt err i ((n, atts_p), el_p as e) = match Xmlm.peek i with
  | `El_end -> None
  | `El_start (n', atts) when n' = n -> Some (_parse_el err i atts atts_p el_p)
  | `El_start _ -> skip i; parse_opt err i e
  | `Data _ -> ignore (Xmlm.input i); parse_opt err i e
  | `Dtd _ -> assert false

  let parse_seq err i e =
    let rec aux err i e acc = match parse_opt err i e with
    | None -> List.rev acc | Some ed -> aux err i e (ed :: acc)
    in
    aux err i e []

  let rec parse_data err i =
    let rec aux err i acc = match Xmlm.peek i with
    | `El_end -> String.concat "" (List.rev acc)
    | `Data d -> ignore (Xmlm.input i); aux err i (d :: acc)
    | `El_start _ -> skip i; aux err i acc
    | `Dtd _ -> assert false
    in
    aux err i []
end

(* Formatter tools *)

let pp ppf fmt = Format.fprintf ppf fmt
let pp_cut = Format.pp_print_cut
let pp_bool = Format.pp_print_bool
let pp_int = Format.pp_print_int
let pp_str = Format.pp_print_string
let pp_float = Format.pp_print_float
let pp_opt ?(pp_none = fun ppf () -> ()) pp_v ppf = function
| None -> pp_none ppf () | Some v -> pp_v ppf v

let rec pp_list ?(pp_sep = pp_cut) pp_v ppf = function
| v :: vs -> pp_v ppf v; pp_sep ppf (); pp_list ~pp_sep pp_v ppf vs
| [] -> ()

let pp_enum enums ppf v =
  let str, _ = List.find (fun (_, enum) -> v = enum) enums in
  pp_str ppf str

(* Fine reader elements and attributes. *)

let ns = "http://www.abbyy.com/FineReader_xml/FineReader6-schema-v1.xml"
let a s = ("", s)
let el s atts = El.spec (ns, s) atts

(* Attributes common to many tags *)

type rect = { l : int; t : int; r : int; b : int }
let a_l = a "l", (`Int `Required)
let a_t = a "t", (`Int `Required)
let a_r = a "r", (`Int `Required)
let a_b = a "b", (`Int `Required)
let atts_rect err atts =
  { l = Att.int err atts a_l; t = Att.int err atts a_t;
    r = Att.int err atts a_r; b = Att.int err atts a_b; }

let pp_rect ppf r = pp ppf "@[[%d@ %d@ %d@ %d]@]" r.l r.t r.r r.b

(* charParams element *)

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

let a_suspicious = a "suspicious", (`Bool (`Optional (Some false)))
let a_proofed = a "proofed", (`Bool (`Optional (Some false)))
let a_word_start = a "wordStart", (`Bool (`Optional None))
let a_word_from_dictionary = a "wordFromDictionary", (`Bool (`Optional None))
let a_word_normal = a "wordNormal", (`Bool (`Optional None))
let a_word_numeric = a "wordNumeric", (`Bool (`Optional None))
let a_word_identifier = a "wordIdentifier", (`Bool (`Optional None))
let a_char_confidence = a "charConfidence", (`Int (`Optional None))
let a_serif_probability = a "serifProbability", (`Int (`Optional None))
let a_word_penalty = a "wordPenalty", (`Int (`Optional None))
let a_mean_stroke_width = a "meanStrokeWidth", (`Int (`Optional None))
let e_char_params =
  el "charParams"
    [ a_l; a_t; a_r; a_b; a_suspicious; a_proofed; a_word_start;
      a_word_from_dictionary; a_word_normal; a_word_numeric; a_word_identifier;
      a_char_confidence; a_serif_probability; a_word_penalty;
      a_mean_stroke_width ],
  fun err i pos atts ->
    let err' = err pos in
    let char_rect = atts_rect err' atts in
    let suspicious = Att.bool err' atts a_suspicious in
    let proofed = Att.bool err' atts a_proofed in
    let word_start = Att.bool_opt err' atts a_word_start in
    let word_from_dictionary = Att.bool_opt err' atts a_word_from_dictionary in
    let word_normal = Att.bool_opt err' atts a_word_normal in
    let word_numeric = Att.bool_opt err' atts a_word_numeric in
    let word_identifier = Att.bool_opt err' atts a_word_identifier in
    let char_confidence = Att.int_opt err' atts a_char_confidence in
    let serif_probability = Att.int_opt err' atts a_serif_probability in
    let word_penalty = Att.int_opt err' atts a_word_penalty in
    let mean_stroke_width = Att.int_opt err' atts a_mean_stroke_width in
    let char = El.parse_data err i in
    { char_rect; suspicious; proofed; word_start; word_from_dictionary;
      word_normal; word_numeric; word_identifier; char_confidence;
      serif_probability; word_penalty; mean_stroke_width; char }

let pp_char_params ppf c =
  pp ppf "@[<v 1>Char params@,\
          @[char_rect =@ %a@]@,\
          @[suspicious =@ %b@]@,\
          @[proofed =@ %b@]@,\
          @[word_start =@ %a@]@,\
          @[word_from_dictionary =@ %a@]@,\
          @[word_normal =@ %a@]@,\
          @[word_numeric =@ %a@]@,\
          @[word_identifier =@ %a@]@,\
          @[char_confidence =@ %a@]@,\
          @[serif_probability =@ %a@]@,\
          @[word_penalty =@ %a@]@,\
          @[mean_stroke_width =@ %a@]@,\
          @[char ='%s'@]@]"
    pp_rect c.char_rect c.suspicious c.proofed (pp_opt pp_bool)
    c.word_start (pp_opt pp_bool) c.word_from_dictionary
    (pp_opt pp_bool) c.word_normal (pp_opt pp_bool) c.word_numeric
    (pp_opt pp_bool) c.word_identifier (pp_opt pp_int) c.char_confidence
    (pp_opt pp_int) c.serif_probability (pp_opt pp_int)
    c.word_penalty (pp_opt pp_int) c.mean_stroke_width c.char

(* formatting element *)

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

let a_lang = a "lang", (`String `Required)
let a_font_family = a "ff", (`String (`Optional None))
let a_font_size = a "fs", (`Float (`Optional None))
let a_bold = a "bold", (`Bool (`Optional (Some false)))
let a_italic = a "italic", (`Bool (`Optional (Some false)))
let a_subscript = a "subscript", (`Bool (`Optional (Some false)))
let a_superscript = a "superscript", (`Bool (`Optional (Some false)))
let a_smallcaps = a "smallcaps", (`Bool (`Optional (Some false)))
let a_underline = a "underline", (`Bool (`Optional (Some false)))
let a_strikeout = a "strikeout", (`Bool (`Optional (Some false)))
let a_color = a "color", (`Int (`Optional (Some 0)))
let a_scaling = a "scaling", (`Int (`Optional (Some 1000)))
let a_spacing = a "spacing", (`Int (`Optional (Some 0)))
let e_formatting =
  el "formatting"
    [ a_lang; a_font_family; a_font_size; a_bold; a_italic; a_subscript;
      a_superscript; a_smallcaps; a_underline; a_strikeout; a_color;
      a_scaling; a_spacing; ],
  fun err i pos atts ->
    let err' = err pos in
    let lang = Att.str err' atts a_lang in
    let font_family = Att.str_opt err' atts a_font_family in
    let font_size = Att.float_opt err' atts a_font_size in
    let bold = Att.bool err' atts a_bold in
    let italic = Att.bool err' atts a_italic in
    let subscript = Att.bool err' atts a_subscript in
    let superscript = Att.bool err' atts a_superscript in
    let smallcaps = Att.bool err' atts a_smallcaps in
    let underline = Att.bool err' atts a_underline in
    let strikeout = Att.bool err' atts a_strikeout in
    let color = Att.int err' atts a_color in
    let scaling = Att.int err' atts a_scaling in
    let spacing = Att.int err' atts a_spacing in
    let chars = El.parse_seq err i e_char_params in
    { lang; font_family; font_size; bold; italic; subscript; superscript;
      smallcaps; underline; strikeout; color; scaling; spacing; chars }

let pp_formatting ppf f =
  pp ppf "@[<v 1>Formatting@,\
          @[lang =@ %s@]@,\
          @[font_family =@ %a@]@,\
          @[font_size =@ %a@]@,\
          @[bold =@ %b@]@,\
          @[italic =@ %b@]@,\
          @[subscript =@ %b@]@,\
          @[superscript =@ %b@]@,\
          @[smallcaps =@ %b@]@,\
          @[underline =@ %b@]@,\
          @[strikeout =@ %b@]@,\
          @[color =@ %d@]@,\
          @[scaling =@ %d@]@,\
          @[spacing =@ %d@]@,\
          %a@]"
    f.lang (pp_opt pp_str) f.font_family (pp_opt pp_float) f.font_size
    f.bold f.italic f.subscript f.superscript
    f.smallcaps f.underline f.strikeout f.color f.scaling f.spacing
    (pp_list pp_char_params) f.chars

(* line element *)

type line =
  { baseline : int;
    line_rect : rect;
    formattings : formatting list }

let a_baseline = a "baseline", (`Int `Required)
let e_line = el "line" [ a_baseline; a_l; a_t; a_r; a_b ],
             fun err i pos atts ->
               let err' = err pos in
               let baseline = Att.int err' atts a_baseline in
               let line_rect = atts_rect err' atts in
               let formattings = El.parse_seq err i e_formatting in
               { baseline; line_rect; formattings; }

let pp_line ppf l =
  pp ppf "@[<v 1>Line@,\
          @[baseline =@ %d@]@,\
          @[line_rect =@ %a@]@,\
          %a@]"
    l.baseline pp_rect l.line_rect (pp_list pp_formatting) l.formattings

(* par element *)

type par_align = [ `Left | `Center | `Right | `Justify ]
let par_align_enum =
  ["Left", `Left; "Center", `Center; "Right", `Right; "Justified", `Justify]

type par =
  { drop_cap_chars_count : int;
    drop_cap_rect : rect option;
    align : par_align;
    left_indent : int;
    right_indent : int;
    start_indent : int;
    line_spacing : int;
    lines : line list }

let a_drop_cap_chars_count = a "dropCapCharsCount", (`Int (`Optional (Some 0)))
let a_drop_cap_l = a "dropCap-l", (`Int (`Optional None))
let a_drop_cap_t = a "dropCap-t", (`Int (`Optional None))
let a_drop_cap_r = a "dropCap-r", (`Int (`Optional None))
let a_drop_cap_b = a "dropCap-b", (`Int (`Optional None))
let a_align = a "align", (`String (`Optional (Some "Left")))
let a_left_indent = a "leftIndent", (`Int (`Optional (Some 0)))
let a_right_indent = a "rightIndent", (`Int (`Optional (Some 0)))
let a_start_indent = a "startIndent", (`Int (`Optional (Some 0)))
let a_line_spacing = a "lineSpacing", (`Int (`Optional (Some 0)))
let e_par =
  el "par"
    [ a_drop_cap_chars_count; a_drop_cap_l; a_drop_cap_t; a_drop_cap_r;
      a_drop_cap_b; a_align; a_left_indent; a_right_indent; a_start_indent;
      a_line_spacing; ],
  fun err i pos atts ->
    let err' = err pos in
    let drop_cap_chars_count = Att.int err' atts a_drop_cap_chars_count in
    let drop_cap_rect =
      try
        let get_opt att = match Att.int_opt err' atts att with
        | None -> raise Exit | Some v -> v
        in
        Some { l = get_opt a_drop_cap_l; t = get_opt a_drop_cap_t;
               r = get_opt a_drop_cap_r; b = get_opt a_drop_cap_b }
      with Exit -> None
    in
    let align = Att.str_enum err' atts par_align_enum a_align in
    let left_indent = Att.int err' atts a_left_indent in
    let right_indent = Att.int err' atts a_right_indent in
    let start_indent = Att.int err' atts a_start_indent in
    let line_spacing = Att.int err' atts a_line_spacing in
    let lines = El.parse_seq err i e_line in
    { drop_cap_chars_count; drop_cap_rect; align; left_indent; right_indent;
      start_indent; line_spacing; lines }

let pp_par ppf p =
  pp ppf "@[<v 1>Paragraph@,\
          @[drop_char_count =@ %d@]@,\
          @[drop_cap_rect =@ %a@]@,\
          @[align =@ %a@]@,\
          @[left_indent =@ %d@]@,\
          @[right_indent =@ %d@]@,\
          @[start_indent =@ %d@]@,\
          @[line_spacing =@ %d@]@,\
          %a@]"
    p.drop_cap_chars_count (pp_opt pp_rect) p.drop_cap_rect
    (pp_enum par_align_enum) p.align p.left_indent p.right_indent p.start_indent
    p.line_spacing (pp_list pp_line) p.lines

(* text element *)

type orientation = [`Normal | `Rotated_cw | `Rotated_upsidedown | `Rotated_ccw ]
let orientation_enum = [ "Normal",  `Normal; "RotatedClockwise", `Rotated_cw;
                         "RotatedUpsidedown", `Rotated_upsidedown;
                         "RotateCounterclockwise", `Rotated_ccw; ]
type text =
  { orientation : orientation;
    background_color : int;
    mirrored : bool;
    inverted : bool;
    paragraphs : par list; }

let text_on_error = { orientation = `Normal; background_color = 16777215;
                      mirrored = false; inverted = false; paragraphs = [] }

let a_orientation = a "orientation", (`String (`Optional (Some "Normal")))
let a_background_color = a "backgroundColor", (`Int (`Optional (Some 16777215)))
let a_mirrored = a "mirrored", (`Bool (`Optional (Some false)))
let a_inverted = a "inverted", (`Bool (`Optional (Some false)))
let e_text =
  el "text" [ a_orientation; a_background_color; a_mirrored; a_inverted ],
  fun err i pos atts ->
    let err' = err pos in
    let orientation = Att.str_enum err' atts orientation_enum a_orientation in
    let background_color = Att.int err' atts a_background_color in
    let mirrored = Att.bool err' atts a_mirrored in
    let inverted = Att.bool err' atts a_inverted  in
    let paragraphs = El.parse_seq err i e_par in
    { orientation; background_color; mirrored; inverted; paragraphs }

let pp_text ppf t =
  pp ppf "@[<v 1>Text@,\
          @[orientation =@ %a@]@,\
          @[background_color = @ %d@]@,\
          @[mirrored =@ %b@]@,\
          @[inverted =@ %b@]@,\
          %a@]"
    (pp_enum orientation_enum) t.orientation t.background_color t.mirrored
    t.inverted (pp_list pp_par) t.paragraphs

(* cell element *)

type cell_border_type = [ `Absent |`Unknown | `White | `Black ]
let cell_border_enum =
  [ "Absent", `Absent; "Unknown", `Unknown; "White", `White; "Black", `Black ]

type cell_align = [ `Top | `Center | `Bottom ]
let cell_align_enum = [ "Top", `Top; "Center", `Center; "Bottom", `Bottom ]

type cell =
  { cell_text : text list;
    col_span : int;
    row_span : int;
    cell_align : cell_align;
    picture : bool;
    left_border : cell_border_type;
    top_border : cell_border_type;
    right_border : cell_border_type;
    bottom_border : cell_border_type;
    cell_width : int;
    cell_height : int; }

let a_col_span = a "collSpan", (`Int (`Optional (Some 1)))
let a_row_span = a "rowSpan", (`Int (`Optional (Some 1)))
let a_cell_align = a "align", (`String (`Optional (Some "Top")))
let a_picture = a "picture", (`Bool (`Optional (Some false)))
let a_left_border = a "leftBorder", (`String (`Optional (Some "Black")))
let a_top_border = a "topBorder", (`String (`Optional (Some "Black")))
let a_right_border = a "rightBorder", (`String (`Optional (Some "Black")))
let a_bottom_border = a "bottomBorder", (`String (`Optional (Some "Black")))
let a_cell_width = a "width", (`Int `Required)
let a_cell_height = a "height", (`Int `Required)
let e_cell =
  el "cell"
    [ a_col_span; a_row_span; a_cell_align; a_picture; a_left_border;
      a_top_border; a_right_border; a_bottom_border; a_cell_width;
      a_cell_height ],
  fun err i pos atts ->
    let err' = err pos in
    let col_span = Att.int err' atts a_col_span in
    let row_span = Att.int err' atts a_row_span in
    let cell_align = Att.str_enum err' atts cell_align_enum a_cell_align in
    let picture = Att.bool err' atts a_picture  in
    let cell_border_att = Att.str_enum err' atts cell_border_enum in
    let left_border = cell_border_att a_left_border in
    let top_border = cell_border_att a_top_border in
    let right_border = cell_border_att a_right_border in
    let bottom_border = cell_border_att a_bottom_border in
    let cell_width = Att.int err' atts a_cell_width in
    let cell_height = Att.int err' atts a_cell_height in
    let cell_text = El.parse_seq err i e_text in
    { col_span; row_span; cell_align; picture; left_border; top_border;
      right_border; bottom_border; cell_width; cell_height; cell_text; }

(* row element *)

type row = cell list
let e_row = el "row" [], fun err i pos atts -> El.parse_seq err i e_cell

(* rect element *)

let e_rect =
  el "rect" [ a_l; a_t; a_r; a_b ],
  fun err i pos atts -> atts_rect (err pos) atts

(* region element *)

let e_region = el "region" [], fun err i pos atts -> El.parse_seq err i e_rect

(* block element *)

type block =
  { block_type : [ `Text of text | `Table of row list | `Picture | `Barcode ];
    block_rect : rect;
    block_region : rect list; }

let block_type_enum =
  ["Text", `Text; "Table", `Table; "Picture", `Picture; "Barcode", `Barcode]

let a_block_type = a "blockType", (`String `Required)
let e_block =
  el "block" [ a_block_type; a_l; a_t; a_r; a_b; ],
  fun err i pos atts ->
    let err' = err pos in
    let block_rect = atts_rect err' atts in
    let block_type = Att.str_enum err' atts  block_type_enum a_block_type in
    let block_region = El.parse ~on_error:[] err i e_region in
    let block_type = match block_type with
    | `Picture -> `Picture
    | `Barcode -> `Barcode (* no data ? *)
    | `Table -> `Table (El.parse_seq err i e_row)
    | `Text -> `Text (El.parse ~on_error:text_on_error err i e_text)
    in
    { block_type; block_rect; block_region }

let pp_block_type ppf bt = match bt with
| `Picture -> pp ppf "Picture"
| `Barcode -> pp ppf "Barcode"
| `Table rl -> pp ppf "Table TODO"
| `Text t -> pp ppf "%a" pp_text t

let pp_block ppf b =
  pp ppf "@[<v 1>block@,\
          @[block_rect =@ %a@]@,\
          @[block_region =@ @[%a@]@]@,\
          %a@]"
    pp_rect b.block_rect (pp_list pp_rect) b.block_region pp_block_type
    b.block_type

(* page element *)

type page =
  { width : int;
    height : int;
    resolution : int;
    original_coords : bool;
    blocks : block list }

let a_width = a "width", (`Int `Required)
let a_height = a "height", (`Int `Required)
let a_resolution = a "resolution", (`Int `Required)
let a_original_coords = a "originalCoords", (`Bool (`Optional (Some false)))
let e_page =
  el "page" [ a_width; a_height; a_resolution; a_original_coords ],
  fun err i pos atts ->
    let err' = err pos in
    let width = Att.int err' atts a_width in
    let height = Att.int err' atts a_height in
    let resolution = Att.int err' atts a_resolution in
    let original_coords = Att.bool err' atts a_original_coords in
    let blocks = El.parse_seq err i e_block in
    { width; height; resolution; original_coords; blocks }

let pp_page ppf p =
  let r = float p.resolution in
  let w_cm = ((float p.width) /. r) *. 2.54 in
  let h_cm = ((float p.height) /. r) *. 2.54 in
  pp ppf "@[<v 1>page@,\
          @[width =@ %d (%.1fcm)@]@,\
          @[height =@ %d (%.1fcm)@]@,\
          @[resolution =@ %d@]@,\
          @[original_coords =@ %b@]@,\
          %a@]"
    p.width w_cm p.height h_cm p.resolution p.original_coords
    (pp_list pp_block) p.blocks

(* document element *)

type document =
  { version : string;
    producer : string;
    pages_count : int; (* defaults to 1 *)
    main_language : string option;
    languages : string option;
    pages : page list  }

let on_error_document =
  { version = ""; producer = ""; pages_count = 1; main_language = None;
    languages = None; pages = [] }

let a_version = a "version", (`String `Required)
let a_producer = a "producer", (`String `Required)
let a_pages_count = a "pagesCount", (`Int (`Optional (Some 1)))
let a_main_language = a "mainLanguage", (`String (`Optional None))
let a_languages = a "languages", (`String (`Optional None))
let e_document =
  el "document"
    [ a_version; a_producer; a_pages_count; a_main_language; a_languages ],
  fun err i pos atts ->
    let err' = err pos in
    let version = Att.str err' atts a_version in
    let producer = Att.str err' atts a_producer in
    let pages_count = Att.int err' atts a_pages_count in
    let main_language = Att.str_opt err' atts a_main_language in
    let languages = Att.str_opt err' atts a_languages in
    let pages = El.parse_seq err i e_page in
    { version; producer; pages_count; main_language; languages; pages; }

let pp_document ppf d =
  pp ppf "@[<v 1>document@,\
          @[version =@ %s@]@,\
          @[producer =@ %s@]@,\
          @[main_language =@ %a@]@,\
          @[languages =@ %a@]@,\
          @[pages_count =@ %d@]@,\
          %a@]"
    d.version d.producer (pp_opt pp_str) d.main_language
    (pp_opt pp_str) d.languages d.pages_count
    (pp_list pp_page) d.pages

(* FineReader input *)

let input ?(err = fun _ _ -> ()) src =
  let i = Xmlm.make_input ~strip:false src in
  (match Xmlm.peek i with `Dtd _ -> ignore (Xmlm.input i); | _ -> ());
  El.parse ~on_error:on_error_document err i e_document

(* Data dumps *)

let fmt_compare f f' =
  let f = { f with lang = ""; chars = []; spacing = 0; } in
  let f' = {f' with lang = ""; chars = []; spacing = 0; } in
  compare f f'

let fmt_print ppf f =
  let pr_if s ppf b = if b then pp_str ppf s in
  pp ppf "%a %a %a%a%a%a%a%a%ascaling:%d"
    (pp_opt ~pp_none:(fun ppf () -> pp ppf "Unknown") pp_str) f.font_family
    (pp_opt pp_float) f.font_size
    (pr_if "bold ") f.bold
    (pr_if "italic ") f.italic
    (pr_if "subscript ") f.subscript
    (pr_if "superscript ") f.superscript
    (pr_if "smallcaps ") f.smallcaps
    (pr_if "underline ") f.underline
    (pr_if "strikeout ") f.strikeout
    f.scaling

let ascii_art ppf d =
  let pr fmt = pp ppf fmt in
  let chars cs =
    let rec aux start = function
    | [] -> pr "'"
    | c :: cs ->
        if c.word_start = Some true || start then
          (pr "'"; if not start then pr " '");
        pr "%s" c.char;
        aux false cs
    in
    match cs with [] -> () | cs -> aux true cs
  in
  let formatting (first, fmt as acc) f =
    let show_fmt f =
      if not first then pr "@\n";
      pr "|||+- Fmt %a@\n||| " fmt_print f
    in
    let acc' = match fmt with
    |  None -> show_fmt f; false, Some f
    | Some f' when fmt_compare f f' <> 0 -> show_fmt f; false, Some f
    | Some _ -> if first then (pr "||| "; false, fmt) else acc
    in
    chars f.chars;
    acc'
  in
  let line fmt l =
    let _, fmt =  (List.fold_left formatting (true, fmt) l.formattings) in
    pr "@\n"; fmt
  in
  let par fmt p =
    pr "||+- Par, align %a, line spacing %d@\n"
      (pp_enum par_align_enum) p.align p.line_spacing;
    List.fold_left line fmt p.lines
  in
  let cell fmt c =
    let text fmt t = List.fold_left par fmt t.paragraphs in
    pr "||+- Cell@\n"; List.fold_left text fmt c.cell_text
  in
  let row fmt cells = pr "||+- Row@\n"; List.fold_left cell fmt cells; in
  let block fmt b = match b.block_type with
  | `Picture -> pr "|+- Picture block %a@\n" pp_rect b.block_rect; fmt
  | `Barcode -> pr "|+- Barcode block %a@\n" pp_rect b.block_rect; fmt
  | `Table rows ->
      pr "|+- Table block%a@\n" pp_rect b.block_rect;
      List.fold_left row fmt rows
  | `Text t ->
      pr "|+- Text block %a@\n" pp_rect b.block_rect;
      List.fold_left par fmt t.paragraphs
  in
  let page max (i, fmt) p =
    pr "+- Page %d/%d, %dx%d @@ %d dpi\n" i max p.width p.height p.resolution;
    i + 1, List.fold_left block fmt p.blocks;
  in
  let lang = match d.main_language with None -> "unknown" | Some l -> l in
  pr "Document %s, %d pages, main language %s@\n"
    d.producer d.pages_count lang;
  ignore (List.fold_left (page d.pages_count) (1, None) d.pages)

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
