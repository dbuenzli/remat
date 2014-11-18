(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Log.Log

let fstr = format_of_string

type loc = string * (int * int) * (int * int)
(** The type for location specification. Filename and character range. *)

type 'a def = loc * 'a

(* Definition locations *)


(* Basic types. *)

type 'a locales = (Locale.t * 'a) list
type uri = string
type index_id = string
type doc_id = string

(* Formatters. *)

type lookup_substring =
  [ `S_Y | `S_YY | `S_YYYY | `S_M | `S_MM | `S_d | `S_dd | `S_e
  | `S_first of int ]

type lookup =
  { path : string list;
    array : bool;
    substring : lookup_substring option;
    map : (string * string) list option;
    partial : bool; }

type format_el = [`String of string | `Lookup of lookup ]
type formatter = format_el list

(* Repo *)

type repo_d =
  { version : int;
    locales : Locale.t list * uri list;
    name : string locales;
    indexes : index_id list;
    search_href : uri option;
    publisher : string locales;
    publisher_href : uri locales; }

type headings_d =
  { label : formatter locales;
    toc : [`All | `Multiples | `Values of string list ];
    toc_multiple : int; }

type index_d =
  { label : string locales;
    i_ui_href : string;
    sort_key : formatter locales;
    synopses : [ `Isbd | `Formatter of formatter locales ];
    headings : headings_d list; }

type doc_d = { title : string; }

(* Archive database *)

type t =
  { base : string;
    log : Log.t;
    mutable archive_d : [ `Ok of repo_d | `Failed | `None ];
    indexes_d : (index_id, index_d option) Hashtbl.t;
    docs_d : (doc_id, doc_d option) Hashtbl.t;
    mutable doc_ids : [`Ok of doc_id list | `Failed | `None ] }

(* Database filename lookup *)

let err_index_miss = fstr "@[index@ `%s',@ missing@ description@ file `%s`@]"
let err_doc_miss = fstr "@[document@ `%s',@ missing@ description@ file `%s`@]"
let err_archive_d_miss = fstr "@[missing@ archive@ description@ file@ `%s'@]"
let err_doc_dir_miss = fstr "@[missing@ document@ decription@ directory `%s'@]"

let d_dir = "d"
let i_dir = "i"

let dir_exists d = try Sys.is_directory d with Sys_error _ -> false
let file_exists p = Sys.file_exists p && not (Sys.is_directory p)

let repo_d_file db ?(log = db.log.err) ()  =
  let fn = Filename.concat db.base "archive.json" in
  if file_exists fn then Some fn else (log err_archive_d_miss fn; None)

let index_file db ?(log = db.log.err) id =
  let fn = Filename.concat db.base (Filename.concat i_dir (id ^ ".json")) in
  if file_exists fn then Some fn else (log err_index_miss id fn; None)

let doc_file db ?(log = db.log.err) id =
  let fn = Filename.concat db.base (Filename.concat d_dir (id ^ ".json")) in
  if file_exists fn then Some fn else (log err_doc_miss id fn; None)

let docs_dir db ?(log = db.log.err) () =
  let dir = Filename.concat db.base "d" in
  if dir_exists dir then Some dir else (log err_doc_dir_miss dir; None)

(* Database decoder *)

let err_eoi = fstr "unexpected end of input"
let err_locales = fstr "expected a Locales object"
let err_bool = fstr "expected a boolean"
let err_int = fstr "expected an integer"
let err_string = fstr "expected a string"
let err_array = fstr "expected an array"
let err_lookup = fstr "expected a Lookup object"
let err_archive_d = fstr "expected an Archive_d object"
let err_locale_d = fstr "expected an Locale_d object"
let err_index_d = fstr "expected an Index_d object"
let err_dup_locale = fstr "locale `%s' already defined"
let err_dup_uri = fstr "URI `%s' already used"
let err_exp_end = fstr "expected end of input"
let err_mem_miss = fstr "missing required member: `%s'"
let err_index_id = fstr "expected an index id"
let warn_mem = fstr "unexpected member: `%s'"
let warn_mem_dup = fstr "duplicate member: `%s'"
let warn_doc_dir_junk = fstr "suspicious file `%s` in document directory"
let err_locs = fstr "missing definition for locale(s): %a"

type decoder =
  { db : t; inf : string; d : Jsonm.decoder;
    mutable peek : Jsonm.lexeme option option; }

let decode_file inf db d_v =
  try
    let ic = if inf = "-" then stdin else open_in_bin inf in
    let close ic = if inf = "-" then () else close_in ic in
    try
      let d = Jsonm.decoder (`Channel ic) in
      let d = { db; inf; d; peek = None } in
      let v = d_v d in
      close ic; v
    with
    | Sys_error e -> db.log.err "%s" e; close ic; None
  with Sys_error e -> db.log.err "%s" e; None

let d_err d fmt =
  d.db.log.err ("@[%s:%a: " ^^ fmt ^^ "@]") d.inf Fmt.pp_range
    (Jsonm.decoded_range d.d)

let d_warn d fmt =
  d.db.log.warn ("@[%s:%a:@ " ^^ fmt ^^ "@]") d.inf Fmt.pp_range
    (Jsonm.decoded_range d.d)







(*

let d_substring d = failwith "TODO"
let d_str_map d = failwith "TODO"
let d_lookup d =
  let opt = true in
  let o = [] in
  let o, path = d_mem o "path" (d_arr_list d_string) ~absent:[] in
  let o, array = d_mem o ~opt "array" d_bool ~absent:false in
  let o, subs = d_mem o ~opt "substring" (d_some d_substring) ~absent:None in
  let o, map = d_mem o ~opt "map" (d_some d_str_map) ~absent:None in
  let o, partial = d_mem o ~opt "map_partial" d_bool ~absent:false in
  match d_lex d with
  | Some `Os ->
      d_obj ~strict:true o d;
      Some { path = path d; array = array d;
             substring = subs d; map = map d; partial = partial d }
  | Some _ | None -> d_err d err_lookup; None

let ad =
  { version = 0;
    name = [ "*", "ERROR"; ];
    locales = ["en"], ["en"];
    indexes = [];
    search_href = None;
    publisher = [ "en", "ERROR"];
    publisher_href = ["en", "http://www.example.org/ERROR"] }


let d_archive_d d =
  let opt = true in
  let d_index_ids = d_arr_list d_index_id in
  let o = [] in
  let o, version = d_mem o "version" d_int ~absent:0 in
  let o, name = d_mem o "name" d_loc_str ~absent:ad.name in
  let o, locales = d_mem o "locales" d_locales_d ~absent:ad.locales in
  let o, indexes = d_mem o "indexes" d_index_ids ~absent:ad.indexes in
  let o, search_href = d_mem ~opt o "search_href" (d_some d_uri) ~absent:None in
  let o, pub = d_mem o "publisher" d_loc_str ~absent:ad.publisher in
  let o, pub_href = d_mem o "publisher_href" d_loc_uri ~absent:ad.publisher_href
  in
  match d_lex d with
  | Some `Os ->
      d_obj ~strict:true o d;
      Some { version = version d; locales = locales d; name = name d;
             indexes = indexes d; search_href = search_href d;
             publisher = pub d; publisher_href = pub_href d; }
  | Some _ | None -> d_err d err_archive_d; None


let d_index_id d = match d_lex d with
| Some (`String id) when index_file ~log:(d_warn d) d.db id = None -> None
| Some (`String id) -> Some id
| Some l -> d_err d err_index_id; d_skip d l; None
| None -> None

*)

let str = Printf.sprintf
let err_float_int = "expected an integer found a float"
let err_locale = "expected a language tag or `\"*\"'"
let err_lang = "expected a language tag"
let err_uri = "expected an URI"

let d_bool = Json.(undef bool)
let d_int =
  let is_int v = v -. (floor v) <> 0. in
  let to_int (loc, v) =
    if is_int v then `Ok (int_of_float v) else
    `Error (loc, err_float_int)
  in
  Json.(ret & map to_int float)

let d_uri =
  let is_uri s = true in (* TODO *)
  let to_uri (loc, uri) =
    if is_uri uri then `Ok uri else
    `Error (loc, err_uri)
  in
  Json.(ret & map to_uri string)

let d_lang d =
  let to_lang (loc, lang) =
    if Locale.is_lang lang then `Ok lang else
    `Error (loc, err_lang)
  in
  Json.(ret & map to_lang string)


let d_index_id = Json.string
let d_index_ids _ = failwith "TODO"

let d_locales d_val _ = failwith "TODO"
(*
  let add acc (loc, r) v = match acc with
  | `Error _ as e -> e
  | `Ok l ->
      if Locale.is_basic_range r then `Ok ((r, v) :: l) else
      `Error (loc, err_locale)
  in
  let redef (loc, v) = match v with
  | `Error _ as e -> e
  | `Ok l -> `Ok (loc, l)
  in
  Json.(ret & map redef & obj_map ~empty:false add (`Ok []) d_val)
*)

let d_loc_string = d_locales Json.(undef string)
let d_loc_uri = d_locales d_uri
(*

  let d_locale_d d =
    let o = [] in
    let o, locale = d_mem o "locale" (d_some d_lang) ~absent:None in
    let o, ui_href = d_mem o "ui_href" (d_some d_uri) ~absent:None in
    match d_lex d with
    | Some `Os ->
        d_obj ~strict:true o d;
        begin
          match locale d with None -> None | Some locale ->
          match ui_href d with None -> None | Some ui_href ->
          Some (locale, ui_href)
        end
    | Some _ | None -> d_err d err_locale_d; None
  in
  let ret (ls, us) = (List.rev ls), (List.rev us) in
  let fold (ls, us as acc) (l, u) =
    if List.mem l ls then (d_err d err_dup_locale l; acc) else
    if List.mem u us then (d_err d err_dup_uri u; acc) else
    (l :: ls, u :: us)
  in
  d_arr ~ret ~fold ~acc:([], []) d_locale_d d

let d_loc_str ?check = d_locales ?check d_string
let d_loc_uri ?check = d_locales ?check d_string
*)


let d_archive _ = failwith "TODO"
(*
  let open Json in
  let a (_, version) (_, (_, name)) (* (_, locales) *) (_, indexes)
      (_, publisher)
      (_, publisher_href) =
    { version; name; locales = [],[]; indexes; search_href = None;
      publisher; publisher_href }
  in
  undef &
  obj ~kind:"Archive_d" &
  obj_pure a >>
  mem "version" d_int >>
  mem "name" d_loc_string >>
(*  mem "locales" d_locales_d >>  *)
  mem "indexes" d_index_ids >>
  (*  mem "search_href" *)
  mem "publisher" d_loc_uri
*)


let d_index_d d = failwith "TODO"

let d_doc_d d = failwith "TODO"

let create : 'a. ?log:Log.t -> string -> t =
  fun ?(log = Log.default_log) base ->
  { base; log; archive_d = `None;
    indexes_d = Hashtbl.create 100;
    docs_d = Hashtbl.create 1000;
    doc_ids = `None; }

let rec repo_d db = failwith "TODO"
(*
match db.archive_d with
| `Ok ad -> Some ad
| `Failed -> None
| `None ->
    let v = match archive_d_file db () with
    | None -> `Failed
    | Some fn ->
        match decode_file fn db d_archive_d with
        | None -> `Failed
        | Some a -> `Ok a
    in
    db.archive_d <- v; archive_d db
  *)
let rec doc_ids db = match db.doc_ids with
| `Ok ids -> Some ids
| `Failed -> None
| `None ->
    let v = match docs_dir db () with
    | None -> `Failed
    | Some d ->
        let add_id acc fn =
          if (Filename.check_suffix fn ".json")
          then (Filename.chop_suffix fn ".json") :: acc
          else (db.log.warn warn_doc_dir_junk (Filename.concat d fn); acc)
        in
        try `Ok (List.rev (Array.fold_left add_id [] (Sys.readdir d))) with
        | Sys_error e -> db.log.err "%s" e; `Failed
    in
    db.doc_ids <- v; doc_ids db

let rec index_d db id = try Hashtbl.find db.indexes_d id with
| Not_found ->
    let v = match index_file db id with
    | None -> None
    | Some fn -> decode_file fn db d_index_d
    in
    Hashtbl.add db.indexes_d id v; index_d db id

let rec doc_d db id = try Hashtbl.find db.docs_d id with
| Not_found ->
    let v = match doc_file db id with
    | None -> None
    | Some fn -> decode_file fn db d_doc_d
    in
    Hashtbl.add db.docs_d id v; doc_d db id


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
