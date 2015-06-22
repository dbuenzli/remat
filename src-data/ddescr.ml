(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Bos

let string_map_codec ?default ?kind codec =
  let default = match default with None -> String.Map.empty | Some d -> d in
  let objc = Jsont.objc ?kind () in
  let anon = Jsont.anon objc codec in
  let codec = Jsont.obj objc in
  let decode o =
    let add acc k = String.Map.add k (Jsont.get_anon anon k o) acc in
    `Ok (List.fold_left add String.Map.empty (Jsont.anon_names anon o))
  in
  let encode m =
    let add k v acc = Jsont.anonv anon k v :: acc in
    Jsont.new_obj codec (String.Map.fold add m [])
  in
  Jsont.view ~default (decode, encode) codec

type format_str = Jsont.nat_string
let format_str_codec = Jsont.nat_string
(*
  let untag (`Msg s) = s in
  let dec s = R.(to_presult (reword_error untag (Pat.of_string s))) in
  let enc p = Pat.to_string p in
  Jsont.view (dec, enc) Jsont.string
*)

type mem_path = string list
let mem_path_codec = Jsont.(array string)

module Integer_range = struct
  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"Lookup" ()
  let mem_start = Jsont.mem objc "start" Jsont.int
  let mem_end = Jsont.mem_opt objc "end" Jsont.int
  let codec = Jsont.obj objc
  let v ~start ?end_ () =
    Jsont.(new_obj codec [memv mem_start start; memv mem_end end_])

  let start = Jsont.get mem_start
  let end_ = Jsont.get mem_end
end

module Doc_ref = struct
  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"Doc_ref" ()
  let mem_descr = Jsont.mem objc "descr" format_str_codec
  let mem_descr_extra = Jsont.mem_opt objc "descr_extra" format_str_codec
  let mem_date = Jsont.mem_opt objc "date" format_str_codec
  let mem_doc_type = Jsont.mem ~opt:`Yes_rem objc "doc_type" Jsont.bool
  let mem_doc_count = Jsont.mem ~opt:`Yes_rem objc "doc_count" Jsont.bool
  let codec = Jsont.obj objc

  let v ~descr ?descr_extra ?date ?(doc_type = false) ?(doc_count = false) () =
    Jsont.(new_obj codec [memv mem_descr descr;
                          memv mem_descr_extra descr_extra;
                          memv mem_date date;
                          memv mem_doc_type doc_type;
                          memv mem_doc_count doc_count])

  let descr = Jsont.get mem_descr
  let descr_extra = Jsont.get mem_descr_extra
  let date = Jsont.get mem_date
  let doc_type = Jsont.get mem_doc_type
  let doc_count = Jsont.get mem_doc_count
end

module Var_envs = struct
  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"Var_envs" ()
  let mem_file_scan = Jsont.mem_opt objc "file_scan" format_str_codec
  let mem_ranges =
    Jsont.mem_opt objc "ranges"
      (string_map_codec (Jsont.array Integer_range.codec))

  let mem_set =
    Jsont.mem_opt objc "set" (string_map_codec (Jsont.(array string)))

  let codec = Jsont.obj objc

  let v ?file_scan ?ranges ?set:s () =
    Jsont.(new_obj codec [memv mem_file_scan file_scan;
                          memv mem_ranges ranges;
                          memv mem_set s])

  let file_scan = Jsont.get mem_file_scan
  let ranges = Jsont.get mem_ranges
  let set = Jsont.get mem_set
end

module Repo = struct
  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"Repo" ()
  let mem_version = Jsont.mem objc "version" Jsont.int
  let mem_name = Jsont.mem objc "name" D.locv_str_codec
  let mem_publisher = Jsont.mem objc "publisher" D.(locv_codec Ui_link.codec)
  let mem_ui_locales =
    Jsont.mem objc "ui_locales" (Jsont.array D.Ui_locale.codec)

  let mem_indexes = Jsont.mem objc "indexes" (Jsont.array D.index_id_codec)
  let mem_search_href = Jsont.mem_opt objc "search_href" D.uri_codec
  let codec = Jsont.obj objc
  let v ~version ~name ~publisher ~ui_locales ~indexes ?search_href () =
    Jsont.(new_obj codec [memv mem_version version;
                          memv mem_name name;
                          memv mem_publisher publisher;
                          memv mem_ui_locales ui_locales;
                          memv mem_indexes indexes;
                          memv mem_search_href search_href;])

  let version = Jsont.get mem_version
  let name = Jsont.get mem_name
  let publisher = Jsont.get mem_publisher
  let ui_locales = Jsont.get mem_ui_locales
  let indexes = Jsont.get mem_indexes
  let search_href = Jsont.get mem_search_href
end

module Headings = struct
  type toc = [`All | `Multiples | `Custom ]
  let toc_codec =
    let dec = function
    | "all" -> `Ok `All | "multiples" -> `Ok `Multiples
    | "custom" -> `Ok `Custom
    | t -> `Error ("invalid toc type: " ^ t)
    in
    let enc = function
    | `All -> "all" | `Multiples -> "multiples" | `Custom -> "custom"
    in
    Jsont.view ~default:`All (dec, enc) Jsont.string

  (* FIXME could enforce mutually exclusive fields trough a codec *)
  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"Headings" ()
  let mem_label = Jsont.mem objc "label" format_str_codec
  let mem_label_href = Jsont.mem_opt objc "label_href" format_str_codec
  let mem_toc = Jsont.mem_opt objc "toc" toc_codec
  let mem_toc_multiple = Jsont.mem_opt objc "toc_multiple" Jsont.int
  let mem_toc_custom = Jsont.mem_opt objc "toc_custom" Jsont.(array string)
  let codec = Jsont.obj objc
  let v ~label ~label_href ?toc ?toc_multiple ?toc_custom () =
    Jsont.(new_obj codec [memv mem_label label;
                          memv mem_label label_href;
                          memv mem_toc toc;
                          memv mem_toc_multiple toc_multiple;
                          memv mem_toc_custom toc_custom])


  let label = Jsont.get mem_label
  let label_href = Jsont.get mem_label_href
  let toc = Jsont.get mem_toc
  let toc_multiple = Jsont.get mem_toc_multiple
  let toc_custom = Jsont.get mem_toc_custom
 end

module Index = struct
  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"Index" ()
  let mem_name = Jsont.mem objc "name" (D.(locv_codec Ui_link.codec))
  let mem_key = Jsont.mem objc "key" (D.locv_codec format_str_codec)
  let mem_string_maps =
    let eq = String.Map.equal (String.Map.equal ( = )) in
    Jsont.mem ~eq ~opt:`Yes_rem objc "string_maps"
      (string_map_codec (string_map_codec Jsont.string))

  let mem_doc_vars = Jsont.mem objc "doc_vars"
      (string_map_codec ~kind:"String_map.<Mem_path>" mem_path_codec)

  let mem_headings =
    Jsont.mem objc "headings" (D.locv_codec (Jsont.array Headings.codec))

  let mem_doc_ref = Jsont.mem objc "doc_ref" (D.locv_codec Doc_ref.codec)
  let codec = Jsont.obj objc
  let v ~name ~string_maps  ~doc_vars ~key ~headings ~doc_ref () =
    Jsont.(new_obj codec [memv mem_name name;
                          memv mem_string_maps string_maps;
                          memv mem_doc_vars doc_vars;
                          memv mem_key key;
                          memv mem_headings headings;
                          memv mem_doc_ref doc_ref])

  let name = Jsont.get mem_name
  let doc_vars = Jsont.get mem_doc_vars
  let string_maps = Jsont.get mem_string_maps
  let key = Jsont.get mem_key
  let headings = Jsont.get mem_headings
  let doc_ref = Jsont.get mem_doc_ref
end

module View_labels = struct
  type label_type = [ `Arabic | `Roman | `Front_cover | `Back_cover | `Custom ]
  let label_type_codec =
    Jsont.enum ~default:`Roman
      [ "arabic", `Arabic; "roman", `Roman; "front_cover", `Front_cover;
        "back_cover", `Back_cover; "custom", `Custom ]

  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"View_labels" ()
  let mem_type = Jsont.mem objc "type" label_type_codec
  let mem_custom = Jsont.mem_opt objc "custom" format_str_codec
  let mem_start = Jsont.mem_opt objc "start" Jsont.int
  let mem_folio = Jsont.mem objc ~opt:`Yes_rem "folio" Jsont.bool
  let codec = Jsont.obj objc
  let v ~type_ ?custom ?start ?(folio = false) () =
    Jsont.(new_obj codec [memv mem_type type_;
                          memv mem_custom custom;
                          memv mem_start start;
                          memv mem_folio folio])

  let type_ = Jsont.get mem_type
  let custom = Jsont.get mem_custom
  let start = Jsont.get mem_start
  let folio = Jsont.get mem_folio
end

module View_label_range = struct
  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"View_label_range" ()
  let mem_range = Jsont.mem objc "range" Integer_range.codec
  let mem_labels = Jsont.mem objc "labels" View_labels.codec
  let codec = Jsont.obj objc
  let v ~range ~labels () =
    Jsont.(new_obj codec [memv mem_range range; memv mem_labels labels])

  let range = Jsont.get mem_range
  let labels = Jsont.get mem_labels
end

module Doc_views = struct
  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"Doc_views" ()
  let mem_type = Jsont.mem objc "type" D.doc_views_type_codec
  let mem_var_envs = Jsont.mem objc "var_envs" Var_envs.codec
  let mem_view_key = Jsont.mem objc "view_key" format_str_codec
  let mem_thumbs = Jsont.mem_opt objc "thumbs" format_str_codec
  let mem_images = Jsont.mem_opt objc "images" format_str_codec
  let mem_ocrs = Jsont.mem_opt objc "ocrs" format_str_codec
  let mem_thumbs_href = Jsont.mem objc "thumbs_href" format_str_codec
  let mem_images_href = Jsont.mem objc "images_href" format_str_codec
  let mem_view_labels =
    Jsont.mem_opt objc "view_labels" (Jsont.array View_label_range.codec)

  let codec = Jsont.obj objc
  let v ~type_ ~var_envs ~view_key ?thumbs ?images ?ocrs
    ~thumbs_href ~images_href ?view_labels () =
    Jsont.(new_obj codec [memv mem_type type_;
                          memv mem_var_envs var_envs;
                          memv mem_view_key view_key;
                          memv mem_thumbs thumbs;
                          memv mem_images images;
                          memv mem_ocrs ocrs;
                          memv mem_thumbs_href thumbs_href;
                          memv mem_images_href images_href;
                          memv mem_view_labels view_labels])

  let type_ = Jsont.get mem_type
  let var_envs = Jsont.get mem_var_envs
  let view_key = Jsont.get mem_view_key
  let thumbs = Jsont.get mem_thumbs
  let images = Jsont.get mem_images
  let ocrs = Jsont.get mem_ocrs
  let thumbs_href = Jsont.get mem_thumbs_href
  let images_href = Jsont.get mem_images_href
  let view_labels = Jsont.get mem_view_labels
end

type doc = Jsont.obj
let doc_objc = Jsont.objc ~kind:"Doc" ()
let doc_codec = Jsont.obj ~seal:false doc_objc

module Doc_subset = struct
  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"Doc_subset" ()
  let mem_var_envs = Jsont.mem objc "var_envs" Var_envs.codec
  let mem_key = Jsont.mem objc "key" format_str_codec
  let mem_doc = Jsont.mem objc "doc" doc_codec
  let codec = Jsont.obj objc

  let v ~var_envs ~key ~doc () =
    Jsont.(new_obj codec [memv mem_var_envs var_envs;
                          memv mem_key key;
                          memv mem_doc doc])

  let var_envs = Jsont.get mem_var_envs
  let key = Jsont.get mem_key
  let doc = Jsont.get mem_doc
end

module Doc_set = struct
  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"Doc_set" ()
  let mem_type = Jsont.mem objc "type" D.doc_set_type_codec
  let mem_indexes = Jsont.mem objc "indexes" (Jsont.array D.index_id_codec)
  let mem_set = Jsont.mem objc "set" (Jsont.array Doc_subset.codec)
  let codec = Jsont.obj objc
  let codec = codec
  let v ~type_ ~indexes ~set:s () =
    Jsont.(new_obj codec [memv mem_type type_;
                          memv mem_indexes indexes;
                          memv mem_set s])

  let type_ = Jsont.get mem_type
  let indexes = Jsont.get mem_indexes
  let set = Jsont.get mem_set
end

module Doc = struct
  type t = doc
  type doc_data = [ `Views of Doc_views.t | `Set of Doc_set.t ]
  type meta = Jsont.json

  let select_data =
    let doc_views_codec =
      let dec p = `Ok (`Views p :> doc_data) in
      let enc = function `Views d -> d | _ -> assert false in
      Jsont.view (dec, enc) Doc_views.codec
    in
    let doc_set_codec =
      let dec s = `Ok (`Set s :> doc_data) in
      let enc = function `Set s -> s | _ -> assert false in
      Jsont.view (dec, enc) Doc_set.codec
    in
    function
    | `Views -> doc_views_codec
    | `Set -> doc_set_codec

  let objc = doc_objc
  let mem_doc_type = Jsont.mem objc "doc_type" D.doc_type_codec
  let mem_doc_href = Jsont.mem objc "doc_href" D.(locv_codec uri_codec)
  let mem_doc_langs = Jsont.mem objc "doc_langs" (Jsont.array D.lang_codec)
  let mem_doc_data = Jsont.mem_match objc mem_doc_type "doc_data" select_data
  let mem_meta = Jsont.anon objc Jsont.json
  let codec = Jsont.obj ~seal:true objc

  let v ~doc_href ~doc_langs ~doc_data ~meta () =
    let doc_type = match doc_data with | `Views _ -> `Views | `Set _ -> `Set in
    let anons =
      let add_meta acc (k,v) = Jsont.anonv mem_meta k v :: acc in
      List.fold_left add_meta [] meta
    in
    Jsont.(new_obj codec ([memv mem_doc_type doc_type;
                           memv mem_doc_href doc_href;
                           memv mem_doc_langs doc_langs;
                           memv mem_doc_data doc_data] @ anons))

  let doc_type = Jsont.get mem_doc_type
  let doc_href = Jsont.get mem_doc_href
  let doc_langs = Jsont.get mem_doc_langs
  let doc_data = Jsont.get mem_doc_data
end


(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli.
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
