(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

module Doc_ref = struct
  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"Doc_ref" ()
  let mem_data_href = Jsont.mem objc "data_href" D.uri_codec
  let mem_ui_href = Jsont.mem objc "doc_href" D.uri_codec
  let mem_descr = Jsont.mem objc "descr" Jsont.nat_string
  let mem_descr_extra = Jsont.mem_opt objc "descr_extra" Jsont.nat_string
  let mem_date = Jsont.mem_opt objc "date" D.date_fuzzy_codec
  let mem_doc_type = Jsont.mem_opt objc "doc_type" D.doc_type_codec
  let mem_doc_count = Jsont.mem_opt objc "doc_count" Jsont.int_strict
  let codec = Jsont.obj objc

  let v ~data_href ~ui_href ~descr ?descr_extra ?date ?doc_type ?doc_count () =
    Jsont.(new_obj codec [ memv mem_data_href data_href;
                           memv mem_ui_href ui_href;
                           memv mem_descr descr;
                           memv mem_descr_extra descr_extra;
                           memv mem_date date;
                           memv mem_doc_type doc_type;
                           memv mem_doc_count doc_count ])

  let data_href = Jsont.get mem_data_href
  let ui_href = Jsont.get mem_ui_href
  let descr = Jsont.get mem_descr
  let descr_extra = Jsont.get mem_descr_extra
  let date = Jsont.get mem_date
  let doc_type = Jsont.get mem_doc_type
  let doc_count = Jsont.get mem_doc_count
end

module Index = struct
  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"Index" ()
  let mem_name = Jsont.mem objc "name" D.Ui_link.codec
  let mem_node_href = Jsont.mem objc "node_href" D.uri_codec
  let mem_descr = Jsont.mem_opt objc "descr" Jsont.nat_string
  let codec = Jsont.obj objc

  let v ~name ~node_href ?descr () =
    Jsont.(new_obj codec [ memv mem_name name;
                           memv mem_node_href node_href;
                           memv mem_descr descr ])

  let name = Jsont.get mem_name
  let node_href = Jsont.get mem_node_href
  let descr = Jsont.get mem_descr
end

module Index_node = struct
  type t = Jsont.obj
  type child_type = [ `Headings | `Doc_refs ]
  type childs = [ `Headings of t list | `Doc_refs of Doc_ref.t list ]

  let objc = Jsont.objc ~kind:"Index_node" ()
  let codec = Jsont.obj ~seal:false objc

  let child_type_codec =
    Jsont.enum ["headings", `Headings; "doc_refs", `Doc_refs ]

  let childs_headings_codec =
    let dec hs = `Ok (`Headings hs :> childs) in
    let enc = function `Headings hs -> hs | _ -> assert false in
    Jsont.view (dec, enc) (Jsont.array codec)

  let childs_doc_refs_codec =
    let dec ds = `Ok (`Doc_refs ds :> childs) in
    let enc = function `Doc_refs ds -> ds | _ -> assert false in
    Jsont.view (dec, enc) (Jsont.array Doc_ref.codec)

  let mem_heading = Jsont.mem objc "heading" D.Ui_link.codec
  let mem_in_toc = Jsont.mem objc "in_toc" Jsont.bool
  let mem_child_type = Jsont.mem objc "child_type" child_type_codec
  let mem_childs =
    let select = function
    | `Headings -> childs_headings_codec
    | `Doc_refs -> childs_doc_refs_codec
    in
    Jsont.mem_match objc mem_child_type "childs" select

  let codec = Jsont.obj ~seal:true objc

  let v ~heading ~in_toc ~childs () =
    let child_type = match childs with
    | `Headings _ -> `Headings
    | `Doc_refs _ -> `Doc_refs
    in
    Jsont.(new_obj codec [ memv mem_heading heading;
                           memv mem_in_toc in_toc;
                           memv mem_child_type child_type;
                           memv mem_childs childs; ])

  let heading = Jsont.get mem_heading
  let in_toc = Jsont.get mem_in_toc
  let child_type = Jsont.get mem_child_type
  let childs = Jsont.get mem_childs
end

module Repo = struct
  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"Repo" ()
  let mem_version = Jsont.mem objc "version" Jsont.int
  let mem_name = Jsont.mem objc "name" D.locv_str_codec
  let mem_publisher =
    Jsont.mem objc "publisher" D.(locv_codec Ui_link.codec)

  let mem_ui_locales =
    Jsont.mem objc "ui_locales" (Jsont.array D.Ui_locale.codec)

  let mem_indexes =
    let i = D.locv_codec Index.codec in
    Jsont.mem objc "indexes" (Jsont.array i)

  let mem_search_href = Jsont.mem_opt objc "search_href" D.uri_codec
  let codec = Jsont.obj objc

  let v ~version ~name ~publisher ~ui_locales ~indexes ?search_href () =
    Jsont.(new_obj codec [ memv mem_version version;
                           memv mem_name name;
                           memv mem_publisher publisher;
                           memv mem_ui_locales ui_locales;
                           memv mem_indexes indexes;
                           memv mem_search_href search_href;])

  let version = Jsont.get mem_version
  let name = Jsont.get mem_name
  let publisher = Jsont.get mem_publisher
  let indexes = Jsont.get mem_indexes
  let ui_locales = Jsont.get mem_ui_locales
  let search_href = Jsont.get mem_search_href
end

module Doc_view = struct
  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"Doc_view" ()
  let mem_label = Jsont.mem_opt objc "label" Jsont.nat_string
  let mem_thumb_href = Jsont.mem objc "thumb_href" D.uri_codec
  let mem_image_href = Jsont.mem objc "iamge_href" D.uri_codec
  let codec = Jsont.obj objc
  let v ?label ~thumb_href ~image_href () =
    Jsont.(new_obj codec [memv mem_label label;
                          memv mem_thumb_href thumb_href;
                          memv mem_image_href image_href;])

  let label = Jsont.get mem_label
  let thumb_href = Jsont.get mem_thumb_href
  let image_href = Jsont.get mem_image_href
end

module Doc_views = struct
  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"Doc_views" ()
  let mem_type = Jsont.mem objc "type" D.doc_views_type_codec
  let mem_views = Jsont.mem objc "views" (Jsont.array Doc_view.codec)
  let codec = Jsont.obj objc
  let v ~type_ ~views () =
    Jsont.(new_obj codec [memv mem_type type_;
                          memv mem_views views ])

  let type_ = Jsont.get mem_type
  let views = Jsont.get mem_views
end

module Doc_set = struct
  type t = Jsont.obj
  let objc = Jsont.objc ~kind:"Doc_set" ()
  let mem_type = Jsont.mem objc "type" D.doc_set_type_codec
  let mem_indexes =
    let i = D.locv_codec Index.codec in
    Jsont.mem objc "indexes" (Jsont.array i)

  let codec = Jsont.obj objc
  let v ~type_ ~indexes () =
    Jsont.(new_obj codec [memv mem_type type_;
                          memv mem_indexes indexes])

  let type_ = Jsont.get mem_type
  let indexes = Jsont.get mem_indexes
end


module Doc = struct
  type t = Jsont.obj
  type doc_data = [ `Views of Doc_views.t | `Set of Doc_set.t ]

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

  let objc = Jsont.objc ~kind:"Doc" ()
  let mem_doc_type = Jsont.mem objc "doc_type" D.doc_type_codec
  let mem_doc_href = Jsont.mem objc "doc_href" D.(locv_codec uri_codec)
  let mem_doc_langs = Jsont.mem objc "doc_langs" (Jsont.array D.lang_codec)
  let mem_doc_data = Jsont.mem_match objc mem_doc_type "doc_data" select_data
  let codec = Jsont.obj objc

  let v ~doc_href ~doc_langs ~doc_data () =
    let doc_type = match doc_data with | `Views _ -> `Views | `Set _ -> `Set in
    Jsont.(new_obj codec [memv mem_doc_type doc_type;
                          memv mem_doc_href doc_href;
                          memv mem_doc_langs doc_langs;
                          memv mem_doc_data doc_data])

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
