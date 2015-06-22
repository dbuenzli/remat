(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Remat API JSON data model. *)

(** {1 API data model} *)

(** Document references ({{:../api.html#doc_ref}spec}) *)
module Doc_ref : sig
  type t
  val codec : t Jsont.codec
  val v :
    data_href:D.uri ->
    ui_href:D.uri ->
    descr:Jsont.nat_string ->
    ?descr_extra:Jsont.nat_string ->
    ?date:D.date_fuzzy ->
    ?doc_type:D.doc_type ->
    ?doc_count:int ->
    unit -> t

  val data_href : t -> D.uri
  val ui_href : t -> D.uri
  val descr : t -> Jsont.nat_string
  val descr_extra : t -> Jsont.nat_string option
  val date : t -> D.date_fuzzy option
  val doc_type : t -> D.doc_type option
  val doc_count : t -> int option
end

(** Index ({{:../api.html#index}spec}) *)
module Index : sig
  type t
  val codec : t Jsont.codec
  val v : name:D.Ui_link.t -> node_href:D.uri ->
    ?descr:Jsont.nat_string -> unit -> t

  val name : t -> D.Ui_link.t
  val node_href : t -> D.uri
  val descr : t -> Jsont.nat_string option
end

(** Index node ({{:../api.html#index_node}spec}) *)
module Index_node : sig
  type t
  type child_type = [ `Headings | `Doc_refs ]
  type childs = [ `Headings of t list | `Doc_refs of Doc_ref.t list ]
  val codec : t Jsont.codec
  val v : heading:D.Ui_link.t -> in_toc:bool -> childs:childs -> unit -> t
  val heading : t -> D.Ui_link.t
  val in_toc : t -> bool
  val child_type : t -> [ `Headings | `Doc_refs ]
  val childs : t -> childs
end

(** Repository ({{:../api.html#repo}spec}) *)
module Repo : sig
  type t
  val codec : t Jsont.codec
  val v :
    version:int ->
    name:Jsont.nat_string Locv.t ->
    publisher:D.Ui_link.t Locv.t ->
    ui_locales:D.Ui_locale.t list ->
    indexes:Index.t Locv.t list ->
    ?search_href:D.uri ->
    unit -> t

  val version : t -> int
  val name : t -> Jsont.nat_string Locv.t
  val publisher : t -> D.Ui_link.t Locv.t
  val ui_locales : t -> D.Ui_locale.t list
  val indexes : t -> Index.t Locv.t list
  val search_href : t -> D.uri option
end

(** Doc view ({{:../api.html#doc_view}spec}) *)
module Doc_view : sig
  type t
  val codec : t Jsont.codec
  val v : ?label:Jsont.nat_string -> thumb_href:D.uri -> image_href:D.uri ->
    unit -> t
  val label : t -> Jsont.nat_string option
  val thumb_href : t -> D.uri
  val image_href : t -> D.uri
end

(** Doc views ({{:../api.html#doc_views}spec}) *)
module Doc_views : sig
  type t
  val codec : t Jsont.codec

  val v : type_:D.doc_views_type -> views:Doc_view.t list -> unit -> t
  val type_ : t -> D.doc_views_type
  val views : t -> Doc_view.t list
end

(** Doc sets ({{:../api.html#doc_set}spec}) *)
module Doc_set : sig
  type t
  val codec : t Jsont.codec
  val v : type_:D.doc_set_type -> indexes:Index.t Locv.t list -> unit -> t
  val type_ : t -> D.doc_set_type
  val indexes : t -> Index.t Locv.t list
end

(** Docs ({{:../api.html#doc}spec}) *)
module Doc : sig
  type doc_data = [ `Views of Doc_views.t | `Set of Doc_set.t ]
  type t
  val codec : t Jsont.codec

  val v : doc_href:D.uri Locv.t -> doc_langs:Nlang.t list ->
    doc_data:doc_data -> unit -> t
  val doc_type : t -> D.doc_type
  val doc_href : t -> D.uri Locv.t
  val doc_langs : t -> Nlang.t list
  val doc_data : t -> doc_data
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
