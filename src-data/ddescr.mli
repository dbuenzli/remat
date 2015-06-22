(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Remat repository description data. *)

(** {1 Base types} *)

open Bos

(** {2 String maps ({{:../describe.html#string_map}spec})} *)

val string_map_codec : ?default:'a String.Map.t -> ?kind:string ->
  'a Jsont.codec -> 'a String.Map.t Jsont.codec

(** {2 Format strings ({{:../describe.html#format_str}spec})} *)

type format_str = Jsont.nat_string
val format_str_codec : format_str Jsont.codec

(** {2 Member paths ({{:../describe.html#mem_path}spec})} *)

type mem_path = string list
val mem_path_codec : mem_path Jsont.codec

(** {1 Objects} *)

(** Integer range ({{:../describe.html#integer_range}spec}) *)
module Integer_range : sig
  type t
  val codec : t Jsont.codec
  val v : start:int -> ?end_:int -> unit -> t
  val start : t -> int
  val end_ : t -> int option
end

(** Document references ({{:../describe.html#doc_ref}spec}) *)
module Doc_ref : sig
  type t
  val codec : t Jsont.codec
  val v : descr:format_str -> ?descr_extra:format_str -> ?date:format_str ->
    ?doc_type:bool -> ?doc_count:bool -> unit -> t
  val descr : t -> format_str
  val descr_extra : t -> format_str option
  val date : t -> format_str option
  val doc_type : t -> bool
  val doc_count : t -> bool
end

(** Variable environments ({{:../describe.html#var_envs}spec}) *)
module Var_envs : sig
  type t
  val codec : t Jsont.codec
  val v : ?file_scan:format_str ->
    ?ranges:Integer_range.t list String.Map.t ->
    ?set:string list String.Map.t -> unit -> t
  val file_scan : t -> format_str option
  val ranges : t -> Integer_range.t list String.Map.t option
  val set : t -> string list String.Map.t option
end

(** Repository ({{:../describe.html#repo}spec}) *)
module Repo : sig
  type t
  val codec : t Jsont.codec
  val v : version:int -> name:Jsont.nat_string Locv.t ->
    publisher:D.Ui_link.t Locv.t ->
    ui_locales:D.Ui_locale.t list ->
    indexes:D.index_id list ->
    ?search_href:D.uri -> unit -> t
  val version : t -> int
  val name : t -> Jsont.nat_string Locv.t
  val publisher : t -> D.Ui_link.t Locv.t
  val ui_locales : t -> D.Ui_locale.t list
  val indexes : t -> D.index_id list
  val search_href : t -> D.uri option
end

(** Headings ({{:../describe.html#headings}spec}) *)
module Headings : sig
  type toc = [`All | `Multiples | `Custom ]
  type t
  val codec : t Jsont.codec
  val v : label:format_str -> label_href:format_str -> ?toc:toc ->
    ?toc_multiple:int -> ?toc_custom:string list -> unit -> t
  val label : t -> format_str
  val label_href : t -> format_str option
  val toc : t -> toc option
  val toc_multiple : t -> int option
  val toc_custom : t -> string list option
end

(** Index ({{:../describe.html#index}spec}) *)
module Index : sig
  type t
  val codec : t Jsont.codec
  val v : name:D.Ui_link.t Locv.t ->
    string_maps:string String.Map.t String.Map.t ->
    doc_vars:mem_path String.Map.t ->
    key:format_str Locv.t ->
    headings:Headings.t list Locv.t ->
    doc_ref:Doc_ref.t Locv.t -> unit -> t
  val name : t -> D.Ui_link.t Locv.t
  val string_maps : t -> string String.Map.t String.Map.t
  val doc_vars : t -> mem_path String.Map.t
  val key : t -> format_str Locv.t
  val headings : t -> Headings.t list Locv.t
  val doc_ref : t -> Doc_ref.t Locv.t
end

(** View_labels ({{:../describe.html#view_labels}spec}) *)
module View_labels : sig
  type label_type = [ `Arabic | `Roman | `Front_cover | `Back_cover | `Custom ]
  type t
  val codec : t Jsont.codec
  val v : type_:label_type -> ?custom:format_str -> ?start:int ->
    ?folio:bool -> unit -> t
  val type_ : t -> label_type
  val custom : t -> format_str option
  val start : t -> int option
  val folio : t -> bool
end

(** View_label_range ({{:../describe.html#view_label_range}spec}) *)
module View_label_range : sig
  type t
  val codec : t Jsont.codec
  val v : range:Integer_range.t -> labels:View_labels.t -> unit -> t
  val range : t -> Integer_range.t
  val labels : t -> View_labels.t
end

(** Doc_views ({{:../describe.html#doc_views}spec}) *)
module Doc_views : sig
  type t
  val codec : t Jsont.codec
  val v : type_:D.doc_views_type ->
    var_envs:Var_envs.t -> view_key:format_str ->
    ?thumbs:format_str -> ?images:format_str ->
    ?ocrs:format_str -> thumbs_href:format_str ->
    images_href:format_str -> ?view_labels:View_label_range.t list ->
    unit -> t
  val type_ : t -> D.doc_views_type
  val var_envs : t -> Var_envs.t
  val view_key : t -> format_str
  val thumbs : t -> format_str option
  val images : t -> format_str option
  val ocrs : t -> format_str option
  val thumbs_href : t -> format_str
  val images_href : t -> format_str
  val view_labels : t -> View_label_range.t list option
end

type doc
(** See {!Doc.t} *)

(** Doc_subset ({{:../describe.html#doc_subset}spec}) *)
module Doc_subset : sig
  type t
  val codec : t Jsont.codec
  val v : var_envs:Var_envs.t -> key:format_str -> doc:doc -> unit -> t
  val var_envs : t -> Var_envs.t
  val key : t -> format_str
  val doc : t -> doc
end

(** Doc_set ({{:../describe.html#doc_set}spec}) *)
module Doc_set : sig
  type t
  val codec : t Jsont.codec
  val v : type_:D.doc_set_type -> indexes:D.index_id list ->
    set:Doc_subset.t list -> unit -> t
  val type_ : t -> D.doc_set_type
  val indexes : t -> D.index_id list
  val set : t -> Doc_subset.t list
end

(** Doc ({{:../describe.html#doc}spec}) *)
module Doc : sig
  type doc_data = [ `Views of Doc_views.t | `Set of Doc_set.t ]
  type meta = Jsont.json
  type t
  val codec : t Jsont.codec

  val v : doc_href:D.uri Locv.t -> doc_langs:Nlang.t list ->
    doc_data:doc_data -> meta:(string * Jsont.json) list  -> unit -> t
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
