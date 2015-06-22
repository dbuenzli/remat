(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Base remat data types.

    These types are common to both {!Ddescr} and {!Dapi}. *)

(** {1 Version} *)

val version : int
(** [version] is the remat data model version. *)

(** {2 URIs ({{:../describe.html#uri}spec})} *)

type uri = Jsont.nat_string
val uri_codec : uri Jsont.codec

(** {2 Lang ({{:../describe.html#uri}spec})} *)

val lang_codec : Nlang.t Jsont.codec

(** {2 Document IDs ({{:../describe.html#doc_id}spec})} *)

type doc_id = Jsont.nat_string
val doc_id_codec : doc_id Jsont.codec

(** {2 Index IDs ({{:../describe.html#index_id}spec})} *)

type index_id = Jsont.nat_string
val index_id_codec : doc_id Jsont.codec

(** {2 Document types ({{:../describe.html#doc_type}spec})} *)

type doc_type = [`Set | `Views]
val doc_type_codec : doc_type Jsont.codec

(** {2 Document views types ({{:../describe.html#doc_views_type}spec})} *)

type doc_views_type = [`Monograph | `Serial_issue | `Image ]
val doc_views_type_codec : doc_views_type Jsont.codec

(** {2 Document set types ({{:../describe.html#doc_set_type}spec})} *)

type doc_set_type = [`Collection | `Serial ]
val doc_set_type_codec : doc_set_type Jsont.codec

(** {2 Date fuzzy ({{:../describe.html#dates}spec})} *)

type date_fuzzy = Jsont.nat_string
val date_fuzzy_codec : date_fuzzy Jsont.codec

(** {2 Localized values ({{:../describe.html#localized}spec}) } *)

val locv_codec : ?default:'a Locv.t -> ?kind:string ->
  'a Jsont.codec -> 'a Locv.t Jsont.codec

val locv_str_codec : Jsont.nat_string Locv.t Jsont.codec
val locv_uri_codec : uri Locv.t Jsont.codec
val locv_msgs_codec : Locv.msgs Jsont.codec

(** {2 UI link} *)

(** UI links ({{:../describe.html#ui_link}spec}) *)
module Ui_link : sig
  type t
  val codec : t Jsont.codec
  val v : text:Jsont.nat_string -> href:uri -> unit -> t
  val text : t -> Jsont.nat_string
  val href : t -> uri
end

(** {2 UI locale} *)

(** UI locale ({{:../describe.html#ui_locale}spec}) *)
module Ui_locale : sig
  type t
  val codec : t Jsont.codec
  val v : locale:Nlang.t -> href:uri -> unit -> t
  val locale : t -> Nlang.t
  val href : t -> uri
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
