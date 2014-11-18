(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Repository database. *)

(** {1 Description types} *)

type loc = string * (int * int) * (int * int)
(** The type for location specification. Filename and character range. *)

type 'a def = loc * 'a

(** {2 Basic types} *)

type 'a locales = (Locale.t * 'a) list
(** The type for localized data. *)

type uri = string
(** The type for uris. *)

type index_id = string
(** The type for index ids. *)

type doc_id = string
(** The type for document ids. *)

(** {2 Formatter}  *)

type lookup_substring =
  [ `S_Y | `S_YY | `S_YYYY | `S_M | `S_MM | `S_d | `S_dd | `S_e
  | `S_first of int ]
(** The type for format lookup. *)

type lookup =
  { path : string list;
    array : bool;
    substring : lookup_substring option;
    map : (string * string) list option;
    partial : bool; }
(** The type for format lookups. *)

type format_el = [`String of string | `Lookup of lookup ]
type formatter = format_el list
(** The type for formatters. *)

(** {2 Repo_d} *)

type repo_d =
  { version : int;
    locales : Locale.t list * uri list;
    name : string locales;
    indexes : index_id list;
    search_href : uri option;
    publisher : string locales;
    publisher_href : uri locales; }
(** The type for repo descriptions. *)

(** {2 Index_d} *)

type headings_d =
  { label : formatter locales;
    toc : [`All | `Multiples | `Values of string list ];
    toc_multiple : int; }
(** The type for heading descriptions. *)

type index_d =
  { label : string locales;
    i_ui_href : string;
    sort_key : formatter locales;
    synopses : [ `Isbd | `Formatter of formatter locales ];
    headings : headings_d list; }
(** The type for index descriptions. *)

type doc_d = { title : string; }
(** The type for document descriptions. *)

(** {1 Archive database}

    {b Note.} When lookups fail errors are reported on the database log. *)

type t
(** The type for databases. *)

val create : string -> t
(** [create dir] is a database that reads descriptions from [dir],
    errors and warnings are logged using [log]. *)

val repo_d : t -> repo_d option
(** [repo_d db] is the repo description of [db].
    [None] is returned in case of error. *)

val index_d : t -> index_id -> index_d option
(** [index_d db id] is the index description of [db] with id [id].
    [None] is returned in case of error. *)

val doc_d : t -> doc_id -> doc_d option
(** [index_d db id] is the index description of [db] with id [id].
    [None] is returned in case of error. *)

val doc_ids : t -> doc_id list option
(** [doc_ids db] is the list of document ids in the database. *)


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
