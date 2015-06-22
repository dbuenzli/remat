(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Repository description. *)

(** {1 Repository description} *)

open Rresult
open Bos

type t
(** The type for repository descriptions. *)

val create : Path.t -> (t, R.msg) result
(** [create dir] is a repository description that reads from [dir]. *)

val repo : t -> Ddescr.Repo.t
(** [repo r] is the repo description of [r]. *)

val index : t -> D.index_id -> Ddescr.Index.t
(** [index r id] is the index with id [id] in [r]. *)

val index_ids : t -> D.index_id list
(** [index_ids r] is the list of index ids in the description. *)

val doc : t -> D.doc_id -> Ddescr.Doc.t * Ddescr.Doc.meta
(** [doc r id] is the document description with id [id] in [r]. *)

val doc_ids : t -> D.doc_id list
(** [doc_ids db] is the list of document ids in the description. *)

(** {1 Member lookup} *)

val lookup : Ddescr.mem_path -> Jsont.json -> (string list, R.msg) result

(** {1 Formatting} *)


val format : ?buf:Buffer.t -> Ddescr.format_str ->
  env:Pat.env -> smaps:string String.Map.t String.Map.t ->
  (string, R.msg * string) result

val formats : ?buf:Buffer.t -> Ddescr.format_str ->
  env:Pat.env -> smaps:string String.Map.t String.Map.t ->
  (string list, R.msg * string list) result

(** {1 Variable environments} *)

val file_scan : Ddescr.format_str -> Bos.Pat.env list


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
