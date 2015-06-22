(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Bos

type t =
  { descr : Descr.t;
    dest : path;
    locales : Nlang.t list; }

let create descr dest =
  let r = Descr.repo descr in
  let locales = List.map D.Ui_locale.locale (Ddescr.Repo.ui_locales r) in
  { descr; dest; locales }


let descr g = g.descr
let dest g = g.dest
let locales g = g.locales

let repo g = Descr.repo g.descr
let repo_index_ids g = Ddescr.Repo.indexes (Descr.repo g.descr)
let repo_doc_ids _ = [](*  failwith "TODO" *)
(*
Descr.doc_ids g.des
  let descr = Api_gen.descr g in
  lte repo = Api
  let doc_ids = Descr.doc_ids descr in
*)

(* Localisation handling *)

(* FIXME maybe we could try to report errors at parsing time. *)

let loc_data ~locale ~kind ~mem ~miss lv = match Locv.find locale lv with
| Some n -> n
| None ->
    Log.err
      "no data for locale %a in member %s of %s, using placeholder data"
      Nlang.pp locale mem kind;
    miss

let locv g f =
  let add_locale acc l = Locv.add acc (Nlang.to_range l) (f l) in
  List.fold_left add_locale Locv.empty g.locales



(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli.
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
