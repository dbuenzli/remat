(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let doc_base = Path.Rel.base "d"
let doc_dir gen = Path.(gen.dest // doc_base)
let doc_href id = Path.Rel.(doc_base / strf "%s.json" id)
let doc_setmem_href id sid = doc_href (strf "%s-%s" id sid)


let doc_views g views meta =
  let type_ = Ddescr.Doc_views.type_ views in
  let views = [] (* Jsont.default Dapi.Doc_view *) in
  Dapi.Doc_views.v ~type_ ~views ()

let doc_set g set meta =
  let type_ = Ddescr.Doc_set.type_ set in
  let indexes = [] in
  Dapi.Doc_set.v ~type_ ~indexes ()

let v g did =
  let doc, meta = Descr.doc (Api_gen.descr g) did in
  let doc_href = Ddescr.Doc.doc_href doc in
  let doc_langs = Ddescr.Doc.doc_langs doc in
  let doc_data = match Ddescr.Doc.doc_data doc with
  | `Views views -> `Views (doc_views g views meta)
  | `Set set -> `Set (doc_set g set meta)
  in
  Dapi.Doc.v ~doc_href ~doc_langs ~doc_data ()


(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
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
