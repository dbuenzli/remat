(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Bos_unix


(* File system writes *)

let path g p = Path.(Api_gen.dest g // p)

let encode_file file codec v =
  let encode oc () =
    let e = Jsonm.encoder (`Channel oc) in
    let e = Jsont.encoder e codec v in
    let rec loop () = match Jsont.encode e with
    | `Ok -> R.ok ()
    | `Partial -> loop ()
    in
    loop ()
  in
  OS.File.with_outf encode file ()

(* Repo *)

let write_repo g =
  let repo = Api_gen_repo.v g in
  let file = path g Api_gen_repo.repo_href in
  encode_file file Dapi.Repo.codec repo

(* Repo indexes *)

let write_repo_index gen doc_ids id =
  failwith "TODO"
(*
  let write locale =
    let index = make_index_node gen locale id doc_ids in
    let file = gen_path gen (index_repo_href id locale) in
    encode_file file Dapi.Index_node.codec index
    |> Log.on_error_msg ~use:()
  in
  List.iter write gen.locales
*)

let write_repo_indexes g = Ok ()
(*
  let index_ids = Api_gen.repo_index_ids g in
  let doc_ids = Api_gen.repo_doc_ids g in
  let dir = path g Api_gen_index.base_href in
  OS.Dir.create dir
  >>= fun () ->
  R.ok (List.iter (write_repo_index g doc_ids) (Ddescr.Repo.indexes r))
*)
(*

let write_docs gen =
  let write gen did =
    let doc = make_doc gen did in
    let file = gen_path gen (doc_href did) in
    encode_file file Dapi.Doc.codec doc
    |> Log.on_error_msg ~use:()
  in
  let r = Descr.repo gen.descr in
  let doc_ids = Descr.doc_ids gen.descr in
  R.ok (List.iter (write gen) doc_ids)
*)



let write g =
  OS.Dir.create (Api_gen.dest g)
  >>= fun () -> write_repo g
  >>= fun () -> write_repo_indexes g
(*
  >>= fun () -> OS.Dir.create (gen_path gen doc_base)
  >>= fun () -> write_docs gen
*)
  >>= fun () -> R.ok ()



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
