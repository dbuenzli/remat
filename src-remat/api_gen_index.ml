(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Bos

let base_href = Path.Rel.base "i"
let href p = Path.Rel.(base_href / p)

let repo_index_href id locale =
  href (strf "repo-%s-%s.json" id (Nlang.to_string locale))

let doc_index_href id did l =
  href (strf "doc-%s-%s-%s" did id (Nlang.to_string l))

let repo_index g id =
  let index = Descr.index (Api_gen.descr g) id in
  let index locale =
    let name =
      let miss = Jsont.default D.Ui_link.codec in
      let name = Ddescr.Index.name index in
      Api_gen.loc_data ~locale ~kind:"index" ~mem:"name" ~miss name
    in
    let node_href = Path.Rel.to_string (repo_index_href id locale) in
    Dapi.Index.v ~name ~node_href ()
  in
  Api_gen.locv g index

(* Generate indexes *)

(*
let make_doc_env gen vars doc_id =
  let doc, meta = Descr.doc gen.descr doc_id in
  let lookup mem_path =
    (Descr.lookup mem_path meta)
    |> R.reword_error_msg ~replace:true (fun e -> R.msgf "lookup: %s" e)
    |> Log.on_error_msg ~use:[]
  in
  String.Map.map lookup vars

let handle_doc_err did = function
| Ok v -> v | Error (`Msg err, v) -> Log.err "doc %s: %s" did err; v

let make_doc_ref gen locale doc_ref env smaps did =
  let doc, _ = Descr.doc gen.descr did in (* TODO reorganize to rem that *)
  let may_fmt fmt = match fmt with
  | None -> None | Some fmt ->
      Some (handle_doc_err did (Descr.format fmt ~env ~smaps))
  in
  let data_href = Path.Rel.to_string (doc_href did) in
  let ui_href =
    let uri_def = Jsont.default D.uri_codec in
    get_loc "doc_href" "doc" locale (Ddescr.Doc.doc_href doc) uri_def
  in
  let descr =
    handle_doc_err did (Descr.format (Ddescr.Doc_ref.descr doc_ref) ~env ~smaps)
  in
  let descr_extra = may_fmt (Ddescr.Doc_ref.descr_extra doc_ref) in
  let date = may_fmt (Ddescr.Doc_ref.date doc_ref) in
  let doc_type = match Ddescr.Doc_ref.doc_type doc_ref with
  | false -> None
  | true -> Some (Ddescr.Doc.doc_type doc)
  in
  let doc_count = match Ddescr.Doc_ref.doc_count doc_ref with
  | false -> None
  | true -> Some 0 (* TODO *)
  in
  Dapi.Doc_ref.v ~data_href ~ui_href ~descr ?descr_extra ?date ?doc_type
    ?doc_count ()

let make_doc_leaves gen locale key headings doc_ref vars smaps acc did =
  let env = make_doc_env gen vars did in
  let key = handle_doc_err did (Descr.format key ~env ~smaps) in
  let heading h =
    let label = Ddescr.Headings.label h in
    let label_href = Ddescr.Headings.label_href h in
    let label = handle_doc_err did (Descr.format label ~env ~smaps) in
    let label_href = match label_href with
    | None -> label
    | Some label_href ->
        handle_doc_err did (Descr.format label_href ~env ~smaps)
    in
    (label, label_href)
  in
  let doc_headings = List.rev (List.rev_map heading (headings)) in
  let doc_ref = make_doc_ref gen locale doc_ref env smaps did in
  let doc_node = doc_headings, doc_ref in
  String.Map.add key doc_node acc

let make_node (label, href) ~in_toc ~rev_childs =
  let heading = D.Ui_link.v ~text:label ~href () in
  let childs = match rev_childs with
  | `Headings l -> `Headings (List.rev l)
  | `Doc_refs l -> `Doc_refs (List.rev l)
  in
  Dapi.Index_node.v ~heading ~in_toc ~childs ()

let rec chop_common pattern ~from = match pattern, from with
| [], [] -> []
| (p, _) :: pattern, (f, _) :: from when p = f -> chop_common pattern ~from
| _, from -> from

let build_index_node idx_name dmap =
  let new_headings doc_ref to_recreate levels =
    let rec loop new_headings to_recreate levels =
      match to_recreate, levels with
      | [], levels ->
          let new_headings =
            let doc_ref = match doc_ref with None -> [] | Some d -> [d] in
            match new_headings with
            | [] -> (* index with no headers *)
                begin match levels with
                | [h, `Doc_refs []] -> [h, `Doc_refs doc_ref]
                | [h, `Headings []] -> [h, `Doc_refs doc_ref]
                | _ -> assert false
                end
            | (h, `Headings []) :: hs -> (h, `Doc_refs doc_ref) :: hs
            | _ -> assert false
          in
          new_headings, levels
      | (h :: to_recreate),
        (close_h, rev_childs) :: (up_h, `Headings up_childs) :: rest ->
          let closed = make_node close_h ~in_toc:false ~rev_childs in
          let new_headings = (h, `Headings []) :: new_headings in
          let levels = (up_h, `Headings (closed :: up_childs)) :: rest in
          loop new_headings to_recreate levels
      | (h :: to_recreate), (top :: [] as levels) -> (* init *)
          let new_headings = (h, `Headings []) :: new_headings in
          loop new_headings to_recreate levels
      | _ -> assert false
    in
    loop [] to_recreate levels
  in
  let add_doc _ (doc_headings, doc_ref) (last_headings, levels) =
    match chop_common last_headings ~from:doc_headings with
    | [] -> (* Same heading *)
        begin match levels with
        | (h, `Doc_refs rrefs) :: levels ->
            last_headings, (h, `Doc_refs (doc_ref :: rrefs)) :: levels
        | (h, `Headings []) :: [] -> (* no headings *)
            last_headings, (h, `Doc_refs ([doc_ref])) :: []
        | _ -> assert false
        end
    | to_close -> (* New headings *)
        let new_headings, levels =
          new_headings (Some doc_ref) to_close levels
        in
        doc_headings, (List.append new_headings levels)
  in
  let init =
    [], [(D.Ui_link.text idx_name, D.Ui_link.href idx_name), `Headings []]
  in
  let last, levels = String.Map.fold add_doc dmap init in
  match new_headings None last levels with
  | _, [h, rev_childs] -> make_node h ~in_toc:false ~rev_childs
  | _ -> assert false

let make_index_node gen locale id doc_ids =
  let idx = Descr.index gen.descr id in
  let idx_name =  (* TODO code replication *)
    let name_default = Jsont.default D.Ui_link.codec in
    get_loc "name" "index" locale (Ddescr.Index.name idx) name_default
  in
  let smaps = Ddescr.Index.string_maps idx in
  let vars = Ddescr.Index.doc_vars idx in
  let key =
    let fmt_def = Jsont.default Ddescr.format_str_codec in
    get_loc "key" "index" locale (Ddescr.Index.key idx) fmt_def
  in
  let headings =
    get_loc "headings" "index" locale (Ddescr.Index.headings idx) []
  in
  let doc_ref =
    let dref_def = Jsont.default Ddescr.Doc_ref.codec in
    get_loc "doc_ref" "index" locale (Ddescr.Index.doc_ref idx) dref_def
  in
  let make_doc_leaves =
    make_doc_leaves gen locale key headings doc_ref vars smaps
  in
  let doc_leaves = List.fold_left make_doc_leaves String.Map.empty doc_ids in
  Printf.printf "index node: %s\n" id;
  build_index_node idx_name doc_leaves
*)

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
