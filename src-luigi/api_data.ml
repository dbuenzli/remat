(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let err_not_dir f = Ui.log_err "%s: Not a directory" f
let err_unix f a e = Ui.log_err "%s %s: %s" f a (Unix.error_message e)
let err_sys e = Ui.log_err "%s" e

type dst = { db : Db.t; base : string }

let rec mkdir d =                                                (* not T.R. *)
  if Sys.file_exists d then
    (if Sys.is_directory d then true else (err_not_dir d; false))
  else
    let p = Filename.dirname d in
    let p_exists = if p = Filename.current_dir_name then true else mkdir p in
    if not p_exists then false else
    try Unix.mkdir d 0o777; true
    with Unix.Unix_error (e, f, a) -> (err_unix f a e); false

let create ?workers db base = { db; base }

let doc_dir = "d"
let repo = "repo.json"
let dst_path dst p = Filename.concat dst.base p
let doc_path db id = dst_path db (Filename.concat doc_dir (id ^ ".json"))

(* Encoder *)

type enc = { dst : dst; outf : string; e : Jsonm.encoder; }

let e_end e = ignore (Jsonm.encode e.e `End)
let e_lex e l = ignore (Jsonm.encode e.e (`Lexeme l))
let e_int e i = e_lex e (`Float (float i))
let e_string e s = e_lex e (`String s)
let e_name e n = e_lex e (`Name n)
let e_mem e n e_v v = e_name e n; e_v e v
let e_opt_mem e n e_v = function None -> () | Some v -> e_mem e n e_v v
let e_uri = e_string                                (* TODO PCT encode ? *)
let e_locales e_v e locs =
  let e_locale (l, v) = e_mem e l e_v v in
  e_lex e `Os; List.iter e_locale locs; e_lex e `Oe

let e_loc_str = e_locales e_string
let e_loc_uri = e_locales e_string
let e_locales_d e (ls, us) =
  let e_locale_d e l u =
    e_lex e `Os;
    e_mem e "locale" e_string l;
    e_mem e "ui_href" e_string u;
    e_lex e `Oe;
  in
  e_lex e `As; List.iter2 (e_locale_d e) ls us; e_lex e `Ae

let e_repo e = match Db.repo_d e.dst.db with
| None -> `Fail
| Some a ->
    match Db.doc_ids e.dst.db with
    | None -> `Fail
    | Some dids ->
        let open Db in
        e_lex e `Os;
        e_mem e "version" e_int a.version;
        e_mem e "locales" e_locales_d a.locales;
        e_mem e "name" e_loc_str a.name;
        (* e_name "indexes";  *)
        e_opt_mem e "search_href" e_uri a.search_href;
        e_mem e "publisher" e_loc_str a.publisher;
        e_mem e "publisher_href" e_loc_uri a.publisher_href;
        e_lex e `Oe;
        e_end e;
        `Ok

let publish dst =
  if not (mkdir dst.base && mkdir (dst_path dst doc_dir)) then `Fail else
  let outf = dst_path dst repo in
  try
    let oc = open_out outf in
    let e = { dst; outf; e = Jsonm.encoder (`Channel oc) } in
    e_repo e
  with
  | Sys_error e -> err_sys e; `Fail






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
