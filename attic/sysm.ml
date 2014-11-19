(*---------------------------------------------------------------------------
   Copyright 2013, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Wg

let err_not_dir d = Wlog.msg "%s: Not a directory" d
let err_unix f a e = Wlog.msg "%s %s: %s" f a (Unix.error_message e)
let err_sys e = Wlog.msg "%s" e

(* Environment *)

let getenv k = try Some (Sys.getenv k) with Not_found -> None

(* File system operations *)

let rec mkdir d =                                                (* not T.R. *)
  if Sys.file_exists d then
    (if Sys.is_directory d then true else (Wlog.err (err_not_dir d); false))
  else
    let p = Filename.dirname d in
    let p_exists = if p = Filename.current_dir_name then true else mkdir p in
    if not p_exists then false else
    try Unix.mkdir d 0o777; true
    with Unix.Unix_error (e, f, a) -> Wlog.err (err_unix f a e); false

let rename f1 f2 =
  try Unix.rename f1 f2; true
  with Unix.Unix_error (e, f, a) -> Wlog.err (err_unix f a e); false

let tmp_file f = try
  let temp_dir = Filename.dirname f in
  Some (Filename.temp_file ~temp_dir (Filename.basename f ^ ".") ".tmp")
with Sys_error e -> Wlog.err (err_sys e); None

(* Finding file *)

let make_relative r f =                                        (* very ugly. *)
  let lr = String.length r and lf = String.length f in
  let last = lr - 1 in
  if lf < lr || last < 0 then f else
  try
    for i = 0 to last do if r.[i] <> f.[i] then raise Exit done;
    if lr = lf then Filename.current_dir_name else
    let s =
      if f.[lr] <> Filename.dir_sep.[0] then raise Exit else
      if r.[last] = Filename.dir_sep.[0] then String.sub f lr (lf - lr) else
      String.sub f (lr + 1) (lf - lr - 1)
    in
    if s = "" then Filename.current_dir_name else s
  with Exit -> f

let readdir d = try Array.to_list (Sys.readdir d) with Sys_error _ -> []
let is_dir d = try Sys.is_directory d with Sys_error _ -> false
let find_file dirs file =
  let in_dir file d = List.mem file (readdir d) in
  try Some (Filename.concat (List.find (in_dir file) dirs) file)
  with Not_found -> None

let find_file_rec dirs file =
  let rec aux file = function
    | (d :: ds) :: up ->
	let files = readdir d in
	if List.mem file files then Some (Filename.concat d file) else
	let dirs = List.filter is_dir (List.map (Filename.concat d) files) in
	aux file (dirs :: ds :: up)
    | [] :: [] -> None
    | [] :: up -> aux file up
    | _ -> assert false
  in
  aux file (dirs :: [])

let fold_files_rec dirs f acc =
  let rec aux f acc = function
    | (d :: ds) :: up ->
	let files = List.rev (List.rev_map (Filename.concat d) (readdir d)) in
	let dirs, files = List.partition is_dir files in
	let acc = List.fold_left f acc files in
	aux f acc (dirs :: ds :: up)
    | [] :: [] -> acc
    | [] :: up -> aux f acc up
    | _ -> assert false
  in
  aux f acc (dirs :: [])

(* IO brackets *)

let with_channel f x ~finally = try let r = f x in finally x; r
with e -> try finally x; raise e with _ -> raise e

let with_inf inf err f = try
  let ic = if inf = "-" then stdin else open_in_bin inf in
  let close ic = if inf = "-" then () else
  (Wlog.debug (Wlog.msg "closing: %s" inf); close_in ic)
  in
  with_channel f ic ~finally:close
with Sys_error e -> Wlog.err (err_sys e); err

let with_outf outf err f = try
  let oc = if outf = "-" then stdout else open_out_bin outf in
  let close oc = if outf = "-" then () else close_out oc in
  with_channel f oc ~finally:close
with Sys_error e -> Wlog.err (err_sys e); err

let with_outf_pp outf fmt = try
  let oc = if outf = "-" then stdout else open_out_bin outf in
  let close oc = if outf = "-" then () else close_out oc in
  let close_ppf oc ppf = Format.pp_print_flush ppf (); close oc in
  let ppf = Format.formatter_of_out_channel oc in
  try Format.kfprintf (close_ppf oc) ppf fmt
  with e -> try close oc; raise e with _ -> raise e
with Sys_error e ->
  Wlog.err (err_sys e); Format.ifprintf Format.std_formatter fmt (* nop *)

let with_out_path outf err f =
  if not (mkdir (Filename.dirname outf)) then err else
  with_outf outf err f

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

   3. Neither the name of the Daniel C. Bünzli nor the names of
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
