(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf

type msg = [ `Sys_error of string ]
type date = (int * (int * int option) option) option (* YYYY, MM, DD *)
type image = string
type group = string * (date * image list) list
type t = group list

let rsplit_string s sep =
  let rec split acc j =
    let i = try (String.index_from s j sep) with Not_found -> String.length s in
    let acc' = String.sub s j (i - j) :: acc in
    if (i = String.length s) then acc' else split acc' (i + 1)
  in
  split [] 0

let split_fname f =                (* split into prefix, postfix, date, page *)
  let f = try Filename.chop_extension f with Invalid_argument _ -> f in
  let ss = rsplit_string f '_' in
  let cat = String.concat "_" in
  let p_num = function [] -> `EOS
    | s :: ss -> try `Num (int_of_string s, ss) with Failure _ -> `Str (s, ss)
  in
  let rec next_num ss =
    let rec aux acc ss = match p_num ss with
    | `Num (n, ss) -> `Num (cat acc, n, ss)
    | `EOS -> `EOS (cat acc)
    | `Str (s, ss) -> aux (s :: acc) ss
    in
    aux [] ss
  in
  match next_num ss with
  | `EOS pre -> pre, "", None, 1
  | `Num (post, page, ss) -> match next_num ss with
    | `EOS pre -> pre, post, None, page
    | `Num (post', n1, ss) ->
        let post = match post', post with
        | "", post -> post
        | post, "" -> post
        | post', post -> cat [post'; post ]
        in
        match next_num ss with
        | `Num ("", n2, ss) ->
            begin match next_num ss with
            | `Num ("", n3, ss) ->
                cat (List.rev ss), post, Some (n3, Some (n2, Some n1)), page
            | _ -> cat (List.rev ss), post, Some (n2, Some (n1, None)), page
            end
        | _ -> cat (List.rev ss), post, Some (n1, None), page

let of_files nmap fs =
  let nmap n = try List.assoc n nmap with Not_found -> n in
  let add acc f =
    let pre, post, date, page = split_fname f in
    ((str "%s %s" (nmap pre) post), date, page, f) :: acc
  in
  let rec group gs n ds d ps = function
    | (n', d', _, f) :: fs when n <> n' ->
        let g = n, List.rev ((d, List.rev ps) :: ds) in
        group (g :: gs) n' [] d' [f] fs
    | (_, d', _, f) :: fs when d <> d' ->
        group gs n ((d, List.rev ps) :: ds) d' [f] fs
    | (_, _, _, f) :: fs ->
        group gs n ds d (f :: ps) fs
    | [] -> List.rev gs
  in
  match List.sort compare (List.fold_left add [] fs) with [] -> []
  | (n, d, _, f) :: fs -> group [] n [] d [f] fs

let pr = Printf.fprintf
let pr_date oc d = match d with
| Some (y, Some (m, Some d)) -> pr oc "%04d-%02d-%02d" y m d
| Some (y, Some (m, None)) -> pr oc "%04d-%02d" y m
| Some (y, None) -> pr oc "%04d" y
| None -> pr oc "unknown date"

let pr_groups oc b =
  let pr_file oc f = pr oc "  %s\n" f in
  let pr_el oc (d, fs) = pr oc "  %a\n" pr_date d; List.iter (pr_file oc) fs in
  let pr_group oc (n, els) = pr oc "* %s\n" n; List.iter (pr_el oc) els in
  List.iter (pr_group stdout) b

let pr_escape oc s = pr oc "%s" s
let uri_group_name n =
  let b = Bytes.unsafe_of_string (String.lowercase n) in
  let max = Bytes.length b - 1 in
  for i = 0 to max do if b.[i] = ' ' then Bytes.set b i '-' done;
  Bytes.unsafe_to_string b

let output_html title pr_body oc b =
  let id = "id1" in
  let base = "http://heyho.local/cira/browser/" in
  pr oc "<!DOCTYPE html><html lang=\"en\" id=\"%s\">" id;
  pr oc "<head><meta charset=\"utf-8\">";
  pr oc "<base href=\"%s\">" base;
  pr oc "<title>%a</title>" pr_escape title;
  pr oc "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\">";
  pr oc "</head><body>%a</body></html>" pr_body b

let output_group_files dst g = ()
let output_group_index log dst b =
  let pr_group oc (n, _) =
    pr oc "<li><a href='%s'>%s</a></li>" (uri_group_name n) n
  in
  let pr_groups oc b = List.iter (pr_group oc) b in
  let pr_body oc b =
    pr oc "<nav></nav>";
    pr oc "<h1 class=\"title\">Index</h1>";
    pr oc "<ol>%a</ol>" pr_groups b;
  in
  try
    let oc = open_out (Filename.concat dst "index.html") in
    output_html "OCR Index" pr_body oc b
  with
  | Sys_error e -> log (`Sys_error e)

let output_files ?(log = fun _ -> ()) b dst =
  output_group_index log dst b;
  List.iter (output_group_files dst) b;
  `Ok

(* "Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec" *)

(*
2012 Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
      01  11
      05  12
      07  23
*)


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
