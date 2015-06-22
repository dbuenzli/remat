(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Bos

(* Repository description *)

type t =
  { dir : Path.t;
    mutable repo : Ddescr.Repo.t option;
    mutable index_ids : D.index_id list option;
    indexes : (D.index_id, Ddescr.Index.t) Hashtbl.t;
    mutable doc_ids : D.doc_id list option;
    docs : (D.doc_id, Ddescr.Doc.t * Ddescr.Doc.meta) Hashtbl.t; }

(* Description filename lookup *)

let warn_junk_file = format_of_string "suspicious file `%a` in %s directory"
let err_miss_repo p _ = R.msgf "no repository description file `%a'" Path.pp p
let err_miss_dir dir p _ = R.msgf "missing %s directory `%a'" dir Path.pp p
let err_miss_file k id p _ =
  R.msgf "%s `%s': missing description file `%a'" k id Path.pp p

let lookup_file err_msg f =
  (OS.File.exists ~err:true f >>= fun _ -> R.ok f)
  |> R.reword_error_msg ~replace:true (err_msg f)

let lookup_dir err_msg d =
  (OS.Dir.exists ~err:true d >>= fun _ -> R.ok d)
  |> R.reword_error_msg ~replace:true (err_msg d)

let repo_file d = lookup_file err_miss_repo Path.(d.dir / "repo.json")
let index_path d = Path.(d.dir / "i")
let index_dir d = lookup_dir (err_miss_dir "index") (index_path d)
let index_file d id =
  let err = err_miss_file "index" id in
  lookup_file err Path.(index_path d / strf "%s.json" id)

let doc_path d = Path.(d.dir / "d")
let doc_dir d = lookup_dir (err_miss_dir "document") (doc_path d)
let doc_file d id =
  let err = err_miss_file "document" id in
  lookup_file err Path.(doc_path d / strf "%s.json" id)

(* Description decoder *)

let decode_file file codec =
  let decode ic () =
    let d = Jsonm.decoder (`Channel ic) in
    let d = Jsont.decoder ~dups:`Error ~unknown:`Error d codec in
    let rec loop () = match Jsont.decode d with
    | `Ok v -> R.ok v
    | `Await -> loop ()
    | `Error (loc, e) ->
        let err = (Jsont.error_to_string e) in
        Log.show "%a:%a: %s" Path.pp file Fmt.pp_range loc err;
        loop ()
    in
    loop ()
  in
  OS.File.with_inf decode file ()

let create dir =
  OS.Dir.exists ~err:true dir
  >>= fun _ -> R.ok { dir;
                       repo = None;
                       index_ids = None;
                       indexes = Hashtbl.create 100;
                       doc_ids = None;
                       docs = Hashtbl.create 1000; }

let rec repo d = match d.repo with
| Some r -> r
| None ->
    (repo_file d >>= fun file -> decode_file file Ddescr.Repo.codec)
    |> Log.on_error_msg ~use:Jsont.(invalid_def (default Ddescr.Repo.codec))
    |> fun (_, r) -> d.repo <- Some r; r

let find_ids kind dir =
  let add_id acc p =
    if Path.has_ext `Json p then Path.(basename (rem_ext p)) :: acc else
    (Log.warn warn_junk_file Path.pp p kind; acc)
  in
  (dir
   >>= OS.Dir.contents
   >>= fun paths -> R.ok (List.fold_left add_id [] paths))
  |> Log.on_error_msg ~use:[]

let index_ids d = match d.index_ids with
| Some ids -> ids
| None ->
    let ids = find_ids "index" (index_dir d) in
    d.index_ids <- Some ids; ids

let index d id =
  match try Some (Hashtbl.find d.indexes id) with Not_found -> None with
  | Some i -> i
  | None ->
      (index_file d id >>= fun file -> decode_file file Ddescr.Index.codec)
      |> Log.on_error_msg ~use:Jsont.(invalid_def (default Ddescr.Index.codec))
      |> fun (_, i) -> Hashtbl.add d.indexes id i; i

let doc_ids d = match d.doc_ids with
| Some ids -> ids
| None ->
    let ids = find_ids "document" (doc_dir d) in
    d.doc_ids <- Some ids; ids

let doc d id = (* FIXME see if modification of jsont can avoid double parse *)
  match try Some (Hashtbl.find d.docs id) with Not_found -> None with
  | Some d -> d
  | None ->
      (doc_file d id
       >>= fun file -> decode_file file Ddescr.Doc.codec
       >>= fun (_, doc) -> decode_file file Jsont.json
       >>= fun (_, meta) -> R.ok (doc, meta))
      |> Log.on_error_msg ~use:(Jsont.(default Ddescr.Doc.codec), `O [])
      |> fun doc -> Hashtbl.add d.docs id doc; doc

(* Member lookup *)

let path_to_str ps = String.concat "." ps
let value_type = function
| `Null -> "null" | `Bool _ -> "boolean" | `Float _ -> "number"
| `String _ -> "string" | `A _ -> "array" | `O _ -> "object"

let err_find_type path seen j =
  R.error_msgf "path %s stops at %s: value of type %s"
    (path_to_str path) (path_to_str seen) (value_type j)

let err_find_name path seen =
  R.error_msgf "path %s stops at %s: no such member."
    (path_to_str path) (path_to_str seen)

let json_find path j =
  let rec loop j seen = function
  | [] -> R.ok j
  | p :: ps ->
      match j with
      | `O mems ->
          begin match try Some (List.assoc p mems) with Not_found -> None with
          | None -> err_find_name path (List.rev (p :: seen))
          | Some j -> loop j (p :: seen) ps
          end
      | j -> err_find_type path (List.rev (p :: seen)) j
  in
  loop j [] path

let lookup_to_str = function
| `Bool b -> R.ok (strf "%b" b)
| `Float f -> R.ok (strf "%g" f)
| `String s -> R.ok s
| `A _ | `O _ | `Null as v ->
    R.error_msgf "unexpected %s in member data" (value_type v)

let lookup path obj =
  json_find path obj >>= function
  | `A vs ->
      let rec loop acc = function
      | v :: vs -> lookup_to_str v >>= fun s -> loop (s :: acc) vs
      | [] -> R.ok (List.rev acc)
      in
      loop [] vs
  | v -> lookup_to_str v >>= fun s -> R.ok [s]

(* Formatting

   TODO better error reports, correct string extractors.
*)

let parse_fuzzy_date s =
  let is_digit c = (0x0030 <= c && c <= 0x0039) || c = 0x23 (* # *) in
  let check_digits n s =
    let len = String.length s in
    if len <> n then false else
    try
      for i = 0 to len - 1 do
        if not (is_digit (Char.code s.[i])) then raise Exit
      done;
      true
    with Exit -> false
  in
  match String.split ~sep:"-" s with
  | [y; m; d] when check_digits 4 y && check_digits 2 m && check_digits 2 d ->
      R.ok (y, Some m, Some d)
  | [y; m] when check_digits 4 y && check_digits 2 m  ->
      R.ok (y, Some m, None)
  | [y] when check_digits 4 y ->
      R.ok (y, None, None)
  | _ ->
      R.error_msgf "could not parse fuzzy date (%s)" s

let map_todo m =
  let err = R.msgf "map %s is unimplemented" m in
  Ok (fun s -> R.error (err, s))

(* let err fmt = Printf.ksprintf (fun e -> R.error e) fmt *)
let err_map ~use fmt = Printf.ksprintf (fun e -> R.error (`Msg e, use)) fmt

let map_case var kind = match kind with
| "less" | "lower" | "upper" -> map_todo ("case_" ^ kind)
| _ -> R.error_msgf "variable $(%s): unknown case map kind `%s`" var kind

(* TODO implement dates correctly *)

let map_date_y s = match parse_fuzzy_date s with
| Error err -> Error (err, s)
| Ok (y, _, _) -> Ok y

let map_date_yy s = match parse_fuzzy_date s with
| Error err -> Error (err, s)
| Ok (y, _, _) -> Ok (String.sub y 2 2)

let map_date_yyyy s = match parse_fuzzy_date s with
| Error err -> Error (err, s)
| Ok (y, _, _) -> Ok y

let map_date_m s = match parse_fuzzy_date s with
| Error err -> Error (err, s)
| Ok (_, m, _) -> Ok (match m with None -> "#" | Some m -> m)

let map_date_mm s = match parse_fuzzy_date s with
| Error err -> Error (err, s)
| Ok (_, m, _) -> Ok (match m with None -> "##" | Some m -> m)

let map_date_d s = match parse_fuzzy_date s with
| Error err -> Error (err, s)
| Ok (_, _, d) -> Ok (match d with None -> "#" | Some m -> m)

let map_date_dd s = match parse_fuzzy_date s with
| Error err -> Error (err, s)
| Ok (_, _, d) -> Ok (match d with None -> "##" | Some m -> m)

let map_date var kind = match kind with
| "Y" -> Ok map_date_y | "YY" -> Ok map_date_yy | "YYYY" -> Ok map_date_yyyy
| "M" -> Ok map_date_m | "MM" -> Ok map_date_mm
| "d" -> Ok map_date_d | "dd" -> Ok map_date_dd
| "e" -> map_todo "date_e"
| _ -> R.error_msgf "variable $(%s): unknown date map kind `%s`" var kind

let map_letter var n = match R.int_of_string n with
| None -> R.error_msgf "variable $(%s): unknown letter map kind `%s`" var n
| Some n ->
    let map s = Ok (if n > String.length s then s else (String.sub s 0 n)) in
    Ok map

let map_int var count = match R.int_of_string count with
| None -> R.error_msgf "variable $(%s): unknown int map kind `%s`" var count
| Some count ->
    let map s =
      let fmt count i = Printf.sprintf "%0*d" count i in
      try Ok (fmt count (int_of_string s)) with
      | Failure _ ->
          err_map ~use:(fmt count 0)
            "variable $(%s): value `%s` not an int" var s
    in
    Ok map

let map_id_find var smaps id = match (String.Map.find id smaps) with
| None -> R.error_msgf "variable $(%s): unknown map id `%s`" var id
| Some m -> Ok m

let map_id var smaps id =
  map_id_find var smaps id >>= fun m ->
  let map s = match String.Map.find s m with
  | Some v -> Ok v
  | None ->
      err_map ~use:s
        "variable $(%s): map id `%s` could not map `%s`" var id s
  in
  Ok map

let pmap_id var smaps id = match map_id_find var smaps id with
| Error _ as e -> e
| Ok m ->
    let map s = match String.Map.find s m with
    | None -> Ok s
    | Some s -> Ok s
    in
    Ok map

let get_map var smaps m = match String.cut ~sep:"_" (String.trim m) with
| Some ("case", kind) -> map_case var kind
| Some ("letter", n) ->  map_letter var n
| Some ("date", kind) -> map_date var kind
| Some ("int", count) -> map_int var count
| Some ("map", id) -> map_id var smaps m
| Some ("pmap", id) -> pmap_id var smaps m
| None | _ -> R.error_msgf "variable $(%s): unknown map `%s`" var m

let lookup_var env smaps var_spec = (* TODO splicing, de-uglify *)
  let r = match String.split ~sep:"," var_spec with
  | var :: maps ->
      let add_map acc m = match acc with
      | Error _ as e -> e
      | Ok maps ->
          match get_map var smaps m with
          | Error _ as e -> e
          | Ok m -> Ok (m :: maps)
      in
      begin match List.fold_left add_map (Ok []) maps with
      | Error err -> Error (err, "MAPERROR")
      | Ok maps -> Ok (String.trim var, List.rev maps)
      end
  | _ ->
      Error (R.msgf "var `$(%s)`: illegal format variable." var_spec, "ILLEGAL")
  in
  match r with
  | Error _ as e -> e
  | Ok (var, maps) ->
      match String.Map.find var env with
      | None | Some [] ->
          Error (R.msgf "var `%s`: undefined variable: `$(%s)'" var_spec var,
                  "UNDEFINED")
      | Some [v] ->
          let apply acc m = match acc with
          | Error _ as e -> e
          | Ok s -> m s
          in
          List.fold_left apply (Ok v) maps
      | Some l ->
          Error (R.msgf "var `%s`: unspliced multiple value" var_spec,
                  "UNSPLICED")

let format ?buf fmt ~env ~smaps = failwith "TODO"
(*
  let buf = match buf with Some b -> b | None -> Buffer.create 255 in
  let err = ref (`Msg "") in
  let lookup_var var_spec = match lookup_var env smaps var_spec with
  | Error (e, v) -> err := e; v
  | Ok v -> v
  in
  Buffer.clear buf; Buffer.add_substitute buf lookup_var fmt;
  let data = Buffer.contents buf in
  if !err <> (`Msg "") then Error (!err, data) else Ok data
*)

let formats ?buf fmt ~env ~smaps = failwith "TODO"

(*

let rec product vss =                 (* ordered cartesian product of lists. *)
  let rec push_v acc v = function
  | l :: lists -> push_v ((v :: l) :: acc) v lists
  | [] -> acc
  in
  let rec push_vs acc lists = function
  | v :: vs -> push_vs (push_v acc v lists) lists vs
  | [] -> acc
  in
  let rec loop acc = function
  | vs :: vss -> loop (push_vs [] (List.rev acc) (List.rev vs)) vss
  | [] -> acc
  in
  if vss = [] then [] else loop [[]] (List.rev vss)

let format fmt env = (* FIXME better error report *)
  let lookup_var env var =
    match try Some (List.assoc var env) with Not_found -> None with
    | None ->
        (* FIXME this shouldn't occur here *)
        Log.err "variable %s undefined" var; "UNDEFINED"
    | Some l -> l
  in

  let rec assigns acc = function
  | [] -> acc
  | (name, Error e) :: vars ->
      Log.err "var %s lookup error: %s" name e;
      assigns ([(name, "ERROR")] :: acc) vars
  | (name, Ok vs) :: vars  ->
      assigns ((List.map (fun v -> (name, v)) vs) :: acc) vars
  in
  let vars = Ddescr.Formatter.vars fmt in
  let assigns = assigns [] (List.map (fun (k, l) -> k, lookup l j) vars) in
  let envs = product assigns in
  let format = Ddescr.Formatter.format fmt in
  let add_run b acc run =
    Buffer.clear b;
    Buffer.add_substitute b (lookup_var run) format;
    Buffer.contents b :: acc
  in
  let b = Buffer.create 255 in
  List.fold_left (add_run b) [] envs

let format_str fmt j =
  (* FIXME report error in case of list ? *)
  String.concat "" (format fmt j)
*)

(* Variable environements *)

let cache = Hashtbl.create 255

type fmt = [`Lit of string | `Var of string ] list
let parse_fmt ?buf s =
  try
    let b = match buf with
    | None -> Buffer.create 255 | Some buf -> Buffer.clear buf; buf
    in
    let acc = ref [] in
    let flush b = let s = Buffer.contents b in (Buffer.clear b; s) in
    let flush_lit b =
      if Buffer.length b <> 0 then acc := `Lit (flush b) :: !acc
    in
    let state = ref `Lit in
    for i = 0 to String.length s - 1 do match !state with
    | `Lit ->
        begin match s.[i] with
        | '$' -> state := `Dollar
        | c -> Buffer.add_char b c
        end
    | `Dollar ->
        begin match s.[i] with
        | '$' -> state := `Lit; Buffer.add_char b '$'
        | '(' -> state := `Var; flush_lit b;
        | _ -> raise Exit
        end
    | `Var ->
        begin match s.[i] with
        | ')' -> state := `Lit; acc := (`Var (flush b)) :: !acc;
        | c -> Buffer.add_char b c
        end
    done;
    if !state <> `Lit then raise Exit else
    (flush_lit b; Ok (List.rev !acc))
  with Exit -> Error (strf "malformed format: `%s`" s)


let cache = Hashtbl.create 255

let file_scan pat = try Hashtbl.find cache pat with
| Not_found ->
    (OS.Path.unify (Path.of_string pat)
     >>= fun envs -> R.ok (List.rev_map snd envs))
    |> Log.on_error_msg ~use:[]
    |> fun envs -> Hashtbl.add cache pat envs; envs





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
