(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf
let err_lang l = str "`%s' is not a BCP 47 language tag" l
let err_basic r = str "`%s' is not a BCP 47 basic language range" r
let err_range r = str "`%s' is not a BCP 47 extended language range" r

let is_alpha i = 0x41 <= i && i <= 0x5A || 0x61 <= i && i <= 0x7A
let is_digit i = 0x30 <= i && i <= 0x39
let is_alnum i = is_alpha i || is_digit i
let is_bcp47_tag_list ~range s =
  let c = ref 0 in
  let first = ref true in
  try                   (*  parse (1*8alpha | '*') *('-' (1*8alnum | '*')) *)
    for i = 0 to String.length s - 1 do
      if s.[i] = '-' then
        (if !c <> 0 && !c <= 8 then (first := false; c := 0) else raise Exit)
      else if s.[i] = '*' && !c = 0 then
        (if range then c := 8 else raise Exit)
      else if !first then
        (if is_alpha (Char.code s.[i]) then incr c else raise Exit)
      else
      (if is_alnum (Char.code s.[i]) then incr c else raise Exit)
    done;
      !c <> 0 && !c <= 8
  with Exit -> false

let is_lang l = is_bcp47_tag_list ~range:false l
let is_basic_range r = r = "*" || is_bcp47_tag_list ~range:false r
let is_range r = is_bcp47_tag_list ~range:true r

let str_subtags l =
  let i = ref (String.length l - 1) in               (* start from the end. *)
  let ts = ref [] in
  while (!i >= 0) do
    let i' = try String.rindex_from l !i '-' with Not_found -> -1 in
    ts := (String.lowercase (String.sub l (i' + 1) (!i - i'))) :: !ts;
    i := i' - 1
  done;
  !ts

let subtags_equal ts ts' = ts = ts'
let subtags_compare ts ts' =
  let rec loop c ts ts' = match ts, ts' with
  | t :: ts, t' :: ts' ->
      if c <> 0 then loop c ts ts' else loop (compare t t') ts ts'
  | [], [] -> c
  | [], t' -> -1
  | t, [] -> 1
  in
  loop 0 ts ts'

(* Language tags *)

type t = string list

let of_string l = if is_lang l then `Ok (str_subtags l) else `Error (err_lang l)
let v l = match of_string l with
| `Ok l -> l
| `Error e -> invalid_arg e

let subtags l = l
let equal = subtags_equal
let compare = subtags_compare
let to_string l = String.concat "-" l
let pp ppf l = Format.pp_print_string ppf (to_string l)

(* Language ranges *)

type range = string list
let any = ["*"]
let to_range l = l

module Range = struct
  type t = range

  let of_string ?(basic = false) r = match basic with
  | true ->
      if is_basic_range r then `Ok (str_subtags r) else `Error (err_basic r)
  | false ->
      if is_range r then `Ok (str_subtags r) else `Error (err_range r)

  let v ?basic r = match of_string ?basic r with
  | `Ok r -> r
  | `Error e -> invalid_arg e

  let subtags r = subtags r
  let equal = subtags_equal
  let compare = subtags_compare
  let to_string r = String.concat "-" r
  let pp ppf r = Format.pp_print_string ppf (to_string r)
end

(* Language range matching *)

let matches l r =
  let rec loop ls rs = match ls, rs with
  | l :: ls, r :: rs when l = r ->  loop ls rs
  | (l :: ls as ls'), ("*" :: rs as rs') ->
      loop ls rs || loop ls rs' || loop ls' rs
  | [], "*" :: rs -> loop [] rs
  | ls, [] -> true
  | _, _ -> false
  in
  loop l r

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
