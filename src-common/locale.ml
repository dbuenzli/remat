(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

type t = string

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
let subtags l =
  let i = ref (String.length l - 1) in               (* start from the end. *)
  let ts = ref [] in
  while (!i >= 0) do
    let i' = try String.rindex_from l !i '-' with Not_found -> -1 in
    ts := (String.lowercase (String.sub l (i' + 1) (!i - i'))) :: !ts;
    i := i' - 1
  done;
  !ts

let compare r r' =
  let rec loop c ts ts' = match ts, ts' with
  | t :: ts, t' :: ts' ->
      if c <> 0 then loop c ts ts' else loop (compare t t') ts ts'
  | [], [] -> c
  | [], t' -> -1
  | t, [] -> 1
  in
  loop 0 (subtags r) (subtags r')

let equal r r' = r = r'
let matches r r' =
  let rec loop ls rs = match ls, rs with
  | l :: ls, r :: rs when l = r ->  loop ls rs
  | (l :: ls as ls'), ("*" :: rs as rs') ->
      loop ls rs || loop ls rs' || loop ls' rs
  | [], "*" :: rs -> loop [] rs
  | ls, [] -> true
  | _, _ -> false
  in
  loop (subtags r) (subtags r')


(* Docs of matching

 `P "A locale matches a range if the subtags of the range are a prefix
       of the locale. The wildcard subtag * matches any sequence of
       subtags (as such it cannot be used to denote that a certain number of
       subtags will appear in a matching locale). Subtag comparison is case
       insensitive.";
   `P "For example the locale de-CH-1996-x-mobile-tablet could match
       any of these locale range :";
   `P "de-CH-1996-x-mobile-tablet, de-CH-1997-*-x-mobile-tablet"; `Noblank;
   `P "de-CH-1996-x-mobile, de-CH-1996-*, de-*-CH-x-*, de-*-*-tablet";`Noblank;
   `P "de-CH-1996, de-*-1996, de-*-tablet"; `Noblank;
   `P "de-CH, de-*, *-CH, *-tablet"; `Noblank;
   `P "de, *";
   `P "But it wouldn't match any of these:";
   `P "fr, de-DE, *-FR, de-*-DE-*, de-*-goethe";
   `P "Given a set of matching ranges $(b,w.locales) selects the most
       \"precise\" one, that is the range with most subtags. To resolve ties,
       ranges are sorted lexicographically per subtag with * smaller than
       anything in increasing order and the greatest one is taken. For example
       for the locale de-CH if the ranges de, *-CH, de-* and de-CH are
       selected, de-CH is taken and if *-CH and de-* are selected,
       de-* is taken.";
   `P "Note that RFC 4647 defines no algorithm to lookup a language range
       with a language tag, hence the above definition.";

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
