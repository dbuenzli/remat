(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let err_e () = invalid_arg "empty rectangle"

type r = { ox : float; oy : float; w : float; h : float }
type t = E | R of r

let empty = E
let v ox oy w h = R { ox; oy; w; h }
let vi ox oy w h = v (float ox) (float oy) (float w) (float h)

let o = function E -> err_e () | R r -> r.ox, r.oy
let ox = function E -> err_e () | R r -> r.ox
let oy = function E -> err_e () | R r -> r.oy
let size = function E -> err_e () | R r -> r.w, r.h
let w = function E -> err_e () | R r -> r.w
let h = function E -> err_e () | R r -> r.h
let zero = v 0. 0. 0. 0.
let unit = v 0. 0. 1. 1.
let of_ltrb_i l t r b = vi l t (r - l) (t - b)

(* Functions. *)

let min = o
let minx = ox
let miny = oy
let max = function E -> err_e () | R r -> r.ox +. r.w, r.oy +. r.h
let maxx = function E -> err_e () | R r -> r.ox +. r.w
let maxy = function E -> err_e () | R r -> r.oy +. r.h
let mid = function
  | E -> err_e () | R r -> (r.ox +. 0.5 *. r.w), (r.oy +. 0.5 *. r.h)

let midx = function E -> err_e () | R r -> r.ox +. 0.5 *. r.w
let midy = function E -> err_e () | R r -> r.oy +. 0.5 *. r.h
let bottom_left = min
let bottom_right = function E -> err_e () | R r -> (r.ox +. r.w), r.oy
let top_left = function E -> err_e () | R r -> r.ox, (r.oy +. r.h)
let top_right = max
let area = function E -> 0. | R r -> r.w *. r.h
let inter r r' = match r, r' with
| E, _ | _, E -> E
| R r, R r' ->
    let l = r.ox in let r_ = l +. r.w in
    let l' = r'.ox in let r_' = l' +. r'.w in
    if (r_ < l') || (r_' < l) then E else
    let b = r.oy in let t = b +. r.h in
    let b' = r'.oy in let t' = b' +. r'.h in
    if (t < b') || (t' < b) then E else
    let ox = if l > l' then l else l' in
    let oy = if b > b' then b else b' in
    let w = (if r_ < r_' then r_ else r_') -. ox in
    let h = (if t < t' then t else t') -. oy in
    v ox oy w h

let union r r' = match r, r' with
| E, r | r, E -> r
| R r, R r' ->
    let ox = if r.ox < r'.ox then r.ox else r'.ox in
    let oy = if r.oy < r'.oy then r.oy else r'.oy in
    let w =
      let r = r.ox +. r.w in let r' = r'.ox +. r'.w in
      (if r > r' then r else r') -. ox
    in
    let h =
      let t = r.oy +. r.h in let t' = r'.oy +. r'.h in
      (if t > t' then t else t') -. oy
    in
    v ox oy w h

let inset (dx,dy) r = match r with
| E -> E
| R r ->
    let ox = r.ox +. dx in
    let oy = r.oy +. dy in
    let w = r.w -. 2. *. dx in
    let h = r.h -. 2. *. dy in
    if w < 0. || h < 0. then E else
    v ox oy w h

let round r = match r with
| E -> E
| R r ->
    let ox = floor r.ox in
    let oy = floor r.oy in
    let w = if (r.w = 0. && ox <> r.ox) then 1. else ceil r.w in
    let h = if (r.h = 0. && oy <> r.oy) then 1. else ceil r.h in
    v ox oy w h

let map_f f = function E -> E | R r -> v (f r.ox) (f r.oy) (f r.w) (f r.h)

(* Predicates and comparisons *)

let is_empty = function E -> true | R _ -> false
let is_pt = function E -> false | R r -> r.w = 0. && r.h = 0.
let is_seg = function
  | E -> false
  | R r -> (r.w = 0. && r.h <> 0.) || (r.w <> 0. && r.h = 0.)

let isects r r' = match r, r' with
| E, _ | _, E -> false
| R r, R r' ->
    let l = r.ox in let r_ = l +. r.w in
    let l' = r'.ox in let r_' = l' +. r'.w in
    if (r_ < l') || (r_' < l) then false else
    let b = r.oy in let t = b +. r.h in
    let b' = r'.oy in let t' = b' +. r'.h in
    if (t < b') || (t' < b) then false else
    true

let subset r r' = match r, r' with
| r, E -> false
| E, r -> true
| R r, R r' ->
    (r'.ox <= r.ox) && (r'.oy <= r.oy) && (r.w <= r'.w) && (r.h <= r'.h)

let mem (x, y) r = match r with
| E -> false
| R r ->
    (r.ox <= x) && (x <= r.ox +. r.w) &&
    (r.oy <= y) && (y <= r.oy +. r.h)

let equal r r' = r = r'
let equal_f eq r r' = match r, r' with
| E, E -> true
| E, _ | _, E -> false
| R r, R r' ->
    eq r.ox r'.ox && eq r.oy r'.oy && eq r.w r'.w && eq r.h r'.h

let compare r r' = Pervasives.compare r r'
let compare_f cmp  r r' = match r, r' with
| E, E -> 0
| E, _ -> -1
| _, E -> 1
| R r, R r' ->
    let c = cmp r.ox r'.ox in if c <> 0 then c else
    let c = cmp r.oy r'.oy in if c <> 0 then c else
    let c = cmp r.w r'.w in if c <> 0 then c else
    cmp r.h r'.h

(* Printers. *)

let print ppf = function
| E -> Format.fprintf ppf "@[<1>(rect@ empty)@]"
| R r ->
    Format.fprintf ppf
    "@[<1>(rect @[<1>(o@ %g@ %g)@] @[<1>(size@ %g@ %g)@]@]"
      r.ox r.oy r.w r.h

let to_string r =
  Format.fprintf Format.str_formatter "%a" print r;
  Format.flush_str_formatter ()

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
