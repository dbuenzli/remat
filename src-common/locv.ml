(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf
let err_no_match l = str "no match found for `%s'" (Nlang.to_string l)

(* Localized values *)

module Rmap = Map.Make (Nlang.Range)
type 'a t = 'a Rmap.t

let empty = Rmap.empty
let is_empty = Rmap.is_empty
let any v = Rmap.add Nlang.any v Rmap.empty
let mem r lv = Rmap.mem r lv
let add lv r v = Rmap.add r v lv
let rem lv r = Rmap.remove r lv

let _find l lv =
  let find_match r v acc =
    if not (Nlang.matches l r) then acc else
    match acc with
    | None -> Some (r, v)
    | Some (r', _) -> if Nlang.Range.compare r r' > 0 then Some (r, v) else acc
  in
  match Rmap.fold find_match lv None with
  | None -> None
  | Some (r, v) -> Some (r, v)

let find l lv = match _find l lv with None -> None | Some (_, v) -> Some v
let find_range l lv = match _find l lv with None -> None | Some (r, _) -> Some r
let get l lv = match find l lv with
| None -> invalid_arg (err_no_match l)
| Some l -> l

let get_range l lv = match find_range l lv with
| None -> invalid_arg (err_no_match l)
| Some l -> l

let fold f acc lv = Rmap.fold (fun r v acc -> f acc r v) lv acc
let of_list l = List.fold_left (fun acc (r, v) -> Rmap.add r v acc) Rmap.empty l
let to_list = Rmap.bindings

module Msg = struct
  type t = string

  let v m = m
  let get m = m
  let of_string m = `Ok m
  let to_string m = m

  module Map = struct
    type msg = t
    type key = string
    module M = Map.Make(String)

    type t = { locale : Nlang.range;
               map : msg M.t }

    let empty locale = { locale; map = M.empty }
    let locale m = m.locale
    let add m k msg = { m with map = M.add k msg m.map }
    let rem m k = { m with map = M.remove k m.map }
    let mem m k = M.mem k m.map
    let get ?absent m k = try M.find k m.map with
    | Not_found ->
        match absent with
        | None -> v (str "%s:%s: no translation"
                       (Nlang.Range.to_string m.locale) k)
        | Some m -> m

    let fold f acc m =  M.fold (fun k v acc -> f acc k v) m.map acc

  end

  type map = Map.t
end

type msgs = Msg.map t


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
