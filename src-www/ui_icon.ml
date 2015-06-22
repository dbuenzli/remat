(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Font Awesome icons *)

type name = Br.str

let angle_down = Br.str "\xEF\x84\x87"                             (* U+F107 *)
let bars = Br.str "\xEF\x83\x89"                                   (* U+F0C9 *)
let caret_down = Br.str "\xEF\x83\x97"                             (* U+F0D7 *)
let caret_right = Br.str "\xEF\x83\x9A"                            (* U+F0DA *)
let chevron_circle_down = Br.str "\xEF\x84\xBA"                    (* U+F13A *)
let chevron_down = Br.str "\xEF\x81\xB8"                           (* U+F078 *)
let expand = Br.str "\xEF\x81\xA5"                                 (* U+F065 *)
let plus = Br.str "\xEF\x81\xA7"                                   (* U+F067 *)
let plus_circle = Br.str "\xEF\x81\x95"                            (* U+F055 *)
let plus_square = Br.str "\xEF\x83\xBE"                            (* U+F0FE *)
let plus_square_o = Br.str "\xEF\x86\x96"                          (* U+F196 *)
let search = Br.str "\xEF\x80\x82"                                 (* U+F002 *)

let v ?(classes = []) icon_name =
  let classes = (Br.str "icon") :: classes in
  Br.El.(v span ~classes [`Txt icon_name])

let space = `Txt (Br.str " ")


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
