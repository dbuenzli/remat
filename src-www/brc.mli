(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Browser graphical controls.

    {b Warning.} This module is experimental and subject to change. *)

open React

(** {1 Links} *)

type link

val link : ?id:Br.str -> ?classes:Br.str list signal -> ?title:Br.str signal ->
  href:Br.str signal -> Br.El.child list signal -> link

val link_el : link -> Br.el
val link_actuate : link -> Br.str React.event

(** {1 Link list} *)



(** {1 List element} *)

(*
type list_element

val list_element :
  ?id:Br.str -> ?classes:Br.str list signal -> ?title:Br.str signal ->
  ?eq:('a -> 'a -> bool) ->
  label:('a -> Br.el) ->
  ?init:'a -> set:'a event -> 'a list signal -> list_element

val list_element_el : Br.el
val list_element_value : list_element -> 'a signal
*)


(*
type button
val button : ?classes:Br.str list -> label:Br.str signal -> button
val button_el : button -> Br.el
*)

(*
(** {1 Menu} *)

type menu
val selector : ?id:Br.str -> ?classes:Br.str list signal ->
  ?title:Br.str signal -> label:Br.el signal ->
  items:(Br.el * 'a) list signal -> unit -> menu

val menu_el : menu -> Br.el

(** {1 Selector} *)

val selector : ?eq:'a -> 'a -> bool ->
  ?id:Br.str -> ?classes:Br.str list signal ->
  ?title:Br.str signal -> 'a -> (Br.el * 'a) list signal ->
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
