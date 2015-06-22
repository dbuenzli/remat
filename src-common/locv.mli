(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Localized values and messages. *)

(** {1 Localized values} *)

type 'a t
(** The type for localized values of type ['a]. Localized value
    bind language {{!Lang.range}ranges} to values. *)

val empty : 'a t
(** [v value] is an empty localized value. *)

val is_empty : 'a t -> bool
(** [is_empty lv] is [true] iff [lv] is empty. *)

val any : 'a -> 'a t
(** [any v] is a localized value with [v] mapping to {!Nlang.any}. *)

val add : 'a t -> Nlang.range -> 'a -> 'a t
(** [add lv r v] is [lv] with [r] bound to [v]. *)

val rem : 'a t -> Nlang.range -> 'a t
(** [rem lv r] is [lv] without [r]'s binding. *)

val mem : Nlang.range -> 'a t -> bool
(** [mem lv r] is [true] if [lv] has a binding for [r]. *)

val find : Nlang.t -> 'a t -> 'a option
(** [find l lv] get a localized value for locale [l] in [lv] (if any).

    The algorithm looks for ranges that are matching [l] in [lv]
    selects the most precise one, that is the range with most subtags
    and if there are ties the greatest one according to {!Nlang.Range.compare}.

    {b Note.} RFC 4647 defines no algorithm to lookup a language range
    with a language tag, hence the above definition. FIXME review
    RFC 4647 did you miss something ? *)

val find_range : Nlang.t -> 'a t -> Nlang.range option
(** [find_range l lv] is the range matched by [l] in [lv] (if any). *)

val get : Nlang.t -> 'a t -> 'a
(** [get l lv] is like {!find} but @raise Invalid_argument if [l] doesn't
    match in [lv]. *)

val get_range : Nlang.t -> 'a t -> Nlang.range
(** [get_range l lv] is like {!find_range} but @raise Invalid_argument if [l]
    doesn't match in lv. *)

val fold : ('b -> Nlang.range -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** [ized_fold f acc lv] folds over the bindings of [lv] with [f] starting
    with [acc]. *)

val of_list : (Nlang.range * 'a) list -> 'a t
(** [ized_of_list bs] is a localized value from the bindings [bs]. *)

val to_list : 'a t -> (Nlang.range * 'a) list
(** [ized_to_list lv] is the list of bindings of [lv]. *)

(** {1 Localized messages} *)

(** Formatting messages.

    {b TODO.} Functional unparsing API. *)
module Msg : sig

  type t
  (** The type for messages. *)

  val v : string -> t
  (** [v msg] is a message from [v]. *)

  val get : t -> string
  (** [get m] is the message [m]. *)

  val of_string : string -> [ `Ok of t | `Error of string ]
  (** [of_string] is like {!v} but returns an [`Error] when parsing
      fails. *)

  val to_string : t -> string
  (** [to_string m] is [m] as a string. *)

  type map
  (** The type for message maps. *)

  module Map : sig
    type msg = t
    type key = string
    type t = map
    val empty : Nlang.range -> t
    val locale : t -> Nlang.range
    val add : map -> key -> msg -> t
    val rem : map -> key -> map
    val mem : map -> key -> bool
    val get : ?absent:msg -> map -> key -> msg
    val fold : ('b -> key -> msg -> 'b) -> 'b -> map -> 'b
  end
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
