(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** JSON decoding combinators.

    High-level JSON decoding combinators using [Jsonm] and
    applicative functors (why ? sometimes you may want a monad, e.g.
    switch).

    {b Limitations.} Blocking. Stops on first error. Considers
    multiple member name definition as an error. Lookahead is very
    limited. All this could be lifted at the cost of a slighly more
    complex implementation/interface. But is enough here since we
    control the JSON. *)

(** {1 Decode} *)

type error = [ Jsonm.error | `Other of string ]
(** The type for errors. {!Jsonm.error} augmented with [`Other]. *)

type loc = (int * int) * (int * int)
(** The type for locations. *)

type 'a def = loc * 'a
(** The type for values tupled with location. *)

type 'a t
(** The type for JSON decoders evaluating to values of type ['a]. *)

val decode : Jsonm.decoder -> 'a t ->
  [ `Ok of 'a | `Error of error def ]
(** [decode jdec d] evaluates [d] with [jdec]. *)

val ( & ) : ('a -> 'b) -> 'a -> 'b
(** [f & x] is [f x] and right associative. *)

val ( >> ) : 'a -> ('a -> 'b) -> 'b
(** [x >> f] is [f x] and left associative. *)

(** {2 Base combinators} *)

val pure : 'a -> 'a t
(** [pure v] is a JSON decoder evaluating to [v]. *)

val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t
(** [f $ v] is a JSON decoder that evaluates to the result of applying
    the evaluation of [v] to the one of [f]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f v] is [pure f $ v]. *)

val ret : [ `Error of string def | `Ok of 'a ] t -> 'a t
(** [ret d] evaluates to [v] if [d] evaluates to [`Ok v] and to an error
    otherwise *)

val choose : 'a t -> 'a t -> 'a t
(** [choose l r] evaluates to either [l] or [r].

    {b Note.} This is LL(1) on JSON lexemes. Only sufficient
    to distinguish arrays, objects and ground values. *)

val undef : 'a def t -> 'a t
(** [undef d] is [d] without the location information. *)

val eoi : 'a t -> 'a t
(** [eoi d] is a JSON decoder that evaluates like [d] and
    parses the end of input. *)

val opt : 'a def t -> 'a option def t
(** [opt d] is a JSON decoder that decodes like [d] or
    decodes a JSON null. Location spans either [d] or the null. *)

(** {2 Ground values} *)

val null : unit def t
(** [null] decodes a JSON null. Location spans the null. *)

val bool : bool def t
(** [bool] decodes a JSON bool. Location spans the bool. *)

val float : float def t
(** [float] decodes a JSON float. Location spans the float. *)

val string : string def t
(** [string] is a JSON decoder for a string. Location spans the string. *)

(** {2 Arrays} *)

val array : ?empty:bool -> ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a def t
(** [array empty f acc d] is a JSON decoder for an array. Array values
    are decoded with [d] and folded over with [f]. If [empty] is
    [true] (default) the array can be empty, if [false] an error
    is returned in that case. Location spans the array. *)

(** {2 Object decoding} *)

type 'a obj
(** The type for an object JSON decoder evaluating to values of type ['a]. *)

val no_mem : 'a -> 'a obj
(** [no_mem v] is an object JSON decoder for the empty object and
    evaluates to [v]. *)

val mem : string -> 'a t -> ('a def -> 'b) obj -> 'b obj
(** [mem name d o] decodes the members of [o] and the required member
    [name] with [d]. Location spans the member name and value. *)

val mem_opt : string -> 'a t -> ('a option def -> 'b) obj -> 'b obj
(** [mem_opt name d obj] decodes the members of [o] and the optional
    member [name] with [d]. Location spans the member name and value
    if present or the decoded object otherwise. *)

val mem_skip : string -> 'a obj -> 'a obj
(** [mem_skip name] decodes the members of [o] and skips the member
    [name]. *)

val obj : ?strict:bool -> ?kind:string -> 'a obj -> 'a def t
(** [obj strict kind o] is a JSON decoder for the object [o].  [kind]
    can be used to specify the kind of object expected for error
    messages.  If [strict] is [true] (default) presence of members
    unknown to [obj] results in an error, if [false] they are
    skipped. *)

val obj_all : ?kind:string -> ('a -> string def -> 'b -> 'a) -> 'a -> 'b t ->
  ('a -> 'c) obj -> 'c def t
(** [obj_all kind f acc v o] is a JSON decoder for the object [o]. Unknown
    members are parsed with [v] and folded over with [f]. [kind] can be
    used to specify the kind of object expected for error messages. *)

(** {2 JSON soup decoding} *)

type soup =
  [ `Null | `Bool of bool | `Float of float | `String of string
  | `A of soup def list | `O of (string def * soup def) list ]
(** The type for JSON soup (generic JSON representation). *)

val soup : soup def t
(** [soup] is a JSON decoder for a JSON soup value. *)

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
