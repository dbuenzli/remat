(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Language tags and ranges (BCP47)

    {b FIXME} Implement RFC 4647 matching.
    Review use of Language range.

    {e Release %%VERSION%% – %%MAINTAINER%% }
    {3 References}
    {ul
    {- BCP 47 – {{:http://tools.ietf.org/html/bcp47}
    Tags for Identifying Languages}}}
*)

(** {1 Language tags} *)

type t
(** The type for language tags. *)

val v : string -> t
(** [v lang] is a language tag from the
    {{:http://tools.ietf.org/html/rfc4646#section-2.1}BCP 47 language
    tag} [lang].

    @raise Invalid_argument if [lang] is not a valid tag. Use
    {!of_string} to deal with errors. *)

val subtags : t -> string list
(** [subtags l] is [l]'s lowercased list of subtags. *)

val equal : t -> t -> bool
(** [equal l l'] is [true] iff the subtags of [l] and [l'] are equal. *)

val compare : t -> t -> int
(** [compare l l'] orders [l] and [l'] by the number of subtags and,
    if equal, by lexicographic order on the subtags. *)

val of_string : string -> [ `Ok of t | `Error of string ]
(** [of_string] is like {!v} but returns an [`Error] when parsing
    fails. *)

val to_string : t -> string
(** [to_string l] is [l] as a string. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf l] prints [l] as a string on [ppf]. *)

(** {1 Language ranges} *)

type range
(** The type for language ranges. A value of this type denotes
    a set of language tags. *)

val any : range
(** [any] is the wildcard language range [*], it denotes
    the set of all language tags. *)

val to_range : t -> range
(** [to_range l] is [l] as a locale range. *)

(** Language ranges. *)
module Range : sig

  (** {1 Language ranges} *)

  type t = range
  (** The type for language ranges. A value of this type denotes
      a set of language tags. *)

  val v : ?basic:bool -> string -> t
  (** [v range] is a language range from the
      {{:http://tools.ietf.org/html/rfc4647#section-2.2}BCP 47
      extended language range} [range]. If [basic] is [true] (defaults
      to [false]), [range] should be a
      {{:http://tools.ietf.org/html/rfc4647#section-2.1}BCP 47 basic
      language range}.

      @raise Invalid_argument if [range] is not a valid range. Use
      {!of_string} to deal with errors. *)

  val subtags : range -> string list
  (** [subtags r] is [r]'s lowercased list of subtags. *)

  val equal : range -> range -> bool
  (** [equal r r'] is [true] iff the subtags of [r] and [r'] are equal. *)

  val compare : range -> range -> int
  (** [compare r r'] orders [r] and [r'] by the number of subtags and,
      if equal, by lexicographic order on the subtags with * smaller
      than anything. *)

  val of_string : ?basic:bool -> string -> [ `Ok of range | `Error of string ]
  (** [of_string] is like {!v} but returns an [`Error] when parsing
      fails. *)

  val to_string : range -> string
  (** [to_string r] is [r] as a string. *)

  val pp : Format.formatter -> range -> unit
  (** [pp ppf r] prints [r] as a string on [ppf]. *)
end

(** {1 Language range matching} *)

val matches : t -> range -> bool
(** [matches l r] is [true] if locale [l] matches range [r].

    We say that a locale identifier [l] matches a range [r] if [l] is
    included in the set of locale identifiers denoted by [r]. More
    precisely [l] matches [r] if the subtags of [r] are a prefix of
    those of [l] with any subtags [*] matching any sequence of subtags
    (as such [*] cannot be used to denote that a certain number of
    subtags will appear in a matching locale). Subtag comparison is
    case insensitive.

    For example the locale [de-CH-1996-x-mobile-tablet] could match any
    of these locale ranges:
{[
de-CH-1996-x-mobile-tablet
de-CH-1996-*-x-mobile-tAbLet
de-CH-1996-x-mobile
de-CH-1996-*
de-*-CH-x-*
de-*-*-tablet
de-CH-1996
de-*-1996
de-*-tablet
de-CH
de-*
*-CH
*-tablet
de
*
]}
But it wouldn't match any of these:
{[fr, de-DE, *-FR, de-*-DE-*, de-*-goethe ]} *)

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
