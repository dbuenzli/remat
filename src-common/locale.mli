(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Locale identifiers and locale ranges.

    Locale identifiers and ranges are
    {{:http://tools.ietf.org/html/bcp47}BCP 47} language tags and ranges. *)

type t = string
(** The type for locale identifiers and ranges. *)

val is_lang : t -> bool
(** [is_lang l] is [true] if [l] is a BCP 47
    {{:http://tools.ietf.org/html/rfc4646#section-2.1}language
       tag}. *)

val is_basic_range : t -> bool
(** [is_basic_range] is [true] if [l] is a
    {{:http://tools.ietf.org/html/rfc4647#section-2.1}basic language
     range}. *)

val is_range : t -> bool
(** [is_range r] is [true] if [r] is a BCP 47
    {{:http://tools.ietf.org/html/rfc4647#section-2.2}extended language
      range}. *)

val subtags : t -> string list
(** [subtags r] is the lowercased list of subtags of the locale or
    range [r]. *)

val compare : t -> t -> int
(** [compare r r'] orders [r] and [r'] by the number of subtags and,
    if equal, by by lexicographic order on the subtags. *)

val equal : t -> t -> bool
(** [equal r r'] is [true] iff the atoms [r] and [r'] are equal. *)

val matches : t -> t -> bool
(** [matches l r] is [true] if locale [l] matches range [r]. *)

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
