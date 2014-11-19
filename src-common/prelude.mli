(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** A few extensions to the standard library.

    Open the module to use it. It defines only modules
    and one value in your scope. *)

(** {1 Strings} *)

val str : ('a, Format.formatter, unit, string) format4 -> 'a
(** [str] is {!Format.asprintf}. *)

(** Extended [String] module, string sets and maps. *)
module String : sig

  (** {1 String} *)

  include module type of String

  (** These are sorely missing from the standard library. *)

  val split : sep:string -> string -> string list
  (** [split sep s] is the list of all (possibly empty) substrings of
      [s] that are delimited by matches of the non empty separator
      string [sep].

      Matching separators in [s] starts from the beginning of [s] and once
      one is found, the separator is skipped and matching starts again
      (i.e. separator matches can't overlap). If there is no separator
      match in [s], [[s]] is returned.

      The invariants [String.concat sep (String.split sep s) = s] and
      [String.split sep s <> []] always hold.

      @raise Invalid_argument if [sep] is the empty string. *)

  val rsplit : sep:string -> string -> string list
  (** [rsplit sep s] is like {!split} but the matching is
      done backwards, starting from the end of [s].

      @raise Invalid_argument if [sep] is the empty string. *)

  val cut : sep:string -> string -> (string * string) option
  (** [cut sep s] is either the pair [Some (l,r)] of the two
      (possibly empty) substrings of [s] that are delimited by the first
      match of the non empty separator string [sep] or [None] if [sep]
      can't be matched in [s]. Matching starts from the beginning of [s].

      The invariant [l ^ sep ^ r = s] holds.

      @raise Invalid_argument if [sep] is the empty string. *)

  val rcut : sep:string -> string -> (string * string) option
  (** [rcut sep s] is like {!cut} but the matching is done backwards
      starting from the end of [s].

      @raise Invalid_argument if [sep] is the empty string. *)

  val slice : ?start:int -> ?stop:int -> string -> string
  (** [slice ~start ~stop s] is the string s.[start], s.[start+1], ...
      s.[stop - 1]. [start] defaults to [0] and [stop] to [String.length s].

      If [start] or [stop] are negative they are subtracted from
      [String.length s]. This means that [-1] denotes the last
      character of the string. *)

  val tokens : string -> string list
  (** [tokens s] is the list of non empty strings made of characters
      that are not separated by [' '], ['\t'], ['\n'], ['\r'] characters in
      [s], the order of character appearance in the list is the same as
      in [s]. *)

  val uniquify : string list -> string list
  (** [uniquify ss] is [ss] without duplicates, the list order is preserved. *)

  (** {1 String sets and maps} *)

  module Set : sig
    include Set.S with type elt = string
    val of_list : string list -> t
    (** [of_list ss] is a set from the list [ss]. *)
  end

  val make_unique_in : ?suff:string -> Set.t -> string -> string option
  (** [make_unique_in ~suff set elt] is a string that does not belong
      [set].  If [elt] in not in [set] then this is [elt] itself
      otherwise it is a string defined by [Printf.sprintf "%s%s%d" s
      suff d] where [d] is a positive number starting from [1]. [suff]
      defaults to ["~"].  [None] in the unlikely case that all
      positive numbers were exhausted. *)

  module Map : sig
    include Map.S with type key = string
    val dom : 'a t -> Set.t
    (** [dom m] is the domain of [m]. *)
  end
end

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
