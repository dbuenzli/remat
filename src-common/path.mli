(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** File system paths, path sets and maps.

    [Path] provides three types for handling paths. Values of type
    {!Path.t} are for paths that are either relative or absolute while
    those of type {!Path.rel} and {!Path.abs} specialize to either
    case.

    Relative paths and absolute path each have corresponding modules
    {!Rel} and {!Abs} with specialized functions. {{!conversions}Conversion}
    between the three type of paths are explicit.

    {b FIXME}. We need to properly handle {!Filename.current_dir_name} and
    {!Filename.parent_dir_name} in path segments. *)

(** {1:filepaths File paths} *)

type filename = string
(** The type for file names (basenames). *)

type rel
(** The type for relative paths. *)

type abs
(** The type for absolute paths. *)

type t
(** The type for absolute or relative paths. *)

val root : t
(** [root] is the root absolute path (empty list of segments). *)

val empty : t
(** [empty] is the empty relative path (empty list of segments). *)

val dash : t
(** [dash] is the ["-"] relative path. *)

val add : t -> string -> t
(** [add p seg] concatenates [seg] at the end of [p]. For any [p],
    [add p "" = p]. *)

val concat : t -> rel -> t
(** [concat p p'] concatenates [p'] at the end of [p]. *)

val ( / ) : t -> string -> t
(** [p / c] is [add p c]. Left associative. *)

val ( // ) : t -> rel -> t
(** [p // p'] is [concat p p']. Left associative. *)

val file : filename -> t
(** [file name] is [add empty f]. *)

val base : string -> t
(** [base name] is [add empty f]. *)

val basename : t -> string
(** [basename p] is the basename of [p]. If [p] has no segments the
    empty string is returned. *)

val dirname :  t -> t
(** [dirname p] is the dirname of [p]. If [p] has no segments [p]
    is returned. *)

val rem_prefix : t -> t -> rel option
(** [rem_prefix pre p] is [p] with the literal prefix [pre] removed. [None]
    if [pre] is not a prefix of [p]. *)

val find_prefix : t -> t -> t option
(** [find_prefix p p'] is a common prefix for [p] and [p']. There is
    always a common prefix between path of the same kind (either {!root}
    or {!empty} and [None] is only returned if [p] and [p'] are of
    different kind. *)

(** {1:predicates Predicates and comparison} *)

val is_root : t -> bool
(** [is_root p] is [true] iff [p] is {!root}. *)

val is_empty : t -> bool
(** [is_empty p] is [true] iff [p] is {!empty}. *)

val is_dash : t -> bool
(** [is_dash p] is [true] iff [p] is {!dash}. *)

val is_rel : t -> bool
(** [is_rel p] is [true] iff [p] is a relative path. *)

val is_abs : t -> bool
(** [is_abs p] is [true] iff [p] is an absolute path. *)

val is_prefix : t -> t -> bool
(** [is_prefix p p'] is [true] if [p] is a literal prefix of [p']. *)

val equal : t -> t -> bool
(** [equal p p'] is [p = p']. *)

val compare : t  -> t -> int
(** [compare p p'] is [Pervasives.compare p p']. *)

(** {1:conversions Conversions} *)

val to_rel : t -> rel option
(** [to_rel p] is [Some r] if [p] is a relative path. *)

val of_rel : rel -> t
(** [of_rel r] is [r] as a path. *)

val to_abs : t -> abs option
(** [to_abs p] is [Some a] if [p] is an absolute path. *)

val of_abs : abs -> t
(** [of_abs a] is [a] as a path. *)

val to_segs : t -> [ `Abs of string list | `Rel of string list ]
(** [to_segs p] is [p]'s segments. *)

val of_segs : [ `Abs of string list | `Rel of string list ] -> t
(** [of_segs segs] is a path from [segs] segments. *)

val to_string : t -> string
(** [to_string p] is the path [p] as a string according to
    the driver's platform convention with {!Filename.dir_sep}. *)

val of_string : string -> t
(** [of_string s] is the string [s] as a path. [s] is splitted
    according to the driver's platform convention with {!Filename.dir_sep}. *)

val quote : t -> string
(** [quote p] is the path [p] as a string, quoted according
     to the driver's platform conventions with {!Filename.quote}. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf p] prints path [p] on [ppf] using {!to_string}. *)

(** {1:file_exts File extensions} *)

type ext =
  [ `A | `Byte | `C | `Cma | `Cmi | `Cmo | `Cmt | `Cmti | `Cmx | `Cmxa
  | `Cmxs | `Css | `Dll | `Exe | `Gif | `H | `Html | `Install | `Img
  | `Jpeg | `Js | `Json | `Lib | `Md | `Ml | `Ml_dep | `Ml_pp | `Mli
  | `Mli_dep | `Mli_pp | `Native | `O | `Opt | `Png | `Sh | `So | `Tar
  | `Tbz | `Xml | `Zip
  | `Ext of string ]
(** The type for file extensions. *)

val ext_to_string : ext -> string
(** [ext_to_string ext] is [ext] as a string (without separator). *)

val ext_of_string : string -> ext
(** [ext_of_string ext] is [ext] as a file extension ([ext] without
    separator). *)

val pp_ext : Format.formatter -> ext -> unit
(** [pp_ext ppf p] prints file extension [ext] on [ppf] using
    {!ext_to_string}. *)

val ext : t -> ext option
(** [ext p] is [p]'s last segment file extension (if any). *)

val get_ext : t -> ext
(** [get_ext p] is [p]'s last segment file extension.

    @raise Invalid_argument if [p]'s last segment has no file extension. *)

val add_ext : t -> ext -> t
(** [add_ext p ext] is [p] with [ext] concatenated to [p]'s last segment. *)

val rem_ext : t -> t
(** [rem_ext p] is [p] with [ext] removed from [p]'s last segment
    (if it has an extension). *)

val change_ext : t -> ext -> t
(** [change_ext p e] is [add_ext (rem_ext p)]. *)

val ( + ) : t -> ext -> t
(** [p + ext] is [add_ext p e]. Left associative. *)

val has_ext : ext -> t -> bool
(** [has_ext p ext] is [true] iff [p]'s last segment has file extension
    [ext]. *)

val ext_matches : ext list -> t -> bool
(** [ext_matches exts p] is [true] iff [p]'s last segment has a file
    extension in [exts]. *)

(** {1:rel Relative paths} *)

(** Relative paths. *)
module Rel : sig

  (** {1 Relative paths} *)

  type path = t
  (** The type for absolute or relative paths. *)

  type t = rel
  (** The type for relative paths. *)

  val empty : rel
  (** See {!Path.empty}. *)

  val dash : rel
  (** See {!Path.dash}. *)

  val add : rel -> string -> rel
  (** See {!Path.add}. *)

  val concat : rel -> rel -> rel
  (** See {!Path.concat}. *)

  val file : filename -> rel
  (** [file name] is [add empty f]. *)

  val base : string -> rel
  (** [base name] is [add empty f]. *)

  val ( / ) : rel -> string -> rel
  (** See {!Path.( / )}. *)

  val ( // ) : rel -> rel -> rel
  (** See {!Path.( // )}. *)

  val basename : rel -> string
  (** See {!Path.basename}. *)

  val dirname :  rel -> rel
  (** See {!Path.dirname}. *)

  val rem_prefix : rel -> rel -> rel option
  (** See {!Path.rem_prefix}. *)

  val find_prefix : rel -> rel -> rel
  (** See {!Path.find_prefix}. *)

  (** {1:predicates Predicates and comparison} *)

  val is_empty : rel -> bool
  (** See {!Path.is_empty}. *)

  val is_dash : rel -> bool
  (** See {!Path.is_dash}. *)

  val is_prefix : rel -> rel -> bool
  (** See {!Path.is_prefix}. *)

  val equal : rel -> rel -> bool
  (** See {!Path.equal}. *)

  val compare : rel  -> rel -> int
  (** See {!Path.compare}. *)

  (** {1 Conversions} *)

  val to_segs : rel -> string list
  (** [to_segs r] is [r]'s segments. *)

  val of_segs : string list -> rel
  (** [of_segs segs] is a path from [segs] segments. *)

  val to_string : rel -> string
  (** See {!Path.to_string}. *)

  val quote : rel -> string
  (** See {!Path.quote}. *)

  val pp : Format.formatter -> rel -> unit
  (** See {!Path.pp}. *)

  (** {1:file_exts File extensions} *)

  val ext : rel -> ext option
  (** See {!Path.ext}. *)

  val get_ext : rel -> ext
  (** See {!Path.get_ext}. *)

  val add_ext : rel -> ext -> rel
  (** See {!Path.add_ext}. *)

  val rem_ext : rel -> rel
  (** See {!Path.rem_ext}. *)

  val change_ext : rel -> ext -> rel
  (** See {!Path.change_ext}. *)

  val ( + ) : rel -> ext -> rel
  (** See {!Path.( + )}. *)

  val has_ext : ext -> rel -> bool
  (** See {!Path.has_ext}. *)

  val ext_matches : ext list -> rel -> bool
  (** See {!Path.ext_matches}. *)

  (** {1:sets_maps Path sets and maps} *)

  module Set : sig
    include Set.S with type elt = rel
    val of_list : elt list -> t
  end

  module Map : sig
    include Map.S with type key = rel
    val dom : 'a t -> Set.t
    (** [dom m] is the domain of [m]. *)
  end
end

(** {1:abs Absolute paths} *)

(** Absolute paths. *)
module Abs : sig

  (** {1 Absolute paths} *)

  type path = t
  (** The type for absolute or relative paths. *)

  type t = abs
  (** The type for absolute paths. *)

  val root : abs
  (** See {!Path.root}. *)

  val add : abs -> string -> abs
  (** See {!Path.add}. *)

  val concat : abs -> rel -> abs
  (** See {!Path.concat}. *)

  val ( / ) : abs -> string -> abs
  (** See {!Path.( / )}. *)

  val ( // ) : abs -> rel -> abs
  (** See {!Path.( // )}. *)

  val basename : abs -> string
  (** See {!Path.basename}. *)

  val dirname :  abs -> abs
  (** See {!Path.dirname}. *)

  val rem_prefix : abs -> abs -> rel option
  (** See {!Path.rem_prefix}. *)

  val find_prefix : abs -> abs -> abs
  (** See {!Path.find_prefix}. *)

  (** {1:predicates Predicates and comparison} *)

  val is_root : abs -> bool
  (** See {!Path.is_root}. *)

  val is_prefix : abs -> abs -> bool
  (** See {!Path.is_prefix}. *)

  val equal : abs -> abs -> bool
  (** See {!Path.equal}. *)

  val compare : abs  -> abs -> int
  (** See {!Path.compare}. *)

  (** {1:conversions Conversions} *)

  val to_segs : abs -> string list
  (** [to_segs a] is [a]'s segments. *)

  val of_segs : string list -> abs
  (** [of_segs segs] is a path from [segs] segments. *)

  val to_string : abs -> string
  (** See {!Path.to_string}. *)

  val quote : abs -> string
  (** See {!Path.quote}. *)

  val pp : Format.formatter -> abs -> unit
  (** See {!Path.pp}. *)

  (** {1:file_exts File extensions} *)

  val ext : abs -> ext option
  (** See {!Path.ext}. *)

  val get_ext : abs -> ext
  (** See {!Path.get_ext}. *)

  val add_ext : abs -> ext -> abs
  (** See {!Path.add_ext}. *)

  val rem_ext : abs -> abs
  (** See {!Path.rem_ext}. *)

  val change_ext : abs -> ext -> abs
  (** See {!Path.change_ext}. *)

  val ( + ) : abs -> ext -> abs
  (** See {!Path.( + )}. *)

  val has_ext : ext -> abs -> bool
  (** See {!Path.has_ext}. *)

  val ext_matches : ext list -> abs -> bool
  (** See {!Path.ext_matches}. *)

  (** {1:sets_maps Path sets and maps} *)

  module Set : sig
    include Set.S with type elt = abs
    val of_list : elt list -> t
  end

  module Map : sig
    include Map.S with type key = abs
    val dom : 'a t -> Set.t
    (** [dom m] is the domain of [m]. *)
  end
end

(** {1:sets_maps Path sets and maps} *)

module Set : sig
  include Set.S with type elt = t
  val of_list : elt list -> t
end

module Map : sig
  include Map.S with type key = t
  val dom : 'a t -> Set.t
  (** [dom m] is the domain of [m]. *)
end

(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli.
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
