(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Rectangles.

    A rectangle is defined by a point ([ox],[oy]) its {e origin} and
    a size ([w],[h]). Operations on rectangles with negative sizes are
    undefined.

    The surface S([r]) spanned by [r] is \[[ox]; [ox] + [w]\] x
    \[[oy]; [oy] + [h]\]. The extremum points of this space are
    the box's {e corners}. There is a distinguished n-dimensional
    [empty] box such that S([empty]) is empty.
*)

(** {1 Constructors, accessors and constants.} *)

type t
(** The type for rectangles. *)

val empty : t
(** The empty rectangle. *)

val v : float -> float -> float -> float -> t
(** [v ox oy w h] is a rectangle with origin [(ox,oy)] and
    size [(w, h)]. *)

val vi : int -> int -> int -> int -> t
(** [vi] is like {!v} with integers. *)

val o : t -> float * float
(** [o r] is the origin of [r]. *)

val ox : t -> float
(** [ox r] is [fst (o r)]. *)

val oy : t -> float
(** [oy r] is [snd (o r)]. *)

val size : t -> float * float
(** [size r] is the size of [r]. *)

val w : t -> float
(** [w r] is [fst (size r)]. *)

val h : t -> float
(** [h r] is [snd (size r)]. *)

val zero : t
(** [zero] is a rectangle with zero origin and size. *)

val unit : t
(** [unit] is a rectangle with zero origin and unit size. *)

val of_ltrb_i : int -> int -> int -> int -> t
(** [of_ltrb_i l t r b] is rectangle from integer left, top, right
    and bottom coordinates. *)

(** {1 Functions}  *)

val min : t -> float * float
(** [min r] is the smallest point of [r] (its origin). *)

val minx : t -> float
(** [minx r] is [fst (min r)]. *)

val miny : t -> float
(** [miny r] is [snd (min r)]. *)

val max : t -> float * float
(** [max r] is the greatest point of [r] (its size added to the origin). *)

val maxx : t -> float
(** [maxx r] is [fst (max r)]. *)

val maxy : t -> float
(** [maxy r] is [snd (max r)]. *)

val mid : t -> float * float
(** [mid r] is the mid point between [min] and [max]. *)

val midx : t -> float
(** [midx r] is [fst (mid r)]. *)

val midy : t -> float
(** [midy r] is [snd (mid b)]. *)

val bottom_left : t -> float * float
(** [bottom_left r] is the bottom-left corner of [r]. *)

val bottom_right : t -> float * float
(** [bottom_right r] is the bottom-right corner of [r]. *)

val top_left : t -> float * float
(** [top_left r] is the top-left corner of [r]. *)

val top_right : t -> float * float
(** [top_right r] is the top-right corner of [r]. *)

val area : t -> float
(** [area r] is the surface area of [r]. *)

val inter : t -> t -> t
(** [inter r r'] is a box whose space is the intersection of S([r])
    and S([r']). *)

val union : t -> t -> t
(** [union r r'] is the smallest box whose space contains
      S([r]) and S([r']). *)

val inset : (float * float) -> t -> t
(** [inset d r] is [r] whose edges are inset in each dimension
     according to amounts in [d]. Negative values in [d] outset. If
     the resulting size is negative returns {!empty}.  Returns
     {!empty} on {!empty}. *)

val round : t -> t
(** [round r] is the smallest box containing [r] with integer valued
     corners. Returns {!empty} on {!empty}. *)

val map_f : (float -> float) -> t -> t
(** [map_f f r] is the box whose origin and size are those of [b] with
    their components mapped by [f]. Returns {!empty} on {!empty}. *)

(** {1 Predicates and comparison} *)

val is_empty : t -> bool
(** [is_empty r] is [true] iff [r] is {!empty}. *)

val is_pt : t -> bool
(** [is_pt r] is [true] iff [r] is not {!empty} and its size is equal
    to 0 in every dimension. *)

val is_seg : t -> bool
(** [is_seg r] is [true] iff [r] is not {!empty} and its size is
    equal to 0 in exactly one dimension. *)

val isects : t -> t -> bool
(** [isects r r'] is [not (is_empty (inter r r'))]. *)

val subset : t -> t -> bool
(** [subset r r'] is [true] iff S([r]) is included in S([r']). *)

val mem : float * float -> t -> bool
(** [mem p r] is [true] iff [p] is in S([r]). *)

val equal : t -> t -> bool
(** [equal r r'] is [r = r']. *)

val equal_f : (float -> float -> bool) -> t -> t -> bool
(** [equal_f eq r r'] tests [r] and [r'] like {!equal}
    but uses [eq] to test floating point values. *)

val compare : t -> t -> int
(** [compare r r'] is [Pervasives.compare r r']. *)

val compare_f : (float -> float -> int) -> t -> t -> int
(** [compare_f cmp r r'] compares [r] and [r'] like {!compare}
    but uses [cmp] to compare floating point values. *)

(** {1 Printers} *)

val to_string : t -> string
(** [to_string r] is a textual representation of [r]. *)

val print : Format.formatter -> t -> unit
(** [print ppf b] prints a textual representation of [b] on [ppf]. *)


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
