(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** UNIX worker processes.

    [Workers] implements sets of UNIX worker processes.

    {b Warning.} Functions of this module are not thread-safe.
    {b Warning.} Currently there's no indication when child processed
    are killed and work is lost. *)

(** {1 Workers} *)

val cpu_count : unit -> int
(** [cpu_count ()] returns the number of CPUs on the machine. *)

type 'a t
(** The type for workers returning results of type 'a. *)

val create : ?count:int -> unit -> 'a t
(** [create count ()] is a set of [count] workers processes (defaults to
    {!cpu_count} [()]). *)

val apply : 'a t -> ('b -> 'a) -> 'b -> unit
(** [apply ws f v] applies [f v] asynchronously in [ws]. *)

val apply_count : 'a t -> int
(** [apply_count ws] is the number of application being done in [ws]. *)

val result : ?timeout:float -> 'a t ->
  [ `Ok of 'a | `Exn of (exn * string) | `Timeout ]
(** [result timeout ws] waits [timeout] seconds (defaults to forever) or
    until a result is available from [ws]. *)

val stop : ?kill:bool -> 'a t -> unit
(** [stop kill ws] stops [ws] and reclaims its resources. If [kill] is
    [true] (default to [false]), abrubtly kills the workers.

    {b Raises.} [Invalid_argument] if there are still
    applications in [ws] and [kill] is [false]. *)


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
