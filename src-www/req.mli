(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** XMLHttpRequests

    A request just wraps an XHMLHttpRequest and derives a few events
    and signals for user feedback and orchestration. *)

open React

(** {1 Base types} *)

type error = [ `Http of int | `Unknown ]
(** The type for request errors. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] prints and unspecified representation of [e]
    on [ppf]. *)

type 'a result = [ `Ok of 'a | `Aborted | `Error of error ]
(** The type for request results. *)

val pp_result : pp_v:(Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a result -> unit
(** [pp_result ~pp_v ppf r] prints an unspecified representation of
    [r] on [ppf]. *)

type progress = [ `Elapsed of Br.Time.span | `Bytes of int * int ]
(** The type for request progress.
    {ul
    {- [`Bytes (loaded, total)], the number of bytes in the request
        is known and [loaded] bytes were loaded out of
        [total].}
    {- [`Elapsed t], the number of bytes in the request is unknown
       but it started [t] seconds ago.}} *)

val pp_progress : Format.formatter -> progress -> unit
(** [pp_progress ppf p] prints an unspecified representation of [p]
    on [ppf]. *)

(** {1 Requests} *)

type 'a t
(** The type for requests. The type variable is the result
    of the request. *)

val get_string : ?abort:'b event -> Br.str ->
  (Br.str -> 'a) -> 'a t
(** [get_string abort uri f] makes a request on [uri] and applies the
    result to [f]. The request aborts whenever [abort] occurs (defaults
    to {!React.E.never}). *)

val get_json : ?abort:'b event -> Br.str -> 'a Jsont.codec -> 'a t
(** [get_json abort uri codec] is like {!get_string} but the result is
    decoded with [codec]. *)

(** {1 Request monitoring and result} *)

val uri : 'a t -> Br.str
(** [uri r] is the URI requested by [r]. *)

val result : 'a t -> [ `Ok of 'a | `Aborted | `Error of error ] event
(** [result r] occurs when the request is over with the status of the
    request. *)

val value : 'a t -> 'a event
(** [value r] occurs when the request completes with [`Ok]. *)

val aborted : 'a t -> unit event
(** [aborted r] occurs when the request [`Aborted]. *)

val error : 'a t -> error event
(** [error r] occurs when the request [`Error]s. *)

val progress : 'a t -> progress signal
(** [progress r] is a signal that updates while the request is active. See
    {!type:progress}. *)

val active : 'a t -> bool signal
(** [active r] is [true] and becomes [false] when {!result} occurs.  *)

val over : 'a t -> Br.Time.span event
(** [over r] occurs when {!result} occurs with the time it took for
    the request. *)

val elapsed : 'a t -> Br.Time.span
(** [elapsed r] is the time that elapsed since the start of the request
    until {!result} occurs. *)

(** {1 Global request monitoring} *)

val activity : bool signal
(** [activity] is [true] whenever there exists a request that is active. *)

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
