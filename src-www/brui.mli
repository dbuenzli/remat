(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** User interaction *)

open React

(** Human factors. *)
module Human : sig

  (** {1 System latency feelings}

      These values are from
      {{:http://www.nngroup.com/articles/response-times-3-important-limits/}
      here}. *)

  val noticed : Br.Time.span
  (** [noticed] is [0.1]s, the time span after which the user will
      notice a delay and feel that the system is not reacting
      instantaneously. *)

  val interrupted : Br.Time.span
  (** [interrupted] is [1.]s, the time span after which the user will
      feel interrupted and feedback from the system is needed. *)

  val left : Br.Time.span
  (** [left] is [10.]s, the time span after which the user will
      switch to another task and feedback indicating when the system
      expects to respond is needed. *)

  val feel : unit -> [ `Interacting | `Interrupted | `Left ] signal
  (** [feel ()] is a signal that varies according to user latency
      constants:
      {ul
      {- \[[user_feel ()]\]{_t} [= `Interacting] if
         [t < User.interrupted].}
      {- \[[user_feel ()]\]{_t} [= `Interrupted] if
         [t < User.left].}
      {- \[[user_feel ()]\]{_t} [= `Left] if [t >= User.left].}} *)
  (** {1 Touch target and finger sizes}

      These values are from
      {{:http://msdn.microsoft.com/en-us/library/windows/apps/hh465415.aspx#touch_targets}here}.
*)

  val touch_target_size : float
  (** [touch_target_size] is [9.]mm, the recommended touch target size in
      millimiters. *)

  val touch_target_size_min : float
  (** [touch_size_min] is [7.]mm, the minimal touch target size in
      millimeters. *)

  val touch_target_pad : float
  (** [touch_target_pad] is [2.]mm, the minimum padding size in
      millimeters between two touch targets. *)

  val average_finger_width : float
  (** [average_finger_width] is [11.]mm, the average {e adult} finger width. *)
end

(** Application *)
module App : sig

  (** {1:environment Environment} *)

  val env : string -> default:'a -> (string -> 'a) -> 'a
  (** [env var ~default parse] lookups [var] in the environment,
      parses it with [parse] and returns the result. Lookups th the
      query string of [window.location] for the first matching
      [var=value] pair. *)

  (** {1:userquit User requested quit} *)

  val quit : unit event
  (** [quit] occurs whenever the user requested to quit. The browser window
      is closing and it's your last chance to peform something. *)

  (** {1 Run} *)

  val run : ?name:string -> (unit -> unit) -> unit
end


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
