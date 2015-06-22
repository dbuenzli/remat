(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {!React} support for {!Br}.  *)

open React

(** {1 Event and signal sinks} *)

module Sink : sig

  val event : 'a event -> unit
  (** [event e] keeps a reference on [e] until {!release} is called. *)

  val signal : 'a signal -> unit
  (** [signal s] keeps a reference on [s] until {!release} is called. *)

  val release : unit -> unit
  (** stops and release sinked event and signals. Stops are
      {{!React.strongstop}strong}. *)
end

(** {1 Monotonic time} *)

module Time : sig

  val tick : Br.Time.span -> Br.Time.span event
  (** [tick span] is an event that occurs once in [span] seconds with
      the value [span - span'] where [span'] is the actual delay
      performed by the system.

      {b Note.} Since the system may introduce delays you cannot
      assume that two different calls to {!tick} will necessarily
      yield two non-simultaneous events. *)
end

(** {1 DOM} *)

(** Reactive DOM elements *)

module El : sig

  val v : ?id:Br.str ->
    ?title:Br.str signal ->
    ?classes:Br.str list signal ->
    ?atts:(Br.str * Br.str) list signal ->
    Br.El.name -> Br.El.child list signal -> Br.el

  val children : Br.el -> Br.El.child list signal -> unit
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
