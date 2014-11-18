(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Map files in file hierarchies.

    Uses {{!Workers}workers} to fold over the files of
    files hiearachies. *)

(** {1 Map files} *)

type 'a t
(** The type for the map operation. *)

val map : ?workers:int -> ?suffix:string -> (string -> 'a) -> string list ->
  'a t
(** [map workers suffix f files] maps [f] over the files in [files].
    Directories are walked recursively and in those only the files whose
    suffix matches [suffix] (defaults to [""]) are mapped.
    [workers] is the number of workers (default, see {!Workers.create}).  *)

val result : ?timeout:float -> 'a t ->
  [ `File of string * 'a | `Exn of exn * string | `Timeout | `End ]
(** [result timeout m] waits [timeout] seconds (defaults to forever) or
    until a map result is available from [m]. Returns [`End] once
    all files were mapped. *)

val count : 'a t -> int
(** [count m] is the number of files in the map operation. *)

val remaining : 'a t -> int
(** [remaining m] is the number of file maps remaining the map
    operation. *)

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
