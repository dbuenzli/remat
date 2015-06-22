(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open React

(* Event and signal sinks *)

module Sink = struct

  type sink = Esink : 'a event -> sink | Ssink : 'a signal -> sink
  let sinks = ref []
  let event e = sinks := Esink e :: !sinks
  let signal s = sinks := Ssink s :: !sinks
  let release () =
    let release = function
    | Esink e -> E.stop ~strong:true e
    | Ssink s -> S.stop ~strong:true s
    in
    List.iter release !sinks; sinks := []

end

(* Monotonic time *)

module Time = struct

  let tick span =
    let e, send_e = E.create () in
    let c = Br.Time.counter () in
    let action () = send_e (Br.Time.counter_value c) in
    let ms = span *. 1000. in
    ignore (Dom_html.window ## setTimeout (Js.wrap_callback action, ms));
    e

end

(* DOM *)

module El = struct

  let opt_finalize s = match s with None -> () | Some s -> S.stop ~strong:true s
  let opt_signal_setter f s = match s with
  | None -> None
  | Some s -> Some (S.map f s)

  let v ?id ?title ?classes ?atts name children =
    let el = Br.El.v ?id name [] in
    let set_children = S.map (Br.El.set_children el) children in
    let set_title = opt_signal_setter (Br.El.set_att el Br.Att.title) title in
    let set_classes = opt_signal_setter (Br.El.set_class_list el) classes in
    let set_atts atts = List.iter (fun (a, v) -> Br.El.set_att el a v) atts in
    let set_atts = opt_signal_setter set_atts atts in
    let finalize () =
      S.stop ~strong:true set_children;
      opt_finalize set_title;
      opt_finalize set_classes;
      opt_finalize set_atts;
    in
    Br.El.add_finalizer el finalize;
    el


  let children el children =
    let replacer = S.map (Br.El.set_children el) children in
    let finalize () = S.stop ~strong:true replacer in
    Br.El.add_finalizer el finalize;
    ()
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
