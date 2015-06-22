(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open React

(* Global request montitoring *)

let activity, set_activity = S.create false
let tasks = ref 0
let incr_task () = incr tasks; if !tasks > 0 then set_activity true
let decr_task step =
  decr tasks; if !tasks <= 0 then (tasks := 0; set_activity ~step false)

(* Base types *)

type error = [ `Http of int | `Unknown ]
type 'a result = [ `Aborted | `Error of error | `Ok of 'a ]
type progress = [ `Elapsed of Br.Time.span | `Bytes of int * int ]

let pp_error ppf = function
| `Http code -> Format.fprintf ppf "@[`Http %d@]" code
| `Unknown -> Format.fprintf ppf "`Unknown"

let pp_result ~pp_v ppf = function
| `Aborted -> Format.fprintf ppf "`Aborted"
| `Error e -> Format.fprintf ppf "`Error %a" pp_error e
| `Ok v -> Format.fprintf ppf "`Ok %a" pp_v v

let pp_progress ppf = function
| `Elapsed t -> Br.Time.pp_s ppf t
| `Bytes (transf, total) -> Format.fprintf ppf "%d/%d bytes" transf total

(* Requests *)

type 'a t =
  { uri : Js.js_string Js.t;
    start : Br.Time.counter;
    mutable duration : Br.Time.span;
    result : [ `Aborted | `Error of error | `Ok of 'a ]  event;
    progress : [ `Elapsed of Br.Time.span | `Bytes of int * int ] signal;
    aborter : unit event; }

let handler_start r =
  let step = React.Step.create () in
  E.stop ~strong:true r.aborter; (* avoid leaks *)
  r.duration <- Br.Time.counter_value r.start;
  decr_task step;
  Some step

let handler_finish step =
  let step = match step with None -> assert false | Some step -> step in
  React.Step.execute step;
  Js._false

let request_complete f send r req _ =
  let step = handler_start r in
  let status = req ## status in
  if status <> 200
  then send ?step (`Error (`Http status))
  else send ?step (`Ok (f (req ## responseText)));
  handler_finish step

let request_error send r req _ = (* FIXME not possible to get more info ? *)
  let step = handler_start r in
  send ?step (`Error `Unknown);
  handler_finish step

let request_abort send r req _ =
  let step = handler_start r in
  send ?step `Aborted;
  handler_finish step

let request_progress set r e =
  let e = (e :> 'a File.progressEvent Js.t) in
  let computable = Js.to_bool (e ## lengthComputable) in
  if computable then set ?step:None (`Bytes (e ## loaded, e ## total)) else
  set ?step:None (`Elapsed (Br.Time.counter_value r.start));
  Js._false

let method_get = Js.string "GET"

let create ?(abort = E.never) uri f =
  incr_task ();
  let req = XmlHttpRequest.create () in
  let start = Br.Time.counter () in
  let duration = -1. in
  let result, send_result = E.create () in
  let progress, set_progress = S.create (`Elapsed 0.) in
  let aborter = E.map (fun _ -> req ## abort ()) abort in
  let r = { uri; start; duration; result; progress; aborter; } in
  req ## onload <- Dom.full_handler (request_complete f send_result r);
  req ## onerror <- Dom.full_handler (request_error send_result r);
  req ## onabort <- Dom.full_handler (request_abort send_result r);
  req ## onprogress <- Dom.handler (request_progress set_progress r);
  req ## _open (method_get, uri, Js._true);
  req ## send (Js.Opt.empty);
  r

let get_string ?abort uri f = create ?abort uri f
let get_json ?abort uri codec =
  let decode data =
    let d = Jsont_codec.decoder data in
    let d = Jsont.decoder d codec in
    let rec loop () = match Jsont.decode d with
    | `Await -> loop ()
    | `Ok v -> v
    | `Error (_, e) ->
        Br.Log.err ~header:"JSON" "%a: %s" Br.Str.pp uri
          (Jsont.error_to_string e); loop ()
    in
    snd (loop ())
  in
  create ?abort uri decode

let uri r = r.uri
let result r = r.result
let value r = E.fmap (function `Ok v -> Some v | _ -> None) r.result
let aborted r = E.fmap (function `Aborted -> Some () | _ -> None) r.result
let error r = E.fmap (function `Error e -> Some e | _ -> None) r.result
let progress r = r.progress
let active r =
  if r.duration = -1. then S.hold true (E.map (fun _ -> false) r.result) else
  S.const false

let over r = E.map (fun _ -> r.duration) r.result
let elapsed r =
  if r.duration = -1. then Br.Time.counter_value r.start else r.duration


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
