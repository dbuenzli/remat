(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let err_neg_count = "count must be strictly positive"
let err_stopped = "stopped workers"
let err_stop = "cannot stop, apply_count not 0"

let send_value oc v = Marshal.to_channel oc v [Marshal.Closures]; flush oc
let recv_value ic = Marshal.from_channel ic
let cpu_count () =
  try match Sys.os_type with
  | "Win32" -> int_of_string (Sys.getenv "NUMBER_OF_PROCESSORS")
  | _ ->
      let i = Unix.open_process_in "getconf _NPROCESSORS_ONLN" in
      let close () = ignore (Unix.close_process_in i) in
      try Scanf.fscanf i "%d" (fun n -> close (); n) with e -> close (); raise e
  with
  | Not_found | Sys_error _ | Failure _ | Scanf.Scan_failure _
  | End_of_file | Unix.Unix_error (_, _, _) -> 1

(* Worker *)

module Worker = struct
  type 'a input = [ `Stop | `Apply of unit -> 'a ]
  type 'a output = [ `Ok of 'a | `Exn of exn * string ]
  type t = { pid : int; recv : in_channel; send : out_channel; }

  let rec work recv send = match recv_value recv with
  | `Stop -> close_in recv; close_out send
  | `Apply (f : unit -> 'a) ->
      (try send_value send (`Ok (f ()))
      with e -> send_value send (`Exn (e, Printexc.get_backtrace ())));
      work recv send

  let fork () =
    let parent_recv, worker_send = Unix.pipe () in
    let worker_recv, parent_send = Unix.pipe () in
    match Unix.fork () with
    | 0 ->
        let recv = Unix.in_channel_of_descr worker_recv in
        let send = Unix.out_channel_of_descr worker_send in
        Unix.close parent_recv; Unix.close parent_send;
        work recv send; exit 0
    | pid ->
        let recv = Unix.in_channel_of_descr parent_recv in
        let send = Unix.out_channel_of_descr parent_send in
        Unix.close worker_recv; Unix.close worker_send;
        { pid; recv; send; }

  let recv w = recv_value w.recv
  let recv_fd w = Unix.descr_of_in_channel w.recv
  let send w v = send_value w.send v
  let kill w =
    try Unix.kill w.pid Sys.sigkill with
    | Unix.Unix_error (Unix.ESRCH, _, _) -> () (* may already be killed *)

  let stop do_kill w =
    if do_kill then kill w else send w `Stop;
    close_in w.recv; close_out w.send

  let rec waitpid w =
    try ignore (Unix.waitpid [] w.pid) with
    | Unix.Unix_error (Unix.ECHILD, _, _) -> () (* may already be killed *)
    | Unix.Unix_error (Unix.EINTR, _, _) -> waitpid w
end

(* Workers *)

type 'a t =
    { mutable stopped : bool ;
      mutable busy : (Unix.file_descr * Worker.t) list;
      mutable free : Worker.t list;
      mutable apply_count : int;
      work : [`Apply of (unit -> 'a)] Queue.t }

let create ?(count = cpu_count ()) () =
  if count <= 0 then invalid_arg err_neg_count;
  let rec workers acc = function
    | 0 -> acc | count -> workers (Worker.fork () :: acc) (count - 1)
  in
  { stopped = false; busy = []; free = workers [] count; apply_count = 0;
    work = Queue.create () }

let iter_workers f p = List.iter (fun (_, w) -> f w) p.busy; List.iter f p.free
let apply_count ws = ws.apply_count
let _apply ws app = match ws.free with
| [] -> Queue.add app ws.work
| w :: free ->
    ws.busy <- (Worker.recv_fd w, w) :: ws.busy;
    ws.free <- free;
    Worker.send w app

let apply ws f v =
  if ws.stopped then invalid_arg err_stopped;
  ws.apply_count <- ws.apply_count + 1;
  _apply ws (`Apply (fun () -> f v))

let rec result ?(timeout = -1.) ws =
  let recvs = List.rev_map fst ws.busy in
  let start = Unix.gettimeofday () in
  let retry () =
    let diff = Unix.gettimeofday () -. start in
    if diff >= timeout then `Timeout else result ~timeout:diff ws
  in
  try match Unix.select recvs [] [] timeout with
  | [], _, _ -> `Timeout
  | fd :: _, _, _ ->
      let w = List.assoc fd ws.busy in
      try
        let r = Worker.recv w in
        ws.busy <- List.remove_assoc fd ws.busy;
        ws.free <- w :: ws.free;
        ws.apply_count <- ws.apply_count - 1;
        if not (Queue.is_empty ws.work) then _apply ws (Queue.take ws.work);
        r
      with End_of_file (* child killed, could do something better here *) ->
        Worker.stop true w;
        ws.busy <- List.remove_assoc fd ws.busy;
        ws.apply_count <- ws.apply_count - 1;
        retry ()
  with Unix.Unix_error (Unix.EINTR, _, _) -> retry ()

let stop ?(kill = false) ws =
  if ws.stopped then invalid_arg err_stopped;
  if not kill && ws.apply_count <> 0 then invalid_arg err_stop;
  ws.stopped <- true;
  iter_workers (Worker.stop kill) ws;
  iter_workers Worker.waitpid ws

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
