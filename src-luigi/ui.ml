(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let pr = Format.fprintf
let str_with_pp pp v =
  pr Format.str_formatter "%a" pp v; Format.flush_str_formatter ()

(* Duration and times pretty printers. *)

let pr_dur_unit u div ff d =
  let i = truncate (d /. div) in
  if i = 0 then () else pr ff "%d%s" i u

let pr_sec ff d = pr ff "%.3fs" d
let pr_min_sec ff d = pr ff "%a%a"
    (pr_dur_unit "m" 60.) d (pr_dur_unit "s" 1.) (mod_float d 60.)

let pr_hour_min ff d = pr ff "%a%a"
    (pr_dur_unit "h" 3600.) d (pr_dur_unit "m" 60.) (mod_float d 3600.)

let pr_day_hour_min ff d =
  let day = truncate (d /. 86_400.) in
  let day_str = if day = 1 then "day" else "days" in
  let rd = mod_float d 86_400. in
  pr ff "%d %s" day day_str; if (rd >= 60.) then pr ff ", %a" pr_hour_min rd

let pp_duration ff d =
  if d < 60. then pr_sec ff d else
  if d < 3600. then pr_min_sec ff d else
  if d < 86_400. then pr_hour_min ff d else
  pr_day_hour_min ff d

let pp_times ff start =
  let ts = Unix.times () in
  let real = Unix.gettimeofday () -. start in
  pr ff "%a (user %a, sys %a)" pp_duration real
    pp_duration (ts.Unix.tms_utime +. ts.Unix.tms_cutime)
    pp_duration (ts.Unix.tms_stime +. ts.Unix.tms_cstime)

(* TTY status line. *)

module Status : sig
  val clear : Format.formatter -> unit -> unit
  val show_last : Format.formatter -> unit -> unit
  val show : Format.formatter -> ('a, Format.formatter, unit, unit) format4
    -> 'a
end = struct
  let max = 80
  let adjust s =
    let l = String.length s in
    if l <= max then s ^ (String.make (max - l) ' ') else
    let half = (max - 3) / 2 in
    String.sub s 0 (max - half - 3) ^ "..." ^ (String.sub s (l - half) half)

  let last = ref ""
  let blank = String.make max ' '
  let show_last ff () = pr ff "\r%s@?" !last
  let clear ff () = pr ff "\r%s\r@?" blank
  let show ff fmt =
    let printf _ =
      let s = adjust (Format.flush_str_formatter ()) in
      Format.fprintf ff "%a%s@?" clear () s; last := s
    in
    Format.kfprintf printf Format.str_formatter fmt
end

(* TTY activity indicator *)

let show_activity ff () =                              (* copied from unison. *)
  pr ff "%c" "-\\|/".[truncate (mod_float (4. *. Unix.gettimeofday ()) 4.)]

(* TTY UI *)

let start_time = Unix.gettimeofday ()
let out = ref Format.std_formatter
let set_formatter ff = out := ff
let verbosity = ref `Normal
let set_verbosity v = verbosity := v

let log_msg fmt =
  let flush ff = pr ff "@.%a" Status.show_last () in
  Status.clear !out ();
  Format.kfprintf flush !out fmt

let log_msg_err fmt =
  let flush ff = pr ff "@." in
  Format.kfprintf flush !out fmt

let log fmt = match !verbosity with `Normal -> log_msg fmt
| `Quiet | `Error ->  Format.ifprintf !out fmt

let log_err fmt = match !verbosity with `Quiet -> Format.ifprintf !out fmt
| `Normal -> log_msg ("Error:" ^^ fmt) | `Error -> log_msg_err ("Error:" ^^ fmt)

(* Mapf TTY UI *)

let show_mapf_stats m _ =
  if !verbosity = `Normal then
    let n = Mapf.count m - Mapf.remaining m in
    pr !out "%a" Status.clear ();
    if n = Mapf.count m
    then pr !out "Processed %d files" n
    else pr !out "Interrupted.@\nProcessed %d of %d files" n (Mapf.count m);
    pr !out " in %a.@." pp_times start_time

let show_mapf_status m result =
  if !verbosity = `Normal then
    let n = Mapf.count m - Mapf.remaining m in
    Status.show !out "%a [%d/%d] %s" show_activity () n (Mapf.count m) result

let show_mapf pp action m =
  at_exit (show_mapf_stats m);
  Sys.set_signal Sys.sigquit (Sys.Signal_handle (fun _ -> exit 1));
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> exit 1));
  let last_result = ref "Processing started..." in
  try while true do
    show_mapf_status m !last_result;
    match Mapf.result ~timeout:0.24 m with
    | `Timeout -> ()
    | `File (f, o) -> action (f, o); last_result := str_with_pp pp (f, o)
    | `Exn (e, bt) -> log_err "%s@\n%s" (Printexc.to_string e) bt
    | `End -> raise Exit
  done;
  with Exit -> ()

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
