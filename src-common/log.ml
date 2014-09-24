(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* TODO define a structure to allow mutiple first class log definition.
   Basically take a reporter and return a Log.Log.t.

   TODO, reporter seems absurd. Simply return a Log.Log.t from
   a formatter and have a set_default_log.
*)

type level = [ `Debug | `Error | `Info | `Warning ]
type 'a level_log = ('a, Format.formatter, unit, unit) Pervasives.format4 -> 'a
type 'a log = level -> 'a level_log

module Log = struct
  type t =
    { log : 'a. 'a log;
      err : 'a. 'a level_log;
      warn : 'a. 'a level_log;
      info : 'a. 'a level_log;
      debug : 'a. 'a level_log; }
end

type t = Log.t

type verbosity = [ `Verbose | `Normal | `Error | `Quiet ]
let drop_msg verb l = match verb with
| `Normal -> (match l with `Debug -> true | _ -> false)
| `Error -> (match l with `Error -> false | _ -> true)
| `Quiet -> true
| `Verbose -> false

type reporter = { report : 'a. 'a log }

let str_of_level = function
  | `Debug -> "debug" | `Error -> "error"
  | `Info -> "info" | `Warning -> "warning"

let pp_time ppf () =
  let tz_offset local utc =      (* computes the timezone offset w.r.t. utc. *)
    let dd = local.Unix.tm_yday - utc.Unix.tm_yday in
    let dh = local.Unix.tm_hour - utc.Unix.tm_hour in
    let dm = dh * 60 + (local.Unix.tm_min - utc.Unix.tm_min) in
    if dd = 1 || dd < -1 (* year wrap *) then dm + (24 * 60) else
    if dd = -1 || dd > 1 (* year wrap *) then dm - (24 * 60) else
    dm (* same day *)
  in
  let now = Unix.gettimeofday () in
  let local = Unix.localtime now in
  let utc = Unix.gmtime now in
  let tz = tz_offset local utc in
  Format.fprintf ppf "%04d-%02d-%02d %02d:%02d:%02d%c%02d%02d"
    (local.Unix.tm_year + 1900) (local.Unix.tm_mon + 1) local.Unix.tm_mday
    local.Unix.tm_hour local.Unix.tm_min local.Unix.tm_sec
    (if tz < 0 then '-' else '+') (tz / 60) (tz mod 60)

let pp_start ppf l = Format.fprintf ppf "%a [%s] @[" pp_time () (str_of_level l)
let pp_end ppf = Format.fprintf ppf "@]@."
let nil_reporter =
  { report = fun _ fmt -> Format.ifprintf Format.err_formatter fmt }

let formatter_reporter verbosity ppf = { report = fun l fmt ->
  if drop_msg verbosity l then Format.ifprintf ppf fmt else
  let prefix = match l with
  | `Debug -> "Debug: " | `Error -> "Error: " | `Warning -> "Warning: "
  | `Info -> "Info: "
  in
  let flush ppf = Format.pp_print_newline ppf () in
  Format.pp_print_string ppf prefix;
  Format.kfprintf flush ppf fmt
}

let timed_formatter_reporter verbosity ppf = { report = fun l fmt ->
  if drop_msg verbosity l then Format.ifprintf ppf fmt else
  (pp_start ppf l; Format.kfprintf pp_end ppf fmt) }

let errors = ref 0
let r = ref nil_reporter
let set_reporter r' = r := r'
let log l fmt = if l = `Error then incr errors; !r.report l fmt
let err fmt = log `Error fmt
let warn fmt = log `Warning fmt
let info fmt = log `Info fmt
let debug fmt = log `Debug fmt
let default_log = Log.({ log; err; warn; info; debug })

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
