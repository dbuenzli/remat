(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Prelude

(* Initialization is common to all commands *)

type init =
  { fmt_utf_8_enabled : bool;
    fmt_style_tags : [ `Ansi | `None ];
    log_level : Log.level option;
    workers : int; }

let set_init c =
  Fmt.set_utf_8_enabled c.fmt_utf_8_enabled;
  Fmt.set_style_tags c.fmt_style_tags;
  Log.set_level c.log_level;
  ()

let color_enum = ["auto", `Auto; "always", `Always; "never", `Never]
let color_doc = Cmdliner.Arg.doc_alts_enum color_enum
let color_conv = Cmdliner.Arg.enum color_enum

let log_level_enum =
  [ "quiet", None; "error", Some Log.Error; "warning", Some Log.Warning;
    "info", Some Log.Info; "debug", Some Log.Debug; ]
let log_level_doc = Cmdliner.Arg.doc_alts_enum log_level_enum
let log_level_conv = Cmdliner.Arg.enum log_level_enum

(* Environment variables *)

let env_bool e = match Sysm.env e with
| None -> None
| Some v ->
    match String.lowercase v with
    | "" | "false" | "0" -> Some false
    | _ -> Some true

let env_enum e enum_def = match Sysm.env e with
| None -> None
| Some v ->
    let v = String.lowercase v in
    try Some (List.assoc v enum_def) with Not_found -> None

let var_color = "REMAT_COLOR"
let var_utf8_msgs = "REMAT_UTF8_MSGS"
let var_verbose = "REMAT_VERBOSE"

let man_vars =
  let doc var doc = `I (str "$(i,%s)" var, doc) in
  [ doc var_color "See option $(b,--color).";
    doc var_utf8_msgs "Use UTF-8 characters in $(mname) messages.";
    doc var_verbose "See option $(b,--verbose)."; ]

let init color verb workers =
  (* Override command line with environment variables *)
  let override value ~on = match on with None -> value | Some v -> v in
  let fmt_utf_8_enabled = override true ~on:(env_bool var_utf8_msgs) in
  let fmt_style_tags =
    match override color ~on:(env_enum var_color color_enum) with
    | `Auto (* FIXME when Unix.is_atty stdout, $TERM  *) -> `Ansi
    | `Always -> `Ansi
    | `Never -> `None
  in
  let log_level = override verb ~on:(env_enum var_verbose log_level_enum) in
  { fmt_utf_8_enabled; fmt_style_tags; log_level; workers; }

(* Command line interface *)

open Cmdliner

let docs = "COMMON OPTIONS"

let color_opt  =
  let doc = str "Colorize the output. $(docv) must be %s." color_doc in
  Arg.(value & opt color_conv `Auto & info ["color"] ~doc ~docv:"WHEN" ~docs)

let verbose_opts =
  let verbose =
    Arg.(value & opt ~vopt:(Some Log.Info) log_level_conv (Some Log.Warning) &
         info ["v"; "verbose"] ~docs ~docv:"LEVEL"
           ~doc:(str "Be more or less verbose. $(docv) must be %s."
                   log_level_doc))
  in
  let quiet =
    let doc = "Be quiet. Takes over $(b,--verbose)." in
    Arg.(value & flag & info ["q"; "quiet"] ~doc ~docs)
  in
  let choose quiet verbose_opt = if quiet then None else verbose_opt in
  Term.(pure choose $ quiet $ verbose)

let workers =
  let doc = "Number of worker processes to use. Defaults to machine processor
             count."
  in
  Arg.(value & opt int (Workers.cpu_count ()) & info [ "j"; "jobs" ] ~doc ~docs)

let help_sections =
  [ `S docs;
    `P "These options are common to all commands.";
    `S "ENVIRONMENT";
    `P "$(mname) commands make use of the following environment variables:";
  ] @ man_vars @
  [ `S "BUGS";
    `P "Check bug reports at https://github.com/dbuenzli/remat/issues.";
    `S "AUTHORS";
    `P "Daniel C. Buenzli <daniel.buenzl i@erratique.ch>"; ]

let see_also_section cmds =
  let base = if cmds = [] then "$(b,rematd)(1)" else "$(b,$(mname))(1)" in
  let see_also = List.map (str "$(b,$(mname)-%s)(1)") cmds in
  let see_also = String.concat ", " (base :: see_also) in
  [ `S "SEE ALSO"; `P see_also ]

let cmd name cmd ~doc ~man ~see_also =
  let man = man @ help_sections @ see_also_section see_also in
  let wrap init cmd = set_init init; cmd init in
  let init = Term.(pure init $ color_opt $ verbose_opts $ workers) in
  Term.(ret (pure wrap $ init $ cmd)),
  Term.info name ~version:"%%VERSION%%" ~doc ~sdocs:docs ~man

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
