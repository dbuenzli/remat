(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD3 license, see license at the end of the file.
   %%NAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Command implementations. *)


let reopen_logfile () = failwith "umipl"
(*
reopen_logfile()
{
    int lfd, rc;

    if(logfile == NULL)
        return 0;

    lfd = open(logfile, O_CREAT | O_WRONLY | O_APPEND, 0644);
    if(lfd < 0)
        return -1;

    fflush(stdout);
    fflush(stderr);

    rc = dup2(lfd, 1);
    if(rc < 0)
        return -1;

    rc = dup2(lfd, 2);
    if(rc < 0)
        return -1;

    if(lfd > 2)
        close(lfd);

    return 1;
}
*)

let fos = format_of_string
let msg_alive = fos "Remat's daemon is already running with PID %d."
let msg_dead = fos "Remat's daemon is not running."
let msg_not_found =
  fos "Remat's daemon PID file %s exists but no process %d found."

let init_log verb ppf = Log.set_reporter (Log.formatter_reporter verb ppf)

let init_config f = match Config.of_file f with
| `Error e -> `Error (false, e)
| `Ok c as r -> (* N.B. initially we log on stderr. *)
    init_log c.Config.log_verbosity Format.err_formatter; r

let default _ = `Help (`Pager, None)

let start c daemonize =
  if not daemonize then (ignore (Service.run c); `Ok 0) else
  match Daemon.status c.Config.pid_file with
  | `Alive pid ->  Log.info msg_alive pid; `Ok 0
  | `Not_found pid -> Log.err msg_not_found c.Config.pid_file pid; `Ok 1
  | `Error e -> Log.err "%s" e; `Ok 1
  | `Dead ->
      match Daemon.daemonize ~dir:"/" ~umask:0o022 with
      | `Parent `Ok -> `Ok 0
      | `Parent (`Not_forked e) -> Log.err "%s" e; `Ok 1
      | `Daemon (`Error e) ->
          (* reopen_logfile () *) Log.err "%s" e; `Ok 1
      | `Daemon `Ok  ->
          Log.info "daemonization sucessfull";
          Daemon.enable_command_reception (); (* sig handlers *)
          (* reopen_logfile () *)
          match Daemon.set_status c.Config.pid_file `Alive with
          | `Error e -> Log.err "%s" e; `Ok 1
          | `Ok ->
              let c' = Service.run c in
              match Daemon.set_status c'.Config.pid_file `Dead with
              | `Error e -> Log.err "%s" e; `Ok 1
              | `Ok -> `Ok 0

let send_command ~dead_ret c cmd = match Daemon.status c.Config.pid_file with
| `Error e -> Log.err "%s" e; `Ok 1
| `Not_found pid -> Log.err msg_not_found c.Config.pid_file pid; `Ok 1
| `Dead -> Log.err msg_dead; `Ok dead_ret
| `Alive pid ->
    match Daemon.send_command pid cmd with
    | `Error e -> Log.err "%s" e; `Ok 1
    | `Not_found -> Log.err msg_not_found c.Config.pid_file pid; `Ok 1
    | `Ok -> `Ok 0

let stop c = send_command ~dead_ret:0 c Daemon.Stop
let reload c = send_command ~dead_ret:1 c Daemon.Restart
let reopen_logs c = send_command ~dead_ret:1 c Daemon.Reopen_logs

let status c = match Daemon.status c.Config.pid_file with
| `Alive pid -> Format.printf msg_alive pid; `Ok 0
| `Not_found pid -> Format.printf msg_not_found c.Config.pid_file pid; `Ok 1
| `Dead -> Format.printf msg_dead; `Ok 3
| `Error e -> Log.err "%s" e; `Ok 4

let help _ man_format topic commands = match topic with
| None -> `Help (man_format, None)
| Some topic ->
    let topics = "topics" :: commands in
    let topics = List.rev_append (List.rev_map fst Man.pages) topics in
    let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
    match conv topic with
    | `Error e -> `Error (false, e)
    | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok 0
    | `Ok t when List.mem t commands -> `Help (man_format, Some t)
    | `Ok t ->
        let page = try List.assoc t Man.pages with Not_found -> assert false in
        Cmdliner.Manpage.print man_format Format.std_formatter page; `Ok 0

(* Command line interface *)

open Cmdliner;;

(* Options common to all commands. *)

let copts_sec = "COMMON OPTIONS"
let copts_man = [ `S copts_sec; `P "These options are common to all commands." ]

let config =
  let config_file =
    let default = try Sys.getenv "LUIGI_CONF" with
    | Not_found -> "/etc/remat/rematd.conf" (* TODO config var *)
    in
    let doc = "Use $(docv) for the configuration file." in
    Arg.(value & opt string default & info ["c"; "config"] ~docv:"FILE"
           ~doc ~docs:copts_sec)
  in
  Term.(ret (pure init_config $ config_file))

(* Commands *)

let start =
  let daemonize =
    Arg.(value & flag & info ["D"; "daemonize"] ~doc:"Daemonize the service.")
  in
  let doc = "start Remat's daemon" in
  let man = [
    `S "DESCRIPTION";
    `P "This command starts the daemon."] @ copts_man @ [
    `S "SEE ALSO";
    `P "$(b,$(mname))(1)" ]
  in
  Term.(ret (pure start $ config $ daemonize)),
  Term.info "start" ~sdocs:copts_sec ~doc ~man

let stop =
  let doc = "stop Remat's daemon" in
  let man = [
    `S "DESCRIPTION";
    `P "The command $(b,$(tname)) stops Remat's deamon. This can also be
        achieved by sending SIGQUIT signal to the daemon."] @ copts_man @ [
    `S "SEE ALSO";
    `P "$(b,$(mname))(1)" ]
  in
  Term.(ret (pure stop $ config)),
  Term.info "stop" ~sdocs:copts_sec ~doc ~man

let reload =
  let doc = "reload Remat's configuration" in
  let man = [
    `S "DESCRIPTION";
    `P "The command $(b,$(tname)) the configuration file without stopping
        and restarting the daemon.";
    `P "It also closes and reopens the index, the dictionnary and the log files
        accordingly."; ] @ copts_man @ [
    `S "SEE ALSO";
    `P "$(b,$(mname))(1)" ]
  in
  Term.(ret (pure reload $ config)),
  Term.info "reload" ~sdocs:copts_sec ~doc ~man

let status =
  let doc = "return Remat's running status" in
  let man = [
    `S "DESCRIPTION";
    `P "The command $(b,$(tname)) prints Remat's status on stdout and
        returns."; ] @ copts_man @ [
    `S "EXIT CODES";
    `I ("0", "Remat is running.");
    `I ("1", "Remat is not running but pid file exists.");
    `I ("3", "Remat is not running.");
    `I ("4", "Remat's status is unknown");
    `S "SEE ALSO";
    `P "$(b,$(mname))(1)" ]
  in
  Term.(ret (pure status $ config)),
  Term.info "status" ~sdocs:copts_sec ~doc ~man

let help =
  let topic =
    let doc = "The topic to get help on, `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "show help about Remat" in
  let man = [
    `S "DESCRIPTION";
    `P "The command $(b,$(tname)) shows help about $(mname) and its commands.";
    `P "Use `topics' as $(i,TOPIC) to get a list of topics." ] @ copts_man @ [
    `S "SEE ALSO";
    `P "$(b,$(mname))(1)"; ]
  in
  Term.(ret (pure help $ config $ Term.man_format $ topic $ Term.choice_names)),
  Term.info "help" ~sdocs:copts_sec ~doc ~man

let default =
  let version = "%%VERSION%%" in
  let doc = "%%SYNOPSIS%%" in
  let man = [
    `S "DESCRIPTION";
    `P "$(b,rematd) is a service to search and display
        digitized documents on the World Wide Web. It interacts with a
        web server via SCGI, see $(b,rematd-webserver)(7).";
    `P "Use 'rematd help config' for help about Remat's configuration.";
    `Noblank;
    `P "Use 'rematd help webserver' for help about web server configuration.";
    `Noblank;
    `P "Use 'rematd help $(i,COMMAND)' for help about $(i,COMMAND).";
    `Noblank;
    `P "Use 'rematd help topics' for a list of help topics.";
    `S "ENVIRONMENT";
    `I ("$(i,LUIGI_CONF)", "The configuration file to use. The command line
         option $(b,--conf) takes precedence over the environment variable.");
    `S "SIGNALS";
    `I ("SIGTERM, SIGINT",
        "Quick daemon termination (doesn't handle pending requests).");
    `I ("SIGQUIT",
        "Graceful daemon termination, equivalent to $(b,rematd-stop)(1).");
    `I ("SIGHUP",
        "Daemon configuration reload, equivalent to $(b,rematd-reload)(1).");
    `I ("SIGUSR1",
        "Reopens the log files, equivalent to $(b,rematd-reopen-logs)(1)");
    `S "BUGS AND FEEDBACK";
    `P "Email them to <%%EMAIL%%>.";
    `S "AUTHOR";
    `P "Daniel C. Buenzli, $(i,http://erratique.ch)";
    `S "SEE ALSO";
    `P "TODO" ]
  in
  Term.(ret (pure default $ config)),
  Term.info "rematd" ~version ~sdocs:copts_sec ~doc ~man

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
