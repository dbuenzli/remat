(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Bos

let cmd_error = function
| Error (`Msg e) -> `Error (false, e)
| Ok v -> `Ok v

let publish repo_dir dest force init =
  begin
    let dst = Path.of_string dest in
    OS.Dir.exists dst >>= fun exists ->
    if exists && not force
    then R.error_msgf "%s already exists use `-f' to overwrite" dest else
    begin
      Descr.create (Path.of_string repo_dir)
      >>= (fun repo -> Api.(write (Api_gen.create repo dst)))
      >>= fun () -> R.ok 0
    end
    |> Log.kon_error_msg ~use:(Ok 1)
  end
  |> cmd_error

(* Command line interface *)

open Cmdliner

let repo =
  let default = Filename.concat Filename.current_dir_name "remat-repo" in
  let doc = "Repository description directory." in
  Arg.(value & opt dir default & info ["repo"] ~docv:"DIR" ~doc)

let dest =
  let default = Filename.concat Filename.current_dir_name "remat-data" in
  let doc = "Destination directory (must not exist)." in
  Arg.(value & pos 0 string default & info [] ~docv:"DEST" ~doc)

let force =
  let doc = "Do not fail if destination directory exists." in
  Arg.(value & flag & info ["f"; "force"] ~doc)

let cmd =
  let doc = "generate the webserver static data files for a repository" in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,$(tname)) command generates the static data files that
        can be used with Remat's web client.";
    `S "SEE ALSO";
    `P "$(mname)(1)" ]
  in
  let publish = Term.(pure publish $ repo $ dest $ force) in
  Cmd_base.cmd "publish" publish ~doc ~man ~see_also:[]

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli.
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
