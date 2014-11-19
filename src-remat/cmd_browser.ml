(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let browser i_suffix src dst nmap init =
  try
    let image f = Filename.check_suffix f i_suffix in
    let files = List.filter image (Array.to_list (Sys.readdir src)) in
    let b = Browser.of_files nmap files in
    match Browser.output_files b dst with
    | `Ok -> `Ok 0
    | `Error e -> Ui.log_err "%s" e; `Ok 1
  with Sys_error e -> Ui.log_err "%s" e; `Ok 1

(* Command line interface *)

open Cmdliner

let src =
  let doc = "Input directory with image files." in
  Arg.(required & pos 0 (some dir) None & info [] ~docv:"SRC" ~doc)

let dst =
  let doc = "Output directory." in
  Arg.(required & pos 1 (some dir) None & info [] ~docv:"DST" ~doc)

let i_suffix =
  let doc = "In $(i,SRC) only files ending with $(docv) are processed." in
  let docv = "SUFFIX" in
  Arg.(value & opt string ".png" & info ["I"; "in-suffix"] ~docv ~doc)

let names =
  let doc = "Map the filename prefix $(i,PREFIX) to $(i,NAME) in the browsing
               interface. This option is repeatable."
  in
  let docv = "PREFIX,NAME" in
  Arg.(value & opt_all (pair string string) [] & info ["m";"map"] ~docv ~doc)

let cmd =
  let doc = "generate an OCR data browsing interface" in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,$(tname)) command generates a primitive browsing interface
        for visualising OCR data.";
    `P "A browsing hierarchy for the images in the $(i,SRC) directory
        is defined according to their file name (see below). The resulting
        interface is written in the $(i,DST) directory as a set of HTML and
        JavaScript files.";
    `S "FILENAME CONVENTION";
    `P "The general pattern is:";
    `P "$PREFIX_YYYY_MM_DD[_XX]*_PAGENUM[_XX]*.SUFFIX";
    `P "The analysis starts from the end, the first parsed number defines
        PAGENUM, after that at three or less numbers are parsed, defining
        an optional day DD, optional month MM and optional year YYYY.";
    `P "The PREFIX part can be mapped in the browsing
        interface to a more readable name with the $(b,-m)
        option. Unique prefixes together with the [_XX] part define top
        level browsing groups.";
    `P "The YYYY_MM_DD part defines a date based browsing group.";
    `P "Files that share the same prefix until PAGENUM are presented on
        the same page.";
    `S "SEE ALSO";
    `P "$(mname)(1)" ]
  in
  let browser = Term.(pure browser $ i_suffix $ src $ dst $ names) in
  Cmd_base.cmd "browser" browser ~doc ~man ~see_also:[]

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
