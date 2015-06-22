(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos

let quote = Cmdliner.Arg.doc_quote

let pp_status ff (f, r) = match r with
| `Status s -> Fmt.pp ff "%s" s
| _ -> Fmt.pp ff "%s" f

let filename_update_suffix f suff = try (Filename.chop_extension f) ^ suff with
| Invalid_argument _ -> (* no ext *) f ^ suff

let truncate f = try close_out (open_out_bin f) with
| Sys_error e -> Ui.log_err "error truncate %s: %s" f e

let apply f x ~finally y =
  let result = try f x with exn -> finally y; raise exn in
  finally y;
  result

type convert_func_log =
  { ic : 'a. ('a, Format.formatter, unit, unit) Pervasives.format4 -> 'a;
    oc : 'a. ('a, Format.formatter, unit, unit) Pervasives.format4 -> 'a; }

let finereader_to_pbin log _ ic oc =
  let fr_log (l, c) e = log.ic "%d.%d: %s" l c e in
  let pdoc_log m = log.ic "[%d] %a"
    (Unix.getpid ())
    Pdoc.pp_fineReader_conversion_msg m
  in
  let src = `Channel ic in
  try
    let d = FineReader.input ~err:fr_log src in
    let pd = Pdoc.of_fineReader ~log:pdoc_log d in
    Pdoc.output oc pd; `Ok
  with Xmlm.Error (loc, e) -> fr_log loc (Xmlm.error_message e); `Error

let pbin_to_ptext log _ ic oc = match Pdoc.input ic with
| `Error -> log.ic "invalid file format or corrupted file"; `Error
| `Ok d -> Fmt.pp (Format.formatter_of_out_channel oc) "%a@?" Pdoc.pp_txt d; `Ok

type fmt = [ `FineReader | `Pdoc_binary | `Pdoc_text | `Pdoc_json ]

let string_to_fmt_assoc =
  [ "finereader", `FineReader;
    "pbin", `Pdoc_binary;
    "ptext", `Pdoc_text; ]

let fmt_to_string = function
| `FineReader -> "finereader"
| `Pdoc_binary -> "pbin"
| `Pdoc_text -> "ptext"
| `Pdoc_json -> "pjson"

let fmt_suffix = function
| `FineReader -> ".xml"
| `Pdoc_binary -> ".pbin"
| `Pdoc_text -> ".p.txt"
| `Pdoc_json -> ".p.json"

let fmt_conversions = function
| `FineReader -> [ `Pdoc_binary, finereader_to_pbin]
| `Pdoc_binary -> [ `Pdoc_text, pbin_to_ptext]
| _ -> []

type io =
  { i_fmt : fmt;
    i_suffix : string;
    i : string list;
    o_fmt : fmt;
    o_suffix : string;
    o : [`Dir of string | `File of string ] option; }

let io i_fmt i_suffix i o_fmt o_suffix o_file o_dir =
  let suff fmt s = match s with Some s -> s | None -> fmt_suffix fmt in
  let i_suffix = suff i_fmt i_suffix in
  let o_suffix = suff o_fmt o_suffix in
  let o = match o_dir with Some o -> Some (`Dir o)
  | None -> match o_file with Some o -> Some (`File o) | None -> None
  in
  { i_fmt; i_suffix; i; o_fmt; o_suffix; o }

let err_convert i o convs =
  let msg = match convs with
  | [] -> strf "%s is an output only format it cannot be converted."
    ( quote (fmt_to_string i))
  | _ ->
    strf "%s can be converted to %s but not to %s."
      (quote (fmt_to_string i))
      (String.concat ", " (List.map (fun (f, _) -> fmt_to_string f) convs))
      (quote (fmt_to_string o))
  in
  `Error (true, msg)

let convert io init =
  let convs = fmt_conversions io.i_fmt in
  let o_fmt (fmt, _) = fmt = io.o_fmt in
  match try Some (List.find o_fmt convs) with Not_found -> None with
  | None -> err_convert io.i_fmt io.o_fmt convs
  | Some (_, f) ->
    let conv f args io input =                    (* executed by workers. *)
      let err_status () = `Status (strf "error on %s" input) in
      try
        let ic = if input <> "-" then open_in_bin input else stdin in
        let close_i ic = if input <> "-" then close_in ic else () in
        let with_ic ic =
          let output, flags =
            let flags = [Open_wronly; Open_binary; Open_creat ] in
            let truncate = Open_trunc :: flags in
            match io.o with
            | None -> filename_update_suffix input io.o_suffix, truncate
            | Some (`File o) -> o, Open_append :: flags
            | Some (`Dir d) ->
              let f = Filename.concat d (Filename.basename input) in
              filename_update_suffix f io.o_suffix, truncate
          in
          let oc =
            if output <> "-" then open_out_gen flags 0o664 output else stdout
          in
          let close_o oc = if output <> "-" then close_out oc else () in
          let with_oc oc =
            let log =
              { ic = (fun fmt -> Ui.log_err ("%s: " ^^ fmt) input);
                oc = (fun fmt -> Ui.log_err ("%s: " ^^ fmt) output); }
            in
            match f log args ic oc with
            | `Error -> err_status ()
            | `Ok -> `Status input
            | `Size s -> `Size s
          in
          apply with_oc oc ~finally:close_o oc
        in
        apply with_ic ic ~finally:close_i ic
      with
      | Sys_error e -> Ui.log_err "%s" e; err_status ()
    in
    let workers = match io.o with Some (`Dir _) | None -> init.Cmd_base.workers
      | Some (`File o) -> if o <> "-" then truncate o; 1
    in
    let args = () (* for now *) in
    let m = Mapf.map ~workers ~suffix:io.i_suffix (conv f args io) io.i in
    let action (f, r) = match r with
      | `Size s -> Ui.log "size: %.1f Mo" ((float s) /. 1024.)
      | _ -> ()
    in
    Ui.show_mapf pp_status action m; `Ok 0

(* Command line interface *)

open Cmdliner

let io =
  let fmt = Arg.enum string_to_fmt_assoc in
  let i_fmt =
    let doc = "Input file format, see below for a list." in
    Arg.(required & pos 0 (some fmt) None & info [] ~docv:"I_FMT" ~doc)
  in
  let o_fmt =
    let doc = "Output file format, see below for a list." in
    Arg.(required & pos 1 (some fmt) None & info [] ~docv:"O_FMT" ~doc)
  in
  let i =
    let doc = "Files and directories to process. Directories are processed
               recursively and the file name `-' is standard input."
    in
    Arg.(non_empty & pos_right 1 file [] & info [] ~docv:"INPUT" ~doc)
  in
  let i_suffix =
    let doc = "In directories only files ending with $(docv) are processed.
               Default depends on $(i,I_FMT)."
    in
    let docv = "SUFFIX" in
    Arg.(value & opt (some string) None & info ["I"; "in-suffix"] ~docv ~doc)
  in
  let o_suffix =
    let doc = "Use $(docv) for the suffix of output files. Default depends on
               $(i,O_FMT)."
    in
    let docv = "SUFFIX" in
    Arg.(value & opt (some string) None & info ["O"; "out-suffix"] ~docv ~doc)
  in
  let o_file =
    let doc = "Output to $(docv), the file name `-' is stdout." in
    let docv = "FILE" in
    Arg.(value & opt (some string) None & info ["o"; "output"] ~docv ~doc)
  in
  let o_dir =
    let doc = "Write output files in $(docv). If two input files have the same
              name, some output will be overwritten."
    in
    let docv = "DIR" in
    Arg.(value & opt (some dir) None & info ["d"; "output-dir"] ~docv ~doc)
  in
  Term.(pure io $ i_fmt $ i_suffix $ i $ o_fmt $ o_suffix $ o_file $ o_dir)

let cmd =
  let doc = "converts OCR data from/to various formats and representations." in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,$(tname)) command converts OCR data from/to various formats and
        representations.";
    `P "The argument $(i,I_FMT) specifies the input format and $(i,O_FMT)
        the output format. See below for a list of supported formats and
        conversions.";
    `P "The command processes the $(i,INPUT) files and directories. Directories
        are processed recursively. In directories only files matching the suffix
        specified with $(b,-I) are processed. If $(b,-I) is unspecified
        a default suffix is chosen according to $(i,I_FMT). The
        file name `-' denotes standard input.";
    `P "There are three different ways to output the conversion results:";
    `P "1. If no output option is specified, the result for each input
        file is written to the same path except that the suffix is changed to
        the value specified with $(b,-O). If $(b,-O) is unspecified a
        default suffix is chosen according to $(i,O_FMT).";
    `P "2. If the $(b,-d) $(i,DIR) option is specified, the result for each
        input file is written in $(i,DIR) with the same file name and the
        suffix changed to the value specified with $(b,-O). If $(b,-O) is
        unspecified a default suffix is chosen according to $(i,O_FMT).
        If the same file name appears twice in the input files, some output
        will be overwritten.";
    `P "3. If the $(b,-o) $(i,FILE) option is specified, all the results are
        sequentially written to $(i,FILE). The file name `-' denotes standard
        output. Currently, this option disables multiprocessing.";
    `S "SUPPORTED FORMATS AND CONVERSIONS";
    `P "Formats are listed in alphabetical order. For each format
        we indicate, the format name to use in $(i,I_FMT) or $(i,O_FMT),
        the default file suffix in parenthesis, and the formats to which
        it can be converted.";
    `I ("$(b,finereader) (.xml) -> pbin",
        "The XML representation output by the ABBYY FineReader engine 6v1.
          $(i,http://www.abbyy.com/FineReader_xml/FineReader6-schema-v1.xml)");
    `I ("$(b,pbin) (.pbin) -> ptext",
        "Remat's internal binary physical document model. This format is used
          as an efficient representation for FineReader's XML and is the
          Rosetta stone to most other representations. The format is
          undocumented and may change in the future, do not use for archival
          purposes.");
    `I ("$(b,ptext) (.p.txt) output only",
        "UTF-8 text file with the text in the order found in Remat's physical
         document model, page by page, block by block, line by line.");
    `S "SEE ALSO";
    `P "$(mname)(1)"; ]
  in
  let convert = Term.(pure convert $ io) in
  Cmd_base.cmd "convert" convert ~doc ~man ~see_also:[]

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
