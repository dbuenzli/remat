(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

type 'a t = { workers : (string * 'a) Workers.t; count : int; }

let map_files ws suffix f files =
  let map_non_dirs ws suffix f files =
    let is_dir d = try Sys.is_directory d with Sys_error _ -> false in
    let has_suffix suff file = Filename.check_suffix file suff in
    let dirs, files = List.partition is_dir files in
    let files = List.find_all (has_suffix suffix) files in
    List.iter (Workers.apply ws f) files; dirs
  in
  let rec aux ws suffix f = function
    | (d :: ds) :: up ->
        let rdir d = try Array.to_list (Sys.readdir d) with Sys_error _ -> [] in
        let files = List.rev (List.rev_map (Filename.concat d) (rdir d)) in
        let dirs = map_non_dirs ws suffix f files in
        aux ws suffix f (dirs :: ds :: up)
    | [] :: up -> aux ws suffix f up
    | [] -> ()
  in
  let dirs = map_non_dirs ws "" f files in
  aux ws suffix f (dirs :: [])

let map ?workers ?(suffix = "") f files =
  let workers = Workers.create ?count:workers () in
  let f' s = s, f s in
  map_files workers suffix f' files;
  { workers; count = Workers.apply_count workers; }

let result ?timeout m =
  if Workers.apply_count m.workers = 0 then `End else
  let r = match Workers.result ?timeout m.workers with
  | `Ok r -> `File r | (`Exn _ | `Timeout) as r -> r
  in
  if Workers.apply_count m.workers = 0 then Workers.stop m.workers;
  r

let count m = m.count
let remaining m = Workers.apply_count m.workers

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
