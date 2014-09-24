(*---------------------------------------------------------------------------
   Copyright 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Additional man pages *)

let luigi_manual = "Luigi manual"
let luigi_version = "Luigi %%VERSION%%"
let conf_page = ("LUIGI-CONF", 5, "", luigi_version, luigi_manual),
  [ `S "NAME";
    `P "luigi-conf - information about Luigi's configuration file";
    `S "DESCRIPTION";
    `P "A luigi configuration file. TODO";
    `S "CONFIGURATION OPTIONS";
    `S "SEE ALSO";
    `P "$(b,luigi)(1)"; ]

let webserver_page = ("LUIGI-WEBSERVER", 7, "", luigi_version, luigi_manual),
  [ `S "NAME";
    `P "luigi-webserver - information about Luigi's web server interaction";
    `S "DESCRIPTION";
    `P "TODO";
    `S "SEE ALSO";
    `P "$(b,luigi)(1)"; ]

let pages = [ "conf", conf_page; "webserver", webserver_page ]

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
