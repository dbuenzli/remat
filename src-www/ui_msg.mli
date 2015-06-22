(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** End-user messages. *)

open React

val set_locale : ?step:React.step -> Nlang.t -> unit
val set_msgs : ?step:React.step -> Locv.msgs -> unit
type msg = Br.El.child list
type msg_str = Br.str

(** {1 Navigation} *)

val nav_thumbs : msg signal
val nav_thumbs_tip : msg signal
val nav_record : msg signal
val nav_record_tip : msg signal
val nav_divs : msg signal
val nav_divs_tip : msg signal
val nav_pdf : msg signal
val nav_pdf_tip : msg signal
val nav_txt : msg signal
val nav_txt_tip : msg signal
val nav_help : msg signal
val nav_help_tip : msg signal
val nav_lang : msg signal
val nav_lang_tip : msg signal
val nav_browse_tip : msg signal
val nav_settings : msg signal
val nav_settings_tip : msg signal

(** {1 Page} *)

val page_abbr : msg signal
val page_scale_fit : msg signal
val page_scale_fit_tip : msg signal
val page_scale_width : msg signal
val page_scale_width_tip : msg signal
val page_scale_actual : msg signal
val page_scale_actual_tip : msg signal
val page_scale_pct_tip : msg signal
val page_unit_1cm : msg signal
val page_text : msg signal
val page_text_tip : msg signal

(** {1 Search} *)

val search_search : msg_str signal
val search_all : msg signal
val search_this_document : msg signal
val search_this_collection : msg signal
val search_highlight : msg signal
val search_highlight_tip : msg signal
val search_this_document : msg signal

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
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
