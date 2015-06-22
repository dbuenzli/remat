(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open React

type msg = Br.El.child list
type msg_str = Br.str

let locale_default = Nlang.v "en"
let locale, set_locale = S.create locale_default
let msgs, set_msgs = S.create (Locv.any (Locv.Msg.Map.empty Nlang.any))
let msgs = S.l2 Locv.get locale msgs

let get_str id msgs = Br.str (Locv.Msg.(get (Map.get msgs id)))
let msg_str id = S.map (get_str id) msgs
let msg id =
  let get msgs = [`Txt (get_str id msgs)] in
  S.map get msgs

(* Navigation} *)

let nav_thumbs =
  msg ("nav_thumbs" (* [@localize "Preview"] *))

let nav_thumbs_tip =
  msg ("nav_thumbs_tip" (* [@localize "Show the document's page thumbnails"] *))

let nav_record =
  msg ("nav_record" (* [@localize "Record"] *))

let nav_record_tip =
  msg ("nav_record_tip" (* [@localize "Show the document's record"] *))

let nav_divs =
  msg ("nav_divs" (* [@localize "Divisions"] *))

let nav_divs_tip =
  msg ("nav_div_tip" (* [@localize "Show the document's divisons"] *))

let nav_pdf =
  msg ("nav_pdf" (* [@localize "PDF"] *))

let nav_pdf_tip =
  msg ("nav_pdf_tip" (* [@localize "Download the document in PDF format"] *))

let nav_txt =
  msg ("nav_txt" (* [@localize "TXT"] *))

let nav_txt_tip =
  msg ("nav_txt_tip" (* [@localize "Download the document in text format"] *))

let nav_help =
  msg ("nav_help" (* [@localize "Help"] *))

let nav_help_tip =
  msg ("nav_help_tip" (* [@localize "Show help"] *))

let nav_lang =
  msg ("nav_lang" (* [@localize "en"] *))

let nav_lang_tip =
  msg ("nav_lang_tip" (* [@localize "Switch language"] *))

let nav_settings =
  msg ("nav_settings" (* [@localize "Settings"] *))

let nav_settings_tip =
  msg ("nav_settings_tip" (* [@localize "Viewer settings"] *))

let nav_browse_tip =
  msg ("nav_browse_tip" (* [@localize "Browse the document repository"] *))

(* Page *)

let page_abbr =
  msg ("page_abbr" (* [@localized "Page abbreviation (p.)"] *))

let page_scale_fit =
  msg ("page_scale_fit" (* [@localize "Fit"] *))

let page_scale_fit_tip =
  msg ("page_scale_fit_tip" (* [@localize "Scale to fit the page"] *))

let page_scale_width =
  msg ("page_scale_width" (* [@localize "Width"] *))

let page_scale_width_tip =
  msg ("page_scale_width_tip"
  (* [@localize "Scale to fit the browser page width"] *))

let page_scale_actual =
  msg ("page_scale_actual" (* [@localize "1:1"] *))

let page_scale_actual_tip =
  msg ("page_scale_actual_tip"
  (* [@localize "Scale to the page's physical size"] *))

let page_scale_pct_tip =
  msg ("page_scale_pct_tip" (* [@localize "Scale to percentage"] *))

let page_unit_1cm =
  msg ("page_unit_1cm" (* [@localize "1cm"] *))

let page_text =
  msg ("page_text" (* [@localize "Text"] *))

let page_text_tip =
  msg ("page_text_tip" (* [@localize "Show the text of the page"] *))

(* Search *)

let search_search =
  msg_str ("search_search" (* [@localize "Search"] *))

let search_all =
  msg ("search_all" (* [@localize "Search all"] *))

let search_this_document =
  msg ("search_this_document" (* [@localize "Search this document"] *))

let search_this_collection =
  msg ("search_this_collection" (* [@localize "Search this collection"] *))

let search_highlight =
  msg ("search_highlight" (* [@localize "Terms"] *))

let search_highlight_tip =
  msg ("search_highlight_tip" (* [@localize "Highlight searched terms"] *))

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
