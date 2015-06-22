(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open React

type t = { el : Br.el; }

let space = `Txt (Br.str " ")

let publisher_home ui =
  let r = Ui_init.repo ui in
  let caret_right = `El Ui_icon.(v caret_right) in
  let locale = D.Ui_locale.locale (Ui_init.locale ui) in
  let pub = Locv.get locale (Dapi.Repo.publisher r) in
  let name = `Txt (D.Ui_link.text pub) in
  let atts = [Br.Att.href, D.Ui_link.href pub] in
  let classes = [Br.str "button"; Br.str "link"] in
  let link = Br.El.v Br.El.a ~atts ~classes [name; space; caret_right] in
  let classes = [Br.str "publisher_home"] in
  Br.El.v Br.El.div ~classes [`El link]

let repo_home ui =
  let r = Ui_init.repo ui in
  let bars = `El Ui_icon.(v bars) in
  let locale = D.Ui_locale.locale (Ui_init.locale ui) in
  let href = D.Ui_locale.href (Ui_init.locale ui) in
  let href = S.const (Br.Str.app (Ui_init.ui_base ui) href) in
  let name = `Txt (Locv.get locale (Dapi.Repo.name r)) in
  let classes = S.const [ Br.str "button"; Br.str "link"; ] in
  let link = Brc.link ~classes ~href (S.const [bars; space; name]) in
  let classes = [ Br.str "home" ] in
  Br.(el El.div ~classes [`El (Brc.link_el link) ])

let locale_menu ui =
  let icon_down = `El Ui_icon.(v caret_down) in
  let locale = D.Ui_locale.locale (Ui_init.locale ui) in
  let locale = `Txt (Br.str (Nlang.to_string locale)) in
  let classes = [Br.str "locale_menu"; Br.str "button"] in
  Br.(el El.span ~classes [ locale; space; icon_down ])

let search_scope ui =
  let down = `El Ui_icon.(v caret_down) in
  let scope = `El (Brr.El.v Br.El.span Ui_msg.search_all) in
  let classes = [Br.str "label"; Br.str "button" ] in
  Br.(el El.div ~classes [scope; space; down ])

let search ui =
  let childs = match Dapi.Repo.search_href (Ui_init.repo ui) with
  | None -> []
  | Some href ->
      let scope = `El (search_scope ui) in
      let atts =
        S.map (fun s -> [Br.Att.placeholder, s]) Ui_msg.search_search
      in
      let input = `El (Brr.El.v Br.El.input ~atts (S.const [])) in
      let classes = [Br.str "button" ] in
      let button = `El Br.(el El.button ~classes ([`El Ui_icon.(v search)])) in
      [scope; input; button]
  in
  Br.(el El.div ~classes:[Br.str "search"] childs)

let repo_tools ui =
  let classes = [Br.str "button" ] in
  let fullscreen = `El Ui_icon.(v ~classes expand) in
  let locale_menu = `El (locale_menu ui) in
  let classes = [Br.str "repo_tools"] in
  Br.(el El.div ~classes [fullscreen; locale_menu])

let v ui =
  let repo_home = `El (repo_home ui) in
  let search = `El (search ui) in
  let repo_tools = `El (repo_tools ui) in
  let publisher_home = `El (publisher_home ui) in
  let el = Br.(el El.header [repo_home; search;
                             repo_tools;
                             publisher_home; ]) in
  { el }

let el h = h.el


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
