(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open React

(* DOM external references *)

let root_id = Js.string "remat"                    (* div id for the viewer. *)
let data_log_level = Js.string "data-log-level"       (* attr for log level. *)
let data_ui_base = Js.string "data-ui-base"        (* attr for base ui href. *)
let data_api_base = Js.string "data-api-base"    (* attr for  api base href. *)

(* Errors *)

let str = Format.asprintf

let err_miss_att a =
  str "missing attribute `%a' on %a element" Br.Str.pp a Br.Str.pp root_id

let err_no_root_el () =
  str "no element with id `%a' in document" Br.Str.pp root_id

let err_ui_base b loc =
  str "not published under base URI `%a' (current location: `%a')"
    Br.Str.pp b Br.Str.pp loc

let err_miss_locale = "no locale defined in document repository"
let err_unknown_locale_prefix l =
  str "unknown locale href prefix in repo: `%a'" Br.Str.pp l

(* Initalisation. *)

type ui_state = Br.str list * Br.str

type raw_state =
  { path : Br.str list;
    query : Br.str;
    frag : Br.str; }

type t =
  { root_el : Br.el;
    ui_base : Br.str;
    api_base : Br.str;
    repo : Dapi.Repo.t;
    locale : D.Ui_locale.t;
    raw_state : raw_state; }

let root_el ui = ui.root_el
let ui_base ui = ui.ui_base
let api_base ui = ui.api_base
let repo ui = ui.repo
let locale ui = ui.locale
let raw_state ui = ui.raw_state

let init_log root = match Br.El.att root data_log_level with
| None -> ()
| Some l ->
    let l = match Js.to_string l with
    | "show" -> Some Br.Log.Show
    | "error" -> Some Br.Log.Error
    | "warning" -> Some Br.Log.Warning
    | "info" -> Some Br.Log.Info
    | "debug" -> Some Br.Log.Debug
    | "none" -> None
    | l -> Br.Log.warn "unknown log level: `%s'" l; Br.Log.level ()
    in
    Br.Log.set_level l

let default_locale repo = match Dapi.Repo.ui_locales repo with
| [] -> Br.Log.err "%s" err_miss_locale; None
| default :: _ as all ->
    let langs =
      let add_lang acc l = match Nlang.of_string (Br.Str.to_string l) with
      | `Error msg -> Br.Log.warn "%s" msg; acc
      | `Ok l -> l :: acc
      in
      List.rev (List.fold_left add_lang [] (Br.Info.languages ()))
    in
    (* FIXME have a look at RFC 4647 for matching *)
    let find_lang lang =
      let matching_locale loc = (D.Ui_locale.locale loc) = lang in
      (try Some (List.find matching_locale all) with Not_found -> None)
    in
    let rec lookup = function
    | l :: ls -> (match find_lang l with Some l -> l | None -> lookup ls)
    | [] -> default
    in
    Some (lookup langs)

let init_locale app_uri repo =
  let locale_href, path = match app_uri with
  | [] -> None, []
  | [l] when Br.Str.is_empty l -> None, []
  | l :: path -> Some l, path
  in
  let locale = match locale_href with
  | None -> default_locale repo
  | Some href ->
      let matching_href loc = (D.Ui_locale.href loc) = href in
      let locales = Dapi.Repo.ui_locales repo in
      match (try Some (List.find matching_href locales) with Not_found -> None)
      with
      | Some loc -> Some loc
      | None ->
          Br.Log.err "%s" (err_unknown_locale_prefix href); default_locale repo
  in
  match locale with
  | None -> failwith "TODO" (* FIXME error *)
  | Some l ->
      let raw_state =
        { path; query = Br.Loc.query (); frag = Br.Loc.fragment ()}
      in
      (l, raw_state)

let get_ui_msgs api_href =
  let uri = Js.string "remat-msgs.json" (* FIXME *) in
  let r = Req.get_json uri D.locv_msgs_codec in
  (* TODO errors *)
  Req.value r

let get_repo api_href =
  let uri = Br.Str.app api_href (Js.string "api/repo.json") in
  let r = Req.get_json uri Dapi.Repo.codec in
  (* TODO errors *)
  Req.value r

let v () = match Br.El.of_id root_id with
| None -> `Error (err_no_root_el ())
| Some root_el ->
    init_log root_el;
    match Br.El.att root_el data_ui_base with
    | None -> `Error (err_miss_att data_ui_base)
    | Some ui_base ->
        let api_base = match Br.El.att root_el data_api_base with
        | None -> ui_base
        | Some href -> href
        in
        let loc = Br.Loc.path () in
        let app_uri = match Br.Str.chop ~prefix:ui_base loc with
        | None -> Br.Log.err "%s" (err_ui_base ui_base loc); []
        | Some app_uri -> Br.Str.split (Br.str "/") app_uri
        in
        let repo = get_repo api_base in
        let msgs = get_ui_msgs api_base in
        let locale_init = E.map (init_locale app_uri) repo in
        Brr.Sink.event (E.map Ui_msg.set_msgs msgs);
        `Ok (E.l2 (fun repo (locale, raw_state) ->
            Ui_msg.set_locale (D.Ui_locale.locale locale);
            { root_el; ui_base; api_base; repo; locale; raw_state})
            repo locale_init)


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
