(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Version *)

let version = 0

(* URIs *)

type uri = Jsont.nat_string
let uri_codec =
  Jsont.(with_default (nat_string_of_string "http://example.org") nat_string)

(* Language tag *)

let lang_codec =
  let dec s = Nlang.of_string s in
  let enc l = Nlang.to_string l in
  Jsont.view ~default:(Nlang.v "en") (dec, enc) Jsont.string

(* Document IDs *)

type doc_id = Jsont.nat_string
let doc_id_codec =
  Jsont.(with_default (nat_string_of_string "UNKNOWN") nat_string)

(* Index IDs *)

type index_id = Jsont.nat_string
let index_id_codec =
  Jsont.(with_default (nat_string_of_string "UNKNOWN") nat_string)

(* Document types *)

type doc_type = [ `Views | `Set ]
let doc_type_codec = Jsont.enum ["views", `Views; "set", `Set]

(* Document views types *)

type doc_views_type = [`Monograph | `Serial_issue | `Image ]
let doc_views_type_codec =
  Jsont.enum ["monograph", `Monograph; "serial_issue", `Serial_issue;
              "image", `Image ]

(* Document set types *)

type doc_set_type = [`Collection | `Serial ]
let doc_set_type_codec =
  Jsont.enum ["collection", `Collection; "serial", `Serial]

(* Date_fuzzy *)

type date_fuzzy = Jsont.nat_string
let date_fuzzy_codec =
  Jsont.(with_default (nat_string_of_string "####-##-##") nat_string)

(* Localized values *)

let locv_codec ?default ?kind codec =
  let default = match default with
  | None -> Locv.(add empty Nlang.any (Jsont.default codec))
  | Some d -> d
  in
  let objc = Jsont.objc ?kind () in
  let anon = Jsont.anon objc codec in
  let codec = Jsont.obj objc in
  let decode o =
    let add_loc acc loc_str = match acc with
    | `Error _ as e -> e
    | `Ok acc ->
        match Nlang.Range.of_string ~basic:true loc_str with
        | `Error _ as e -> e
        | `Ok loc -> `Ok (Locv.add acc loc (Jsont.get_anon anon loc_str o))
    in
    List.fold_left add_loc (`Ok Locv.empty) (Jsont.anon_names anon o)
  in
  let encode lv =
    let add_loc anons loc v =
      Jsont.anonv anon (Nlang.Range.to_string loc) v :: anons
    in
    Jsont.new_obj codec (Locv.fold add_loc [] lv)
  in
  Jsont.view ~default (decode, encode) codec

let locv_str_codec =
  locv_codec ~kind:"localized_string" Jsont.nat_string

let locv_uri_codec =
  let default = Locv.any (Jsont.default uri_codec) in
  locv_codec ~default ~kind:"localized_uri" uri_codec

let msg_codec = Jsont.view Locv.Msg.(of_string, to_string) Jsont.string
let msg_map_codec locale =
  let objc = Jsont.objc ~kind:"localized_msg_map" () in
  let anon = Jsont.anon objc msg_codec in
  let codec = Jsont.obj objc in
  let decode o =
    let add_msg acc key =
      Locv.Msg.Map.add acc key (Jsont.get_anon anon key o)
    in
    `Ok (List.fold_left add_msg (Locv.Msg.Map.empty locale)
           (Jsont.anon_names anon o))
  in
  let encode map =
    let add_msg anons key msg =
      Jsont.anonv anon key msg :: anons
    in
    Jsont.new_obj codec (Locv.Msg.Map.fold add_msg [] map)
  in
  Jsont.view ~default:(Locv.Msg.Map.empty locale) (decode, encode) codec

let locv_msgs_codec =
  locv_codec ~kind:"localized_msgs" (msg_map_codec Nlang.any (* FIXME *))

module Ui_link = struct
  type t = Jsont.obj

  let objc = Jsont.objc ~kind:"Ui_link" ()
  let mem_text = Jsont.mem objc "text" Jsont.nat_string
  let mem_href = Jsont.mem objc "href" uri_codec
  let codec = Jsont.obj objc

  let v ~text ~href () =
    Jsont.(new_obj codec [ memv mem_text text; memv mem_href href; ])

  let text = Jsont.get mem_text
  let href = Jsont.get mem_href
end

module Ui_locale = struct
  type t = Jsont.obj

  let objc = Jsont.objc ~kind:"Ui_locale" ()
  let mem_locale = Jsont.mem objc "locale" lang_codec
  let mem_href = Jsont.mem objc "href" uri_codec
  let codec = Jsont.obj objc

  let v ~locale ~href () =
    Jsont.(new_obj codec [ memv mem_locale locale; memv mem_href href ])

  let locale = Jsont.get mem_locale
  let href = Jsont.get mem_href
end


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
