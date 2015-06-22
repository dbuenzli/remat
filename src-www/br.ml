(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open React

(* Global objects *)

let window = Dom_html.window
let document = Dom_html.document

(* Array utilities *)

let array_to_list a =
  let acc = ref [] in
  for i = a ## length - 1 downto 0 do acc := Js.Unsafe.get a i :: !acc done;
  !acc

let array_of_list l =
  let a = jsnew Js.array_empty () in
  let rec loop i = function
  | [] -> a
  | v :: vs -> Js.array_set a i v; loop (i + 1) vs
  in
  loop 0 l

(* Log and debug *)

module Log = struct

  (* Log level and output *)

  type level = Show | Error | Warning | Info | Debug

  let level = ref (Some Warning)
  let set_level l = level := l
  let level () = !level

  let should_log l = match level () with
  | None -> false | Some l' -> l <= l'

  (* Logging *)

  let console = Firebug.console
  let ppf = Format.str_formatter
  let err_count = ref 0
  let warn_count = ref 0

  let kmsg ?header k l fmt =
    let k log _ = log (Format.flush_str_formatter ()); k () in
    if not (should_log l) then Format.ikfprintf (k (fun _ -> ())) ppf fmt else
    let pp_msg ppf log fmt = match header with
    | None -> Format.kfprintf (k log) ppf ("@[" ^^ fmt ^^ "@]@.")
    | Some label ->
        Format.kfprintf (k log) ppf ("[%s] @[" ^^ fmt ^^ "@]@.") label
    in
    match l with
    | Show ->
        pp_msg ppf (fun s -> console ## log (Js.string s)) fmt
    | Error ->
        incr err_count;
        pp_msg ppf (fun s -> console ## error (Js.string s)) fmt
    | Warning ->
        incr warn_count;
        pp_msg ppf (fun s -> console ## warn (Js.string s)) fmt
    | Info ->
        pp_msg ppf (fun s -> console ## info (Js.string s)) fmt
    | Debug ->
        pp_msg ppf (fun s -> console ## debug (Js.string s)) fmt

  let msg ?header l fmt = kmsg ?header (fun () -> ()) l fmt
  let show ?header fmt = msg ?header Show fmt
  let err ?header fmt = msg ?header Error fmt
  let warn ?header fmt = msg ?header Warning fmt
  let info ?header fmt = msg ?header Info fmt
  let debug ?header fmt = msg ?header Debug fmt

  let set_formatter spec ppf =
    warn "Log.set_formatter has no effect on this platform"

  (* Log monitoring *)

  let err_count () = !err_count
  let warn_count () = !warn_count
end

module Debug = struct
  external enter : unit -> unit = "debugger"
  let pp_v ppf v =
    let s = (Js.Unsafe.coerce v) ## toString () in
    Format.pp_print_string ppf (Js.to_string s)

  let log v = Firebug.console ## debug (v)
end

(* Browser strings *)

type str = Js.js_string Js.t

let str = Js.string
let fstr fmt =
  let k _ = str (Format.flush_str_formatter ()) in
  Format.kfprintf k Format.str_formatter fmt

module Str = struct

  let empty = str ""
  let is_empty s = s ## length = 0

  let app s0 s1 = s0 ## concat (s1)
  let split ~sep s = array_to_list (Js.str_array (s ## split (sep)))
  let concat ~sep ss = (array_of_list ss) ## join (sep)
  let slice ?(start = 0) ?stop s = match stop with
  | None -> s ## slice_end (start)
  | Some stop -> s ## slice (start, stop)

  let trim s = (Js.Unsafe.(coerce s) ## trim ())
  let chop ~prefix s =
    let i = s ## indexOf (prefix) in
    if i <> 0 then None else
    Some (s ## substring_toEnd (prefix ## length))

  let rchop ~suffix s =
    let i = s ## indexOf (suffix) in
    let suff_start = s ## length - suffix ## length in
    if i <> suff_start then None else
    Some (s ## substring (0, suff_start))

  let to_string = Js.to_string
  let pp ppf s = Format.pp_print_string ppf (Js.to_string s)
end

(* Browser location *)

module Loc = struct

  (* Location URI *)

  let uri () = window ## location ## href
  let scheme () =
    let p = window ## location ## protocol in
    if p ## length <> 0 then p ## slice (0, -1) (* remove last ':' *) else p

  let host () = window ## location ## hostname
  let port () =
    let p = window ## location ## port in
    if p ## length = 0 then None else
    let p = Str.to_string p in
    try Some (int_of_string p) with
    | Failure _ -> Log.err "port not an integer (`%s')" p; None

  let query () =
    let q = window ## location ## search in
    if q ## length = 0 then q else
    q ## slice_end (1) (* remove '?' *)

  let path () = window ## location ## pathname

  let fragment () =
    let f = window ## location ## hash in
    if f ## length = 0 then f else
    f ## slice_end (1) (* remove '#' *)

  let set_fragment frag = window ## location ## hash <- frag

  let update ?scheme ?host ?port ?path ?query ?fragment () =
    let l = window ## location in
    (match scheme with None -> () | Some s -> l ## protocol <- s);
    (match host with None -> () | Some h -> l ## hostname <- h);
    (match port with
     | None -> ()
     | Some p ->
         match p with
         | None -> ()
         | Some p -> l ## port <- (Js.string (string_of_int p)));
    (match path with None -> () | Some p -> l ## pathname <- p);
    (match query with None -> () | Some q -> l ## search <- q);
    (match fragment with None -> () | Some f -> l ## hash <- f);
    ()

  (* Location changes *)

  let set ?(replace = false) uri =
    if replace then window ## location ## replace (uri) else
    window ## location ## assign (uri)

  let reload () = window ## location ## reload ()
end

module History = struct

  let history = window ## history

  (* Moving in history *)

  let length () = history ## length
  let go delta = history ## go (Js.some delta)
  let back () = history ## back ()
  let forward () = history ## forward ()

  (* History state *)

  type 'a state = str * 'a
  let create_state ~version s = (version, s)
  let state ~version ~default () =
    match Js.Opt.to_option (Obj.magic (history ## state)) with
    | None -> default
    | Some (version', s) -> if version <> version' then default else s

  (* Making history *)

  let push ?(replace = false) ?state ~title uri =
    if replace
    then history ## replaceState (Js.Opt.option state, title, Js.some uri)
    else history ## pushState (Js.Opt.option state, title, Js.some uri)
end

module Info = struct
  let languages () =
    let langs = (Js.Unsafe.coerce (window ## navigator)) ## languages in
    match Js.Optdef.to_option langs with
    | Some l -> array_to_list l
    | None ->
        match Js.Optdef.to_option (window ## navigator ## language) with
        | None -> [str "en"]
        | Some l -> [l]
end

(* Monotonic time *)

module Time = struct

  (* Time span *)

  type span = float

  (* Passing time *)

  let warn_time () = Log.warn "performance.now () missing, using Date.now ()"

  let tick_now =
    let date_now () = (jsnew Js.date_now () ## getTime ()) /. 1000. in
    let perf_now () =
      (Js.Unsafe.coerce Dom_html.window) ## performance ## now () /. 1000.
    in
    let perf = (Js.Unsafe.coerce Dom_html.window) ## performance in
    match Js.Optdef.to_option perf with
    | None -> warn_time (); date_now
    | Some p ->
        match (Js.Unsafe.coerce p) ## now with
        | None -> warn_time (); date_now
        | Some n -> perf_now

  let start = tick_now ()
  let elapsed () = tick_now () -. start

  (* Counting time *)

  type counter = span
  let counter () = tick_now ()
  let counter_value c = tick_now () -. c

  (* Pretty printing time *)

  let pp = Format.fprintf
  let pp_s ppf s = pp ppf "%gs" s
  let pp_ms ppf s = pp ppf "%gms" (s *. 1e3)
  let pp_mus ppf s = pp ppf "%gμs" (s *. 1e6)
end

(* Storage *)

module Store = struct

  type scope = [ `Session | `Persist ]

  let scope_store = function
  | `Session -> Js.Optdef.to_option (Dom_html.window ## sessionStorage)
  | `Persist -> Js.Optdef.to_option (Dom_html.window ## localStorage)

  let support scope = scope_store scope <> None

  type 'a key = Js.js_string Js.t

  let key_prefix = Js.string "k"
  let key =
    let id = ref (-1) in
    fun ?ns () ->
      id := !id + 1;
      let id = Js.string (string_of_int !id) in
      match ns with
      | None -> Str.app key_prefix id
      | Some ns -> Str.app (Str.app key_prefix ns) id

  let version = key ()

  let mem ?(scope = `Persist) k = match scope_store scope with
  | Some s -> Js.Opt.test (s ## getItem (k))
  | None -> false

  let add ?(scope = `Persist) k v = match scope_store scope with
  | Some s -> s ## setItem (k, Json.output v) | None -> ()

  let rem ?(scope = `Persist) k = match scope_store scope with
  | Some s -> s ## removeItem (k) | None -> ()

  let find ?(scope = `Persist) k = match scope_store scope with
  | None -> None
  | Some s ->
      begin match Js.Opt.to_option (s ## getItem (k)) with
      | None -> None
      | Some vs -> Some (Json.unsafe_input vs)
      end

  let get ?(scope = `Persist) ?absent k = match scope_store scope with
  | None -> invalid_arg "store unsupported"
  | Some s ->
      begin match Js.Opt.to_option (s ## getItem (k)) with
      | None ->
          begin match absent with
          | None -> invalid_arg "key unbound"
          | Some v -> v
          end
      | Some vs -> Json.unsafe_input vs
      end

  let force_version ?(scope = `Persist) v = match scope_store scope with
  | None -> ()
  | Some s ->
      match find ~scope version with
      | None -> add ~scope version v
      | Some sv -> if v <> sv then (s ## clear (); add ~scope version v)

  let clear ?(scope = `Persist) () = match scope_store scope with
  | Some s -> s ## clear () | None -> ()
end

(* DOM *)

type el = Dom_html.element Js.t

module El = struct

  (* Elements *)

  type name = Js.js_string Js.t
  type child = [ `El of el | `Txt of str ]
  type t = el

  let rec add_children : el -> child list -> unit = fun n children ->
    match children with
    | [] ->  ()
    | `Txt txt :: cs ->
        let txt = Dom_html.document ## createTextNode (txt) in
        ignore (n ## appendChild ((txt :> Dom.node Js.t))); add_children n cs
    | `El e :: cs ->
        ignore (n ## appendChild ((e :> Dom.node Js.t))); add_children n cs

  let a_title = Js.string "title"
  let v ?id ?title ?(classes = []) ?(atts = []) name children =
    let e = Dom_html.document ## createElement (name) in
    List.iter (fun c -> e ## classList ## add (c)) classes;
    begin match id with
    | None -> () | Some id -> e ## id <- id
    end;
    begin match title with
    | None -> () | Some t -> e ## setAttribute (a_title, t)
    end;
    List.iter (fun (att, attv) -> e ## setAttribute(att, attv)) atts;
    add_children e children;
    e

  let of_id id = Js.Opt.to_option (Dom_html.document ## getElementById (id))

  (* Attributes *)

  let att e a = Js.Opt.to_option (e ## getAttribute (a))
  let set_att e a v = e ## setAttribute (a, v)
  let rem_att e a = e ## removeAttribute (a)

  (* Classes *)

  let has_class e a = Js.to_bool (e ## classList ## contains (a))
  let classify e c is_c =
    if is_c then e ## classList ## add (c) else
    e ## classList ## remove (c)

  let class_list e = array_to_list (e ## classList)
  let set_class_list el cl =
    let c = Str.concat ~sep:(str " ") cl in
    el ## className <- c

  (* Children *)

  let rec rem_children n = match Js.Opt.to_option (n ## firstChild) with
  | None -> ()
  | Some c -> ignore (n ## removeChild (c))

  let set_children n children = rem_children n; add_children n children

  (* Finalizers

     N.B. we tried to used the capturing phase to automatically trigger
     the event on the element's children but it doesn't seem to work. Hence
     we use a class to find out the nodes we need to finalize. *)

  let finalize_class = Js.string "ocaml_br_finalize"
  let finalize_class_select = Js.string ".ocaml_br_finalize"
  let finalize_ev_string = "ocaml_br_finalizer"
  let finalize_ev = Js.string finalize_ev_string
  let finalize_ev_type = Js.string "HTMLEvents"
  let finalize_ev = Dom.Event.make "ocaml_br_finalizer"

  let add_finalizer e f =            (* N.B. set handler on capturing phase. *)
    let f _ e = Dom_html.stopPropagation e; f (); Js._false in
    let f = Dom.full_handler f in
    ignore (Dom.addEventListener e finalize_ev f Js._false);
    ignore (e ## classList ## add (finalize_class));
    ()

  let finalize e =
    let dispatch n =
      let ev = (Js.Unsafe.coerce document) ## createEvent (finalize_ev_type) in
      ignore ((Js.Unsafe.coerce ev) ##
                initEvent (finalize_ev, Js._true, Js._true));
      ignore ((Js.Unsafe.coerce n) ## dispatchEvent(ev));
    in
    let matches = e ## querySelectorAll(finalize_class_select) in
    dispatch e;
    for i = 0 to matches ## length - 1 do
      let n = Js.Opt.get (matches ## item (i)) (fun () -> assert false) in
      dispatch n;
    done;
    ()

  (* Element names *)

  let name s = s
  let a = Js.string "a"
  let abbr = Js.string "abbr"
  let address = Js.string "address"
  let area = Js.string "area"
  let article = Js.string "article"
  let aside = Js.string "aside"
  let audio = Js.string "audio"
  let b = Js.string "b"
  let base = Js.string "base"
  let bdi = Js.string "bdi"
  let bdo = Js.string "bdo"
  let blockquote = Js.string "blockquote"
  let body = Js.string "body"
  let br = Js.string "br"
  let button = Js.string "button"
  let canvas = Js.string "canvas"
  let caption = Js.string "caption"
  let cite = Js.string "cite"
  let code = Js.string "code"
  let col = Js.string "col"
  let colgroup = Js.string "colgroup"
  let command = Js.string "command"
  let datalist = Js.string "datalist"
  let dd = Js.string "dd"
  let del = Js.string "del"
  let details = Js.string "details"
  let dfn = Js.string "dfn"
  let div = Js.string "div"
  let dl = Js.string "dl"
  let dt = Js.string "dt"
  let em = Js.string "em"
  let embed = Js.string "embed"
  let fieldset = Js.string "fieldset"
  let figcaption = Js.string "figcaption"
  let figure = Js.string "figure"
  let footer = Js.string "footer"
  let form = Js.string "form"
  let h1 = Js.string "h1"
  let h2 = Js.string "h2"
  let h3 = Js.string "h3"
  let h4 = Js.string "h4"
  let h5 = Js.string "h5"
  let h6 = Js.string "h6"
  let head = Js.string "head"
  let header = Js.string "header"
  let hgroup = Js.string "hgroup"
  let hr = Js.string "hr"
  let html = Js.string "html"
  let i = Js.string "i"
  let iframe = Js.string "iframe"
  let img = Js.string "img"
  let input = Js.string "input"
  let ins = Js.string "ins"
  let kbd = Js.string "kbd"
  let keygen = Js.string "keygen"
  let label = Js.string "label"
  let legend = Js.string "legend"
  let li = Js.string "li"
  let link = Js.string "link"
  let map = Js.string "map"
  let mark = Js.string "mark"
  let menu = Js.string "menu"
  let meta = Js.string "meta"
  let meter = Js.string "meter"
  let nav = Js.string "nav"
  let noscript = Js.string "noscript"
  let object_ = Js.string "object"
  let ol = Js.string "ol"
  let optgroup = Js.string "optgroup"
  let option = Js.string "option"
  let output = Js.string "output"
  let p = Js.string "p"
  let param = Js.string "param"
  let pre = Js.string "pre"
  let progress = Js.string "progress"
  let q = Js.string "q"
  let rp = Js.string "rp"
  let rt = Js.string "rt"
  let ruby = Js.string "ruby"
  let s = Js.string "s"
  let samp = Js.string "samp"
  let script = Js.string "script"
  let section = Js.string "section"
  let select = Js.string "select"
  let small = Js.string "small"
  let source = Js.string "source"
  let span = Js.string "span"
  let strong = Js.string "strong"
  let style = Js.string "style"
  let sub = Js.string "sub"
  let summary = Js.string "summary"
  let sup = Js.string "sup"
  let table = Js.string "table"
  let tbody = Js.string "tbody"
  let td = Js.string "td"
  let textarea = Js.string "textarea"
  let tfoot = Js.string "tfoot"
  let th = Js.string "th"
  let thead = Js.string "thead"
  let time = Js.string "time"
  let title = Js.string "title"
  let tr = Js.string "tr"
  let track = Js.string "track"
  let u = Js.string "u"
  let ul = Js.string "ul"
  let var = Js.string "var"
  let video = Js.string "video"
  let wbr = Js.string "wbr"
end

let el = El.v

(* Attribute names *)

module Att = struct
  let height = Js.string "height"
  let href = Js.string "href"
  let id = Js.string "id"
  let name = Js.string "name"
  let placeholder = Js.string "placeholder"
  let src = Js.string "src"
  let tabindex = Js.string "tabindex"
  let target = Js.string "target"
  let title = Js.string "title"
  let type_ = Js.string "type"
  let width = Js.string "width"
end

module Ev = struct

  (* Events and event kinds *)

  type 'a target = (#Dom_html.eventTarget as 'a) Js.t
  type 'a kind = (#Dom_html.event as 'a) Js.t Dom_html.Event.typ
  type 'a t = (#Dom_html.event as 'a) Js.t

  (* Event callbacks *)

  type cb = Dom.event_listener_id
  type cb_ret = bool Js.t

  let add_cb ?(capture = false) e k cb =
    Dom.addEventListener e k (Dom.full_handler cb) (Js.bool capture)

  let rem_cb cb = Dom.removeEventListener cb

  let propagate ?default e propagate =
    let default = match default with None -> propagate | Some v -> v in
    if not default then Dom.preventDefault e;
    if not propagate then Dom_html.stopPropagation e;
    Js.bool propagate

  (* Event kinds *)

  let kind = Dom.Event.make

  (* source: http://www.w3.org/TR/html5/index.html#events-0 *)

  let abort = Dom.Event.make "abort"
  let afterprint = Dom.Event.make "afterprint"
  let beforeprint = Dom.Event.make "beforeprint"
  let beforeunload = Dom.Event.make "beforeunload"
  let blur = Dom.Event.make "blur"
  let change = Dom.Event.make "change"
  let click = Dom.Event.make "click"
  let domContentLoaded = Dom.Event.make "DOMContentLoaded"
  let error = Dom.Event.make "error"
  let focus = Dom.Event.make "focus"
  let hashchange = Dom.Event.make "hashchange"
  let input = Dom.Event.make "input"
  let invalid = Dom.Event.make "invalid"
  let load = Dom.Event.make "load"
  let message = Dom.Event.make "message"
  let offline = Dom.Event.make "offline"
  let online = Dom.Event.make "online"
  let pagehide = Dom.Event.make "pagehide"
  let pageshow = Dom.Event.make "pageshow"
  let popstate = Dom.Event.make "popstate"
  let readystatechange = Dom.Event.make "readystatechange"
  let reset = Dom.Event.make "reset"
  let submit = Dom.Event.make "submit"
  let unload = Dom.Event.make "unload"
end




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
