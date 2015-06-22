(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Minimal browser interaction. *)

open React

(** {1 Browser strings} *)

type str = Js.js_string Js.t
(** The type for browser strings. *)

val str : string -> str
(** [str s] is [s] as a browser string. *)

val fstr : ('a, Format.formatter, unit, str) format4 -> 'a
(** [fstr fmt ...] is a formatted browser string. *)

(** Browser strings. *)
module Str : sig

  (** {1 Empty string} *)

  val empty : str
  (** [empty] is an empty string. *)

  val is_empty : str -> bool
  (** [is_empty s] is [true] if [s] is an empty string. *)

  (** {1 String operations} *)

  val app : str -> str -> str
  (** [app s0 s1] appends [s1] to [s0]. *)

  val split : sep:str -> str -> str list
  (** [split sep s] is the list of all (possibly empty) substrings
      of [s] that are delimited by matches of the non empty separator
      string [sep]. *)

  val concat : sep:str -> str list -> str
  (** [concat sep ss] is the concatenates the list of strings [ss] inserting
      [sep] between each of them. *)

  val slice : ?start:int -> ?stop:int -> str -> str
  (** [slice ~start ~stop s] is the string s.[start], s.[start+1], ...
      s.[stop - 1]. [start] defaults to [0] and [stop] to [String.length s].

      If [start] or [stop] are negative they are subtracted from
      [String.length s]. This means that [-1] denotes the last
      character of the string. *)

  val trim : str -> str
  (** [trim s] is [s] without whitespace from the beginning and end of the
      string. *)

  val chop : prefix:str -> str -> str option
  (** [chop prefix s] is [s] without the prefix [prefix] or [None] if
      [prefix] is not a prefix of [s]. *)

  val rchop : suffix:str -> str -> str option
  (** [rchop suffix s] is [s] without the suffix [suffix] or [None] if
      [suffix] is not a suffix of [s]. *)

  val to_string : str -> string
  (** [to_string s] is [s] as an OCaml string. *)

  val pp : Format.formatter -> str -> unit
  (** [pp ppf s] prints [s] on [ppf]. *)
end

(** {1 Location and history} *)

(** Browser location

    {b Warning.} We use the terminology and return data according to
    {{:http://tools.ietf.org/html/rfc3986}RFC 3986},
    not according to the broken HTML URLUtils interface.  *)
module Loc : sig

  (** {1:info Location URI} *)

  val uri : unit -> str
  (** [uri ()] is the browser's location full URI. *)

  val scheme : unit -> str
  (** [scheme ()] is the scheme of {!uri}[ ()]. *)

  val host : unit -> str
  (** [host ()] is the host of {!uri}[ ()]. *)

  val port : unit -> int option
  (** [port ()] is the port of {!uri}[ ()]. *)

  val path : unit -> str
  (** [path ()] is the path of {!uri}[ ()]. *)

  val query : unit -> str
  (** [query ()] is the query of {!uri}[ ()]. *)

  val fragment : unit -> str
  (** [fragment ()] is fragment of {!uri}[ ()]. *)

  val set_fragment : str -> unit
  (** [set_fragment frag] sets the fragment of {!uri}[ ()] to [frag].
      This does not reload the page but triggers the {!Ev.hashchange}
      event. *)

  val update : ?scheme:str -> ?host:str -> ?port:int option -> ?path:str ->
    ?query:str -> ?fragment:str -> unit -> unit
  (** [update ~scheme ~hostname ~port ~path ~query ~fragment ()] updates the
      corresponding parts of the location's URI. *)

  (** {1:info Location changes} *)

  val set : ?replace:bool -> str -> unit
  (** [set replace uri] sets to browser location to [uri], if
      [replace] is [true] the current location is removed from the
      session history (defaults to [false]). *)

  val reload : unit -> unit
  (** [reload ()] reloads the current location. *)
end

(** Browser history. *)
module History : sig

  (** {1 Moving in history} *)

  val length : unit -> int
  (** [length ()] is the number of elements in the history including
      the currently loaded page. *)

  val go : int -> unit
  (** [go step] goes [step] numbers forward (positive) or backward (negative)
      in history. *)

  val back : unit -> unit
  (** [back ()] is [go ~-1]. *)

  val forward : unit -> unit
  (** [forward ()] is [go 1]. *)

  (** {1 History state}

      {b Warning.} History state is unsafe if you don't properly version
      you state. Any change in state representation should entail a new
      version. *)

  type 'a state
  (** The type for state. *)

  val create_state : version:str -> 'a -> 'a state
  (** [create_state version v] is state [v] with version [version]. *)

  val state : version:str -> default:'a -> unit -> 'a
  (** [state version deafult ()] is the current state if it matches
      [version]. If it doesn't match or there is no state [default] is
      returned. *)

  (** {1 Making history} *)

  val push : ?replace:bool -> ?state:'a state -> title:str -> str -> unit
  (** [push ~replace ~state ~title uri] changes the browser location to
      [uri] but doesn't load the URI. [title] is a human title for the
      location to which we are moving and [state] is a possible value
      associated to the location. If [replace] is [true] (defaults to
      [false]) the current location is replaced rather than added to
      history. *)
end

(** Browser information *)
module Info : sig

  val languages : unit -> str list
  (** [languages ()] is the user's preferred languages as BCP 47 language
      tags.

      FIXME support languagechange event on window. *)
end

(** {1 Monotonic time} *)

(** Monotonic time. *)
module Time : sig

  (** {1 Time span} *)

  type span = float
  (** The type for time spans, in seconds. *)

  (** {1 Passing time} *)

  val elapsed : unit -> span
  (** [elapsed ()] is the number of seconds elapsed since the
      beginning of the program. *)

  (** {1 Counters} *)

  type counter
  (** The type for time counters. *)

  val counter : unit -> counter
  (** [counter ()] is a counter counting time from call time on. *)

  val counter_value : counter -> span
  (** [counter_value c] is the current counter value in seconds. *)

  (** {1 Pretty printing time} *)

  val pp_s : Format.formatter -> span -> unit
  (** [pp_s ppf s] prints [s] seconds in seconds. *)

  val pp_ms : Format.formatter -> span -> unit
  (** [pp_ms ppf s] prints [s] seconds in milliseconds. *)

  val pp_mus : Format.formatter -> span -> unit
  (** [pp_mus ppf s] prints [s] seconds in microseconds. *)
end

(** {1 Storage} *)

(** Persistent storage.

    Persisent key-value store implemented over
    {{:http://www.w3.org/TR/webstorage/}webstorage}. Safe if no one
    tampers with the storage outside of the program. *)
module Store : sig

  (** {1 Storage scope} *)

  type scope = [ `Session | `Persist ]
  (** The storage scope. *)

  val support : scope -> bool
  (** [support scope] is [true] iff values can be stored in [scope]. *)

  (** {1 Keys} *)

  type 'a key
  (** The type for keys whose lookup value is 'a *)

  val key : ?ns:str -> unit -> 'a key
  (** [key ~ns ()] is a new storage key in namespace [ns]. If [ns]
      is unspecified, the key lives in a global namespace.

      {b Warning.} Reordering invocations of {!key} in the same
      namespace will most of the time corrupt existing storage. This
      means that all {!key} calls should always be performed at
      initialization time. {!Store.force_version} can be used to
      easily version your store and aleviate this problem. *)

  (** {1 Storage}

      In the functions below [scope] defaults to [`Persist]. *)

  val mem : ?scope:scope -> 'a key -> bool
  (** [mem k] is [true] iff [k] has a mapping. *)

  val add : ?scope:scope -> 'a key -> 'a -> unit
  (** [add k v] maps [k] to [v]. *)

  val rem : ?scope:scope -> 'a key -> unit
  (** [rem k] unbinds [k]. *)

  val find : ?scope:scope -> 'a key -> 'a option
  (** [find k] is [k]'s mapping in [m], if any. *)

  val get : ?scope:scope -> ?absent:'a -> 'a key -> 'a
  (** [get k] is [k]'s mapping. If [absent] is provided and [m] has
      not binding for [k], [absent] is returned.

      @raise Invalid_argument if [k] is not bound and [absent]
      is unspecified or if [scope] is not {!support}ed. *)

  val clear : ?scope:scope -> unit -> unit
  (** [clear ()], clears all mapping. *)

  (** {1 Versioning} *)

  val force_version : ?scope:scope -> string -> unit
  (** [force_version v] checks that the version of the store is [v].  If
      it's not it {!clear}s the store and sets the version to [v]. *)
end

(** {1 DOM} *)

type el = Dom_html.element Js.t
(** The type for elements. *)

(** DOM elements *)
module El : sig

  (** {1:elements Elements} *)

  type name
  (** The type for element names, see {!element_names}. *)

  type child = [ `El of el | `Txt of str ]
  (** The type for element childs. *)

  type t = el
  (** The type for elements. *)

  val v : ?id:str -> ?title:str ->
    ?classes:str list -> ?atts:(str * str) list -> name -> child list -> el
  (** [v id title classes atts name children] is a DOM element [name]
      with id [id], title [title], classes [classes], attribute [atts],
      and children [children]. *)

  val of_id : str -> el option
  (** [of_id id] looks for and element with id [id] in the document. *)

  (** {1 Attributes} *)

  val att : el -> str -> str option
  (** [att e a] is the value of attribute [a] of [e] (if any). *)

  val set_att : el -> str -> str -> unit
  (** [set_att e a v] sets the value of attribute [a] of [e] to [v]. *)

  val rem_att : el -> str -> unit
  (** [rem_att e a] remove the attribute [a] from [e]. *)

  (** {1 Classes} *)

  val has_class : el -> str -> bool
  (** [has_class e class] is [true] iff [e] has class [class]. *)

  val classify : el -> str -> bool -> unit
  (** [classify e class pred] classifies [e] as being a member of
      class [class] according to [pred]. *)

  val class_list : el -> str list
  (** [class_list e] is the list of classes of [e]. *)

  val set_class_list : el -> str list -> unit
  (** [set_class_list cl] sets the classes of [e] to [cl]. *)

  (** {1 Children} *)

  val set_children : el -> child list -> unit
  (** [set_children e children] sets [e]'s children to [children] *)

  (** {1 Finalizers} *)

  val add_finalizer : el -> (unit -> unit) -> unit
  (** [finalizer e f] sets [f] as a finalisation function for [e]. *)

  val finalize : el -> unit
  (** [finalize el] finalizes [el] and its descendents elements. *)

  (** {1:element_names Element names}

      See {{:http://www.w3.org/TR/html5/index.html#element-interfaces}spec}. *)

  val name : str -> name
  (** [name s] is an element name [s]. *)
  val a : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/a}a} *)
  val abbr : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/abbr}abbr} *)
  val address : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/address}address} *)
  val area : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/area}area} *)
  val article : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/article}article} *)
  val aside : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/aside}aside} *)
  val audio : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/audio}audio} *)
  val b : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/b}b} *)
  val base : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/base}base} *)
  val bdi : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/bdi}bdi} *)
  val bdo : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/bdo}bdo} *)
  val blockquote : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/blockquote}blockquote}
      *)
  val body : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/body}body} *)
  val br : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/br}br} *)
  val button : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/button}button} *)
  val canvas : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/canvas}canvas} *)
  val caption : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/caption}caption} *)
  val cite : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/cite}cite} *)
  val code : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/code}code} *)
  val col : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/col}col} *)
  val colgroup : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/colgroup}colgroup} *)
  val command : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/command}command} *)
  val datalist : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/datalist}datalist} *)
  val dd : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/dd}dd} *)
  val del : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/del}del} *)
  val details : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/details}details} *)
  val dfn : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/dfn}dfn} *)
  val div : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/div}div} *)
  val dl : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/dl}dl} *)
  val dt : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/dt}dt} *)
  val em : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/em}em} *)
  val embed : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/embed}embed} *)
  val fieldset : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/fieldset}fieldset} *)
  val figcaption : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/figcaption}figcaption}
  *)
  val figure : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/figure}figure} *)
  val footer : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/footer}footer} *)
  val form : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/form}form} *)
  val h1 : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/h1}h1} *)
  val h2 : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/h2}h2} *)
  val h3 : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/h3}h3} *)
  val h4 : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/h4}h4} *)
  val h5 : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/h5}h5} *)
  val h6 : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/h6}h6} *)
  val head : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/head}head} *)
  val header : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/header}header} *)
  val hgroup : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/hgroup}hgroup} *)
  val hr : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/hr}hr} *)
  val html : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/html}html} *)
  val i : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/i}i} *)
  val iframe : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/iframe}iframe} *)
  val img : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/img}img} *)
  val input : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/input}input} *)
  val ins : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/ins}ins} *)
  val kbd : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/kbd}kbd} *)
  val keygen : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/keygen}keygen} *)
  val label : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/label}label} *)
  val legend : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/legend}legend} *)
  val li : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/li}li} *)
  val link : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/link}link} *)
  val map : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/map}map} *)
  val mark : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/mark}mark} *)
  val menu : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/menu}menu} *)
  val meta : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/meta}meta} *)
  val meter : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/meter}meter} *)
  val nav : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/nav}nav} *)
  val noscript : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/noscript}noscript} *)
  val object_ : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/object}object} *)
  val ol : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/ol}ol} *)
  val optgroup : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/optgroup}optgroup} *)
  val option : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/option}option} *)
  val output : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/output}output} *)
  val p : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/p}p} *)
  val param : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/param}param} *)
  val pre : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/pre}pre} *)
  val progress : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/progress}progress} *)
  val q : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/q}q} *)
  val rp : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/rp}rp} *)
  val rt : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/rt}rt} *)
  val ruby : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/ruby}ruby} *)
  val s : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/s}s} *)
  val samp : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/samp}samp} *)
  val script : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/script}script} *)
  val section : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/section}section} *)
  val select : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/select}select} *)
  val small : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/small}small} *)
  val source : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/source}source} *)
  val span : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/span}span} *)
  val strong : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/strong}strong} *)
  val style : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/style}style} *)
  val sub : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/sub}sub} *)
  val summary : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/summary}summary} *)
  val sup : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/sup}sup} *)
  val table : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/table}table} *)
  val tbody : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/tbody}tbody} *)
  val td : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/td}td} *)
  val textarea : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/textarea}textarea} *)
  val tfoot : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/tfoot}tfoot} *)
  val th : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/th}th} *)
  val thead : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/thead}thead} *)
  val time : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/time}time} *)
  val title : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/title}title} *)
  val tr : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/tr}tr} *)
  val track : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/track}track} *)
  val u : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/u}u} *)
  val ul : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/ul}ul} *)
  val var : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/var}var} *)
  val video : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/video}video} *)
  val wbr : name
  (** {{:https://docs.webplatform.org/wiki/html/elements/wbr}wbr} *)
end

val el : ?id:str -> ?title:str ->
  ?classes:str list -> ?atts:(str * str) list -> El.name -> El.child list -> el



(** DOM element attribute names. *)
module Att : sig

  (** {1 Attribute names} *)

  val height : str
  val href : str
  val id : str
  val name : str
  val placeholder : str
  val src : str
  val tabindex : str
  val target : str
  val title : str
  val type_ : str
  val width : str


end

(** DOM events *)
module Ev : sig

  (** {1:events Events and event kinds} *)

  type 'a target = (#Dom_html.eventTarget as 'a) Js.t
  (** The type for event targets. *)

  type 'a kind = (#Dom_html.event as 'a) Js.t Dom_html.Event.typ
  (** The type for kind of events. See {!kinds}. *)

  type 'a t = (#Dom_html.event as 'a) Js.t
  (** The type for events. *)

  (** {1:cb Event callbacks} *)

  type cb
  (** The type for event callbacks. *)

  type cb_ret
  (** The type for callback return. *)

  val add_cb : ?capture:bool -> 'a target -> 'b kind ->
    ('a target -> 'b t -> cb_ret) -> cb
  (** [add_cb capture e k f] is calls [f e ev] whenever an event [ev] of
      kind [k] occurs on [e]. If [capture] is [true] the callback occurs
      during the capture phase (defaults to [false]). *)

  val rem_cb : cb -> unit
  (** [rem_cb cb] removes the callback [cb]. *)

  val propagate : ?default:bool -> 'a t -> bool -> cb_ret
  (** [propogate default e propagate] propagate event [e] if it is
      [propagate] is [true]. The default action is performed iff [default]
      is [true] (defaults to [false] if [propagate] is [false] and [true]
      if [propagate] is [true]). *)

  (** {1:kinds Event kinds}

      See {{:http://www.w3.org/TR/html5/index.html#events-0}spec}. *)

  val kind : string -> 'a kind
  val abort : Dom_html.event kind
  val afterprint : Dom_html.event kind
  val beforeprint : Dom_html.event kind
  val beforeunload : Dom_html.event kind (* FIXME make more precise *)
  val blur : Dom_html.event kind
  val change : Dom_html.event kind
  val click : Dom_html.event kind
  val domContentLoaded : Dom_html.event kind
  val error : Dom_html.event kind
  val focus : Dom_html.event kind
  val hashchange : Dom_html.event kind (* FIXME make more precise *)
  val input : Dom_html.event kind
  val invalid : Dom_html.event kind
  val load : Dom_html.event kind
  val message : Dom_html.event kind (* FIXME make more precise *)
  val offline : Dom_html.event kind
  val online : Dom_html.event kind
  val pagehide : Dom_html.event kind (* FIXME make more precise *)
  val pageshow : Dom_html.event kind (* FIXME make more precise *)
  val popstate : Dom_html.event kind (* FIXME make more precise *)
  val readystatechange : Dom_html.event kind
  val reset : Dom_html.event kind
  val submit : Dom_html.event kind
  val unload : Dom_html.event kind
end

val window : Dom_html.window Js.t
(** [window] is {!Dom_html.window}. *)

val document : Dom_html.document Js.t
(** [document] is {!Dom_html.document}. *)

(** {1 Log and debug} *)

(** Browser log.

    {b TODO} Adjust documentation, assert sensitiveness, recheck
    with command line Log module. *)
module Log : sig
  (** {1 Log level} *)

  (** The type for log levels. *)
  type level = Show | Error | Warning | Info | Debug

  val msg : ?header:string -> level ->
    ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [msg header l fmt ...] logs a message with level [l]. [header] is
      the message header, default depends on [l]. *)

  val kmsg : ?header:string ->
    (unit -> 'a) -> level -> ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** [kmsg header k l fmt ...] is like [msg header l fmt] but calls [k ()]
      before returning. *)

  val show : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [show fmt ...] logs a message with level [Show]. [header] defaults
      to [None]. *)

  val err : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [err fmt ...] logs a message with level [Error]. [header] defaults
      to ["ERROR"]. *)

  val warn : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [warn fmt ...] logs a message with level [Warning]. [header] defaults
      to ["WARNING"]. *)

  val info : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [info fmt ...] logs a message with level [Info]. [header] defaults
      to ["INFO"]. *)

  val debug : ?header:string -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [debug info ...] logs a message with level [Debug]. [header] defaults
      to ["DEBUG"]. *)

  (** {1 Log level and output} *)

  val level : unit -> level option
  (** [level ()] is the log level (if any). If the log level is [(Some l)]
      any message whose level is [<= l] is logged. If level is [None]
      no message is ever logged. Initially the level is [(Some Warning)]. *)

  val set_level : level option -> unit
  (** [set_level l] sets the log level to [l]. See {!level}. *)

  val set_formatter : [`All | `Level of level ] -> Format.formatter -> unit
  (** [set_formatter spec ppf] sets the formatter for a given level or
      for all the levels according to [spec]. Initially the formatter
      of level [Show] is {!Format.std_formatter} and all the other level
      formatters are {!Format.err_formatter}. *)

  (** {1 Log monitoring} *)

  val err_count : unit -> int
  (** [err_count ()] is the number of messages logged with level [Error]. *)

  val warn_count : unit -> int
  (** [warn_count ()] is the number of messages logged with level
      [Warning]. *)
end

(** Debugging tools. *)
module Debug : sig

  (** {1 Debug} *)

  val enter : unit -> unit
  (** [enter ()] stops and enters the JavaScript debugger (if available). *)

  val pp_v : Format.formatter -> < .. > Js.t -> unit
  (** [pp_v ppf v] applies the method [toString] to [v] and prints the
      the resulting string on [ppf]. *)

  val log : < .. > Js.t -> unit
  (** [log v] logs [v] on the browser console with level debug. *)
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
