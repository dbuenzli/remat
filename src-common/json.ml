(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Error strings *)

let str = Printf.sprintf
let err_eoi = "unexpected end of input"
let err_exp_eoi = "expected end of input"
let err_null = "expected a null"
let err_bool = "expected a boolean"
let err_float = "expected a number"
let err_string = "expected a string"
let err_array = "expected an array"
let err_array_empty = "expected a non empty array"
let err_mem_miss m = str "missing required member: `%s'" m
let err_mem m = str "unexpected member: `%s'" m
let err_mem_dup m = str "duplicate member: `%s'" m (* should be a warning *)
let err_obj_empty = "expected an non empty object"
let err_mem_multi = "an object member has been defined more than once"
let err_obj = function
| None -> "expected an object"
| Some k -> Printf.sprintf "expected a %s object" k

(* Decode *)

type error = [ Jsonm.error | `Other of string ]
type loc = (int * int) * (int * int)
type 'a def = loc * 'a
type 'a result = [ `Ok of 'a | `Error of error def ]

type decoder =
  { d : Jsonm.decoder;
    mutable peek : Jsonm.lexeme result option;
    mutable stack : loc list; }

type 'a t =
  { choose : decoder -> bool; (* returns [true] if parsing can proceed. *)
    parse : decoder -> 'a result; }

let decode d dec =
  let d = { d; peek = None; stack = [] } in
  dec.parse d

external ( & ) : ('a -> 'b) -> 'a -> 'b = "%apply"
external ( >> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

let loc_merge (s, _) (_, e) = (s, e)
let loc d = Jsonm.decoded_range d.d
let loc_push d = d.stack <- (loc d) :: d.stack
let loc_top d = match d.stack with [] -> assert false | l :: ls -> l
let loc_pop d = match d.stack with
| [] -> assert false
| l :: ls -> d.stack <- ls; l

let def d v = `Ok (loc d, v)
let err d msg = `Error (loc d, `Other msg)
let err_jsonm d e = `Error (loc d, (e :> error))

let lex_dec d = match Jsonm.decode d.d with
| `Lexeme l -> `Ok l
| `End -> err d err_eoi
| `Error e -> err_jsonm d e
| `Await -> assert false

let lex_peek d = match d.peek with
| Some l -> l
| None -> let l = lex_dec d in d.peek <- Some l; l

let lex d = match d.peek with
| Some l -> d.peek <- None; l
| None -> lex_dec d

let rec skip d =                     (* skips a ground value or a structure. *)
  let rec skip_struct ss se count =
    if count = 0 then `Ok () else
    match lex_dec d with
    | `Ok l when l = ss -> skip_struct ss se (count + 1)
    | `Ok l when l = se -> skip_struct ss se (count - 1)
    | `Ok _ -> skip_struct ss se count
    | `Error _ as e -> e
  in
  match lex d with
  | `Ok (`Null | `Bool _ | `Float _ | `String _ ) -> `Ok ()
  | `Ok `Os -> skip_struct `Os `Oe 1
  | `Ok `As -> skip_struct `As `Ae 1
  | `Ok _ -> assert false
  | `Error _ as e -> e

let pure v =
  let choose _ = true in
  let parse _ = `Ok v in
  { choose; parse }

let ( $ ) f v =
  let choose = f.choose in
  let parse d = match f.parse d with
  | `Ok f ->
      begin match v.parse d with
      | `Ok v -> `Ok (f v)
      | `Error _ as e -> e
      end
  | `Error _ as e -> e
  in
  { choose; parse }

let map f v =
  let choose = v.choose in
  let parse d = match v.parse d with
  | `Ok v -> `Ok (f v)
  | `Error _ as e -> e
  in
  { choose; parse }

let ret v =
  let choose = v.choose in
  let parse d = match v.parse d with
  | `Ok (`Ok _ as ok) -> ok
  | `Ok (`Error (loc, e)) -> `Error (loc, `Other e)
  | `Error _ as e -> e
  in
  { choose; parse }

(* N.B. The following could be better. w.r.t. to error report by having
   the error info the 'a t type instead of hiding it in the parse
   function. Here in case both fail we return the error of r.parse. *)

let choose l r =
  let choose d = l.choose d || r.choose d in
  let parse d = if l.choose d then l.parse d else r.parse d in
  { choose; parse }

let undef v =
  let choose = v.choose in
  let parse d = match v.parse d with
  | `Ok (_, v) -> `Ok v
  | `Error _ as e -> e
  in
  { choose; parse }

let eoi v =
  let choose _ = false (* you cannot choose eoi in JSON *) in
  let parse d = match v.parse d with
  | `Ok _ as r ->
      begin match Jsonm.decode d.d with
      | `End -> r
      | `Lexeme _ -> err d err_exp_eoi
      | `Error e -> err_jsonm d e
      | `Await -> assert false
      end
  | `Error _ as e -> e
  in
  { choose; parse }

let opt v =
  let choose d = match lex_peek d with `Ok `Null -> true | _ -> v.choose d in
  let parse d = match lex_peek d with
  | `Ok `Null -> ignore (lex d); def d None
  | `Ok _ ->
      begin match v.parse d with
      | `Error _ as e -> e
      | `Ok (loc, v) -> `Ok (loc, Some v)
      end
  | `Error _ as e -> e
  in
  { choose; parse }

(* Ground values *)

let null =
  let choose d = match lex_peek d with `Ok `Null -> true | _ -> false in
  let parse d = match lex d with
  | `Ok `Null -> def d ()
  | `Ok _ -> err d err_null
  | `Error _ as e -> e
  in
  { choose; parse }

let bool =
  let choose d = match lex_peek d with `Ok `Bool _ -> true | _ -> false in
  let parse d = match lex d with
  | `Ok `Bool b -> def d b
  | `Ok _ -> err d err_bool
  | `Error _ as e -> e
  in
  { choose; parse }

let float =
  let choose d = match lex_peek d with `Ok `Float _ -> true | _ -> false in
  let parse d = match lex d with
  | `Ok `Float f -> def d f
  | `Ok _ -> err d err_float
  | `Error _ as e -> e
  in
  { choose; parse }

let string =
  let choose d = match lex_peek d with `Ok `String _ -> true | _ -> false in
  let parse d = match lex d with
  | `Ok `String s -> def d s
  | `Ok _ -> err d err_string
  | `Error _ as e -> e
  in
  { choose; parse }

(* Arrays *)

let array ?(empty = true) f acc v =
  let choose d = match lex_peek d with `Ok `As -> true | _ -> false in
  let parse d = match lex d with
  | `Ok `As ->
      loc_push d;
      if not empty && lex_peek d = `Ok `Ae
      then `Error (loc_merge (loc_pop d) (loc d), `Other err_array_empty)
      else
      let rec loop acc = match lex_peek d with
      | `Ok `Ae -> ignore (lex d); `Ok (loc_merge (loc_pop d) (loc d), acc)
      | `Ok _ ->
          begin match v.parse d with
          | `Ok v -> loop (f acc v)
          | `Error _ as e -> e
          end
      | `Error _ as e -> e
      in
      loop acc
  | `Ok _ -> err d err_array
  | `Error _ as e -> e
  in
  { choose; parse }


module Smap = struct
  module Key = struct
    type t = string
    let compare : string -> string -> int = Pervasives.compare
  end
  module M = (Map.Make (Key) : Map.S with type key = Key.t)
  type 'a t = 'a M.t
  let empty = M.empty
  let find m k = try Some (M.find k m) with Not_found -> None
  let add m k v = M.add k v m
end

(* A universal type, as usual see http://mlton.org/PropertyList. *)
let u_thunk (type v) () =
  let module Store = struct exception V of v end in
  let set = fun v -> Store.V v in
  let get = function Store.V v -> v | _ -> invalid_arg err_mem_multi in
  set, get

type ustore = exn Smap.t        (* stores parsed fields in a universal type. *)

type 'a obj =
  { get : decoder -> ustore -> 'a result;
    mems : (decoder -> ustore -> ustore result) Smap.t }

let no_mem v = { get = (fun _ _ -> `Ok v) ; mems = Smap.empty }
let mem name v o =
  let v_set, v_get = u_thunk () in
  let get d ustore = match Smap.find ustore name with
  | None -> `Error (loc_merge (loc_top d) (loc d), `Other (err_mem_miss name))
  | Some v ->
      let v = v_get v in
      begin match o.get d ustore with
      | `Ok f -> `Ok (f v)
      | `Error _ as e -> e
      end
  in
  let parse d ustore =
    loc_push d;
    match Smap.find ustore name with
    | Some _ -> err d (err_mem_dup name)
    | None ->
        begin match v.parse d with
        | `Ok v ->
            let u = v_set (loc_merge (loc_pop d) (loc d), v) in
            `Ok (Smap.add ustore name u)
        | `Error _ as e -> e
        end
  in
  { get; mems = Smap.add o.mems name parse }

let mem_opt name v o =
  let v_set, v_get = u_thunk () in
  let get d ustore = match o.get d ustore with
  | `Ok f ->
      begin match Smap.find ustore name with
      | None -> `Ok (f (loc_merge (loc_top d) (loc d), None))
      | Some v ->
          let loc, v = v_get v in
          `Ok (f (loc, Some v))
      end
  | `Error _ as e -> e
  in
  let parse d ustore =
    loc_push d;
    match Smap.find ustore name with
    | Some _ -> err d (err_mem_dup name)
    | None ->
        begin match v.parse d with
        | `Ok v ->
            let u = v_set (loc_merge (loc_pop d) (loc d), v) in
            `Ok (Smap.add ustore name u)
        | `Error _ as e -> e
        end
  in
  { get; mems = Smap.add o.mems name parse }

let mem_skip name o =
  let get d ustore = o.get d ustore in
  let parse d ustore = match skip d with
  | `Ok () -> `Ok ustore
  | `Error _ as e -> e
  in
  { get; mems = Smap.add o.mems name parse }

let obj ?(strict = true) ?kind o =
  let choose d = match lex_peek d with `Ok `Os -> true | _ -> false in
  let parse d = match lex d with
  | `Ok `Os ->
      loc_push d;
      let rec loop ustore = match lex d with
      | `Ok `Oe ->
          begin match o.get d ustore with
          | `Ok v -> `Ok (loc_merge (loc_pop d) (loc d), v)
          | `Error _ as e -> e
          end
      | `Ok `Name n ->
          begin match (Smap.find o.mems n) with
          | None ->
              if strict then err d (err_mem n) else
              begin match skip d with
              | `Ok () -> loop ustore
              | `Error _ as e -> e
              end
          | Some parse ->
              begin match parse d ustore with
              | `Error _ as e -> e
              | `Ok ustore -> loop ustore
              end
          end
      | `Error _ as e -> e
      | `Ok _ -> assert false
      in
      loop Smap.empty
  | `Ok _ -> err d (err_obj kind)
  | `Error _ as e -> e
  in
  { choose; parse }

let obj_all ?kind f acc v o =
  let choose d = match lex_peek d with `Ok `Os -> true | _ -> false in
  let parse d = match lex d with
  | `Ok `Os ->
      loc_push d;
      let rec loop acc ustore = match lex d with
      | `Ok `Oe ->
          begin match o.get d ustore with
          | `Ok f -> `Ok (loc_merge (loc_pop d) (loc d), f acc)
          | `Error _ as e -> e
          end
      | `Ok `Name n ->
          begin match (Smap.find o.mems n) with
          | None ->
              let n = loc d, n in
              begin match v.parse d with
              | `Ok v -> loop (f acc n v) ustore
              | `Error _ as e -> e
              end
          | Some parse ->
              begin match parse d ustore with
              | `Error _ as e -> e
              | `Ok ustore -> loop acc ustore
              end
          end
      | `Error _ as e -> e
      | `Ok _ -> assert false
      in
      loop acc Smap.empty
  | `Ok _ -> err d (err_obj kind)
  | `Error _ as e -> e
  in
  { choose; parse }

(* JSON soup decoding *)

type soup =
  [ `Null | `Bool of bool | `Float of float | `String of string
  | `A of soup def list | `O of (string def * soup def) list ]

let soup =
  let choose d = true in
  let rec parse d =
    let loc_pop k = function
    | `Ok v -> k (`Ok (loc_merge (loc_pop d) (loc d), v))
    | `Error _ as e -> e
    in
    let rec value k = match lex d with
    | `Ok `Os -> loc_push d; obj [] (loc_pop k)
    | `Ok `As -> loc_push d; arr [] (loc_pop k)
    | `Ok (`Null | `Bool _ | `String _ | `Float _ as v) -> k (def d v)
    | `Error _ as e -> k e
    | `Ok _ -> assert false
    and arr vs k = match lex_peek d with
    | `Ok `Ae -> ignore (lex d); k (`Ok (`A (List.rev vs)))
    | `Ok _ ->  value (function `Ok v -> arr (v :: vs) k | `Error _ as e -> k e)
    | `Error _ as e -> k e
    and obj ms k = match lex_peek d with
    | `Ok `Oe -> ignore (lex d); k (`Ok (`O (List.rev ms)))
    | `Ok `Name n ->
        let n = loc d, n in
        value (function `Ok v -> obj ((n, v) :: ms) k | `Error _ as e -> k e)
    | `Error _ as e -> k e
    | `Ok _ -> assert false
    in
    value (fun v -> v)
  in
  { choose; parse }


(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli.
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
