(*
Copyright (c) 2010, Mickaël Delahaye <mickael.delahaye@gmail.com>

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
*)

(** Clerk: a Git-like configuration file loader for OCaml *)


(** Generic parameter *)
type 'a parameter

(** Get a parameter's current value. Raises {!Not_found} if no value is assigned to this parameter. *)
val get : 'a parameter -> 'a
(** Set a parameter value *)
val set : 'a parameter -> 'a -> unit


(** Configuration table type*)
type t
type table = t

(** Parser module *)
module Parser :
  sig
  
    exception ParsingError of string

    val parse_stream :
      (string -> string option -> unit) -> char Stream.t -> unit
    val parse_channel :
      (string -> string option -> unit) -> in_channel -> unit
  end

(** Make a configuration table. Optional hint size. *)
val make : ?size:int -> unit -> t

(** Extracting parameters *)

val mk_bool : t -> ?default:bool -> string -> bool parameter
val mk_string : t -> ?default:string -> string -> string parameter
val mk_int : t -> ?default:int -> string -> int parameter
val mk_int64 : t -> ?default:int64 -> string -> int64 parameter
val mk_float : t -> ?default:float -> string -> float parameter

(** Loading *)

(** Load configurations from an input channel into a configuration table *)
val load_channel : t -> in_channel -> unit
(** Load configurations from an file into a configuration table *)
val load_file : t -> string -> unit
(** Load configurations form a character stream into a configuration table *)
val load : t -> char Stream.t -> unit

(** Printing and storing *)

val print : Format.formatter -> t -> unit

val store_channel : t -> out_channel -> unit
val store : t -> string -> unit
