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

(** Loading *)

(** Load configurations from an input channel into a configuration table *)
val load_channel : t -> in_channel -> unit
(** Load configurations from an file into a configuration table *)
val load_file : t -> string -> unit
(** Load configurations form a character stream into a configuration table *)
val load : t -> char Stream.t -> unit

(** Printing and storing *)

val print : ?default:bool -> Format.formatter -> t -> unit

val store_channel : t -> out_channel -> unit
val store_file : t -> string -> unit

module type PARAM = sig
  type t
  val get : unit -> t
  val set : t -> unit
end

module type CONFIG = sig

  module String (S : sig
    val name : string
    val default : string option
  end) : PARAM with type t = string

  module Int (S : sig
    val name : string
    val default : int option
  end) : PARAM with type t = int

  module Int64 (S : sig
    val name : string
    val default : int64 option
  end) : PARAM with type t = int64

  module Float (S : sig
    val name : string
    val default : float option
  end) : PARAM with type t = float

  module Bool (S : sig
    val name : string
    val default : bool option
  end) : PARAM with type t = bool

end

module Make (S : sig
  val table : t
end) : CONFIG
