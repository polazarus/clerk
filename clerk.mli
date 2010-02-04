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

(** Parser module *)
module Parser :
  sig
    val parse_stream :
      (string -> string option -> unit) -> char Stream.t -> unit
    val parse_channel :
      (string -> string option -> unit) -> in_channel -> unit
  end

(** Table module *)
module Table :
  sig
    type t

    val make : ?size:int -> unit -> t

    val get_bool : t -> string -> bool
    val get_string : t -> string -> string
    val get_int : t -> string -> int
    val get_int64 : t -> string -> int64
    val get_float : t -> string -> float

    val get_bool_default : t -> string -> bool -> bool
    val get_string_default : t -> string -> string -> string
    val get_int_default : t -> string -> int -> int
    val get_int64_default : t -> string -> int64 -> int64
    val get_float_default : t -> string -> float -> float
    
    val set_bool : t -> string -> bool -> unit
    val set_string : t -> string -> string -> unit
    val set_int : t -> string -> int -> unit
    val set_int64 : t -> string -> int64 -> unit
    val set_float : t -> string -> float -> unit

    val load_channel : t -> in_channel -> unit
    val load : t -> string -> unit

    val print : Format.formatter -> t -> unit

    val store_channel : t -> out_channel -> unit
    val store : t -> string -> unit
  end

(** Shortcuts working with a default table *)

val get_default_table : unit -> Table.t
val set_default_table : Table.t -> unit

val get_bool : string -> bool
val get_string : string -> string
val get_int : string -> int
val get_int64 : string -> int64
val get_float : string -> float

val get_bool_default : string -> bool -> bool
val get_string_default : string -> string -> string
val get_int_default : string -> int -> int
val get_int64_default : string -> int64 -> int64
val get_float_default : string -> float -> float

val set_bool : string -> bool -> unit
val set_string : string -> string -> unit
val set_int : string -> int -> unit
val set_int64 : string -> int64 -> unit
val set_float : string -> float -> unit

val load_channel : in_channel -> unit
val load : string -> unit

val print : Format.formatter -> unit

val store_channel : out_channel -> unit
val store : string -> unit
