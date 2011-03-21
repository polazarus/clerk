(**
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

(** An extended strtoll *)
let strtoll s =
  let l = String.length s and i = ref 0 in
  let positive = match s.[!i] with
    | '+' -> incr i; true
    | '-' -> incr i; false
    | _ -> true
  and base = match s.[!i] with
    | '0' ->
      incr i;
      begin match s.[!i] with
      | 'x' -> incr i; 16
      | 'b' -> incr i; 2
      | 'o' -> incr i; 8
      | _ -> incr i; 8
      end
    | _ -> 10
  and res = ref 0L in
  let base64 = Int64.of_int base in
  while !i < l do
    let digit =
      match s.[!i] with
      | '0'..'9' as c ->
        let i = (Char.code c) - 48 in
          if i >= base then invalid_arg "invalid integer";
          Int64.of_int i
      | 'A'..'F' as c ->
        let i = (Char.code c) - 55 in
          if i >= base then invalid_arg "invalid integer";
          Int64.of_int i
      | 'a'..'f' as c ->
          let i = (Char.code c) - 87 in
          if i >= base then invalid_arg "invalid integer";
          Int64.of_int i
      | ('k' | 'K') when !i+1 = l -> 1024L
      | ('m' | 'M') when !i+1 = l -> 1048576L
      | ('g' | 'G') when !i+1 = l -> 1073741824L
      | _ ->invalid_arg "invalid integer"
    in
    incr i;
    res := Int64.add digit (Int64.mul base64 !res)
  done;
  if positive then !res else (Int64.neg !res)

(* Parsing *)  

module Parser = struct

  exception ParsingError of string

  let rec skip_line stream =
    match Stream.peek stream with
    | None -> ()
    | Some '\n' ->
      Stream.junk stream
    | Some _ ->
      Stream.junk stream;
      skip_line stream

  let rec skip_space stream =
    match Stream.peek stream with
    | Some (' '  | '\n' | '\t' | '\r' | '\x0C' | '\x0B') ->
      Stream.junk stream; skip_space stream
    | _ -> ()
    
  let rec skip_horizontal_space stream =
    match Stream.peek stream with
    | Some (' '  | '\t') ->
      Stream.junk stream; skip_horizontal_space stream
    | _ -> ()

  let add_escape_sequence buff stream =
    match Stream.next stream with
    | ('\\' | '\"') as c ->
      Buffer.add_char buff c
    | 't' ->
      Buffer.add_char buff '\t'
    | 'n' ->
      Buffer.add_char buff '\n'
    | 'b' ->
      Buffer.add_char buff '\b'
    | '\n' -> ()
    | '\r' when Stream.next stream = '\n' -> ()
    | _ ->
      raise (ParsingError "Unknown escape sequence")

  let rec add_value_in_quote buff stream =
    match Stream.next stream with
    | '\"' -> ()
    | '\\' ->
      add_escape_sequence buff stream;
      add_value_in_quote buff stream
    | '\n' ->
      raise (ParsingError "Unexpected end of line in value")
    | c ->
      Buffer.add_char buff c;
      add_value_in_quote buff stream

  let get_and_clear buff =
    let v = Buffer.contents buff in
      Buffer.clear buff;
      v

  let rec read_value buff stream spaces =
    match Stream.peek stream with
    | None ->
      get_and_clear buff
    | Some c ->
      Stream.junk stream;
      let add_spaces () = for i = 1 to spaces do Buffer.add_char buff ' ' done in
        match c with
        | '\n' ->
          get_and_clear buff
        | ' ' | '\t' | '\r' | '\x0C' | '\x0B' ->
          read_value buff stream (if Buffer.length buff > 0 then spaces +1 else spaces)
        (* extension *)
        | '"' ->
          add_spaces ();
          add_value_in_quote buff stream;
          read_value buff stream 0
        (* /extension *)
        | '#' | ';' ->
          skip_line stream;
          get_and_clear buff
        | '\\' ->
          add_spaces ();
          add_escape_sequence buff stream;
          read_value buff stream 0
        | c ->
          add_spaces ();
          Buffer.add_char buff c;
          read_value buff stream 0

  let rec read_assign buff stream =
    match Stream.peek stream with
    | None ->
      get_and_clear buff,None
    | Some c ->
      Stream.junk stream;
      match c with
      | ( 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '.'
        (* extension *) | '_' (* /extension *) )  as c->
        Buffer.add_char buff (Char.lowercase c);
        read_assign buff stream
      | '\n' ->
        get_and_clear buff,None
      | ' ' | '\t' ->
        skip_horizontal_space stream;
        let name = get_and_clear buff
        and value = match Stream.peek stream with
          | Some '=' -> Stream.junk stream; Some (read_value buff stream 0)
          | Some ('\r' | '\n') -> Stream.junk stream; None
          | None -> None
          | _ -> raise (ParsingError "Invalid character after variable name ('=' or line break expected)")
        in
          name, value
      | '=' ->
        let name = get_and_clear buff in
          name, Some (read_value buff stream 0)
      | _ -> raise (ParsingError "Invalid character after variable name ('=' or line break expected)")

  let rec read_extended_section buff stream =
    match Stream.next stream with
    | '"' ->
      ()
    | '\n' ->
      raise (ParsingError "Invalid character in section (unexpected line break)")
    | '\\' ->
      let c = Stream.next stream in
      if c = '\n' then
        raise (ParsingError "Invalid character in section (unexpected line break)");
      Buffer.add_char buff c;
      read_extended_section buff stream
    | c ->
      Buffer.add_char buff c;
      read_extended_section buff stream

  let rec read_section buff stream =
    match Stream.next stream with
    | ']'  ->
      get_and_clear buff
    | '"' ->
      read_extended_section buff stream;
      read_section buff stream
    | ' ' | '\t' | '\r' | '\x0C' | '\x0B' ->
      skip_space stream;
      begin match Stream.next stream with
      | '\"' ->
        Buffer.add_char buff '.';
        read_extended_section buff stream;
        read_section buff stream
      | _ ->
        raise (ParsingError "Invalid character in section name ('\"' expected)");
      end
    | ('A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '.'
    (* extension *) | '_' (* /extension *)
    ) as c ->
      Buffer.add_char buff (Char.lowercase c);
      read_section buff stream
    | _ ->
      raise (ParsingError "Invalid character in section name")

  let rec parse_rec f buff stream section =
    match Stream.peek stream with
    | None -> ()
    | Some c ->
      Stream.junk stream;
      match c with
      | '#' | ';' ->
        skip_line stream;
        parse_rec f buff stream section
      | '[' ->
        parse_rec f buff stream (read_section buff stream)
      | ' '  | '\n' | '\t' | '\r' | '\x0C' | '\x0B' ->
        parse_rec f buff stream section
      (* extension *)
      | '0'..'9' | '-' | '_'
      (* /extension*)
      | 'A'..'Z' | 'a'..'z' ->
        Buffer.add_string buff section;
        Buffer.add_char buff '.';
        Buffer.add_char buff c;
        let (name,value) = read_assign buff stream in
          f name value;
          parse_rec f buff stream section
      | _ ->
        raise (ParsingError "Invalid character")

  let parse_stream f stream =
    let buff = Buffer.create 256 in
    try
      parse_rec f buff stream ""
    with Stream.Failure ->
      raise (ParsingError "Unexpected end of file")

  let parse_channel f input =
    parse_stream f (Stream.of_channel input)
end

(** Parameter *)
type 'a parameter = 'a option ref

(** Get parameter's value *)
let get p =
  match !p with
  | Some value -> value
  | None -> raise Not_found

(** Replace parameter's value *)
let set p value =
  p := Some value


(** Configuration table *)

let default_default_table_size = 50

type setter = string option -> unit
type getter = bool -> string option option
type entry =  Unhandled of string option | Handled of setter * getter

type t = (string, entry) Hashtbl.t


(* Pretty printing *)

module Printer = struct
  open Format

  let must_quote s l=
    if l = 0 then
      true
    else
      let i = ref 0 and quote = ref false in
      while not !quote && !i < l do
        match s.[!i] with
        | ' '  | '\n' | '\t' | '\r' | '\x0C' | '\x0B' | '#' | ';' | '[' | '=' | '"' ->
          quote := true
        | _ -> incr i
      done;
      !quote
      
  let print_value_string formatter s =
    let l = String.length s in
    if must_quote s l then begin
      pp_print_char formatter '"';
      for i = 0 to l-1 do
        match s.[i] with
        | ('"' | '\\' as c) ->
          pp_print_char formatter '\\'; pp_print_char formatter c
        | '\n' ->
          pp_print_string formatter "\\n"
        | '\t' ->
          pp_print_string formatter "\\t"
        | '\b' ->
          pp_print_string formatter "\\b"
        | c ->
          pp_print_char formatter c
      done;
      pp_print_char formatter '"';
    end else
      pp_print_string formatter s

  let print_pair f (name,value) = 
    match value with
    | None ->
      fprintf f "@,@[<hov 2>%s@]" name
    | Some value ->
      fprintf f "@,@[<hov 2>%s =@ %a@]" name print_value_string value
  
  let print ?(default=false) formatter table =
    let sections = Hashtbl.create 10 in
    let add_section name value =
      let section,name =
        let l = String.length name and i = String.rindex name '.' in
          String.sub name 0 i, String.sub name (i+1) (l-i-1)
      in
        if Hashtbl.mem sections section then
          Hashtbl.replace sections section ((name,value)::(Hashtbl.find sections section))
        else
          Hashtbl.add sections section [name,value]
    in
    let f name value =
      match value with
      | Unhandled value -> add_section name value
      | Handled (_,getter) ->
        match getter default with
        | None -> ()
        | Some value -> add_section name value 
    in
      Hashtbl.iter f table;
    let print_section section pairs =
      fprintf formatter "@[<v 2>[%s]" section;
      List.iter (print_pair formatter) (List.rev pairs);
      fprintf formatter "@]@."
    in
      Hashtbl.iter print_section sections
end

(* All the rest *)



(** Make a configuration table *)
let make ?(size=default_default_table_size) () =
  Hashtbl.create size

let register tbl name setter getter =
  if Hashtbl.mem tbl name then
    match Hashtbl.find tbl name with
    | Unhandled v ->
      setter v;
      Hashtbl.replace tbl name (Handled (setter,getter))
    | Handled _ ->
      failwith "already use"
  else
    Hashtbl.add tbl name (Handled (setter,getter))

let load_one t name valueopt =
  if Hashtbl.mem t name then
    match Hashtbl.find t name with
    | Handled (setter,_) -> setter valueopt
    | Unhandled _ -> Hashtbl.replace t name (Unhandled valueopt)
  else
    Hashtbl.add t name (Unhandled valueopt)

let load_channel t channel =
  Parser.parse_channel (load_one t) channel

let load_file t filepath =
  load_channel t (open_in filepath)

let load t stream =
  Parser.parse_stream (load_one t) stream

let print = Printer.print

let store_channel t channel =
  Printer.print (Format.formatter_of_out_channel channel) t

let store t filepath =
  store_channel t (open_out filepath)

exception Undefined of string

module type PARAM = sig
  type t
  val get : unit -> t
  val set : t -> unit
end

module type CONVERTER = sig
  type t
  val of_raw : string option -> t
  val to_raw : t -> string option
end

(******************************************************************************)

(** Conversion *)
let bool_convert s =
  match s with
  | None -> true
  | Some s ->
    let s = String.lowercase s in
      if s = "true" || s = "yes" || s = "on" then
        true
      else if s = "" || s = "false" || s = "no" || s = "off" then
        false
      else
        try
          strtoll s <> 0L
        with Invalid_argument _->
          failwith "invalid boolean value"
let option_value s =
  match s with Some s -> s | None -> failwith "a value was expected"
let string_convert s = 
  option_value s
let int_convert s = 
  try
    Int64.to_int (strtoll (option_value s))
  with Invalid_argument _->
    failwith "invalid int value"
let int64_convert s = 
  try
    strtoll (option_value s)
  with Invalid_argument _ ->
    failwith "invalid int value"
let float_convert s =
  try
    float_of_string (option_value s)
  with Invalid_argument _ ->
    invalid_arg "invalid float value"

(** Modular parameterization *)
    
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
    
module Make (T : sig
  val table : t
end) = struct

let register = register T.table

module Simple (C : CONVERTER) (S : sig
    val name : string
    val default : C.t option
  end) = struct

  type t = C.t

  let value = ref None

  let get () =
    match !value with
    | Some v -> v
    | None ->
      match S.default with
      | Some d -> d
      | None ->
        raise (Undefined S.name)

  let set v =
    value := Some v

  let set_raw s =
    set (C.of_raw s)    

  let get_raw default =
    match !value with
    | Some v -> Some (C.to_raw v)
    | None ->
      if default then
        match S.default with
        | Some v -> Some (C.to_raw v)
        | None -> None
      else
        None

  let () =
    register S.name set_raw get_raw
end

module BoolConv = struct
  type t = bool
  let of_raw = bool_convert
  let to_raw b =
    Some (if b then "true" else "false")
end

module IntConv = struct
  type t = int
  let of_raw = int_convert
  let to_raw i = Some (string_of_int i)
end

module Int64Conv = struct
  type t = int64
  let of_raw = int64_convert
  let to_raw i = Some (Int64.to_string i)
end

module FloatConv = struct
  type t = float
  let of_raw = float_convert
  let to_raw f = Some (string_of_float f)
end

module StringConv = struct
  type t = string
  let of_raw = string_convert
  let to_raw s = Some s
end

module Bool (S : sig
  val name : string
  val default : bool option
end) = Simple (BoolConv) (S)

module Int (S : sig
  val name : string
  val default : int option
end) = Simple (IntConv) (S)

module Int64 (S : sig
  val name : string
  val default : int64 option
end) = Simple (Int64Conv) (S)

module Float (S : sig
  val name : string
  val default : float option
end) = Simple (FloatConv) (S)

module String (S : sig
  val name : string
  val default : string option
end) = Simple (StringConv) (S)

end
