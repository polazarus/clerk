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

  let rec add_value_in_quote buff stream =
    match Stream.next stream with
    | '\"' -> ()
    | '\\' ->
      begin match Stream.next stream with
      | ('\\' | '\"') as c ->  Buffer.add_char buff c
      | 't' -> Buffer.add_char buff '\t'
      | 'n' -> Buffer.add_char buff '\n'
      | 'b' -> Buffer.add_char buff '\b'
      | '\n' -> ()
      | '\r' when Stream.next stream = '\n' -> ()
      | _ ->
        failwith "unknown escape sequence in value"
      end;
      add_value_in_quote buff stream
    | '\n' ->
      failwith "unexpected end of line in value" 
    | c ->
      Buffer.add_char buff c;
      add_value_in_quote buff stream

  let get_and_clear buff =
    let v = Buffer.contents buff in
      Buffer.clear buff;
      v

  let rec read_value buff stream offset spaces =
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
          let spaces = if Buffer.length buff > offset then spaces +1 else spaces in
          read_value buff stream offset spaces
        | '"' ->
          add_spaces ();
          add_value_in_quote buff stream;
          read_value buff stream offset 0
        | '#' | ';' ->
          skip_line stream;
          get_and_clear buff
        | '\\' ->
          add_spaces ();
          begin match Stream.next stream with
            | ('\\' | '\"') as c ->
              Buffer.add_char buff c; read_value buff stream offset 0
            | 't' ->
              Buffer.add_char buff '\t'; read_value buff stream offset 0
            | 'n' ->
              Buffer.add_char buff '\n'; read_value buff stream offset 0
            | 'b' ->
              Buffer.add_char buff '\b'; read_value buff stream offset 0
            | '\n' ->
              read_value buff stream (Buffer.length buff) 0
            | '\r' when Stream.next stream = '\n' ->
              read_value buff stream (Buffer.length buff) 0
            | _ -> failwith "unknown escape sequence in value"
          end
        | c ->
          add_spaces ();
          Buffer.add_char buff c;
          read_value buff stream offset 0

  let rec read_assign buff stream =
    match Stream.peek stream with
    | None ->
      get_and_clear buff,None
    | Some c ->
      Stream.junk stream;
      match c with
      | ('A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '.')  as c->
        Buffer.add_char buff (Char.lowercase c);
        read_assign buff stream
      | '\n' ->
        get_and_clear buff,None
      | ' ' | '\t' ->
        skip_horizontal_space stream;
        let name = get_and_clear buff
        and value = match Stream.peek stream with
          | Some '=' -> Stream.junk stream; Some (read_value buff stream 0 0)
          | Some ('\r' | '\n') -> Stream.junk stream; None
          | None -> None
          | _ -> failwith "invalid character after variable name ('=' or line break expected)"
        in
          name, value
      | _ -> failwith "invalid character after variable name ('=' or line break expected)"

  let rec read_extended_section_rec buff stream =
    match Stream.next stream with
    | '"' ->
      ()
    | '\n' ->
      failwith "invalid character in section (unexpected line break)"
    | '\\' ->
      let c = Stream.next stream in
      if c = '\n' then
        failwith "invalid character in section (unexpected line break)";
      Buffer.add_char buff c;
      read_extended_section_rec buff stream
    | c ->
      Buffer.add_char buff c;
      read_extended_section_rec buff stream

  let rec read_section buff stream =
    match Stream.next stream with
    | ']'  ->
      get_and_clear buff
    | ' '  | '\n' | '\t' | '\r' | '\x0C' | '\x0B' ->
      skip_space stream;
      begin match Stream.next stream with
      | '\"' ->
        Buffer.add_char buff '.';
        read_extended_section_rec buff stream;
        read_section buff stream
      | _ ->
        failwith "invalid character in section ('\"' expected)";
      end
    | ('A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '.') as c ->
      Buffer.add_char buff (Char.lowercase c);
      read_section buff stream
    | _ ->
      failwith "invalid character in section"

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
      | 'A'..'Z' | 'a'..'z' ->
        Buffer.add_string buff section;
        Buffer.add_char buff '.';
        Buffer.add_char buff c;
        let (name,value) = read_assign buff stream in
          f name value;
          parse_rec f buff stream section
      | _ ->
        failwith "invalid character"

  let parse_stream f stream =
    let buff = Buffer.create 256 in
    try
      parse_rec f buff stream ""
    with Stream.Failure ->
      failwith "unexpected end of file"

  let parse_channel f input =
    parse_stream f (Stream.of_channel input)
end


(* Configuration table *)

module Table = struct

  let default_default_table_size = 50
  
  type config_type =
  | ConfigBool of bool
  | ConfigString of string
  | ConfigInt of int
  | ConfigInt64 of int64
  | ConfigFloat of float
  | Untyped of string

  type t = (string,config_type) Hashtbl.t
  

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
        
    
    let print formatter table =
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
        Hashtbl.iter add_section table;
      let convert formatter value = 
        match value with
        | ConfigBool true -> pp_print_string formatter "=true"
        | ConfigBool false -> pp_print_string formatter "=false"
        | ConfigInt i -> pp_print_char formatter '='; pp_print_string formatter (string_of_int i)
        | ConfigInt64 i -> pp_print_char formatter '='; pp_print_string formatter (Int64.to_string i)
        | ConfigString s -> pp_print_char formatter '='; print_value_string formatter s
        | ConfigFloat f -> pp_print_char formatter '='; pp_print_float formatter f
        | Untyped s -> pp_print_char formatter '='; print_value_string formatter s
      in
      let print_section section pairs =
        fprintf formatter "@[<v 2>[%s]" section;
        List.iter (fun (name,value) -> fprintf formatter "@,@[<hov 2>%s%a@]" name convert value) (List.rev pairs);
        fprintf formatter "@]@."
      in
        Hashtbl.iter print_section sections
  end

(* All the rest *)

  let make ?(size=default_default_table_size) () =
    Hashtbl.create size

  let bool_converter t name = 
    match (Hashtbl.find t name) with
    | ConfigBool b -> b
    | Untyped s ->
      let s = String.lowercase s in
      let b =
        if s = "true" || s="yes" || s = "on" then
          true
        else if s = "" || s = "false" || s = "no" || s = "off" then
          false
        else
          try
            strtoll s <> 0L
          with Invalid_argument _->
            invalid_arg "invalid boolean value"
      in
        Hashtbl.replace t name (ConfigBool b);
        b
    | _ -> invalid_arg "type error"
  
  let string_converter t name = 
    match (Hashtbl.find t name) with
    | ConfigString s -> s
    | Untyped s ->
        Hashtbl.replace t name (ConfigString s);
        s
    | _ -> invalid_arg "type error"

  let int_converter t name = 
    match (Hashtbl.find t name) with
    | ConfigInt i -> i
    | Untyped s ->
      let i =
        try
          Int64.to_int (strtoll s)
        with Invalid_argument _->
          invalid_arg "invalid int value"
      in
        Hashtbl.replace t name (ConfigInt i);
        i
    | _ -> invalid_arg "type error"

  let int64_converter t name = 
    match (Hashtbl.find t name) with
    | ConfigInt64 i -> i
    | Untyped s ->
      let i =
        try
          strtoll s
        with Invalid_argument _ ->
          invalid_arg "invalid int value"
      in
        Hashtbl.replace t name (ConfigInt64 i);
        i
    | _ -> invalid_arg "type error"
    
  let float_converter t name =
    match Hashtbl.find t name with
    | ConfigFloat f -> f
    | Untyped s ->
      let f =
        try
          float_of_string s
        with Invalid_argument _ ->
          invalid_arg "invalid float value"
      in
        Hashtbl.replace t name (ConfigFloat f);
        f
    | _ -> invalid_arg "type error"

  let get_generic t convertgetter name =
    if Hashtbl.mem t name then
      convertgetter t name
    else
      invalid_arg "invalid name"
  let get_generic_default t convertgetter name default =
    if Hashtbl.mem t name then
      convertgetter t name
    else
      default
  let set_generic t (name : string) value =
      Hashtbl.replace t name value

  let get_bool t name =
    get_generic t bool_converter name
  let get_string t name =
    get_generic t string_converter name
  let get_int t name =
    get_generic t int_converter name
  let get_int64 t name =
    get_generic t int64_converter name
  let get_float t name =
    get_generic t float_converter name

  let get_bool_default t name default =
    get_generic_default t bool_converter name default
  let get_string_default t name default =
    get_generic_default t string_converter name default
  let get_int_default t name default =
    get_generic_default t int_converter name default
  let get_int64_default t name default =
    get_generic_default t int64_converter name default
  let get_float_default t name default =
    get_generic_default t float_converter name default

  let set_bool t name value =
    set_generic t name (ConfigBool value)
  let set_string t name value =
    set_generic t name (ConfigString value)
  let set_int t name value =
    set_generic t name (ConfigInt value)
  let set_int64 t name value =
    set_generic t name (ConfigInt64 value)
  let set_float t name value =
    set_generic t name (ConfigFloat value)
  let set_untyped t name value =
    match value with
    | Some s -> set_generic t name (Untyped s)
    | _ -> set_generic t name (ConfigBool true)

  let load_channel t channel =
    Parser.parse_channel (set_untyped t) channel
  let load t filepath =
    load_channel t (open_in filepath)

  let print = Printer.print

  let store_channel t channel =
    Printer.print (Format.formatter_of_out_channel channel) t

  let store t filepath =
    store_channel t (open_out filepath)
end

let default_table = ref None

let get_default_table () =
    match !default_table with
    | None -> let t = Table.make () in default_table := Some t; t
    | Some t -> t

let set_default_table table =
  default_table := Some table

let get_bool = Table.get_bool (get_default_table ())
let get_string = Table.get_string (get_default_table ())
let get_int = Table.get_int (get_default_table ())
let get_int64 = Table.get_int64 (get_default_table ())
let get_float = Table.get_float (get_default_table ())

let get_bool_default = Table.get_bool_default (get_default_table ())
let get_string_default = Table.get_string_default (get_default_table ())
let get_int_default = Table.get_int_default (get_default_table ())
let get_int64_default = Table.get_int64_default (get_default_table ())
let get_float_default = Table.get_float_default (get_default_table ())

let set_bool = Table.set_bool (get_default_table ())
let set_string = Table.set_string (get_default_table ())
let set_int = Table.set_int (get_default_table ())
let set_int64 = Table.set_int64 (get_default_table ())
let set_float = Table.set_float (get_default_table ())


let load_channel = Table.load_channel (get_default_table ())
let load = Table.load (get_default_table ())
let print formatter= Table.print formatter (get_default_table ())
let store_channel = Table.store_channel (get_default_table ())
let store = Table.store (get_default_table ())
