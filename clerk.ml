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

(** Replace paramter's value *)
let set p value =
  p := Some value


(** Configuration table *)

let default_default_table_size = 50

type typedparam =
  Int of int parameter
| Int64 of int64 parameter
| Bool of bool parameter
| String of string parameter
| Float of float parameter

type tablevalue =
| Untyped of string option list
| Simple of typedparam

type t = (string, tablevalue) Hashtbl.t
type table = t


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
    | Simple (Int {contents=Some value}) ->
      fprintf f "@,@[<hov 2>%s =@ %d@]" name value
    | Simple (Int64 {contents=Some value}) ->
      fprintf f "@,@[<hov 2>%s =@ %s@]" name (Int64.to_string value)
    | Simple (Bool {contents=Some value}) ->
      fprintf f "@,@[<hov 2>%s =@ %B@]" name value
    | Simple (Float {contents=Some value}) ->
      fprintf f "@,@[<hov 2>%s =@ %f@]" name value
    | Simple (String {contents=Some value}) ->
      fprintf f "@,@[<hov 2>%s =@ %a@]" name print_value_string value
    | Simple _ ->
      ()
    | Untyped l ->
      let print s =
        match s with
        | Some s -> fprintf f "@,@[<hov 2>%s=%s@]" name s
        | None -> fprintf f "@,@[<hov 2>%s@]" name
      in
        List.iter print (List.rev l)
  
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

let get_param t (wrap, unwrap, make) name =
  if Hashtbl.mem t name then
    match Hashtbl.find t name with
    | Untyped l ->
      let param = make l in
        Hashtbl.replace t name (wrap param);
        param
    | wrapped_param ->
      unwrap wrapped_param
  else
    let param = make [] in
      Hashtbl.replace t name (wrap param);
      param  

let mk_generic t (wrap, unwrap, make) name default_value =
  let param = get_param t (wrap, unwrap, make) name in
    if !param = None then
      param := default_value;
    param

let make_unique convert values =
  let initial_value = match values with
    | [] -> None
    | head :: _ -> Some (convert head)
  in
    ref initial_value

let make_list convert values =
  let values = List.rev_map (convert values) in
    ref values

let wrap_int p = Simple (Int p)
let wrap_int64 p = Simple (Int64 p)
let wrap_bool p = Simple (Bool p)
let wrap_float p = Simple (Float p)
let wrap_string p = Simple (String p)

let unwrap_int tp =
  match tp with
  | Simple (Int p) -> p 
  | _ -> failwith "type error"
let unwrap_int64 tp =
  match tp with
  | Simple (Int64 p) -> p 
  | _ -> failwith "type error"
let unwrap_bool tp =
  match tp with
  | Simple (Bool p) -> p 
  | _ -> failwith "type error"
let unwrap_float tp =
  match tp with
  | Simple (Float p) -> p 
  | _ -> failwith "type error"
let unwrap_string tp =
  match tp with
  | Simple (String p) -> p 
  | _ -> failwith "type error"


let mk_int t ?default name =
  mk_generic t (wrap_int, unwrap_int, make_unique int_convert) name default
let mk_int64 t ?default name =
  mk_generic t (wrap_int64, unwrap_int64, make_unique int64_convert) name default
let mk_bool t ?default name =
  mk_generic t (wrap_bool, unwrap_bool, make_unique bool_convert) name default
let mk_float t ?default name =
  mk_generic t (wrap_float, unwrap_float, make_unique float_convert) name default
let mk_string t ?default name =
  mk_generic t (wrap_string, unwrap_string, make_unique string_convert) name default

let update_simple tp valueopt =
  match tp with
  | Int p -> p := Some (int_convert valueopt)
  | Int64 p -> p := Some (int64_convert valueopt)
  | Bool p -> p := Some (bool_convert valueopt)
  | Float p -> p := Some (float_convert valueopt)
  | String p -> p := Some (string_convert valueopt)

let load_one t name valueopt =
  if Hashtbl.mem t name then
    match Hashtbl.find t name with
    | Untyped l -> Hashtbl.replace t name (Untyped (valueopt :: l))
    | Simple tp -> update_simple tp valueopt
  else
    Hashtbl.add t name (Untyped [valueopt])

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
