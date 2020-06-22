(* Type definitions - CONSTANTS *)
type stackValue = INT of int | BOOL of bool | STRING of string | NAME of string | UNIT | ERROR
type command = QUIT | PRINTLN | TOSTRING | ADD | SUB | MUL | DIV | REM | NEG | SWAP | POP | PUSH of stackValue

let int_regexp = Str.regexp "[+]?[-]?[0-9]+$"
let name_regexp = Str.regexp "[_A-Za-z]+[_A-Za-z0-9]?"

let interpreter (input, output) =

let ic = open_in input in

let oc = open_out output in

let rec loop_read acc =
	try
		let l = String.trim(input_line ic) in loop_read (l::acc) 
	with
	| End_of_file -> List.rev acc in

let file_write stack_val = Printf.fprintf oc "%s\n" stack_val in

let program_as_list = loop_read [] in

let rec map func some_list =
	match some_list with
	| [] -> []
	| hd::tl -> (func hd)::(map func tl) in

let str_to_stack_val some_str =
	match some_str with
	|":true:" -> PUSH (BOOL (true))
	|":false:" -> PUSH (BOOL (false))
	|":error:" -> PUSH ERROR
	|":unit:" -> PUSH UNIT
	|other -> if (String.sub other 0 1) = "\"" then PUSH (STRING (String.sub other 1 ((String.length other)-2)))
			  else if (Str.string_match name_regexp other 0) then PUSH (NAME (other))
			  else if (Str.string_match int_regexp other 0) then PUSH (INT (int_of_string other))
			  else PUSH ERROR in

let stack_val_to_str some_sv =
	match some_sv with
	|INT (some_int) -> string_of_int(some_int)
	|BOOL (some_bool) -> (match some_bool with
						  | true -> ":true:"
				      	  | false -> ":false:")
	|ERROR -> ":error:"
	|STRING (some_string) -> some_string
	|NAME (some_name) -> some_name
	|UNIT -> ":unit:" in

let str_to_command str_from_program =
	match str_from_program with
	|"add" -> ADD
	|"sub" -> SUB
	|"mul" -> MUL 
	|"div" -> DIV 
	|"rem" -> REM 
	|"neg" -> NEG
	|"println" -> PRINTLN 
	|"toString" -> TOSTRING 
	|"swap" -> SWAP 
	|"pop" -> POP 
	|"quit" -> QUIT
	| anything_else -> 
	if Str.string_before anything_else 4 = "push" then str_to_stack_val (Str.string_after anything_else 5)
	else PUSH ERROR in

(* let command_to_str command_from_program =
	match command_from_program with
	| ADD -> "add" | SUB -> "sub" | MUL -> "mul" | DIV -> "div" | REM -> "rem" | NEG -> "neg"
	| PRINTLN -> "println" | TOSTRING -> "toString"
	| SWAP -> "swap" | POP -> "pop" | QUIT -> "quit"
	| PUSH sv -> "push " ^ (match sv with
				| INT (some_int) -> string_of_int(some_int)
				| BOOL (some_bool) -> (match some_bool with
									   | true -> ":true:"
									   | false -> ":false:")
				| ERROR -> ":error:"
				| STRING (some_string) -> some_string
				| NAME (some_name) -> some_name
				| UNIT -> ":unit:") in *)

let command_list = map str_to_command program_as_list in

(* let test_comList_to_strList = map command_to_str command_list in

let rec print_comList_as_strList c =
	match c with
	| [] -> file_write "DONE"
	| com::tl -> file_write com; print_comList_as_strList tl in *)

let rec processor commands stack =
	match commands, stack with
	| QUIT::restOfComs, restOfStack -> close_out oc
	| PUSH(sv)::restOfComs, restOfStack -> processor restOfComs (sv::restOfStack)
	| POP::restOfComs, top_elem::restOfStack -> processor restOfComs restOfStack
	| TOSTRING::restOfComs, top_elem::restOfStack -> processor restOfComs (STRING(stack_val_to_str top_elem)::restOfStack)
	| PRINTLN::restOfComs, STRING(elem_string)::restOfStack -> file_write elem_string; processor restOfComs restOfStack
	| ADD::restOfComs, INT(num_one)::INT(num_two)::restOfStack -> processor restOfComs (INT(num_one+num_two)::restOfStack)
	| SUB::restOfComs, INT(num_one)::INT(num_two)::restOfStack -> processor restOfComs (INT(num_two-num_one)::restOfStack)
	| MUL::restOfComs, INT(num_one)::INT(num_two)::restOfStack -> processor restOfComs (INT(num_one*num_two)::restOfStack)
	| DIV::restOfComs, INT(num_one)::INT(num_two)::restOfStack ->
	  if num_one != 0 then processor restOfComs (INT(num_two/num_one)::restOfStack)
	  else processor restOfComs (ERROR::INT(num_one)::INT(num_two)::restOfStack)
	| REM::restOfComs, INT(num_one)::INT(num_two)::restOfStack -> 
	  if num_one != 0 then processor restOfComs (INT(num_two mod num_one)::restOfStack)
	  else processor restOfComs (ERROR::INT(num_one)::INT(num_two)::restOfStack)
	| NEG::restOfComs, INT(num_one)::restOfStack -> processor restOfComs (INT(-num_one)::restOfStack)
	| SWAP::restOfComs, elem_one::elem_two::restOfStack -> processor restOfComs (elem_two::elem_one::restOfStack)
	| invalid::restOfComs, restOfStack -> processor restOfComs (ERROR::restOfStack) in

processor command_list []

(* print_comList_as_strList test_comList_to_strList  *)

;;

(* interpreter ("test.txt", "output.txt") *)

