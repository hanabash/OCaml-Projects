(* Type definitions - CONSTANTS *)
type stackValue = INT of int | BOOL of bool | STRING of string | NAME of string | UNIT | ERROR
type command = QUIT | PRINTLN | TOSTRING | ADD | SUB | MUL | DIV | REM | NEG | SWAP | CAT | AND | OR | NOT | EQUAL | LESSTHAN | IF | BIND | LET | END | POP | PUSH of stackValue

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
	|"cat" -> CAT
	|"and" -> AND
	|"or" -> OR
	|"not" -> NOT
	|"equal" -> EQUAL
	|"lessThan" -> LESSTHAN
	|"if" -> IF
	|"bind" -> BIND
	|"let" -> LET
	|"end" -> END 
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

(* let rec add key value structure =
	match structure with
	| [] -> (key, value)::[]
	| (some_k, some_v)::restOfStructure ->
	  if some_k == key then (key, value)::restOfStructure
	  else add key value restOfStructure in *)

let rec lookup key structure =
	match structure with
	| [] -> None
	| (NAME(k), v)::restOfStructure -> 
	  if k = key then (match v with
	  	  | INT(value) -> Some (INT(value))
	  	  | BOOL(some_bool_val) -> Some (BOOL(some_bool_val))
	  	  | STRING(some_str_val) -> Some (STRING(some_str_val)))
	  else lookup key restOfStructure in

(* let test_comList_to_strList = map command_to_str command_list in

let rec print_comList_as_strList c =
	match c with
	| [] -> file_write "DONE"
	| com::tl -> file_write com; print_comList_as_strList tl in *)

let rec processor commands stack env =
	match commands, stack, env with
	| QUIT::restOfComs, restOfStack, env -> close_out oc
	| PUSH(sv)::restOfComs, restOfStack, env -> processor restOfComs (sv::restOfStack) env
	| POP::restOfComs, top_elem::restOfStack, env -> processor restOfComs restOfStack env
	| TOSTRING::restOfComs, top_elem::restOfStack, env -> processor restOfComs (STRING(stack_val_to_str top_elem)::restOfStack) env
	| PRINTLN::restOfComs, STRING(elem_string)::restOfStack, env -> file_write elem_string; processor restOfComs restOfStack env
	| ADD::restOfComs, INT(num_one)::INT(num_two)::restOfStack, env -> processor restOfComs (INT(num_one+num_two)::restOfStack) env
	| ADD::restOfComs, NAME(elem_name)::INT(elem_int)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name)::INT(elem_int)::restOfStack) env
	  | Some value -> (match value with | INT(some_num) -> (processor restOfComs (INT(elem_int+some_num)::restOfStack)) env
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name)::INT(elem_int)::restOfStack) env))
	| ADD::restOfComs, INT(elem_int)::NAME(elem_name)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::INT(elem_int)::NAME(elem_name)::restOfStack) env
	  | Some value -> (match value with | INT(some_num) -> (processor restOfComs (INT(elem_int+some_num)::restOfStack)) env
	    | anything_else -> processor restOfComs (ERROR::INT(elem_int)::NAME(elem_name)::restOfStack) env))
	| ADD::restOfComs, NAME(elem_name_one)::NAME(elem_name_two)::restOfStack, env ->
	  (match lookup elem_name_one env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value_one -> (match value_one with | INT(some_num_one) -> (match lookup elem_name_two env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value_two -> (match value_two with | INT(some_num_two) -> (processor restOfComs (INT(some_num_one+some_num_two)::restOfStack)) env
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	| SUB::restOfComs, INT(num_one)::INT(num_two)::restOfStack, env -> processor restOfComs (INT(num_two-num_one)::restOfStack) env
	| SUB::restOfComs, NAME(elem_name)::INT(elem_int)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name)::INT(elem_int)::restOfStack) env
	  | Some value -> (match value with | INT(some_num) -> (processor restOfComs (INT(elem_int-some_num)::restOfStack)) env
	  	| anything_else -> processor restOfComs (ERROR::NAME(elem_name)::INT(elem_int)::restOfStack) env))
	| SUB::restOfComs, INT(elem_int)::NAME(elem_name)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::INT(elem_int)::NAME(elem_name)::restOfStack) env
	  | Some value -> (match value with | INT(some_num) -> (processor restOfComs (INT(some_num-elem_int)::restOfStack)) env
	  	| anything_else -> processor restOfComs (ERROR::INT(elem_int)::NAME(elem_name)::restOfStack) env))
	| SUB::restOfComs, NAME(elem_name_one)::NAME(elem_name_two)::restOfStack, env ->
	  (match lookup elem_name_one env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value_one -> (match value_one with | INT(some_num_one) -> (match lookup elem_name_two env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value_two -> (match value_two with | INT(some_num_two) -> (processor restOfComs (INT(some_num_two-some_num_one)::restOfStack)) env
	  	| anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	| MUL::restOfComs, INT(num_one)::INT(num_two)::restOfStack, env -> processor restOfComs (INT(num_one*num_two)::restOfStack) env
	| MUL::restOfComs, NAME(elem_name)::INT(elem_int)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name)::INT(elem_int)::restOfStack) env
	  | Some value -> (match value with | INT(some_num) -> (processor restOfComs (INT(elem_int*some_num)::restOfStack)) env
	  	| anything_else -> processor restOfComs (ERROR::NAME(elem_name)::INT(elem_int)::restOfStack) env))
	| MUL::restOfComs, INT(elem_int)::NAME(elem_name)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::INT(elem_int)::NAME(elem_name)::restOfStack) env
	  | Some value -> (match value with | INT(some_num) -> (processor restOfComs (INT(elem_int*some_num)::restOfStack)) env
	  	| anything_else -> processor restOfComs (ERROR::INT(elem_int)::NAME(elem_name)::restOfStack) env))
	| MUL::restOfComs, NAME(elem_name_one)::NAME(elem_name_two)::restOfStack, env ->
	  (match lookup elem_name_one env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value_one -> (match value_one with | INT(some_num_one) -> (match lookup elem_name_two env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value_two -> (match value_two with | INT(some_num_two) -> (processor restOfComs (INT(some_num_one*some_num_two)::restOfStack)) env
	  	| anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	| DIV::restOfComs, INT(num_one)::INT(num_two)::restOfStack, env ->
	  if num_one != 0 then processor restOfComs (INT(num_two/num_one)::restOfStack) env
	  else processor restOfComs (ERROR::INT(num_one)::INT(num_two)::restOfStack) env
	| DIV::restOfComs, NAME(elem_name)::INT(elem_int)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name)::INT(elem_int)::restOfStack) env
	  | Some value -> (match value with | INT(some_num) ->
	  	if some_num != 0 then processor restOfComs (INT(elem_int/some_num)::restOfStack) env
	    else processor restOfComs (ERROR::NAME(elem_name)::INT(elem_int)::restOfStack) env
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name)::INT(elem_int)::restOfStack) env))
	| DIV::restOfComs, INT(elem_int)::NAME(elem_name)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::INT(elem_int)::NAME(elem_name)::restOfStack) env
	  | Some value -> (match value with | INT(some_num) ->
	  	if elem_int != 0 then processor restOfComs (INT(some_num/elem_int)::restOfStack) env
	    else processor restOfComs (ERROR::INT(elem_int)::NAME(elem_name)::restOfStack) env
	    | anything_else -> processor restOfComs (ERROR::INT(elem_int)::NAME(elem_name)::restOfStack) env))
	| DIV::restOfComs, NAME(elem_name_one)::NAME(elem_name_two)::restOfStack, env ->
	  (match lookup elem_name_one env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value_one -> (match value_one with | INT(some_num_one) -> (match lookup elem_name_two env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value_two -> (match value_two with | INT(some_num_two) ->
	  	if some_num_one != 0 then processor restOfComs (INT(some_num_two/some_num_one)::restOfStack) env
	    else processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	| REM::restOfComs, INT(num_one)::INT(num_two)::restOfStack, env -> 
	  if num_one != 0 then processor restOfComs (INT(num_two mod num_one)::restOfStack) env
	  else processor restOfComs (ERROR::INT(num_one)::INT(num_two)::restOfStack) env
	| REM::restOfComs, NAME(elem_name)::INT(elem_int)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name)::INT(elem_int)::restOfStack) env
	  | Some value -> (match value with | INT(some_num) ->
	  	if some_num != 0 then processor restOfComs (INT(elem_int mod some_num)::restOfStack) env
	    else processor restOfComs (ERROR::NAME(elem_name)::INT(elem_int)::restOfStack) env
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name)::INT(elem_int)::restOfStack) env))
	| REM::restOfComs, INT(elem_int)::NAME(elem_name)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::INT(elem_int)::NAME(elem_name)::restOfStack) env
	  | Some value -> (match value with | INT(some_num) ->
	  	if elem_int != 0 then processor restOfComs (INT(some_num mod elem_int)::restOfStack) env
	    else processor restOfComs (ERROR::INT(elem_int)::NAME(elem_name)::restOfStack) env
	    | anything_else -> processor restOfComs (ERROR::INT(elem_int)::NAME(elem_name)::restOfStack) env))
	| REM::restOfComs, NAME(elem_name_one)::NAME(elem_name_two)::restOfStack, env ->
	  (match lookup elem_name_one env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value_one -> (match value_one with | INT(some_num_one) -> (match lookup elem_name_two env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value_two -> (match value_two with | INT(some_num_two) ->
	  	if some_num_one != 0 then processor restOfComs (INT(some_num_two mod some_num_one)::restOfStack) env
	    else processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	| NEG::restOfComs, INT(num_one)::restOfStack, env -> processor restOfComs (INT(-num_one)::restOfStack) env
	| NEG::restOfComs, NAME(elem_name)::restOfStack, env -> (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name)::restOfStack) env
	  | Some value -> (match value with | INT(some_num) -> (processor restOfComs (INT(-some_num)::restOfStack)) env
	  	| anything_else -> processor restOfComs (ERROR::NAME(elem_name)::restOfStack) env))
	| SWAP::restOfComs, elem_one::elem_two::restOfStack, env -> processor restOfComs (elem_two::elem_one::restOfStack) env
	| CAT::restOfComs, STRING(elem_string_one)::STRING(elem_string_two)::restOfStack, env -> processor restOfComs (STRING(elem_string_two ^ elem_string_one)::restOfStack) env
	| CAT::restOfComs, NAME(elem_name)::STRING(elem_string)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name)::STRING(elem_string)::restOfStack) env
	  | Some value -> (match value with | STRING(some_str) -> processor restOfComs (STRING(elem_string ^ some_str)::restOfStack) env
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name)::STRING(elem_string)::restOfStack) env)) 
	| CAT::restOfComs, STRING(elem_string)::NAME(elem_name)::restOfStack, env -> 
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::STRING(elem_string)::NAME(elem_name)::restOfStack) env
	  | Some value -> (match value with | STRING(some_str) -> processor restOfComs (STRING(some_str ^ elem_string)::restOfStack) env
	    | anything_else -> processor restOfComs (ERROR::STRING(elem_string)::NAME(elem_name)::restOfStack) env))  
	| CAT::restOfComs, NAME(elem_name_one)::NAME(elem_name_two)::restOfStack, env ->
	  (match lookup elem_name_one env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value -> (match value with | STRING(some_str_one) -> (match lookup elem_name_two env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value -> (match value with | STRING(some_str_two) -> processor restOfComs (STRING(some_str_two ^ some_str_one)::restOfStack) env
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))  
	| AND::restOfComs, BOOL(elem_bool_one)::BOOL(elem_bool_two)::restOfStack, env -> processor restOfComs (BOOL(elem_bool_one && elem_bool_two)::restOfStack) env
	| AND::restOfComs, NAME(elem_name)::BOOL(elem_bool)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name)::BOOL(elem_bool)::restOfStack) env
	  | Some value -> (match value with | BOOL(some_bool) -> processor restOfComs (BOOL(some_bool && elem_bool)::restOfStack) env
	  	| anything_else -> processor restOfComs (ERROR::NAME(elem_name)::BOOL(elem_bool)::restOfStack) env)) 
	| AND::restOfComs, BOOL(elem_bool)::NAME(elem_name)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::BOOL(elem_bool)::NAME(elem_name)::restOfStack) env
	  | Some value -> (match value with | BOOL(some_bool) -> processor restOfComs (BOOL(some_bool && elem_bool)::restOfStack) env
	  	| anything_else -> processor restOfComs (ERROR::BOOL(elem_bool)::NAME(elem_name)::restOfStack) env)) 
	| AND::restOfComs, NAME(elem_name_one)::NAME(elem_name_two)::restOfStack, env ->
	  (match lookup elem_name_one env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value -> (match value with | BOOL(some_bool_one) -> (match lookup elem_name_two env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value -> (match value with | BOOL(some_bool_two) -> processor restOfComs (BOOL(some_bool_one && some_bool_two)::restOfStack) env
	  	| anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	| OR::restOfComs, BOOL(elem_bool_one)::BOOL(elem_bool_two)::restOfStack, env -> processor restOfComs (BOOL(elem_bool_one || elem_bool_two)::restOfStack) env
	| OR::restOfComs, NAME(elem_name)::BOOL(elem_bool)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name)::BOOL(elem_bool)::restOfStack) env
	  | Some value -> (match value with | BOOL(some_bool) -> processor restOfComs (BOOL(some_bool || elem_bool)::restOfStack) env
	  	| anything_else -> processor restOfComs (ERROR::NAME(elem_name)::BOOL(elem_bool)::restOfStack) env))
	| OR::restOfComs, BOOL(elem_bool)::NAME(elem_name)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::BOOL(elem_bool)::NAME(elem_name)::restOfStack) env
	  | Some value -> (match value with | BOOL(some_bool) -> processor restOfComs (BOOL(some_bool || elem_bool)::restOfStack) env
	  	| anything_else -> processor restOfComs (ERROR::BOOL(elem_bool)::NAME(elem_name)::restOfStack) env)) 
	| OR::restOfComs, NAME(elem_name_one)::NAME(elem_name_two)::restOfStack, env ->
	  (match lookup elem_name_one env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value -> (match value with | BOOL(some_bool_one) -> (match lookup elem_name_two env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value -> (match value with | BOOL(some_bool_two) -> processor restOfComs (BOOL(some_bool_one || some_bool_two)::restOfStack) env
	  	| anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	| NOT::restOfComs, BOOL(elem_bool_one)::restOfStack, env -> processor restOfComs (BOOL(not elem_bool_one)::restOfStack) env
	| NOT::restOfComs, NAME(elem_name)::restOfStack, env -> (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name)::restOfStack) env
	  | Some value -> (match value with | BOOL(some_bool) -> (processor restOfComs (BOOL(not some_bool)::restOfStack)) env
	  	| anything_else -> processor restOfComs (ERROR::NAME(elem_name)::restOfStack) env))
	| EQUAL::restOfComs, INT(num_one)::INT(num_two)::restOfStack, env -> 
	  if num_one = num_two then processor restOfComs (BOOL(true)::restOfStack) env
	  else processor restOfComs (BOOL(false)::restOfStack) env
	| EQUAL::restOfComs, INT(num_one)::NAME(elem_name)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::INT(num_one)::NAME(elem_name)::restOfStack) env
	  | Some value -> (match value with | INT(some_num) -> 
	    if some_num = num_one then processor restOfComs (BOOL(true)::restOfStack) env
	    else processor restOfComs (BOOL(false)::restOfStack) env
	    | anything_else -> processor restOfComs (ERROR::INT(num_one)::NAME(elem_name)::restOfStack) env)) 
	| EQUAL::restOfComs, NAME(elem_name)::INT(num_two)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name)::INT(num_two)::restOfStack) env
	  | Some value -> (match value with | INT(some_num) -> 
	    if num_two = some_num then processor restOfComs (BOOL(true)::restOfStack) env
	    else processor restOfComs (BOOL(false)::restOfStack) env
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name)::INT(num_two)::restOfStack) env)) 
	| EQUAL::restOfComs, NAME(elem_name_one)::NAME(elem_name_two)::restOfStack, env ->
	  (match lookup elem_name_one env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value_one -> (match value_one with | INT(some_num_one) -> (match lookup elem_name_two env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value_two -> (match value_two with | INT(some_num_two) ->
	  	if some_num_two = some_num_one then processor restOfComs (BOOL(true)::restOfStack) env
	    else processor restOfComs (BOOL(false)::restOfStack) env
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))  
	| LESSTHAN::restOfComs, INT(num_one)::INT(num_two)::restOfStack, env ->
	  if num_two < num_one then processor restOfComs (BOOL(true)::restOfStack) env
	  else processor restOfComs (BOOL(false)::restOfStack) env
	| LESSTHAN::restOfComs, INT(num_one)::NAME(elem_name)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::INT(num_one)::NAME(elem_name)::restOfStack) env
	  | Some value -> (match value with | INT(some_num) -> 
	    if some_num < num_one then processor restOfComs (BOOL(true)::restOfStack) env
	    else processor restOfComs (BOOL(false)::restOfStack) env
	    | anything_else -> processor restOfComs (ERROR::INT(num_one)::NAME(elem_name)::restOfStack) env))
	| LESSTHAN::restOfComs, NAME(elem_name)::INT(num_two)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name)::INT(num_two)::restOfStack) env
	  | Some value -> (match value with | INT(some_num) -> 
	    if num_two < some_num then processor restOfComs (BOOL(true)::restOfStack) env
	    else processor restOfComs (BOOL(false)::restOfStack) env
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name)::INT(num_two)::restOfStack) env))
	| LESSTHAN::restOfComs, NAME(elem_name_one)::NAME(elem_name_two)::restOfStack, env ->
	  (match lookup elem_name_one env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value_one -> (match value_one with | INT(some_num_one) -> (match lookup elem_name_two env with
	  | None -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env
	  | Some value_two -> (match value_two with | INT(some_num_two) ->
	  	if some_num_two < some_num_one then processor restOfComs (BOOL(true)::restOfStack) env
	    else processor restOfComs (BOOL(false)::restOfStack) env
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	    | anything_else -> processor restOfComs (ERROR::NAME(elem_name_one)::NAME(elem_name_two)::restOfStack) env))
	| IF::restOfComs, elem_one::elem_two::BOOL(elem_bool)::restOfStack, env ->
	  if elem_bool == true then processor restOfComs (elem_one::restOfStack) env
	  else processor restOfComs (elem_two::restOfStack) env
	| IF::restOfComs, elem_one::elem_two::NAME(elem_name)::restOfStack, env ->
	  (match lookup elem_name env with
	  | None -> processor restOfComs (ERROR::elem_one::elem_two::NAME(elem_name)::restOfStack) env
	  | Some value -> (match value with | BOOL(some_bool) -> 
	    if some_bool == true then processor restOfComs (elem_one::restOfStack) env
	    else processor restOfComs (elem_two::restOfStack) env
	    | anything_else -> processor restOfComs (ERROR::elem_one::elem_two::NAME(elem_name)::restOfStack) env))	  
	| BIND::restOfComs, INT(top_elem)::NAME(elem_name)::restOfStack, env -> processor restOfComs (UNIT::restOfStack) ((NAME(elem_name), INT(top_elem))::env)
	| BIND::restOfComs, STRING(top_elem)::NAME(elem_name)::restOfStack, env -> processor restOfComs (UNIT::restOfStack) ((NAME(elem_name), STRING(top_elem))::env)
	| BIND::restOfComs, BOOL(top_elem)::NAME(elem_name)::restOfStack, env -> processor restOfComs (UNIT::restOfStack) ((NAME(elem_name), BOOL(top_elem))::env)
	| BIND::restOfComs, UNIT::NAME(elem_name)::restOfStack, env -> processor restOfComs (UNIT::restOfStack) ((NAME(elem_name), UNIT)::env)
	| BIND::restOfComs, NAME(top_elem)::NAME(elem_name)::restOfStack, env -> 
	  (match lookup top_elem env with 
	  	  | Some value -> processor restOfComs (UNIT::restOfStack) ((NAME(elem_name), value)::env)
	  	  | None -> processor restOfComs (ERROR::NAME(top_elem)::NAME(elem_name)::restOfStack) env)
	| LET::restOfComs, restOfStack, env -> processor restOfComs restOfStack env
	| END::restOfComs, restOfStack, env -> processor restOfComs restOfStack env
	| invalid::restOfComs, restOfStack, env -> processor restOfComs (ERROR::restOfStack) env in

processor command_list [] []

(* print_comList_as_strList test_comList_to_strList  *)

;;

interpreter ("test.txt", "output.txt")

