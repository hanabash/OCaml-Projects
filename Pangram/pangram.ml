let pangram (inFile : string) (outFile : string) : unit =


  let ic = open_in inFile in

  let oc = open_out outFile in 

  let rec loop_read acc =
      try 
          let l = input_line ic in loop_read (l::acc)
      with
      | End_of_file -> List.rev acc in

  let file_write bool_val = Printf.fprintf oc "%b\n" bool_val in

  let ls_str = loop_read [] in

  let characters = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 
                   'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'] in

  let rec check_pangram s charList = 
  match charList with
  [] -> file_write true
  | x::xs -> 
      if(String.contains s x) then
        check_pangram s xs
      else
        file_write false in 

  let rec traverse str_list =
  match str_list with
  [] -> ()
  | y::ys -> check_pangram y characters; traverse ys in

  traverse ls_str

  ;;

pangram "input.txt" "output.txt"
