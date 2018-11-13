open Prop_def;;
 
open List;;
open Prop_lexer;;


let rec fnc  term =
	match term with
		(Var x) 		 ->  
						 print_string x
		|Vrai 			 ->      
						 print_string "vrai";
		|Faux  			 ->     
						 print_string "faux";
		|OU(x,y) 		 ->   (
             print_string "(";
             print_term x;
             print_string "#";
             print_term y;
             print_string ")"; 
						)             
		|ET(x,y)  	 ->	(
             print_string "("; 
             print_term x;
             print_string "&";
             print_term y;
             print_string ")"; 
						)             
		|IMPLIQ(x,y) -> (
             print_string "("; 
             print_term x;
             print_string "->";
             print_term y;
             print_string ")";
						)             
		|NEG x			 -> (
						 print_string "~"; 
						 print_term x
						)
;;
 

let boucle in_channel =
	let lexbuffer = Lexing.from_channel in_channel in
		let lire_prop_expr () = 
		Prop_parser.programme Prop_lexer.token lexbuffer in
			let p = lire_prop_expr () in 
				let k p = 			
			  print_string "\n\t**********\n";
				print_term p;
		  	print_string "\n\t**********\n";	in 
					k p ;
	exit 0;;

boucle stdin;;




