open Prop_def;;
 
open List;;
open Prop_lexer;;
 

let boucle in_channel =
	let lexbuffer = Lexing.from_channel in_channel in
		let lire_prop_expr () = 
		Prop_parser.programme Prop_lexer.token lexbuffer in
			let p = lire_prop_expr () in 
				let k a= 			
			  	print_string "\n\t**********\n"; 						in k (print_term p) ;
	exit 0;;

boucle stdin;;


