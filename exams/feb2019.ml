type color = Rosso | Verde | Neutro
let cols = [(2,Rosso); (3,Verde); (4,Verde); (6,Verde); (7,Rosso)]
let lst = [1;2;3;4;5;6;7;8;9;10]

let conta_colori cols lista = 
	let rec aux col_list = function
			[] -> []
		| (x,c)::rest -> 
				if List.mem x lista 
				then (x,c)::aux ((x,c)::col_list) rest
				else aux col_list rest
	in aux [] cols


(*
#use "feb2019.ml";;
*)