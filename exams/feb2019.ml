type color = Rosso | Verde | Neutro
type 'a graph = ('a * 'a) list
let cols = [(2,Rosso); (3,Verde); (4,Verde); (6,Verde); (7,Rosso)]
let lst = [1;2;3;4;5;6;7;8;9;10]

let rec aggiungi col = function
	| [] -> []
	| (c, n)::rest ->
		if c=col then (c, n+1)::rest
		else (c, n)::aggiungi col rest

let conta_colori cols lst = 
	let rec aux result = function
		| [] -> result
		| x::rest ->
			try
				let temp = List.assoc x cols
				in aux (aggiungi temp result) rest
			with _ -> aux (aggiungi Neutro result) rest 
	in aux ([(Rosso, 0); (Verde, 0); (Neutro, 0)]) lst


let rec vicini nodo = function
	| [] -> []
	| (x, y)::rest -> 
		if nodo=x then y::vicini nodo rest
		else if nodo=y then x::vicini nodo rest
		else vicini nodo rest


let rec drop y = function
	| [] -> []
	| x::rest -> 
		if y=x then rest
		else x::drop y rest

let g = [(1,2);(1,3);(2,3);(2,5);(3,4);(3,5);(4,5);(5,6);(5,7);(6,7);(7,8)]
let c= [(1,Neutro);(5,Neutro);(8,Neutro);(2,Rosso);(7,Rosso);(3,Verde);
           (4,Verde);(6,Verde)]
let lst = [Rosso;Verde;Neutro]
exception NotFound
let rec assoc x lst = 
	try
		List.assoc x lst
	with _ -> raise NotFound

let path (g: 'a graph) cols col_path start =
	let rec from_node visited path n = 
		if List.mem n visited then raise NotFound
		else if path=[(assoc n cols)] then [n]
		else
			let new_list = if (List.hd path)=(assoc n cols) then drop (List.hd path) path
						   else path
			in n::from_list (n::visited) new_list (vicini n g)
	and from_list visited path = function
		| [] -> raise NotFound
		| n::rest -> 
			try from_node visited path n
			with NotFound -> from_list visited path rest
	in from_node [] col_path start
(*
#use "feb2019.ml";;
*)