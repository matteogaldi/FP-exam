type 'a graph = ('a * 'a) list
let g = [(1,2);(1,3);(3,4);(4,2);(5,6)]
exception NotFound

let rec vicini nodo = function
		[] -> []
	| (x,y)::rest -> 
			if x=nodo then y::vicini nodo rest
			else if y=nodo then x::vicini nodo rest
				else vicini nodo rest

let rec successori_ nodo = function
		[] -> []
	| (x,y)::rest -> 
			if y=nodo then y::successori_ nodo rest
			else successori_ nodo rest

let successori nodo grafo = 
	List.map snd (List.filter (fun (x,y) -> x=nodo) grafo)


let search_path grafo start finish = 
	let rec from_node visited n = 
		if List.mem n visited
		then raise NotFound
		else if n=finish then [n]
			else n::from_list (n::visited) (vicini n grafo)
	and from_list visited = function
			[] -> raise NotFound
		| n::rest -> 
				try from_node visited n
				with NotFound -> from_list visited rest
	in from_node [] start


let search_path_tappa grafo start tappa finish =
	let rec from_node visited n = 
		if List.mem n visited
		then raise NotFound
		else if ((List.mem tappa visited) && (n=finish)) then [n]
			else n::from_list (n::visited) (vicini n grafo)
	and from_list visited = function
			[] -> raise NotFound
		| n::rest -> 
				try from_node visited n
				with NotFound -> from_list visited rest
	in from_node [] start