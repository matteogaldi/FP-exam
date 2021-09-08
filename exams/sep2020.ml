let rec remove x = function
| [] -> []
| y::rest -> 
	if y=x then remove x rest
	else y::remove x rest

let remove_it x lst = 
	List.filter (fun y -> y<>x) lst

let remove_it2 x lst = 
	if List.length lst=0 then lst
else
	let rec aux result i y = 
		if i<0 then result
		else if y=x then aux result (i-1) (List.nth lst i)
		else aux (y::result) (i-1) (List.nth lst i)
	in aux [] ((List.length lst)-1) (List.hd (List.rev lst))

let remove_it3 x lst = 
	let rec aux temp = function
	| [] -> temp
	| y::rest ->
		if y=x then aux temp rest
		else aux (temp@[y]) rest
	in aux [] lst 

type 'a graph = ('a * 'a) list
exception NotFound

let rec successori nodo = function
	| [] -> []
	| (x,y)::rest -> 
		if nodo=x 
		then y::(successivo nodo rest)
		else successivo nodo rest

let percorso g start tappa target = 
	let rec from_node visited n = 
		if List.mem n visited
		then raise NotFound
		else if n=target && (List.mem tappa visited || n=tappa) then [n]
			else n::from_list (n::visited) (successori n g)
			and from_list visited = function
			| [] -> raise NotFound
			| n::rest -> 
				try from_node visited n
				with NotFound -> from_list visited rest
	in from_node [] start


let grafo = [(1,2); (1,3); (1,4); (2,6); (3,5); (4,6); (5,4); (6,5); (6,7)]
(*
#use "sep2020.ml";;
*)
