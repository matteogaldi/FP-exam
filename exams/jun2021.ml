exception NotFound
type 'a graph = ('a * 'a) list
let g = [(1,2); (1,4); (2,6); (3,5); (4,6); (5,4); (6,5); (6,7)]

let successori nodo grafo = 
	List.map snd (List.filter (fun (x,y) -> x=nodo) grafo)

(*
from_node: 'a list -> int -> 'a -> 'a list
from_list: 'a list -> 'a -> 'a list
*)
let depth_limited g start goal depth = 
	let rec from_node visited depth x = 
		if List.mem x visited || depth<0 then raise NotFound
		else if x=goal then [x]
		else x::from_list (x::visited) (depth-1) (successori x g)
	and from_list visited d = function
			[] -> raise NotFound
		| n::rest ->
				try from_node visited d n
				with NotFound -> from_list visited d rest
	in from_node [] depth start 


let path g start goal maxdepth =
	let rec aux actual max = 
		if actual>max then raise NotFound
		else
			try
				depth_limited g start goal actual
			with NotFound -> aux (actual+1) max
	in aux 0 maxdepth
(*
#use "jun2021.ml";;
*)