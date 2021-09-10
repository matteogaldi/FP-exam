let times = [25;30;45;45]

let rec durations_to_end_times = function
		[] -> []
	| x::[] -> [x]
	| x::y::rest -> x::durations_to_end_times ((y+x)::rest)


type 'a graph = ('a * 'a) list
type 'a money = ('a * int) list
exception NotFound


let refresh_money wallet n money =
	try (money + (List.assoc n wallet))
	with _ -> money

let rec vicini nodo = function
		[] -> []
	| (x,y)::rest -> 
			if x=nodo then y::vicini nodo rest
			else if y=nodo then x::vicini nodo rest
			else vicini nodo rest 

let safe_path g wallet start goal init =
	let rec from_node visited money n = 
		if List.mem n visited || (refresh_money wallet n money)<0 then raise NotFound
		else if n = goal then [n]
		else n::from_list (n::visited) (refresh_money wallet n money) (vicini n g)
	and from_list visited money = function
			[] -> raise NotFound
		| x::rest -> 
				try from_node visited money x
				with NotFound -> from_list visited money rest
	in from_node [] init start


let grafo = [('A', 'B'); ('A', 'C'); ('A', 'D'); ('B', 'E'); ('D', 'F'); ('C', 'F'); ('C', 'E'); ('E', 'G'); ('F', 'G')];;
let wallet = [('C',-7);('D',-15);('F',3);('G',-5)];;
(*
#use "feb2018.ml";;
*)