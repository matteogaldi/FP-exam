let rec filquad ints quads =
match ints with
| [] -> []
| x::rest -> 
	if List.mem (x*x) quads then x::filquad rest quads
	else filquad rest quads

let filquad_f ints quads = 
	List.filter (fun x -> List.mem (x*x) quads) ints

let filquad_it (ints : int list) (quads : int list) = 
	let rec aux result i x = 
		if i<0 then result
		else if List.mem (x*x) quads then aux (x::result) (i-1) (List.nth ints (i-1))
	  else aux result (i-1) (List.nth ints i)
	in aux [] ((List.length ints)-1) (List.hd (List.rev ints))


type 'a ntree = Tr of 'a * 'a ntree list

let t = 
Tr(1, 
	[

	Tr(2, 
		[Tr(3,[]);Tr(4,[]);Tr(2,[])]);
	
	Tr(5, 
		[Tr(11,[]);Tr(10,[]);]);

	Tr(3, 
		[Tr(9,[]);Tr(7,[]);Tr(10,[]);]);

	]
)

let rec pesi (Tr(x, tlist)) = 
	match tlist with
	| [] -> [x]
	| _ -> 
		List.map ((+) x) (List.concat (List.map pesi tlist))


(*
#use "feb21.ml";;
*)