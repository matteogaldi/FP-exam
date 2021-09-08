type metro = (int * int * string) list

let m = [(1,2,"A"); (2,3,"A"); (3,1,"A"); (2,4,"B"); (4,5,"B"); (4,6,"C"); (6,3,"C"); (5,7,"D"); (6,7,"D")];;

(*
normalizza: 'a list -> 'a list
normalizza lista = lista senza duplicati

aux: a' list -> 'a list -> 'a list
*)
let normalizza lst =
	let rec aux result = function
	| [] -> result
	| x::rest -> 
		if (List.mem x result)
		then  aux result rest
		else aux (result@[x]) rest
	in aux [] lst

let line m ln = 
	normalizza (List.flatten(List.map (fun (s1,s2,_) -> [s1;s2]) (List.filter (fun (_,_,s) -> s=ln) m)))


let vicini stazione m = 
	let rec aux = function
	| [] -> []
	| (s1,s2,l)::rest -> 
		if stazione = s1 then (s2, l) :: aux rest
		else (s1, l) :: aux rest
	in aux (List.filter (fun (s1, s2, _) -> s1=stazione || s2=stazione) m)

(*
#use "feb2020.ml";;
*)