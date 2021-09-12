type metro = (int * int * string) list
exception NotFound

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


let raggiungi (m: metro) maxc start goal = 
	let rec from_node visited linea cambi s = 
		if List.mem s visited || cambi>maxc then raise NotFound
		else if s=goal then [s]
		else s::from_list (s::visited) linea cambi (vicini s m)
	and from_list visited linea cambi = function
		| [] -> raise NotFound
		| (s, l)::rest -> 
			let new_cambi = if l=linea then cambi else cambi+1
			in
				try from_node visited l new_cambi s
				with NotFound -> from_list visited l new_cambi rest
	in from_node [] "" (-1) start

(*
#use "feb2020.ml";;
*)