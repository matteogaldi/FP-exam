exception NotFound

let rec find p = function
	| [] -> raise NotFound
	| x::rest -> 
		if p x then x 
		else find p rest


let rec takewhile p = function
	| [] -> []
	| x::rest ->
		if p x then x::takewhile p rest
		else takewhile p rest


let rec dropwhile p = function
	| [] -> []
	| x::rest -> 
		if not (p x) then x::dropwhile p rest
		else dropwhile p rest


let rec partition p = function
	| [] -> ([],[])
	| x::rest -> 
		let (yes, no) = partition p rest
		in if p x then (x::yes, no)
		else (yes, x::no)


let pairwith y lst = 
	List.map (fun x -> (y,x)) lst


let verifica_matrice n mat = 
	List.exists (List.for_all (fun x -> x<n)) mat


let setdiff set_a set_b = 
	List.filter (fun x -> not (List.mem x set_b)) set_a


let subset set1 set2 = 
	List.for_all (fun x -> List.mem x set2) set1


let duplica lst = 
	List.map (fun x -> x*2) lst


let map_cons lst b = 
	List.map (fun (y,l) -> (y, b::l)) lst


let duplica_matrice lst =
	List.map (fun x -> 
		List.map (fun y -> y*2) x) lst

let cons x rest = x::rest
let rec tutte_liste_con n x y = 
	if n=0 then [[]]
	else let tmp = tutte_liste_con (n-1) x y
	in (List.map (cons x) tmp) @ (List.map (cons y) tmp)


let rec _map f = function
	| [] -> []
	| x::rest -> (f x)::_map f rest


let labirinto = 
(
	5, 
	[((1,0),"oro"); 
	((3,1),"oro"); 
	((4,3),"oro"); 
	((0,1),"argento"); 
	((2,4),"argento"); 
	((0,2),"mostro"); 
	((1,1),"mostro"); 
	((1,3),"mostro"); 
	((2,3),"mostro"); 
	((3,0),"mostro"); 
	((4,2),"mostro")]
)

 
let in_riga (_, lab) riga valore = 
	List.exists (fun ((r,_),v) -> r=riga && v=valore) lab


let trova_colonna (_, lab) riga valore = 
	snd(fst(List.find (fun ((r,_),v) -> r=riga && v=valore) lab))


(*let rec _find x = function
| [] -> raise NotFound
| y::rest -> 
	if y=x then ([], rest)
	else let (prima, dopo) = find x rest
	in (y::prima, dopo)


let spezza x lst = 
	_find x (snd (find x lst))
*)

let rec prendi p = function
| [] -> raise NotFound
| x::rest -> 
	if p x then (x, rest)
	else let (y, result) = prendi p rest
	in (y, x::result)