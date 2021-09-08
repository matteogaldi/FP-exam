type expr =
Int of int
| Var of string
| Sum of expr * expr
| Diff of expr * expr
| Mult of expr * expr
| Div of expr * expr

let rec subexpr e1 e2 =
	e1=e2 ||
	match e1 with
    Sum(x,y) | Diff(x,y) | Mult(x,y) | Div(x,y) -> 
      subexpr x e2 || subexpr y e2
  | _ -> false


type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let my_tree = Tr(1, Tr(2, Tr(4, Empty, Empty), Tr(6, Empty, Empty)), Tr(3, Tr(5, Empty, Empty), Tr(7, Empty, Empty)))

let rec reflect = (function
| Empty -> Empty
| Tr(x, t1, t2) -> Tr(x, reflect t2, reflect t1))


let fulltree n = 
	let rec aux n k =
		if n=0 then Empty
		else Tr(k, aux (n-1) (2*k), aux (n-1) (2*k+1))
	in aux n 1


let rec conta_nodi = function
| Empty -> 0
| Tr(x, t1, t2) -> 1 + (conta_nodi t1) + (conta_nodi t2)


let rec heigth = function
| Empty -> 0
| Tr(_, t1, t2) -> 1 + max (heigth t1) (heigth t2)


let rec balanced = function
| Empty -> true
| Tr(_, t1, t2) -> abs (heigth t1 - heigth t2) <= 1 && balanced t1 && balanced t2


let rec preorder = function
| Empty -> []
| Tr(x, t1, t2) -> x::(preorder t1 @ preorder t2)


let rec postorder = function
| Empty -> []
| Tr(x, t1, t2) -> (postorder t1) @ (postorder t2) @ [x]


let rec inorder = function
| Empty -> []
| Tr(x, t1, t2) -> (inorder t1) @ (x::inorder t2)


let rec foglie_in_lista lst = function
| Empty -> true
| Tr(x, Empty, Empty) -> List.mem x lst
| Tr(_, t1, t2) -> foglie_in_lista lst t1 && foglie_in_lista lst t2

let rec num_foglie = function
| Empty -> 0
| Tr(x, Empty, Empty) -> 1
| Tr(_, t1, t2) -> num_foglie t1 + num_foglie t2


exception NotFound

let rec segui_bool lst tree = 
	match (tree, lst) with
		(Empty, _) -> raise NotFound
		| (Tr(x,_,_), []) -> x
		| (Tr(_, t1, t2), y::rest) -> 
			if y then segui_bool rest t1
			else segui_bool rest t2


let rec foglia_costo = function
| Tr(x, Empty, Empty) -> (x, x)
| Tr(x, left, Empty) -> 
	let (y, c) = foglia_costo left
	in (y, c+x)
| Tr(x, Empty, right) ->
	let (y, c) = foglia_costo right
	in (y, c+x)
| Tr(x, left, right) -> 
	let (yleft, cleft) = foglia_costo left in
	let (yright, cright) = foglia_costo right in
	if cleft >= cright then (yleft, cleft + x)
	else (yright, cright+x)


let rec stessa_struttura t1 t2 = 
	match (t1, t2) with
	| (Empty, Empty) -> true
	| (Tr(_,t1,t2),Tr(_,u1,u2)) -> 
			stessa_struttura t1 u1 && stessa_struttura t2 u2
	| _ -> false



let path p = function
| Empty -> []
| Tr

(*
#use "08.ml";;
*)

