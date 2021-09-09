(*
drop: 'a -> 'a list -> 'a list
drop n lst: elimina tutte le occorrenze di n da lst
*)
let rec drop n = function
		[] -> []
	| x::rest -> 
			if n=x then drop n rest
			else x::drop n rest


let rec complemento superset = function
		[] -> superset
	| x::rest -> 
			if List.mem x superset 
			then complemento (drop x superset) (drop x rest)
			else failwith "complemento"


type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
let tree = Tr(5, Tr(2, Tr(3, Empty, Empty), Tr(4, Empty, Empty)), Tr(2, Tr(2, Empty, Empty), Tr(9, Empty, Empty)));;

(*
labels: 'a tree -> 'a list
*)
let rec labels = function
		Empty -> []
	| Tr(x, t1, t2) ->  x::(labels t1 @ labels t2)

(*
discendenti: 'a -> 'a tree -> 'a list
*)
let rec discendenti x = function
		Empty -> []
	| Tr(r, tl, tr) as t -> 
			if r=x then labels t
			else (discendenti x tl)@(discendenti x tr)
(*
#use "sep2019.ml";;
*)
