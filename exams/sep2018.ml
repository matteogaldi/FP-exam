let rec is_sorted = function
		[] | _::[] -> true
	| x::y::rest -> x<=y && is_sorted (y::rest)

exception NotFound
type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
let t = Tr(3,
           Tr(4,Tr(3,Empty,Empty),Tr(16,Tr(15,Empty,Empty),Empty)),
             Tr(6,Tr(9,Tr(13,Tr(15,Empty,Empty),Empty),Tr(-19,Empty,Empty)),Empty))

let sorted_branch t x =
	let rec aux path = function
			Empty -> raise NotFound
		| Tr(r, Empty, Empty) -> 
				if r=x && is_sorted (path@[r]) then [r]
				else raise NotFound
		| Tr(r, tleft, tright) ->
				if is_sorted (path@[r])
				then r::(
					try aux (path@[r]) tleft
					with NotFound -> aux (path@[r]) tright
				)
				else raise NotFound
	in aux [] t

type 'a graph = ('a * 'a) list
let rec successori nodo = function
    [] -> []
  | (x,y)::rest -> 
      if x=nodo then y::successori nodo rest
      else successori nodo rest

let _succ nodo grafo = 
  List.map snd (List.filter (fun (x,y) -> x=nodo) grafo)


let sorted_path g start goal = 
  let rec from_node visited path n = 
    if List.mem n visited then raise NotFound
    else if n=goal && is_sorted (path@[n]) then [n]
    else n::from_list (n::visited) (path@[n]) (successori n g)
  and from_list visited path = function
      [] -> raise NotFound
    | n::rest -> 
        try from_node visited path n
        with NotFound -> from_list visited path rest
  in from_node [] [] start

let grafo = [(1,2); (2,3); (3,2); (3,5); (5,1); (5,6); (6,4)];;
(*
#use "sep2018.ml";;
*)