type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
exception EmptyTree

let rec size = function
	Empty -> 0
	| Tr(_,t1,t2) -> 1 + size t1 + size t2

let is_empty t = t = Empty

let root t = function
	Tr(x,_,_) -> x 
	| _ -> raise EmptyTree

let left = (function
| Tr(_,t,_) -> t
| _ -> raise EmptyTree)

let right = (function
| Tr(_,_,t) -> t
| _ -> raise EmptyTree)

let is_leaf = (function
| Tr(x,Empty,Empty) -> true	
| _ -> false)

let rec height = (function
| Empty -> -1
| Tr(_, t1, t2) -> 1 + max (height t1) (height t2))

let rec exist p = (function
| Empty -> false
| Tr(x,t1,t2) -> 
	p x || exist p t1 || exist p t2

let rec raccogli = function
	Empty -> []
	| Tr(x, t1, t2) -> x::((raccogli t1)@(raccogli t2))