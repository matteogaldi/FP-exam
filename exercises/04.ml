let rec length = (function
| [] -> 0
| _::rest -> 1 + length rest)

let length_it lst = 
	let rec aux len = (function
	| [] -> len
	| _::rest -> aux (len+1) rest)
	in aux 0 lst

let rec sumof = (function
| [] -> 0
| x::rest -> x + sumof rest)

let sumof_it lst = 
	let rec aux sum = (function
	| [] -> sum
	| x::rest -> aux (sum+x) rest)
	in aux 0 lst

let rec maxlist = function
| [] -> failwith "qualcosa"
| [x] -> x
| x::rest -> max x (maxlist rest)

let maxlist_it lst = 
	let rec aux tmp = function
	| [] -> tmp
	| x::rest -> aux (max tmp x) rest
	in aux 0 lst

let rec drop n = function
| [] -> []
| _::rest as lst -> 
	if n>0 then drop (n-1) rest
	else lst

let rec append lst1 lst2 =
	match lst1 with
	| [] -> lst2
	| x::rest -> x::append rest lst2

let rec reverse = (function
| [] -> []
| x::rest -> (reverse rest)@[x])

let reverse_it lst = 
	let rec aux tmp = (function
	| [] -> tmp
	| x::rest -> aux (x::tmp) rest)
	in aux [] lst


let rec nth n = (function
| [] -> failwith "qualcosa"
| x::rest -> if n=0 then x else nth (n-1) rest)


(* copy n x -> lista con x ripetuto n volte*)
let rec copy n x = 
	if n=0 then []
	else x::(copy (n-1) x)


let rec nondec = (function
| [] | [_]-> true
| x::y::rest -> (x <= y) && nondec rest)


let rec pairwith y = (function
| [] -> []
| x::rest -> (y,x):: (pairwith y rest))


let rec duplica = (function
| [] -> []
| x::rest -> x::x::(duplica rest))


let enumera lst = 
	let rec aux count = function
	| [] -> []
	| x::rest -> (count, x):: aux (count+1) rest
	in aux 0 lst


let rec alternate = function
	[] | [_] -> []
	| _::y::rest -> y::alternate rest


let rec min_dei_max = (function
| [] -> failwith "min_dei_max"
| [lst] -> maxlist_it lst
| lst::rest -> min (maxlist lst) (min_dei_max rest))