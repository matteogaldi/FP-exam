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

(*
#use "sep2019.ml";;
*)
