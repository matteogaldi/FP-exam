let rec some_all p = function
	[] -> failwith "some_all"
	|x::rst -> if List.for_all p x then x
		else some_all p rst;;

type path_form = AE of string | EA of string;;

type 'a ntree = Tr of 'a * 'a ntree list;;

let rec verify pform = function
	Tr(r, []) -> (match pform with
		AE x | EA x -> List.mem x r)
	|Tr(r, tlist) -> (match pform with
		AE x -> List.mem x r || verify_list pform tlist
		|EA x -> List.mem x r && verify_list pform tlist)
	and verify_list pform = function
		[] -> false
		|[t] -> verify pform t
		|t::rst -> (match pform with
			AE x -> verify pform t || verify_list pform rst
			|EA x -> verify pform t && verify_list pform rst);;