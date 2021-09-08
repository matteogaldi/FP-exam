type 'a graph = ('a * 'a ) list;;
type 'a option = Some of 'a | None;;

let check x = function
	Some y -> y = x
	|None -> true;;

let successori n g =
	List.map snd (List.filter (function (x,y) -> x = n) g);;

let which_path g pl start goal =
	let rec from_node n visited pl =
		if pl = [] then
			if List.mem n visited then failwith "path"
			else if n = goal then [n]
			else n::from_list (n::visited) pl (successori n g) 
		else if check n (List.hd pl) && n = goal then [n]
		else if check n (List.hd pl) then
			n::from_list (n::visited) (List.tl pl) (successori n g)
		else failwith "path"
		and from_list visited pl = function
			[] -> failwith "path"
			|x::rst -> try from_node x visited pl 
				with _ -> from_list visited pl rst
	in from_node start [] pl;;

let g = [(1,3); (3,4); (4,2); (1,2); (2,5); (5,5); (4,3)];;

