type 'a ntree = Tr of 'a * 'a list

let rec size (Tr(_, tlist)) = 
	1 + sumof (List.map size tlist)