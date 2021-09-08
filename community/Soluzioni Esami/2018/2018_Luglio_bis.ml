(*Esercizio 3*)

type 'a ntree = Tr of 'a * ('a ntree list);;

exception NotFound

let leaf x = Tr(x,[]);;

let nalb = Tr(1,
	[Tr(2,
		[leaf 5;
		Tr(8,[Tr(9,[leaf 15]); leaf 10])]);
	Tr(3,[leaf 6; leaf 7; Tr(18,[leaf 65; leaf 4])]);
	Tr(14,[leaf 19; leaf 12; Tr(29, [leaf 13])])]);;

let rec path x1 = function
	|Tr(x,[])-> if (x=x1) then [x] else raise NotFound
	|Tr(x,rest)-> if (x=x1) then [x] else
			x::aux x1 rest
			and aux x1 = function
				[]-> raise NotFound
				|x::rest-> 
					(try
						path x1 x
						with
						| NotFound -> aux x1 rest);;

(*depth: 'a -> 'a ntree-> int*)

let depth x1 ntr = (List.length (path x1 ntr))-1;;

(*findtr: 'a -> 'a ntree -> 'a ntree*)

let rec findtr x1 = function
	Tr(x,[])-> if (x=x1) then Tr(x,[]) else raise NotFound
	|Tr(x,rest) -> if (x=x1) then Tr(x,rest) else aux x1 rest
	and aux x1 = function
		|[]-> raise NotFound
		|x::rest -> try
			findtr x1 x
		with
		| NotFound -> aux x1 rest;;

(*parentela: 'a ntree -> 'a -> 'a -> int*)

let rec parentela ntr start goal =
	let (lenstart,lengoal) = ((depth start ntr),(depth goal ntr))
	in (let rec find = function
			([],[])-> raise NotFound
			|(lstart,lgoal) -> if (List.mem goal lstart || List.mem start lgoal) 
				then abs(lenstart-lengoal)
					else if ((List.exists (fun z -> List.mem z lgoal) (List.tl lstart)) 
					|| (List.exists (fun z -> List.mem z lstart) (List.tl lgoal))) then
						
						(parentela (findtr (List.nth lstart ((min lenstart lengoal)-1)) ntr) start goal)

				else ((lenstart+lengoal))
		in find ((path start ntr),(path goal ntr)));;