let swap a x1 x2 =
	let tmp = a.(x1) in
	a.(x1) <- a.(x2);
	a.(x2) <- tmp

let selection_sort a =
	let len = Array.length a in
	for i=0 to len-2 do
		let min = ref i in
		for j=i+1 to len-1 do
			if a.(j) < a.(!min) then min := j
		done;
		if i <> !min then swap a i !min
	done
