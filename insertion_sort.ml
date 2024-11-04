let insertion_sort a =
	let len = Array.length a in
	for i=1 to len-1 do
		let x = a.(i) in
		let j = ref i in
		while !j > 0 && x < a.(!j-1) do
			a.(!j) <- a.(!j-1);
			j := !j-1
		done;
		a.(!j) <- x;
	done
