let selection_sort a =
	let len = Array.length a in
	for i=0 to len-2 do
		let min = ref i in
		for j=i+1 to len-1 do
			if a.(j) < a.(!min) then min := j
		done;
		if !min <> i then begin
			let tmp = a.(i) in
			a.(i) <- a.(!min);
			a.(!min) <- tmp
		end
	done
