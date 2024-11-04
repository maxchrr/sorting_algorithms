let bubble_sort a =
	let len = Array.length a in
	for i=len-1 downto 1 do
		for j=0 to i-1 do
			if a.(j+1) < a.(j) then
				let tmp = a.(j+1) in
				a.(j+1) <- a.(j);
				a.(j) <- tmp
		done;
	done
