let swap a x1 x2 =
	let tmp = a.(x1) in
	a.(x1) <- a.(x2);
	a.(x2) <- tmp

let bubble_sort a =
	let len = Array.length a in
	for i=len-1 downto 1 do
		for j=0 to i-1 do
			if a.(j+1) < a.(j) then swap a (j+1) j
		done;
	done
