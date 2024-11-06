let swap a x1 x2 =
	let tmp = a.(x1) in
	a.(x1) <- a.(x2);
	a.(x2) <- tmp

let cocktail_sort a =
	let swapped = ref true in
	while !swapped do
		swapped := false;
		for i=0 to Array.length a - 2 do
			if a.(i) > a.(i+1) then begin
				swap a i (i+1);
				swapped := true
			end
		done;
		for i=Array.length a - 2 downto 0 do
			if a.(i) > a.(i+1) then begin
				swap a i (i+1);
				swapped := true
			end
		done
	done
		
