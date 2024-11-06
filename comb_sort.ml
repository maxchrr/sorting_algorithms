let swap a x1 x2 =
	let tmp = a.(x1) in
	a.(x1) <- a.(x2);
	a.(x2) <- tmp

let comb_sort a =
	let r = ref (Array.length a) in
	let state = ref true in
	while !r > 1 && !state do
		r := int_of_float ((float_of_int !r) /. 1.3);
		if !r < 1 then r := 1;
		let i = ref 0 in
		state := false;
		while !i < (Array.length a) - !r do
			if a.(!i) > a.(!i + !r) then begin
				swap a !i (!i + !r);
				state := true
			end;
			i := !i+1
		done
	done
