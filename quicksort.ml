let swap a x1 x2 =
	let tmp = a.(x1) in
	a.(x1) <- a.(x2);
	a.(x2) <- tmp

let partition a l r =
    let p = a.(l) in
    let m = ref l in
    for i=l+1 to r-1 do
      	if a.(i) <= p then begin
			swap a !m i;
			m := !m+1
		end
    done;
    if l <> !m then swap a l !m;
    !m

let rec quick_rec a l r =
	if l < r then begin
		let p = partition a l r in
		quick_rec a l p;
		quick_rec a (p+1) r
	end

let quicksort a = quick_rec a 0 (Array.length a)
