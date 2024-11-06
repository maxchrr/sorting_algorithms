let merge a1 a2 l m r =
  let i = ref l in
  let j = ref (m+1) in 
  for k=l to r-1 do
    if a1.(!i) < a1.(!j) && (!i) <> (m+1) then begin
      a2.(k) <- a1.(!i);
      i := !i+1
    end else begin
      a2.(k) <- a1.(!j);
      j := !j+1
    end
  done
;;

let rec merge_rec a fst lst =
  let tmp = Array.copy a in
  if fst < lst-1 then begin
    let m = (fst+lst)/2 in
    merge_rec a fst m;
    merge_rec a (m+1) lst;
    merge tmp a fst m lst 
  end

let merge_sort a = merge_rec a 0 (Array.length a)
