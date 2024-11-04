(*Tri deux partie d'un même tableau entre les valeurs i et j
 et j+1 et k*)
let merge(tab,i,j,k : int array * int *int *int): unit =
  let taille : int = (k-i+1) in
  let tab_int : int array = Array.make taille tab.(i) in
  let l : int ref = ref i in
  let h : int ref = ref (j+1) in 
  for p = 0 to (taille - 1)  do
    (
      if tab.(!l) < tab.(!h) && (!l) <> (j+1)
      then 
        (
          tab_int.(p) <- tab.(!l);
          l:= !l +1
        )
        else
          (
            tab_int.(p) <- tab.(!h);
            h:= !h + 1
          )
    )
  done 
;;

(*Tri un tableau avec l'algorithme du tri fusion*)
let rec merge_sort_rec(tab, i ,j  : int array * int * int): unit = 
  if Array.length tab <= 1
  then ()
  else 
    (
      let taille : int = (i+j)/2 in
      merge_sort_rec(tab,0,taille);
      merge_sort_rec(tab,taille+1,j);
      merge(tab,i,taille,j) 
    ) 
;;

let merge_sort (tab : int array):unit =
  let len : int = Array.length tab in 
  merge_sort_rec(tab,0,len)
;;