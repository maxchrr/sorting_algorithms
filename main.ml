#use "insertion_sort.ml";;
#use "selection_sort.ml";;
#use "bubble_sort.ml";;

let fill_new_array n =
	let a = Array.make n 0 in
	for i = 0 to n-1 do
		a.(i) <- Random.int n
	done;
	a

let create_random_array () =
	Random.self_init ();
	let len = Random.int 100 in
	fill_new_array len

let time f x =
	let current_time = Sys.time () in
	let fx = f x in
	Printf.printf "Execution time: %.3fms\n" ((Sys.time () -. current_time) *. 1000.);
	fx
;;

time insertion_sort (create_random_array ());;
time selection_sort (create_random_array ());;
time bubble_sort (create_random_array ());;
