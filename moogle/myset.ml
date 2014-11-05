(* Definitions for sets. *)

exception TODO

(* An interface for set modules *)
module type SET = 
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* chooses some member from the set, removes it 
   * and returns that element plus the new set.  
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a

  (* functions to convert our types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string

  (* runs our tests. See TESTING EXPLANATION *)
  val run_tests : unit -> unit
end



(* parameter to Set modules -- we must pass in some 
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module type COMPARABLE = 
sig
  type t
  val compare : t -> t -> Order.order
  val string_of_t : t -> string

  (* The functions below are used for testing. See TESTING
   * EXPLANATION *)

  (* Generate a value of type t. The same t is always returned *)
  val gen : unit -> t

  (* Generate a random value of type t. *)
  val gen_random : unit -> t

  (* Generate a t greater than the argument. *)
  val gen_gt : t -> unit -> t

  (* Generate a t less than the argument. *)
  val gen_lt : t -> unit -> t

  (* Generate a t between the two arguments. Return None if no such
   * t exists. *)
  val gen_between : t -> t -> unit -> t option
end



(* An example implementation of our COMPARABLE signature. Use this
 * struct for testing. *)
module IntComparable : COMPARABLE =
struct
  open Order
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
  let gen_lt x () = x - 1
  let gen_between x y () = 
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end



(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) = 
struct
  open Order
  type elt = C.t 
  type set = elt list

  (* INVARIANT: sorted, no duplicates *)
  let empty = []
  let is_empty xs = 
    match xs with 
      | [] -> true
      | _ -> false
  let singleton x = [x]
  let rec insert x xs = 
    match xs with 
      | [] -> [x]
      | y::ys -> (match C.compare x y with 
          | Greater -> y::(insert x ys)
          | Eq -> xs
          | Less -> x::xs)

  let union xs ys = List.fold_right insert xs ys
  let rec remove y xs = 
    match xs with 
      | [] -> []
      | x::xs1 -> (match C.compare y x with 
          | Eq -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys = 
    match xs, ys with 
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> (match C.compare xh yh with 
          | Eq -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x = 
    match xs with 
      | [] -> false
      | y::ys -> (match C.compare x y with
          | Eq -> true
          | Greater -> member ys x
          | Less -> false)

  let choose xs = 
    match xs with 
      | [] -> None
      | x::rest -> Some (x,rest)
  let fold f e = List.fold_left (fun a x -> f x a) e 
    
  let string_of_elt = C.string_of_t
  let string_of_set (s: set) : string = 
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left f "" s) ^ "])"


  (****************************************************************)
  (* Tests for our ListSet functor                                *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: set) (lst: elt list) : set = 
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    ()

  let test_union () =
    ()

  let test_intersect () =
    ()

  let test_member () =
    ()

  let test_choose () =
    ()

  let test_fold () =
    ()

  let test_is_empty () =
    ()

  let test_singleton () =
    ()

  let run_tests () = 
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end


(******************************************************************)
(* DictSet: a functor that creates a SET by calling our           *)
(* Dict.Make functor                                              *)
(******************************************************************)

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) = 
struct
  module D = Dict.Make(struct
		type key = C.t
		type value = bool
		let compare = C.compare
    let string_of_key = C.string_of_t
    let string_of_value = string_of_bool

    (* These functions are for testing purposes *)
    let gen_key () = C.gen ()
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = C.gen_between x y ()
		let gen_value = (fun () -> true)
    let gen_pair () = (gen_key(),gen_value())
  end)
	
	open Order
  type elt = D.key
  type set = D.dict
	
	(* INVARIANT: sorted, no duplicates *)
  let empty = D.empty
  let is_empty d = 
		D.fold (fun _ _ size -> size + 1) 0 d = 0
  let singleton k = D.insert D.empty k true
  let insert k d = D.insert d k true
  let union d1 d2 = D.fold (fun k v _ -> D.insert d2 k v) D.empty d1
  let remove k d = D.remove d k
  let intersect d1 d2 = 
		D.fold (fun k v d -> 
			if D.member d2 k then D.insert d k v else d) D.empty d1

  let member d k = D.member d k 

  let choose d = 
		match D.choose d with 
		| None -> None
		| Some (k,_,set) -> Some (k,set)
  
	(*val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a*)
 (* val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a *)

  let fold f b = 
		let f2 = (fun k _ d' -> f k d') in
		D.fold f2 b

  let string_of_elt = D.string_of_key
  let string_of_set s = D.string_of_dict s

  (****************************************************************)
  (* Tests for our DictSet functor                                *)
  (* Use the tests from the ListSet functor to see how you should *)
  (* write tests. However, you must write a lot more              *)
  (* comprehensive tests to test ALL your functions.              *)
  (****************************************************************)

	(* testing helper method to compute size of a set *)
  let size (set:set) : int = 
    let rec aux (s:set) (acc:int) =
      match D.choose s with
      | None -> acc
      | Some (_, _, d') -> aux d' (acc + 1)
    in
    aux set 0

(*
  let generate_random_dict (size: int) : set =
    let rec aux (size: int) (d: set) = 
			if size <= 0 then d
    	else aux (size - 1) (D.insert d (C.gen_random()) true)
		in aux size D.empty *)
		
	(* testing helper method to create a set with elements 1,2,3...,size *)
	let generate_consecutive_dict (size: int) : set =
    let rec aux (size: int) (d: set) (inc : C.t) = 
			let inc' = ((C.gen_gt inc) ()) in
			if size <= 0 then d
    	else aux (size - 1) (D.insert d inc' true) inc'
		in aux size D.empty (C.gen())
	
	(* generate a random list of a given size *)
  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

	(* test the insert method *)
  let test_insert () =
		let i = C.gen_random() in

    (* test inserting into empty set *)
    let s1 = insert i empty in
    assert(D.member s1 i) ;
    assert(size s1 = 1);

    (* test that size of empty is 0*)
    assert(size empty = 0);

    (* test that all elements in a randomly generated list are placed in the set *)
    let randLs = generate_random_list 100 in
    let s2 = List.fold_left (fun dict element -> insert element dict) empty randLs in
    assert ((List.fold_left (fun dict element -> assert(D.member dict element); dict) s2 randLs) = s2);

    (* test to make sure that inserting the same element twice doesn't change size of list *)
    let s3 = insert i empty in
    let s3' = insert i s3 in
    assert (size s3' = 1);
    assert (D.member s3' i);
    ()

  (* test the remove method *)
  let test_remove () =
		let zero = C.gen() in 
		let one = C.gen_gt zero () in
		let i = C.gen_random() in
		
		(* test removing from empty set *)
		assert(remove (zero) D.empty = D.empty) ;
		
		(* s1 : {1} *)
		let s1 = D.insert D.empty i true in
		
		assert(remove i s1 = D.empty) ;
		assert(remove (C.gen_gt i ()) s1 = s1) ;
		
		(* s2 : {1,2,3...100} *)
		let s2 = generate_consecutive_dict 100 in 
		
		assert (remove zero s2 = s2) ;
		assert (size (remove one s2) = 99) ; 
		assert (not (D.member (remove one s2) one)) ; 
    ()

  let test_union () =
    ()

  let test_intersect () =
    ()

  let test_member () =
    ()

  let test_choose () =
    ()

  let test_fold () =
    ()

  let test_is_empty () =
		
		assert(is_empty empty) ; 
		
		let i = C.gen_random() in
		let s1 = D.insert D.empty i true in
		
		assert(not (is_empty s1)) ;
		assert(is_empty (D.remove s1 i)) ;
    ()

  let test_singleton () =
    let i = C.gen_random() in

    let s = singleton i in
    (* test that the size of a singleton is 1 *)
    assert (size s = 1); 
    (* test that the item added to the singleton is indeed in it *)
    assert (D.member s i);
    (* test that singleton's with the same item are indeed equal *)
    let s' = singleton i in
    assert(s = s');

    (* test that singleton's with the same item are separate structures *)
    assert(s != s');
    ()

  (* add your test functions to run_tests *)
  let run_tests () = 
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()
end




(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a set of ints using our ListSet functor. *)
module IntListSet = ListSet(IntComparable) ;;
IntListSet.run_tests();;

(* Create a set of ints using our DictSet functor
 * 
 * Uncomment out the lines below when you are ready to test your
 * 2-3 dict set implementation *)

module IntDictSet = DictSet(IntComparable) ;;
IntDictSet.run_tests();;



(******************************************************************)
(* Make: a functor that creates a SET by calling our              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) = 
  (* Change this line to use our dictionary implementation when your are 
   * finished. *)
  (*ListSet (C)*)
  DictSet (C) 

