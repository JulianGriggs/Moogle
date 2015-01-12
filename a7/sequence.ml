open System
open Future 
open Mpi


module type S = sig
  type 'a t
  val tabulate : (int -> 'a) -> int -> 'a t
  val seq_of_array : 'a array -> 'a t
  val array_of_seq : 'a t -> 'a array
  val iter: ('a -> unit) -> 'a t -> unit
  val length : 'a t -> int
  val empty : unit  ->'a t
  val cons : 'a -> 'a t -> 'a t
  val singleton : 'a -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val nth : 'a t -> int -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map_reduce : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'b -> 'a t -> 'b
  val reduce : ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a
  val flatten : 'a t t -> 'a t
  val repeat : 'a -> int -> 'a t
  val zip : ('a t * 'b t) -> ('a * 'b) t
  val split : 'a t -> int -> 'a t * 'a t
  val scan: ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a t
end



(*******************************************************)
(* Sequential Sequences Based on a List Representation *)
(*******************************************************)

module ListSeq : S = struct

  type 'a t = 'a list

  let length = List.length

  let empty () = []

  let cons (x:'a) (s:'a t) = x::s

  let singleton x = [x]

  let append = List.append

  let tabulate f n =
    let rec helper acc x =
      if x = n then List.rev acc
      else helper ((f x)::acc) (x+1) in
    helper [] 0

  let nth = List.nth

  let filter = List.filter

  let map = List.map

  let reduce = List.fold_left

  let map_reduce m r b s = reduce r b (map m s)

  let repeat x n =
    let rec helper x n acc =
      if n = 0 then acc else helper x (n-1) (x::acc) in
    helper x n []

  let flatten = List.flatten

  let zip (s1,s2) = List.combine s1 s2

  let split s i =
    let rec helper s i acc =
      match s,i with
        | [],_ -> failwith "split"
        | _,0 -> (List.rev acc,s)
        | h::t,_ -> helper t (i-1) (h::acc) in
    helper s i []

  let iter = List.iter

  let array_of_seq = Array.of_list

  let seq_of_array = Array.to_list

  let scan f b s = 
    let (_,xs) = List.fold_left (fun (v,ls) e -> let r = f v e in (r,r::ls)) (b,[]) s in
    List.rev xs

end


(*******************************************************)
(* Parallel Sequences                                  *)
(*******************************************************)

module type SEQ_ARGS = sig 
  val use_mpi: bool
end

module F = Future.PFuture

module Seq (Par : Future.S) (Arg : SEQ_ARGS) : S = struct

  type 'a t = 'a array


  let num_cores = System.cpu_count ()


  let flatten seqseq = 
		let list_of_seq = Array.to_list seqseq in
		Array.concat list_of_seq

	let print_array (arr:int array) = 
		Array.iter (fun x -> print_string (string_of_int x)) arr 

	let tabulate_chunk_debug (f:int->int) (num_chunk:int) (n:int) = 
		let chunk_size = n/num_cores + 1 in
		let start_i = num_chunk*chunk_size in
		let size = if start_i + chunk_size < n then chunk_size else n - start_i in
		Array.init size (fun i -> f (i + start_i))
	 

  let tabulate f n = 
		let chunk_size = n/num_cores + 1 in
		let tabulate_chunk (f:int->'a) (num_chunk:int) (n:int) = 
			let start_i = num_chunk*chunk_size in
			let size = if start_i + chunk_size < n then chunk_size 
				else (if n - start_i < 0 then 0 else n - start_i) in
			if (size < 0) then print_string (string_of_int start_i) else ();
			Array.init size (fun i -> f (i + start_i))
		in 
		let tabulate_each =
			Array.init num_cores (fun i -> F.future (tabulate_chunk f i) n) 
		in
		flatten (Array.map F.force tabulate_each)
		

  let seq_of_array a = a


  let array_of_seq seq = seq


  let iter f seq = Array.iter f seq


  let length seq = Array.length seq


  let empty () = [||]


  let cons elem seq = 
		Array.append [|elem|] seq


  let singleton elem = [|elem|]


  let append seq1 seq2 = Array.append seq1 seq2


  let nth seq i = seq.(i)


  let map f seq = 
		tabulate (fun i -> f seq.(i)) (Array.length seq)


  let map_reduce m r b seq = failwith "implement me"

		let reduce_chunk_debug (f:int->int->int) (base:int) (num_chunk:int) (n:int) seq =
			let length = Array.length seq in
			let chunk_size = length/num_cores + 1 in
			let start_i = num_chunk*chunk_size in
			let size = if start_i + chunk_size < n then chunk_size 
				else (if n - start_i < 0 then 0 else n - start_i) in
			let copy = Array.make size seq.(0) in
			Array.blit seq start_i copy 0 size;
			Array.fold_right f copy base

(*	let reduce_each_debug f base length seq = 
			Array.init num_cores (fun i -> F.future (reduce_chunk_debug f base i length seq) length)
*)
  
		
	let reduce (f:'a->'a->'a) (base:'a) seq = 
		let length = Array.length seq in
    if length = 0 then base 
		else if length < 2*num_cores then Array.fold_right f seq base 
    else 
  		let chunk_size = length/num_cores + 1 in
  		let reduce_chunk (f:'a->'a->'a) (num_chunk:int) (n:int) =
  			let start_i = num_chunk*chunk_size in
  			let size = 
					if start_i + chunk_size < n then chunk_size 
  				else (if n - start_i < 0 then 0 else n - start_i) in
				if size = 0 then failwith "Impossible"
				else if size = 1 then seq.(start_i) 
				else
  				let copy = Array.make (size-1) seq.(0) in
  				Array.blit seq start_i copy 0 (size-1);
  				Array.fold_right f copy seq.(start_i + size-1)
  		in
  		let reduce_each = 
  			Array.init num_cores (fun i -> F.future (reduce_chunk f i) length)
      in
      let temp1 = Array.map F.force reduce_each in
  		Array.fold_right f (temp1) (base)


  let repeat elem num = Array.copy (Array.make num elem)


  let zip (seq1,seq2) = 
		if Array.length seq1 = 0 || Array.length seq2 = 0
		then ([||]) 
		else 
			let length = if Array.length seq1 > Array.length seq2 
			then Array.length seq2 else Array.length seq1 in
			let zipped = Array.make length (seq1.(0),seq2.(0)) in
			let rec aux n = 
				match n with
				| 0 -> ()
				| _ -> zipped.(n-1) <- (seq1.(n-1),seq2.(n-1)); 
					aux (n-1)
			in aux length; 
			Array.copy zipped
		

  let split seq x = 
		let length = Array.length seq in
		if x >= length then failwith "split"
		else 
			let seq1 = Array.make x seq.(0) in
			let seq2 = Array.make (length - x) seq.(0) in 
			Array.blit seq 0 seq1 0 x; Array.blit seq x seq2 0 (length - x);
			(Array.copy seq1, Array.copy seq2)
		


  (*******************************************************)
  (* Parallel Prefix Sum                                 *)
  (*******************************************************)

  (* Here you will implement a version of the parallel prefix scan for a sequence 
   * [a0; a1; ...], the result of scan will be [f base a0; f (f base a0) a1; ...] *)
  let scan (f: 'a -> 'a -> 'a) (base: 'a) (seq: 'a t) : 'a t =
    failwith "implement me"
        
end







