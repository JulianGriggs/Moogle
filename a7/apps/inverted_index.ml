open Future
open Util
open Sequence
open Map

(*module PSeq = Sequence.Seq(PFuture)(struct let use_mpi = true end)*)
module PSeq = ListSeq
(* module Seq = ListSeq.Make() *)
(* module Seq = ParaSeq.Make() *)

module Dict = Map.Make(String)

(* inverted_index computes an inverted index for the contents of * a given *)
(* file. The filename is the given string. * The results are output to     *)
(* stdout.                                                                 *)
let build_one_index (d:document) : string list Dict.t =
	let id = string_of_int d.id in
	let contents = d.contents in
	let low_contents = String.lowercase contents in
	let words = split_words low_contents in
	let array = Array.of_list words in
	let seq = PSeq.seq_of_array array in
	let map_dicts = PSeq.map (fun x -> Dict.singleton x [id]) seq in
	(* for this function, inputs are guaranteed to have the same id *)
	let dict_merge (key) (first:string list option) (second:string list option) : string list option = 
		match first,second with
		| None, None -> None
		| None, s -> s
		| f, None -> f
		| f, s -> f 
	in 
	PSeq.reduce (fun x y -> Dict.merge dict_merge x y) (Dict.empty) map_dicts 

let mkindex (args : string ) : unit = 
	let documents = load_documents args in 
	let array = Array.of_list documents in
	let seq = PSeq.seq_of_array array in
	let map_dicts = PSeq.map build_one_index seq in
	(* for this function, inputs are guaranteed to be disjoint *)
	let dict_merge (key) (first:string list option) (second:string list option) : string list option = 
		match first,second with
		| None, None -> None
		| None, s -> s
		| f, None -> f
		| Some f, Some s -> Some (f @ s)
	in		
	let reduce_dicts = PSeq.reduce (fun x y -> Dict.merge dict_merge x y) (Dict.empty) map_dicts in
(*	let print_format = Dict.map (fun x -> (x, Dict.find x reduce_lists)) reduce_lists in *)
	let print_format = Dict.fold (fun key a b -> (key, a) :: b) reduce_dicts [] in
	print_reduce_results print_format
	
let tests () = 
	mkindex "profiles.txt"




