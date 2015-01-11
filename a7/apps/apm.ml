
open Util
open Future

type profile = {
  firstname : string;
  lastname : string;
  sex : string;
  age : int;
  lo_agepref : int;
  hi_agepref : int;
  profession : string;
  has_children : bool;
  wants_children : bool;
  leisure : string;
  drinks : bool;
  smokes : bool;
  music : string;
  orientation : string;
  build : string;
  height : string
}

let convert (p : string) : profile =
  let s = String.concat " " (Str.split (Str.regexp_string "@") p) in
  Scanf.sscanf s "%s@ %s@ %s@ %d %d %d %s@ %B %B %s@ %B %B %s@ %s@ %s@ %s"
  (fun firstname lastname sex age lo_agepref hi_agepref profession has_children
       wants_children leisure drinks smokes music orientation build height ->
   { firstname = firstname;
     lastname = lastname;
     sex = sex;
     age = age;
     lo_agepref = lo_agepref;
     hi_agepref = hi_agepref;
     profession = profession;
     has_children = has_children;
     wants_children = wants_children;
     leisure = leisure;
     drinks = drinks;
     smokes = smokes;
     music = music;
     orientation = orientation;
     build = build;
     height = height
   })

let print_profile ({
     firstname = firstname;
     lastname = lastname;
     sex = sex;
     age = age;
     lo_agepref = lo_agepref;
     hi_agepref = hi_agepref;
     profession = profession;
     has_children = has_children;
     wants_children = wants_children;
     leisure = leisure;
     drinks = drinks;
     smokes = smokes;
     music = music;
     orientation = orientation;
     build = build;
     height = height } : profile) : unit =
  Printf.printf "%s %s\n" firstname lastname;
  Printf.printf "  sex: %s  age: %d  profession: %s\n" sex age profession;
  Printf.printf "  %s  %s\n" (if drinks then "social drinker" else "nondrinker") (if smokes then "smoker" else "nonsmoker");
  Printf.printf "  %s  %s\n"
    (if has_children then "has children" else "no children")
    (if wants_children then "wants children" else "does not want children");
  Printf.printf "  prefers a %s partner between the ages of %d and %d\n"
    (if (orientation="straight" && sex="F") || (orientation = "gay/lesbian" && sex="M") then "male" else "female")
    lo_agepref hi_agepref;
  Printf.printf "  likes %s music and %s\n" music leisure


let print_matches (n : string) ((p, ps) : profile * (float * profile) list) : unit =
  print_string "------------------------------\nClient: ";
  print_profile p;
  Printf.printf "\n%s best matches:\n" n;
  List.iter (fun (bci, profile) ->
    Printf.printf "------------------------------\nCompatibility index: %f\n" bci; print_profile profile) ps;
  print_endline ""



(* module PSeq = Sequence.Seq(PFuture)(struct let use_mpi = true end) *)
module PSeq = Sequence.ListSeq


(* apm computes the potential love of your life.  The filename of a file
 * containing candidate profiles is in location 0 of the given array.
 * The number of matches desired is in location 1.  The first and last name
 * of the (human) client are in location 2 and 3, respectively.  The client's
 * profile must be in the profiles file.  Results are output to stdout. *)
let p_equals (p1:profile) (p2:profile) : bool = 
	p1.firstname = p2.firstname && p1.lastname = p2.lastname

let compute_match (me:profile) (you:profile) : float = 0.

let matchme (args : string array) : unit = 
	let (filename, num_matches, firstname, lastname) 
		= (args.(0), args.(1), args.(2), args.(3)) in
	let file = read_whole_file filename in
	let string_profs = Str.split (Str.regexp "\n") file in
	let array_string_profs = Array.of_list string_profs in
	let seq_string_profs = PSeq.seq_of_array array_string_profs in
	let seq_profs = PSeq.map (fun x -> convert x) seq_string_profs in
	let profile = PSeq.reduce (fun x y -> if x.firstname = firstname && x.lastname = lastname then x else y) 
		(PSeq.nth seq_profs 0) seq_profs in
	let matched_profs = PSeq.map (fun x -> ((compute_match profile x), x)) seq_profs in
	let matched_format_array = PSeq.array_of_seq matched_profs in
	let matched_list = Array.to_list matched_format_array in
	
(*	let array_matched_profs = PSeq.array_of_seq matched_profs in
	let sorted = Array.sort (fun (x_prof, x_float) (y_prof, y_float) -> if x_float <. y_float then (  *)
	(*
	let rec aux (seq:(profile * float) PSeq.t) (n:int) (l:profile * (float * profile) list) = 
		match n with 
		| 0 -> l
		| _ -> 			
			let (best_prof, best_float) = PSeq.reduce (fun (x_prof, x_float) (y_prof, y_float) ->
				if x_float > y_float then (x_prof, x_float) else (y_prof, y_float)) (profile, -1.0) seq
			in 
			let next_seq = PSeq.reduce (fun ((x_prof:profile), (x_float:float)) (y:(profile*float) PSeq.t) -> 
				if p_equals x_prof best_prof then y 
				else y)
				(PSeq.singleton (profile, -1.0)) seq
			in
			aux next_seq (n-1) ((best_prof, best_float) :: l)
	in 
	let matched_list = aux matched_profs num_matches [] in *)
	print_matches num_matches (profile, take matched_list (int_of_string num_matches))
		




			(*	let (x_
				let (p_old, f_old) = old in
				let (y_seq, y_match) = y in
				let (p_new, f_new) = y_match in
				if f_new > f_old then ((PSeq.cons (p_old, f_old) y), Some (p_new, f_new)) 
				else ((PSeq.cons (p_new, f_new) y), Some (p_old, f_old))) 
		  (PSeq.empty, (profile, -1)) seq in
			let (best_p, best_f) = best_match in
			aux next_seq (n-1) ((profile, (best_f, best_p)) :: l)

*)








