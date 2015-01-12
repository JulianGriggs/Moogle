open Array
open Future
open Sequence
open Printf

(* module S = Seq(PFuture)(struct let use_mpi = true end) *)
module S = ListSeq
type group = {pop:int; lat:float; lon:float}
type area = {left:float; right:float; top:float; bottom:float}


(* convert latitude to compensate for rectangular projection of map *)
let mercator_conv (lat: float) : float = 
  let pi = (4. *. atan 1.0) in
  let latpi = lat *. (pi /. 180.0) in 
  log ( (tan latpi) +. (1.0 /. (cos latpi)) )


(* used to build parsed data *)
let group_of_data (pop, lat, lon) : group = 
  {pop=pop; lat=(mercator_conv lat); lon=(lon)}


(* compute the row and column in the GUI given the latitude and longitude *)
let rowcol_of_latlon (area: area) (rows,cols) (lat,lon) : int*int =
  let delx = (area.right -. area.left) /. (float_of_int cols) in 
  let dely = (area.top -. area.bottom) /. (float_of_int rows) in 
  let c = int_of_float ((lon -. area.left) /. delx) in
  let r = int_of_float ((lat -. area.bottom) /. dely) in
  let r = if r = rows then r else r + 1 in 
  let c = if c = cols then c else c + 1 in 
  (r,c)


(* latitude and longitude area for a box in terms of rows and colums in the GUI
 * rows, cols:  total number of rows and columns
 * l,b,r,t:     left, bottom, right, and top of box respectively *)
let latlon_of_rowcol (area: area) (rows,cols) (l,b,r,t) : area =
  let delx = (area.right -. area.left) /. (float_of_int cols) in 
  let dely = (area.top -. area.bottom) /. (float_of_int rows) in 
  {left = area.left +. (float_of_int (l - 1)) *. delx;
   right = area.left +. (float_of_int r) *. delx;
   top = area.bottom +. (float_of_int t) *. dely;
   bottom = area.bottom +. (float_of_int (b - 1)) *. dely }


(**************************************************************************
 * Part 1
 * 
 * Find the smallest area encompassing all of the census groups.
 * i.e., the area containing all of the population of the United States
 **************************************************************************)

let closest_to_edge (r1: area) (r2: area) : area =
  {left = (min r1.left r2.left); 
   right = (max r1.right r2.right);
   top = (max r1.top r2.top);
   bottom = (min r1.bottom r2.bottom)}


let encompassing_area (groups: group S.t) : area = 
    let inf = Pervasives.infinity in 
    let ninf = Pervasives.neg_infinity in 
    S.map_reduce 
      (fun g -> {left=g.lon; right=g.lon; top=g.lat; bottom=g.lat}) 
      closest_to_edge 
      {left=inf; right=ninf; top=ninf; bottom=inf} 
      groups


(**************************************************************************
 * Part 2
 *
 * Compute the population within an area by looking at all n census groups
 * and summing their total populations using reduce
 **************************************************************************)



let contains_group (r: area) (g: group) : bool = 
  (g.lon >= r.left && g.lon < r.right) &&
  (g.lat <= r.top && g.lat > r.bottom)


let contains_area (r1: area) (r2: area) : bool = 
  (r2.left >= r1.left && r2.left < r1.right) &&
  (r2.right <= r1.right && r2.right > r1.left) &&
  (r2.top <= r1.top && r2.top > r1.bottom) &&
  (r2.bottom >= r1.bottom && r2.bottom < r1.top)


let population_search (groups: group S.t) (query: area) : int = 
  let g = 
    S.reduce (fun g1 g2 -> 
          match (contains_group query g1, contains_group query g2) with 
          | (false, false) -> {pop=0; lat=query.top; lon=query.left}
          | (true, false) -> g1 
          | (false, true) -> g2 
          | (true, true) -> {pop=g1.pop+g2.pop; lat=g1.lat; lon=g2.lon} 
             ) {pop=0; lat=query.top; lon=query.left} groups in 
  g.pop


(************************************************************************** 
 * Part 3
 *
 * Compute the population within an area by precomputing the total population
 * at each rectangle (x,y) for all groups in rectangles from (1,1) to (x,y).
 * You MUST perform this precompution using parallel prefix scans
 **************************************************************************)
let initialize_grid (groups: group S.t) (us_area: area) (rows,cols) : int S.t S.t = 
  let us_pop_array = S.array_of_seq (S.tabulate (fun x -> S.array_of_seq (S.tabulate (fun y -> 0) cols)) rows) in
	let _ = S.iter (fun group -> 
		let (row, col) = rowcol_of_latlon us_area (rows,cols)	(group.lat,group.lon) in
    let row' = row - 1 in
    let col' = col - 1 in  
		let old_value = Array.get (Array.get us_pop_array row') col' in 
		Array.set (Array.get us_pop_array row') col' (old_value + group.pop)) groups
	in 
	let arr_of_seq = Array.map (fun x -> S.seq_of_array x) us_pop_array in
	S.seq_of_array arr_of_seq

let precompute (groups: group S.t) (us_area: area) (rows,cols) : int S.t S.t = 
	if rows = 0 then S.empty ()
	else  
		let initial = initialize_grid groups us_area (rows,cols) in
		(*initial*)
		let length = S.length (S.nth initial 0) in
		let right_pass = S.map (S.scan (+) 0) initial in
		(*right_pass*)
		let f = (fun seq1 seq2 -> S.tabulate (fun i -> (S.nth seq1 i) + (S.nth seq2 i)) length) in 
		let zero_row = S.tabulate (fun i -> 0) length in
		S.scan f zero_row right_pass 

 
let population_lookup (summed_areas: int S.t S.t) (l,b,r,t) : int = 
	let l' = if l = 1 then 0 else l-2 in
	let b' = if b = 1 then 0 else b-2 in
	let r' = r-1 in
	let t' = t-1 in
  let top_left = S.nth (S.nth summed_areas t') l' in
	let top_right = S.nth (S.nth summed_areas t') r' in
	let bottom_left = S.nth (S.nth summed_areas b') l' in
	let bottom_right = S.nth (S.nth summed_areas b') r' in
	let a1 = top_right in
	if l = 1 && b = 1 then top_right
	else if l = 1 then top_right - bottom_right 
	else if b = 1 then top_right - top_left
	else top_right - bottom_right - top_left + bottom_left
	







