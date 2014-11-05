open Util ;;    
open CrawlerServices ;;
open Order ;;
open Pagerank ;;


module MoogleRanker
  = InDegreeRanker (PageGraph) (PageScore)
  (*
     = RandomWalkRanker (PageGraph) (PageScore) (struct 
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct 
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s = 
  let _ = Printf.printf "%s\n" s in
  flush_all();;


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* TODO: Build an index as follows:
 * 
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)

let rec addLinkForEachWord (page : page) (d : WordDict.dict) : WordDict.dict =
  let url = page.url in
  let words = page.words in
    List.fold_right 
      (fun x y -> 
        match WordDict.lookup y x with
        | None -> 
          (* Create a set with the only value equaling the url*)
          let newSet = LinkSet.singleton url in
          (* Update the dictionary with k,v pair (new word, set(url)*)
          WordDict.insert y x newSet
        | Some set ->
          let newSet = LinkSet.insert url set in
          WordDict.insert y x newSet
        ) 
      words 
      d
  ;;

let rec addLinksToSet (page : page) (set: LinkSet.set) : LinkSet.set = 
  let links = page.links in
  List.fold_right (fun x y -> LinkSet.insert x y) links set
;;


let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict = 
  if n = 0 then d
  else 
     match LinkSet.choose frontier with
      | None -> d
      | Some (link, newSet) -> 
        let updatedVis = LinkSet.insert link visited in 
        (match get_page link with
          | None -> crawl n newSet updatedVis d
          | Some page -> 
            let updatedDict = addLinkForEachWord page d in
            let updatedFrontier = addLinksToSet page newSet in
            crawl (n-1) updatedFrontier updatedVis updatedDict 
        )
;;

let crawler () = 
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;

(* Some Testing *)
let test_addLinkForEachWord (page: page) : unit = 
  let words = page.words in
  let url = page.url in
  let dict1 = addLinkForEachWord page WordDict.empty in
  List.iter (fun x -> match WordDict.lookup dict1 x with
                      | None -> failwith "Word not inserted into dict"
                      | Some value -> assert (value = (LinkSet.insert url LinkSet.empty))
                    ) words
;;

let testURL = { host = "www.test.com" ; port = 80; path = "/" } in
  let testPage = {url = testURL ; links = [testURL] ; words = ["this"; "is"; "a"; "test"]} in
  test_addLinkForEachWord testPage;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
