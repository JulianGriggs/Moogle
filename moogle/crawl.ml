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

(* This is a function that takes a page and a WordDict.dict and returns an 
 * updated WordDict.dict.  For each word on the page, if it exists in the 
 * dictionary, then the link for the page is simply added to the set.  If it
 * doesn't exist then the word is added as a new key with value equal to the
 * singleton set with the page's link as the sole value.
 *)
let addLinkForEachWord (page : page) (d : WordDict.dict) : WordDict.dict =
  let url = page.url in
  let words = page.words in
    List.fold_left 
      (fun x y -> 
        match WordDict.lookup x y with
        | None -> 
          (* Create a set with the only value equaling the url*)
          let newSet = LinkSet.singleton url in
          (* Update the dictionary with k,v pair (new word, set(url)*)
          WordDict.insert x y newSet
        | Some set ->
          let newSet = LinkSet.insert url set in
          WordDict.insert x y newSet
        ) 
      d 
      words
  ;;

let assertNoOverlap (frontier: LinkSet.set) (visited:LinkSet.set) : unit = 
  LinkSet.fold (fun elt a -> assert(LinkSet.member visited elt = false) ; a) () frontier

(* Takes a page and a LinkSet.set 'frontier' and a LinkSet.set 'visited'.  
 * Returns a new set with all of the links on the page, that have not already
 * been visited, added to the frontier.
 *)
let addUnseenLinksToFrontier (page : page) (frontier: LinkSet.set) 
                                (visited:LinkSet.set) : LinkSet.set = 
  let links = page.links in
  List.fold_right 
  (fun x y -> if (LinkSet.member visited x) then y else LinkSet.insert x y)
    links frontier
;;

(* Build an index as follows:
 * 
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict = 
  if n = 0 then d
  else 
     match LinkSet.choose frontier with
      | None -> d
      | Some (link, setMinusCurrentLink) -> 
        let updatedVis = LinkSet.insert link visited in 
        (match get_page link with
          | None -> crawl n setMinusCurrentLink updatedVis d
          | Some page -> 
            let updatedDict = addLinkForEachWord page d in
            let updatedFrontier = 
              addUnseenLinksToFrontier page setMinusCurrentLink updatedVis in
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
