(*
   Instant runoff voting
*)

open Printf

type candidate = string
type ballot = candidate list

(*
   Remove duplicate candidate names from a ballot, keeping only the first
   occurrence.
*)
let unique ballot =
  let tbl = Hashtbl.create (List.length ballot) in
  let r =
    List.fold_left (fun acc cand ->
      if Hashtbl.mem tbl cand then
        acc
      else (
        Hashtbl.add tbl cand ();
        cand :: acc
      )
    ) [] ballot
  in
  List.rev r

let top l =
  match l with
  | [] -> 0, []
  | (candidate, n) :: tl ->
      let rec aux acc = function
        | [] -> acc
        | (cand, n') :: tl ->
            if n' = n then
              aux (cand :: acc) tl
            else
              acc
      in
      n, aux [candidate] tl

let map f l = List.rev (List.rev_map f l)

let filter_ballots bottom_candidates ballots =
  map (fun l ->
    List.filter
      (fun cand -> not (List.mem cand bottom_candidates))
      l
  ) ballots

let sort_candidates l = List.sort String.compare l

(*
   Ensure every candidate has a counter, including those who are not
   a top candidate in any ballot.
*)
let init_counters ballots =
  let tbl = Hashtbl.create 10 in
  List.iter (fun l ->
    List.iter (fun cand ->
      if not (Hashtbl.mem tbl cand) then
        Hashtbl.add tbl cand (ref 0)
    ) l
  ) ballots;
  tbl

let rec count ballots =
  let tbl = init_counters ballots in
  List.iter (fun l ->
    match l with
    | cand :: _ ->
        let counter =
          try Hashtbl.find tbl cand
          with Not_found -> assert false
        in
        incr counter
    | [] ->
        ()
  ) ballots;
  let l =
    Hashtbl.fold (fun candidate r acc -> (candidate, !r) :: acc) tbl []
  in
  let ranked =
    List.sort (fun (_, n1) (_, n2) -> compare n2 n1) l
  in
  let top_votes, top_candidates = top ranked in
  let top_candidates = sort_candidates top_candidates in
  let bottom_votes, bottom_candidates = top (List.rev ranked) in
  let bottom_candidates = sort_candidates bottom_candidates in
  let ballot_count = List.length ballots in
  printf "top: %s; bottom: %s\n%!"
    (String.concat ", " top_candidates)
    (String.concat ", " bottom_candidates);
  if top_votes > ballot_count / 2 then
    (* winner *)
    top_candidates
  else if top_candidates = bottom_candidates then
    (* tie *)
    top_candidates
  else
    (* iterate *)
    let filtered_ballots = filter_ballots bottom_candidates ballots in
    count filtered_ballots

let run (ballots : candidate list list) : candidate list =
  count (map unique ballots)

let test1 () =
  let ballots = [
    [ "A"; "B"; "C"; "D"; "E"; ];
    [ "A"; "C"; "B"; "E"; "D"; ];
    [ "E"; "C"; "B"; "D"; "D" ];
    [ "C"; "D"; "E"; "A" ];
  ] in
  run ballots = ["A"]

let test2 () =
  let ballots = [
    [ "A"; "B"; "C"; "D"; "E"; ];
    [ "A"; "C"; "B"; "E"; "D"; ];
    [ "A"; "D"; "B"; "A"; "E" ];
    [ "B"; "D"; "E"; "A" ];
  ] in
  run ballots = ["A"]

let test3 () =
  let ballots = [
    [ "A"; "B"; "C"; "D"; "E"; ];
    [ "B"; "C"; "E"; "D"; ];
    [ "A"; "B"; "D"; "E"; ];
    [ "B"; "D"; "E"; "A"; ];
  ] in
  run ballots = ["A";"B"]

let tests = [
  "test1", test1;
  "test2", test2;
  "test3", test3;
]
