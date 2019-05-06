module WBState =
       struct
         type t = int * int * int
         let compare (x0,y0,z0) (x1,y1,z1) =
           match Pervasives.compare x0 x1 with
               0 -> (match Pervasives.compare y0 y1 with
                    0-> Pervasives.compare z0 z1
                    |d -> d)
             | _ as c -> c
       end

module Wbset = Set.Make(WBState)
exception Invalid_Bucket 
let capacity = function
   0 ->12
   | 1 -> 8
   | 2 -> 5
   | _ -> raise Invalid_Bucket

let current bucket cur =
  let x,y,z = cur in
  match bucket with
   0 -> x
   |1 -> y
   |2 -> z 
   |_ -> raise Invalid_Bucket

let change bucket value cur_state =
  let x,y,z = cur_state in
  match bucket with
   0 -> (x+value,y,z)
   |1 -> (x,y+value,z)
   |2 -> (x,y,z+ value)
   |_ -> raise Invalid_Bucket

(*let min_val x y = if x< y then x else y*)

let do_pour action cur_state =
    let src,dst = action in
    if current src cur_state = 0 then cur_state
    else if current dst cur_state = capacity dst then cur_state
    else
       let k = min (current src cur_state) (capacity dst - current dst cur_state) in
          change src (0-k) cur_state |>  change dst k 
       
let print_action action =
    let x,y = action in
    Printf.printf "From %d to %d\n" x y

let wbsolve start_state goal =
  let seen = ref  Wbset.(empty |> add start_state) in 
  let pour_actions = [ (0,1); (0,2); (1,0); (1,2); (2,0); (2,1)] in 
  let rec solve cur_state  = 
    if WBState.compare cur_state goal = 0 then true
    else
        let sub_solve action =
          let new_state = do_pour action cur_state in
          if Wbset.mem  new_state !seen then false 
          else (
            seen := Wbset.add new_state !seen ; 
          if solve new_state 
          then (print_action action; true) 
          else false
          ) 
        in
        List.exists sub_solve pour_actions
  in
  solve start_state

let () =
  Printf.printf "Actions in reverse order\n";
  let goal = (6,6,0) in
  let start_state = (12,0,0) in
  wbsolve start_state goal |> ignore 

