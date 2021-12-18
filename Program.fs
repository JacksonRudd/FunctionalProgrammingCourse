open System
open System.Collections.Generic
open System.IO

let product xs ys = 
    List.collect (fun y -> List.map (fun i -> i,y) xs) ys

let self_product i = product i i

let get_neighborhood i j = 
    self_product [-1 .. 1]
    |> List.filter (fun (x,y) -> not ((x,y) = (0,0)) )
    |> List.map (fun (a, b) -> (a+i, b+j)) 

let filter_neighborhood neighborhood boundary_x boundary_y = List.filter (fun (x,y)-> x < boundary_x && y< boundary_y)  neighborhood

let filtered_neighborhood i  j  boundary_x boundary_y = filter_neighborhood (get_neighborhood i  j) boundary_x boundary_y 

let living_rule is_currently_alive num_of_alive_neighbors = 
    if is_currently_alive then 
        num_of_alive_neighbors = 2 || num_of_alive_neighbors = 3 
    else
        num_of_alive_neighbors = 3

let rec is_cell_alive i j  boundary_x boundary_y iteration alive_originally_set memo_fun =  
    if iteration = 0 then
        alive_originally_set |> Set.contains (i,j)
    else
        let neighborhood = filtered_neighborhood i j boundary_x boundary_y 
        let count_of_live_neighbors = (List.filter memo_fun neighborhood ).Length
        let my_val = memo_fun (i, j)
        living_rule my_val count_of_live_neighbors

let dict = Dictionary<_, _>();

let memo iteration (i, j) = 
    let it_mod = iteration % 2
    let _, value = dict.TryGetValue((i,j, it_mod))
    value

let set_memo iteration (i,j) answer= 
    let it_mod = iteration % 2
    dict.Remove((i,j, it_mod))
    dict.Add((i,j, it_mod), answer)


let mem_is_cell_alive i j  boundary_x boundary_y iteration alive_originally_set = 
    let last_memo = memo  (iteration - 1) 
    set_memo iteration (i,j) (is_cell_alive i j  boundary_x boundary_y iteration alive_originally_set last_memo )
    memo iteration (i,j)
    

let print_board  boundary_x boundary_y iteration alive_originally= 
    for y = 0 to boundary_y - 1 do 
      let cells = List.map (fun x -> mem_is_cell_alive x y boundary_x boundary_y iteration alive_originally) [0..boundary_x]
      let format cell_list = String.concat "" (List.map (fun x -> if x then "*" else "-") cell_list  )
      printfn "%s" (format cells)
       
let loop time_end boundary_x boundary_y alive_originally= 
    for time = 0 to time_end do 
        Console.Clear() 
        print_board boundary_x boundary_y time alive_originally
        Threading.Thread.Sleep(50)

let get_x_axis str y = 
    Seq.zip [0..200] str 
    |> Seq.filter (fun (x, c) -> c = 'O')
    |> Seq.map (fun (x,i) -> (x,y))
let lines = File.ReadLines "gosper.txt" |> Seq.filter (fun x -> x.[0] <> '!')
let s = Set.ofSeq (Seq.zip [0..200] lines |> Seq.collect(fun (y, str) -> get_x_axis str y))

loop 1000 40 20 s
