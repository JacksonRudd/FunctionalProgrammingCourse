open System
open System.Collections.Generic

let product xs ys = 
    List.collect (fun y -> List.map (fun i -> i,y) xs) ys

let self_product i = product i i

let get_neighborhood i j = List.map (fun (a, b) -> (a+i, b+j)) (self_product [-1 .. 1])

let filter_neighborhood neighborhood boundary_x boundary_y = List.filter (fun (x,y)-> x < boundary_x && y< boundary_y)  neighborhood

let filtered_neighborhood i  j  boundary_x boundary_y = filter_neighborhood (get_neighborhood i  j) boundary_x boundary_y 

let living_rule num = 
    num > 0

let memoize f =
    let dict = Dictionary<_, _>();
    fun c ->
        let exist, value = dict.TryGetValue c
        match exist with
        | true -> value
        | _ -> 
            let value = f c
            dict.Add(c, value)
            value


let rec is_cell_alive i j  boundary_x boundary_y iteration alive_originally_set=  
    if iteration = 0 then
        alive_originally_set |> Set.contains (i,j)
    else
        let neighborhood = filtered_neighborhood i j boundary_x boundary_y 
        let last_iteration = iteration - 1
        let get_last (n_i,n_j) = is_cell_alive n_i n_j boundary_x boundary_y last_iteration alive_originally_set
        let count_of_live_neighbors = (List.filter get_last neighborhood ).Length
        living_rule count_of_live_neighbors



let print_board  boundary_x boundary_y iteration alive_originally= 
    for y = 0 to boundary_y - 1 do 
      let cells = List.map (fun x -> is_cell_alive x y boundary_x boundary_y iteration alive_originally) [0..boundary_x]
      let format cell_list = String.concat "" (List.map (fun x -> if x then "*" else "-") cell_list  )

      printfn "%s" (format cells)
       
let loop time_end boundary_x boundary_y alive_originally= 
    for time = 0 to time_end do 
        Threading.Thread.Sleep(2000)
        Console.Clear() 
        print_board boundary_x boundary_y time alive_originally




loop 10 20 10 (set [(0,0)])

