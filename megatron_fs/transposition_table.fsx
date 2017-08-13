type Node = { xy:(int*int); value:int; parent:Node Option }
type Route = { moves:int; value:int; path:Node; }
type tt = Map<(int*int),Route list> // Transposition table type

let board = ["A *";
             "...";
             ".#."]

let width = board.[0].Length
let height = board.Length

let at (board:string list) (x,y) = board.[y].[x]

let listToString (list:char list) = list |> List.fold (fun acc c -> acc + c.ToString()) ""
let newBoard board player (xNew,yNew) =
    [0..height - 1]
    |> List.map (fun y ->
        [0..width - 1]
        |> List.map (fun x -> if (xNew,yNew) = (x,y) then player else if at board (x,y) = player then ' ' else at board (x,y)) 
        |> listToString)

let weight value =
    match value with
    | '.' -> 1
    | '*' -> 5
    | 'B' -> -10
    | _   -> 0

let validMove board (move:int*int->int*int) (x,y) =
    let (x',y') = move (x,y)
    if x' >= width || x' < 0 || y' >= height || y' < 0 || at board (x',y') = '#' then false
    else true

let up (x,y) = (x,y - 1)
let right (x,y) = (x + 1,y)
let down (x,y) = (x,y + 1)
let left (x,y) = (x - 1,y)

let validMoves board node =
    [up; right; down; left]
    |> List.filter (fun move -> validMove board move node.xy)
    |> List.map (fun move -> 
        { 
            xy = move node.xy; 
            value = node.value + (weight (at board (move node.xy))); 
            parent = Some(node) 
        })

let firstTT (map:tt) =
    let key = Map.pick (fun key value -> Some(key)) map
    let value = map.[key]
    (key, value)

let mergeTTvalue (map:tt) (key,value) =
    if map.ContainsKey(key) then
        map.Add(key, value @ map.[key])
    else
        map.Add(key, value)

let rec mergeTT (map:tt) (map':tt) =
    match map.Count, map'.Count with
    | 0,_ -> map'
    | _,0 -> map
    | 1,_ -> 
        let (key, value) = firstTT map
        mergeTTvalue map' (key, value)
    | _,_ ->
        let (key, value) = firstTT map
        mergeTT (map.Remove(key)) (mergeTTvalue map' (key, value))

let rec buildTT depth board node =
    match depth with
    | 3 -> Map.empty.Add(node.xy, [{ moves = depth; value = node.value; path = node}])
    | _ ->
        let tt = Map.empty.Add(node.xy, [{ moves = depth; value = node.value; path = node}])
        validMoves board node 
        |> List.map (fun move -> buildTT (depth + 1) (newBoard board 'A' move.xy) move)
        |> List.fold (fun acc tt -> mergeTT acc tt) tt

let acc_tt = buildTT 0 board { xy = (0,0); value = 0; parent = None }


//
//
// info for specific target node
for line in board do printfn "%A" line
acc_tt |> Map.iter (fun (x,y) routes -> printfn "(%d,%d) - %A routes" x y routes.Length)
printfn "routes to (0,1):"
printfn "%A" acc_tt.[(0,1)]

// optimal route
let optimalRoute (map:tt) = 
    Map.toSeq map
    |> Seq.maxBy (fun (xy,routes) -> routes |> List.maxBy (fun route -> route.value))
    |> (fun (xy,routes) -> routes)
    |> List.maxBy (fun route -> route.value)

printfn "optimal route in n moves: %A" (optimalRoute acc_tt)