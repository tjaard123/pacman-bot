let initialFile     = "initial.state"
let stateFile       = "game.state"
let historyFile     = "megatron.history"
let searchDepth     = 36
let print           = true

type node = { xy:int * int; parent:node Option; }
type Tree =
    | End of (int * int)
    | Target of (int * int)
    | Node of (int * int) * Tree list

let printBoard board = board |> Array.map (String.collect (fun c -> " " + c.ToString())) |> Array.map (printfn "%s")

[<EntryPoint>]
let main argv = 
    let board = System.IO.File.ReadAllLines(argv.[0])
    let width = board.[0].Length
    let height = board.Length

    let weight value =
        match value with
        | '.' -> 1
        | '*' -> 5
        | 'B' -> -10
        | _   -> 0

    let at (x,y) = board.[y].[x]
    let rec findValue y value =
        if y >= height then None
        else if board.[y].Contains(value) then Some(board.[y].IndexOf(value),y) else findValue (y + 1) value
    let xy = findValue 0
    let A = (xy "A").Value
    let B = (xy "B").Value

    // Determine target
    let q1 = board.[0..(height / 2) - 1] |> Array.map (fun y -> y.[0..width / 2])
    let q2 = board.[0..(height / 2) - 1] |> Array.map (fun y -> y.[width / 2..width - 1])
    let q3 = board.[height / 2..height - 1] |> Array.map (fun y -> y.[0..width / 2])
    let q4 = board.[height / 2..height - 1] |> Array.map (fun y -> y.[width / 2..width - 1])
    let tally (section:string []) = section |> Array.sumBy (fun y -> y.ToCharArray() |> Array.sumBy (fun c -> weight c))
    
    let target =
        match xy "*" with
        | None -> '.'
        | Some(_) -> '*'
    let target_xy = xy (target.ToString())

    let validMove (move:int*int->int*int) (x,y) =
        let (x',y') = move (x,y)
        if x' >= width || x' < 0 || y' >= height || y' < 0 || at (x',y') = '#' then false
        else true

    let up (x,y) = (x,y - 1)
    let right (x,y) = (x + 1,y)
    let down (x,y) = (x,y + 1)
    let left (x,y) = (x - 1,y)

    let validChildren node =
        [up; right; down; left]
        |> List.filter (fun move -> validMove move node.xy)
        |> List.map (fun move -> { xy = move node.xy; parent = Some(node)})

    let validChildren' xy =
        [up; right; down; left]
        |> List.filter (fun move -> validMove move xy)
        |> List.map (fun move -> move xy)

    let containsTarget nodes target = List.exists (fun n -> n.xy = target) nodes
    let containsTarget' nodes target = List.exists (fun n -> at n.xy = target) nodes
    let containsTarget'' xy's target = List.exists (fun xy -> xy = target) xy's

    // BFS for target xy
    let rec BFSxy (nodesAtCurrentDepth:node list) n target visited =
        let nodesAtNextDepth = 
            nodesAtCurrentDepth 
            |> List.fold (fun acc node -> acc @ validChildren node) []
            |> List.filter (fun node -> not (containsTarget visited node.xy))

        if containsTarget nodesAtNextDepth target then [nodesAtNextDepth]
        else if n = searchDepth then failwith "There is no route to this target!"
        else  nodesAtNextDepth :: BFSxy nodesAtNextDepth (n + 1) target (visited @ nodesAtNextDepth)

    // BFS for target value
    let rec BFSt (nodesAtCurrentDepth:node list) n target visited =
        let nodesAtNextDepth = 
            nodesAtCurrentDepth 
            |> List.fold (fun acc node -> acc @ validChildren node) []
            |> List.filter (fun node -> not (containsTarget visited node.xy))

        if containsTarget' nodesAtNextDepth target then [nodesAtNextDepth]
        else if n = searchDepth then failwith "There is no route to this target!"
        else  nodesAtNextDepth :: BFSt nodesAtNextDepth (n + 1) target (visited @ nodesAtNextDepth)

    // DFS
    let rec GetNode n target visited xy =
        if n = searchDepth then End(xy)
        else if xy = target then Target(xy)
        else Node(xy, 
                (validChildren' xy) 
                |> List.filter (fun xy' -> not (containsTarget'' visited xy'))
                |> List.map (GetNode (n + 1) target (visited @ (validChildren' xy))))

    let tree = GetNode 0 target_xy.Value [A] A

    let GetNodeChildren node =
        match node with
        | End(_) -> []
        | Target(xy) -> []
        | Node(xy, children) -> children |> List.map { xy =  }

    let rec PathsToTarget nodes n =
        let nodesAtNextDepth =
            nodes
            |> List.fold (fun acc node -> acc @ (GetNodeChildren node)) []
            //|> List.map (fun Node(xy', children) -> { xy = xy'; parent = Some(this)})

        if n = searchDepth then failwith "?"
        else nodesAtNextDepth :: PathsToTarget nodesAtNextDepth (n + 1)
    
    //let tree = BFSxy [{ xy = A; parent = None; }] 0 target_xy []
    //let shortestPath = tree.[tree.Length - 1] |> List.find (fun n -> n.xy = target_xy)

    let tree = BFSt [{ xy = A; parent = None; }] 0 target []
    let shortestPath = tree.[tree.Length - 1] |> List.find (fun n -> at n.xy = target)

    let rec nextMove node =
        match node.parent.Value.parent with
        | None -> node.xy
        | Some(_) -> nextMove node.parent.Value

    let next_xy = (nextMove shortestPath)

    // OH MY WORD THERE HAS TO BE A BETTER WAY!!!
    board.[snd next_xy] <- board.[snd next_xy].[0..(fst next_xy - 1)] + "A" + board.[snd next_xy].[(fst next_xy + 1)..width - 1]
    board.[snd A] <- board.[snd A].[0..(fst A - 1)] + " " + board.[snd A].[(fst A + 1)..width - 1]
    
    System.IO.File.WriteAllLines(stateFile, board)


    // PRINTING
    
    let printCoordinate (x,y) = System.String.Format(" {2}|{0},{1}| ",x, y, (at (x,y)))
    printBoard board
    if (print) then
        printfn ""
        printfn "BFS"
        tree |> List.map (fun lvl -> printfn "%s" (lvl |> List.fold (fun acc n -> acc + (printCoordinate n.xy)) "")) |> ignore
        printfn ""
        printfn "Best move: %A" (nextMove shortestPath)
        printfn ""
    0