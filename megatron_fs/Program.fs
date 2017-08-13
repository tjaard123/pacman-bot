let megatronStart   = System.DateTime.Now
let historyFile     = "megatron.history"
let searchDepth     = 36
let allowedRevisits = 0
let ttMaxDuration   = 3000 // milliseconds

type Node = { xy:(int*int); value:int; parent:Node Option }
type Route = { moves:int; value:int; path:Node; }
type tt = Map<(int*int),Route list> // transposition table type
type history = { round:int; opponentWentUp:bool; }

let printBoard board = board |> Array.map (String.collect (fun c -> " " + c.ToString())) |> Array.map (printfn "%s")

[<EntryPoint>]
let main argv = 
    let board = if argv.Length <> 0 then System.IO.File.ReadAllLines(argv.[0]) else System.IO.File.ReadAllLines(@"C:\git\entelect_challenge\megatron_fs\megatron_fs\bin\Debug\test.state")
    let width = board.[0].Length
    let height = board.Length
    let respawn = (9,10)
    let respawnExit1 = (9,9)
    let respawnExit2 = (9,11)
    let starQ1 = (1,2)
    let starQ2 = (17,2)
    let starQ3 = (1,16)
    let starQ4 = (17,16)
    let rec findValue y value =
        if y >= height then None
        else if board.[y].Contains(value) then Some(board.[y].IndexOf(value),y) else findValue (y + 1) value
    let rec findValueQ2 y value =
        if y >= height then None
        else if board.[y].[width / 2..width - 1].Contains(value) then Some(board.[y].LastIndexOf(value),y) else findValueQ2 (y + 1) value
    let rec findValueQ3 y value =
        if y < 0 then None
        else if board.[y].Contains(value) then Some(board.[y].IndexOf(value),y) else findValueQ3 (y - 1) value
    let rec findValueQ4 y value =
        if y < 0 then None
        else if board.[y].[width / 2..width - 1].Contains(value) then Some(board.[y].LastIndexOf(value),y) else findValueQ4 (y - 1) value
    let xy = findValue 0
    let A = (xy "A").Value
    let B = (xy "B")
    let weAreLeft =
        match A with
        | (x,_) when x < (width / 2) -> true
        | _ -> false

    let at (board:string []) (x,y) = board.[y].[x]

    let arrayToString (array:char []) = array |> Array.fold (fun acc c -> acc + c.ToString()) ""
    let newBoard (board:string []) player (xNew,yNew) dropPill =
        let oldPositionChar = if dropPill then '!' else ' '
        [|0..height - 1|]
        |> Array.map (fun y ->
            [|0..width - 1|]
            |> Array.map (fun x -> if (xNew,yNew) = (x,y) then player 
                                    else if at board (x,y) = player then oldPositionChar 
                                    else at board (x,y)) 
            |> arrayToString)

    let weight value =
        match value with
        | '.' -> 1
        | '*' -> 5
        | 'B' -> -1
        | _   -> 0

    let validMove board (move:int*int->int*int) (x,y) =
        let (x',y') = move (x,y)
        if (x',y') = (8,10) || (x',y') = (10,10) then false // no go respawn area
        else if x' >= width || x' < 0 || y' >= height || y' < 0 || at board (x',y') = '#' then false
        else if not (B = None) && (x,y) = respawn && (x',y') = B.Value then false // can't eat in respawn
        else if (x,y) = respawnExit1 && (x',y') = respawn then false // have to exit
        else if (x,y) = respawnExit2 && (x',y') = respawn then false // have to exit
        else if ((x,y) <> respawnExit1 && (x,y) <> respawnExit2 && (x,y) <> respawn) 
            && ((x',y') = respawnExit1 || (x',y') = respawnExit2) then false // can't go into respawn
        else true

    let up (x,y) = (x,y - 1)
    let right (x,y) = (x + 1,y)
    let down (x,y) = (x,y + 1)
    let left (x,y) = (x - 1,y)

    let openingLeftDown = [right;down;down;down;down;left;left;left;down;down;up;]
    let openingRightDown = [left;down;down;down;down;right;right;right;down;down;up;]
    let openingLeftUp = [right;up;up;up;up;up;up;left;left;left;up;up;down;]
    let openingRightUp = [left;up;up;up;up;up;up;right;right;right;up;up;down;]
    let poisonAtDown = 9
    let poisonAtUp = 11

    let count xy visited = visited |> List.filter (fun xy' -> xy' = xy) |> (fun list -> list.Length)
    let validMoves board visited node =
        let availableMoves = 
            if node.xy = (0,10) then (fun xy -> (18,10)) :: [up; right; down; left]
            else if node.xy = (18,10) then (fun xy -> (0,10)) :: [up; right; down; left]
            else [up; right; down; left]

        availableMoves
        |> List.filter (fun move -> validMove board move node.xy)
        |> List.filter (fun move -> count (move node.xy) visited <= allowedRevisits)
        |> List.map (fun move -> 
            { 
                xy = move node.xy; 
                value = node.value + (weight (at board (move node.xy))); 
                parent = Some(node) 
            })

    // determine target
    let q1 = board.[0..(height / 2) - 1] |> Array.map (fun y -> y.[0..width / 2])
    let q2 = board.[0..(height / 2) - 1] |> Array.map (fun y -> y.[width / 2..width - 1])
    let q3 = board.[height / 2..height - 1] |> Array.map (fun y -> y.[0..width / 2])
    let q4 = board.[height / 2..height - 1] |> Array.map (fun y -> y.[width / 2..width - 1])
    let tally (section:string []) = section |> Array.sumBy (fun y -> y.ToCharArray() |> Array.sumBy (fun c -> weight c))
    let tallyQ1 = tally q1
    let tallyQ2 = tally q2
    let tallyQ3 = tally q3
    let tallyQ4 = tally q4
    let countingStars = (if at board starQ1 = '*' then 1 else 0) + (if at board starQ2 = '*' then 1 else 0) + (if at board starQ3 = '*' then 1 else 0) + (if at board starQ4 = '*' then 1 else 0)
    let isInSection (section:string []) thing = section |> Array.exists (fun y -> y.Contains(thing))
    let writeHistory history =
        System.IO.File.WriteAllText(historyFile, history.round.ToString())
    let history =
        if (not (System.IO.File.Exists(historyFile))) || (not (B = None) && (((A = (3,10) && (B.Value = (15,10) || B.Value = (14,10) || B.Value = (16,10))) || ((B.Value = (3,10) || B.Value = (2,10) || B.Value = (4,10)) && A = (15,10)))) && (tally board) >= 179) then 
            // start of game
            if (System.IO.File.Exists(historyFile)) then System.IO.File.Delete(historyFile);
            { round = 0; opponentWentUp = false }
        else
            let text = System.IO.File.ReadAllText(historyFile).Split(',')
            let round = int text.[0]
            let opponentWentUp =
                if round = 1 then
                    if (isInSection [|board.[11]|] "B") then false
                    else true
                else (text.[1] = "TRUE")
            { round = round; opponentWentUp = opponentWentUp; }

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

    let rec buildTT depth board visited (startTime:System.DateTime) target node =
        if (System.DateTime.Now - startTime) > System.TimeSpan(0,0,0,0,ttMaxDuration) then // OUT OF TIME :(
            Map.empty.Add(node.xy, [{ moves = depth; value = node.value; path = node}])
        else if searchDepth = depth then
            Map.empty.Add(node.xy, [{ moves = depth; value = node.value; path = node}])
        else if node.xy = target then
            Map.empty.Add(node.xy, [{ moves = depth; value = node.value; path = node}])
        else if tally board > 50 && depth > 8 && (node.value = 0 || (depth / node.value) > 3) then // path aint doing too well
            Map.empty.Add(node.xy, [{ moves = depth; value = node.value; path = node}])
        else
            let tt = Map.empty.Add(node.xy, [{ moves = depth; value = node.value; path = node}])
            validMoves board visited node 
            |> List.map (fun move -> buildTT (depth + 1) (newBoard board 'A' move.xy false) (move.xy :: visited) startTime target move)
            |> List.fold (fun acc tt -> mergeTT acc tt) tt

    let leastMoves (routes:Route list) =
        routes
        |> List.minBy (fun route -> route.moves)


    let rec BFS visited depth nodesAtCurrentDepth =
        let stars = 
            nodesAtCurrentDepth 
            |> List.filter (fun node -> at board node.xy = '*')
            |> List.map (fun node -> (node.xy,{ moves = depth; value = node.value; path = node }))
        let nodesAtNextDepth = nodesAtCurrentDepth |> List.fold (fun acc node -> acc @ validMoves board visited node) []

        if depth = 40 then stars
        else if visited |> List.exists (fun xy -> xy = starQ1)
            && visited |> List.exists (fun xy -> xy = starQ2)
            && visited |> List.exists (fun xy -> xy = starQ3)
            && visited |> List.exists (fun xy -> xy = starQ4)
            then stars
        else stars @ BFS ((nodesAtCurrentDepth |> List.map (fun node -> node.xy)) @ visited) (depth + 1) nodesAtNextDepth

    // optimal route to a target
    let rec optimalRouteToTarget optimalRoute minMoves maxMoves routes =
        if optimalRoute.value >= optimalRoute.moves then
            optimalRoute
        else if minMoves = maxMoves then
            optimalRoute
        else
            let routesAtMinMoves = routes |> List.filter (fun route -> route.moves = minMoves)
            if routesAtMinMoves.Length = 0 then
                optimalRouteToTarget optimalRoute (minMoves + 1) maxMoves routes
            else
                let minMovesMaxValue = routesAtMinMoves |> List.maxBy (fun route -> route.value)
                if (double optimalRoute.moves / double optimalRoute.value) <= (double minMovesMaxValue.moves / double minMovesMaxValue.value) then
                    optimalRouteToTarget optimalRoute (minMoves + 1) maxMoves routes
                else
                    optimalRouteToTarget minMovesMaxValue (minMoves + 1) maxMoves routes

    // optimal route given n moves
    let maxValue (map:tt) =
        Map.toSeq map
        |> Seq.maxBy (fun (xy,routes) -> routes |> List.maxBy (fun route -> route.value))
        |> (fun (xy,routes) -> routes)
        |> List.maxBy (fun route -> route.value)
        |> (fun route -> route.value)

    let maxRoutes (map:tt) maxValue =
        Map.toSeq map
        |> Seq.filter (fun (xy,routes) -> routes |> List.exists (fun route -> route.value = maxValue))
        |> Seq.fold (fun acc (xy,routes) -> acc @ routes) []

    let rec nextMove node =
        match node.parent.Value.parent with
        | None -> node.xy
        | Some(_) -> nextMove node.parent.Value

    // BFS for target value
    let containsTarget nodes target = List.exists (fun n -> at board n.xy = target) nodes
    let rec BFSt (nodesAtCurrentDepth:Node list) n target visited =
        let nodesAtNextDepth = 
            nodesAtCurrentDepth 
            |> List.fold (fun acc node -> acc @ validMoves board visited node) []
            |> List.filter (fun node -> not (List.exists (fun xy -> xy = node.xy) visited))

        if containsTarget nodesAtNextDepth target then [nodesAtNextDepth]
        else if n = searchDepth then [nodesAtNextDepth]
        else  nodesAtNextDepth :: BFSt nodesAtNextDepth (n + 1) target (visited @ (nodesAtNextDepth |> List.map (fun node -> node.xy)))

    let defaultToBFS =
        let tree = BFSt [{ xy = A; value = 0; parent = None; }] 0 '.' []
        let shortestPath = tree.[tree.Length - 1] |> List.find (fun n -> at board n.xy = '.')
        let rec nextMove node =
            match node.parent.Value.parent with
            | None -> node.xy
            | Some(_) -> nextMove node.parent.Value

        nextMove shortestPath

    // print helpers
    let rec nodeToList node =
        match node.parent with
        | None -> []
        | Some(node') -> node.xy :: (nodeToList node')

    let printPath node board =
        let path = nodeToList node
        [|0..height - 1|]
        |> Array.map (fun y ->
            [|0..width - 1|]
            |> Array.map (fun x -> if List.exists (fun (x',y') -> (x,y) = (x',y')) path then 'x' else at board (x,y)) 
            |> arrayToString)

    // start working!
    let myMove =
        if history.opponentWentUp && history.round < openingLeftDown.Length then
            if weAreLeft then
                openingLeftDown.[history.round] A
            else
                openingRightDown.[history.round] A
        else if not history.opponentWentUp && history.round < openingLeftUp.Length then
            if weAreLeft then
                openingLeftUp.[history.round] A
            else
                openingRightUp.[history.round] A
        else
            let target_xy =
                if countingStars = 1 then
                    findValue 0 "*"
                else if countingStars > 1 then
                    if (B = None) then
                        findValue 0 "*"
                    else if countingStars = 2 then
                        let opponentBFStoStars = BFS [] 0 [{ xy = B.Value; value = 0; parent = None }]
                        let BFStoStars = BFS [] 0 [{ xy = A; value = 0; parent = None}]
                        let closestStarForB = opponentBFStoStars |> List.minBy (fun route -> double (snd route).moves / double (snd route).value)
                        let furthestStarForB = opponentBFStoStars |> List.maxBy (fun route -> double (snd route).moves / double (snd route).value)
                        let A_closestStarForB = BFStoStars |> List.find (fun route -> (fst route) = (fst closestStarForB))
                        let isopponentFarAway (_,myRoute) (_,hisRoute) =
                            if (myRoute.value >= myRoute.moves && myRoute.moves < hisRoute.moves) || (myRoute.moves + 4) < hisRoute.moves then true
                            else false

                        if (isopponentFarAway A_closestStarForB closestStarForB) then
                            Some(A_closestStarForB |> (fun furthest -> fst furthest))
                        else
                            Some(furthestStarForB |> (fun furthest -> fst furthest))
                    else
                        let opponentBFStoStars = BFS [] 0 [{ xy = B.Value; value = 0; parent = None }]
                        let furthestStarForB = opponentBFStoStars |> List.maxBy (fun route -> (snd route).moves)
                        Some(furthestStarForB |> (fun furthest -> fst furthest))                        
                else if tally board > 10 then
                    let quadrants = [(1,q1);(2,q2);(3,q3);(4,q4)]
                    let avg = quadrants |> List.averageBy (fun q -> double (tally (snd q)))
                    let BisInQuadrant = if isInSection q1 "B" then 1 else if isInSection q2 "B" then 2 else if isInSection q3 "B" then 3 else 4
                    let maxQ = quadrants |> List.maxBy (fun q -> tally (snd q))
                    let max = tally (snd maxQ)

                    // if the max quadrant is significantly more than the rest, go there
                    if float max > (avg + (avg / 2.0)) then
                        if fst maxQ = 1 then
                            findValue 1 "."
                        else if fst maxQ = 2 then
                            findValueQ2 1 "."
                        else if fst maxQ = 3 then
                            findValueQ3 (height - 2) "."
                        else
                            findValueQ4 (height - 2) "."
                    else if isInSection q1 "A" && (float tallyQ1) > (avg - 2.0) then
                        // if my quadrant is above average, stay...
                        findValue 1 "."
                    else if isInSection q2 "A" && (float tallyQ2) > (avg - 2.0) then
                        findValueQ2 1 "."
                    else if isInSection q3 "A" && (float tallyQ3) > (avg - 2.0) then
                        findValueQ3 (height - 2) "."
                    else if isInSection q4 "A" && (float tallyQ4) > (avg - 2.0) then
                        findValueQ4 (height - 2) "."
                    else
                        // minimax (sort of), go to the quadrant that will hurt B the most
                        if BisInQuadrant = 1 || BisInQuadrant = 4 then
                            // which is best between 2 and 3, and are they close to average
                            if (tallyQ2 > tallyQ3) && (float tallyQ2 > (avg - (avg / 3.0))) then
                                findValueQ2 1 "."
                            else if (float tallyQ3 > (avg - (avg / 3.0))) then
                                findValueQ3 (height - 2) "."
                            else if (float tallyQ4 > (avg - (avg / 3.0))) then
                                findValueQ4 (height - 2) "."
                            else
                                findValue 1 "."
                        else if BisInQuadrant = 2 || BisInQuadrant = 3 then
                            if (tallyQ1 > tallyQ4) && (float tallyQ1 > (avg - (avg / 3.0))) then
                                findValue 1 "."
                            else if (float tallyQ4 > (avg - (avg / 3.0))) then
                                findValueQ4 (height - 2) "."
                            else if (float tallyQ3 > (avg - (avg / 3.0))) then
                                findValueQ3 (height - 2) "."
                            else
                                findValueQ2 1 "."
                        else
                            if fst maxQ = 1 then
                                findValue 1 "."
                            else if fst maxQ = 2 then
                                findValueQ2 1 "."
                            else if fst maxQ = 3 then
                                findValueQ3 (height - 2) "."
                            else
                                findValueQ4 (height - 2) "."
                else
                    None

                    
            if (target_xy = None) then
//                let startTT = System.DateTime.Now
//                let acc_tt = buildTT 0 board [A] startTT target_xy.Value { xy = A; value = 0; parent = None }
//                printfn "build TT duration: %A" (System.DateTime.Now - startTT)
//                let optimalRoute (map:tt) = 
//                    Map.toSeq map
//                    |> Seq.maxBy (fun (xy,routes) -> routes |> List.maxBy (fun route -> route.value))
//                    |> (fun (xy,routes) -> routes)
//                    |> List.maxBy (fun route -> route.value)
//
//                let myOptimalRoute = optimalRoute acc_tt
//                printBoard (printPath myOptimalRoute.path board)
//                nextMove myOptimalRoute.path
                defaultToBFS
            else
                let startTT = System.DateTime.Now
                let acc_tt = buildTT 0 board [A] startTT target_xy.Value { xy = A; value = 0; parent = None }
#if DEBUG
                printfn "build TT duration: %A" (System.DateTime.Now - startTT)
#endif
                if (acc_tt.TryFind(target_xy.Value) = None) then
                    defaultToBFS
                else
                    let routesToTarget = acc_tt.[target_xy.Value] |> List.sortBy (fun route -> route.moves)
                    let minMovesRouteToTarget = routesToTarget |> List.minBy (fun route -> route.moves)    
                    let myOptimalRoute = (leastMoves (maxRoutes acc_tt (maxValue acc_tt)))
                    let myOptimalRouteToTarget = optimalRouteToTarget minMovesRouteToTarget routesToTarget.[0].moves searchDepth routesToTarget

#if DEBUG
                    printfn "routes to target %A: %d" target_xy acc_tt.[target_xy.Value].Length
                    System.Console.ReadLine()
                    let sortedRoutes = acc_tt.[target_xy.Value] |> List.sortBy (fun route -> route.moves)
                    for route in sortedRoutes do
                        System.Console.Clear()
                        printBoard (printPath route.path board)
                        printfn "current route..."
                        printfn "moves: %d value: %d" route.moves route.value
                        printfn ""
                        printfn "optimal route..."
                        printfn "moves: %d value: %d" myOptimalRouteToTarget.moves myOptimalRouteToTarget.value
                        System.Console.ReadLine()

                    System.Console.Clear()
                    printfn "transposition table:"
                    acc_tt |> Map.iter (fun (x,y) routes -> printfn "(%d,%d) - %d routes, (min,moves) %A" x y routes.Length (routes |> List.minBy (fun route -> route.value) |> (fun route -> (route.value, route.moves))))

                    System.Console.ReadLine()
                    System.Console.Clear()

                    printfn "optimal route to target %A:" target_xy
                    printBoard (printPath myOptimalRouteToTarget.path board)
                    System.Console.ReadLine()
                    System.Console.Clear()

                    printfn "optimal route node:"
                    printfn "%A" myOptimalRouteToTarget

                    System.Console.ReadLine()
                    System.Console.Clear()
                    printBoard (printPath myOptimalRouteToTarget.path board)
                    System.Console.ReadLine()
                    System.Console.Clear()
#endif

                    nextMove myOptimalRouteToTarget.path

    // write history
    let history' = { history with round = history.round + 1; }
    System.IO.File.WriteAllText(historyFile, history'.round.ToString() + "," + history'.opponentWentUp.ToString().ToUpper())

    let myBoard = 
        if history.opponentWentUp && history.round = poisonAtDown then // Poison pill move
            newBoard board 'A' myMove true
        else if not history.opponentWentUp && history.round = poisonAtUp then
            newBoard board 'A' myMove true
        else
            newBoard board 'A' myMove false
#if DEBUG
    printfn "board to be written out:"
    printBoard myBoard
    printfn "total duration: %A" (System.DateTime.Now - megatronStart)
    printfn "====================================================================="
#endif
    System.IO.File.WriteAllLines("game.state", myBoard)
    0