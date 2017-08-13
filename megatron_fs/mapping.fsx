type Node = { xy:(int*int); value:int; parent:Node Option }
type Route = { moves:int; value:int; path:Node; }
type tt = Map<(int*int),Route list> // Transposition table type

let dummyNode = { xy = (0,0); value = 1; parent = None }

let acc_tt = Map.empty
                .Add((0,0), [{ moves = 5; value = 4; path = dummyNode };
                             { moves = 7; value = 7; path = dummyNode };
                             { moves = 5; value = 3; path = dummyNode };
                             { moves = 5; value = 4; path = dummyNode };
                             { moves = 4; value = 3; path = dummyNode };
                             { moves = 7; value = 12; path = dummyNode }])

let rec findOptimalRoute optimalRoute minMoves maxMoves routes =
    if optimalRoute.value > optimalRoute.moves then
        optimalRoute
    else if minMoves = maxMoves then
        optimalRoute
    else
        let routesAtMinMoves = routes |> List.filter (fun route -> route.moves = minMoves)
        if routesAtMinMoves.Length = 0 then
            findOptimalRoute optimalRoute (minMoves + 1) maxMoves routes
        else
            let minMovesMaxValue = routesAtMinMoves |> List.maxBy (fun route -> route.value)
            findOptimalRoute minMovesMaxValue (minMoves + 1) maxMoves routes

let node00 = acc_tt.[(0,0)]
printfn "%A" (findOptimalRoute node00.[0] 3 50 node00)