type tt = Map<(int*int),int list> // Transposition table type

let join (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

let rec dive depth (acc_tt:tt) node =
    if depth = 4 then 
        if acc_tt.ContainsKey(node) then // Maybe this check is not required
            acc_tt.Add(node, depth :: acc_tt.[node])
        else
            acc_tt.Add(node, [depth])
    else
        let moves = validMoves board node
        dive (depth + 1) (join acc_tt (acc_tt.Add(node, [depth]))) moves.[0]

let rec dive' depth (acc_tt:tt) node =
    if depth = 4 then 
        if acc_tt.ContainsKey(node) then // Maybe this check is not required
            acc_tt.Add(node, depth :: acc_tt.[node])
        else
            acc_tt.Add(node, [depth])
        //[ { xy = node; routes = [ { depth = depth }] } ]
    else
        let moves = validMoves board node

        join (acc_tt.Add(node, [depth])) (dive' (depth + 1) acc_tt moves.[0])
        //dive (depth + 1) (join acc_tt (acc_tt.Add(node, [depth]))) moves.[0]

        //dive (depth - 1) moves.[0] @ dive (depth - 1) moves.[1]
        //moves |> List.fold (fun acc move -> (dive (depth - 1) move) @ acc) []