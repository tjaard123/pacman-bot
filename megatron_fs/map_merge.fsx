type tt = Map<(int*int),int list> // Transposition table type

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


let tt1 = Map.empty.Add((0,0), [1])
let tt2 = Map.empty.Add((0,0), [2;3]).Add((0,1), [1])
printfn "Normal: %A" (mergeTT tt1 tt2)
printfn "Reverse: %A" (mergeTT tt2 tt1)

printfn "Empty: %A" (mergeTT Map.empty tt2)
printfn "Empty: %A" (mergeTT tt2 Map.empty)
printfn "Empty: %A" (mergeTT Map.empty Map.empty)
