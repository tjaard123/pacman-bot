let board = ".A. B"

let A = 1
let B = 4

let getChildren (node:char) (board:string) =
    [board.IndexOf(node) - 1; board.IndexOf(node) + 1]
    |> List.filter (fun p -> p > -1 && p < board.Length)

let getValue char =
    match char with
    | '.' -> 1
    | _ -> 0

let listToString (list:char list) = list |> List.fold (fun acc c -> acc + c.ToString()) ""
let newBoard (board:string) player move =
    [0..board.Length - 1] 
    |> List.map (fun i -> if i = move then player else if board.[i] = player then ' ' else board.[i]) 
    |> listToString

// minimax
let rec minimax depth maximizingPlayer (board:string) (board':string) node =
    if depth = 0 then 
        printfn "%d: |%s|.[%d] = %d" depth board node (getValue board.[node])
        getValue board.[node]
    else if maximizingPlayer then
        let list = (getChildren 'A' board') |> List.map (fun move -> minimax (depth - 1) false board (newBoard board' 'A' move) move)
        let max = List.max (-100 :: list)
        printfn "%d: |%s| A options: = %A" depth board' list
        max
    else
        let list = (getChildren 'B' board') |> List.map (fun move -> minimax (depth - 1) true board (newBoard board' 'B' move) move)
        let min = List.min (100 :: list)
        printfn "%d: |%s| B options: = %A" depth board' list
        min

minimax 4 true board board A