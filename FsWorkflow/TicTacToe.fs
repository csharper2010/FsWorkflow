module TicTacToe

type Player =
    | X
    | O

type TicTacToeBoard = private TicTacToeBoard of List<Option<Player>>
module TicTacToeBoard =
    let createNew = TicTacToeBoard (List.replicate 9 None)
    let updateWith player pos (TicTacToeBoard state) = 
        if state |> List.item pos |> Option.isNone 
        then state |> List.updateAt pos (Some player) |> TicTacToeBoard |> Some
        else None

type TicTacToePlayingState = {
    Next : Player
    Board : TicTacToeBoard
    }

type TicTacToeFinishedState = {
    Winner : Option<Player>
    Board : TicTacToeBoard
    }

type TicTacToeState =
    | Playing of TicTacToePlayingState
    | Finished of TicTacToeFinishedState

let getRows (TicTacToeBoard state) =
    state |> Seq.indexed |> Seq.chunkBySize 3

let startGame () = Playing { Next = X; Board = TicTacToeBoard.createNew }
let getLines (cells : List<Option<Player>>) =
    seq {
        for startIndex in 0..3..8 do
            yield cells[startIndex..startIndex+2]
        for startIndex in 0..2 do
            yield cells |> Seq.indexed |> Seq.choose (fun (i, p) -> if i % 3 = startIndex then Some p else None) |> List.ofSeq
        for indexSet in [set [0; 4; 8]; set [2;4;6]] do
            yield cells |> Seq.indexed |> Seq.choose (fun (i, p) -> if indexSet |> Set.contains i then Some p else None) |> List.ofSeq
    }

let private getWinner (TicTacToeBoard board) =
    board 
    |> getLines 
    |> Seq.filter (fun l -> l.Head |> Option.isSome && not (l |> List.exists (fun i -> i <> l.Head))) 
    |> Seq.map (fun l -> l.Head)
    |> Seq.choose id
    |> Seq.tryHead

let move player pos state = 
    match state with 
    | Playing playing ->
        let updated = playing.Board |> TicTacToeBoard.updateWith player pos
        match updated with
        | Some updatedBoard ->
            match getWinner updatedBoard with
            | Some player -> Finished { Winner = Some player; Board = updatedBoard}
            | None -> 
                match updatedBoard with 
                | TicTacToeBoard cells when cells |> List.exists (fun x -> x.IsNone) ->
                    Playing { Next = (if playing.Next = X then O else X); Board = updatedBoard }
                | _ -> Finished { Winner = None; Board = updatedBoard }
        | None -> state
    | Finished _ -> state


