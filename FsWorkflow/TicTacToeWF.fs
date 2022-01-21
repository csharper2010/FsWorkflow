module TicTacToeWF

open TicTacToe
open BasicWF

let showBoard board = 
    board 
        |> getRows 
        |> Seq.iter (fun r -> 
            r |> Seq.iter (fun (i, p) -> 
                match p with 
                | Some player -> printf " %A " player 
                | None -> printf " %i " (i + 1))
            printfn "")

type Move =
    | Cell of int
    | Cancel

let ticTacToeGame () =
    let rec loop state =
        System.Console.Clear()
        printfn "TicTacToe"
        let (nextState, result) = 
            match state with 
            | Finished finished ->
                showBoard finished.Board
                match finished.Winner with
                | Some player -> message $"Player %A{player} won!"
                | None -> message "There was no winner!"
                (state, Some ())
            | Playing playing ->
                showBoard playing.Board
                let nextMove = 
                    inputBox
                        $"What's %A{playing.Next}'s next move?\n\
                        [1]-[9] for selected cell\n\
                        [C] to cancel game"
                        (fun s -> 
                            match System.Int32.TryParse s with
                            | (true, i) 
                                when i >= 1 && i <= 9 -> Some (Cell (i - 1))
                            | (false, _) 
                                when s 
                                    |> Seq.tryHead 
                                    |> Option.filter (fun c -> c = 'C' || c = 'c') 
                                    |> Option.isSome -> Some Cancel
                            | _ -> None)
                match nextMove with
                | Cell c -> 
                    (state |> move playing.Next c, None)
                | Cancel -> 
                    (state, Some ())

        match result with 
        | Some value -> value
        | None -> loop nextState

    loop <| startGame()

let ticTacToe () = 
    let rec loop () =
        ticTacToeGame ()
        let finished = match messageBoxYesNo "Do you want to play another game?" with | Yes -> None | No -> Some ()

        match finished with
        | Some value -> value
        | None -> loop ()
    
    loop ()