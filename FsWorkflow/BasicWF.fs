module BasicWF

open System;
open Printf;

let message (msg : string) = printfn "%s" msg

let inputBox msg map =
    message msg
    seq {
        while true do yield Console.ReadLine()
    }
    |> Seq.map map 
    |> Seq.choose id
    |> Seq.head

let stringInputBox msg = 
    inputBox msg (fun x -> if String.IsNullOrEmpty(x) then None else Some x)

let messageBoxOK msg =
    message msg
    Console.ReadLine()

type YesNoResult =
    | Yes
    | No

let messageBoxYesNo msg =
    message msg
    inputBox
        "[Y]es  [N]o" 
        (fun s -> 
            (Seq.tryHead s) 
            |> Option.bind (fun c ->
                match c with
                | 'Y' | 'y' -> Some Yes
                | 'N' | 'n' -> Some No
                | _ -> None))



