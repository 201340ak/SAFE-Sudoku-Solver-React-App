module Client

open System.Collections.Generic

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Shared

type Msg =
| Solve
| Init of Result<Sudoku, exn>
let toSudoku x : Sudoku = 
    x
    |> Seq.map Seq.toArray
    |> Seq.toArray
let InitialSudokuState : Sudoku =
    [[0; 0; 8;  3; 0; 0;  6; 0; 0]
     [0; 0; 4;  0; 0; 0;  0; 1; 0]
     [6; 7; 0;  0; 8; 0;  0; 0; 0]

     [0; 1; 6;  4; 3; 0;  0; 0; 0]
     [0; 0; 0;  7; 9; 0;  0; 2; 0]
     [0; 9; 0;  0; 0; 0;  4; 0; 1]

     [0; 0; 0;  9; 1; 0;  0; 0; 5]
     [0; 0; 3;  0; 5; 0;  0; 0; 2]
     [0; 5; 0;  0; 0; 0;  0; 7; 4]] |> toSudoku


let init () : Sudoku * Cmd<Msg> =
  let model = InitialSudokuState
  let cmd =
    Cmd.ofPromise 
      (fetchAs<int array array> "/api/init") 
      [] 
      (Ok >> Init) 
      (Error >> Init)
  model, cmd

let rows = id
let cols (sudoku:Sudoku) =
    sudoku
    |> Array.mapi (fun a row -> row |> Array.mapi (fun b cell -> sudoku.[b].[a]))

let getBoxIndex count row col = 
   let n = row/count
   let m = col/count
   n * count + m

let boxes (sudoku:Sudoku) = 
    let d = sudoku |> Array.length |> float |> System.Math.Sqrt |> int
    let list = new List<_>()
    for a in 0..(d*d) - 1 do list.Add(new List<_>())

    for a in 0..(Array.length sudoku - 1) do
        for b in 0..(Array.length sudoku - 1) do
            list.[getBoxIndex d a b].Add(sudoku.[a].[b])

    list 
      |> Seq.map Seq.toArray
let allUnique numbers =
    let set = new HashSet<_>()
    numbers
    |> Seq.filter ((<>) 0)
    |> Seq.forall set.Add

let solvable sudoku =
    rows sudoku
    |> Seq.append (cols sudoku)
    |> Seq.append (boxes sudoku)
    |> Seq.forall allUnique

let replaceAtPos (x:Sudoku) row col newValue :Sudoku =     
    [| for a in 0..(Array.length x - 1) ->
        [| for b in 0..(Array.length x - 1) -> 
            if a = row && b = col then newValue else x.[a].[b] |] |]


let rec substitute row col (x:Sudoku) = 
    let a,b = if col >= Array.length x then row+1,0 else row,col
    if a >= Array.length x then seq { yield x } else
    if x.[a].[b] = 0 then 
        [1..Array.length x]           
            |> Seq.map (replaceAtPos x a b)  
            |> Seq.filter solvable                     
            |> Seq.map (substitute a (b+1))
            |> Seq.concat
     else substitute a (b+1) x

let getFirstSolution = substitute 0 0 >> Seq.head

let update (msg : Msg) (model : Sudoku) : Sudoku * Cmd<Msg> = 
    let model' = 
        match msg with
        | Solve -> getFirstSolution model
        | Init (Ok x) -> model
    model', Cmd.none

let inputs (sudoku: Sudoku) =
  div [] 
    [for i in 0..sudoku.Length-1 ->
      div 
        [] 
        [for j in 0..sudoku.Length-1 ->
            input [
                MaxLength 1.
                Value 
                    (match sudoku.[i].[j] with
                     | 0 -> unbox ""
                     | v -> unbox (v.ToString()))
                OnChange 
                  (fun ev ->
                        sudoku.[i].[j] <- int (unbox ev.target))
                AutoFocus true]
        ]
    ]

let view (model : Sudoku) (dispatch : Msg -> unit) =
  div [] 
    [ h1 [] [unbox "Sudoku"]
      div [] 
       [inputs model
        br []
        button [
                  ClassName "button"
                  OnClick (fun _ -> dispatch Solve )
              ] [ unbox "Solve" ]
       ]
      ]


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

// This command is the thing that "binds" all the 
// elm-architected pieces together.
Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
