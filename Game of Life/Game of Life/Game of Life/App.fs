module MainApp

open System
open System.Windows
open System.Windows.Media
open System.Windows.Controls
open System.Windows.Controls.Primitives
open FSharpx

(* 
    CELLS:
    ======
    - Create cells on a WPF ToggleButton Grid
    - Get/Set their status
*)
module cells =
    let coordsToSeqNum (x,y) columns = y*columns+x //Convert (x, y) coordinates to a linear array index
    let moldCell (x, y) (grid:Grid) = //Create a cell in the UI
        let cell = ToggleButton()
        Grid.SetColumn(cell, x)
        Grid.SetRow(cell, y)
        cell.Background <- SolidColorBrush(Color.FromScRgb(255.0f, 255.0f, 255.0f, 255.0f))
        ignore (grid.Children.Add(cell))
        1
    let rec moldCellRow row column columns (grid:Grid) = //Create a row of cells
        if column < columns
        then moldCell (column, row) grid + moldCellRow row (column+1) columns grid
        else 0
    let rec moldCellGrid row rows columns (grid:Grid) = //Create a grid of cells
        if row < rows
        then moldCellRow row 0 columns grid + moldCellGrid (row+1) rows columns grid
        else 0
    let setCellStatus (x,y) alive (grid:Grid) columns = //Set toggle value of cell
        grid.Children.Item(coordsToSeqNum (x,y) columns).SetValue(ToggleButton.IsCheckedProperty, alive) 
    let getCellStatus (x,y) (grid:Grid) columns = //Get toggle value of cell
        System.Convert.ToBoolean(grid.Children.Item(coordsToSeqNum (x,y) columns).GetValue(ToggleButton.IsCheckedProperty))
    let live (x,y) (grid:Grid) columns = setCellStatus (x,y) true grid columns //Make cell alive
    let die (x,y) (grid:Grid) columns = setCellStatus (x,y) false grid columns //Kill cell
    let alive (x,y) (grid:Grid) columns = getCellStatus (x,y) grid columns //Is cell alive?
    let dead (x,y) (grid:Grid) columns = not (alive (x,y) grid columns) //Is cell dead?
    let boolToInt value = if value then 1 else 0
    let cellGrid scale (grid:Grid) = Array2D.init scale scale (fun x y -> (alive (x, y) grid scale)) //Cell toggle values as bool grid
    let getCellFromGrid (x, y) scale (cellGrid:bool [,]) = //Get cell toggle value from grid with out of bounds checks
        (if x >= 0 && x < scale && y >= 0 && y < scale then cellGrid.[x, y] else false) |> boolToInt

(* 
    GAME OF LIFE:
    ======
    - Cycle through the cell grid
    - Clear the cell grid
*)
module gameOfLife =
    let cycleCell (x, y) scale (cellGrid: bool [,]) (grid:Grid) = //Run through a cells life cycle
        let a = 
            cells.getCellFromGrid (x-1, y) scale cellGrid +
            cells.getCellFromGrid (x-1, y+1) scale cellGrid +
            cells.getCellFromGrid (x-1, y-1) scale cellGrid +
            cells.getCellFromGrid (x+1, y) scale cellGrid +
            cells.getCellFromGrid (x+1, y+1) scale cellGrid +
            cells.getCellFromGrid (x+1, y-1) scale cellGrid +
            cells.getCellFromGrid (x, y+1) scale cellGrid +
            cells.getCellFromGrid (x, y-1) scale cellGrid
        if a < 2
        then
            cells.die (x, y) grid scale
            let t = cellGrid.[x, y]
            if t then 1 else 0
        else if a > 3
        then
            cells.die (x, y) grid scale
            let t = cellGrid.[x, y]
            if t then 1 else 0
        else if a = 3
        then
            cells.live (x, y) grid scale
            let t = not (cellGrid.[x, y])
            if t then 1 else 0
        else 0
    let rec cycleRow row column columns scale (cellGrid:bool [,]) (grid:Grid) = //Cycle through a row of cells life cycle
        if column < columns
        then cycleCell (column, row) scale cellGrid grid + cycleRow row (column+1) columns scale cellGrid grid
        else 0
    let rec cycleGrid row rows columns scale (cellGrid:bool [,]) (grid:Grid) = //Cycle through a grid of cells life cycle
        if row < rows
        then cycleRow row 0 columns scale cellGrid grid + cycleGrid (row+1) rows columns scale cellGrid grid
        else 0
    let rec resetRow row column columns scale (grid:Grid) = //Reset a row of cells toggle values
        if column < columns
        then
            cells.die (column, row) grid scale
            1 + resetRow row (column+1) columns scale grid
        else 0
    let rec resetGrid row rows columns scale (grid:Grid) = //Reset the grid of cells
        if row < rows
        then resetRow row 0 columns scale grid + resetGrid (row+1) rows columns scale grid
        else 0

(* 
    UI:
    ======
    - Get variables from UI elements
*)
module UI =
    type MainWindow = XAML<"MainWindow.xaml">
    let getN (slider:Slider) = int (slider.Value) //Get the number of times to run the game
    let isNInf (inf:ToggleButton) = System.Convert.ToBoolean(inf.GetValue(ToggleButton.IsCheckedProperty)) //Is N = infinity ?

(* 
    LOAD WINDOW:
    ======
    - Initialize cell grid
    - Connect UI with internal functions
*)
let loadWindow() =
    let window = UI.MainWindow() //Create window
    let grid = window.gameGrid //Grid
    let scale = 16 //Size of grid
    let cellCount = cells.moldCellGrid 0 scale scale window.gameGrid //Initialize cell grid

    let cycle _ = gameOfLife.cycleGrid 0 scale scale scale (cells.cellGrid scale grid) grid //Run one cycle
    let rec cycleInf _ = if (cycle 0) > 0 && UI.isNInf window.inf then cycleInf 0 //Run inifinately
    let rec cycleN n = if n > 0 then (let progress = cycle 0 in if progress <> 0 then progress + (cycleN (n-1)) else progress) else 0 //Run N times
    
    let cycle1 _ = cycle 0 |> ignore //Callback for play once
    let reset _ = gameOfLife.resetGrid 0 scale scale scale grid |> ignore //Callback for reset grid
    let cycleM _ = if UI.isNInf window.inf then cycleInf 0 else cycleN (UI.getN window.slider) |> ignore //Callback for play N

    window.play1.Click.Add(cycle1)
    window.playN.Click.Add(cycleM)
    window.kill.Click.Add(reset)

    window.Root

[<STAThread>]
(new Application()).Run(loadWindow()) |> ignore