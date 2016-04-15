module MainApp

open System
open System.Windows
open System.Windows.Media
open System.Windows.Controls
open System.Windows.Controls.Primitives
open FSharpx

module cells =
    let coordsToSeqNum (x,y) columns = y*columns+x
    let moldCell (x, y) (grid:Grid) =
        let cell = ToggleButton()
        Grid.SetColumn(cell, x)
        Grid.SetRow(cell, y)
        cell.Background <- SolidColorBrush(Color.FromScRgb(255.0f, 255.0f, 255.0f, 255.0f))
        ignore (grid.Children.Add(cell))
        1
    let rec moldCellRow row column columns (grid:Grid) =
        if column < columns
        then moldCell (column, row) grid + moldCellRow row (column+1) columns grid
        else 0
    let rec moldCellGrid row rows columns (grid:Grid) =
        if row < rows
        then moldCellRow row 0 columns grid + moldCellGrid (row+1) rows columns grid
        else 0
    let setCellStatus (x,y) alive (grid:Grid) columns =
        grid.Children.Item(coordsToSeqNum (x,y) columns).SetValue(ToggleButton.IsCheckedProperty, alive) 
    let getCellStatus (x,y) (grid:Grid) columns =
        System.Convert.ToBoolean(grid.Children.Item(coordsToSeqNum (x,y) columns).GetValue(ToggleButton.IsCheckedProperty))
    let live (x,y) (grid:Grid) columns = setCellStatus (x,y) true grid columns
    let die (x,y) (grid:Grid) columns = setCellStatus (x,y) false grid columns
    let alive (x,y) (grid:Grid) columns = getCellStatus (x,y) grid columns
    let dead (x,y) (grid:Grid) columns = not (alive (x,y) grid columns)
    let boolToInt value = if value then 1 else 0
    let cellGrid scale (grid:Grid) = Array2D.init scale scale (fun x y -> (alive (x, y) grid scale))
    let getCellFromGrid (x, y) scale (cellGrid:bool [,]) =
        (if x >= 0 && x < scale && y >= 0 && y < scale then cellGrid.[x, y] else false) |> boolToInt

module gameOfLife =
    let cycleCell (x, y) scale (cellGrid: bool [,]) (grid:Grid) = 
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
    let rec cycleRow row column columns scale (cellGrid:bool [,]) (grid:Grid) =
        if column < columns
        then cycleCell (column, row) scale cellGrid grid + cycleRow row (column+1) columns scale cellGrid grid
        else 0
    let rec cycleGrid row rows columns scale (cellGrid:bool [,]) (grid:Grid) =
        if row < rows
        then cycleRow row 0 columns scale cellGrid grid + cycleGrid (row+1) rows columns scale cellGrid grid
        else 0
    let rec resetRow row column columns scale (grid:Grid) =
        if column < columns
        then
            cells.die (column, row) grid scale
            1 + resetRow row (column+1) columns scale grid
        else 0
    let rec resetGrid row rows columns scale (grid:Grid) =
        if row < rows
        then resetRow row 0 columns scale grid + resetGrid (row+1) rows columns scale grid
        else 0

module UI =
    type MainWindow = XAML<"MainWindow.xaml">
    let getN (slider:Slider) = int (slider.Value)
    let isNInf (inf:ToggleButton) = System.Convert.ToBoolean(inf.GetValue(ToggleButton.IsCheckedProperty))

let loadWindow() =
    let window = UI.MainWindow()
    let grid = window.gameGrid
    let scale = 16
    let cellCount = cells.moldCellGrid 0 scale scale window.gameGrid
    let cycle _ = gameOfLife.cycleGrid 0 scale scale scale (cells.cellGrid scale grid) grid
    let cycle1 _ = cycle 0 |> ignore
    let rec cycleInf _ = if (cycle 0) > 0 && UI.isNInf window.inf then cycleInf 0
    let rec cycleN n = if n > 0 then (let progress = cycle 0 in if progress <> 0 then progress + (cycleN (n-1)) else progress) else 0
    let cycleM _ = if UI.isNInf window.inf then cycleInf 0 else cycleN (UI.getN window.slider) |> ignore
    let reset _ = gameOfLife.resetGrid 0 scale scale scale grid |> ignore
    window.play1.Click.Add(cycle1)
    window.playN.Click.Add(cycleM)
    window.kill.Click.Add(reset)
    window.Root

[<STAThread>]
(new Application()).Run(loadWindow()) |> ignore