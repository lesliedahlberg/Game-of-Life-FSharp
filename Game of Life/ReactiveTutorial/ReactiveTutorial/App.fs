module MainApp

open System
open System.Windows
open System.Windows.Controls

open FSharpx

type MainWindow = XAML<"MainWindow.xaml">

let c = 16

let loadWindow() =
    let window = MainWindow()
   
    let rec popRow r c i =
        if i < c
        then
            let b = System.Windows.Controls.Primitives.ToggleButton()
            Grid.SetRow(b, r)
            Grid.SetColumn(b, i)
            b.Height <- 20.0
            ignore (window.gameGrid.Children.Add(b))
            popRow r c (i+1)
        else
            ignore 0

    let rec popRows r c i =
        if i < r
        then
            popRow i c 0
            popRows r c (i+1)
        else
            ignore 0

    popRows 16 16 0

    let xy2i (x,y) c =
        y*c+x
    let setCell (x,y) b =
        window.gameGrid.Children.Item(xy2i (x,y) c).SetValue(Controls.Primitives.ToggleButton.IsCheckedProperty, b)
    let getCell (x,y) =
        System.Convert.ToBoolean(window.gameGrid.Children.Item(xy2i (x,y) c).GetValue(Controls.Primitives.ToggleButton.IsCheckedProperty))
    let live (x,y) =
        setCell (x,y) true
    let die (x,y) =
        setCell (x,y) false
    let alive (x,y) =
        getCell (x,y)
    let dead (x,y) =
        not (alive (x,y))

    let boolRow r =
        [| for j in 0 .. 15 -> alive (j, r) |]
    let boolRows =
        (boolRow 1)

    let boolGrid n =
        Array2D.init n n (fun i j -> (alive (i, j)))

    let bool2int b =
        if b then 1 else 0

    let getCellSafe (x, y) l1 l2 (g:bool [,]) =
        bool2int (if x >= l1 && x <= l2 && y >= l1 && y <= l2
                    then g.[x, y]
                    else false)
        

    let step (x, y) g = 
        let a = 
            getCellSafe (x-1, y) 0 15 g +
            getCellSafe (x-1, y+1) 0 15 g +
            getCellSafe (x-1, y-1) 0 15 g +
            getCellSafe (x+1, y) 0 15 g +
            getCellSafe (x+1, y+1) 0 15 g +
            getCellSafe (x+1, y-1) 0 15 g +
            getCellSafe (x, y+1) 0 15 g +
            getCellSafe (x, y-1) 0 15 g
        if a < 2
        then
            die (x, y)
            1
        else if a > 3
        then
            die (x, y)
            1
        else if a = 3
        then
            live (x, y)
            1
        else 0

    let rec stepRow r c i g =
        if i < c
        then
            (step (i, r) g) + (stepRow r c (i+1) g)
        else
            0

    let rec stepRows r c i g =
        if i < r then
            (stepRow i c 0 g) + (stepRows r c (i+1) g)
        else 0

    let isInf _ =
        System.Convert.ToBoolean(window.inf.GetValue(Controls.Primitives.ToggleButton.IsCheckedProperty))

    let play n = 
        async{
            let rec play1 n = 
                if n > 0
                then
                    let a = (stepRows 16 16 0 (boolGrid 16))
                    if a > 0 then a + (play1 (n-1)) else a
                else 0
            in ignore (play1 n)
        } |> Async.StartImmediate
                
             
    let getSliderN _ =
        if isInf 0 then 100 else int (window.slider.Value)
        

    let play1Click x = 
        ignore (play 1)

    let playNClick x = 
        
            ignore (play (getSliderN 0))
        
        
        
    let rec killRow r c i =
        if i < c
        then
            die (i, r)
            killRow r c (i+1)
        else
            ignore 0

    let rec killRows r c i =
        if i < r then
            killRow i c 0
            killRows r c (i+1)
        else ignore 0

    let kill x =
        ignore (killRows 16 16 0)
       
    
    window.play1.Click.Add(play1Click)
    window.playN.Click.Add(playNClick)
    
    window.kill.Click.Add(kill)

    
   

    window.Root

[<STAThread>]
(new Application()).Run(loadWindow()) |> ignore