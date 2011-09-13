#r "WindowsBase"
#r "PresentationCore"
#r "PresentationFramework"
#r "System.Xaml"

//============================================================================================
//  UI/Drawing
//============================================================================================
open System
open System.Windows
open System.Windows.Controls
open System.Windows.Shapes
open System.Windows.Media
open System.Windows.Media.Imaging

let c = new Canvas(Margin = new Thickness(20.,40.,10.,10.))
let w = new Window(Content = c, Width = 1024.0, Height = 768.0,Background = Brushes.Gray)
w.Show()

let spriteBrush imgFile nCols nRows id =
    let imgSource = new BitmapImage(new Uri(imgFile))
    let cardW, cardH = 1./(float nCols), 1./(float nRows)
    let viewBox = new Rect(cardW*(float (id % 12)), cardH*(float (id / 12)), cardW, cardH)
    let brush = new ImageBrush(ImageSource = imgSource, Viewbox = viewBox)
    brush

type SpriteAtlas = { 
  File: string;
  Cols: int;
  Rows: int;
  Frames: int;
  FrameWidth: float;
  FrameHeight: float; 
}

let bgAtlas = {
  File = "brick.png";
  Cols = 2; Rows = 1; Frames = 2;
  FrameWidth = 74.; FrameHeight = 85.;
}

let cardAtlas = {
  File = "cards.png";
  Cols = 12; Rows = 8; Frames = 70;
  FrameWidth = 64.; FrameHeight = 60.;
}

let cellSz = 64., 75.
let levShift = -7., -10.

let createBrick x y id =
  let bgBrush = spriteBrush (__SOURCE_DIRECTORY__ + "/" + "brick.png") 2 1 0
  let bg = new Rectangle(Width=74., Height=85., Fill = bgBrush)
  Canvas.SetLeft(bg, x)
  Canvas.SetTop(bg, y)   
  
  let imgBrush = spriteBrush (__SOURCE_DIRECTORY__ + "/" + "cards.png") 12 8 id
  let fg = new Rectangle(Width=64., Height=60., Fill = imgBrush)
  Canvas.SetLeft(fg, x + 2.)
  Canvas.SetTop(fg, y + 10.)   
  (bg, fg)

let clear() = c.Children.Clear()

let cellCoord (i, j, level) =
  let x = (float i)*(fst cellSz)*0.5 + (float level)*(fst levShift)
  let y = (float j)*(snd cellSz)*0.5 + (float level)*(snd levShift)
  (x, y, fst cellSz - fst levShift, snd cellSz - snd levShift)

let createCell (id, (i, j, level)) =
  let (x, y, _, _) = cellCoord(i, j, level)
  createBrick x y id

//============================================================================================
//  Layouts  
//============================================================================================
let turtle = "
  111111111111111111111111  
  111111111111111111111111      
      1122222222222211
      1122222222222211
    11112233333333221111
    11112233333333221111
  111111223344443322111111
111111112233455433221111111111
111111112233455433221111111111
  111111223344443322111111
    11112233333333221111
    11112233333333221111        
      1122222222222211
      1122222222222211
  111111111111111111111111  
  111111111111111111111111
"

let dragon = "
  224422              224422
2222442233          3322442222
22      33112233221133      22
44  22    1122332211    22  44 
44  22    22      22    22  44 
22        22      22        22
222244221122  22  221122442222
  2244221122  22  2211224422
      22  22      22  22
      22  22      22  22
    4411112222222222111144
    4411112222222222111144
    44                  44
    44  44  44  44  44  44
    3311441144114411441133
    3311  11  11  11  1133
"

let crab = "
 11222222          22222211
 11233332          23333211
 11233332   2332   23333211 
 11222222   2332   22222211  
     112211111111112211
     112211111111112211
2222     1122332211     2222
2222     1122332211     2222
22      112234432211      22
22      112234432211      22
    11112222344322221111
    11112222344322221111
11222222  11111111  22222211
11233332  11111111  23333211
11233332   223322   23333211
11222222   223322   22222211
"

let spider ="
     2233   22  22   3322     
     2233   22  22   3322     
       22    1111    22       
 33    2233  1111  3322    33 
 3322    331122221133    2233 
   223322  11222211  223322   
     33221122333322112233     
         112233332211         
        11223333332211        
332233221122334433221122332233
33223322 112234432211 22332233
         112233332211         
       22  11333311  22       
     3322  11333311  2233     
 332233      2222      332233 
 3322        2222        2233
"

let parseLayout (str:string) = 
  let charToHeight ch = 
    match Int32.TryParse(string ch) with
    | (true, n) -> n
    | _ -> -1
  let strToRow (s:string) =
    s.TrimEnd(' ').ToCharArray()
    |> Array.map charToHeight 
  str.Split('\n') 
  |> Array.map strToRow
  |> Array.filter (fun r -> r.Length <> 0)
 
let layoutToCellCoord (layout) = 
  let maxLevel = layout |> Array.maxBy Array.max |> Array.max
  seq {
    let l = Array.copy layout
    let shifts = [|(0, 0); (1, 0); (0, 1); (1, 1)|]
    for level in maxLevel .. -1 .. 1 do
      for row in 0 .. layout.Length - 2 do
        for col in 0 .. layout.[row].Length - 2 do
        let isBlock = Array.forall (fun (x,y) -> l.[row + x].[col + y] = level) shifts
        if isBlock then
          yield (col, row, level)
          shifts |> Array.iter (fun (x, y) -> l.[row + x].[col + y] <- level - 1) 
  }

let cellCoords = 
  turtle 
  |> parseLayout 
  |> layoutToCellCoord 
  |> Seq.sortBy (fun (x, y, h) -> x + y + h*1000) 
  |> Seq.toArray

//============================================================================================
//  Generic utils
//============================================================================================
let shuffle arr =
  let a = arr |> Array.copy
  let rand = new System.Random()
  let flip i _ =
    let j = rand.Next(i, Array.length arr)
    let tmp = a.[i]
    a.[i] <- a.[j]
    a.[j] <- tmp
  a |> Array.iteri flip
  a

let tryFindLastIndexi f (array : _[]) = 
  let rec searchFrom n = 
    if n < 0 then None 
    elif f n array.[n] then Some n else searchFrom (n - 1)
  searchFrom (array.Length - 1)

let choosei f (array: _[]) =
  let res = new System.Collections.Generic.List<_>()
  for i = 0 to array.Length - 1 do 
      let x = array.[i] 
      match f (i, x) with
      | Some v -> res.Add(v)
      | None -> ignore()
  res.ToArray()

//============================================================================================
//  MUTABLE STATE
//============================================================================================
type CardState =
  | Visible
  | Selected
  | Hidden

let cardStates = [|for x in 1 .. cellCoords.Length do yield Visible|]
let mutable curSelected:int option = None

let cardIDs = 
  let c1 = [|1..70|] |> shuffle 
  let c2 = c1.[1 .. (Array.length cellCoords)/4]
  c2 |> Array.append c2 |> Array.append c2 |> Array.append c2 |> shuffle

clear()
let cardParts = 
  cellCoords
  |> Array.zip cardIDs
  |> Array.map createCell

cardParts 
  |> Array.iter (fun (bg, fg) -> c.Children.Add(bg) |> ignore; c.Children.Add(fg) |> ignore)


let isFree cardID = 
  let m = System.Collections.Generic.Dictionary()
  let addCell i (x, y, h) = 
    [|(0, 0); (1, 0); (0, 1); (1, 1)|]
    |> Array.iter (fun (dx, dy) -> m.Add((x + dx, y + dy, h), i))
  cellCoords |> Array.iteri addCell
  let (x, y, h) = cellCoords.[cardID]
  let isBlockedBy offsets = 
    let isBlocking (dx, dy, dh) =
      let key = (x + dx, y + dy, h + dh)
      if m.ContainsKey key then cardStates.[m.[key]] <> Hidden else false
    offsets |> List.exists isBlocking
  let top = [(0, 0, 1); (0, 1, 1); (1, 0, 1); (1, 1, 1)]
  let left = [(-1, 0, 0); (-1, 1, 0)]
  let right = [(2, 0, 0); (2, 1, 0)]
  not (isBlockedBy top || (isBlockedBy left && isBlockedBy right)) 

let setCardState state cellID =
  cardStates.[cellID] <- state
  let spriteID = if state = Selected then 1 else 0
  let selBrush = spriteBrush (__SOURCE_DIRECTORY__ + "/brick.png") 2 1 spriteID
  match state with
  | Hidden -> (fst cardParts.[cellID]).Fill <- null; (snd cardParts.[cellID]).Fill <- null
  | _ -> (fst cardParts.[cellID]).Fill <- selBrush

let getFree () =
  [0 .. cellCoords.Length - 1]
  |> List.filter (fun i -> cardStates.[i] <> Hidden)
  |> List.filter isFree

let getMatches () = 
  let free = getFree () 
  free
  |> Set.ofList 
  |> Set.filter (fun i -> 
    (List.sumBy (function 
      | a when cardIDs.[a] = cardIDs.[i] -> 1 
      | _ -> 0) free) > 1)
  |> Seq.toList

let showFree = getFree >> List.iter (setCardState Selected)
let showMatches = getMatches >> List.iter (setCardState Selected)

let shuffleRemaining () =
  let visibleIdx = cardStates |> choosei (function | idx, Visible -> Some idx | _ -> None)
  visibleIdx
  |> Array.map (fun idx -> cardIDs.[idx])
  |> shuffle
  |> Array.iteri (fun i id -> 
    let idx = visibleIdx.[i]
    let imgBrush = 
      match cardStates.[idx] with
      | Hidden -> null
      | _ -> spriteBrush (__SOURCE_DIRECTORY__ + "/" + "cards.png") 12 8 id
    (snd cardParts.[idx]).Fill <- imgBrush
    cardIDs.[idx] <- id)

let pickCell mx my = 
    cellCoords 
    |> Array.map cellCoord 
    |> tryFindLastIndexi (fun n (x, y, w, h) -> 
      cardStates.[n] <> Hidden && mx > x && my > y && mx < x + w && my < y + h)

let handleClick (mx, my) = 
  let cell = match pickCell mx my with
             | Some cellIdx when isFree cellIdx -> Some cellIdx
             | _ -> None
  let unselectAll () = 
    curSelected <- None
    [|0 .. cellCoords.Length - 1|] 
    |> Array.filter (fun i -> cardStates.[i] <> Hidden) 
    |> Array.iter (setCardState Visible)
  match cell, curSelected with
  | Some c, Some s when c = s -> unselectAll ()
  | Some c, Some s when cardIDs.[c] = cardIDs.[s] -> 
      unselectAll (); setCardState Hidden c; setCardState Hidden s;
      if (cardStates |> Array.forall (fun st -> st = Hidden)) then 
        MessageBox.Show "Amazing, you've won in this impossible game!" |> ignore
      elif (getMatches().Length = 0) then 
        MessageBox.Show "No more possible moves. You've lost, sorry" |> ignore
        shuffleRemaining()
  | Some c, None -> setCardState Selected c; curSelected <- Some(c)
  | _, _ -> unselectAll ()

let events = 
  w.MouseDown
  |> Event.filter (fun mi -> (mi.ChangedButton = Input.MouseButton.Left && 
                              mi.ButtonState = Input.MouseButtonState.Pressed))
  |> Event.map (fun mi -> (mi.GetPosition(c).X, mi.GetPosition(c).Y))
  |> Event.add handleClick
 