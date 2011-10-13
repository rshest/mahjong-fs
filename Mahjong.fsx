#r "WindowsBase"
#r "PresentationCore"
#r "PresentationFramework"
#r "System.Xaml"
#time 

//============================================================================================
//  General utils
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

let replicate numTimes s = 
  seq {for i in [1..numTimes] do yield! s}

let rec tryMap maxDepth f arr = 
  match maxDepth, (f arr) with
  | 0, _ -> None
  | _, Some s -> Some s
  | _, None -> tryMap (maxDepth - 1) f arr

//============================================================================================
//  UI/Drawing
//============================================================================================
open System
open System.Windows
open System.Windows.Controls
open System.Windows.Shapes
open System.Windows.Media
open System.Windows.Media.Imaging

let c = new Canvas(Margin = new Thickness(30.,50.,30.,50.))
let g = new StackPanel(Orientation=Orientation.Vertical)

let w = new Window(Content = g, Width = 1024.0, Height = 768.0,Background = Brushes.Gray)
w.Show()

type Control =
  | MenuItem of string

let s = new Menu();
let mitem = new MenuItem(Header="Turtle")
s.Items.Add(mitem);
s.Items.Add(new MenuItem(Header="Dragon"))
s.Items.Add(new MenuItem(Header="Spider"))
s.Items.Add(new MenuItem(Header="Crab"))

let menuFree = new MenuItem(Header="Show Free")
s.Items.Add(menuFree)

let menuMatches = new MenuItem(Header="Show Matches")
s.Items.Add(menuMatches)

let menuUndo = new MenuItem(Header="Undo")
s.Items.Add(menuUndo)

let menuShuffle = new MenuItem(Header="Shuffle")
s.Items.Add(menuShuffle)

s.Items.Add(new MenuItem(Header="Hide Level"))
s.Items.Add(new MenuItem(Header="Unhide Level"));

g.Children.Add(s);
g.Children.Add(c);

type SpriteAtlas = { 
  File: string;
  Cols: int;
  Rows: int;
  Frames: int;
  FrameWidth: float;
  FrameHeight: float; 
}

let spriteBrush atlas id =
  let imgFile = __SOURCE_DIRECTORY__ + "/" + atlas.File
  let imgSource = new BitmapImage(new Uri(imgFile))
  let cardW, cardH = 1./(float atlas.Cols), 1./(float atlas.Rows)
  let viewBox = new Rect(cardW*(float (id % atlas.Cols)), cardH*(float (id / atlas.Cols)), cardW, cardH)
  new ImageBrush(ImageSource = imgSource, Viewbox = viewBox)

let bgAtlas = {
  File = "brick.png";
  Cols = 2; Rows = 1; Frames = 2;
  FrameWidth = 74.; FrameHeight = 85.;
}

let fgAtlas = {
  File = "cards.png";
  Cols = 12; Rows = 8; Frames = 91;
  FrameWidth = 64.; FrameHeight = 60.;
}

let createBrick x y id =
  let bg = new Rectangle(Width=bgAtlas.FrameWidth, Height=bgAtlas.FrameHeight)
  Canvas.SetLeft(bg, x)
  Canvas.SetTop(bg, y)   
  let fg = new Rectangle(Width=fgAtlas.FrameWidth, Height=fgAtlas.FrameHeight)
  Canvas.SetLeft(fg, x + 2.)
  Canvas.SetTop(fg, y + 10.)   
  (bg, fg)

type CardState =
  | Visible
  | Selected
  | Hidden

let updateCardVisual parts id state = 
  let (bg:Rectangle), (fg:Rectangle) = parts
  let spriteID = if state = Selected then 1 else 0
  let selBrush = spriteBrush bgAtlas spriteID
  match state with
  | Hidden -> bg.Fill <- null; fg.Fill <- null
  | _ -> bg.Fill <- selBrush
  let imgBrush = 
    match state with
    | Hidden -> null
    | _ -> spriteBrush fgAtlas id
  fg.Fill <- imgBrush

let cellSz = 64., 75.
let levShift = -7., -10.

let cellCoord (i, j, level) =
  let lx, ly = levShift
  let x = (float i)*(fst cellSz)*0.5 + (float level)*lx
  let y = (float j)*(snd cellSz)*0.5 + (float level)*ly
  (x, y, fst cellSz - lx, snd cellSz - ly)

let createCell (id, (i, j, level)) =
  let (x, y, _, _) = cellCoord(i, j, level)
  let parts = createBrick x y id
  updateCardVisual parts id Visible
  parts

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

let layouts = [|turtle; dragon; crab; spider|]

let parseLayout (str:string) =   
  let charToHeight ch = 
    match Int32.TryParse(string ch) with
    | (true, n) -> n
    | _ -> -1
  let strToRow (s:string) =
    s.TrimEnd(' ').ToCharArray()
    |> Array.map charToHeight 
  let layout = 
    str.Split('\n') 
    |> Array.map strToRow
    |> Array.filter (fun r -> r.Length <> 0)
  let maxLevel = 
    layout 
    |> Array.maxBy Array.max 
    |> Array.max
  seq {
    let l = Array.copy layout
    let shifts = [|0, 0; 1, 0; 0, 1; 1, 1|]
    for level in maxLevel .. -1 .. 1 do
      for row in 0 .. layout.Length - 2 do
        for col in 0 .. layout.[row].Length - 2 do
        let isBlock = Array.forall (fun (x,y) -> l.[row + x].[col + y] = level) shifts
        if isBlock then
          yield (col, row, level)
          shifts |> Array.iter (fun (x, y) -> l.[row + x].[col + y] <- level - 1) 
  } |> Seq.toArray


//============================================================================================
//  CARD UTILS
//============================================================================================
let isFree coords = 
  //  create hash table with coordinates to quickly find neighbors
  let m = System.Collections.Generic.Dictionary()
  let addCell i (x, y, h) = 
    [|0, 0; 1, 0; 0, 1; 1, 1|]
    |> Array.iter (fun (dx, dy) -> m.Add((x + dx, y + dy, h), i))
  coords |> Array.iteri addCell
  //  return function which checks if given card is free (using the cached hash table)
  fun (states:CardState[]) cardID ->
    let (x, y, h) = coords.[cardID]
    let isBlockedBy offsets = 
      let isBlocking (dx, dy, dh) =
        let key = (x + dx, y + dy, h + dh)
        if m.ContainsKey key then states.[m.[key]] <> Hidden else false
      offsets |> List.exists isBlocking
    let top = [0, 0, 1; 0, 1, 1; 1, 0, 1; 1, 1, 1]
    let left = [-1, 0, 0; -1, 1, 0]
    let right = [2, 0, 0; 2, 1, 0]
    not (isBlockedBy top || (isBlockedBy left && isBlockedBy right))

let getFree coords (states:CardState[]) =
  [0 .. states.Length - 1]
  |> List.filter (fun i -> states.[i] <> Hidden)
  |> List.filter (isFree coords states)

let getMatches (ids:int[]) coords states = 
  let free = getFree coords states 
  free
  |> Set.ofList 
  |> Set.filter (fun i -> 
    (List.sumBy (function 
      | a when ids.[a] = ids.[i] -> 1 
      | _ -> 0) free) > 1)
  |> Seq.toList

let arrangeRandom types =
  types
  |> replicate 4
  |> Seq.toArray
  |> shuffle

let tryArrangeSolvable (coords:(int*int*int)[]) (cardTypes:int[]) = 
  let cardPairTypes =
    cardTypes
    |> Array.sort
    |> choosei (function | i, x when i%2 = 0 -> Some x | _ -> None)
    |> shuffle
  let isFree' = isFree coords
  let s = seq { 
    let states = [|for x in 1 .. coords.Length do yield Visible|]    
    for c in cardPairTypes do
      let nextFree = 
        [0..(coords.Length - 1)]
        |> List.filter (fun x -> states.[x] = Visible)
        |> List.filter (isFree' states)
        |> List.toArray
        |> shuffle
        |> Seq.truncate 2
        |> Seq.map (fun x -> (x, c))
      nextFree |> Seq.iteri (fun i (x, c) -> states.[x] <- Hidden)
      yield! nextFree
  } 
  let ids = s |> Seq.toArray |> Array.sortBy fst |> Array.map snd 
  let numCards = Seq.length cardTypes
  if ids.Length <> numCards then None else Some ids


let shuffleVisible coords (ids:int[]) (states:CardState[]) =
  let shuffled = 
    ids
    |> choosei (function | i, id when states.[i] = Visible -> Some id | _ -> None)
    |> tryMap 32 (tryArrangeSolvable coords)
  match shuffled with
  | Some s -> 
    ids 
    |> Array.zip3 states [|0..(states.Length - 1)|] 
    |> Array.filter (fun (st, _, _) -> st = Visible)
    |> Array.iteri (fun i (_, idx, _) -> ids.[idx] <- s.[i])
  | None -> MessageBox.Show "Not possible to create solvable position!" |> ignore
  
//============================================================================================
//  MUTABLE STATE
//============================================================================================
let cardCoords = 
  let l = layouts |> shuffle 
  l.[0]
  |> parseLayout 
  |> Array.sortBy (fun (x, y, h) -> x + y + h*1000) 

let cardStates = [|for x in 1 .. cardCoords.Length do yield Visible|]
let mutable curSelected:int option = None
let mutable moves:int list = []

let cardIDs = 
  let cardTypes = 
    [|1..fgAtlas.Frames|] 
    |> shuffle 
    |> Seq.truncate ((Array.length cardCoords)/4)
    |> replicate 4
    |> Seq.toArray
    |> tryMap 32 (tryArrangeSolvable cardCoords)
  match cardTypes with
  | Some s -> s
  | None -> MessageBox.Show "Not possible to create solvable position!" |> ignore; Array.empty

let cardParts = 
  cardCoords
  |> Array.zip cardIDs
  |> Array.map createCell

cardParts 
  |> Array.iter (fun (bg, fg) -> c.Children.Add(bg) |> ignore; c.Children.Add(fg) |> ignore)

let updateCardVisuals () =
  cardStates |> Array.iteri (fun i state -> updateCardVisual cardParts.[i] cardIDs.[i] state)
  
let setCardState state idx =
  cardStates.[idx] <- state
  updateCardVisual cardParts.[idx] cardIDs.[idx] state

let undoMove () = 
  match moves with
  | a::b::rest -> moves <- rest; setCardState Visible a; setCardState Visible b;
  | _ -> ()

let showFree () = getFree cardCoords cardStates |> List.iter (setCardState Selected)
let showMatches () = getMatches cardIDs cardCoords cardStates |> List.iter (setCardState Selected)

let pickCell mx my = 
    cardCoords 
    |> Array.map cellCoord 
    |> tryFindLastIndexi (fun n (x, y, w, h) -> 
      cardStates.[n] <> Hidden && mx > x && my > y && mx < x + w && my < y + h)

let unselectAll () = 
    curSelected <- None
    [|0 .. cardCoords.Length - 1|] 
    |> Array.filter (fun i -> cardStates.[i] <> Hidden) 
    |> Array.iter (setCardState Visible)

let handleClick (mx, my) = 
  let cell = match pickCell mx my with
             | Some cellIdx when (isFree cardCoords cardStates cellIdx) -> Some cellIdx
             | _ -> None
  match cell, curSelected with
  | Some c, Some s when c = s -> unselectAll ()
  | Some c, Some s when cardIDs.[c] = cardIDs.[s] -> 
      unselectAll (); setCardState Hidden c; setCardState Hidden s; moves <- c::s::moves;
      if (cardStates |> Array.forall (fun st -> st = Hidden)) then 
        MessageBox.Show "Amazing, you've won in this impossible game!" |> ignore
        // start the new game
      elif ((getMatches cardIDs cardCoords cardStates).Length = 0) then 
        MessageBox.Show "No more possible moves. You've lost, sorry" |> ignore
        shuffleVisible cardCoords cardIDs cardStates
        updateCardVisuals()
  | Some c, None -> setCardState Selected c; curSelected <- Some(c)
  | _, _ -> unselectAll ()


let events = 
  w.MouseDown
  |> Event.filter (fun mi -> (mi.ChangedButton = Input.MouseButton.Left && 
                              mi.ButtonState = Input.MouseButtonState.Pressed))
  |> Event.map (fun mi -> (mi.GetPosition(c).X, mi.GetPosition(c).Y))
  |> Event.add handleClick
 

menuUndo.Click.Add(fun _ -> undoMove())
menuShuffle.Click.Add(fun _ -> shuffleVisible cardCoords cardIDs cardStates; updateCardVisuals())
menuFree.Click.Add(fun _ -> showFree())
menuMatches.Click.Add(fun _ -> showMatches())