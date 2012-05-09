//  Simple Mahjong solitaire game
//  2011, Ruslan Shestopalyuk

#r "WindowsBase"
#r "PresentationCore"
#r "PresentationFramework"
#r "System.Xaml"
#time 

//============================================================================================
//  General utils
//============================================================================================
//  Shuffles array in-place (good old Fished-Yites)
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

//  returns Some(index) of the last element in array satisfying the predicate, None if none found
let tryFindLastIndexi f (array : _[]) = 
  let rec searchFrom n = 
    if n < 0 then None 
    elif f n array.[n] then Some n else searchFrom (n - 1)
  searchFrom (array.Length - 1)

//  the same as Array.choose, but also passes the index into the predicate
let choosei f (array: _[]) =
  let res = new System.Collections.Generic.List<_>()
  for i = 0 to array.Length - 1 do 
      let x = array.[i] 
      match f (i, x) with
      | Some v -> res.Add(v)
      | None -> ignore()
  res.ToArray()

//  creates the sequence by replicating the original one given amount of times
let replicate numTimes s = 
  seq {for i in [1..numTimes] do yield! s}

//  tries to apply function to the argument until it returns Some() option (at most 'maxDepth' times)
let rec tryApply maxDepth f arg = 
  match maxDepth, (f arg) with
  | 0, _ -> None
  | _, Some s -> Some s
  | _, None -> tryApply (maxDepth - 1) f arg

//============================================================================================
//  UI/Drawing
//============================================================================================
open System
open System.Windows
open System.Windows.Controls
open System.Windows.Shapes
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Markup
open System.Xml
open System.IO

let fullPath file = __SOURCE_DIRECTORY__ + "/"+ file

let loadXamlWindow (filename:string) =
  let reader = XmlReader.Create(filename)
  XamlReader.Load(reader) :?> Window

let window = loadXamlWindow(fullPath "mahjong.xaml")
window.Show()
let canvas = window.FindName("BoardCanvas") :?> Canvas

type SpriteAtlas = { 
  File: string;
  Cols: int;
  Rows: int;
  FrameWidth: float;
  FrameHeight: float; 
}

let spriteBrush atlas id =
  let imgSource = new BitmapImage(new Uri(fullPath atlas.File))
  let cardW, cardH = 1./(float atlas.Cols), 1./(float atlas.Rows)
  let viewBox = new Rect(cardW*(float (id % atlas.Cols)), 
                         cardH*(float (id / atlas.Cols)), cardW, cardH)
  new ImageBrush(ImageSource = imgSource, Viewbox = viewBox)

let bgAtlas = {
  File = "brick.png";
  Cols = 2; Rows = 1;
  FrameWidth = 74.; FrameHeight = 85.;
}

let fgAtlas = {
  File = "cards.png";
  Cols = 12; Rows = 9;
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

let updateCardVisual cardControls id state = 
  let (bg:Rectangle), (fg:Rectangle) = cardControls
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

let setCardOpacity opacity (fg:Rectangle, bg:Rectangle) =
    fg.Opacity <- opacity; bg.Opacity <- opacity;

let cellSz = 64., 75.
let levShift = -7., -10.

let cellCoord (i, j, level) =
  let lx, ly = levShift
  let x = (float i)*(fst cellSz)*0.5 + (float level)*lx
  let y = (float j)*(snd cellSz)*0.5 + (float level)*ly
  (x, y, fst cellSz - lx, snd cellSz - ly)

let createCell (id, (i, j, level)) =
  let (x, y, _, _) = cellCoord(i, j, level)
  let cardControls = createBrick x y id
  updateCardVisual cardControls id Visible
  cardControls

//============================================================================================
//  Layouts  
//============================================================================================
let layouts = 
  let splitSections (res, s) (line:string) = 
    if line.StartsWith("-") then (s::res, "") else (res, s + "\n" + line)
  seq {
      use sr = new StreamReader(fullPath "layouts.txt")
      while not sr.EndOfStream do yield sr.ReadLine()
  } |> Seq.fold splitSections ([],"") 
    |> fst 
    |> List.rev 
    |> List.filter (fun l -> l.Length > 0) 
    |> List.toArray 
    |> choosei (function | i, x when i%2 = 1 -> Some x | _ -> None)

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

let langs = 
  seq {
        use sr = new StreamReader(fullPath "languages.txt")
        while not sr.EndOfStream do yield sr.ReadLine()
    } 
    |> Seq.map (fun s -> s.Trim().Split('|'))
    |> Seq.filter (fun el -> el.Length = 2)
    |> Seq.map (fun el -> (el.[0], el.[1]))
    |> Seq.toArray

let numCardTypes = Array.length langs
//============================================================================================
//  CARD FUNCTIONS
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

let arrangeRandom (coords:(int*int*int)[]) (cardTypes:int[]) = 
  Some(cardTypes |> shuffle)

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

let MAX_ARRANGE_ATTEMPTS = 50

let shuffleVisible coords (ids:int[]) (states:CardState[]) shuffleFn =
  let isVisible = (function | i, id when states.[i] = Visible -> Some id | _ -> None)
  let shuffled = 
    ids
    |> choosei isVisible
    |> tryApply MAX_ARRANGE_ATTEMPTS (shuffleFn (choosei isVisible coords))
  match shuffled with
  | Some (s:int[]) -> 
    ids 
    |> Array.zip3 states [|0..(states.Length - 1)|] 
    |> Array.filter (fun (st, _, _) -> st = Visible)
    |> Array.iteri (fun i (_, idx, _) -> ids.[idx] <- s.[i])
  | None -> MessageBox.Show "Not possible to create solvable position!" |> ignore

let genCardCoords layoutID =
  layouts.[layoutID]
  |> parseLayout 
  |> Array.sortBy (fun (x, y, h) -> x + y + h*1000)

let getMaxLevel coords = 
  let  _, _, m = coords |> Array.maxBy (fun (_, _, h) -> h)
  m
  
//============================================================================================
//  GAME DATA STRUCTURE
//============================================================================================
type Game = 
  { cardCoords: (int*int*int)[]
    cardStates: CardState[]
    cardIDs: int[]
    cardControls:(Rectangle*Rectangle)[] 
    mutable curSelected:int option
    mutable moves:int list
    mutable numHiddenLevels: int }

let newGame layoutID =
  let coords = genCardCoords layoutID
  let states = Array.init coords.Length (fun _ -> Visible)
  let ids = 
    [|1..numCardTypes|] 
    |> shuffle 
    |> Seq.truncate ((Array.length coords)/4)
    |> replicate 4
    |> Seq.toArray
  shuffleVisible coords ids states tryArrangeSolvable
  let controls = 
    coords
    |> Array.zip ids
    |> Array.map createCell
  canvas.Children.Clear()
  controls 
    |> Array.iter (fun (bg, fg) -> canvas.Children.Add(bg) |> ignore
                                   canvas.Children.Add(fg) |> ignore)
  { cardCoords =  coords
    cardStates = states
    curSelected = None
    moves = []
    cardIDs = ids
    numHiddenLevels = 0
    cardControls = controls }

//============================================================================================
//  MUTABLE STATE
//============================================================================================
let mutable game = newGame ((new System.Random()).Next(0, Array.length layouts))

//============================================================================================
//  GAME CODE
//============================================================================================
let updateCardVisuals () =
  game.cardStates 
  |> Array.iteri (fun i state -> updateCardVisual game.cardControls.[i] game.cardIDs.[i] state)
  
let setCardState state idx =
  game.cardStates.[idx] <- state
  updateCardVisual game.cardControls.[idx] game.cardIDs.[idx] state

let undoMove () = 
  match game.moves with
  | a::b::rest -> game.moves <- rest; setCardState Visible a; setCardState Visible b;
  | _ -> ()

let pickCell mx my = 
    game.cardCoords 
    |> Array.map cellCoord 
    |> tryFindLastIndexi (fun n (x, y, w, h) -> 
      game.cardStates.[n] <> Hidden && mx > x && my > y && mx < x + w && my < y + h)

let unselectAll () = 
    game.curSelected <- None
    [|0 .. game.cardCoords.Length - 1|] 
    |> Array.filter (fun i -> game.cardStates.[i] <> Hidden) 
    |> Array.iter (setCardState Visible)

let hideLevels num = 
  let maxLevel = getMaxLevel game.cardCoords
  if num >= 0 && num < maxLevel then
    game.numHiddenLevels <- num
    game.cardCoords |> Array.iteri (fun i (_, _, h) -> 
      setCardOpacity (if h <= maxLevel - num then 1.0 else 0.2) game.cardControls.[i])

let showFree () =  
  unselectAll ()
  getFree game.cardCoords game.cardStates |> List.iter (setCardState Selected)

let showMatches () = 
  unselectAll ()
  getMatches game.cardIDs game.cardCoords game.cardStates |> List.iter (setCardState Selected)

let handleClick (mx, my) = 
  let cell = match pickCell mx my with
             | Some cellIdx when (isFree game.cardCoords game.cardStates cellIdx) -> Some cellIdx
             | _ -> None
  match cell, game.curSelected with
  | Some c, Some s when c = s -> unselectAll ()
  | Some c, Some s when game.cardIDs.[c] = game.cardIDs.[s] -> 
      unselectAll () 
      setCardState Hidden c 
      setCardState Hidden s 
      game.moves <- c::s::game.moves;
      if (game.cardStates |> Array.forall (fun st -> st = Hidden)) then 
        MessageBox.Show "Amazing, you've won in this impossible game!" |> ignore
        game <- newGame 0
      elif ((getMatches game.cardIDs game.cardCoords game.cardStates).Length = 0) then 
        MessageBox.Show "No more possible moves. You've lost, sorry" |> ignore
        shuffleVisible game.cardCoords game.cardIDs game.cardStates tryArrangeSolvable
        updateCardVisuals()
  | Some c, None -> 
      setCardState Selected c
      game.curSelected <- Some(c)
      let url, lang = langs.[game.cardIDs.[c] - 1]
      let status = window.FindName("CardName") :?> TextBlock
      status.Text <- lang
  | _, _ -> unselectAll ()

let events = 
  window.MouseDown
  |> Event.filter (fun mi -> (mi.ChangedButton = Input.MouseButton.Left && 
                              mi.ButtonState = Input.MouseButtonState.Pressed))
  |> Event.map (fun mi -> (mi.GetPosition(canvas).X, mi.GetPosition(canvas).Y))
  |> Event.add handleClick
 
let startGame id = 
  fun _ -> game <- newGame id

let bindMenuItem (name, fn) =
  let menuItem = window.FindName(name) :?> MenuItem
  menuItem.Click.Add(fun _ -> fn ())

[<Literal>]
let HELP_URL = @"http://en.wikipedia.org/wiki/Mahjong_solitaire"

[ "MenuUndo", undoMove
  "MenuShuffleSolvable", fun _ -> shuffleVisible game.cardCoords game.cardIDs game.cardStates tryArrangeSolvable
                                  updateCardVisuals()
  "MenuShuffle", fun _ -> shuffleVisible game.cardCoords game.cardIDs game.cardStates arrangeRandom
                          updateCardVisuals()
  "MenuShowFree", showFree
  "MenuShowMatches", showMatches
  "MenuHideLevel", fun _ -> hideLevels (game.numHiddenLevels + 1)
  "MenuUnhideLevel", fun _ -> hideLevels (game.numHiddenLevels - 1)
  "MenuRandom", startGame ((new System.Random()).Next(0, Array.length layouts))
  "MenuTurtle", startGame 0
  "MenuDragon", startGame 1
  "MenuCrab", startGame 2
  "MenuSpider", startGame 3
  "MenuExit", window.Close
  "MenuAbout", fun _ -> Diagnostics.Process.Start HELP_URL |> ignore
] |> List.iter bindMenuItem

let cardURL = window.FindName("CardURL") :?> Button
cardURL.Click.Add(fun _ -> 
                    match game.curSelected with
                    | Some sel -> Diagnostics.Process.Start (fst langs.[game.cardIDs.[sel] - 1]) |> ignore
                    | None -> ())

