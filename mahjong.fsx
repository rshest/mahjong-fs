//  Mahjong solitaire game script
//  2011, Ruslan Shestopalyuk

#r "WindowsBase"
#r "PresentationCore"
#r "PresentationFramework"
#r "System.Xaml"

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Shapes
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Markup
open System.Xml
open System.IO

//============================================================================================
//  General utils
//============================================================================================
module Utils =
    //  Shuffles array in-place (good old Fisher-Yates)
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

    //  returns the full path of a file located in the same directory as the script
    let fullPath file = __SOURCE_DIRECTORY__ + "/"+ file

type StoneState =
  | Visible
  | Selected
  | Hidden

let STONE_EXTENTS = 64., 75.
let STONE_3D_OFFSET = -7., -10.

let getStoneLocations (i, j, layer) =
  let lx, ly = STONE_3D_OFFSET
  let sx, sy = STONE_EXTENTS
  let x = (float i)*sx*0.5 + (float layer)*lx
  let y = (float j)*sy*0.5 + (float layer)*ly
  (x, y, sx - lx, sy - ly)

//============================================================================================
//  UI/Drawing
//============================================================================================
let loadXamlWindow (filename:string) =
  let reader = XmlReader.Create(filename)
  XamlReader.Load(reader) :?> Window

let window = loadXamlWindow(Utils.fullPath "mahjong.xaml")
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
  let imgSource = new BitmapImage(new Uri(Utils.fullPath atlas.File))
  let stoneW, stoneH = 1./(float atlas.Cols), 1./(float atlas.Rows)
  let viewBox = new Rect(stoneW*(float (id % atlas.Cols)), 
                         stoneH*(float (id / atlas.Cols)), stoneW, stoneH)
  new ImageBrush(ImageSource = imgSource, Viewbox = viewBox)

let bgAtlas = { File = "stones_bg.png"; Cols = 2; Rows = 1; FrameWidth = 74.; FrameHeight = 85.; }
let fgAtlas = { File = "stones_fg.png"; Cols = 12; Rows = 9; FrameWidth = 64.; FrameHeight = 60.; }

let updateStoneControl stoneControl id state = 
  let (bg:Rectangle), (fg:Rectangle) = stoneControl
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

let setStoneOpacity opacity (fg:Rectangle, bg:Rectangle) =
    fg.Opacity <- opacity; bg.Opacity <- opacity;


let createStoneControls stoneDataArr =
  let createControlPair (id, (i, j, layer)) =
    let (x, y, _, _) = getStoneLocations(i, j, layer)
    let bg = new Rectangle(Width=bgAtlas.FrameWidth, Height=bgAtlas.FrameHeight)
    Canvas.SetLeft(bg, x)
    Canvas.SetTop(bg, y)   
    let fg = new Rectangle(Width=fgAtlas.FrameWidth, Height=fgAtlas.FrameHeight)
    Canvas.SetLeft(fg, x + 2.)
    Canvas.SetTop(fg, y + 10.)
    let controls = (bg, fg)
    updateStoneControl controls id Visible
    controls

  canvas.Children.Clear()
  stoneDataArr 
    |> Array.map createControlPair 
    |> Array.map (fun (bg, fg) -> 
                    canvas.Children.Add(bg) |> ignore
                    canvas.Children.Add(fg) |> ignore
                    (bg, fg))

//============================================================================================
//  Loading game data from the text files  
//============================================================================================
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
  let maxLayer = 
    layout 
    |> Array.maxBy Array.max 
    |> Array.max
  seq {
    let l = Array.copy layout
    let shifts = [|0, 0; 1, 0; 0, 1; 1, 1|]
    for layer in maxLayer .. -1 .. 1 do
      for row in 0 .. layout.Length - 2 do
        for col in 0 .. layout.[row].Length - 2 do
        let isBlock = Array.forall (fun (x,y) -> l.[row + x].[col + y] = layer) shifts
        if isBlock then
          yield (col, row, layer)
          shifts |> Array.iter (fun (x, y) -> l.[row + x].[col + y] <- layer - 1) 
  } |> Seq.toArray

let layouts = 
  let splitSections (res, s) (line:string) = 
    if line.StartsWith("-") then (s::res, "") else (res, s + "\n" + line)
  seq {
      use sr = new StreamReader(Utils.fullPath "layouts.txt")
      while not sr.EndOfStream do yield sr.ReadLine()
  } |> Seq.fold splitSections ([],"") 
    |> fst 
    |> List.rev 
    |> List.filter (fun l -> l.Length > 0) 
    |> List.toArray 
    |> Utils.choosei (function | i, x when i%2 = 1 -> Some x | _ -> None)
    |> Array.map parseLayout
    |> Array.map (Array.sortBy (fun (x, y, h) -> x + y + h*1000))

let langs = 
  seq {
        use sr = new StreamReader(Utils.fullPath "languages.txt")
        while not sr.EndOfStream do yield sr.ReadLine()
    } 
    |> Seq.map (fun s -> s.Trim().Split('|'))
    |> Seq.filter (fun el -> el.Length = 2)
    |> Seq.map (fun el -> (el.[0], el.[1]))
    |> Seq.toArray

//============================================================================================
//  Stone manipulation utilities
//============================================================================================
//  returns function which checks if given stone is free (using a cached hash table)
let isFree coords = 
  //  create hash table with coordinates to quickly find neighbors
  let m = System.Collections.Generic.Dictionary()
  let addStone i (x, y, h) = 
    [|0, 0; 1, 0; 0, 1; 1, 1|]
    |> Array.iter (fun (dx, dy) -> m.Add((x + dx, y + dy, h), i))
  coords |> Array.iteri addStone
  fun (states:StoneState[]) stoneID ->
    let (x, y, h) = coords.[stoneID]
    let isBlockedBy offsets = 
      let isBlocking (dx, dy, dh) =
        let key = (x + dx, y + dy, h + dh)
        if m.ContainsKey key then states.[m.[key]] <> Hidden else false
      offsets |> List.exists isBlocking
    let top = [0, 0, 1; 0, 1, 1; 1, 0, 1; 1, 1, 1]
    let left = [-1, 0, 0; -1, 1, 0]
    let right = [2, 0, 0; 2, 1, 0]
    not (isBlockedBy top || (isBlockedBy left && isBlockedBy right))

//  returns a list of free stone indices
let getFree coords (states:StoneState[]) =
  [0 .. states.Length - 1]
  |> List.filter (fun i -> states.[i] <> Hidden)
  |> List.filter (isFree coords states)

//  returns a list of stone indices that have removable matches  
let getMatches (ids:int[]) coords states = 
  let free = getFree coords states 
  free
  |> Set.ofList 
  |> Set.filter (fun i -> 
    (List.sumBy (function 
      | a when ids.[a] = ids.[i] -> 1 
      | _ -> 0) free) > 1)
  |> Seq.toList

let arrangeRandom (coords:(int*int*int)[]) (stoneTypes:int[]) = 
  Some(stoneTypes |> Utils.shuffle)

let tryArrangeSolvable (coords:(int*int*int)[]) (stoneTypes:int[]) = 
  let stonePairTypes =
    stoneTypes
    |> Array.sort
    |> Utils.choosei (function | i, x when i%2 = 0 -> Some x | _ -> None)
    |> Utils.shuffle
  let isFree' = isFree coords
  let s = seq { 
    let states = [|for x in 1 .. coords.Length do yield Visible|]    
    for c in stonePairTypes do
      let nextFree = 
        [0..(coords.Length - 1)]
        |> List.filter (fun x -> states.[x] = Visible)
        |> List.filter (isFree' states)
        |> List.toArray
        |> Utils.shuffle
        |> Seq.truncate 2
        |> Seq.map (fun x -> (x, c))
      nextFree |> Seq.iteri (fun i (x, c) -> states.[x] <- Hidden)
      yield! nextFree
  } 
  let ids = s |> Seq.toArray |> Array.sortBy fst |> Array.map snd 
  let numStones = Seq.length stoneTypes
  if ids.Length <> numStones then None else Some ids

let MAX_ARRANGE_ATTEMPTS = 50

let shuffleVisible coords (ids:int[]) (states:StoneState[]) shuffleFn =
  let isVisible = (function | i, id when states.[i] = Visible -> Some id | _ -> None)
  let shuffled = 
    ids
    |> Utils.choosei isVisible
    |> Utils.tryApply MAX_ARRANGE_ATTEMPTS (shuffleFn (Utils.choosei isVisible coords))
  match shuffled with
  | Some (s:int[]) -> 
    ids 
    |> Array.zip3 states [|0..(states.Length - 1)|] 
    |> Array.filter (fun (st, _, _) -> st = Visible)
    |> Array.iteri (fun i (_, idx, _) -> ids.[idx] <- s.[i])
  | None -> MessageBox.Show "Not possible to create solvable position!" |> ignore

let getMaxLayer coords = 
  let  _, _, m = coords |> Array.maxBy (fun (_, _, h) -> h)
  m
  
//============================================================================================
//  Game data structure
//============================================================================================
type Game = 
  { stoneCoords: (int*int*int)[]
    stoneStates: StoneState[]
    stoneIDs: int[]
    stoneControls:(Rectangle*Rectangle)[] 
    mutable curSelected:int option
    mutable moves:int list
    mutable numHiddenLayers: int }

let newGame layoutID =
  let coords = layouts.[layoutID]
  let states = Array.init coords.Length (fun _ -> Visible)
  let ids = 
    [|0..(Array.length langs)|] 
    |> Utils.shuffle 
    |> Seq.truncate ((Array.length coords)/4)
    |> Utils.replicate 4
    |> Seq.toArray
  shuffleVisible coords ids states tryArrangeSolvable
  let controls = 
    coords
    |> Array.zip ids
    |> createStoneControls
  { stoneCoords =  coords
    stoneStates = states
    curSelected = None
    moves = []
    stoneIDs = ids
    numHiddenLayers = 0
    stoneControls = controls }

//============================================================================================
//  MUTABLE STATE
//============================================================================================
let mutable game = newGame ((new System.Random()).Next(0, Array.length layouts))

//============================================================================================
//  Game logic
//============================================================================================
let updateStoneControls () =
  game.stoneStates 
  |> Array.iteri (fun i state -> updateStoneControl game.stoneControls.[i] game.stoneIDs.[i] state)
  
let setStoneState state idx =
  game.stoneStates.[idx] <- state
  updateStoneControl game.stoneControls.[idx] game.stoneIDs.[idx] state

let undoMove () = 
  match game.moves with
  | a::b::rest -> game.moves <- rest; setStoneState Visible a; setStoneState Visible b;
  | _ -> ()

let pickStone mx my = 
    game.stoneCoords 
    |> Array.map getStoneLocations 
    |> Utils.tryFindLastIndexi (fun n (x, y, w, h) -> 
      game.stoneStates.[n] <> Hidden && mx > x && my > y && mx < x + w && my < y + h)

let unselectAll () = 
    game.curSelected <- None
    [|0 .. game.stoneCoords.Length - 1|] 
    |> Array.filter (fun i -> game.stoneStates.[i] <> Hidden) 
    |> Array.iter (setStoneState Visible)

let hideLayers num = 
  let maxLayer = getMaxLayer game.stoneCoords
  if num >= 0 && num < maxLayer then
    game.numHiddenLayers <- num
    game.stoneCoords |> Array.iteri (fun i (_, _, h) -> 
      setStoneOpacity (if h <= maxLayer - num then 1.0 else 0.2) game.stoneControls.[i])

let showFree () =  
  unselectAll ()
  getFree game.stoneCoords game.stoneStates |> List.iter (setStoneState Selected)

let showMatches () = 
  unselectAll ()
  getMatches game.stoneIDs game.stoneCoords game.stoneStates |> List.iter (setStoneState Selected)

let handleClick (mx, my) = 
  let stone = match pickStone mx my with
              | Some stoneIdx when (isFree game.stoneCoords game.stoneStates stoneIdx) -> Some stoneIdx
              | _ -> None
  match stone, game.curSelected with
  | Some c, Some s when c = s -> unselectAll ()
  | Some c, Some s when game.stoneIDs.[c] = game.stoneIDs.[s] -> 
      unselectAll () 
      setStoneState Hidden c 
      setStoneState Hidden s 
      game.moves <- c::s::game.moves;
      if (game.stoneStates |> Array.forall (fun st -> st = Hidden)) then 
        MessageBox.Show "Amazing, you've won in this impossible game!" |> ignore
        game <- newGame 0
      elif ((getMatches game.stoneIDs game.stoneCoords game.stoneStates).Length = 0) then 
        MessageBox.Show "No more possible moves. You've lost, sorry" |> ignore
        shuffleVisible game.stoneCoords game.stoneIDs game.stoneStates tryArrangeSolvable
        updateStoneControls()
  | Some c, None -> 
      setStoneState Selected c
      game.curSelected <- Some(c)
      let url, lang = langs.[game.stoneIDs.[c]]
      let status = window.FindName("StoneName") :?> TextBlock
      status.Text <- lang
  | _, _ -> unselectAll ()

let startGame id = 
  fun _ -> game <- newGame id


//============================================================================================
//  User Input
//============================================================================================
let events = 
  window.MouseDown
  |> Event.filter (fun mi -> (mi.ChangedButton = Input.MouseButton.Left && 
                              mi.ButtonState = Input.MouseButtonState.Pressed))
  |> Event.map (fun mi -> (mi.GetPosition(canvas).X, mi.GetPosition(canvas).Y))
  |> Event.add handleClick

let bindMenuItem (name, fn) =
  let menuItem = window.FindName(name) :?> MenuItem
  menuItem.Click.Add(fun _ -> fn ())

[<Literal>]
let HELP_URL = @"http://en.wikipedia.org/wiki/Mahjong_solitaire"

[ "MenuUndo", undoMove
  "MenuShuffleSolvable", fun _ -> shuffleVisible game.stoneCoords game.stoneIDs game.stoneStates tryArrangeSolvable
                                  updateStoneControls()
  "MenuShuffle", fun _ -> shuffleVisible game.stoneCoords game.stoneIDs game.stoneStates arrangeRandom
                          updateStoneControls()
  "MenuShowFree", showFree
  "MenuShowMatches", showMatches
  "MenuHideLayer", fun _ -> hideLayers (game.numHiddenLayers + 1)
  "MenuUnhideLayer", fun _ -> hideLayers (game.numHiddenLayers - 1)
  "MenuRandom", startGame ((new System.Random()).Next(0, Array.length layouts))
  "MenuTurtle", startGame 0
  "MenuDragon", startGame 1
  "MenuCrab", startGame 2
  "MenuSpider", startGame 3
  "MenuExit", window.Close
  "MenuAbout", fun _ -> Diagnostics.Process.Start HELP_URL |> ignore
] |> List.iter bindMenuItem

let stoneURL = window.FindName("StoneInfoURL") :?> Button
stoneURL.Click.Add(fun _ -> 
                    match game.curSelected with
                    | Some sel -> Diagnostics.Process.Start (fst langs.[game.stoneIDs.[sel]]) |> ignore
                    | None -> ())

