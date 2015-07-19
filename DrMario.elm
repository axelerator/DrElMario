import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Keyboard
import Window
import Random

type alias Item =
  { x : Int
  , y : Int
  , kind: ItemType
  , o: Orientation
  , hover: Float
  }

type alias Field = (Int, Int)
type alias Fields = List Field

type alias ColoredField = (Field, ItemColor)
type alias ColoredFields = List ColoredField

type ItemColor = Blue | Yellow | Red

type Command = None | StrafeLeft | StrafeRight | RotLeft | RotRight

type Orientation = Vertical | Horizontal

type ItemType = Virus ItemColor | HalfPill ItemColor | FullPill ItemColor ItemColor

type alias Keys = { x:Int, y:Int }

type alias Items = List Item

type alias Board = 
  { items : Items
  , seed: Random.Seed
  }

fieldWidth05 = 10
fieldWidth = 20
defaultHover = 10.0

toCol : ItemColor -> Color
toCol c =
  case c of
    Red -> rgba 111 11 11 0.6
    Blue -> rgba 11 11 111 0.6
    Yellow -> rgba 11 111 11 0.6

geo : Item -> Form
geo item =
  let
    (ox, oy) = 
      case item.kind of
        Virus _ -> (fieldWidth05, fieldWidth05)
        HalfPill _ -> (fieldWidth05, fieldWidth05)
        FullPill _ _ -> case item.o of
                          Horizontal -> (fieldWidth, fieldWidth05)
                          Vertical -> (fieldWidth05, fieldWidth)
    pos = (item.x * 20 + ox, item.y * 20 + oy)
    res = case item.kind of
      Virus itemColor -> circle (fieldWidth * 0.5)
                          |> filled (toCol itemColor)
      HalfPill itemColor -> rect fieldWidth fieldWidth
                          |> filled (toCol itemColor)
      FullPill c1 c2 -> case item.o of
                          Horizontal -> toForm
                                          (collage (2 * fieldWidth) fieldWidth [
                                            rect fieldWidth fieldWidth
                                              |> filled (toCol c1)
                                              |> move (fieldWidth * -0.5, 0)
                                            ,
                                            rect fieldWidth fieldWidth
                                              |> filled (toCol c2)
                                              |> move (fieldWidth * 0.5, 0)
                                            ])
                          Vertical -> toForm
                                          (collage fieldWidth (2 * fieldWidth) [
                                            rect fieldWidth fieldWidth
                                              |> filled (toCol c1)
                                              |> move (0, fieldWidth * -0.5)
                                            ,
                                            rect fieldWidth fieldWidth
                                              |> filled (toCol c2)
                                              |> move (0, fieldWidth * 0.5)
                                            ])
  in
    res 
      |> move pos



gravity items dt item =
  let
    pulled = { item | hover <- item.hover - dt }
    lower = { item | hover <- defaultHover
                   , y <- item.y - 1}
    itemsWithoutCurrent = List.filter (\i -> i.x /= item.x || i.y /= item.y) items
    dropPositionFree = canPlace itemsWithoutCurrent lower
  in
    if pulled.hover < 0 && dropPositionFree then lower else pulled

nextColor : Random.Seed -> (ItemColor, Random.Seed)
nextColor seed =
  let
    (colNum, newSeed) = Random.generate (Random.int 0 2) seed
  in
    case colNum of
      0 -> (Yellow, newSeed)
      1 -> (Red, newSeed)
      2 -> (Blue, newSeed)

readKeys : Keys -> Command
readKeys keys =
  let
    strafe = if keys.x == 0 then
                None
             else
                if keys.x < 0 then
                  StrafeLeft
                else
                  StrafeRight
    rotate = if keys.y == 0 then
                None
             else
                if keys.y < 0 then
                  RotLeft
                else
                  RotRight
  in
     if strafe == None then
       rotate
     else
       strafe

interact : Command -> Item -> Item
interact cmd item =
  case cmd of
    StrafeLeft -> { item | x <- item.x - 1 }
    StrafeRight -> { item | x <- item.x + 1 }
    RotLeft -> { item | o <- if item.o == Vertical then Horizontal else Vertical}
    RotRight -> { item | o <- if item.o == Vertical then Horizontal else Vertical}
    None -> item

coloredFields : Item -> List ColoredField
coloredFields item = 
  case item.kind of
    Virus color -> [((item.x, item.y), color)]
    HalfPill color -> [((item.x, item.y), color)]
    FullPill colorLeft colorRight -> case item.o of
                      Horizontal -> [((item.x, item.y), colorLeft), ((item.x + 1, item.y), colorRight)]
                      Vertical -> [((item.x, item.y), colorLeft), ((item.x, item.y + 1), colorRight)]


takenFields : Item -> List Field
takenFields = (List.map fst) << coloredFields 

moveItem : Keys -> Item -> Item
moveItem keys item =
  let
    cmd = readKeys keys
  in
    interact cmd item

addOrNewGroup : (a, b) -> List (a, List b) -> List (a, List b)
addOrNewGroup (key, x) groups  = case groups of
  [] -> [(key, [x])]
  (lastKey, vs)::xs -> if lastKey == key then
                          (lastKey, x::vs)::xs
                       else
                          (key, [x])::(lastKey, vs)::xs

groupBy : (a -> comparable) -> List a -> List (comparable, List a)
groupBy f xs =
  let
    keys = List.map f xs
    zipped = List.sortBy fst (List.map2 (,) keys xs)
  in
    List.foldr addOrNewGroup [] zipped

areNeighbours : Orientation -> ColoredField -> ColoredField -> Bool
areNeighbours o ((x0, y0),_) ((x1, y1),_) = 
  case o of
    Vertical   -> y0 == y1 + 1 || y0 == y1 - 1
    Horizontal -> x0 == x1 + 1 || x0 == x1 - 1

sameColor : ColoredField -> ColoredField -> Bool
sameColor (_, c0) (_, c1) = c0 == c1

belongsToCluster : Orientation -> ColoredFields -> ColoredField -> Bool
belongsToCluster o others field =
  case others of
    [] -> False
    x::xs -> (sameColor x field) && (areNeighbours o x field)

addOrNewCluster : Orientation -> ColoredField -> List ColoredFields -> List ColoredFields
addOrNewCluster o current clusters =
  case clusters of
    [] -> [[current]]
    last::rest -> if belongsToCluster o last current then
                    (current::last)::rest
                  else
                    [current]::clusters

ignoreColor : ColoredField -> Field
ignoreColor (pos, _) = pos

dropTooShort : List ColoredFields -> List ColoredFields
dropTooShort = List.filter (\l -> List.length l > 2)

findClusters : ColoredFields -> Orientation -> Fields
findClusters coloredFields orientation =
  let
    sortDir = case orientation of
      Vertical   -> \((x,y), _) -> y
      Horizontal -> \((x,y), _) -> x
    groupDir = case orientation of
      Vertical   -> \((x,y), _) -> x
      Horizontal -> \((x,y), _) -> y
    byOrientation = List.map snd (groupBy groupDir coloredFields)
    byOrientationSorted = List.map (List.sortBy sortDir) byOrientation
    clusterFolder = addOrNewCluster orientation
    foldGroup = List.foldr clusterFolder []
    clustersPerLine = List.map foldGroup byOrientationSorted
    longClustersPerLine = List.map dropTooShort clustersPerLine
    forAllLines = List.concat (List.concat longClustersPerLine)
   in
     List.map ignoreColor forAllLines

cluster : (a -> comparable) -> List a -> List (comparable, List a)
cluster f xs =
  let
    keys = List.map f xs
    zipped = List.map2 (,) keys xs
  in
    List.foldr addOrNewGroup [] zipped

colorToInt col = case col of
  Blue -> 0
  Yellow -> 1
  Red -> 2
  
findMatchesInOrderedFields : ColoredFields -> ColoredFields
findMatchesInOrderedFields fields =
  let 
    byColor = groupBy (snd >> colorToInt) fields
    withoutKey = List.map snd byColor
  in
    List.concat (List.filter (\fs -> (List.length fs) > 3) withoutKey)

leftMatch : Item -> Fields -> Bool
leftMatch item ((fx, fy)::_) = item.x /= fx || item.y /= fy

splitI : Item -> Items
splitI item = case item.kind of
  Virus _ -> [item]
  HalfPill _ -> [item]
  FullPill left right -> case item.o of
    Horizontal -> [ { item | kind <- HalfPill left}
                  , { item | kind <- HalfPill right, x <- item.x + 1}
                  ]
    Vertical   -> [ { item | kind <- HalfPill left}
                  , { item | kind <- HalfPill right, y <- item.y + 1}
                  ]

split : Item -> Fields -> Item
split item fields = 
  let
    (leftHalf::rightHalf::[]) = splitI item
  in
    if leftMatch item fields then
      leftHalf 
    else
      rightHalf
      

isInFields : Fields -> Item -> Items
isInFields fields item =
  let
    itemFields = takenFields item
    containedInFields f = List.any (isSameField f) fields
    matches = List.filter containedInFields itemFields
    matchesAny = (List.length matches) > 0
  in
    if matchesAny then
      case item.kind of
        Virus _ -> []
        HalfPill _ -> []
        FullPill _ _ -> if (List.length matches) == 2 then
                          []
                        else
                          [split item matches]
    else
      [item]

      

clearMatches : Items -> Items
clearMatches items =
  let
    allTakenFields = List.concat (List.map coloredFields items)
    allHorizontalMatches = findClusters allTakenFields Horizontal
    allVerticalMatches = findClusters allTakenFields Vertical
    allMatches = List.concat [allHorizontalMatches, allVerticalMatches]
    --allMatches = allHorizontalMatches
    dropIfMatch item notDropped = List.concat [(isInFields allMatches item), notDropped]
  in
    List.foldr dropIfMatch [] items


update : (Float, Keys) -> Board -> Board
update (dt, keys) board =
  let
    (newPos, colorSeed) = Random.generate (Random.int 0 10) board.seed
    (newCol0, colorSeed2) = nextColor colorSeed
    (newCol1, lastSeed) = nextColor colorSeed2
    stone =
      { x = 0 --newPos
      , y = 15
      , kind = FullPill newCol0 newCol1
      , o = Horizontal
      , hover = defaultHover
      }
    
    (last :: other) = List.map (gravity board.items dt) board.items
    movedItems = last :: other
    keyMovedItem = moveItem keys last
    maybeMovedItem = if canPlace other keyMovedItem then
                       keyMovedItem
                     else
                       last 
  in
    if sameFields board.items movedItems then
      {  board | items <- stone :: (clearMatches movedItems)
               , seed <- lastSeed
      }
    else
      { board | items <- maybeMovedItem :: other}

sameFields : Items -> Items -> Bool
sameFields i0 i1 =
  let
    simplified = \item -> (item.x, item.y, item.kind)
    anyMoving = List.any (\item -> item.hover > 0.0) i0
    allRested = not anyMoving
    items0 = List.map simplified i0
    items1 = List.map simplified i1
  in
    allRested && (items0 == items1)

canPlace : Items -> Item -> Bool
canPlace items item = List.all (areFree items) (takenFields item)


areFree : Items -> Field -> Bool
areFree is f = isFree f is

isSameField : Field -> Field -> Bool
isSameField (x0,y0) (x1,y1) = x0 == x1 && y0 == y1

isFree : Field -> Items -> Bool
isFree field items =
  let
    (x,y) = field
    occupiedFields = List.concat (List.map takenFields items )
    blockedByOther = List.any (isSameField field) occupiedFields
    isAtBottom = y == -1
  in
    not (blockedByOther || isAtBottom)

blocks : Field -> Item -> Bool
blocks (x,y) item =
  let
    sameRow = item.y == y
    sameCol = 
      case item.kind of
        Virus _ -> item.x == x
        HalfPill _ -> item.x == x
        FullPill _ _ -> item.x == x || item.x + 1 == x
  
  in
    sameRow && sameCol
      
input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 20)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
    
view : (Int, Int) -> Board -> Element
view (w', h') board =
 let
   bgScale = 2
   bg = toForm (image (79*bgScale) (175*bgScale) "/bg.jpg") |> move (0, (175*(bgScale/2))) 
   forms = bg::(List.map geo board.items)
 in
   collage w' h' forms


stone1 =
  { x = 5
  , y = 12
  , kind = Virus Blue
  , o = Vertical
  , hover = defaultHover
  }

stone2 =
  { x = 5
  , y = 1
  , kind = Virus Red
  , o = Horizontal
  , hover = defaultHover
  }


board = 
  { items = splitI stone2
  , seed = Random.initialSeed 31415
  }
  
main = Signal.map2 view Window.dimensions (Signal.foldp update board input)

