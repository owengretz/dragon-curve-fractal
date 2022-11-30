import Dict 
import Round

sliderTextCol = black
sliderBGCol = rgb 169 229 187
sliderKnobCol = rgb 65 179 164

myShapes model =
  [ roundedRect 110 10 5 |> filled sliderBGCol |> addOutline (solid 0.5) black |> move (0,-58)
  , case model.state of
      PlayThis x ->
          animateDragon model.state (state2num model.state) model.time
      otherwise ->
          drawDragon model.state (floor (state2num model.state)) (state2num model.state)
  , case model.state of
      Dragging x -> circle 5 |> filled sliderKnobCol |> addOutline (solid 0.5) black |> move (toPos x, -58)
      Waiting x -> circle 4 |> filled sliderKnobCol |> addOutline (solid 0.5) black |> move (toPos x, -58)
      PlayThrough x -> circle 4 |> outlined (solid 0.5) sliderKnobCol |> addOutline (solid 0.5) black |> move (toPos x, -58)
      PlayThis x -> circle 4 |> outlined (solid 0.5) sliderKnobCol |> addOutline (solid 0.5) black |> move (toPos x, -58)
  , List.map ( \ idx -> String.fromInt idx |> text |> centered |> size 4 |> filled sliderTextCol |> move (toPos (toFloat idx),-59.5) )
      (List.range 0 10)
      |> group
  , roundedRect 110 10 5 |> filled sliderBGCol |> makeTransparent 0
      |> move (0,-58)
      |> ( case model.state of 
             Waiting _  -> notifyMouseDownAt StartDragAt
             Dragging _ -> notifyMouseMoveAt MoveDragAt
                             >> notifyMouseUp StopDrag
                             >> notifyLeave StopDrag
             PlayThrough _ -> identity
             PlayThis _ -> identity
         )
  --, text (state2str model.state) |> size 3 |> centered |> filled black |> move (0,60)
  , group [
      rect 50 10 |> filled sliderBGCol |> addOutline (solid 0.5) black
    , text (if getState model.state == "Play Through"
            then "Stop"
            else "Play Through Generations")
            |> size 4.5 |> centered |> filled sliderTextCol |> move (0,-1.5)
  ] |> notifyTap (if getState model.state == "Play Through"
                  then StopPlayThrough
                  else StartPlayThrough) 
    |> move (-30,-45)
  , group [
      rect 50 10 |> filled sliderBGCol |> addOutline (solid 0.5) black
    , text (if getState model.state == "Play This"
            then "Stop"
            else "Animate This Generation")
            |> size 4.5 |> centered |> filled sliderTextCol |> move (0,-1.5)
  ] |> notifyTap (if getState model.state == "Play This"
                  then StopPlayThis
                  else StartPlayThis) 
    |> move (30,-45)
    , rect 60 10 |> filled sliderBGCol |> move (-69, 57)
  , text "Dragon Curve Fractal" |> size 6 |> selectable |> underline |> filled black |> move (-94,55)
  , makeText 
  [ "Click play to animate the fractal."
  , "Drag the slider to change generations."
  , ""
  , "This fractal is an L-system and is defined by a string."
  , "Each iteration the string is redefined in terms of itself."
  , ""
  , "It starts as \"f\", and each generation the rule is:"
  , "     Replace \"f\" with \"f - h\""
  , "     Replace \"h\" with \"f + h\""
  , ""
  , "When the fractal is drawn:"
  , "     \"f\" and \"h\" mean go forward"
  , "     \"+\" means turn 90 degrees right"
  , "     \"-\" means turn 90 degrees left"
  , ""
  , "For example, at generation 2 the rule is:"
  , "     \"f - h - f + h\""
  , "Therefore, starting upwards, the sequence is:"
  , "     F, L, F, L, F, R, F"
  ]
  ]

makeText lines =(List.indexedMap (\i idx -> 
                 text (getAtIndex lines idx "")
                 |> size 4 |> selectable |> filled black |> move (-94,47- (5 * toFloat idx))
                 ) 
                 (List.range 0 (List.length lines))) |> group

type alias Point = (Float,Float)

type Msg 
  = Tick Float GetKeyState
  | StartDragAt Point
  | MoveDragAt Point
  | StopDrag
  | StartPlayThrough
  | StopPlayThrough
  | StartPlayThis
  | StopPlayThis
  
type alias Model = { time : Float }

type State 
  = Waiting Float
  | Dragging Float
  | PlayThrough Float
  | PlayThis Float
  
state2num state =
  case state of
    Waiting x -> x
    Dragging x -> x
    PlayThrough x -> x
    PlayThis x -> x

state2str state = 
  case state of
    Waiting x -> "Waiting " ++ Round.round 2 x
    Dragging x -> "Dragging " ++ Round.round 2 x
    PlayThrough x -> "Play Through " ++ Round.round 2 x
    PlayThis x -> "Play This " ++ Round.round 2 x

getState state =
  case state of
    Waiting x -> "Waiting"
    Dragging x -> "Dragging"
    PlayThrough x -> "Play Through"
    PlayThis x -> "Play This"

update msg model 
  = case msg of
      Tick t _   -> { model | state = case model.state of 
                                PlayThrough old -> 
                                  let new = old + t - model.time
                                  in if new > 10 then Waiting 10 else PlayThrough new 
                                PlayThis _ -> 
                                  if model.time >= (-1 + toFloat (List.length (getPts (state2num model.state)))) / (linesPerSec (state2num model.state))
                                  then Waiting (state2num model.state) 
                                  else PlayThis (state2num model.state)
                                otherwise -> otherwise
                            , time = (if getState model.state == "Play This"
                                      then model.time + (t - model.prevFrameTime)
                                      else t)
                            , prevGen = state2num model.state
                            , prevFrameTime = t
                            }
      StartDragAt (x,_) -> 
        case model.state of 
          Waiting _ -> { model | state = Dragging (toGen x) } -- set up for 10 generations
          _ -> model
      MoveDragAt (x,_) -> 
        case model.state of 
          Dragging _ -> { model | state = Dragging ( toGen x ) } -- set up for 10 generations
          _ -> model
      StopDrag -> 
        case model.state of 
          Dragging generation -> { model | state = Waiting (toFloat <| round generation) } -- set up for 10 generations
          _ -> model
      StartPlayThrough ->
        case model.state of 
          Waiting generation -> { model | state = PlayThrough (if generation > 9.9 then 0 else generation) } -- set up for 10 generations
          _ -> model
      StopPlayThrough ->
        case model.state of 
          PlayThrough generation -> { model | state = Waiting generation } -- set up for 10 generations
          _ -> model
      StartPlayThis ->
        case model.state of 
          Waiting generation -> { model | state = PlayThis (toFloat(floor generation)), time = 0 }
          _ -> model
      StopPlayThis ->
        case model.state of 
          PlayThis generation -> { model | state = Waiting generation }
          _ -> model

-- convert mouse position to generation number and vice versa
toGen mouseX = 
  let
    raw = 0.1 * (mouseX + 50) 
  in
    if raw < 0 then 
      0
    else if raw > 10 then
      10
    else 
      raw
      
toPos genNum = 10 * genNum - 50

initialPt = (67,-2.5)
initialAngle = 90
dst = 2.8

linesPerSec gen = 1.7 ^ gen
getLines gen t = floor ((linesPerSec gen) * t)
getPt gen t pts start = (getAtIndex pts (start+getLines gen t) (getAtIndex pts ((start-1)+getLines gen t) (getAtIndex pts ((start-2)+getLines gen t) (getAtIndex pts ((start-3)+getLines gen t) (0,0)))))
getPts gen = (0,0)::(dragonPts (getDragonList ['f'] gen) (0,0) initialAngle)
smoothPt (p1x,p1y) (p2x,p2y) t = (p1x + (p2x-p1x)*t,p1y + (p2y-p1y)*t)


drawDragon state gen t = group [
    openPolygon
    (
    getPts gen
    )
    |> outlined (solid 0.5) black
    ,
    if gen < 10 then
    openPolygon
    (
    getPts gen
    )
    |> outlined (solid 0.5) black
    |> move (vecMult (lastPtPos gen) -1)
    |> rotate (getRot t)
    |> move (lastPtPos gen)
    else [] |> group
    ] |> move initialPt

rotSpeed = 1
getRot time = if time < 10/rotSpeed 
              then let t = floatMod (1/rotSpeed) time
                   in -((t*rotSpeed) * (pi/2))
              else -pi/2
--getRot time = let t = (if time < 10/rotSpeed then floatMod rotSpeed time else 9)
  --            in -((t/rotSpeed) * (pi/2))

vecMult (px,py) s = (px*s,py*s)
lastPtPos gen = let pts = getPts gen
                in
                getAtIndex pts (-1 + List.length pts) (0,0)

animateDragon state gen t = 
    openPolygon
    (
    let pts = getPts gen
    in List.take (1 + getLines gen t) pts
    ++ if getState state  == "Play This"
       then [smoothPt
            (getPt gen t pts 0)
            (getPt gen t pts 1)
            (floatMod 1 (t * (linesPerSec gen)))
            ]
       else []
    )
    |> outlined (solid 0.5) black
    |> move initialPt
dragonPts instr (px,py) prevAngle =
    case instr of
        [] -> []
        (step::rest) ->
            if step == 'f' || step == 'h'
            then 
                let pt = case prevAngle of
                             0   -> (px+dst,py)
                             90  -> (px,py+dst)
                             180 -> (px-dst,py)
                             270 -> (px,py-dst)
                             otherwise -> (px,py)
                in
                    pt::(dragonPts rest pt prevAngle)
            else dragonPts rest (px,py) 
                    (case step of
                        '+' -> modBy 360 (prevAngle + 270)
                        '-' -> modBy 360 (prevAngle + 90)
                        otherwise -> prevAngle)

getDragonList list iter = if iter == 0 then list
                          else getDragonList (nextDragonList list) (iter-1)       
nextDragonList : List Char -> List Char
nextDragonList prev = 
    case prev of
        [] -> []
        (s::ss) -> (case s of 
                      'f' -> ['f','-','h']
                      'h' -> ['f','+','h']
                      otherwise -> [s])
                    ++ nextDragonList ss

init = { state = Waiting 10
       , time = 0
       , prevGen = 10
       , prevFrameTime = 0
       }

main = gameApp Tick { model = init, view = view, update = update, title = "Dragon Curve" }

view model = collage 192 128 (myShapes model)


floatMod modBy num = if num < modBy then num else floatMod modBy (num - modBy)

getAtIndex : List a -> Int -> a -> a
getAtIndex xs n default = fromMaybe (List.head (List.drop n xs)) default
fromMaybe maybe default = 
    case maybe of
    Just item -> item
    otherwise -> default
