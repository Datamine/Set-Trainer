-- John Loeber -- March 2015 -- www.johnloeber.com -- elm 0.14.1
-- note on style: I just couldn't make elm work with the 80char rule.
-- still not sure how to elegantly write elm. My style in this piece is pretty disastrous; apologies.
-- (I rushed the production of this -- it really needs to be refactored for style.)

module Setgame where

import Window
import Signal(..)
import Signal
import List((::))
import Text 
import Graphics.Element as E
import Graphics.Input (button, customButton)

import Graphics.Collage(..)
import Graphics.Collage as C
import Random(generate,Generator,int,Seed,initialSeed)
import Color as Co
import List
import Mouse
import Debug(crash)

--Card Stuff -------------------------------------------------------------------

-- properties of the cards
type NumShape = One | Two | Three
type Shape = Circle | Square | Triangle
type Filling = Empty | Striped | Full
type Color = Green | Red | Purple

type Card = Card (NumShape, Shape, Filling, Color)

-- this is type-unsafe, so I must be sure to use it correctly.
getIndex : List a -> Int -> a
getIndex list i = List.head (List.drop i list)

-- to make a new card
newCard : Seed -> (Card,Seed)
newCard s = 
    let p1 = generate probability s
        p2 = generate probability (snd p1)
        p3 = generate probability (snd p2)
        p4 = generate probability (snd p3)
        nums = [One,Two,Three]
        shapes = [Circle,Square,Triangle]
        fillings = [Empty, Striped, Full]
        colors = [Green,Red,Purple]        
    in (Card (getIndex nums (fst p1),getIndex shapes (fst p2), getIndex fillings (fst p3), getIndex colors (fst p4)),(snd p4))

--Probability Stuff ------------------------------------------------------------

-- used the "At startup with a port" solution posted on http://stackoverflow.com/questions/28606248 for this.
-- (uncomment the two lines below if you're running this file using elm-reactor for testing purposes)
--startTimeSeed : Seed
--startTimeSeed = initialSeed 5

-- (comment out the three lines below if you're running this file using elm-reactor for testing)
port startTime : Float
startTimeSeed : Seed
startTimeSeed = initialSeed <| round startTime

-- makes some number of cards
makeCards : Int -> Seed -> List Card -> (List Card,Seed)
makeCards number seed list = 
    if number == 0 then (list,seed)
    else 
        let (c,s) = newCard seed
        in if (not (List.member c list)) then  makeCards (number-1) s (c::list)
        else makeCards number s list

probability : Generator Int
probability = int 0 2

-- makes new cards. guards against presence of duplicates.
makeNewCards : List Card -> Int -> Seed -> (List Card,Seed)
makeNewCards cards num s = 
    let new = makeCards num s []
        add = fst new
        sp = snd new
    in
    if (List.any (\x -> (List.member x cards)) add) then (makeNewCards cards num sp)
    else let almost = cards++add in 
    if (setCheck almost) then (almost,sp)
    else (makeNewCards [] 12 sp)

--Set Stuff --------------------------------------------------------------------

-- checks if three cards constitute a valid set
setTest : Card -> Card -> Card -> Bool
setTest (Card (numa,shapea,filla,colora)) (Card (numb,shapeb,fillb,colorb)) (Card (numc,shapec,fillc,colorc)) = 
    let checkn = ((numa==numb)&& (numa==numc)) || ((numa/=numb)&&(numa/=numc)&&(numb/=numc))
        checks = ((shapea==shapeb)&& (shapea==shapec)) || ((shapea/=shapeb)&&(shapea/=shapec)&&(shapeb/=shapec))
        checkf = ((filla==fillb)&& (filla==fillc)) || ((filla/=fillb)&&(filla/=fillc)&&(fillb/=fillc))
        checkc = ((colora==colorb)&& (colora==colorc)) || ((colora/=colorb)&&(colora/=colorc)&&(colorb/=colorc))
    in checkn && checks && checkf && checkc

-- recursively checks if there's a set in the list of card-triples
recurCheck : List (Card,Card,Card) -> Bool
recurCheck cardl =  
    if cardl == [] then False
    else let (a,b,c) = List.head cardl in if (setTest a b c) then True else recurCheck (List.tail cardl)

-- checks if there is a set among the three cards. This is pretty inefficient. TODO: write better algorithm in Subset.elm.
-- currently this generates a list of size 1728 as opposed to a list of size 220. This should be improved.
setCheck : List Card -> Bool
setCheck cards = 
    let allcombinations = (List.map (\x -> List.map (\y -> List.map (\z -> (x,y,z)) cards)cards)cards)
        flattened = List.foldr (++) [] (List.foldr (++) [] allcombinations)
    in recurCheck flattened

-- Drawing Cards, Buttons ------------------------------------------------------

cardStyle : LineStyle
cardStyle = { defaultLine | width <- 6}

shapeStyle : Co.Color -> LineStyle
shapeStyle c = { defaultLine | width <- 4,  color <- c }

-- TODO: implement the shapes being actually striped rather than just low-opacity
-- Tried, couldn't get it to work. Problems with Graphics.Collage.Textured.
-- Satisfied with current state, low-opacity is more elegant anyway.

-- draws a card
drawCard : Card -> Form
drawCard (Card (num, shap, fill, col)) =
    let outline = group[C.filled Co.lightGrey (C.rect 140 240), C.outlined cardStyle (C.rect 140 240)]
        shape = case shap of Circle     -> C.circle 30
                             Square     -> C.square 60
                             Triangle   -> C.ngon 3 30
        color = case col of Green   -> Co.darkGreen
                            Red     -> Co.red
                            Purple  -> Co.purple
        filledshape = case fill of  Empty   -> C.group [C.outlined (shapeStyle color) shape]
                                    Full    -> C.filled color shape
                                    Striped -> C.group[C.outlined (shapeStyle color) shape,C.alpha 0.25 (C.filled color shape)]
        n = case num of One -> 1
                        Two -> 2
                        Three -> 3
     in if 
        | n==1 -> group [outline, filledshape]
        | n==2 -> group [outline, move (0,-40) filledshape, move (0,40) filledshape]
        | n==3 -> group [outline, move (0,-80) filledshape, filledshape, move (0,80) filledshape]

-- yellow rectangle to signify a selected card
yellowrec : Form
yellowrec = 
    let recStyle = { defaultLine | width <- 6,  color <- Co.lightYellow }
    in C.outlined recStyle (C.rect 140 240)

-- typeface for the counts
toText : Co.Color -> String -> number -> C.Form
toText c s h = 
    C.toForm <| (Text.fromString s
        |> Text.color c
        |> Text.typeface ["Helvetica Neue","Sans-serif"]
        |> Text.height h
        |> Text.centered)

-- State Stuff -----------------------------------------------------------------

type alias Current = List Card
-- selected: (Index,1) if the corresponding card is selected, (Index,0) if not
type alias Selected = List (Int,Int)
type alias Right = Int
type alias Wrong = Int

type alias State = (Current,Selected,Right,Wrong,Seed)

locations : List (number,number)
locations = [(-310,260),(-150,260),(10,260),(170,260),(-310,0),(-150,0),(10,0),(170,0),(-310,-260),(-150,-260),(10,-260),(170,-260)]

blankSelect : List (Int,Int)
blankSelect = [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0),(9,0),(10,0),(11,0)]

initState : State
initState = 
    let cards = makeNewCards [] 12 startTimeSeed
        curr = fst cards
        s = snd cards
    in (curr,blankSelect,0,0,s)

type alias Click = (Int,Int)

-- finds on which card (or nowhere) the user clicked
getClick : Int -> Int -> List (number,number) -> Int -> Int
getClick x y list tracker = 
    if list==[] then 1000 else
    let (locx,locy) = List.head list
    in  if  (locx-70 <= x && x <= locx + 70) &&
            ((locy-120) <= y && y <= (locy + 120))
        then tracker
        else getClick x y (List.tail list) (tracker+1)

-- finds the selected cards out of the list of cards
getselected : List (Int,Int) -> List Card -> List Card
getselected selected cards = 
    if selected==[] then [] else
    let (index,select)=List.head selected
        c = List.head cards
    in if select==1 then (c::getselected (List.tail selected) (List.tail cards)) else (getselected (List.tail selected) (List.tail cards))

-- originally did this with a map expression and Bitwise.xor, but it threw an error
-- so I had to replace it with this function. Purpose: change the 'selected' list according to what the user has clicked
getNewselected : Int -> List (Int,Int) -> List (Int,Int)
getNewselected cardloc selected = 
    if selected==[] then [] else
    let (a,b) = List.head selected
        newb = if b==0 then 1 else 0
    in 
    if a==cardloc then (a,newb):: getNewselected cardloc (List.tail selected)
    else (a,b):: getNewselected cardloc (List.tail selected)

upState : Click -> State -> State
upState (x,y) (current,selected,right,wrong,seed) =
    let cardloc = (getClick x y locations 0) in
    if cardloc == 1000 then (current,selected,right,wrong,seed)
    else let newselected = (getNewselected cardloc selected) in
         if (List.sum (snd (List.unzip newselected)))/=3 then (current,newselected,right,wrong,seed) else
         let newnewselected = List.map (\(a,b) -> (a,0)) selected
             selectedset = getselected newselected current
             nonselectedset = List.filter (\x -> if (List.member x selectedset)==False then True else False) current
         in case selectedset of [c1,c2,c3] -> if not (setTest c1 c2 c3) then (current,blankSelect,right,wrong+1,seed)
                                             else let newcards = makeNewCards nonselectedset 3 seed
                                                      news = fst newcards
                                                      s = snd newcards
                                             in (news,blankSelect,right+1,wrong,s)
                                _          -> crash "165"

view (w,h) (current,selected,right,wrong,seed) = 
    let cards = List.map (\x -> drawCard x) current
        selects = List.map(\(_,ff) -> if ff==1 then yellowrec else (toForm E.empty)) selected
        cs = List.map2 (\ card select -> C.group [card,select]) cards selects
        rr= move(320,310)  <| toText Co.darkGreen (toString right) 72
        ww = move(320,210)  <| toText Co.red (toString wrong) 72
        cap1 = move (320,355) <| toText Co.darkGreen "Number Correct" 12
        cap2 = move (320,170) <| toText Co.red "Number Incorrect" 12
        coll = C.collage 800 800 ([C.filled Co.grey (C.square 1000)] ++ [rr,ww,cap1,cap2] ++ List.map2(\(f,s) c -> move (f,s)c) locations cs)
    in E.container (w-1) (h-1) E.middle coll

clickLoc : Signal (Int,Int)
clickLoc  = Signal.sampleOn Mouse.clicks Mouse.position

clickDims : Signal (Int,Int)
clickDims = Signal.sampleOn Mouse.clicks Window.dimensions

-- to convert between the collage-coordinates (central) and window-coordinates (top left)
clickAdjusted (x,y) (w,h) =
    let xp = x - (w//2)
        yp = y - (h//2)
    in (xp,-yp)

main : Signal E.Element
main =
    let ca = (Signal.map2 clickAdjusted clickLoc clickDims) in
  Signal.map2 view Window.dimensions (Signal.foldp (\ (a,b) (c,s,r,w,sd) -> upState(a,b)(c,s,r,w,startTimeSeed)) initState ca)
