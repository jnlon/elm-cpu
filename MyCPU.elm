-- http://www.cas.mcmaster.ca/~anand/1JC3Pics/CPU.pdf
-- mm's this week go to chinnh@mcmaster.ca (Natalie)
module MyCPU exposing (..)

import RunCPU exposing (..)
import CPU exposing (..)

import Array exposing (Array)
import GraphicSVG exposing (..)
import List exposing(concat,map,map2,foldr,indexedMap,filter,concatMap)
import Time
import Set
import List exposing (member)

--main = show <| (initialState, initialTinyData)

stateOutputToGraphic = []

range start stop step =
    if (start <= stop) then
        start :: (range (start + step) stop step)
    else
        []

getInstructionStringHistory programGetter count =
  let getInstr i =
      case (programGetter i) of
        Just i -> toString i 
        _ -> ""
  in
  List.reverse (List.map getInstr (range 0 count 1))

makeTextList lst =
  let putLine i str = 
        text str |> size 8 |> filled black |> move (0,(toFloat -(i*20)))
      arrow = 
        group
          [ line (0,0) (40,0) |> outlined (solid 4) red
          , line (0,0) (15,10) |> outlined (solid 4) red
          , line (0,0) (15,-10) |> outlined (solid 4) red
          , circle 2 |> filled red |> move (0,0) ]
  in
    group [ arrow |> rotate (degrees 180) |> move (0, -20)
          , (group (List.indexedMap putLine lst)) |> move (5, -5) ]


makeRegBoxes regs cmp = 
  case regs of 
    (r1,r2,r3,r4,r5,r6,r7,r8) ->
      group 
      [  rect 400 150 |> filled grey
      ,  text "REGISTERS" |> bold |> size 15 |> filled black |> move (-90, 30)
      ,  text ("r1=[" ++ (toString r1)++"]") |> size 14 |> filled black |> move (-90, 00)
      ,  text ("r2=[" ++ (toString r2)++"]") |> size 14 |> filled black |> move (0, 00)
      ,  text ("r3=[" ++ (toString r3)++"]") |> size 14 |> filled black |> move (90, 00)
      ,  text ("r4=[" ++ (toString r4)++"]") |> size 14 |> filled black |> move (-90, -30)
      ,  text ("r5=[" ++ (toString r5)++"]") |> size 14 |> filled black |> move (0, -30)
      ,  text ("r6=[" ++ (toString r6)++"]") |> size 14 |> filled black |> move (90, -30)
      ,  text ("r7=[" ++ (toString r7)++"]") |> size 14 |> filled black |> move (-90, -60)
      ,  text ("r8=[" ++ (toString r8)++"]") |> size 14 |> filled black |> move (0, -60)
      ,  text ("cmp=[" ++ (toString cmp)++"]") |> size 14 |> filled black |> move (90, -60) ]


--type Instruction  = Load RegisterNumber    -- put value here
--                  RegisterNumber           -- from address which is sum of this register value
--                  RegisterNumber           -- and this register value
-- reading a number in the margin of the Sudoku puzzle
-- | Store          RegisterNumber           -- store value in this register
--                  RegisterNumber           -- to address which is sum of this register value
--                  RegisterNumber           --   and this register value
-- writing something in the margin of the Sudoku puzzle
-- | LoadImmediate  RegisterNumber           -- put value here
--                  RegisterValue            -- the value
-- reading a number as part of a logic puzzle
-- | Add            RegisterNumber           -- put result here
--                  RegisterNumber           -- first thing to add
--                  RegisterNumber           -- second thing to add
--
-- | Multiply       RegisterNumber           -- put result here
--                  RegisterNumber           -- first thing to multiply
--                  RegisterNumber           -- second thing to multiply
--
-- | And            RegisterNumber           -- put result here
--                  RegisterNumber           -- first thing to and
--                  RegisterNumber           -- second thing to and
--
-- | Or             RegisterNumber           -- put result here
--                  RegisterNumber           -- first thing to or
--                  RegisterNumber           -- second thing to or
--
-- | Not            RegisterNumber           -- put result here
--                  RegisterNumber           -- reverse bits from here
--
-- | Rotate         RegisterNumber           -- put result here
--                  RegisterNumber           -- value to rotate
--                  Int                      -- rotate bits (left is positive)
--
-- | Compare        RegisterNumber           -- compare the value in this register
--                  RegisterNumber           -- to the value in this register (result goes in CPU State)
--
-- | Branch         (List ComparisonResult)  -- results (maybe all) which will cause branch
--                  RegisterNumber           -- instruction to branch to if true
--
-- | Halt



regToArray regs = case regs of
    (r1,r2,r3,r4,r5,r6,r7,r8) -> Array.fromList [r1,r2,r3,r4,r5,r6,r7,r8]

cmpToString cmp = 
  case cmp of
    EQ -> "="
    LT -> "<"
    GT -> "<"

-- TODO: Implement regLookup,
-- a List with with values of registers at index, so we can use their values here
makeAlu instr regArray cmp = 
  let 
    r i = Maybe.withDefault 0 (Array.get i regArray) -- Get register
    str s = toString s
    msg = 
      case instr of
         Load ra rb rc -> "Load " ++ (str ra) ++ " into " ++ (str rc)
         Store ra rb rc -> "Store " ++ (str ra) ++ " into address at " ++ (str (rb + rc))
         LoadImmediate ra v -> "LoadImmediate: Load " ++ (str v) ++ " @reg " ++ (str ra)
         Add ra rb rc -> "Add: "++(str rb)++" + "++(str rc)++" = "++(str (r ra) ++ " (@reg " ++ (str ra) ++ ")")
         Multiply ra rb rc -> "Multiply: "++(str rb)++" + "++(str rc)++" = "++(str (r ra) ++ " (@reg " ++ (str ra) ++ ")")
         And ra rb rc -> "And: "++(str rb)++" + "++(str rc)++" = "++(str (r ra) ++ " (@reg " ++ (str ra) ++ ")")
         Or ra rb rc -> "Or: "++(str rb)++" + "++(str rc)++" = "++(str (r ra) ++ " (@reg " ++ (str ra) ++ ")")
         Not ra rb -> "Not: " ++ "!" ++ (str rb) ++ " (@reg " ++ (str ra) ++ ")"
         Rotate ra rb i -> "Rotate: " ++ (str ra) ++ " " ++ (str rb)
         Compare ra rb -> "Compare: "++(str (r ra)) ++ " `compare` " ++ (str (r rb)) ++ " = " ++ (toString (compare (r ra) (r rb)))
         Branch lst ra -> "Branch: " ++ if (cmp `member` lst) then ("@reg "++ (str ra)) else "Not Branching"
         Halt -> "Halt"
    aluBox = 
      group 
        [ rect 400 100 |> filled lightOrange |> move (0,-20)
        , text "ALU" |> bold |> size 15 |> filled black |> move (-90,0)]
    in 
  group 
  [ aluBox
  , text msg |> size 10 |> filled black |> move (-90,-30) ]

view model = 
  case model.cpu of 
    CPUState regs curr cmp Nothing ->
      let progLst = getInstructionStringHistory model.program (curr+1)
          thisInstr = 
            case model.program curr of
               Just i -> i
               _ -> Halt
          regArray = regToArray regs
      in 
        collage 
          500 
          500 
          [ makeTextList progLst |> move (100,200)
          , makeRegBoxes regs cmp |> move (-150, 200)
          , makeAlu thisInstr regArray cmp |> move (-150, 100)
          -- , model.stateOutput |> move (100,22)
          --, model.instrOutput |> move (-10,-22)
          , circle 10 |> filled (if isHalted model.cpu then red else green)
                      |> notifyTap NextInstr ]
    _ -> collage 500 500 []

type Msg = NextInstr

update NextInstr model =
  case model.cpu of

     CPUState regs curr cmp Nothing ->
       let ((newCpu,newDat),thisInstr) =
             case model.program curr of
               Just i  -> (executeOne i (model.cpu, model.dat),toString i)
               Nothing -> ((CPUState regs curr cmp (Just IllegalInstrAddress),model.dat),"illegal")
       in { model | cpu = newCpu
                  , dat = newDat
                  , stateOutput = group [ toString newCpu |> text |> centered |> filled brown
                                                                  , model.stateOutput |> move (0,18)
                                                                  ]
                  , instrOutput = group [ thisInstr       |> text |> centered |> filled green
                                                                  , model.instrOutput |> move (0,-18)
                                                                  ]
          }

     CPUState regs curr cmp (Just halt) -> model -- CPU has stopped

init = { cpu = initialState
       , dat = initialData
       , stateOutput = group [toString initialState |> text |> centered |> filled brown ]
       , instrOutput = group []
       , program = mkProgram [ LoadImmediate 1 7
                             , LoadImmediate 2 3
                             , LoadImmediate 4 20
                             , LoadImmediate 5 4
                             , Add 1 2 1
                             , Compare 1 4
                             , Branch [LT] 5
                             , Halt
                             ]
       }

main = notificationsApp { model = init , view = view , update = update }

--main = show <| runProgram (mkProgram [LoadImmediate 1 7
--                                     ,Halt]
--                          )
--                          (initialState, initialTinyData)

--view = collage 600 600 <|
--  [text <| toString initialState]
