-- http://www.cas.mcmaster.ca/~anand/1JC3Pics/CPU.pdf
-- mm's this week go to chinnh@mcmaster.ca (Natalie)
module TryCPU exposing (..)

import RunCPU exposing (..)
import CPU exposing (..)

import Array exposing (Array)
import GraphicSVG exposing (..)
import List exposing(concat,map,map2,foldr,indexedMap,filter,concatMap)
import Time
import Set

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
    group [ arrow |> rotate (degrees 180) |> move (0, -40)
          , (group (List.indexedMap putLine lst)) |> move (5, -5) ]


makeRegBoxes regs = 
  case regs of 
    (r1,r2,r3,r4,r5,r6,r7,r8) ->
      group 
      [  rect 200 150 |> filled grey
      ,  text ("r1=[" ++ (toString r1)++"]") |> size 14 |> filled black |> move (-90, 30)
      ,  text ("r2=[" ++ (toString r2)++"]") |> size 14 |> filled black |> move (-30, 30)
      ,  text ("r3=[" ++ (toString r3)++"]") |> size 14 |> filled black |> move (30, 30)
      ,  text ("r4=[" ++ (toString r4)++"]") |> size 14 |> filled black |> move (-90, 0)
      ,  text ("r5=[" ++ (toString r5)++"]") |> size 14 |> filled black |> move (-30, 0)
      ,  text ("r6=[" ++ (toString r6)++"]") |> size 14 |> filled black |> move (30, 0)
      ,  text ("r7=[" ++ (toString r7)++"]") |> size 14 |> filled black |> move (-90, -30)
      ,  text ("r8=[" ++ (toString r8)++"]") |> size 14 |> filled black |> move (-30, -30) ]


type Instruction  = Load           RegisterNumber RegisterNumber RegisterNumber
                  | Store          RegisterNumber RegisterNumber RegisterNumber
                  | LoadImmediate  RegisterNumber RegisterValue 
                  | Add            RegisterNumber RegisterNumber RegisterNumber
                  | Multiply       RegisterNumber RegisterNumber RegisterNumber
                  | And            RegisterNumber RegisterNumber RegisterNumber
                  | Or             RegisterNumber RegisterNumber RegisterNumber
                  | Not            RegisterNumber RegisterNumber 
                  | Rotate         RegisterNumber RegisterNumber Int           
                  | Compare        RegisterNumber RegisterNumber

                  | Branch         (List ComparisonResult)  -- results (maybe all) which will cause branch
                                   RegisterNumber           -- instruction to branch to if true

                  | Halt

-- TODO: Implement regLookup,
-- a List with with values of registers at index, so we can use their values here
makeAlu instr regLookup = 
  case instr of
     Load ra rb rc -> "Load " ++ (toString ra) ++ " into " (toString rc)
     Store ra rb rc -> "Store " ++ (toString ra) ++ " into address at " (rb + rc)
     LoadImmediate ra v -> 
     Add ra rb rc ->
     Multiply ra rb rc ->  
     And ra rb rc -> 
     Or ra rb rc ->  
     Not ra rb -> 
     Rotate ra rb i -> 
     Compare ra rb -> 
     Branch lst ra -> 
     Halt ->

view model = 
  case model.cpu of 
    CPUState regs curr cmp Nothing ->
      let progLst = getInstructionStringHistory model.program (curr+1)
          thisInstr = 
                case model.program curr of
                   Just i -> toString i
                   _ -> ""
      in 
        collage 
          500 
          500 
          [ makeTextList progLst |> move (100,200)
          , makeRegBoxes regs |> move (-100, 200)
          , model.stateOutput |> move (100,22)
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
