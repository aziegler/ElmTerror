module World where

import Signal exposing (foldp, map2, sampleOn, map)
import Window exposing (dimensions)
import Mouse exposing (position)
import Graphics.Element exposing (Element)
import Population
import Population exposing (Population)
import People exposing (LocatedPeople)
import Display exposing (viewPop)



type alias Parameters = {turnRadius : Int, popsize : Int, killRadius : Int, displaySize : Int}


config : Parameters
config = {turnRadius = 90, popsize = 200, killRadius = 20, displaySize = 400}

world : Signal Population.World
world = Signal.map2 (\pop -> \pos -> {population = pop, mousePos = pos}) (Signal.foldp modifyPop (Population.initPop config.displaySize config.popsize 4) clickLocations) Mouse.position

modifyPop : (Int,Int) -> Population  -> Population
modifyPop killLocation initPop = 
    let modify' pop (x,y) kills newPop = 
        case pop of 
            [] -> turnPopulation kills newPop
            hd::tl -> let modifyHd = (People.attemptKill config.killRadius hd ((x-config.displaySize), (y-config.displaySize)))
            in let newKills =
                if hd.people.vital == People.Alive && modifyHd.people.vital == People.Dead 
                    then modifyHd::kills
                    else kills
                in modify' tl killLocation newKills (modifyHd::newPop)
                      
    in modify' initPop killLocation [] []

turnPopulation :  List(LocatedPeople) -> Population -> Population
turnPopulation kills pop =
    case pop of  
    [] -> []
    hd::tl -> (People.attemptTurn config.turnRadius hd kills)::(turnPopulation kills tl)


countClick : Signal Int
countClick =
  Signal.foldp (\clk count -> count + 1) 0 Mouse.clicks



clickLocations : Signal (Int,Int)
clickLocations =
  Signal.sampleOn Mouse.clicks Mouse.position



main : Signal Element
main = Signal.map2  (Display.viewPop config.displaySize config.killRadius config.turnRadius) Window.dimensions world