module People (attemptTurn,LocatedPeople, Side(Terrorist,Civilian), Vitals(Dead,Alive), init, isTurned, attemptKill) where

import List exposing (map)

import Basics

type Side = Terrorist | Civilian

type Vitals = Alive | Dead

type alias People = { side : Side, vital : Vitals }

type alias LocatedPeople = { people: People, x : Int, y : Int}



init : Float -> Int -> Int -> LocatedPeople
init sideSeed x y  =
    let side = if sideSeed < 0.3 then Terrorist else Civilian in     
    { people = { side = side, vital = Alive}, x = x, y = y }


isTerrorist : LocatedPeople -> Bool
isTerrorist {people,x,y} =  
    if people.side == Terrorist then True else False
    
distance : (Int, Int) -> (Int, Int) -> Int
distance (x,y) (x',y') = abs (x - x') + abs (y - y')

isTurned : Int -> LocatedPeople -> List(LocatedPeople) -> Bool
isTurned radius {people,x,y} kills = if (people.vital == Dead) then False else 
    List.any (turns radius (x,y)) kills

turns : Int -> (Int,Int) -> LocatedPeople -> Bool
turns radius (x,y) peo = if peo.people.side == Terrorist then False else if (distance ( x, y) ( peo.x, peo.y) < radius) then True else False

isKilled : Int -> LocatedPeople -> (Int,Int) -> Bool
isKilled radius {people,x,y} (x1,y1) = if (distance (x1,y1) (( x),( y))) < radius then True else False  

attemptKill : Int -> LocatedPeople -> (Int, Int) -> LocatedPeople
attemptKill radius people (x,y) = if 
                (isKilled radius people (x,y)) then 
                    {people | people <- {side = people.people.side, vital = Dead}}
                else people    

attemptTurn : Int -> LocatedPeople -> List(LocatedPeople) -> LocatedPeople
attemptTurn radius people kills = if (isTurned radius people kills) then
        {people | people <- {side = Terrorist , vital = Alive}}
    else people

side : Float -> Side
side b = if b < 0.3 then Terrorist else Civilian







