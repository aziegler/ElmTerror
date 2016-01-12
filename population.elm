module Population(Population, World, countPop, initPop) where 
import People exposing (LocatedPeople, Side, Vitals)
import Random

type alias Population = List (LocatedPeople)

type alias World = {population : Population, mousePos : (Int,Int)}


initPop : Int -> Int -> Int -> Population
initPop displaySize total initSeed =
    let sideGen = (Random.float 0 1) in 
    let posGen = (Random.int 0 displaySize) in
    let seed = (Random.initialSeed initSeed) in 
        let init' total pop seed = 
            if total == 0 then
                pop
            else
                let (randFloat, seed') = (Random.generate sideGen seed) in
                let (xGen, seed'') = (Random.generate posGen seed') in
                let (yGen, seed''') = (Random.generate posGen seed'') in
                init'
                    (total - 1)

                    ((People.init randFloat xGen yGen)::pop)
                    seed'''
        in init' total [] seed


countPop : Population -> (Int,Int,Int,Int)
countPop pop = let count' pop (alTer,alCiv,kilTer,kilCiv) = 
    case pop of 
        [] -> (alTer,alCiv,kilTer,kilCiv)
        {people,x,y}::tl ->
            case people.side of 
                People.Terrorist -> case people.vital of 
                    People.Dead -> count' tl (alTer,alCiv,kilTer+1,kilCiv)
                    People.Alive -> count' tl (alTer+1,alCiv,kilTer,kilCiv)
                People.Civilian -> case people.vital of 
                    People.Dead -> count' tl (alTer,alCiv,kilTer,kilCiv+1)
                    People.Alive -> count' tl (alTer,alCiv+1,kilTer,kilCiv)

    in count' pop (0,0,0,0)
