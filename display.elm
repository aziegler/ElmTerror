module Display (viewPop) where
import Graphics.Element exposing (show, flow, right, height, down)
import Graphics.Collage exposing (rect,collage, Form, move, filled, circle, solid, traced)
import Color exposing (gray, Color, black, red)
import Population
import People

viewPop : Int -> Int -> Int -> (Int,Int) -> Population.World -> Graphics.Element.Element
viewPop displaySize killRadius turnRadius dim world =
            let popCount = Population.countPop world.population in  
                Graphics.Element.layers [status popCount, 
                                        viewPeople world displaySize dim,
                                        displayAim world displaySize killRadius turnRadius dim]
 
status : (Int,Int,Int,Int) -> Graphics.Element.Element
status (alTer, alCiv, kilTer, kilCiv) = (flow down 
                    [flow right [show "Terrorists", 
                     show alTer],
                     flow right [show "Civils",
                     show alCiv],
                     flow right [show "Civilian Casualties",
                     show kilCiv]
                    ])

viewPeople : Population.World -> Int -> (Int,Int) -> Graphics.Element.Element
viewPeople world displaySize (a,b) =    collage a b (List.map (view displaySize (a,b)) world.population)

displayAim : Population.World -> Int -> Int -> Int -> (Int, Int) -> Graphics.Element.Element 
displayAim world displaySize killRadius turnRadius (a,b) =  collage a b 
    [(circle (toFloat killRadius) |> traced (solid red) |> move (toMoveCoord 0 (a,b) world.mousePos )),
     (circle (toFloat turnRadius) |> traced (solid gray) |> move (toMoveCoord 0 (a,b) world.mousePos ))]

view : Int -> (Int,Int) -> People.LocatedPeople -> Form
view displaySize (w,h) model = 
    let token = if model.people.vital == People.Alive then 
    rect 5 5 |> filled (color model.people.side) else 
    rect 5 5 |> filled gray 
    in
     token |> 
     move (toMoveCoord displaySize (w,h) (model.x, model.y) )
  
toMoveCoord : Int-> (Int,Int) -> (Int,Int) -> (Float,Float)
toMoveCoord offset (w,h) (x,y) = (toFloat(offset + x -   (w // 2)), 
            toFloat (( h // 2) -  y -  offset))

color : People.Side -> Color
color side = case side of 
    People.Terrorist -> red
    People.Civilian -> black