import Html
import Html.Attributes
import Svg
import Svg.Attributes
import Time exposing (Time, second)
import Maybe exposing (withDefault)
import Keyboard exposing (KeyCode)
import Random

main = 
    Html.program 
        {init=init
        , view=view
        , update=update
        , subscriptions=subscriptions
        }

---MODEL
type alias Model =
    {xchange: Int,
    ychange: Int,
    snake: List Bod,
    food: Food,
    blocks : List Block,
    inplay: Play}

type alias Bod =
    {x: Int,
    y: Int}

type alias Food =
    {x: Int,
    y: Int}

type alias Block =
    {x: Int,
    y: Int}

scale = 5


move : List Bod -> Bod -> List Bod
move snek h = 
    h :: List.reverse (withDefault [] (List.tail (List.reverse snek)) )

nexth : Model -> Bod
nexth model =
    Bod ((withDefault (Bod 0 0) (List.head model.snake)).x+model.xchange) ( (withDefault (Bod 0 0) (List.head model.snake)).y+model.ychange)

overlap x f =
    (x.x == f.x) && (x.y==f.y)

tick model =
    if (overlap (withDefault (Bod 0 0) (List.head model.snake)) model.food)
    then Eat
    else if ( (withDefault (Bod 0 0) (List.head model.snake)).x > 19)||((withDefault (Bod 0 0) (List.head model.snake)).x < 0)||((withDefault (Bod 0 0) (List.head model.snake)).y < 0)|| ((withDefault (Bod 0 0) (List.head model.snake)).y > 19) || (List.foldl (\n a-> if a then a else n) False (List.map (overlap (withDefault (Bod 0 0) (List.head model.snake))) (withDefault [] (List.tail model.snake)))) || overlapsblock model (withDefault (Bod 0 0) (List.head model.snake))
        then Die
    else Go
neednew : Model -> (Int, Int) -> Msg
neednew mod tup =
    if (overlapsblock mod (Food (Tuple.first tup) (Tuple.second tup))) then NeedNew
    else NewFood tup

newfood mod tup = NewFood tup

overlapsblock: Model->{x:Int,y:Int}->Bool
overlapsblock m f =
    List.foldl (\n a -> if a then a else n) False (List.map (overlap f) m.blocks) 

---UPDATE
type Play = InProgress | Stopped

type Msg = Go Time| Right | Left | Up | Down | Eat Time| NewFood (Int, Int) |NeedNew| Die Time|Noop
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        Go t->
            ({model|snake = move model.snake (nexth model)}, Cmd.none)
        Right ->
            (Model 1 0 model.snake model.food model.blocks model.inplay, Cmd.none)
        Left ->
            (Model -1 0 model.snake model.food model.blocks model.inplay, Cmd.none)
        Down ->
            (Model 0 1 model.snake model.food model.blocks model.inplay, Cmd.none)
        Up ->
            (Model 0 -1 model.snake model.food model.blocks model.inplay, Cmd.none)
        Eat t->
            ({model|snake = List.concat [model.snake, [(nexth model)]]}, Random.generate (neednew model) (Random.pair (Random.int 0 19) (Random.int 0 19)))

        NewFood f-> 
            ({model | food = Food (Tuple.first f) (Tuple.second f)}, Cmd.none)
        NeedNew ->
            (model, Random.generate (neednew model) (Random.pair (Random.int 0 19) (Random.int 0 19)))
        Die t->
            (Model 0 0 model.snake model.food model.blocks Stopped, Cmd.none)
        Noop ->
            (model, Cmd.none)

drawBod : {x:Int, y:Int} ->String-> Svg.Svg msg
drawBod b color = Svg.rect [Svg.Attributes.fill color,Svg.Attributes.height "30", Svg.Attributes.width "30", Svg.Attributes.x (toString (b.x*30)), Svg.Attributes.y (toString (b.y*30))] []

drawSnake : Model -> List (Svg.Svg msg)
drawSnake model = [Svg.svg [Svg.Attributes.height "100%", Svg.Attributes.width "100%"]
                ((drawBod model.food "#EE312F" ) :: List.concat [List.foldr (List.append) [] (List.map (\s -> [drawBod s "#00B4EB"]) model.snake), List.foldr (List.append) [] (List.map (\s -> [drawBod s "#7C8084"]) model.blocks)])]

---VIEW
view model =
    case model.inplay of
        InProgress ->
            Html.div [Html.Attributes.class "container"] 
            [Html.div [Html.Attributes.class "meow"] (drawSnake model),
            Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []]
        Stopped ->
            Html.div [Html.Attributes.class "dead"] [Html.text "Game Over", Html.node "link" [Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css"] [] ]

---SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
      Sub.batch [Time.every (100) (tick model), Keyboard.ups (key model)]

left model =
    if model.xchange == 0 then Left else Right
up model = if model.ychange == 0 then Up else Down
down model = if model.ychange == 0 then Down else Up
right model = if model.xchange == 0 then Right else Left

key : Model -> KeyCode ->  Msg
key m code =
    case code of
        37 -> left m
        39 -> right m
        38 -> up m
        40 -> down m
        87 -> up m
        65 -> left m
        83 -> down m
        68 -> right m
        _ -> Noop

init = (Model 0 1 [] (Food 17 16) [Bod 15 0, Bod 11 2, Bod 11 3, Bod 11 15, Bod 11 16, Bod 11 17, Bod 11 18, Bod 11 19, Bod 12 15, Bod 12 16, Bod 12 17, Bod 12 18, Bod 12 19, Bod 13 15, Bod 13 16, Bod 13 17, Bod 13 18, Bod 13 19, Bod 14 15, Bod 14 16,Bod 14 17,Bod 14 18,Bod 14 19,Bod 15 15,Bod 15 16,Bod 15 17,Bod 15 18,Bod 15 19,Bod 11 7,Bod 12 7,Bod 13 7,Bod 14 7,Bod 15 7,Bod 16 7,Bod 17 7,Bod 18 7,Bod 19 7,Bod 12 7,Bod 11 8,Bod 12 8, Bod 13 8,Bod 14 8,Bod 15 8,Bod 16 8,Bod 17 8,Bod 18 8,Bod  19 8, Bod 15 9,Bod 16 9,Bod 17 9,Bod 18 9,Bod 19 9,Bod 8 9, Bod 8 12, Bod 8 15,Bod 8 18,Bod 2 4, Bod 3 4,Bod 4 4, Bod 2 11,Bod 3 11, Bod 4 11,Bod 2 12,Bod 3 12,Bod 4 12, Bod 2 13, Bod 3 13,Bod 4 13, Bod 6 9, Bod 6 12, Bod 6 15, Bod 6 18] InProgress, Cmd.none)
