module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, input, br, option, select)
import Html.Events exposing (onClick, onInput)
import Html.Attributes as Attr exposing (type_)
import ListWrapper.Dict as Dict exposing (Dict)

type alias Pos4D a =
  { x : a
  , y : a
  , z : a
  , w : a
  }

type OorX = O | X
type State
  = Playing OorX
  | Won OorX

type alias Model =
  { field : Dict (Pos4D Int) OorX
  , state : State
  , log : List String
  , inputPos : Pos4D Int
  }

initialModel : Model
initialModel =
  { field = Dict.empty
  , state = Playing O
  , log = []
  , inputPos = Pos4D 0 0 0 0
  }

type Pos4 = X_ | Y_ | Z_ | W_

type Msg
  = Input Pos4 String
  | Submit

update msg model =
  case msg of
    Input pos str ->
      let
        n =
          String.toInt str
            |> Maybe.withDefault 0
        updatePos pos_ =
          { model | inputPos = pos_ }
        current = model.inputPos
      in
        case pos of
          X_ -> updatePos { current | x = n }
          Y_ -> updatePos { current | y = n }
          Z_ -> updatePos { current | z = n }
          W_ -> updatePos { current | w = n }
    Submit ->
      case Dict.get model.inputPos model.field of
        Just _ ->
          { model
          | log = model.log ++ ["already exists"]
          }
        Nothing ->
          case model.state of
            Playing O ->
              { model
              | log = delLog model.log ++ ["O : " ++ pos2Str model.inputPos]
              , field = Dict.insert model.inputPos O model.field
              , state = Playing X
              }
                |> victoryUpdate O model.inputPos
            Playing X ->
              { model
              | log = delLog model.log ++ ["X : " ++ pos2Str model.inputPos]
              , field = Dict.insert model.inputPos X model.field
              , state = Playing O
              }
                |> victoryUpdate X model.inputPos
            _ -> model

pos2Str pos =
  [pos.x, pos.y, pos.z, pos.w]
    |> List.map String.fromInt
    |> List.map (String.padRight 2 ' ')
    |> String.join ","
    |> \str -> "("++str++")"

victoryUpdate ox pos model =
  if victoryCheck ox pos model.field
  then
    { model
    | state = Won ox
    , log =
        model.log ++
        [(if ox == O then "O" else "X") ++ " won"]
    }
  else
    model

victoryCheck ox pos field =
  List.map
    (\line ->
      List.foldl
        (\p nOfCorrect ->
          case Dict.get (posPlus pos p) field of
            Just s ->
              if s == ox
              then nOfCorrect + 1
              else nOfCorrect
            _ -> nOfCorrect
        )
        0
        line
    )
    victoryLineList
  |> List.member 5

posPlus pos1 pos2 =
  { x = pos1.x + pos2.x
  , y = pos1.y + pos2.y
  , z = pos1.z + pos2.z
  , w = pos1.w + pos2.w
  }

victoryLineList : List (List (Pos4D Int))
victoryLineList =
  let
    onePosList =
      [List.range -4 4]
        |> cross
          (\n li ->
            List.map ((*)n) li
          )
          [1,0,-1]
        |> List.concat

    cross4D lx ly lz lw =
      let
        lxy =
          cross (List.map2 Tuple.pair) lx ly
            |> List.concat
        lzw =
          cross (List.map2 Tuple.pair) lz lw
            |> List.concat
      in
        cross
          ( List.map2
            (\(x,y) (z,w) ->
              Pos4D x y z w
            )
          )
          lxy lzw
        |> List.concat

  in
    cross4D
      onePosList
      onePosList
      onePosList
      onePosList
    |> List.filter
      ( (/=)
          ( List.repeat 5
            (Pos4D 0 0 0 0)
          )
      )

delLog =
  List.filter ((/=) "already exists")

cross : (a -> b -> c) -> List a -> List b -> List (List c)
cross f la lb =
  List.map
    (\a ->
      List.map (f a) lb
    )
    la


view model =
  let
    input_ pos =
      select
        [ onInput <| Input pos ]
        options

    options =
       List.map
         (\n ->
           option
             [ Attr.value <| String.fromInt n
             , Attr.selected (n == 0)
             ]
             [ text <| String.fromInt n]
         )
         (List.range -2 2 |> List.reverse)

    playingView =
      div []
        <| List.concat
         [ List.map input_ [X_,Y_,Z_,W_]
         , [ button [onClick Submit][text "Submit"]
           , br[][]
           ]
         , [ div
             [ Attr.style "width" "150px"
             , Attr.style "height" "300px"
             , Attr.style "overflow" "scroll"
             ]
             ( List.map text model.log
                 |> List.intersperse (br[][])
             )
           ]
         ]

    winView =
      div
          [ Attr.style "width" "150px"
          , Attr.style "height" "300px"
          , Attr.style "overflow" "scroll"
          ]
        ( List.map text model.log
          |> List.intersperse (br[][])
        )

  in
    case model.state of
      Playing _ -> playingView
      Won _ -> winView

main : Program () Model Msg
main =
  Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }
