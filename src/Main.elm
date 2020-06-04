port module Main exposing (..)

import Browser
import Dict            as Dict exposing (Dict)
import Html            as Html exposing (Html)
import Html.Attributes as Html
import Html.Events     as Html
import Json.Encode     as Encode
import Json.Decode     as Decode

type alias URL = String
type alias Bookmark = (String, URL)
type alias Model = { bookmarks: Dict String URL, flux: Flux }
type alias Flux = { name: String, url: URL, showAdd: Bool }

type Field = Name | Url
type Event =
    Create
  | Delete String
  | Typing Field String
  | ToggleAdd

empty : Model
empty = { bookmarks = Dict.empty, flux = { name = "", url = "", showAdd = False } }

main : Program Encode.Value Model Event
main = Browser.element 
       { init = load
       , view = view
       , update = update
       , subscriptions = \_ -> Sub.none 
       }

load : Encode.Value -> (Model, Cmd Event)
load flags = Decode.decodeValue (Decode.dict Decode.string) flags
  |> Result.map (\parsed -> {empty| bookmarks = parsed })
  |> Result.withDefault empty
  |> void

void : Model -> (Model, Cmd Event)
void m = (m, Cmd.none)

save : Model -> (Model, Cmd Event)
save m = (m, write <| encode m)

update : Event -> Model -> (Model, Cmd Event)
update event model =
    case event of
        Create             ->
            if not (String.isEmpty model.flux.name || String.isEmpty model.flux.url) then
                save { model | bookmarks = Dict.insert model.flux.name model.flux.url model.bookmarks }
            else void model
        Delete name        -> save { model | bookmarks = Dict.remove name model.bookmarks }
        Typing Name name   -> let flux_ = model.flux in
            void { model | flux = { flux_ | name = name } }
        Typing Url url     -> let flux_ = model.flux in
            void { model | flux = { flux_ | url = url } }
        ToggleAdd          -> let flux_ = model.flux in
            void { model | flux = { flux_ | showAdd = not flux_.showAdd } }
                           
view : Model -> Html Event
view model = Html.div [] <| bookmarks model ++ [new model.flux.showAdd]

bookmarks : Model -> List (Html Event)
bookmarks = List.map bookmark << Dict.toList << .bookmarks
                 
bookmark : Bookmark -> Html Event
bookmark (name, destination) = Html.div []
  [ Html.a [Html.href destination] [Html.text name]
  , Html.button [Html.onClick (Delete name)] [Html.text "x"]
  ]

toggle : Html Event
toggle = Html.button [Html.onClick ToggleAdd] [Html.text "..."]

new : Bool -> Html Event
new visible = Html.span [] <|
  if visible then
      [ toggle
      , Html.input  [ Html.type_ "text", Html.placeholder "Name", Html.onInput (Typing Name) ] []
      , Html.input  [ Html.type_ "text", Html.placeholder "URL", Html.onInput (Typing Url)  ] []
      , Html.button [ Html.onClick Create ] [ Html.text "+" ]
      ]
  else [toggle]

port write : Encode.Value -> Cmd msg

encode : Model -> Encode.Value
encode m = Encode.dict identity Encode.string m.bookmarks
