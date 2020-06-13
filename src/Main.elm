port module Main exposing (..)

import Browser
import Browser.Dom     as Dom
import Dict            as Dict exposing (Dict)
import Html            as Html exposing (Html)
import Html.Attributes as Html
import Html.Events     as Html
import Json.Encode     as Encode
import Json.Decode     as Decode
import Task            as Task

type alias URL = String
type alias Bookmark = (String, URL)
type alias Model = { bookmarks: Dict String URL, flux: Flux }
type alias Flux = { name: String, url: URL, filter: String, showAdd: Bool, showFilter: Bool }

type Field = Name | Url | Filter
type Event =
    Create
  | Delete String
  | Typing Field String
  | ToggleAdd
  | ShowFilter
  | HideFilter
  | Idle

type Icon = Expand | Contract | Add

type Modifier = Ctrl | Alt | Meta | Shift

keyFromModifier : Modifier -> String
keyFromModifier modkey =
    case modkey of
        Ctrl  -> "ctrlKey"
        Alt   -> "altKey"
        Meta  -> "metaKey"
        Shift -> "shiftKey"
                             
    
type alias Keybind =
    { key: String
    , event: Event
    , modifier: Maybe Modifier
    }

type alias Keypress =
    { key: String
    , ctrl: Bool
    , alt: Bool
    , meta: Bool
    , shift: Bool
    }
    
empty : Model
empty = { bookmarks = Dict.empty, flux = { name = "", url = "", filter = "", showAdd = False, showFilter = False } }

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

-- for the filter box, specifically
focus : Model -> (Model, Cmd Event)
focus m = (m, Task.attempt (\_ -> Idle) (Dom.focus "filter"))
         
update : Event -> Model -> (Model, Cmd Event)
update event model =
    case event of
        Create             ->
            if not (String.isEmpty model.flux.name || String.isEmpty model.flux.url) then
                save { model | bookmarks = Dict.insert model.flux.name model.flux.url model.bookmarks }
            else void model
        Delete name        -> save { model | bookmarks = Dict.remove name model.bookmarks }
        Typing Name name   -> void  <| modifyFlux model (\f -> { f | name = name })
        Typing Url url     -> void  <| modifyFlux model (\f -> { f | url = url })
        Typing Filter str  -> void  <| modifyFlux model (\f -> { f | filter = str })
        ToggleAdd          -> void  <| modifyFlux model (\f -> { f | showAdd = not f.showAdd })
        ShowFilter         -> focus <| modifyFlux model (\f -> { f | showFilter = True })
        HideFilter         -> void  <| modifyFlux model (\f -> { f | showFilter = False, filter = "" })
        Idle               -> void model

modifyFlux : Model -> (Flux -> Flux) -> Model
modifyFlux m f = { m | flux = f m.flux }
                
view : Model -> Html Event
view model =
    Html.node "body" [ onKeyup [Keybind "/" ShowFilter (Just Alt), Keybind "Escape" HideFilter Nothing] ]
        <| search model.flux.showFilter ++ bookmarks model ++ [new model.flux.showAdd]

find : (a -> Bool) -> List a -> Maybe a
find pred list =
    case list of
        []       -> Nothing
        (x::xs) -> if pred x then Just x else find pred xs
              
onKeyup : List Keybind -> Html.Attribute Event
onKeyup keybinds = Html.on "keyup" (keyEvent keybinds)

findEventOrIdle : (Keybind -> Bool) -> List Keybind -> Event
findEventOrIdle pred = find pred >> Maybe.map .event >> Maybe.withDefault Idle

keyPressDecoder : Decode.Decoder Keypress
keyPressDecoder =
    let k = Decode.field "key" Decode.string
        c = Decode.field (keyFromModifier Ctrl) Decode.bool
        a = Decode.field (keyFromModifier Alt) Decode.bool
        m = Decode.field (keyFromModifier Meta) Decode.bool
        s = Decode.field (keyFromModifier Shift) Decode.bool
    in
    Decode.map5 Keypress k c a m s

hasModifier : Keypress -> Bool
hasModifier kp = kp.ctrl || kp.alt || kp.meta || kp.shift
        
matches : Keypress -> Keybind -> Bool
matches kp kb =
    case kb.modifier of
        Nothing    -> if not <| hasModifier kp  then kb.key == kp.key else False
        Just Ctrl  -> if not kp.ctrl then False else kb.key == kp.key
        Just Alt   -> if not kp.alt then False else kb.key == kp.key
        Just Meta  -> if not kp.meta then False else kb.key == kp.key
        Just Shift -> if not kp.shift then False else kb.key == kp.key                                          
        
keyEvent : List Keybind -> Decode.Decoder Event
keyEvent keybinds = keyPressDecoder
  |> Decode.andThen (\kp -> Decode.succeed <| findEventOrIdle (matches kp) keybinds)
     
bookmarks : Model -> List (Html Event)
bookmarks m = List.map bookmark <| filter m.flux.filter <|  Dict.toList <| m.bookmarks
                 
bookmark : Bookmark -> Html Event
bookmark (name, destination) = Html.div [Html.class "bookmark"]
  [ Html.a [Html.href destination] [Html.text name]
  , Html.button [Html.onClick (Delete name)] [Html.text ""]
  ]
    
toggle : Icon -> Html Event
toggle icn = Html.button [Html.id "toggle", Html.onClick ToggleAdd] [Html.text <| icon icn]

icon : Icon -> String
icon icn = case icn of
  Expand   -> ""
  Contract -> ""
  Add      -> ""
             
new : Bool -> Html Event
new visible = Html.span [] <|
  if visible then
      [ toggle Contract
      , Html.input  [ Html.type_ "text", Html.placeholder "Name", Html.onInput (Typing Name) ] []
      , Html.input  [ Html.type_ "text", Html.placeholder "URL", Html.onInput (Typing Url)  ] []
      , Html.button [ Html.id "create", Html.onClick Create ] [ Html.text <| icon Add ]
      ]
  else [toggle Expand]

search : Bool -> List (Html Event)
search visible = if not visible then []
  else [Html.input
            [ Html.type_ "text"
            , Html.id "filter"
            , Html.placeholder "Search"
            , Html.autofocus True
            , Html.onInput (Typing Filter)
            ] []
       ]
      
filter : String -> List Bookmark -> List Bookmark
filter str = List.filter (\(name, url) -> String.contains str name || String.contains str url)
      
port write : Encode.Value -> Cmd msg

encode : Model -> Encode.Value
encode m = Encode.dict identity Encode.string m.bookmarks
