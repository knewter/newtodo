import StartApp.Simple exposing (start)
import Html exposing (..)
import Html.Attributes exposing (..)

type alias Todo =
  { title : String
  , completed : Bool
  , editing : Bool
  }


type FilterState = All | Active | Completed


type alias Model =
  { todos: List Todo
  , todo: Todo
  , filterState: FilterState
  }


type Action
  = NoOp
  | Add
  | Complete Todo
  | Delete Todo
  | Filter FilterState


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Add ->
      model
    Complete todo ->
      model
    Delete todo ->
      model
    Filter filterState ->
      model


css : String -> Html
css path =
  node "link" [ rel "stylesheet", href path ] []


todoView : Todo -> Html
todoView todo =
  li [ classList [("completed", todo.completed)] ]
    [ div [ class "view" ]
      [ input [ class "toggle", type' "checkbox", checked todo.completed ] []
      , label [] [ text todo.title ]
      , button [ class "destroy" ] []
      ]
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  div []
  [ css "style.css"
  , section [ class "todoapp" ]
    [ header [ class "header" ]
      [ h1 [] [ text "todos" ]
      , input
        [ class "new-todo"
        , placeholder "What needs to be done?"
        , autofocus True
        ] []
      ]
    , section [ class "main" ]
      [ ul [ class "todo-list" ]
        (List.map todoView model.todos)
      ]
    ]
  ]


initialModel : Model
initialModel =
  { todos =
    [ { title = "First real todo"
      , completed = True
      , editing = False
      }
    ]
  , todo = { title = ""
          , completed = False
          , editing = False
          }
  , filterState = All
  }


main =
  StartApp.Simple.start
    { model = initialModel
    , update = update
    , view = view
    }
