import StartApp.Simple exposing (start)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onKeyPress, on, targetValue)

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
  | UpdateTitle String
  | Filter FilterState


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Add ->
      { model
      | todos = model.todo :: model.todos
      , todo = newTodo
      }
    Complete todo ->
      model
    Delete todo ->
      model
    UpdateTitle str ->
      let todo = model.todo
          newTodo = { todo | title = str }
      in
        { model | todo = newTodo }
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


handleKeyPress : Int -> Action
handleKeyPress code =
  case code of
    13 ->
      Add
    _ ->
      NoOp


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
        , value model.todo.title
        , onKeyPress address handleKeyPress
        , on "input" targetValue (\str -> Signal.message address (UpdateTitle str))
        ] []
      ]
    , section [ class "main" ]
      [ ul [ class "todo-list" ]
        (List.map todoView model.todos)
      ]
    ]
  ]


newTodo : Todo
newTodo =
  { title = ""
  , completed = False
  , editing = False
  }


initialModel : Model
initialModel =
  { todos =
    [ { newTodo | title = "Something" }
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
