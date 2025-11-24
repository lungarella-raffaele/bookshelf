module Main exposing (Model, Msg(..), init, main, update, view)

import Browser exposing (Document)
import Browser.Dom
import Browser.Events
import Html exposing (Html, a, br, button, div, form, h1, h3, i, input, label, li, option, p, select, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, required, style, target, value)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Json.Decode
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { books : List Book
    , book : InputBook
    , openDialog : Maybe Dialog
    }


type alias InputBook =
    { title : String
    , author : String
    , categories : List Category
    , totalPages : String
    , currentPage : String
    }



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Bookshelf"
    , body =
        [ div [ class "text-fg py-20 min-h-screen flex justify-between flex-col container mx-auto px-4 sm:px-6 md:px-8 max-w-4xl" ]
            [ viewBody model
            , case model.openDialog of
                Just Form ->
                    viewFormDialog model

                Just Help ->
                    viewHelpDialog

                Nothing ->
                    text ""
            , viewFooter
            ]
        ]
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { books =
            [ { title = "Solaris", author = Just "Stanisław Lem", categories = [ SciFi ], currentPage = Just 20, totalPages = Just 160 }
            , { title = "Il Tallone da Killer"
              , author = Nothing
              , categories =
                    [ Horror
                    , Fiction
                    , Thriller
                    , Fantasy
                    , SciFi
                    , Biography
                    , History
                    , Romance
                    ]
              , currentPage = Nothing
              , totalPages = Nothing
              }
            , { title = "The Mythical Man-Month", author = Nothing, categories = [], currentPage = Nothing, totalPages = Nothing }
            , { title = "Ucronia", author = Just "Emmanuel Carrére", categories = [], currentPage = Nothing, totalPages = Nothing }
            , { title = "On Killing", author = Nothing, categories = [], currentPage = Nothing, totalPages = Nothing }
            ]
      , book = { title = "", author = "", categories = [], totalPages = "", currentPage = "" }
      , openDialog = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = AddBook
    | UpdateName String
    | UpdateAuthor String
    | UpdateTotalPages String
    | UpdateCompletion String
    | ShowDialog Dialog
    | CloseDialog
    | Noop
    | ToggleCategory Category


type Dialog
    = Form
    | Help


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddBook ->
            -- Attempt to parse the optional numeric fields from the input Strings
            let
                parsedPages : Maybe Int
                parsedPages =
                    String.toInt model.book.totalPages

                parsedCompletion : Maybe Int
                parsedCompletion =
                    String.toInt model.book.currentPage

                -- Validate required fields (title must not be empty, author must not be empty)
                isValid =
                    model.book.title /= "" && model.book.author /= ""
            in
            if isValid then
                -- The author field in InputBook is a String. We wrap it in Just for the Book type.
                let
                    newBook : Book
                    newBook =
                        { title = model.book.title
                        , author = Just model.book.author -- String -> Maybe String
                        , categories = model.book.categories
                        , totalPages = parsedPages -- String -> Maybe Int
                        , currentPage = parsedCompletion -- String -> Maybe Int
                        }

                    -- Reset the input form fields
                    resetInput : InputBook
                    resetInput =
                        { title = ""
                        , author = ""
                        , categories = []
                        , totalPages = ""
                        , currentPage = ""
                        }
                in
                ( { model
                    | books = newBook :: model.books
                    , book = resetInput
                    , openDialog = Nothing
                  }
                , Cmd.none
                )

            else
                -- Validation failed (e.g., title or author was empty)
                ( model, Cmd.none )

        UpdateAuthor author ->
            let
                currentBook =
                    model.book

                updatedBook =
                    { currentBook | author = author }
            in
            ( { model | book = updatedBook }
            , Cmd.none
            )

        UpdateName name ->
            let
                currentBook =
                    model.book

                updatedBook =
                    { currentBook | title = name }
            in
            ( { model | book = updatedBook }
            , Cmd.none
            )

        UpdateTotalPages total ->
            let
                currentBook =
                    model.book

                updatedBook =
                    { currentBook | totalPages = total }
            in
            ( { model | book = updatedBook }
            , Cmd.none
            )

        UpdateCompletion curr ->
            let
                currentBook =
                    model.book

                updatedBook =
                    { currentBook | currentPage = curr }
            in
            ( { model | book = updatedBook }
            , Cmd.none
            )

        CloseDialog ->
            ( { model | openDialog = Nothing }
            , Cmd.none
            )

        ShowDialog dialog ->
            case dialog of
                Form ->
                    ( { model | openDialog = Just Form }
                    , Task.attempt (\_ -> Noop) (Browser.Dom.focus "book-title-input")
                    )

                Help ->
                    ( { model | openDialog = Just Help }
                    , Cmd.none
                    )

        ToggleCategory cat ->
            let
                newCategories =
                    if List.member cat model.book.categories then
                        List.filter (\c -> c /= cat) model.book.categories

                    else
                        cat :: model.book.categories

                currentBook =
                    model.book

                updatedBook =
                    { currentBook | categories = newCategories }
            in
            ( { model | book = updatedBook }, Cmd.none )

        Noop ->
            ( model
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown keyDecoder


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\key ->
                case key of
                    "Escape" ->
                        -- Escape always works, even in inputs
                        Json.Decode.succeed CloseDialog

                    _ ->
                        -- For other keys, check if input is focused
                        isInputFocused
                            |> Json.Decode.andThen
                                (\inputFocused ->
                                    if inputFocused then
                                        Json.Decode.fail "Input is focused"

                                    else
                                        case key of
                                            "?" ->
                                                Json.Decode.succeed (ShowDialog Help)

                                            "q" ->
                                                Json.Decode.succeed CloseDialog

                                            "n" ->
                                                Json.Decode.succeed (ShowDialog Form)

                                            _ ->
                                                Json.Decode.fail "Not a shortcut key"
                                )
            )


isInputFocused : Json.Decode.Decoder Bool
isInputFocused =
    Json.Decode.at [ "target", "tagName" ] Json.Decode.string
        |> Json.Decode.andThen
            (\tagName ->
                case String.toUpper tagName of
                    "INPUT" ->
                        Json.Decode.succeed True

                    "TEXTAREA" ->
                        Json.Decode.succeed True

                    "SELECT" ->
                        Json.Decode.succeed True

                    _ ->
                        -- Check for contentEditable
                        Json.Decode.oneOf
                            [ Json.Decode.at [ "target", "isContentEditable" ] Json.Decode.bool
                            , Json.Decode.succeed False
                            ]
            )



-- LAYOUT


viewBody :
    Model
    -> Html Msg
viewBody model =
    div []
        [ div [ class "flex flex-row justify-between mb-4 items-center" ]
            [ h1 [] [ text "Bookshelf" ]
            , button [ onClick (ShowDialog Form), class "btn-primary rounded-lg px-2 py-1 m-0" ] [ span [ class "underline" ] [ text "n" ], text "ew" ]
            ]
        , ul [ class "flex flex-col gap-4" ]
            (model.books
                |> List.map (\book -> viewBook book)
            )
        ]


viewFooter : Html Msg
viewFooter =
    div [ class "flex justify-between items-center text-fg-muted mt-5" ]
        [ text "@2025 Open Source Saturday Milan"
        , a [ href "https://github.com/lungarella-raffaele/bookshelf", target "_blank", class "text-2xl" ]
            [ i [ attribute "data-lucide" "github" ] []
            ]
        ]



-- BOOKS


type alias Book =
    { title : String
    , author : Maybe String
    , categories : List Category
    , totalPages : Maybe Int
    , currentPage : Maybe Int
    }


viewBook : Book -> Html Msg
viewBook book =
    li [ class "p-2 rounded-lg border-fg-muted bg-bg-alt border" ]
        [ h3 [ class "font-bold" ]
            [ text book.title ]
        , case book.author of
            Just author ->
                p []
                    [ span [ class "text-fg-muted" ] [ text "by " ]
                    , br [] []
                    , text author
                    ]

            Nothing ->
                text ""
        , viewCategories book.categories
        , case ( book.currentPage, book.totalPages ) of
            ( Just compl, Just total ) ->
                div [ class "mt-4" ]
                    [ div [ class "text-fg-muted mb-1" ] [ text (String.fromInt compl ++ " of " ++ String.fromInt total ++ " pages read") ]
                    , viewProgress total compl
                    ]

            ( _, _ ) ->
                text ""
        ]


viewProgress : Int -> Int -> Html Msg
viewProgress total value =
    div
        [ class "w-full bg-bg rounded-full h-2" ]
        [ div
            [ class "bg-accent h-2 rounded-full"
            , style "width" (String.fromInt ((value * 100) // total) ++ "%")
            ]
            []
        ]



-- CATEGORY


type Category
    = Horror
    | Fiction
    | Thriller
    | Fantasy
    | SciFi
    | Biography
    | History
    | Romance


availableCategories : List Category
availableCategories =
    [ Horror, Fiction, Thriller, Fantasy, SciFi, Biography, History, Romance ]


categoryToString : Category -> String
categoryToString category =
    case category of
        SciFi ->
            "Sci-fi"

        Thriller ->
            "Thriller"

        Horror ->
            "Horror"

        Fiction ->
            "Fiction"

        Fantasy ->
            "Fantasy"

        Biography ->
            "Biography"

        History ->
            "History"

        Romance ->
            "Romance"


viewCategories : List Category -> Html msg
viewCategories categories =
    case List.length categories of
        0 ->
            text ""

        _ ->
            ul [ attribute "role" "list", class "flex flex-row gap-2 items-center mt-4" ]
                (List.map viewCategory categories)


viewCategory : Category -> Html msg
viewCategory category =
    let
        bgColor =
            case category of
                SciFi ->
                    "bg-sky-700"

                Thriller ->
                    "bg-yellow-800"

                Horror ->
                    "bg-zinc-500"

                Fiction ->
                    "bg-cyan-800"

                Fantasy ->
                    "bg-green-700"

                Biography ->
                    "bg-amber-500"

                History ->
                    "bg-lime-800"

                Romance ->
                    "bg-red-700"
    in
    li []
        [ span [ class ("px-2 py-1 rounded-md text-white " ++ bgColor) ] [ text (categoryToString category) ]
        ]



-- DIALOG


type alias DialogModel =
    { id : String
    , title : String
    , content : List (Html Msg)
    , width : DialogWidth
    }


type DialogWidth
    = Small
    | Medium
    | Large


viewDialog : DialogModel -> Html Msg
viewDialog dialog =
    div [ class "dialog-backdrop", onClick CloseDialog ]
        [ div
            [ class ("dialog-content bg-bg-alt border border-fg-muted rounded-lg shadow " ++ dialogWidthToClass dialog.width)
            , stopPropagationOn "click"
                (Json.Decode.succeed ( Noop, True ))
            ]
            [ div [ class "flex justify-between items-start" ]
                [ h3 [ class "mb-4" ] [ text dialog.title ]
                , button [ onClick CloseDialog, class "text-fg-muted hover:text-accent p-2" ]
                    [ span [ class "underline" ] [ text "q" ], text "lose" ]
                ]
            , div [] dialog.content
            ]
        ]


viewFormDialog : Model -> Html Msg
viewFormDialog model =
    viewDialog
        { id = "form-dialog"
        , title = "Add Book"
        , width = Medium
        , content =
            [ form []
                [ div [ class "flex flex-row justify-between gap-2" ]
                    [ div [ class "w-full" ]
                        [ label [ class "font-bold" ] [ text "Title", span [ class "text-destructive" ] [ text "*" ] ]
                        , input [ id "book-title-input", required True, placeholder "Solaris", value model.book.title, onInput UpdateName, class "mb-4" ] []
                        ]
                    , div [ class "w-full" ]
                        [ label [ class "font-bold" ] [ text "Author" ]
                        , input [ placeholder "Stanisław Lem", value model.book.author, onInput UpdateAuthor ] []
                        ]
                    ]
                , div [ class "flex flex-row justify-between gap-2" ]
                    [ div [ class "w-full" ]
                        [ label [ class "font-bold" ] [ text "Current page" ]
                        , input [ placeholder "42", value model.book.currentPage, onInput UpdateCompletion ] []
                        ]
                    , div [ class "w-full" ]
                        [ label [ class "font-bold" ] [ text "Total" ]
                        , input [ placeholder "255", value model.book.totalPages, onInput UpdateTotalPages ] []
                        ]
                    ]
                , viewSelectCategories availableCategories model.book
                , div [ class "flex flex-row-reverse mt-6" ]
                    [ button [ class "btn-primary px-3 py-2 rounded-lg border", onClick AddBook ] [ text "Aggiungi" ]
                    ]
                ]
            ]
        }


viewSelectCategories : List Category -> InputBook -> Html Msg
viewSelectCategories options input =
    div [ class "mt-4" ]
        [ label [ class "font-bold" ] [ text "Categories" ]
        , div [ class "flex flex-row gap-2" ]
            (List.map (\cat -> viewCategoryButton cat input) options)
        ]


viewCategoryButton : Category -> InputBook -> Html Msg
viewCategoryButton cat input =
    let
        isSelected =
            List.member cat input.categories
    in
    button
        [ onClick (ToggleCategory cat)
        , classList
            [ ( "p-1", True )
            , ( "text-accent underline", isSelected )
            ]
        ]
        [ text (categoryToString cat) ]


dialogWidthToClass : DialogWidth -> String
dialogWidthToClass width =
    case width of
        Small ->
            "min-w-sm"

        Medium ->
            "sm:min-w-2xl rounded-none sm:rounded-lg"

        Large ->
            "min-w-5xl"


viewShortcutHelp : { key : String, description : String } -> Html msg
viewShortcutHelp shortcut =
    div [ class "flex gap-2 items-center mb-2" ]
        [ span [ class "text-accent font-mono bg-bg px-2 py-1 rounded" ] [ text shortcut.key ]
        , span [] [ text shortcut.description ]
        ]


viewHelpDialog : Html Msg
viewHelpDialog =
    viewDialog
        { id = "shortcuts-dialog"
        , title = "Keyboard Shortcuts"
        , width = Large
        , content =
            [ div []
                (List.map viewShortcutHelp
                    [ { key = "?", description = "Open help dialog" }
                    , { key = "esc/q", description = "Close dialog" }
                    , { key = "n", description = "Open new book dialog" }
                    ]
                )
            ]
        }
