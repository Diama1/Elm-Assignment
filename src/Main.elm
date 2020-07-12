module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (href, src, class, style, placeholder, value)
import Html.Events exposing (..)
import Http
import Time exposing (utc, toHour, toMinute, toSecond)
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Array exposing (Array)
import RemoteData exposing (RemoteData, WebData)

---- MODEL ----

type alias News =
    { url : String
    , name : String
    , language : String
    , premiered : String
    }

type alias Model =
    { hackerNews : WebData ( List News )
    , comment : List String
    , currentInput : String
    }


view : Model -> Html Msg 
view model = 
    div [ class "container" ]
        [ button [ onClick SendHttpRequest ]
            [ text "Get the data from server" ]
        , viewPostsOrError model
        ]

    
viewPostsOrError : Model -> Html Msg
viewPostsOrError model =
    case model.hackerNews of
        RemoteData.NotAsked ->
            text ""
        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]
        RemoteData.Success hackerNews ->
            viewPosts hackerNews
        RemoteData.Failure httpError ->
            viewError (buildErrorMessage httpError)

viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]

viewPosts : List News -> Html Msg
viewPosts posts =
    div [ ]
        [ ul []
            ( List.map viewNew posts )
        ]

viewNew : News -> Html Msg
viewNew model =
    li []
     [ a [ href model.url ]
         [ text model.name ]

         , h6 [] [ text " In ", text model.language, text  ", Released Date: ", text model.premiered
         , input [ style "margin-left" "12px", class "form-default", placeholder "Add a note", onEnter Add, onInput UpdateComment ] []
          ]
         
      ]
     
viewComment model =
    h3 [] [text  model.currentInput ]


onEnter msg = 
    let 
        isEnter key = 
            if key == 13 then
                Decode.succeed  msg
            else 
                Decode.fail "not Enter"

    in
        on "keydown" (Decode.andThen isEnter keyCode)

type Msg
    = SendHttpRequest
    | DataReceived (WebData (List News))
    | Add
    | UpdateComment String

newsDecoder : Decoder News
newsDecoder =
    Decode.succeed News
        |> required "url" string
        |> required "name" string
        |> required "language" string
        |> required "premiered" string

httpCommand : Cmd Msg
httpCommand =
    Http.get
        { url = "https://api.tvmaze.com/shows"
        , expect =
            list newsDecoder
                |> Http.expectJson (RemoteData.fromResult >> DataReceived)
        }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( { model | hackerNews = RemoteData.Loading }, httpCommand )

        DataReceived response ->
            ( { model | hackerNews = response }, Cmd.none )

        Add -> 
            let 
                newComment = 
                    List.append model.comment [ model.currentInput ]
            in
                ( { model | comment = newComment }, Cmd.none )
        
        UpdateComment inputValue ->
            ( { model | currentInput = inputValue }, Cmd.none )

buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message

init : () -> ( Model, Cmd Msg )
init _ =
    ( { hackerNews = RemoteData.NotAsked, comment = [""], currentInput= ""}, Cmd.none )

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
