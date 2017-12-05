import Html exposing (Html, text, program, button, a, div)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href)
import Navigation exposing (..)
import UrlParser as Url exposing (..)

type alias Oauth = {code: String, state: String, location: Navigation.Location}
-- type alias Oauth = {code: String, state: String}

type alias Model = 
    { value:Int
    , oauth: Maybe Oauth
    }

type Msg =   NoOp
           | UrlChange Navigation.Location

clientId = "8256469ec6a458a2b111"
repoName = "oauthElm"
redirectUri = "https://dc25.github.io/" ++ repoName
scope = "repo:user"
state = "w9erwlksjdf;kajdsf"

githubOauthUri = "https://github.com/login/oauth/authorize"
                     ++ "?client_id=" ++ clientId 
                     ++ "&redirect_uri=" ++ redirectUri 
                     ++ "&scope=" ++ scope 
                     ++ "&state=" ++ state

type Route = OauthCode (Maybe String) (Maybe String)
route : Parser (Route -> a) a
route = map OauthCode (s repoName <?> stringParam "code" <?> stringParam "state")


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let oauth = parsePath route location
    in case oauth of
        Just (OauthCode (Just c) (Just s)) -> ({value=0, oauth = Just {code=c,state=s,location=location}}, Cmd.none)
        _ -> ({value=0, oauth=Just {code="", state="", location=location}}, Cmd.none)

updateOauth : Model -> Navigation.Location -> (Model, Cmd Msg)
updateOauth model location =
    let oauth = parsePath route location
    in case oauth of
        Just (OauthCode (Just c) (Just s)) -> ({model | oauth = Just {code=c,state=s,location=location}}, Cmd.none)
        _ -> ({model|oauth=Just {code ="", state="", location=location}}, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update c m = 
    case c of
        UrlChange location -> updateOauth m location
        NoOp -> ({m | value=m.value+1}, Cmd.none)

view : Model -> Html Msg
view m = div []
             (  [ a [href githubOauthUri] [text "github auth"]
                , button [onClick NoOp] [text (toString m)]
                ] 
             ++ [
                   text "HELLOOO??"
                ]
             )

main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = \m -> Sub.none
        }

