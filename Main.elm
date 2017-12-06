import Html exposing (Html, text, a, div)
import Html.Attributes exposing (href)
import Navigation exposing (..)
import UrlParser as Url exposing (..)

type Oauth = Oauth (Maybe String) (Maybe String)

type alias Model = 
    { oauth: Maybe Oauth
    }

type Msg = UrlChange Navigation.Location

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


oauthParser : Parser (Oauth -> a) a
oauthParser = map Oauth (   s repoName 
                        <?> stringParam "code" 
                        <?> stringParam "state"
                        )

init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let oauth = parsePath oauthParser location
    in case oauth of
        Just _ -> ({oauth = oauth}, Cmd.none)
        _ -> ({oauth = oauth}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update _ m = (m, Cmd.none)

view : Model -> Html Msg
view m = div []
             [ a [href githubOauthUri] [text "github auth"]
             , text (toString m)
             ] 

main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = \m -> Sub.none
        }

