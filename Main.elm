import Html exposing (Html, text, a, div)
import Html.Attributes exposing (href)
import Navigation exposing (..)
import UrlParser as Url exposing (..)
import Http exposing (..)
import Result exposing (..)
import Json.Decode as JD exposing (decodeString, string, dict, field, list)  

type TokenData = TokenData (Maybe String) (Maybe String)

type alias Model = 
    { oauth: Maybe TokenData
    , auth: Maybe (Result Error String)
    }

type Msg =   UrlChange Navigation.Location 
           | GetAuthorization (Result Error String) 

clientId = "8256469ec6a458a2b111"
clientSecret = "b768bf69c0f44866330780a11d01cbf192ec0727"
repoName = "oauthElm"
redirectUri = "https://dc25.github.io/" ++ repoName
scope = "repo:user"
state = "w9erwlksjdf"

githubOauthUri = "https://github.com/login/oauth/authorize"
                     ++ "?client_id=" ++ clientId 
                     ++ "&redirect_uri=" ++ redirectUri 
                     ++ "&scope=" ++ scope 
                     ++ "&state=" ++ state


redirectParser : Parser (TokenData -> a) a
redirectParser = Url.map TokenData 
                     (   s repoName 
                     <?> stringParam "code" 
                     <?> stringParam "state"
                     )


requestAuthorization : String -> Cmd Msg
requestAuthorization code =
    let url = "https://github.com/login/oauth/access_token/"
        headers = [ (Http.header "Accept" "application/json") ]

        content =    "client_id=" ++ clientId 
                  ++ "&client_secret=" ++ clientSecret 
                  ++ "&code=" ++ code

        body = stringBody "application/x-www-form-urlencoded" content

        tokenDecoder = (field "access_token" JD.string)

        rq = request 
                 { method = "POST"
                 , headers = headers
                 , url = url
                 , body = body
                 -- , expect = expectStringResponse (\resp -> Ok (toString resp))
                 , expect = expectJson tokenDecoder
                 , timeout = Nothing
                 , withCredentials = False
                 }

        prq = post url body tokenDecoder

    in send GetAuthorization rq

init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let oauth = parsePath redirectParser location
    in case oauth of
        Just (TokenData (Just code) (Just _)) -> ({oauth = oauth, auth = Nothing}, requestAuthorization code)
        _ -> ({oauth = Nothing, auth = Nothing}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        GetAuthorization (res) -> ({model | auth = Just res} , Cmd.none)
        _ -> (model, Cmd.none)

view : Model -> Html Msg
view m = div []
             [ a [href githubOauthUri] [text "CAuth"]
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

