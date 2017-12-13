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
    , gazers: Maybe (List String)
    }

type Msg = UrlChange Navigation.Location | GetAuthorization (Result Error String) | GazersFetched (Result Error (List String))

clientId = "8256469ec6a458a2b111"
clientSecret = "b768bf69c0f44866330780a11d01cbf192ec0727"
repoName = "oauthElm"
redirectUri = "https://dc25.github.io/" ++ repoName
scope = "repo:user"
state = "w9erwlksjdf;kajdsf"

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




decodeGazers = list (field "login" JD.string)


getGazersCmd : String -> Cmd Msg
getGazersCmd reponame = 
   let
       url = "https://api.github.com/repos/" ++ reponame ++ "/stargazers"


       request = Http.get url decodeGazers
  in
    Http.send GazersFetched request

requestAuthorizationHack : String -> Cmd Msg
requestAuthorizationHack code =
    let -- url = "https://github.com/login/oauth/access_token"
        -- url = "http://172.17.0.2:8000/"
        url = "https://api.github.com/repos/" ++ "dc25/solitaire" ++ "/stargazers"

        -- headers = [(Http.header "Accept" "application/json")]
        -- headers = [(Http.header "Content-Type" "application/x-www-form-urlencoded")]
        headers = []

        -- content =    "client_id=" ++ clientId 
        --           ++ "&client_secret=" ++ clientSecret 
        --           ++ "&code=" ++ code

        -- mimetype per: https://stackoverflow.com/questions/46677608/how-to-specify-body-media-type-for-elm-post-request
        -- body = stringBody "text/plain;charset=utf-8" content
        body = emptyBody

        rq = request 
                 { method = "GET"
                 , headers = headers
                 , url = url
                 , body = body
                 , expect = expectJson decodeGazers
                 , timeout = Nothing
                 , withCredentials = False
                 }
    in send (GetAuthorization << Result.map String.concat) rq

requestAuthorization : String -> Cmd Msg
requestAuthorization code =
    let -- url = "https://github.com/login/oauth/access_token/"
        url = "http://127.0.0.1:8000/"
        -- url = "http://172.17.0.2:8000/"

        -- headers = [(Http.header "Accept" "application/json")]
        -- headers = [(Http.header "Content-Type" "application/x-www-form-urlencoded")]
        headers = []

        content =    "client_id=" ++ clientId 
                  ++ "&client_secret=" ++ clientSecret 
                  ++ "&code=" ++ code

        body = stringBody "application/x-www-form-urlencoded" content
        -- body = stringBody "" content

        rqp = Http.post url  body JD.string

        rq = request 
                 -- { method = "POST"
                 { method = "GET"
                 , headers = headers
                 , url = url
                 , body = body
                 , expect = expectStringResponse (\resp -> Ok (toString resp))
                 , timeout = Nothing
                 , withCredentials = False
                 }
    in send (GetAuthorization ) rq

init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let oauth = parsePath redirectParser location
    in case oauth of
        Just (TokenData (Just code) (Just state)) -> ({oauth = oauth, auth = Nothing, gazers=Nothing}, requestAuthorization code)
        _ -> ({oauth = Nothing, auth = Nothing, gazers=Nothing}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        GetAuthorization (res) -> ({model | auth = Just res} , Cmd.none)
        GazersFetched (Ok gazers) -> ({model | gazers = Just gazers} , Cmd.none)
        _ -> (model, Cmd.none)

view : Model -> Html Msg
view m = div []
             [ a [href githubOauthUri] [text "authK"]
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

