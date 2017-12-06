import Html exposing (Html, text, a, div)
import Html.Attributes exposing (href)
import Navigation exposing (..)
import UrlParser as Url exposing (..)
import Http exposing (..)
import Json.Decode as JD exposing (decodeString, string, dict, field, list)  

type TokenData = TokenData (Maybe String) (Maybe String)

type alias Model = 
    { oauth: Maybe TokenData
    , auth: Maybe String
    , gazers: Maybe (List String)
    }

type Msg = UrlChange Navigation.Location | GetAuthorization (Result Error String) | GetGazers (Result Error (List String))

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
redirectParser = map TokenData (   s repoName 
                        <?> stringParam "code" 
                        <?> stringParam "state"
                        )

getGazersCmd : String -> Cmd Msg
getGazersCmd repo = 
   let
       url = "https://api.github.com/user/starred/" ++ repo 

       decodeGazers = list (field "login" JD.string)

       rqst = Http.get url decodeGazers
   in Http.send GetGazers rqst

requestAuthorization : String -> Cmd Msg
requestAuthorization _ =
    let url = "https://github.com"
    -- let url = "http://172.17.0.2:80/"
    in Http.send GetAuthorization <|
            Http.getString url

---requestAuthorization : String -> Cmd Msg
---requestAuthorization code =
---    let -- url = "https://github.com/login/oauth/access_token"
---        url = "http://172.17.0.2:8000/"
---
---        headers = [(Http.header "Accept" "application/json")]
---
---        content =    "client_id=" ++ clientId 
---                  ++ "&client_secret=" ++ clientSecret 
---                  ++ "&code=" ++ code
---
---        -- mimetype per: https://stackoverflow.com/questions/46677608/how-to-specify-body-media-type-for-elm-post-request
---        body = stringBody "text/plain;charset=utf-8" content
---
---        rq = request 
---                 { method = "GET"
---                 , headers = headers
---                 , url = url
---                 , body = body
---                 , expect = expectString 
---                 , timeout = Nothing
---                 , withCredentials = False
---                 }
---    in send GetAuthorization rq
---
init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let oauth = parsePath redirectParser location
    in case oauth of
        -- Just (TokenData (Just code) (Just state)) -> ({oauth = oauth, auth = Nothing, gazers=Nothing}, requestAuthorization code)
        Just (TokenData (Just code) (Just state)) -> ({oauth = oauth, auth = Nothing, gazers=Nothing}, getGazersCmd "dc25/solitaire")
        _ -> ({oauth = Nothing, auth = Nothing, gazers=Nothing}, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        GetAuthorization (Err _) -> ({model | auth = Nothing} , Cmd.none)
        GetAuthorization (Ok auth) -> ({model | auth = Just auth} , Cmd.none)
        GetGazers (Ok gazers) -> ({model | gazers = Just gazers} , Cmd.none)
        _ -> (model, Cmd.none)

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

