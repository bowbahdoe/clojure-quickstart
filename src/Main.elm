module Main exposing (..)

import AppUrl
import AssocSet
import Browser
import Browser.Navigation as Navigation
import Bytes.Encode
import Dict
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Input as Input
import File.Download
import Html exposing (Html)
import Json.Decode
import Platform.Cmd as Cmd
import String.Format
import Time
import Url
import Url.Builder
import Url.Parser exposing ((</>), (<?>))
import Zip exposing (Zip)
import Zip.Entry


type DataFormat
    = Json
    | Html
    | Graphql
    | Xml


dataformatToString : DataFormat -> String
dataformatToString dataformat =
    case dataformat of
        Json ->
            "json"

        Html ->
            "html"

        Graphql ->
            "graphql"

        Xml ->
            "xml"


type Page
    = GetStarted
    | DataFormats
    | PrimaryDatabase
    | PickLoggingFramework
    | PreferredEditor
    | VSCodeAdvice
    | IntelliJAdvice
    | OtherEditorAdvice
    | Finish


type AppType
    = MPA
    | SPA


type PrimaryDatabase
    = Postgres
    | Mysql
    | Sqlite


primaryDatabaseToString : PrimaryDatabase -> String
primaryDatabaseToString primaryDatabase =
    case primaryDatabase of
        Postgres ->
            "postgres"

        Mysql ->
            "mysql"

        Sqlite ->
            "sqlite"


primaryDatabaseFromString : String -> Result String PrimaryDatabase
primaryDatabaseFromString s =
    case s of
        "postgres" ->
            Ok Postgres

        "mysql" ->
            Ok Mysql

        "sqlite" ->
            Ok Sqlite

        _ ->
            Err s


type LoggingFramework
    = Slf4jSimple
    | Logback
    | Log4j


loggingFrameworkToString : LoggingFramework -> String
loggingFrameworkToString loggingFramework =
    case loggingFramework of
        Slf4jSimple ->
            "slf4j-simple"

        Logback ->
            "logback"

        Log4j ->
            "log4j"


loggingFrameworkFromString : String -> Result String LoggingFramework
loggingFrameworkFromString s =
    case s of
        "slf4j-simple" ->
            Ok Slf4jSimple

        "logback" ->
            Ok Logback

        "log4j" ->
            Ok Log4j

        _ ->
            Err s


type PreferredEditor
    = VSCode
    | IntelliJ
    | OtherEditor


preferredEditorToString : PreferredEditor -> String
preferredEditorToString preferredEditor =
    case preferredEditor of
        VSCode ->
            "vscode"

        IntelliJ ->
            "intellij"

        OtherEditor ->
            "other"


preferredEditorFromString : String -> Result String PreferredEditor
preferredEditorFromString s =
    case s of
        "vscode" ->
            Ok VSCode

        "intellij" ->
            Ok IntelliJ

        "other" ->
            Ok OtherEditor

        _ ->
            Err s


type alias Model =
    { navKey : Navigation.Key
    , url : AppUrl.AppUrl
    , page : Page
    , dataformats : AssocSet.Set DataFormat
    , appType : AppType
    , primaryDatabase : Maybe PrimaryDatabase
    , loggingFramework : LoggingFramework
    , projectName : String
    , preferredEditor : Maybe PreferredEditor
    }


hydrateModel : Model -> Model
hydrateModel model =
    let
        { url } =
            model
    in
    { model
        | primaryDatabase =
            Dict.get "primaryDatabase" url.queryParameters
                |> Maybe.andThen List.head
                |> Maybe.map primaryDatabaseFromString
                |> Maybe.map Result.toMaybe
                |> Maybe.withDefault model.primaryDatabase
        , preferredEditor =
            Dict.get "preferredEditor" url.queryParameters
                |> Maybe.andThen List.head
                |> Maybe.map preferredEditorFromString
                |> Maybe.andThen Result.toMaybe
                |> Maybe.map Just
                |> Maybe.withDefault model.preferredEditor
        , loggingFramework =
            Dict.get "loggingFramework" url.queryParameters
                |> Maybe.andThen List.head
                |> Maybe.map loggingFrameworkFromString
                |> Maybe.andThen Result.toMaybe
                |> Maybe.withDefault model.loggingFramework
    }


modelToQueryParams : Model -> AppUrl.QueryParameters
modelToQueryParams model =
    (Dict.empty
        |> (model.primaryDatabase
                |> Maybe.map primaryDatabaseToString
                |> Maybe.map (Dict.insert "primaryDatabase")
                |> Maybe.withDefault (\params -> params)
           )
        |> (model.preferredEditor
                |> Maybe.map preferredEditorToString
                |> Maybe.map (Dict.insert "preferredEditor")
                |> Maybe.withDefault (\params -> params)
           )
        |> Dict.insert "loggingFramework" (loggingFrameworkToString model.loggingFramework)
    )
        |> Dict.map (\_ v -> [ v ])
        |> Dict.insert "dataFormat" (model.dataformats |> AssocSet.map dataformatToString |> AssocSet.toList)


type Msg
    = SelectPreferredEditor PreferredEditor
    | SelectDataFormat DataFormat
    | SelectPrimaryDatabase PrimaryDatabase
    | SelectLoggingFramework LoggingFramework
    | Next
    | Prev
    | Download
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | NoOp


type alias Flags =
    Json.Decode.Value


init : Json.Decode.Value -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { navKey = key
      , url = AppUrl.fromUrl url
      , page = GetStarted
      , dataformats = AssocSet.empty
      , appType = MPA
      , primaryDatabase = Nothing
      , loggingFramework = Slf4jSimple
      , projectName = "Clojure QuickStart"
      , preferredEditor = Nothing
      }
    , Cmd.none
    )


type alias Dependency =
    { library : String, version : String }


dependencies : Model -> List Dependency
dependencies model =
    [ { library = "org.clojure/clojure", version = "1.11.0" }
    , { library = "info.sunng/ring-jetty9-adapter", version = "0.30.1" }
    , { library = "org.slf4j/slf4j-api", version = "2.0.9" }
    , { library = "metosin/reitit-ring", version = "0.7.0-alpha7" }
    ]
        ++ (case model.loggingFramework of
                Slf4jSimple ->
                    [ { library = "org.slf4j/slf4j-simple", version = "2.0.9" } ]

                Logback ->
                    [ { library = "ch.qos.logback/logback-classic", version = "1.4.11" } ]

                Log4j ->
                    [ { library = "org.apache.logging.log4j/log4j-core", version = "2.21.1" } ]
           )
        ++ (if AssocSet.member Json model.dataformats then
                [ { library = "cheshire/cheshire", version = "5.12.0" } ]

            else
                []
           )
        ++ (if AssocSet.member Html model.dataformats then
                [ { library = "hiccup/hiccup", version = "2.0.0-RC2" } ]

            else
                []
           )
        ++ (if model.primaryDatabase /= Nothing then
                [ { library = "com.github.seancorfield/next.jdbc", version = "1.3.894" }
                , { library = "com.github.seancorfield/honeysql", version = "2.4.1078" }
                ]

            else
                []
           )
        ++ (if model.primaryDatabase == Just Postgres then
                [ { library = "org.postgresql/postgresql", version = "42.6.0" }
                , { library = "com.zaxxer/HikariCP", version = "5.0.1" }
                ]

            else
                []
           )
        ++ (if model.primaryDatabase == Just Mysql then
                [ { library = "org.xerial/sqlite-jdbc", version = "3.43.2.1" }
                ]

            else
                []
           )
        ++ (if model.primaryDatabase == Just Sqlite then
                [ { library = "com.mysql/mysql-connector-j", version = "8.1.0" }
                , { library = "com.zaxxer/HikariCP", version = "5.0.1" }
                ]

            else
                []
           )


depsToString : List Dependency -> List String
depsToString deps =
    let
        biggestLibraryNameInList =
            deps
                |> List.map (\{ library } -> String.length library)
                |> List.maximum
                |> Maybe.withDefault 0
    in
    List.map (depToString biggestLibraryNameInList) deps


depToString : Int -> Dependency -> String
depToString biggestLibraryNameInList { library, version } =
    library
        ++ String.repeat
            (biggestLibraryNameInList - String.length library + 1)
            " "
        ++ "{:mvn/version \""
        ++ version
        ++ "\"}"


testDependencies : Model -> List Dependency
testDependencies _ =
    [ { library = "lambdaisland/kaocha", version = "1.77.1236" }
    , { library = "lambdaisland/kaocha-junit-xml", version = "1.17.101" }
    ]


depsEdn : Model -> String
depsEdn model =
    let
        deps =
            dependencies model

        testDeps =
            testDependencies model
    in
    """{:deps {{{deps}}}
:paths ["src" "resources"]
:aliases {:dev {:extra-deps {{{dev_deps}}}
                :extra-paths ["dev", "test"]}
          :test {:extra-deps {{{test_deps}}}
                 :extra-paths ["test"]}
          :lint {:deps {clj-kondo/clj-kondo {:mvn/version "2023.01.20"}}
                 :main-opts ["-m" "clj-kondo.main" "--lint" "."]}
          :format {:deps {cljfmt/cljfmt {:mvn/version "0.9.2"}}
                   :main-opts ["-m" "cljfmt.main" "fix" "test" "src" "dev"]}
          :check-format {:deps {cljfmt/cljfmt {:mvn/version "0.9.2"}}
                         :main-opts ["-m" "cljfmt.main" "check" "test" "src" "dev"]}
          :doc {:extra-deps {codox/codox {:mvn/version "0.10.8"}}
                :exec-fn    codox.main/generate-docs
                :exec-args  {:source-paths ["src"]}
                :language   :clojure}}}"""
        |> String.Format.namedValue "deps"
            (deps
                |> depsToString
                |> String.join ("\n" ++ String.repeat 8 " ")
            )
        |> String.Format.namedValue "test_deps"
            (testDeps
                |> depsToString
                |> String.join ("\n" ++ String.repeat 30 " ")
            )
        |> String.Format.namedValue "dev_deps"
            (testDeps
                |> depsToString
                |> String.join ("\n" ++ String.repeat 29 " ")
            )


testsEdn : Model -> String
testsEdn _ =
    """#kaocha/v1 {:plugins [:kaocha.plugin/junit-xml]}
"""


mainClj : Model -> String
mainClj _ =
    """(ns main
  (:require [system :as system]))

(defn -main [& _]
  (system/start-system))
"""


mainTestClj : Model -> String
mainTestClj _ =
    """(ns main-test
  (:require [clojure.test :as t]))

(t/deftest math
  (t/is (= 2 (+ 1 1))))
"""


systemClj : Model -> String
systemClj _ =
    """(ns system
  (:require [ring.adapter.jetty9 :as jetty9]
            [routes :as routes]))

(set! *warn-on-reflection* true)

(defn start-server
  [system]
  (jetty9/run-jetty (if (not= (System/getenv "ENVIRONMENT") "production")
                      (partial #'routes/root-handler system)
                      (routes/root-handler system))
                    {:join? false
                     :host  "0.0.0.0"
                     :port  (or (some-> (System/getenv "PORT")
                                        (parse-long))
                                6000)}))

(defn stop-server
  [server]
  (.stop server))

(defn start-system []
  (let [system {}
        system (assoc system ::server (start-server system))]
    system))

(defn stop-system [system]
  (stop-server (::server system))
  nil)"""


routesClj : Model -> String
routesClj _ =
    """(ns routes
  (:require [rei]))

(defn root-handler
  [_system request]
  {:status 200
   :body   (str "Hello, " (:remote-addr request))})"""


devUser : Model -> String
devUser _ =
    """(ns user
  (:require [system :as system]))

(defonce system nil)

(defn stop-system!
  []
  (when system
    (system/stop-system system))
  (alter-var-root #'system (constantly nil)))

(defn start-system!
  []
  (when-not system
    (alter-var-root #'system (constantly (system/start-system)))))

(defn restart-system!
  []
  (stop-system!)
  (start-system!))

(defn server
  []
  (::system/server system))

(comment
  (start-system!)

  (stop-system!)

  system

  (restart-system!))
"""


cljfmtEdn : Model -> String
cljfmtEdn _ =
    "{:sort-ns-references? true}\n"


cljkondoConfig : Model -> String
cljkondoConfig _ =
    "{:linters {:warn-on-reflection {:level :warning}}}"


resourcesGitkeep : Model -> String
resourcesGitkeep _ =
    ""


lintYml : Model -> String
lintYml _ =
    """name: Lints

on: [push]

jobs:
  linting:
    strategy:
      matrix:
        os: [ ubuntu-latest ]

    runs-on: ${{ matrix.os }}

    steps:
      - name: Checkout
        uses: actions/checkout@v3


      - name: Prepare java
        uses: actions/setup-java@v3
        with:
          distribution: 'adopt'
          java-version: '21'

      - name: Install clojure tools
        uses: DeLaGuardo/setup-clojure@10.2
        with:
          cli: 'latest'
          bb: 'latest'

      - name: Install just
        uses: extractions/setup-just@v1
    
      - name: Cache clojure dependencies
        uses: actions/cache@v3
        with:
          path: |
            ~/.m2/repository
            ~/.gitlibs
            ~/.deps.clj
          key: cljdeps-${{ hashFiles('deps.edn') }}
          restore-keys: cljdeps-${{ hashFiles('deps.edn') }}

      - name: Lint Code
        run: just lint"""


gitignore : Model -> String
gitignore _ =
    """.idea/
.idea
*.iml
junit.xml
target
.lsp
.cpcache
.DS_Store
"""


justfile : Model -> String
justfile _ =
    """# Lists all the available tasks
list:
    just --list

# Run the server
run:
    clojure -M -m main

# Run unit tests
test:
    clojure -A:test -M -m kaocha.runner

# Run lints
lint:
    clojure -M:lint

# Format all the files
format:
    clojure -M:format

# Check that all files are properly formatted
check-format:
    clojure -M:check-format

# Generate documentation for the codebase
doc:
    clojure -X:doc
"""


insertStringIntoZip : { filename : String, contents : String } -> Zip -> Zip
insertStringIntoZip { filename, contents } zip =
    Zip.insert
        (Bytes.Encode.string contents
            |> Bytes.Encode.encode
            |> Zip.Entry.store
                { path = filename
                , lastModified = ( Time.utc, Time.millisToPosix 0 )
                , comment = Nothing
                }
        )
        zip


makeZip : Model -> Zip
makeZip model =
    Zip.empty
        |> insertStringIntoZip { filename = "project/deps.edn", contents = depsEdn model }
        |> insertStringIntoZip { filename = "project/tests.edn", contents = testsEdn model }
        |> insertStringIntoZip { filename = "project/.cljfmt.edn", contents = cljfmtEdn model }
        |> insertStringIntoZip { filename = "project/.gitignore", contents = gitignore model }
        |> insertStringIntoZip { filename = "project/Justfile", contents = justfile model }
        |> insertStringIntoZip { filename = "project/src/main.clj", contents = mainClj model }
        |> insertStringIntoZip { filename = "project/test/main_test.clj", contents = mainTestClj model }
        |> insertStringIntoZip { filename = "project/.clj-kondo/config.edn", contents = cljkondoConfig model }
        |> insertStringIntoZip { filename = "project/dev/user.clj", contents = devUser model }
        |> insertStringIntoZip { filename = "project/src/system.clj", contents = systemClj model }
        |> insertStringIntoZip { filename = "project/src/routes.clj", contents = routesClj model }
        |> insertStringIntoZip { filename = "project/resources/.gitkeep", contents = resourcesGitkeep model }


checkbox : { activated : Bool, text : Element msg, onPress : msg, description : Element msg, singleSelect : Bool } -> Element msg
checkbox { activated, text, onPress, description, singleSelect } =
    Element.column []
        [ Element.el
            [ Element.paddingEach { top = 0, bottom = 32, left = 0, right = 0 } ]
            (Input.checkbox
                []
                { onChange =
                    \_ ->
                        onPress
                , icon =
                    \_ ->
                        Element.el [ Element.paddingEach { top = 0, bottom = 0, left = 0, right = 16 } ] <|
                            Element.el
                                ([ if activated then
                                    Element.Border.solid

                                   else
                                    Element.Border.dashed
                                 , Element.Border.width 2
                                 , Element.width (Element.px 30)
                                 , Element.height (Element.px 30)
                                 ]
                                    ++ (if activated then
                                            [ Element.Background.color (Element.rgb255 92 199 12) ]

                                        else
                                            []
                                       )
                                    ++ (if singleSelect then
                                            [ Element.Border.rounded 100 ]

                                        else
                                            []
                                       )
                                )
                                Element.none
                , checked = True
                , label =
                    Input.labelRight
                        [ Element.centerX
                        ]
                        text
                }
            )
        , description
        ]


question : String -> List (Element msg) -> Element msg
question q children =
    Element.column [ Element.width (Element.px 800), Element.height Element.fill ]
        [ Element.paragraph
            [ Font.size 36
            , Font.center
            ]
            [ Element.text q ]
        , Element.column
            [ Element.spacingXY 0 48
            , Element.paddingEach { left = 0, right = 0, bottom = 0, top = 128 }
            , Element.width Element.fill
            , Element.height Element.fill
            , Element.scrollbarY
            ]
            children
        ]


page : Model -> Element Msg
page model =
    Element.column
        [ Element.centerX
        , Element.paddingEach
            { top = 50
            , left = 0
            , right = 0
            , bottom = 0
            }
        , Element.height Element.fill
        ]
        [ Element.el
            [ Element.height (Element.fillPortion 9)
            , Element.width (Element.px 800)
            ]
            (case model.page of
                IntelliJAdvice ->
                    Element.text "GG using intellij"

                VSCodeAdvice ->
                    Element.text "GG using vscode"

                OtherEditorAdvice ->
                    Element.text "Good luck man"

                PreferredEditor ->
                    question "What editor are you most comfortable with"
                        [ checkbox
                            { onPress = SelectPreferredEditor VSCode
                            , activated = model.preferredEditor == Just VSCode
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.el [ Font.bold ] (Element.text "VSCode")
                                    , Element.el [ Font.color (Element.rgb255 92 199 12) ] (Element.text "(recommended)")
                                    ]
                            , description =
                                Element.el
                                    []
                                    (Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "Editor from Microsoft. The Clojure plugin is open source and actively maintained. VSCode won't do as well with Java interop as IntelliJ, but most programs don't need to do too much of that." ]
                                    )
                            , singleSelect = True
                            }
                        , checkbox
                            { onPress = SelectPreferredEditor IntelliJ
                            , activated = model.preferredEditor == Just IntelliJ
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.el [ Font.bold ] (Element.text "IntelliJ")
                                    ]
                            , description =
                                Element.textColumn
                                    [ Element.spacing 16 ]
                                    [ Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "Editor from JetBrains. The Clojure plugin for it is not open source, but there is a freely available non-commercial license and the cost for an individual license is well worth it if you prefer IntelliJ." ]
                                    ]
                            , singleSelect = True
                            }
                        , checkbox
                            { onPress = SelectPreferredEditor OtherEditor
                            , activated = model.preferredEditor == Just OtherEditor
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.el [ Font.bold ] (Element.text "Another editor")
                                    ]
                            , description =
                                Element.el
                                    []
                                    (Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "Emacs, Vim, Neovim, Notepad, etc. Depending on your editor, getting Clojure working can be an adventure. It has gotten a lot better ever since the advent of clojure-lsp, which if you use a less mainstream editor is a statement which should make sense." ]
                                    )
                            , singleSelect = True
                            }
                        ]

                Finish ->
                    Element.column [ Element.width Element.fill, Element.height Element.fill ]
                        [ Element.text "Summary"
                        , Input.button
                            [ Element.alignRight
                            , Element.paddingXY 16 8
                            , Element.Background.color (Element.rgb255 92 199 12)
                            , Element.Border.rounded 10
                            , Element.Border.color (Element.rgb255 92 199 12)
                            , Element.Border.width 2
                            , Font.color (Element.rgb255 255 255 255)
                            , Element.centerX
                            , Element.centerY
                            , Font.size 36
                            ]
                            { onPress = Just Download, label = Element.text "Download" }
                        ]

                GetStarted ->
                    Input.button
                        [ Element.alignRight
                        , Element.paddingXY 16 8
                        , Element.Background.color (Element.rgb255 92 199 12)
                        , Element.Border.rounded 10
                        , Element.Border.color (Element.rgb255 92 199 12)
                        , Element.Border.width 2
                        , Font.color (Element.rgb255 255 255 255)
                        , Element.centerX
                        , Element.centerY
                        , Font.size 36
                        ]
                        { onPress = Just Next, label = Element.text "Get Started" }

                PrimaryDatabase ->
                    question "Choose a primary database."
                        [ checkbox
                            { onPress = SelectPrimaryDatabase Postgres
                            , activated = model.primaryDatabase == Just Postgres
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.image
                                        [ Element.width (Element.px 30)
                                        , Element.height (Element.px 30)
                                        ]
                                        { src = "/540px-PostgreSQL_logo.3colors.svg.png", description = "JSON logo" }
                                    , Element.el [ Font.bold ] <| Element.text "Postgresql"
                                    , Element.el [ Font.color (Element.rgb255 92 199 12) ] (Element.text "(recommended)")
                                    ]
                            , description =
                                Element.textColumn
                                    [ Element.spacing 16 ]
                                    [ Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "A good default choice" ]
                                    , Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "Open source relational database." ]
                                    ]
                            , singleSelect = True
                            }
                        , checkbox
                            { onPress = SelectPrimaryDatabase Mysql
                            , activated = model.primaryDatabase == Just Mysql
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.image
                                        [ Element.width (Element.px 60)
                                        , Element.height (Element.px 30)
                                        ]
                                        { src = "/powered-by-mysql-167x86.png", description = "JSON logo" }
                                    , Element.text "MySQL"
                                    ]
                            , description =
                                Element.textColumn
                                    [ Element.spacing 16 ]
                                    [ Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "Default choice." ]
                                    ]
                            , singleSelect = True
                            }
                        , checkbox
                            { onPress = SelectPrimaryDatabase Sqlite
                            , activated = model.primaryDatabase == Just Sqlite
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.image
                                        [ Element.width (Element.px 60)
                                        , Element.height (Element.px 30)
                                        ]
                                        { src = "/SQLite370.svg.png", description = "JSON logo" }
                                    , Element.text "SQLite"
                                    ]
                            , description =
                                Element.textColumn
                                    [ Element.spacing 16 ]
                                    [ Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "Small, fast, self-contained, high-reliability, full-featured, SQL database engine." ]
                                    , Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "To use this, your server will have to be run on a single machine." ]
                                    ]
                            , singleSelect = True
                            }
                        ]

                DataFormats ->
                    question "Choose the data formats you want to handle"
                        [ checkbox
                            { onPress = SelectDataFormat Json
                            , activated = AssocSet.member Json model.dataformats
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.image
                                        [ Element.width (Element.px 30)
                                        , Element.height (Element.px 30)
                                        ]
                                        { src = "/JSON_vector_logo.svg.png", description = "JSON logo" }
                                    , Element.text "JSON"
                                    ]
                            , description =
                                Element.textColumn
                                    [ Element.spacing 16 ]
                                    [ Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "Useful for building programmatic APIs." ]
                                    , Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "This is probably what you want if you are planning on using the server along with a JavaScript rendered frontend using something like react." ]
                                    ]
                            , singleSelect = False
                            }
                        , checkbox
                            { onPress = SelectDataFormat Html
                            , activated = AssocSet.member Html model.dataformats
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.image [ Element.width (Element.px 30), Element.height (Element.px 30) ]
                                        { src = "/HTML5_Badge.svg", description = "HTML logo" }
                                    , Element.text "HTML"
                                    ]
                            , description =
                                Element.el
                                    []
                                    (Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "Useful for rendering data to a web browser." ]
                                    )
                            , singleSelect = False
                            }
                        , checkbox
                            { onPress = SelectDataFormat Xml
                            , activated = AssocSet.member Xml model.dataformats
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.image
                                        [ Element.width (Element.px 60)
                                        , Element.height (Element.px 30)
                                        ]
                                        { src = "/Extensible_Markup_Language_(XML)_logo.svg.png", description = "XML logo" }
                                    , Element.text "XML"
                                    ]
                            , description =
                                Element.el
                                    []
                                    (Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "Useful when working with older systems" ]
                                    )
                            , singleSelect = False
                            }
                        ]

                PickLoggingFramework ->
                    question "Choose a logging framework"
                        [ checkbox
                            { onPress = SelectLoggingFramework Slf4jSimple
                            , activated = model.loggingFramework == Slf4jSimple
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.image
                                        [ Element.width (Element.px 60)
                                        , Element.height (Element.px 30)
                                        ]
                                        { src = "/slf4j-logo.jpg", description = "SLF4J logo" }
                                    , Element.text "slf4j-simple"
                                    ]
                            , description =
                                Element.el
                                    []
                                    (Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "Simple logger that unconditionally logs anything INFO or higher."
                                        , Element.text "If you've never thought about logging too much before, this is a good choice. It is pretty easy to change later if you want to."
                                        ]
                                    )
                            , singleSelect = True
                            }
                        , checkbox
                            { onPress = SelectLoggingFramework Logback
                            , activated = model.loggingFramework == Logback
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.image
                                        [ Element.width (Element.px 60)
                                        , Element.height (Element.px 30)
                                        ]
                                        { src = "/Logback_72dpi.png", description = "Logback logo" }
                                    , Element.text "Logback"
                                    ]
                            , description =
                                Element.el
                                    []
                                    (Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "A reliable, generic, fast and flexible logging framework." ]
                                    )
                            , singleSelect = True
                            }
                        , checkbox
                            { onPress = SelectLoggingFramework Log4j
                            , activated = model.loggingFramework == Log4j
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.image
                                        [ Element.width (Element.px 60)
                                        , Element.height (Element.px 30)
                                        ]
                                        { src = "/Apache_Log4j_Logo.png", description = "Log4j logo" }
                                    , Element.text "Log4j2"
                                    ]
                            , description =
                                Element.el
                                    []
                                    (Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "Useful when working with older systems" ]
                                    )
                            , singleSelect = True
                            }
                        ]
            )
        , if model.page /= GetStarted && model.page /= Finish then
            Element.row [ Element.width Element.fill, Element.height (Element.fillPortion 1), Element.centerY ]
                [ Input.button
                    [ Element.alignLeft
                    , Element.paddingXY 16 8
                    , Element.Background.color
                        (Element.rgb255 36 82 161)
                    , Element.Border.rounded 10
                    , Element.Border.color
                        (Element.rgb255 36 82 161)
                    , Element.Border.width 2
                    , Font.color (Element.rgb255 255 255 255)
                    ]
                    { onPress = Just Prev, label = Element.text "Prev" }
                , if model.page == PreferredEditor && model.preferredEditor == Nothing then
                    Element.none

                  else
                    Input.button
                        [ Element.alignRight
                        , Element.paddingXY 16 8
                        , Element.Background.color (Element.rgb255 92 199 12)
                        , Element.Border.rounded 10
                        , Element.Border.color (Element.rgb255 92 199 12)
                        , Element.Border.width 2
                        , Font.color (Element.rgb255 255 255 255)
                        ]
                        { onPress = Just Next, label = Element.text "Next" }
                ]

          else
            Element.el
                [ Element.height
                    (Element.fillPortion 1)
                ]
                Element.none
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Clojure Quickstart"
    , body =
        [ Element.layout
            [ Font.family
                [ Font.typeface "Open Sans"
                , Font.sansSerif
                ]
            ]
            (Element.column
                [ Element.width Element.fill
                , Element.height Element.fill

                -- linear-gradient(to right,#0f2242,#2452a1)
                ]
                [ Element.row
                    [ Element.width Element.fill
                    , Element.height (Element.fillPortion 1)
                    , Element.centerX
                    , Element.Background.gradient
                        { angle = 1.57079632679
                        , steps =
                            [ Element.rgb255 15 34 66
                            , Element.rgb255 36 82 161
                            ]
                        }
                    , Element.spacing 16
                    , Element.paddingXY 16 0
                    ]
                    [ Element.image [ Element.width (Element.px 60) ]
                        { src = "/clojure-logo-120b.png", description = "Clojure Logo" }
                    , Element.el [ Font.size 36, Font.color (Element.rgb255 255 255 255) ] (Element.text "Clojure Quickstart")
                    ]
                , Element.column [ Element.width Element.fill, Element.height (Element.fillPortion 9) ]
                    [ page model
                    ]
                ]
            )
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPreferredEditor preferredEditor ->
            ( { model | preferredEditor = Just preferredEditor }, Cmd.none )

        SelectDataFormat dataFormat ->
            ( { model
                | dataformats =
                    if AssocSet.member dataFormat model.dataformats then
                        AssocSet.remove dataFormat model.dataformats

                    else
                        AssocSet.insert dataFormat model.dataformats
              }
            , Cmd.none
            )

        SelectPrimaryDatabase primaryDatabase ->
            ( { model
                | primaryDatabase =
                    if model.primaryDatabase == Just primaryDatabase then
                        Nothing

                    else
                        Just primaryDatabase
              }
            , Cmd.none
            )

        SelectLoggingFramework loggingFramework ->
            ( { model
                | loggingFramework = loggingFramework
              }
            , Cmd.none
            )

        Next ->
            ( { model
                | page =
                    case model.page of
                        GetStarted ->
                            PreferredEditor

                        PreferredEditor ->
                            case model.preferredEditor of
                                Just IntelliJ ->
                                    IntelliJAdvice

                                Just VSCode ->
                                    VSCodeAdvice

                                _ ->
                                    OtherEditorAdvice

                        IntelliJAdvice ->
                            DataFormats

                        VSCodeAdvice ->
                            DataFormats

                        OtherEditorAdvice ->
                            DataFormats

                        DataFormats ->
                            PrimaryDatabase

                        PrimaryDatabase ->
                            PickLoggingFramework

                        PickLoggingFramework ->
                            Finish

                        Finish ->
                            Finish
              }
            , Cmd.none
            )

        Prev ->
            ( { model
                | page =
                    case model.page of
                        Finish ->
                            PickLoggingFramework

                        PickLoggingFramework ->
                            PrimaryDatabase

                        PrimaryDatabase ->
                            DataFormats

                        DataFormats ->
                            case model.preferredEditor of
                                Just IntelliJ ->
                                    IntelliJAdvice

                                Just VSCode ->
                                    VSCodeAdvice

                                _ ->
                                    OtherEditorAdvice

                        IntelliJAdvice ->
                            PreferredEditor

                        VSCodeAdvice ->
                            PreferredEditor

                        OtherEditorAdvice ->
                            PreferredEditor

                        PreferredEditor ->
                            GetStarted

                        GetStarted ->
                            GetStarted
              }
            , Cmd.none
            )

        Download ->
            ( model
            , makeZip model
                |> Zip.toBytes
                |> File.Download.bytes "archive.zip" "application/zip"
            )

        UrlRequest request ->
            case request of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.navKey (Url.toString url) )

                Browser.External url ->
                    ( model, Navigation.load url )

        UrlChange url ->
            ( { model | url = AppUrl.fromUrl url }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


updateWrapped : Msg -> Model -> ( Model, Cmd Msg )
updateWrapped msg model =
    case msg of
        UrlRequest _ ->
            update msg model

        NoOp ->
            update msg model

        UrlChange _ ->
            update msg model

        _ ->
            let
                ( model2, cmd ) =
                    update msg model

                url =
                    model2.url

                newUrl =
                    { url | queryParameters = modelToQueryParams model }

                model3 =
                    { model2 | url = newUrl }
            in
            if newUrl /= model.url then
                ( hydrateModel model3, Cmd.batch [ cmd, Navigation.pushUrl model.navKey (AppUrl.toString newUrl) ] )

            else
                ( model3, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest =
    UrlRequest


onUrlChange : Url.Url -> Msg
onUrlChange =
    UrlChange


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = updateWrapped
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }
