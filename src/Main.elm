module Main exposing (..)

import AssocSet
import Browser
import Bytes.Encode
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Input as Input
import File
import File.Download
import Html exposing (Html)
import Json.Decode
import Json.Encode
import Platform.Cmd as Cmd
import String.Format
import Time
import Zip exposing (Zip)
import Zip.Entry


type DataFormat
    = Json
    | Html
    | Graphql
    | Xml


type Page
    = GetStarted
    | DataFormats
    | PrimaryDatabase
    | PickLoggingFramework
    | Finish


type AppType
    = MPA
    | SPA


type PrimaryDatabase
    = Postgres
    | Mysql
    | Sqlite


type LoggingFramework
    = Slf4jSimple
    | Logback
    | Log4j


type alias Model =
    { page : Page
    , dataformats : AssocSet.Set DataFormat
    , appType : AppType
    , primaryDatabase : Maybe PrimaryDatabase
    , loggingFramework : LoggingFramework
    , projectName : String
    , groupId : String
    }


type Msg
    = SelectDataFormat DataFormat
    | SelectPrimaryDatabase PrimaryDatabase
    | SelectLoggingFramework LoggingFramework
    | Next
    | Prev
    | Download
    | NoOp


type alias Flags =
    Json.Decode.Value


init : Json.Decode.Value -> ( Model, Cmd Msg )
init _ =
    ( { page = GetStarted
      , dataformats = AssocSet.empty
      , appType = MPA
      , primaryDatabase = Nothing
      , loggingFramework = Slf4jSimple
      , projectName = "Clojure QuickStart"
      , groupId = "org.example"
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


depToString : Dependency -> String
depToString { library, version } =
    library ++ " {:mvn/version \"" ++ version ++ "\"}"


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
:paths ["src"]
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
                |> List.map depToString
                |> String.join ("\n" ++ String.repeat 8 " ")
            )
        |> String.Format.namedValue "test_deps"
            (testDeps
                |> List.map depToString
                |> String.join ("\n" ++ String.repeat 30 " ")
            )
        |> String.Format.namedValue "dev_deps"
            (testDeps
                |> List.map depToString
                |> String.join ("\n" ++ String.repeat 29 " ")
            )


testsEdn : Model -> String
testsEdn _ =
    """#kaocha/v1 {:plugins [:kaocha.plugin/junit-xml]}
"""


mainClj : Model -> String
mainClj _ =
    """(ns main)

(defn -main [& _]
  (println "Hello"))
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
  (:require [routes :as routes]
            [system.server :as server]))

(defn start-system []
  (let [system {}
        system (assoc system ::server (server/start-server
                                       (partial #'routes/root-handler system)))]
    system))

(defn stop-system [system]
  (server/stop-server (::server system))
  nil)"""


routesClj : Model -> String
routesClj _ =
    """(ns routes)

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
  (if system
    (println "Already Started")
    (alter-var-root #'system (constantly (system/start-system)))))

(defn restart-system!
  []
  (stop-system!)
  (start-system!))

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


systemServerClj : Model -> String
systemServerClj _ =
    """(ns system.server
  (:require [ring.adapter.jetty9 :as jetty9]))

(set! *warn-on-reflection* true)

(defn start-server
  [handler]
  (jetty9/run-jetty handler
                    {:join? false
                     :host  "0.0.0.0"
                     :port  (or (some-> (System/getenv "PORT")
                                        (parse-long))
                                6000)}))

(defn stop-server
  [server]
  (jetty9/stop-server server))"""


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
    """default:
    just --list

# Run the program
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
        |> insertStringIntoZip { filename = "project/src/system/server.clj", contents = systemServerClj model }
        |> insertStringIntoZip { filename = "project/src/system.clj", contents = systemClj model }
        |> insertStringIntoZip { filename = "project/src/routes.clj", contents = routesClj model }


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
            , Element.width Element.fill
            ]
            (case model.page of
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
                                    , Element.text "Postgresql"
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
                        , checkbox
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
                                        [ Element.text "Simple logger that unconditionally logs anything INFO or higher to System.err" ]
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
                , Input.button
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


view : Model -> Html Msg
view model =
    Element.layout
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
