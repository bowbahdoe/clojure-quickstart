module Main exposing (..)

import AppUrl
import AssocSet
import BlurHash
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
import Task
import Time
import Url
import Url.Parser exposing ((</>), (<?>))
import Zip exposing (Zip)
import Zip.Entry


type alias Image =
    { path : String, description : String, blurhash : String }


images =
    { html = { path = "/HTML5_Badge.svg", description = "HTML Logo", blurhash = "LFOw{WNH01-U}YWr+vni8ws:uONG" }
    }


imageToElement : { width : Int, height : Int } -> Image -> Element msg
imageToElement { width, height } image =
    Element.image
        [ Element.width (Element.px width)
        , Element.height (Element.px height)
        , Element.Background.color (Element.rgb255 255 255 255)
        , Element.behindContent
            (Element.image
                [ Element.width (Element.px width)
                , Element.height (Element.px height)
                ]
                { src =
                    BlurHash.toUri
                        { width = width, height = height }
                        1.0
                        image.blurhash
                , description = image.description
                }
            )
        ]
        { src = image.path, description = image.description }


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


dataformatFromString : String -> Result String DataFormat
dataformatFromString s =
    case s of
        "json" ->
            Ok Json

        "html" ->
            Ok Html

        "graphql" ->
            Ok Graphql

        "xml" ->
            Ok Xml

        _ ->
            Err s


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


pageToString : Page -> String
pageToString p =
    case p of
        GetStarted ->
            "get_started"

        DataFormats ->
            "data_formats"

        PrimaryDatabase ->
            "primary_database"

        PickLoggingFramework ->
            "pick_logging_framework"

        PreferredEditor ->
            "preferred_editor"

        VSCodeAdvice ->
            "vscode_advice"

        IntelliJAdvice ->
            "intellij_advice"

        OtherEditorAdvice ->
            "other_editor_advice"

        Finish ->
            "finish"


pageFromString : String -> Result String Page
pageFromString s =
    case s of
        "get_started" ->
            Ok GetStarted

        "data_formats" ->
            Ok DataFormats

        "primary_database" ->
            Ok PrimaryDatabase

        "pick_logging_framework" ->
            Ok PickLoggingFramework

        "preferred_editor" ->
            Ok PreferredEditor

        "vscode_advice" ->
            Ok VSCodeAdvice

        "intellij_advice" ->
            Ok IntelliJAdvice

        "other_editor_advice" ->
            Ok OtherEditorAdvice

        "finish" ->
            Ok Finish

        _ ->
            Err s


type AppType
    = MPA
    | SPA


type PrimaryDatabase
    = Postgres
    | Mysql
    | Sqlite
    | MSSql


primaryDatabaseToString : PrimaryDatabase -> String
primaryDatabaseToString primaryDatabase =
    case primaryDatabase of
        Postgres ->
            "postgres"

        Mysql ->
            "mysql"

        Sqlite ->
            "sqlite"

        MSSql ->
            "mssql"


primaryDatabaseFromString : String -> Result String PrimaryDatabase
primaryDatabaseFromString s =
    case s of
        "postgres" ->
            Ok Postgres

        "mysql" ->
            Ok Mysql

        "sqlite" ->
            Ok Sqlite

        "mssql" ->
            Ok MSSql

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
    , time : Time.Posix
    , zone : Time.Zone
    , page : Page
    , dataformats : AssocSet.Set DataFormat
    , appType : AppType
    , primaryDatabase : Maybe PrimaryDatabase
    , loggingFramework : Maybe LoggingFramework
    , projectName : String
    , preferredEditor : Maybe PreferredEditor
    }


needsComposeYml : Model -> Bool
needsComposeYml model =
    model.primaryDatabase
        == Just Postgres
        || model.primaryDatabase
        == Just Mysql


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
                |> Maybe.withDefault initModelValues.primaryDatabase
        , preferredEditor =
            Dict.get "preferredEditor" url.queryParameters
                |> Maybe.andThen List.head
                |> Maybe.map preferredEditorFromString
                |> Maybe.andThen Result.toMaybe
                |> Maybe.map Just
                |> Maybe.withDefault initModelValues.preferredEditor
        , loggingFramework =
            Dict.get "loggingFramework" url.queryParameters
                |> Maybe.andThen List.head
                |> Maybe.map loggingFrameworkFromString
                |> Maybe.andThen Result.toMaybe
                |> Maybe.map Just
                |> Maybe.withDefault initModelValues.loggingFramework
        , page =
            Dict.get "page" url.queryParameters
                |> Maybe.andThen List.head
                |> Maybe.map pageFromString
                |> Maybe.andThen Result.toMaybe
                |> Maybe.withDefault initModelValues.page
        , dataformats =
            Dict.get "dataFormat" url.queryParameters
                |> Maybe.map (List.map dataformatFromString)
                |> Maybe.map (List.filterMap Result.toMaybe)
                |> Maybe.map AssocSet.fromList
                |> Maybe.withDefault initModelValues.dataformats
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
        |> (model.loggingFramework
                |> Maybe.map loggingFrameworkToString
                |> Maybe.map (Dict.insert "loggingFramework")
                |> Maybe.withDefault (\params -> params)
           )
        |> Dict.insert "page" (pageToString model.page)
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
    | TimeAndZone ( Time.Posix, Time.Zone )
    | NoOp


type alias Flags =
    Json.Decode.Value


initModelValues :
    { page : Page
    , dataformats : AssocSet.Set a
    , appType : AppType
    , primaryDatabase : Maybe b
    , loggingFramework : Maybe LoggingFramework
    , projectName : String
    , preferredEditor : Maybe c
    }
initModelValues =
    { page = GetStarted
    , dataformats = AssocSet.empty
    , appType = MPA
    , primaryDatabase = Nothing
    , loggingFramework = Nothing
    , projectName = "Clojure QuickStart"
    , preferredEditor = Nothing
    }


init : Json.Decode.Value -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( hydrateModel
        { navKey = key
        , url = AppUrl.fromUrl url
        , page = initModelValues.page
        , dataformats = initModelValues.dataformats
        , appType = initModelValues.appType
        , primaryDatabase = initModelValues.primaryDatabase
        , loggingFramework = initModelValues.loggingFramework
        , projectName = initModelValues.projectName
        , preferredEditor = initModelValues.preferredEditor
        , time = Time.millisToPosix 0
        , zone = Time.utc
        }
    , Time.now
        |> Task.andThen (\time -> Time.here |> Task.map (\zone -> ( time, zone )))
        |> Task.perform TimeAndZone
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
                Just Slf4jSimple ->
                    [ { library = "org.slf4j/slf4j-simple", version = "2.0.9" } ]

                Just Logback ->
                    [ { library = "ch.qos.logback/logback-classic", version = "1.4.11" } ]

                Just Log4j ->
                    [ { library = "org.apache.logging.log4j/log4j-core", version = "2.21.1" }
                    , { library = "org.apache.logging.log4j/log4j-slf4j2-impl", version = "2.21.1" }
                    ]

                _ ->
                    []
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
        ++ (if model.primaryDatabase == Just Sqlite then
                [ { library = "org.xerial/sqlite-jdbc", version = "3.43.2.1" }
                ]

            else
                []
           )
        ++ (if model.primaryDatabase == Just Mysql then
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
testDependencies model =
    [ { library = "lambdaisland/kaocha", version = "1.77.1236" }
    , { library = "lambdaisland/kaocha-junit-xml", version = "1.17.101" }
    ]
        ++ (if model.primaryDatabase == Just Postgres then
                [ { library = "org.testcontainers/postgresql", version = "1.19.1" } ]

            else
                []
           )
        ++ (if model.primaryDatabase == Just Mysql then
                [ { library = "org.testcontainers/mysql", version = "1.19.1" } ]

            else
                []
           )


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
                :extra-paths ["dev" "test"]
                :main-opts ["-M" "-m" "kaocha.runner"]}
          :test {:extra-deps {{{test_deps}}}
                 :extra-paths ["test"]
                 :main-opts ["-m" "kaocha.runner"]}
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
systemClj model =
    """{{ns_declaration}}}}

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
{{start_postgres}}
(defn stop-server
  [server]
  (jetty9/stop-server server))

(defn start-system []
  (let [system {}
        {{init_postgres}}{{init_mysql}}{{init_sqlite}}
        system (assoc system ::server (start-server system))]
    system))

(defn stop-system [system]
  (stop-server (::server system))
  nil)"""
        |> String.Format.namedValue "ns_declaration"
            (if model.primaryDatabase == Just Postgres then
                """(ns system
  (:require [next.jdbc.connection :as connection]
            [ring.adapter.jetty9 :as jetty9]
            [routes :as routes])
  (:import (com.zaxxer.hikari HikariDataSource)
           (java.lang AutoCloseable)))"""

             else
                """(ns system
  (:require [ring.adapter.jetty9 :as jetty9]
            [routes :as routes]))"""
            )
        |> String.Format.namedValue "start_postgres"
            (if model.primaryDatabase == Just Postgres then
                """(defn start-postgres
  []
  (connection/->pool
   HikariDataSource
   {:jdbcUrl   (or (System/getenv "JDBC_DATABASE_URL")
                   "jdbc:postgresql://localhost:54320/mydatabase?user=myuser&password=secret")
    :classname "org.postgresql.Driver"}))

(defn stop-postgres
  [postgres]
  (when (instance? AutoCloseable postgres)
    (.close ^AutoCloseable postgres)))
"""

             else
                ""
            )
        |> String.Format.namedValue "init_postgres"
            (if model.primaryDatabase == Just Postgres then
                "        system (assoc system ::postgres (start-postgres))"

             else
                ""
            )
        |> String.Format.namedValue "init_mysql"
            (if model.primaryDatabase == Just Postgres then
                ""

             else
                ""
            )
        |> String.Format.namedValue "init_sqlite"
            (if model.primaryDatabase == Just Postgres then
                ""

             else
                ""
            )


routesClj : Model -> String
routesClj _ =
    """(ns routes)

(defn root-handler
  ([_system request]
   {:status 200
    :body   (str "Hello, " (:remote-addr request))})
  ([system]
   (fn [request]
     (root-handler system request))))"""


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


testYml : Model -> String
testYml _ =
    """name: Tests

on: [push]

jobs:
  tests:
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

      - name: Test Code
        run: just test"""


postgresTestClj : Model -> String
postgresTestClj _ =
    """(ns postgres-test
  (:require [clojure.test :as t]
            [next.jdbc :as jdbc]))

(t/deftest simple-postgres-test
  (t/testing "Can select a constant"
    (with-open [db (jdbc/get-connection "jdbc:tc:postgresql:16.0:///testdb")]
      (t/is (= {:result 1}
               (jdbc/execute-one! db
                                  ["SELECT 1 as result"]))))))"""


mysqlTestClj : Model -> String
mysqlTestClj _ =
    """(ns mysql-test
  (:require [clojure.test :as t]
            [next.jdbc :as jdbc]))

(t/deftest simple-mysql-test
  (t/testing "Can select a constant"
    (with-open [db (jdbc/get-connection "jdbc:tc:mysql:8.0.35:///testdb")]
      (t/is (= {:result 1}
               (jdbc/execute-one! db
                                  ["SELECT 1 as result"]))))))"""


sqliteTestClj : Model -> String
sqliteTestClj _ =
    """(ns sqlite-test
  (:require [clojure.test :as t]
            [next.jdbc :as jdbc]))

(t/deftest simple-sqlite-test
  (t/testing "Can select a constant"
    (with-open [db (jdbc/get-connection "jdbc:sqlite:test.db")]
      (t/is (= {:result 1}
               (jdbc/execute-one! db
                                  ["SELECT 1 as result"]))))))"""


composeYml : Model -> String
composeYml model =
    """services:
"""
        ++ (if model.primaryDatabase == Just Postgres then
                """  postgres:
    image: 'postgres:16'
    environment:
      - 'POSTGRES_DB=mydatabase'
      - 'POSTGRES_PASSWORD=secret'
      - 'POSTGRES_USER=myuser'
    ports:
      - '5432'
"""

            else
                ""
           )
        ++ (if model.primaryDatabase == Just Mysql then
                """  mysql:
    image: 'mysql:latest'
    environment:
      - 'MYSQL_DATABASE=mydatabase'
      - 'MYSQL_PASSWORD=secret'
      - 'MYSQL_ROOT_PASSWORD=verysecret'
      - 'MYSQL_USER=myuser'
    ports:
      - '3306'
"""

            else
                ""
           )


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
justfile model =
    """# Lists all the available tasks
list:
    just --list

# Run the server
run:
    clojure -M -m main

# Run unit tests
test:
    clojure -M:test

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

{{docker_cmds}}
"""
        |> String.Format.namedValue "docker_cmds"
            (if needsComposeYml model then
                """# Starts docker images
start-docker:
    docker compose up -d

# Stops docker images
stop-docker:
    docker compose down

"""

             else
                ""
            )


insertStringIntoZip : { a | time : Time.Posix, zone : Time.Zone } -> { filename : String, contents : String } -> Zip -> Zip
insertStringIntoZip { time, zone } { filename, contents } zip =
    Zip.insert
        (Bytes.Encode.string contents
            |> Bytes.Encode.encode
            |> Zip.Entry.store
                { path = filename
                , lastModified = ( zone, time )
                , comment = Nothing
                }
        )
        zip


makeZip : Model -> Zip
makeZip model =
    let
        insert =
            insertStringIntoZip model

        insertIf : Bool -> { filename : String, contents : String } -> Zip -> Zip
        insertIf b args zip =
            if b then
                insert args zip

            else
                zip
    in
    Zip.empty
        |> insert { filename = "project/deps.edn", contents = depsEdn model }
        |> insert { filename = "project/tests.edn", contents = testsEdn model }
        |> insert { filename = "project/.cljfmt.edn", contents = cljfmtEdn model }
        |> insert { filename = "project/.gitignore", contents = gitignore model }
        |> insert { filename = "project/Justfile", contents = justfile model }
        |> insert { filename = "project/src/main.clj", contents = mainClj model }
        |> insert { filename = "project/test/main_test.clj", contents = mainTestClj model }
        |> insert { filename = "project/.clj-kondo/config.edn", contents = cljkondoConfig model }
        |> insert { filename = "project/dev/user.clj", contents = devUser model }
        |> insert { filename = "project/src/system.clj", contents = systemClj model }
        |> insert { filename = "project/src/routes.clj", contents = routesClj model }
        |> insert { filename = "project/resources/.gitkeep", contents = resourcesGitkeep model }
        |> insertIf
            (needsComposeYml model)
            { filename = "project/compose.yml", contents = composeYml model }
        |> insertIf
            (model.primaryDatabase == Just Postgres)
            { filename = "project/test/postgres_test.clj", contents = postgresTestClj model }
        |> insertIf
            (model.primaryDatabase == Just Mysql)
            { filename = "project/test/mysql_test.clj", contents = mysqlTestClj model }
        |> insertIf
            (model.primaryDatabase == Just Sqlite)
            { filename = "project/test/sqlite_test.clj", contents = sqliteTestClj model }


recommended : Element msg
recommended =
    Element.el [ Font.color (Element.rgb255 92 199 12) ] (Element.text "(recommended)")


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


question : String -> Element msg -> List (Element msg) -> Element msg
question q subtitle children =
    Element.column [ Element.width (Element.px 800), Element.height Element.fill ]
        [ Element.paragraph
            [ Font.size 36
            , Font.center
            ]
            [ Element.text q ]
        , Element.textColumn
            [ Font.center
            , Element.paddingEach { left = 0, right = 0, bottom = 16, top = 32 }
            , Element.centerX
            , Element.height (Element.px 116)
            ]
          <|
            [ subtitle ]
        , Element.column
            [ Element.spacingXY 0 48
            , Element.width Element.fill
            , Element.height Element.fill
            , Element.scrollbarY
            , Element.Border.solid
            , Element.Border.width 1
            , Element.padding 4
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
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacingXY 0 16
                        , Element.paddingXY 0 32
                        , Font.center
                        ]
                        [ Element.link
                            [ Element.centerX
                            , Element.Border.rounded 3
                            , Element.Border.width 2
                            , Element.paddingEach { left = 16, right = 16, bottom = 16, top = 16 }
                            ]
                            { url = "https://cursive-ide.com/userguide/"
                            , label =
                                Element.image
                                    [ Element.height (Element.px 64) ]
                                    { src = "/cursive.svg", description = "Cursive Logo" }
                            }
                        , Element.textColumn [ Element.width Element.fill, Element.spacingXY 0 32, Element.paddingXY 0 48 ]
                            [ Element.paragraph [] [ Element.text "Cursive is the name of the IntelliJ extension for Clojure." ]
                            , Element.paragraph []
                                [ Element.text "Take a moment and go through their getting started guide "
                                , Element.link []
                                    { url = "https://cursive-ide.com/userguide/"
                                    , label =
                                        Element.el
                                            [ Font.color (Element.rgb255 53 72 167) ]
                                        <|
                                            Element.text "here"
                                    }
                                , Element.text "."
                                ]
                            , Element.paragraph [] [ Element.text "If you don't want to do this now, just save the link for later." ]
                            ]
                        ]

                VSCodeAdvice ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacingXY 0 16
                        , Element.paddingXY 0 32
                        , Font.center
                        ]
                        [ Element.link
                            [ Element.centerX
                            , Element.Border.rounded 3
                            , Element.Border.width 2
                            , Element.paddingEach { left = 16, right = 16, bottom = 16, top = 16 }
                            ]
                            { url = "https://calva.io/getting-started/"
                            , label =
                                Element.image
                                    []
                                    { src = "/calva-64h.png", description = "Calva Logo" }
                            }
                        , Element.textColumn [ Element.width Element.fill, Element.spacingXY 0 32, Element.paddingXY 0 48 ]
                            [ Element.paragraph [] [ Element.text "Calva is the name of the VSCode extension for Clojure." ]
                            , Element.paragraph []
                                [ Element.text "Take a moment and go through their getting started guide "
                                , Element.link []
                                    { url = "https://calva.io/getting-started/"
                                    , label =
                                        Element.el
                                            [ Font.color (Element.rgb255 53 72 167) ]
                                        <|
                                            Element.text "here"
                                    }
                                , Element.text "."
                                ]
                            , Element.paragraph [] [ Element.text "If you don't want to do this now, just save the link for later." ]
                            ]
                        ]

                OtherEditorAdvice ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spacingXY 0 16
                        , Element.paddingXY 0 32
                        ]
                        [ Element.link
                            [ Element.centerX
                            , Element.Border.rounded 3
                            , Element.Border.width 2
                            , Element.paddingEach { left = 16, right = 16, bottom = 16, top = 16 }
                            ]
                            { url = "https://clojure-lsp.io/"
                            , label =
                                Element.image
                                    [ Element.height (Element.px 64) ]
                                    { src = "/lsp-logo.svg", description = "Clojure LSP Logo" }
                            }
                        , Element.textColumn [ Element.width Element.fill, Element.spacingXY 0 32, Element.paddingXY 0 48 ]
                            [ Element.paragraph [] [ Element.text "clojure-lsp provides Clojure language support to a wide variety of editors." ]
                            , Element.paragraph []
                                [ Element.text "They document setup instructions for some editors "
                                , Element.link []
                                    { url = "https://clojure-lsp.io/"
                                    , label =
                                        Element.el
                                            [ Font.color (Element.rgb255 53 72 167) ]
                                        <|
                                            Element.text "here"
                                    }
                                , Element.text "."
                                ]
                            , Element.paragraph [] [ Element.text "If you don't want to do this now, just save the link for later." ]
                            ]
                        ]

                PreferredEditor ->
                    question "What editor are you most comfortable with"
                        Element.none
                        [ checkbox
                            { onPress = SelectPreferredEditor VSCode
                            , activated = model.preferredEditor == Just VSCode
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.el [ Font.bold ] (Element.text "VSCode")
                                    , recommended
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
                                    [ Element.el [ Font.bold ] (Element.text "Something else")
                                    ]
                            , description =
                                Element.el
                                    []
                                    (Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "Emacs, Vim, Neovim, Notepad, etc." ]
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
                    Element.column [ Element.width Element.fill, Element.height Element.fill ]
                        [ Element.textColumn
                            [ Element.spacing 16 ]
                            [ Element.paragraph
                                [ Element.centerY
                                , Element.centerX
                                , Element.paddingXY 0 4
                                ]
                                [ Element.text "Welcome! 😊" ]
                            , Element.paragraph
                                [ Element.centerY
                                , Element.centerX
                                , Element.paddingXY 0 4
                                ]
                                [ Element.text "This page is intended to walk you through creating a modern, production ready, Clojure web server." ]
                            , Element.paragraph
                                [ Element.centerY
                                , Element.centerX
                                , Element.paddingXY 0 4
                                ]
                                [ Element.text "You will be asked a series of questions about what you are making and at the end you will be given a download link to an appropriate starter project." ]
                            , Element.paragraph
                                [ Element.centerY
                                , Element.centerX
                                , Element.paddingXY 0 4
                                ]
                                [ Element.text "If an option you want is not listed or you have any other feedback you should reach out to me "
                                , Element.link
                                    []
                                    { url = "https://clojurians.slack.com"
                                    , label =
                                        Element.el
                                            [ Font.color (Element.rgb255 53 72 167) ]
                                            (Element.text "@emccue on the Clojure community slack.")
                                    }
                                ]
                            ]
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
                            { onPress = Just Next, label = Element.text "Get Started" }
                        ]

                PrimaryDatabase ->
                    question "Choose a primary database."
                        (Element.paragraph
                            [ Element.width Element.fill, Element.height Element.fill ]
                            [ Element.text "Most web servers are best served by a SQL database. You can click the next button without selecting an option if you think that general advice doesn't match your situation." ]
                        )
                        [ checkbox
                            { onPress = SelectPrimaryDatabase Postgres
                            , activated = model.primaryDatabase == Just Postgres
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.el [ Font.bold ] <| Element.text "Postgresql"
                                    , recommended
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
                                    [ Element.el [ Font.bold ] <| Element.text "MySQL"
                                    ]
                            , description =
                                Element.textColumn
                                    [ Element.spacing 16 ]
                                    [ Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "Open source relational database maintained by Oracle." ]
                                    , Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "Some features are gated behind the enterprise edition, which is not free." ]
                                    ]
                            , singleSelect = True
                            }
                        , checkbox
                            { onPress = SelectPrimaryDatabase Sqlite
                            , activated = model.primaryDatabase == Just Sqlite
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.el [ Font.bold ] <| Element.text "Microsoft SQL Server"
                                    ]
                            , description =
                                Element.textColumn
                                    [ Element.spacing 16 ]
                                    [ Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "Proprietary SQL database from Microsoft." ]
                                    , Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "If you don't work for a company that is already paying for a license, this probably is not what you want." ]
                                    ]
                            , singleSelect = True
                            }
                        , checkbox
                            { onPress = SelectPrimaryDatabase Sqlite
                            , activated = model.primaryDatabase == Just Sqlite
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.el [ Font.bold ] <| Element.text "SQLite"
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
                        Element.none
                        [ checkbox
                            { onPress = SelectDataFormat Json
                            , activated = AssocSet.member Json model.dataformats
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.el [ Font.bold ] <| Element.text "JSON"
                                    ]
                            , description =
                                Element.textColumn
                                    [ Element.spacing 16 ]
                                    [ Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "This is probably what you want if you are planning on using the server along with a JavaScript rendered frontend using something like React." ]
                                    ]
                            , singleSelect = False
                            }
                        , checkbox
                            { onPress = SelectDataFormat Html
                            , activated = AssocSet.member Html model.dataformats
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.el [ Font.bold ] <| Element.text "HTML"
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
                                    [ Element.el [ Font.bold ] <| Element.text "XML"
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
                        Element.none
                        [ checkbox
                            { onPress = SelectLoggingFramework Slf4jSimple
                            , activated = model.loggingFramework == Just Slf4jSimple
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.el [ Font.bold ] <| Element.text "slf4j-simple"
                                    , recommended
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
                            , activated = model.loggingFramework == Just Logback
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.el [ Font.bold ] <| Element.text "Logback"
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
                            , activated = model.loggingFramework == Just Log4j
                            , text =
                                Element.row [ Element.spacing 8 ]
                                    [ Element.el [ Font.bold ] <| Element.text "Log4j2"
                                    ]
                            , description =
                                Element.el
                                    []
                                    (Element.paragraph
                                        [ Element.centerY
                                        , Element.centerX
                                        , Element.paddingXY 0 4
                                        ]
                                        [ Element.text "Logging framework famous for Log4Shell." ]
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
                | loggingFramework =
                    if model.loggingFramework == Just loggingFramework then
                        Nothing

                    else
                        Just loggingFramework
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
            ( model
            , Navigation.back model.navKey 1
            )

        Download ->
            ( model
            , makeZip model
                |> Zip.toBytes
                |> File.Download.bytes "project.zip" "application/zip"
            )

        UrlRequest request ->
            case request of
                Browser.Internal url ->
                    ( hydrateModel { model | url = AppUrl.fromUrl url }, Navigation.pushUrl model.navKey (Url.toString url) )

                Browser.External url ->
                    ( model, Navigation.load url )

        UrlChange url ->
            ( hydrateModel { model | url = AppUrl.fromUrl url }
            , Cmd.none
            )

        TimeAndZone ( time, zone ) ->
            ( { model | time = time, zone = zone }, Cmd.none )

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

        Next ->
            let
                ( model2, cmd ) =
                    update msg model

                url =
                    model2.url

                newUrl =
                    { url | queryParameters = modelToQueryParams model2 }
            in
            ( { model2 | url = newUrl }
            , Cmd.batch [ cmd, Navigation.pushUrl model2.navKey (AppUrl.toString newUrl) ]
            )

        _ ->
            update msg model


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
