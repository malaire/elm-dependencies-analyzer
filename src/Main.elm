module Main exposing (main)

import Backend
import Browser
import Cache exposing (Cache, FetchedValue(..))
import Compatible
import Css as C
import Dict exposing (Dict)
import Elm.Package
import Elm.Project
import Elm.Version
import File exposing (File)
import File.Download
import File.Select
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as HE
import Json.Decode as JD
import Json.Encode as JE
import List.Extra as ListExtra
import Maybe.Extra as MaybeExtra
import Misc exposing (InternalError(..), Package, PackageSolved, PackageStateSolved(..), PackageStateUnsolved(..))
import Monocle.Common
import Monocle.Compose
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)
import RangeDict exposing (RangeDict)
import Result.Extra as ResultExtra
import Set exposing (Set)
import StepResult
import Task
import Version exposing (Version, VersionId, VersionRange)
import ViewCache exposing (ViewCache)



-- TYPES


type alias Model =
    { state : ModelState
    , inputJson : String
    , mouseOverVersion : Maybe VersionId
    , packages : Dict String Package
    , maybePreViewInternalError : Maybe InternalError
    , seenInternalErrors : Set Int

    -- WIP
    , appInfo : Maybe Elm.Project.ApplicationInfo
    , showElmJson : ShowElmJson
    }


type ModelState
    = NothingAnalyzed
    | JsonParsingError String
    | Fetching Int Cache.FetchingCache
    | FetchingSucceeded Cache ViewCache
    | FetchingFailed (List (Html Msg))


type ShowElmJson
    = ShowOriginal
    | ShowNew
    | ShowBoth


type Msg
    = InputJsonChanged String
    | ExampleClick
    | OpenFileClick
    | FileOpened File
    | GotFileContents String
    | SaveFileClick String String String
    | AnalyzeButtonClick
    | ShowElmJsonClick ShowElmJson
    | Fetched Backend.FetchedMsg
    | VersionClick String Version
    | IsDirectCheckboxClick String Bool
    | MouseOverVersion String Version
    | MouseOutVersion String Version
    | NoOp



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view >> H.toUnstyled
        }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = NothingAnalyzed
      , inputJson = ""
      , mouseOverVersion = Nothing
      , packages = Dict.empty
      , maybePreViewInternalError = Nothing
      , seenInternalErrors = Set.empty
      , appInfo = Nothing
      , showElmJson = ShowBoth
      }
    , Cmd.none
    )



-- UPDATE


{-| Main part of update is done in `updateMain`.
After that has been done, `updatePreView` performs some data integrity checks
and data pre-generation for next view.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, updateCmd ) =
            updateMain msg model

        ( maybePreViewInternalError, preViewCmd ) =
            updatePreView newModel
    in
    ( { newModel
        | maybePreViewInternalError = maybePreViewInternalError
        , seenInternalErrors =
            case maybePreViewInternalError of
                Just preViewInternalError ->
                    Set.insert (Misc.internalErrorTag preViewInternalError) model.seenInternalErrors

                Nothing ->
                    model.seenInternalErrors
      }
    , Cmd.batch [ updateCmd, preViewCmd ]
    )


updatePreView : Model -> ( Maybe InternalError, Cmd Msg )
updatePreView model =
    let
        viewCacheCheck : Maybe InternalError
        viewCacheCheck =
            case model.state of
                FetchingSucceeded cache viewCache ->
                    ViewCache.validateForView viewCache

                _ ->
                    Nothing

        -- This is re-determined on every `update`, so previous state is just ignored.
        maybePreViewInternalError =
            viewCacheCheck
    in
    ( maybePreViewInternalError
    , case maybePreViewInternalError of
        Just preViewInternalError ->
            case model.appInfo of
                Just appInfo ->
                    -- only report if this tag hasn't been seen yet
                    if not <| Set.member (Misc.internalErrorTag preViewInternalError) model.seenInternalErrors then
                        Backend.reportInternalError NoOp
                            preViewInternalError
                            appInfo
                            model.packages
                            model.mouseOverVersion

                    else
                        Cmd.none

                Nothing ->
                    -- IMPOSSIBLE
                    Cmd.none

        Nothing ->
            Cmd.none
    )


updateMain : Msg -> Model -> ( Model, Cmd Msg )
updateMain msg model =
    case msg of
        InputJsonChanged inputJson ->
            ( { model | inputJson = inputJson }, Cmd.none )

        ExampleClick ->
            ( { model | inputJson = exampleJson }, Cmd.none )

        OpenFileClick ->
            ( model, File.Select.file [ "application/json" ] FileOpened )

        FileOpened file ->
            ( model, Task.perform GotFileContents (File.toString file) )

        GotFileContents contents ->
            ( { model | inputJson = contents }, Cmd.none )

        SaveFileClick filename mime contents ->
            ( model, File.Download.string filename mime contents )

        AnalyzeButtonClick ->
            updateAnalyzeButtonClick model

        ShowElmJsonClick value ->
            ( { model | showElmJson = value }, Cmd.none )

        Fetched fetched ->
            updateFetched model fetched

        VersionClick name version ->
            let
                newPackages =
                    model.packages
                        |> (selectedVersionOfPackages name).set version

                newState =
                    case model.state of
                        FetchingSucceeded cache viewCache ->
                            ViewCache.updateWithSelectedVersions newPackages cache viewCache
                                |> FetchingSucceeded cache

                        other ->
                            other
            in
            ( { model
                | state = newState
                , packages = newPackages
              }
            , Cmd.none
            )

        IsDirectCheckboxClick name isTestSection ->
            case model.state of
                FetchingSucceeded cache viewCache ->
                    let
                        newPackages =
                            case Dict.get name model.packages of
                                Nothing ->
                                    -- IMPOSSIBLE
                                    model.packages

                                Just package ->
                                    let
                                        newPackage =
                                            { package
                                                | state =
                                                    case ( Misc.isDirect package, isTestSection ) of
                                                        ( False, False ) ->
                                                            DirectNormal_

                                                        ( False, True ) ->
                                                            DirectTest_

                                                        ( True, _ ) ->
                                                            IndirectOrNotNeeded
                                            }
                                    in
                                    Dict.insert name newPackage model.packages

                        newState =
                            ViewCache.updateWithSelectedVersions newPackages cache viewCache
                                |> FetchingSucceeded cache
                    in
                    ( { model
                        | state = newState
                        , packages = newPackages
                      }
                    , Cmd.none
                    )

                _ ->
                    -- IMPOSSIBLE
                    ( model, Cmd.none )

        MouseOverVersion name version ->
            let
                newState =
                    case model.state of
                        FetchingSucceeded cache viewCache ->
                            ViewCache.updateWithMouseOverVersion ( name, version ) cache viewCache
                                |> FetchingSucceeded cache

                        other ->
                            other
            in
            ( { model
                | mouseOverVersion = Just ( name, version )
                , state = newState
              }
            , Cmd.none
            )

        MouseOutVersion name version ->
            if Just ( name, version ) == model.mouseOverVersion then
                ( { model | mouseOverVersion = Nothing }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateAnalyzeButtonClick : Model -> ( Model, Cmd Msg )
updateAnalyzeButtonClick model =
    case JD.decodeString Elm.Project.decoder model.inputJson of
        Err error ->
            ( { model
                | packages = Dict.empty
                , state = JsonParsingError (JD.errorToString error)
              }
            , Cmd.none
            )

        Ok (Elm.Project.Package _) ->
            ( { model
                | packages = Dict.empty
                , state =
                    JsonParsingError
                        ("Only application elm.json is supported, not package elm.json.\n"
                            ++ "If you would find it useful to also support package elm.json,\n"
                            ++ "please open an issue at GitHub repository."
                        )
              }
            , Cmd.none
            )

        Ok (Elm.Project.Application appInfo) ->
            let
                fromElmDeps :
                    Elm.Project.Deps Elm.Version.Version
                    -> PackageStateSolved
                    -> List ( String, Version, PackageStateSolved )
                fromElmDeps deps state =
                    deps
                        |> List.map
                            (\( name, elmVersion ) ->
                                ( Elm.Package.toString name
                                , Elm.Version.toTuple elmVersion
                                , state
                                )
                            )

                appDepends =
                    fromElmDeps appInfo.depsDirect DirectNormal
                        ++ fromElmDeps appInfo.depsIndirect IndirectNormal
                        ++ fromElmDeps appInfo.testDepsDirect DirectTest
                        ++ fromElmDeps appInfo.testDepsIndirect IndirectTest

                newPackageCache =
                    appDepends
                        |> List.map
                            (\( name, version, _ ) ->
                                ( name
                                , { allVersions = NotFetched
                                  , minVersion = version
                                  , minVersionIsJsonVersion = True
                                  }
                                )
                            )
                        |> Dict.fromList

                newPackages =
                    appDepends
                        |> List.map
                            (\( name, version, state ) ->
                                ( name
                                , { state =
                                        case state of
                                            DirectNormal ->
                                                DirectNormal_

                                            DirectTest ->
                                                DirectTest_

                                            IndirectNormal ->
                                                IndirectOrNotNeeded

                                            IndirectTest ->
                                                IndirectOrNotNeeded
                                  , selectedVersion = version
                                  , initialState =
                                        Just
                                            { state = state
                                            , version = version
                                            }
                                  }
                                )
                            )
                        |> Dict.fromList

                newFetchingCache =
                    { packages = newPackageCache
                    , depends = Dict.empty
                    }
                        |> addMissingVersionsToDependsCache

                ( newState, nextCmd ) =
                    case Cache.fetchNextThing newFetchingCache of
                        Just cmd ->
                            ( Fetching 0 newFetchingCache, cmd )

                        Nothing ->
                            case Cache.validate newFetchingCache of
                                Ok cache ->
                                    ( FetchingSucceeded cache (ViewCache.new newPackages cache)
                                    , Cmd.none
                                    )

                                Err errors ->
                                    ( errors
                                        |> List.map
                                            (\error ->
                                                H.li [] [ H.text <| Misc.errorToStr error ]
                                            )
                                        |> FetchingFailed
                                    , Cmd.none
                                    )
            in
            ( { model
                | packages = newPackages
                , state = newState
                , appInfo = Just appInfo
              }
            , Cmd.map Fetched nextCmd
            )


updateFetched :
    Model
    -> Backend.FetchedMsg
    -> ( Model, Cmd Msg )
updateFetched model fetched =
    case model.state of
        Fetching oldDone fetchingCache ->
            let
                newFetchingCache =
                    case fetched of
                        Backend.FetchedVersions name result ->
                            case result of
                                Err error ->
                                    { packages =
                                        fetchingCache.packages
                                            |> (Cache.allVersionsOfFetchingPackageCache name).set Failed
                                    , depends = fetchingCache.depends
                                    }

                                Ok packageVersions ->
                                    { packages =
                                        fetchingCache.packages
                                            |> (Cache.allVersionsOfFetchingPackageCache name).set
                                                (Succeeded packageVersions)
                                    , depends = fetchingCache.depends
                                    }
                                        |> addMissingVersionsToDependsCache

                        Backend.FetchedDepends name version result ->
                            case result of
                                Err error ->
                                    { packages = fetchingCache.packages
                                    , depends = Dict.insert ( name, version ) Failed fetchingCache.depends
                                    }

                                Ok versionRanges ->
                                    let
                                        addonPackageCache =
                                            versionRanges
                                                |> Dict.map
                                                    (\n ( min, max ) ->
                                                        { allVersions = NotFetched
                                                        , minVersion = min
                                                        , minVersionIsJsonVersion = False
                                                        }
                                                    )

                                        newPackageCache =
                                            Dict.merge
                                                Dict.insert
                                                (\mergeName old new ->
                                                    Dict.insert mergeName
                                                        { allVersions = old.allVersions
                                                        , minVersion =
                                                            if old.minVersionIsJsonVersion then
                                                                old.minVersion

                                                            else
                                                                min old.minVersion new.minVersion
                                                        , minVersionIsJsonVersion = old.minVersionIsJsonVersion
                                                        }
                                                )
                                                Dict.insert
                                                fetchingCache.packages
                                                addonPackageCache
                                                Dict.empty
                                    in
                                    { packages = newPackageCache
                                    , depends = Dict.insert ( name, version ) (Succeeded versionRanges) fetchingCache.depends
                                    }
                                        |> addMissingVersionsToDependsCache

                newPackages =
                    case fetched of
                        Backend.FetchedVersions name (Ok packageVersions) ->
                            if not (Dict.member name model.packages) then
                                case ListExtra.last <| List.map Tuple.first <| packageVersions of
                                    Just latestVersion ->
                                        Dict.insert
                                            name
                                            { state = IndirectOrNotNeeded
                                            , selectedVersion = latestVersion
                                            , initialState = Nothing
                                            }
                                            model.packages

                                    Nothing ->
                                        -- TODO IMPOSSIBLE
                                        model.packages

                            else
                                model.packages

                        _ ->
                            model.packages

                maybeNextCmd =
                    Cache.fetchNextThing newFetchingCache

                newDone =
                    oldDone + 1

                newState =
                    case maybeNextCmd of
                        Just _ ->
                            Fetching newDone newFetchingCache

                        Nothing ->
                            case Cache.validate newFetchingCache of
                                Ok cache ->
                                    FetchingSucceeded cache (ViewCache.new model.packages cache)

                                Err errors ->
                                    errors
                                        |> List.map
                                            (\error ->
                                                H.li [] [ H.text <| Misc.errorToStr error ]
                                            )
                                        |> FetchingFailed
            in
            ( { model
                | packages = newPackages
                , state = newState
              }
            , Cmd.map Fetched <| Maybe.withDefault Cmd.none maybeNextCmd
            )

        _ ->
            -- IMPOSSIBLE
            ( model, Cmd.none )



-- UPDATE - HELPERS


{-| Add versions present in packages but not in depends to depends as `NotFetched`, with `minVersion` filtering.
-}
addMissingVersionsToDependsCache : Cache.FetchingCache -> Cache.FetchingCache
addMissingVersionsToDependsCache fetchingCache =
    { fetchingCache
        | depends =
            fetchingCache.packages
                |> Dict.toList
                |> List.concatMap
                    (\( name, item ) ->
                        case item.allVersions of
                            Succeeded allVersions ->
                                allVersions
                                    |> List.filterMap
                                        (\( version, _ ) ->
                                            if version >= item.minVersion then
                                                Just ( ( name, version ), NotFetched )

                                            else
                                                Nothing
                                        )

                            NotFetched ->
                                []

                            Failed ->
                                []
                    )
                |> Dict.fromList
                |> Dict.union fetchingCache.depends
    }



-- VIEW


view : Model -> Html Msg
view model =
    let
        content =
            case model.maybePreViewInternalError of
                Nothing ->
                    let
                        ( rightSectionHtml, maybeNewJson ) =
                            viewRightSection model
                    in
                    H.tr []
                        [ H.td [ A.css [ C.verticalAlign C.top ] ]
                            (viewLeftSection model maybeNewJson)
                        , H.td
                            [ A.css [ C.verticalAlign C.top ] ]
                            rightSectionHtml
                        ]

                Just preViewInternalError ->
                    H.tr []
                        [ H.td [ A.css [ C.color (C.hex "800"), C.fontWeight C.bold ] ]
                            [ H.br [] []
                            , H.text <| Misc.internalErrorToStr preViewInternalError
                            ]
                        ]
    in
    H.table []
        [ H.tr []
            [ H.td [ A.colspan 2 ]
                [ H.text "See README at "
                , H.a
                    [ A.href "https://github.com/malaire/elm-dependencies-analyzer"
                    , A.target "_blank"
                    ]
                    [ H.text "GitHub repository" ]
                , H.text " for usage instructions."
                ]
            ]
        , content
        ]


viewLeftSection : Model -> Maybe String -> List (Html Msg)
viewLeftSection model maybeNewJson =
    [ H.table []
        [ H.tr []
            ((if model.showElmJson == ShowOriginal || model.showElmJson == ShowBoth then
                [ H.td []
                    [ H.textarea
                        [ A.rows 40
                        , A.cols 50
                        , HE.onInput InputJsonChanged
                        , A.property "value" (JE.string model.inputJson)
                        ]
                        []
                    ]
                ]

              else
                []
             )
                ++ (if model.showElmJson == ShowNew || model.showElmJson == ShowBoth then
                        [ H.td []
                            [ H.textarea
                                [ A.rows 40
                                , A.cols 50
                                , A.property "value" (JE.string <| Maybe.withDefault "" <| maybeNewJson)
                                , A.readonly True
                                ]
                                []
                            ]
                        ]

                    else
                        []
                   )
            )
        ]
    , H.text "Show elm.json: "
    ]
        ++ ([ ( "Original", ShowOriginal ), ( "New", ShowNew ), ( "Both", ShowBoth ) ]
                |> List.concatMap
                    (\( title, type_ ) ->
                        [ H.input
                            [ A.type_ "radio"
                            , A.id ("showElmJson-" ++ title)
                            , A.name "showElmJson"
                            , A.checked (model.showElmJson == type_)
                            , HE.onClick (ShowElmJsonClick type_)
                            ]
                            []
                        , H.label [ A.for ("showElmJson-" ++ title) ] [ H.text title ]
                        , H.text " "
                        ]
                    )
           )
        ++ [ H.br [] []
           , H.button [ HE.onClick OpenFileClick ] [ H.text "Open File ..." ]
           , H.text " "
           , H.button [ HE.onClick AnalyzeButtonClick ] [ H.text "Analyze" ]
           , H.text " "
           , H.button
                ([ A.disabled (maybeNewJson == Nothing) ]
                    ++ (case maybeNewJson of
                            Nothing ->
                                []

                            Just newJson ->
                                [ HE.onClick (SaveFileClick "elm.json" "application/json" newJson) ]
                       )
                )
                [ H.text "Save File ..." ]
           , H.hr [] []
           , H.button [ HE.onClick ExampleClick ] [ H.text "Load example" ]
           ]


viewRightSection : Model -> ( List (Html Msg), Maybe String )
viewRightSection model =
    case model.state of
        NothingAnalyzed ->
            ( [ H.text "Nothing analyzed yet." ], Nothing )

        JsonParsingError error ->
            ( [ H.pre [] [ H.text (String.replace "\\n" "\n" error) ] ], Nothing )

        Fetching done _ ->
            ( [ H.text ("Fetching package data ... " ++ String.fromInt done) ], Nothing )

        FetchingFailed errors ->
            ( [ H.ul [] errors ], Nothing )

        FetchingSucceeded cache viewCache ->
            case viewRightSectionWhenFetchingSucceeded model cache viewCache of
                Err error ->
                    ( [ H.text <| Misc.internalErrorToStr error ], Nothing )

                Ok ( html, maybeNewJson ) ->
                    ( html, maybeNewJson )


viewRightSectionWhenFetchingSucceeded :
    Model
    -> Cache
    -> ViewCache
    -> Result InternalError ( List (Html Msg), Maybe String )
viewRightSectionWhenFetchingSucceeded model cache viewCache =
    let
        rSolved : Result InternalError ( Dict String PackageSolved, RangeDict )
        rSolved =
            solveIndirectPackages cache model.packages
    in
    case rSolved of
        Err error ->
            Err error

        Ok ( packagesSolved, allDeps ) ->
            let
                problems =
                    RangeDict.getProblems allDeps

                isInternalInconsistency =
                    case viewCache.selectedVersionsAreCompatible of
                        Nothing ->
                            False

                        Just bool ->
                            (problems == []) /= bool

                rPackagesHtml =
                    viewPackages model.mouseOverVersion cache viewCache packagesSolved
            in
            case rPackagesHtml of
                Err error ->
                    Err error

                Ok packagesHtml ->
                    let
                        html =
                            [ packagesHtml ]
                                ++ (if List.isEmpty problems then
                                        [ H.div
                                            [ A.css [ C.color (C.hex "080") ] ]
                                            [ H.text "Selected versions have no conflicts."
                                            ]
                                        ]

                                    else
                                        [ H.div
                                            [ A.css [ C.color (C.hex "800") ] ]
                                            [ H.text "Selected versions have dependency conflicts:"
                                            , H.ul [] problems
                                            ]
                                        ]
                                   )
                                ++ (if isInternalInconsistency then
                                        [ H.div [] [ H.text "ERROR: Internal inconsistency detected." ] ]

                                    else
                                        []
                                   )

                        maybeNewJson =
                            if List.isEmpty problems then
                                createNewJson model.appInfo packagesSolved

                            else
                                Nothing
                    in
                    Ok ( html, maybeNewJson )


viewPackages :
    Maybe VersionId
    -> Cache
    -> ViewCache
    -> Dict String PackageSolved
    -> Result InternalError (Html Msg)
viewPackages mouseOverVersion cache viewCache packagesSolved =
    let
        allPackagesToShow =
            packagesSolved
                |> Dict.toList
                |> List.filterMap
                    (\( name, package ) ->
                        -- packages of parsed elm.json are shown in sections according to
                        -- initialState ; new packages according to current state
                        case package.initialState of
                            Just initialState ->
                                Just ( name, package, initialState.state )

                            Nothing ->
                                case package.state of
                                    Just state ->
                                        Just ( name, package, state )

                                    Nothing ->
                                        Nothing
                    )
                |> List.sortWith
                    (\( nameA, packageA, _ ) ( nameB, packageB, _ ) ->
                        -- sort new packages last
                        case ( packageA.initialState, packageB.initialState ) of
                            ( Just _, Nothing ) ->
                                LT

                            ( Nothing, Just _ ) ->
                                GT

                            ( Just _, Just _ ) ->
                                compare nameA nameB

                            ( Nothing, Nothing ) ->
                                compare nameA nameB
                    )

        showSection : PackageStateSolved -> String -> List (Result InternalError (Html Msg))
        showSection section emptyText =
            let
                sectionPackages =
                    allPackagesToShow
                        |> List.filter (\( _, _, section_ ) -> section_ == section)
            in
            (if List.isEmpty sectionPackages then
                [ Ok <|
                    H.tr []
                        [ H.td
                            [ A.colspan 4
                            , A.css
                                [ C.fontStyle C.italic
                                , C.color (C.hex "#888")
                                ]
                            ]
                            [ H.text emptyText ]
                        ]
                ]

             else
                sectionPackages
                    |> List.map
                        (\( name, package, _ ) ->
                            viewPackage
                                { mouseOverVersion = mouseOverVersion
                                , cache = cache
                                , viewCache = viewCache
                                , isTestSection =
                                    section == DirectTest || section == IndirectTest
                                , package = package
                                , name = name
                                }
                        )
            )
                ++ [ Ok <| H.tr [] [ H.td [ A.colspan 4 ] [ H.hr [] [] ] ] ]

        rHtml : Result InternalError (List (Html Msg))
        rHtml =
            showSection DirectNormal "No direct dependencies."
                ++ showSection IndirectNormal "No indirect dependencies."
                ++ showSection DirectTest "No direct test-dependencies."
                ++ showSection IndirectTest "No indirect test-dependencies."
                |> ResultExtra.combine
    in
    case rHtml of
        Err error ->
            Err error

        Ok html ->
            Ok <| H.table [] html


viewPackage :
    { mouseOverVersion : Maybe VersionId
    , cache : Cache
    , viewCache : ViewCache
    , isTestSection : Bool
    , package : PackageSolved
    , name : String
    }
    -> Result InternalError (Html Msg)
viewPackage { mouseOverVersion, cache, viewCache, isTestSection, package, name } =
    let
        packageLink =
            if String.left 8 name == "example/" then
                H.span [] [ H.text "src" ]

            else
                H.a
                    [ A.href ("https://github.com/" ++ name)
                    , A.target "_blank"
                    , A.css [ C.textDecoration C.none ]
                    ]
                    [ H.text "src" ]

        isTestHtml =
            if package.state == Just DirectTest || package.state == Just IndirectTest then
                H.span [ A.css [ C.color (C.hex "00A"), C.fontSize C.smaller ] ]
                    [ H.text "TEST" ]

            else
                H.span [] []

        isDirect =
            package.state == Just DirectNormal || package.state == Just DirectTest

        nameStyleColor =
            if isDirect then
                []

            else
                [ C.color (C.hex "#888") ]

        nameStyleStrike =
            if package.state == Nothing then
                [ C.textDecoration3 C.lineThrough C.solid (C.hex "#000") ]

            else
                []

        nameStyle =
            nameStyleColor ++ nameStyleStrike

        rPackageVersionsHtml =
            viewPackageVersions
                { mouseOverVersion = mouseOverVersion
                , cache = cache
                , viewCache = viewCache
                , package = package
                , name = name
                }
    in
    case rPackageVersionsHtml of
        Err error ->
            Err error

        Ok packageVersionsHtml ->
            Ok <|
                H.tr []
                    ([ H.td [] [ packageLink ]
                     , H.td []
                        [ H.input
                            [ A.type_ "checkbox"
                            , A.checked isDirect
                            , HE.on "change" (JD.succeed (IsDirectCheckboxClick name isTestSection))
                            ]
                            []
                        ]
                     , H.td [] [ isTestHtml ]
                     , H.td []
                        ((if package.initialState == Nothing then
                            [ H.span [ A.css [ C.color (C.hex "00A"), C.fontSize C.smaller ] ]
                                [ H.text "NEW " ]
                            ]

                          else
                            []
                         )
                            ++ [ H.span [ A.css nameStyle ] [ H.text name ] ]
                        )
                     ]
                        ++ packageVersionsHtml
                    )


viewPackageVersions :
    { mouseOverVersion : Maybe VersionId
    , cache : Cache
    , viewCache : ViewCache
    , package : PackageSolved
    , name : String
    }
    -> Result InternalError (List (Html Msg))
viewPackageVersions { mouseOverVersion, cache, viewCache, package, name } =
    case Dict.get name cache.versions of
        Just versions ->
            versions
                |> List.map
                    (\( version, _ ) ->
                        viewVersion
                            { mouseOverVersion = mouseOverVersion
                            , cache = cache
                            , viewCache = viewCache
                            , package = package
                            , name = name
                            , version = version
                            }
                    )
                |> ResultExtra.combine

        Nothing ->
            Err <| NameNotFound 6754 name


viewVersion :
    { mouseOverVersion : Maybe VersionId
    , cache : Cache
    , viewCache : ViewCache
    , package : PackageSolved
    , name : String
    , version : Version
    }
    -> Result InternalError (Html Msg)
viewVersion { mouseOverVersion, cache, viewCache, package, name, version } =
    let
        versionHasMouseOver =
            mouseOverVersion == Just ( name, version )

        packageHasMouseOver =
            case mouseOverVersion of
                Just ( moName, _ ) ->
                    moName == name

                Nothing ->
                    False

        isCompatibleWithMouseOver =
            case mouseOverVersion of
                Just moVersionId ->
                    Dict.get ( ( name, version ), moVersionId ) viewCache.pairIsCompatible
                        |> MaybeExtra.join

                Nothing ->
                    Nothing

        isCompatibleWithSelected =
            Dict.get ( name, version ) viewCache.isCompatibleWithSelected
                |> MaybeExtra.join

        isDirect =
            package.state == Just DirectNormal || package.state == Just DirectTest

        styleBase =
            [ C.borderRadius (C.em 0.2)
            ]

        styleColor =
            if isDirect then
                [ C.color (C.hex "000") ]

            else
                [ C.color (C.hex "888") ]

        styleBorder =
            if versionHasMouseOver then
                [ C.border3 (C.px 2) C.solid (C.hex "#00A") ]
                --else if not packageHasMouseOver && isCompatibleWithMouseOver == Ok True then
                --    [ C.border3 (C.px 2) C.solid (C.hex "#0F0") ]

            else if not packageHasMouseOver && isCompatibleWithMouseOver == Just False then
                [ C.border3 (C.px 2) C.solid (C.hex "#F00") ]

            else
                [ C.border3 (C.px 2) C.solid (C.hex "#0000") ]

        styleBgColor =
            if version == package.selectedVersion && package.state /= Nothing then
                [ C.backgroundColor (C.hex "CCE") ]

            else if
                (viewCache.selectedVersionsAreCompatible == Just True)
                    && (isCompatibleWithSelected == Just False)
            then
                [ C.backgroundColor (C.hex "FBB") ]

            else
                []

        style =
            styleBase ++ styleColor ++ styleBorder ++ styleBgColor

        hasError =
            ((mouseOverVersion /= Nothing)
                && not packageHasMouseOver
                && (isCompatibleWithMouseOver == Nothing)
            )
                || (isCompatibleWithSelected == Nothing)
    in
    if hasError then
        Err <|
            OtherInternalError 3073 <|
                "Check in viewVersion failed for "
                    ++ Version.idToStr ( name, version )

    else
        Ok <|
            H.td [ A.css [ C.textAlign C.right ] ]
                [ H.span
                    ([ A.css style
                     , HE.onMouseOver (MouseOverVersion name version)
                     , HE.onMouseOut (MouseOutVersion name version)
                     ]
                        ++ (if package.state /= Nothing then
                                [ HE.onClick (VersionClick name version) ]

                            else
                                []
                           )
                    )
                    [ H.text <| Version.versionToStr version
                    ]
                ]



-- VIEW - HELPERS


{-| TODO WIP
-}
solveIndirectPackages :
    Cache
    -> Dict String Package
    -> Result InternalError ( Dict String PackageSolved, RangeDict )
solveIndirectPackages cache packages =
    let
        rAllDeps =
            packages
                |> Dict.map (\_ package -> ( package.selectedVersion, Misc.isDirect package ))
                |> Cache.rangeDictOfDepends cache.depends

        rNonTestDeps =
            packages
                |> Dict.map (\_ package -> ( package.selectedVersion, package.state == DirectNormal_ ))
                |> Cache.rangeDictOfDepends cache.depends
    in
    case ( rAllDeps, rNonTestDeps ) of
        ( Ok allDeps, Ok nonTestDeps ) ->
            let
                neededPackages =
                    allDeps |> RangeDict.ranges |> Dict.keys |> Set.fromList

                indirectTestPackages =
                    Set.diff
                        neededPackages
                        (nonTestDeps |> RangeDict.ranges |> Dict.keys |> Set.fromList)

                solvedPackages =
                    packages
                        |> Dict.map
                            (\name package ->
                                let
                                    stateSolved =
                                        if package.state == DirectNormal_ then
                                            Just DirectNormal

                                        else if package.state == DirectTest_ then
                                            Just DirectTest

                                        else if Set.member name indirectTestPackages then
                                            Just IndirectTest

                                        else if Set.member name neededPackages then
                                            Just IndirectNormal

                                        else
                                            Nothing
                                in
                                { state = stateSolved
                                , selectedVersion = package.selectedVersion
                                , initialState = package.initialState
                                }
                            )
            in
            Ok ( solvedPackages, allDeps )

        ( Err error, _ ) ->
            Err error

        ( _, Err error ) ->
            Err error


{-| TODO WIP
-}
createNewJson : Maybe Elm.Project.ApplicationInfo -> Dict String PackageSolved -> Maybe String
createNewJson maybeAppInfo packagesSolved =
    let
        toElmDeps :
            List ( String, Version, PackageStateSolved )
            -> PackageStateSolved
            -> Elm.Project.Deps Elm.Version.Version
        toElmDeps packages state =
            packages
                |> List.filterMap
                    (\( name, version, state_ ) ->
                        if state == state_ then
                            case Elm.Package.fromString name of
                                Nothing ->
                                    -- TODO: ERROR
                                    Nothing

                                Just elmName ->
                                    case Elm.Version.fromTuple version of
                                        Nothing ->
                                            -- TODO: ERROR
                                            Nothing

                                        Just elmVersion ->
                                            Just ( elmName, elmVersion )

                        else
                            Nothing
                    )

        sortedPackages =
            packagesSolved
                |> Dict.toList
                |> List.filterMap
                    (\( name, package ) ->
                        case package.state of
                            Just state ->
                                Just ( name, package.selectedVersion, state )

                            Nothing ->
                                Nothing
                    )
                |> List.sortWith
                    (\( nameA, _, _ ) ( nameB, _, _ ) ->
                        compare nameA nameB
                    )
    in
    case maybeAppInfo of
        Just appInfo ->
            let
                newAppInfo =
                    { elm = appInfo.elm
                    , dirs = appInfo.dirs
                    , depsDirect = toElmDeps sortedPackages DirectNormal
                    , depsIndirect = toElmDeps sortedPackages IndirectNormal
                    , testDepsDirect = toElmDeps sortedPackages DirectTest
                    , testDepsIndirect = toElmDeps sortedPackages IndirectTest
                    }
            in
            Elm.Project.encode (Elm.Project.Application newAppInfo)
                |> JE.encode 4
                |> Just

        Nothing ->
            Nothing



-- MONOCLE - OF PACKAGES


selectedVersionOfPackages : String -> Optional (Dict String Package) Version
selectedVersionOfPackages name =
    Monocle.Common.dict name
        |> Monocle.Compose.optionalWithLens
            (Lens .selectedVersion (\b a -> { a | selectedVersion = b }))



-- EXAMPLE JSON


exampleJson =
    """{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.0",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.0",
            "elm/core": "1.0.0",
            "elm/html": "1.0.0",
            "elm/http": "1.0.0",
            "elm/json": "1.0.0",
            "krisajenkins/remotedata": "5.0.0",
            "rtfeldman/elm-css": "16.1.0"
        },
        "indirect": {
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.0",
            "rtfeldman/elm-hex": "1.0.0"
        }
    },
    "test-dependencies": {
        "direct": {
            "elm-explorations/test": "1.0.0",
            "simonh1000/elm-jwt": "6.0.0"
        },
        "indirect": {
            "elm/random": "1.0.0",
            "elm/regex": "1.0.0",
            "truqu/elm-base64": "2.0.4"
        }
    }
}"""
