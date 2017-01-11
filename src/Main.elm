module Main exposing (..)

import Html
import Html.Attributes exposing (rows, cols)


type alias UnoProgram = 
    { using : List String   
    , class : UnoClass
    , errors : List String
    }

initProgram : UnoProgram
initProgram =
    { using = []
    , class =
        { name = "Init"
        , functions = []
        }
    , errors = []
    }

type alias UnoClass = 
    { name : String 
    , functions : List Function 
    }

initFunction : Function
initFunction =
    { name = ""
    , body = ""
    , args = []
    , modifiers = []
    }

type alias Function =
    { name : String 
    , body : String
    , args : List String
    , modifiers : List String    
    }

usingToJs : List String -> List String 
usingToJs usings =
    List.map (\using -> "require('" ++ using ++ "');") usings
        

functionToJs : Function -> List String 
functionToJs function =
    let 
        joinedArgs =
            String.join ", " function.args

        body = 
            function.body 
                |> String.split ";"
                |> String.join ";\n"
    in 
        [ "function " ++ function.name ++ "(" ++ joinedArgs ++ ") {"
        , body
        , "}"
        ]


classToJs : UnoClass -> List String 
classToJs class =
    let 
        indentedFunctions = 
            List.map functionToJs class.functions
                |> List.concat
                |> List.map (\s -> "    " ++ s)
    in 
        [ "function " ++ class.name ++ "() {" ]
        ++ indentedFunctions ++
        [ "}"
        ] 


programToJs : UnoProgram -> String 
programToJs program = 
    [ usingToJs program.using
    , classToJs program.class
    ]
        |> List.concat
        |> String.join "\n"


example : String
example = """
using Uno;

class MainView
{
    public MainView() {
        debug_log("App init!");
        InitializeUX();
    }
}
"""


-- Pre parse, clean the whitespace
-- leave gaps of one
removeWhitespace : String -> String
removeWhitespace =
    String.words 
        >> String.join " "


{-| 
    >>> parseAUsing "using Uno; using Fish;"
    Just (0, 10)

    >>> parseAUsing "using Fish;"
    Just (0, 11)

    >>> parseAUsing "public MainView()"
    Nothing
-}
parseAUsing : String -> Maybe (Int, Int)
parseAUsing text =
    case (String.indexes "using " text, String.indexes ";" text) of 
        ([], _) -> Nothing
        (_, []) -> Nothing
        (usingStart::_, semiColonStart::_) -> 
            if semiColonStart < usingStart then 
                Nothing 
            else 
                Just (usingStart, semiColonStart)


takeUsing : String -> List String -> (String, List String)
takeUsing program usingsFound = 
    case parseAUsing program of 
        Nothing -> (program, usingsFound)
        Just (usingStart, semiColonStart) ->
            let 
                withFirstUsingDropped = 
                    String.dropLeft (semiColonStart - usingStart + 1) program

                statement = 
                    String.left (semiColonStart - usingStart) program
                        |> String.dropLeft ((String.length "using") + 1)
            in
                takeUsing withFirstUsingDropped (statement :: usingsFound)

{-| Find all the using cases in a string and inject them into the program
    Returns the original text with the usings removed and the program itself
-}
parseUsing : UnoProgram -> String -> (String, UnoProgram)
parseUsing program programText =
    let
        (remains, usings) = 
            takeUsing programText []
    in
        (remains, { program | using = usings })

programError : UnoProgram -> String -> UnoProgram
programError program error =
    { program | errors = program.errors ++ [error] }

{-| Grab the class name, inject it into the program
-}
parseClass : UnoProgram -> String -> (String, UnoProgram)
parseClass program programText =
    case String.split "class " programText of
        [classBits, rest] ->
            -- TODO: pull out modifiers for the class
            case String.indexes "{" rest of
                [] ->
                    (programText, programError program "No class found!")
                firstBracket::_ ->
                    let 
                        className =
                            String.slice 0 firstBracket rest 
                                |> String.trim

                        oldClass = program.class

                        class =
                            { oldClass | name = className }
                    in
                        ( getContext programText, { program | class = class })
        [] -> 
            (programText, programError program "No class found!")
        [x] -> 
            (programText, programError program "No class found!")
        _ -> 
            (programText, programError program "Too many classes found!")




createFunction : Int -> String -> (Int, Maybe Function)
createFunction bracketStart text = 
    let
        -- grab the start of the text until the start of the args
        functionHead = 
            String.slice 0 bracketStart text 
                |> String.words

        -- assume the name is the last word
        name = 
            functionHead
                |> List.reverse 
                |> List.head 
                |> Maybe.withDefault ""

        -- and the modifiers are the rest
        modifiers = 
            functionHead 
                |> List.reverse
                |> List.drop 1
                |> List.reverse


        body = 
            text 
                |> String.slice bracketStart -1 
                |> getContext
    in 
        case String.indexes ")" text of
            firstClose::_ ->
                let 
                    args = 
                        String.slice (bracketStart + 1) firstClose text 
                            |> String.split ","
                            |> List.map String.trim 
                            |> List.filter ((/=) "")
                in 
                    ( firstClose
                    , Just 
                        { initFunction 
                        | args = args
                        , body = body
                        , name = name 
                        , modifiers = modifiers
                        }
                    )

            _ ->
                (-1, Nothing )

parseFunction : UnoProgram -> String -> (String, UnoProgram)
parseFunction program text =
    case (String.indexes "(" text, List.reverse <| String.indexes "}" text) of 
        (firstOpen::_, lastClose::_) -> 
            let 
                (endIndex, newFunction) = 
                    String.slice 0 lastClose text
                        |> createFunction firstOpen
            in
                case newFunction of 
                    Nothing  ->
                        (text, program)
                            |> Debug.log "not found"
                    Just func ->
                        let
                            class = 
                                program.class  
                        in 
                            ( text, { program | class = { class | functions = class.functions ++ [ func ] } })

        _ -> 
            (text, program)
                |> Debug.log "didnt"


{-|
    >>> getContext "{ hello } "
    "hello"
    >>> getContext "{ public Main() { } }"
    "public Main() { }"
-}
getContext : String -> String 
getContext text = 
    case (String.indexes "{" text, List.reverse <| String.indexes "}" text) of
        (firstOpen::_, lastClose::_) ->
            String.slice (firstOpen + 1) lastClose text
                |> String.trim
        (_, _) -> text 

    
{-| run the next parser on the string with the program for context
-}
next : (UnoProgram -> String -> (String, UnoProgram)) -> (String, UnoProgram) -> (String, UnoProgram)
next fn (text, program) =
    fn program text


parse : String -> ( String, UnoProgram )
parse =
    removeWhitespace 
        >> parseUsing initProgram
        >> next parseClass
        >> next parseFunction


viewError : String -> Html.Html msg 
viewError errorMessage =
    Html.li 
        [] 
        [ Html.text errorMessage ]


viewErrors : UnoProgram -> Html.Html msg 
viewErrors program =  
    case program.errors of 
        [] -> 
            Html.div 
                [] 
                [ Html.text "No errors found." ]

        errors ->
            List.map viewError errors
                |> Html.ul []

viewProgram : UnoProgram -> Html.Html msg 
viewProgram program =
    Html.div 
        []
        [ Html.div 
            [] 
            [ Html.text <| "Using modules: " ++ (String.join ", " program.using)
            ]
        , Html.div 
            [] 
            [ Html.text <| "Name: " ++ program.class.name
            ]
        , Html.div 
            []
            [ Html.text <| "Functions found: " ++ (String.join ", " <| List.map toString program.class.functions)
            ]
        , viewErrors program
        ]

main = 
    let 
        (leftOvers, program) = parse example
    in  
        Html.div 
            [] 
            [ Html.textarea [ rows 50, cols 50 ] [ Html.text example ]
            , Html.textarea [ rows 50, cols 50 ] [ Html.text <| programToJs program ]
            , viewProgram <| program
            ]