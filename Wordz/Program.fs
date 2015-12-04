let coolWords =
    [
        "FSharp"; "Paris"; "dojo"; "kata"; "Alice"; "Wonderland"; "code";
        "cards"; "doublets"; "alphabet"; "cypher"; "number"; "game"
        "@pirrmann"; "@brandewinder"; "@thinkb4coding"; "computation"; "type";
        "fun"; "async"; "compose"; ">>"; "|>"; "tuple"; "union"; "record"
        "magic"; "square"; "fox"; "goose"; "bag"; "corn"; "love"; "skills"
        "functions"; "map"; "filter"; "choose"; "let"; "rec"; "seq"
    ]

let readWordsFromFile filePath =
    System.IO.File.ReadAllLines(filePath) |> Seq.toList

let fontSizes = [28 .. -4 .. 24] @ [22 .. -2 .. 12] @ [11 .. -1 .. 2]

let buildWordSet wordsToUse =
    wordsToUse |> List.map (fun word -> word, Logic.buildTestCandidates word fontSizes)

let inputFolder = @"../../../"
let outputFolder = @"/Users/fox/Code/"

let inputFiles =
    [
        "Sample.png"
    ]

let words =
    [
        //readWordsFromFile @"/Users/fox/Google Drive/Rfq-hub/Picts/Dog.png" 
        [ "WoW"; "Such"; "Much"; "Code" ]
    ] |> List.collect buildWordSet

[<EntryPoint>]
let main argv =
    printfn "Starting !"

    let sw = System.Diagnostics.Stopwatch.StartNew()
    let tasks =
        [|
            for inputFile in inputFiles do
                yield async { Logic.generate (inputFolder, outputFolder) (inputFile, words) }
        |]
        |> Async.Parallel
        |> Async.RunSynchronously
    printfn "Elapsed : %O" sw.Elapsed

    0
