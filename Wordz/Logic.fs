module Logic

open System.Drawing
open System.Drawing.Imaging
    
let getColors (bitmap:Bitmap) =
    Array2D.init bitmap.Width bitmap.Height (fun x y -> bitmap.GetPixel(x, y))

type TextCandidate = {
    Text: string
    FontSize: int
    Width: int
    Height: int
    Offset: int * int
    Pixels: bool[,]
}

let getFont size = new Font("Arial", float32 size)

let buildTestCandidates word fontSizes =
    use bit = new Bitmap(1, 1)
    use measureGraphics = bit |> Graphics.FromImage
    measureGraphics.TextRenderingHint <- Text.TextRenderingHint.AntiAlias

    let buildTestCandidate margin fontSize =
        use font = getFont fontSize
        let size = measureGraphics.MeasureString(word, font)
        let width, height = int size.Width, int size.Height
        use bitmap = new Bitmap(width, height)
        use g = bitmap |> Graphics.FromImage
        g.TextRenderingHint <- Text.TextRenderingHint.AntiAlias
        g.DrawString(word, font, Brushes.Black, 0.f, 0.f)

        let pixels = Array2D.init width height (fun x y -> bitmap.GetPixel(x, y).A > 15uy)

        let keepMargin margin whiteSpace =
            max (whiteSpace - margin) 0

        let startOffsetX =
            [0 .. width - 1]
            |> Seq.takeWhile(fun x -> pixels.[x, 0 .. height - 1] |> Array.forall ((=) false))
            |> Seq.length
            |> keepMargin margin

        let trimmedEndColumnsCount =
            [0 .. width - 1]
            |> Seq.rev
            |> Seq.takeWhile(fun x -> pixels.[x, 0 .. height - 1] |> Array.forall ((=) false))
            |> Seq.length
            |> keepMargin margin

        let startOffsetY =
            [0 .. height - 1]
            |> Seq.takeWhile(fun y -> pixels.[0 .. width - 1, y] |> Array.forall ((=) false))
            |> Seq.length
            |> keepMargin margin

        let trimmedEndLinesCount =
            [0 .. height - 1]
            |> Seq.rev
            |> Seq.takeWhile(fun y -> pixels.[0 .. width - 1, y] |> Array.forall ((=) false))
            |> Seq.length
            |> keepMargin margin

        let trimmedPixels =
            pixels.[startOffsetX .. width - 1- trimmedEndColumnsCount, startOffsetY .. height - 1 - trimmedEndLinesCount]

        {
            Text = word
            FontSize = fontSize
            Width = width - startOffsetX - trimmedEndColumnsCount
            Height = height - startOffsetY - trimmedEndLinesCount
            Offset = startOffsetX, startOffsetY
            Pixels = trimmedPixels
        }

    fontSizes |> List.map (buildTestCandidate 1)
    
type Spot = {
    X:int
    Y:int
    TextCandidate: TextCandidate
}

type Boundaries = {
    Width: int
    Height: int
    AvailableRight: int [,]
    Coords: (int*int) []}

let groupConsecutive input = seq {
        let mutable currentGroup : option<int * int * _>  = None
        for elem in input do
            match currentGroup with
            | None ->
                currentGroup <- Some (0, 1, elem)
            | Some (start, n, value) ->
                if value = elem then
                    currentGroup <- Some (start, n+1, value)
                else
                    yield start, n, value
                    currentGroup <- Some (start+n, 1, elem)

        match currentGroup with
        | None -> ()
        | Some g -> yield g
    }

let countFalse repetitions = seq {
    for (start, count, value) in repetitions do
        if value then
            for i in count .. -1 .. 1 do yield 0
        else
            for i in count .. -1 .. 1 do yield i
    }

let indexesOfNextTrueAndFalse repetitions = seq {
    for (start, count, value) in repetitions do
        for i in start..start+count-1 do
            if value then
                yield (i, start+count)
            else
                yield (start+count, i)
    }

let updateBoundaries (forbiddenPixels:bool[,]) ((minX, maxX), (minY, maxY)) boundaries =
    [|
        for y in minY .. maxY do
            yield async {
                    let falseCountsOnLine =
                        seq { for x in 0 .. boundaries.Width - 1 do yield forbiddenPixels.[x, y] }
                        |> groupConsecutive
                        |> countFalse

                    let mutable x = 0
                    for count in falseCountsOnLine do
                        boundaries.AvailableRight.[x,y] <- count
                        x <- x + 1
            }
    |]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

    boundaries

let coords (arr: 'a[,]) = seq {
    let width = arr.GetLength(0);
    let height = arr.GetLength(1); 
    for x in 0..width-1 do
        for y in 0..height-1 do
            yield x,y
}

let getBoundaries (forbiddenPixels:bool[,]) = 
    let width = forbiddenPixels.GetLength(0)
    let height = forbiddenPixels.GetLength(1)
    let availableRight = Array2D.zeroCreate width height
    {
        Width = width
        Height = height
        AvailableRight = availableRight
        Coords = coords availableRight |> Array.ofSeq
    } |> updateBoundaries forbiddenPixels ((0, width - 1), (0, height - 1))

type AddingState = {
     ForbiddenPixels: bool[,]
     Boundaries: Boundaries
     WordsSpots: Spot list
     RemainingWords: (string * TextCandidate list) list
     NextIterationWords: (string * TextCandidate list) list
}

let findSpot boundaries (width, height) =
    let rec spotOk (x,y) remaining =
        match remaining with
        | 0 -> true
        | remaining ->
            let available = boundaries.AvailableRight.[x, y]
            if available >= width then
                spotOk (x, y+1) (remaining-1)
            else
                false

    let mutable x = 0
    let mutable y = 0
    let mutable found = false
    while y < boundaries.Height && not found do
        if spotOk (x,y) height then
            found <- true
        else
            if x < boundaries.Width-1 then
                x <- x + 1
            else
                y <- y + 1
                x <- 0

    if found then
        Some (x,y)
    else
        None

let addWord (targetColors: Color[,]) (state:AddingState) =
    let word, textCandidates = state.RemainingWords.Head

    let totalCandidatesCount = (state.RemainingWords @ state.NextIterationWords) |> List.collect snd |> List.length
    
    printfn "%s, remaining candidates = %d" word totalCandidatesCount
    
    let boundaries = state.Boundaries

    let spots = seq {
        for textCandidate in textCandidates do
            match findSpot boundaries (textCandidate.Width, textCandidate.Height) with
            | Some (x, y) ->
                yield {
                    X = x
                    Y = y
                    TextCandidate = textCandidate
                }
            | None -> ()
        }
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let bestSpot = spots |> Seq.tryHead
    printfn "Spot computation: %O" sw.Elapsed 

    let newState =
        match bestSpot with
        | Some spot ->
            for x in 0 .. spot.TextCandidate.Width - 1 do
                for y in 0 .. spot.TextCandidate.Height - 1 do
                    state.ForbiddenPixels.[spot.X + x, spot.Y + y] <- state.ForbiddenPixels.[spot.X + x, spot.Y + y] || spot.TextCandidate.Pixels.[x, y]

            let remainingCandidates = textCandidates |> List.skipWhile (fun c -> c <> spot.TextCandidate)
            let w = System.Diagnostics.Stopwatch.StartNew()
            let updatedBoundaries = state.Boundaries |> updateBoundaries state.ForbiddenPixels ((spot.X, spot.X + spot.TextCandidate.Width - 1), (spot.Y, spot.Y + spot.TextCandidate.Height - 1))
            printfn "Boundaries: %O" w.Elapsed 
            
            {
                ForbiddenPixels = state.ForbiddenPixels
                Boundaries = updatedBoundaries
                WordsSpots = spot :: state.WordsSpots
                RemainingWords = state.RemainingWords.Tail
                NextIterationWords = (word, remainingCandidates) :: state.NextIterationWords
            }

        | None ->
            { state with RemainingWords = state.RemainingWords.Tail }

    newState

let rand =
    let r = new System.Random(42)
    fun () -> r.NextDouble()

let rec addWords targetColors (state:AddingState) =
    match state.RemainingWords, state.NextIterationWords with
    | [], [] -> state
    | [], nextIterationWords ->
        let remainingWords = nextIterationWords |> List.sortBy (fun _ -> rand())
        let state' = { state with RemainingWords = remainingWords
                                  NextIterationWords = [] }
        addWords targetColors state'
    | _ ->
        let w = System.Diagnostics.Stopwatch.StartNew ()
        let state' = addWord targetColors state
        printfn "State evol : %O" w.Elapsed
        addWords targetColors state'

let generate (inputFolder:string, outputFolder:string) (inputFile, words) =

    let target = Bitmap.FromFile(inputFolder + inputFile) :?> Bitmap
    let targetColors = getColors target
    let empty = new Bitmap(target.Width, target.Height)
    let result =
        let forbiddenPixels = Array2D.init target.Width target.Height (fun x y -> targetColors.[x, y].A < 15uy)
        let startingState =
            {
                ForbiddenPixels = forbiddenPixels
                Boundaries = getBoundaries forbiddenPixels
                WordsSpots = []
                RemainingWords = []
                NextIterationWords = words
            }

        let finalState = addWords targetColors startingState

        let result = new Bitmap(target.Width, target.Height)
        use g = Graphics.FromImage result
        g.TextRenderingHint <- Text.TextRenderingHint.AntiAlias

        for spot in finalState.WordsSpots do
            let color = targetColors.[spot.X + spot.TextCandidate.Width / 2, spot.Y + spot.TextCandidate.Height / 2]
            use font = getFont spot.TextCandidate.FontSize
            use brush = new SolidBrush(color)
            g.DrawString(spot.TextCandidate.Text, font, brush, single (spot.X - fst spot.TextCandidate.Offset), single (spot.Y - snd spot.TextCandidate.Offset))

        result
        
    result.Save(outputFolder + inputFile, ImageFormat.Png)
