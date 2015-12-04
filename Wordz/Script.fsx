type Boundaries = {
    Width: int
    Height: int
    AvailableRight: int [,]}

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
        yield async {
            for y in minY .. maxY do
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

let getBoundaries (forbiddenPixels:bool[,]) = 
    let width = forbiddenPixels.GetLength(0)
    let height = forbiddenPixels.GetLength(1)

    {
        Width = width
        Height = height
        AvailableRight = Array2D.zeroCreate width height
        AvailableBottom = Array2D.zeroCreate width height
    } |> updateBoundaries forbiddenPixels ((0, width - 1), (0, height - 1))

let input = [|true;true;false;false;false;true|]
let groupped = input |> groupConsecutive
let counted = groupped |> countFalse

counted |> List.ofSeq

let boundaryData = array2D
                    [
                        [|true;true;false;false;false;true|]
                        [|true;true;false;true;false;true|]
                        [|true;true;false;true;false;true|]
                    ]

let boundaries = getBoundaries boundaryData

let flatify (arr: 'a[,]) = seq {
    let width = arr.GetLength(0);
    let height = arr.GetLength(1); 
    for x in 0..width-1 do
        for y in 0..height-1 do
            yield (x,y), arr.[x,y]
}

let coords (arr: 'a[,]) = seq {
    let width = arr.GetLength(0);
    let height = arr.GetLength(1); 
    for x in 0..width-1 do
        for y in 0..height-1 do
            yield x,y
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
        printfn "checking %i, %i" x y
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

findSpot boundaries (1,1)