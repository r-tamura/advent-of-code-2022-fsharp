open Performance


type Entry =
    | Directory of Directory
    | File of File

and File = { name: string; size: uint64 }

and Directory =
    { name: string
      mutable children: Map<string, Entry> }

module Entry =
    let find (path: string) (root: Entry) =
        let rec find' path pwd =
            match path with
            | [] -> pwd
            | head :: rest ->
                match pwd with
                | Entry.File _ -> failwith $"entry '{path}' not found"
                | Entry.Directory({ name = name; children = children }) ->
                    try
                        find' rest (Map.find head children)
                    with :? System.Collections.Generic.KeyNotFoundException as e ->
                        printfn "%s not found" head
                        raise e

        let pathList =
            match path.Split("/") |> List.ofArray with
            | [] -> failwith $"entry '{path}' not found"
            | path -> path |> List.tail

        find' pathList root

    let add dirPath name entry tree =
        let dir = find dirPath tree

        match dir with
        | Entry.File _ -> failwith $"entry '{dirPath}' not a direcotry"
        | Entry.Directory dir -> dir.children <- Map.add name entry dir.children

        tree

    let rec sumSizes (entry: Entry) : uint64 =
        match entry with
        | Entry.File { size = size } -> size
        | Entry.Directory { children = children } -> Map.values children |> Seq.sumBy sumSizes

    let rec findDirectoriesBy (pred: string -> uint64 -> bool) (pwd: string) entry : (string * uint64) list =
        match entry with
        | File _ -> []
        | Directory { name = name; children = children } ->
            let size = sumSizes entry

            let dirPath =
                match name with
                | "/" -> name
                | _ -> System.IO.Path.Combine(pwd, name)

            let smallDirectries =
                List.collect (findDirectoriesBy pred dirPath) (children |> Map.values |> List.ofSeq)

            if pred dirPath size then
                [ dirPath, size ] @ smallDirectries
            else
                smallDirectries


    let print (root: Entry) =
        let getIndent level =
            Array.create (level * 2) " " |> System.String.Concat

        let rec print' level path =
            let entry = find path root
            let indent = Array.create (level * 2) " " |> System.String.Concat

            match entry with
            | Entry.File { name = name; size = size } -> printfn "%s- %s (file, size=%d)" indent name size
            | Entry.Directory { name = name; children = children } ->
                printfn "%s- %s (dir)" indent name

                match path with
                | "/" -> Map.iter (fun name e -> print' (level + 1) (path + name)) children
                | _ -> Map.iter (fun name e -> print' (level + 1) (path + "/" + name)) children

        print' 0 "/"


type Command =
    | Cd of string
    | Ls

type Line =
    | Command of Command
    | Directory of string
    | File of uint64 * string

let tokenizeLine (line: string) : Line option =
    match line.Split(" ") with
    | [| "$"; "cd"; dir |] -> Some(Line.Command(Cd dir))
    | [| "$"; "ls" |] -> Some(Line.Command Ls)
    | [| "dir"; dir |] -> Some(Line.Directory dir)
    | [| size; file |] -> Some(Line.File(uint64 size, file))
    | _ -> None


let buildDirectoryTree (lines: Line[]) =
    let gotoParent (p: string) =
        p.Split("/") |> (fun path -> path[0 .. path.Length - 2]) |> String.concat "/"

    let folder (pwd, tree: Entry) line =
        match line with
        | Command(Cd "/") -> (pwd, tree)
        | Command(Cd "..") -> (gotoParent pwd, tree)
        | Command(Cd dir) -> (pwd + "/" + dir, tree)
        | Directory(dir) ->
            let newDir = Entry.Directory { name = dir; children = Map [] }
            (pwd, (Entry.add pwd dir newDir tree))
        | File(size, name) ->
            let newFile = Entry.File { name = name; size = size }
            (pwd, (Entry.add pwd name newFile tree))
        | _ -> (pwd, tree)

    let (pwd: string, tree: Entry) =
        lines
        |> Array.fold folder ("", Entry.Directory { name = "/"; children = Map [] })

    tree


let parseInput lines =
    lines |> Array.map tokenizeLine |> Array.choose id

let part1 lines =
    lines
    |> parseInput
    |> buildDirectoryTree
    |> Entry.findDirectoriesBy (fun dirPath size -> size > 100000UL) "/"
    |> printfn "%A"

let FILESYSTEM_STORAGE_SIZE = 70000000UL
let UPDATE_REQUIREMENTS_SIZE = 30000000UL

let part2 liens =
    let root = liens |> parseInput |> buildDirectoryTree
    let usedSpace = root |> Entry.sumSizes
    let unsedSpace = FILESYSTEM_STORAGE_SIZE - usedSpace
    let neededSpace = UPDATE_REQUIREMENTS_SIZE - unsedSpace

    root
    |> Entry.findDirectoriesBy (fun _ size -> size > neededSpace) "/"
    |> List.sortBy (fun (_, size) -> size)
    |> List.head
    ||> (fun _ size -> size)
    |> printfn "%u"



[<EntryPoint>]
let main argv =
    let filepath, part =
        match argv with
        | [| filepath; part |] -> (filepath, part)
        | [| filepath |] -> (filepath, "1")
        | _ -> failwith "usage: dotnet run <filepath> <part>"

    timer {
        let lines = System.IO.File.ReadAllLines filepath

        match part with
        | "1" -> part1 lines
        | "2" -> part2 lines
        | _ -> failwith """part is " 1 " or " 2 """
    }

    0
