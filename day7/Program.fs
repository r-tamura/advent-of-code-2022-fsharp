open Performance

type T = int * int

let T (age, _) = T(423, 40)


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

    let rec findSmallDirectries entry : (Directory * uint64) list =
        match entry with
        | File _ -> []
        | Directory d ->
            let size = sumSizes entry

            let smallDirectries =
                List.collect findSmallDirectries (d.children |> Map.values |> List.ofSeq)

            if size <= 100000uL then
                [ d, size ] @ smallDirectries
            else
                smallDirectries


    let print (root: Entry) =
        let getIndent level =
            Array.create (level * 2) " " |> System.String.Concat

        let rec print' level path =
            let entry = find path root

            if path = "" then
                match entry with
                | Entry.File _ -> failwith "root shoud not be a file"
                | Entry.Directory { name = name
                                    children = children: Map<string, Entry> } ->
                    printfn "/"
                    Map.iter (fun name e -> print' (level + 1) (path + "/" + name)) children
            else
                match entry with
                | Entry.File { name = name; size = size } ->
                    let indent = getIndent level
                    printfn "%s- %s (file, size=%d)" indent name size
                | Entry.Directory { name = name; children = children } ->
                    let indent = Array.create (level * 2) " " |> System.String.Concat
                    printfn "%s- %s (dir)" indent name
                    Map.iter (fun name e -> print' (level + 1) (path + "/" + name)) children

        print' 0 ""


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
    |> Entry.findSmallDirectries
    |> List.map (fun (e, size) -> (e.name, size))
    |> List.sumBy (fun (name, size) -> size)
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
        | _ -> failwith """part is " 1 " or " 2 """
    }

    0
