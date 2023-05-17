module PhoneBook

open System.IO

type PhoneRecord = { Name: string; Phone: string }

type PhoneBook = PhoneRecord list

let addRecord name phone phoneBook =
    List.rev ({ Name = name; Phone = phone } :: (List.rev phoneBook))

let findPhoneByName name phoneBook =
    phoneBook
    |> List.tryFind (fun record -> record.Name = name)
    |> Option.map (fun record -> record.Phone)

let findNameByPhone phone phoneBook =
    phoneBook
    |> List.tryFind (fun record -> record.Phone = phone)
    |> Option.map (fun record -> record.Name)

let saveToFile (fileName: string) phoneBook =
    use writer = new StreamWriter(fileName)

    phoneBook
    |> List.iter (fun record -> writer.WriteLine($"{record.Name}:{record.Phone}"))

let loadFromFile (fileName: string) =
    use reader = new StreamReader(fileName)

    Seq.unfold
        (fun () ->
            match reader.ReadLine() with
            | null -> None
            | line ->
                match line.Split(':') with
                | [| name; phone |] -> Some({ Name = name; Phone = phone }, ())
                | _ -> failwith "Некорректный формат файла")
        ()
    |> List.ofSeq
