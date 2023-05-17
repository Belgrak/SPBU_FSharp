module H4Task3.CLI

open PhoneBook
open System

let rec interactiveLoop phonebook =
    printfn "Please select an action:"
    printfn "1. Add entry"
    printfn "2. Find phone by name"
    printfn "3. Find name by phone"
    printfn "4. Print phonebook"
    printfn "5. Save to file"
    printfn "6. Read from file"
    printfn "7. Exit"

    match Console.ReadLine() with
    | "1" ->
        printfn "Please enter name:"
        let name = Console.ReadLine()
        printfn "Please enter phone:"
        let phone = Console.ReadLine()
        interactiveLoop (addRecord name phone phonebook)
    | "2" ->
        printfn "Please enter name:"
        let name = Console.ReadLine()

        match findPhoneByName name phonebook with
        | Some phone -> printfn $"Phone for {name} is {phone}"
        | None -> printfn "Entry not found"

        interactiveLoop phonebook
    | "3" ->
        printfn "Please enter phone:"
        let phone = Console.ReadLine()

        match findNameByPhone phone phonebook with
        | Some name -> printfn $"Name for {phone} is {name}"
        | None -> printfn "Entry not found"

        interactiveLoop phonebook
    | "4" ->
        printfn "Phonebook:"
        phonebook |> List.iter (fun record -> printfn $"{record.Name}:{record.Phone}")
        interactiveLoop phonebook
    | "5" ->
        printfn "Please enter filename:"
        let filename = Console.ReadLine()
        saveToFile filename phonebook
        interactiveLoop phonebook
    | "6" ->
        printfn "Please enter filename:"
        let filename = Console.ReadLine()
        interactiveLoop (loadFromFile filename)
    | "7" -> printfn "Exiting..."
    | _ ->
        printfn "Invalid input"
        interactiveLoop phonebook

interactiveLoop []
