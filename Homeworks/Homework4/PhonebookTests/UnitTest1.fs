open NUnit.Framework
open FsUnit

open System.IO
open PhoneBook

let testPhoneBook () =
    [ { Name = "Иванов"; Phone = "111-11-11" }
      { Name = "Петров"; Phone = "222-22-22" }
      { Name = "Сидоров"
        Phone = "333-33-33" } ]


[<Test>]
let ``Add record to phone book`` () =
    let name = "Новиков"
    let phone = "444-44-44"
    let phoneBook = testPhoneBook ()
    let newPhoneBook = addRecord name phone phoneBook
    newPhoneBook |> List.length |> should equal (4)
    newPhoneBook.[3].Name |> should equal "Новиков"

[<Test>]
let ``Find phone by name`` () =
    let name = "Петров"
    let phoneBook = testPhoneBook ()
    let phone = findPhoneByName name phoneBook
    phone |> should equal (Some "222-22-22")

[<Test>]
let ``Find name by phone`` () =
    let phone = "333-33-33"
    let phoneBook = testPhoneBook ()
    let name = findNameByPhone phone phoneBook
    name |> should equal (Some "Сидоров")

[<Test>]
let ``Get all records`` () =
    let records = testPhoneBook ()
    records |> List.length |> should equal (3)
    records.[2].Name |> should equal "Сидоров"
    records.[1].Name |> should equal "Петров"
    records.[0].Name |> should equal "Иванов"

[<Test>]
let ``Save and load phone book`` () =
    let fileName = "test_phone_book.csv"
    let phoneBook = testPhoneBook ()
    saveToFile fileName phoneBook
    let loadedPhoneBook = loadFromFile fileName
    loadedPhoneBook |> should equal phoneBook
    File.Delete(fileName)
