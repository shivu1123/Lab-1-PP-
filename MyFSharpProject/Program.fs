// Tail Recursion 1: Product of all elements in a list
let productOfList (lst: int list) : int =
    let rec tailRecProduct (lst: int list) (acc: int) : int =
        match lst with
        | [] -> acc
        | head :: tail -> tailRecProduct tail (head * acc)
    tailRecProduct lst 1

// Test
let numbers = [2; 3; 4; 5]
printfn "Product of all elements in the list: %d" (productOfList numbers)


// Tail Recursion 2: Product of all odd numbers from a given number to 1
let productOfOddNumbers (n: int) : int =
    let rec tailRecProductOdd (n: int) (acc: int) : int =
        if n <= 0 then acc
        else tailRecProductOdd (n - 2) (n * acc)
    tailRecProductOdd n 1

// Test
let oddNumber = 11
printfn "Product of odd numbers from %d to 1: %d" oddNumber (productOfOddNumbers oddNumber)


// Using Map Function: Trim empty spaces in a list of strings
let trimNames (names: string list) : string list =
    names |> List.map (fun name -> name.Trim())

// Test
let gujaratiNames = ["  Narendra"; "Modi  "; "  Mahatma Gandhi  "; "Sardar Patel  "]
let trimmedNames = trimNames gujaratiNames
printfn "Trimmed Names: %A" trimmedNames


// Filter and Reduce: Sum of multiples of 7 and 5 in the first 700 positive integers
let sumOfMultiplesOf7And5 : int =
    [1..700]
    |> List.filter (fun n -> n % 7 = 0 && n % 5 = 0)
    |> List.fold (+) 0

// Test
printfn "Sum of multiples of 7 and 5 in the first 700 numbers: %d" sumOfMultiplesOf7And5


// Filter and Reduce: Concatenate names containing the letter 'I'
let concatenateNamesWithI (names: string list) : string =
    names
    |> List.filter (fun name -> name.IndexOf("i", System.StringComparison.OrdinalIgnoreCase) >= 0)
    |> List.reduce (+)

// Test
let gujaratiStringNames = ["Amit"; "Nitin"; "Jignesh"; "Ravi"; "Bhavin"; "Kaushik"; "Chirag"]
let concatenatedNames = concatenateNamesWithI gujaratiStringNames
printfn "Concatenated names containing the letter 'I': %s" concatenatedNames
