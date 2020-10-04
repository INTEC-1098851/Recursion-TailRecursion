// Learn more about F# at http://fsharp.org

open System


// RECURSIVIDAD
// FACTORIAL DE UN NUMERO MEDIANTE RECURSIVIDAD
let rec recFactorial n = 
    if n<= 1 then
        1
    else
        recFactorial(n-1) * n

// SUMA DE LOS NUMEROS ANTERIORES
let rec recSum n =
    if n<= 1 then
        n
    else
        recSum(n-1) + n

// TAIL RECURSION
// FACTORIAL DE UN NUMERO CON TAIL RECURSION
let tail_factorial n =
    let rec accFactorial n acc = 
        if n <=1 then
            acc
        else
            accFactorial (n-1) (acc * n)
    accFactorial n 1

// SUMA DE LOS NUMEROS ANTERIORES CON TAIL RECURSION
let tail_sum n =
    let rec accSum n acc =
        if n <= 1 then  
            acc
        else
            accSum (n-1) (acc + n)
    accSum n 1

// MAXIMO COMUN DIVISOR CON TAIL RECURSION
let rec tail_GCD n1 n2 =
    if n2 <= 0 then
        n1
    else
        tail_GCD n2 (n1 % n2)

// VERSIONES ITERATIVAS
// FACTORIAL DE UN NUMERO ITERATIVAMENTE
let itFactorial n =
    let mutable fact = 1
    if n < 2 then
        n
    else
        for i in n .. -1 .. 1 do
            fact <- fact * i
        fact

// SUMA DE LOS NUMEROS ANTERIORES ITERATIVAMENTE
let itSum n =
    let mutable sum = 0
    if n < 2 then
        n
    else
        for i in n .. -1 .. 0 do
            sum <- sum + i
        sum
// MAXIMO COMUN DIVISOR ITERATIVAMENTE
let itGCD n1 n2 =
    let mutable num1 = n1
    let mutable num2 = n2
    let mutable modulo = n1 % n2
    while modulo > 0 do
        num1 <- num2
        num2 <- modulo
        modulo <- num1 % num2
    num2
[<EntryPoint>]
let main argv =

    printfn "Factorial de un numero"
    let recursiveFactorial = recFactorial 6
    printfn "Con Recursion: %i" recursiveFactorial
    let tailRecursiveFacotrial = tail_factorial 6
    printfn "Con Tail Recursion: %i" recursiveFactorial
    let iterativeFactorial = itFactorial 6
    printfn "Con Iteracion: %i" iterativeFactorial
    printfn ""

    printfn "Suma de los numeros anteriores"
    let recursiveSum = recSum 6
    printfn "Con Recursion: %i" recursiveSum
    let tailRecursiveSum = tail_sum 6
    printfn "Con Tail Recursion: %i" tailRecursiveSum
    let iterativeSum = itSum 6
    printfn "Con Iteracion: %i" iterativeSum
    printfn ""

    printfn "Maximo Comun Divisor"
    let tailRecursiveGCD = tail_GCD 48 60
    printfn "Con Tail Recursion: %i" tailRecursiveGCD
    let iterativeGCD = itGCD 48 60
    printfn "Con Iteracion: %i" iterativeGCD
    printfn ""


    0 // return an integer exit code
