// Learn more about F# at http://fsharp.org

open System


// RECURSIVIDAD
// FACTORIAL DE UN NUMERO MEDIANTE RECURSIVIDAD
let rec recFactorial (n:double) :double = 
    if n<= double(1) then
        double(1)
    else
        recFactorial(n- double(1)) * double(n)

// SUMA DE LOS NUMEROS ANTERIORES
let rec recSum (n:double):double =
    if n<= double(1) then
        double(n)
    else
        recSum(n- double(1)) + double(n)

// TAIL RECURSION
// FACTORIAL DE UN NUMERO CON TAIL RECURSION
let tail_factorial n :double =
    let rec accFactorial n acc :double =
        if n <= double(1) then  
            acc
        else
            accFactorial (n - double(1)) (acc * n)
    accFactorial n (double(1))

// SUMA DE LOS NUMEROS ANTERIORES CON TAIL RECURSION
let tail_sum (n:double):double =
    let rec accSum (n:double) (acc:double) :double =
        if n <= double(1) then  
            double(acc)
        else
            accSum (n- double(1)) (acc + n)
    accSum (double(n)) (double(1))

// MAXIMO COMUN DIVISOR CON TAIL RECURSION
let rec tail_GCD (n1:double) (n2:double) :double =
    if n2 <= double(0) then
        n1
    else
        tail_GCD (double(n2)) (n1 % n2)

// VERSIONES ITERATIVAS
// FACTORIAL DE UN NUMERO ITERATIVAMENTE
let itFactorial (n:double):double =
    let mutable fact:double = double(1)
    if n < double(2) then
        double(n)
    else
        for i in n .. double(-1) .. double(1) do
            fact <- fact * (double(i))
        double(fact)

// SUMA DE LOS NUMEROS ANTERIORES ITERATIVAMENTE
let itSum (n:double) :double =
    let mutable sum:double = double(0)
    if n < double(2) then
        double(n)
    else
        for i in n .. double(-1) .. double(0) do
            sum <- sum + double(i)
        double(sum)
// MAXIMO COMUN DIVISOR ITERATIVAMENTE
let itGCD n1 n2 :double =
    let mutable num1 = n1:double
    let mutable num2 = n2:double
    let mutable modulo = (n1 % n2):double
    while modulo > double(0) do
        num1 <- num2
        num2 <- modulo
        modulo <- num1 % num2
    double(num2)
[<EntryPoint>]
let main argv = 
    let recursiveFactorial:double = recFactorial (double(6))
    printfn "Con Recursion: %f" recursiveFactorial
    let tailRecursiveFacotrial:double = tail_factorial (double(100000))
    printfn "Con Tail Recursion: %f" tailRecursiveFacotrial
    let iterativeFactorial:double = itFactorial (double(6))
    printfn "Con Iteracion: %f" iterativeFactorial
    printfn ""

    printfn "Suma de los numeros anteriores"
    let recursiveSum:double = recSum (double(6))
    printfn "Con Recursion: %f" recursiveSum
    let tailRecursiveSum:double = tail_sum (double(6))
    printfn "Con Tail Recursion: %f" tailRecursiveSum
    let iterativeSum:double = itSum (double(6))
    printfn "Con Iteracion: %f" iterativeSum
    printfn ""

    printfn "Maximo Comun Divisor"
    let tailRecursiveGCD:double = tail_GCD (double(48)) (double(60))
    printfn "Con Tail Recursion: %f" tailRecursiveGCD
    let iterativeGCD:double = itGCD (double(48)) (double(60))
    printfn "Con Iteracion: %f" iterativeGCD
    printfn ""
    
    0 
