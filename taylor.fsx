open System

let builtInFunc x = exp(2.0 * x)

let a = 0.1
let b = 0.6
let n = 10
let eps = 0.000000001

let rec fact n =
    if n <= 1 then 1.0
    else float n * fact (n - 1)


let taylorNaive x =
    let rec naive k acc term =
        if abs(term) < eps then acc, k
        else
            let term = (2.0 * x) ** float k / fact k
            naive (k + 1) (acc + term) term
    naive 1 1.0 1.0

let taylorSmart x =
    let rec smart k acc term =
        if abs(term) < eps then acc, k
        else
            let term = term * (2.0 * x) / float (k + 1)
            smart (k + 1) (acc + term) term
    smart 0 1.0 1.0 

printfn "|   x   |   Builtin  | Smart Taylor | #terms  | Naive Taylor | #terms |"
printfn "-----------------------------------------------------------------------"
let main =
    for i = 0 to n do
        let x = a + (float i) / float n * (b - a)
        let smartVal, termsSmart = taylorSmart x
        let naiveVal, termsNaive = taylorNaive x
        printfn "| %5.2f |  %-10.6f|   %-10.6f |    %d   |  %-10.6f  |    %d   |" 
            x (builtInFunc x) smartVal termsSmart naiveVal termsNaive

main
