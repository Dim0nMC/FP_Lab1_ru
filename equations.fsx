open System
let eps = 0.00000001

let dichotomy f a b eps =
    let rec loop a b =
        let c = (a + b) / 2.0
        if abs (b - a) < eps then c
        elif f a * f c < 0.0 then loop a c
        else loop c b
    loop a b


let rec iterations phi x0 eps =
    let c = phi x0
    if (abs(x0 - c)) >= eps then
        iterations phi c eps
    else
        x0


let newton f f' x0 eps =
    let phi x = x - f x / f' x
    iterations phi x0 eps


let f1 x = 0.1 * x ** 2.0 - x * log x
let f2 x = tan x - (1.0 / 3.0) * tan(x) ** 3.0 + (1.0 / 5.0) * tan(x) ** 5.0 - 1.0 / 3.0
let f3 x = acos x - sqrt(1.0 - 0.3 * x ** 3.0)

let f1' x = 0.2 * x - log x - 1.0
let f2' x = 1.0 / cos(x) ** 2.0 - tan(x) ** 2.0 * (1.0 / cos(x) ** 2.0) * (tan(x) ** 2.0 - 2.0 / 3.0)
let f3' x = -1.0 / sqrt(1.0 - x ** 2.0) + 0.9 * x ** 2.0 / (2.0 * sqrt(1.0 - 0.3 * x ** 3.0))

let phi1 x = x - f1 x / f1' x
let phi2 x = x - f2 x / f2' x 
let phi3 x = x - f3 x / f3' x 

let main () =
    printfn "| № Eq | Dichotomy |  Iters  | Newthon |"
    printfn "----------------------------------------"
    printfn "|   1  |   %-8.4f| %7.4f | %7.4f |" (dichotomy f1 1.0 2.0 eps) (iterations phi1 1.0 eps) (newton f1 f1' 1.0 eps)
    printfn "|   2  |   %-8.4f| %7.4f | %7.4f |" (dichotomy f2 0.0 0.8 eps) (iterations phi2 0.0 eps) (newton f2 f2' 0.0 eps)
    printfn "|   3  |   %-8.4f| %7.4f | %7.4f |" (dichotomy f3 0.0 1.0 eps) (iterations phi3 0.0 eps) (newton f3 f3' 0.0 eps)

main ()
