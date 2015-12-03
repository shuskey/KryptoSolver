module FSharpKrypto.Krypto

//#region Node Definitions
let unknown x y :float = 0.0

type Node =
    | Operator of operatorFunction : (float->float->float) * operatorName : string
    | Operand of value : float
    | Empty


let myDiv (x: float) y =
    let firstTry = x / y
    if ((firstTry % 1.0) = 0.0)
    then firstTry
    else nan

let myPow (x: float) y =
    x ** y


// Defining some operators for quick reference
let noop x y = 0.0
let OPlus = Operator((+), "+")
let OMinus = Operator((-), "-")
let ODivide = Operator((myDiv), "÷")
let OTimes = Operator((*), "x")
let OPowerOf = Operator((myPow), "^")
let OMod = Operator((%), "%")
let ONone = Operator((noop), "#")

let IsMinus node =
    match node with
    | Empty -> false
    | Operator(f,s) when s = "-" -> true
    | _ -> false

let IsPlus node =
    match node with
    | Empty -> false
    | Operator(f,s) when s = "+" -> true
    | _ -> false

let IsDivide node =
    match node with
    | Empty -> false
    | Operator(f,s) when s = "÷" -> true
    | _ -> false

let IsTimes node =
    match node with
    | Empty -> false
    | Operator(f,s) when s = "x" -> true
    | _ -> false        
let getNodeString node =
    match node with
    | Empty -> ""
    | Operator(operatorName = n) -> n
    | Operand(value = v) -> sprintf " %.0f " v

let getNodeValue node =
    match node with
    | Empty -> 0.0
    | Operator(_) -> 0.0
    | Operand(value = v) -> v

let rec getNodeListString nodeList =
    match nodeList with
    | [] -> ""
    | h::t -> sprintf "%s, %s" (getNodeString h) (getNodeListString t)
//#endregion

//#region helpers
let pause() = 
    printfn "%s" "<Press any key>"
    System.Console.ReadKey() |> ignore
//#endregion

//#region getPermutations
let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

/// Gets all permutations with out repetition
let rec getPermutations = function
    | []      -> [ [] ]
    | x :: xs -> List.concat (List.map (insertions x) (getPermutations xs))

/// Gets all combinations and permutations (with repetition) of specified length from a list.
let rec getPermutationsWithRepitition n lst = 
    match n, lst with
    | 0, _ ->  [[]]
    | _, [] ->  []
    | k, _ -> lst |> List.collect (fun x -> List.map ((@) [x]) (getPermutationsWithRepitition (k - 1) lst))
//#endregion

//#region List utilities
let split n xs =
    let rec splitUtil n xs acc =
        match xs with
        | [] -> List.rev acc, []
        | _ when n = 0 -> List.rev acc, xs
        | x::xs' -> splitUtil (n-1) xs' (x::acc)
    splitUtil n xs []
//#endregion

//#region Krypto Game Setup
// The 56 card, Krypto Card deck consists of 3 each of 1-6, four each of 1-7, 2 each of 11-17, and one each of 18-25
let cardsUpTo6 =  [for f in 1.0..6.0 -> Node.Operand f ]
let cardsUpTo10 = [for f in 7.0..10.0 -> Node.Operand f ]
let cardsUpTo17 = [for f in 11.0..17.0 -> Node.Operand f ]
let cardsUpTo25 = [for f in 18.0..25.0 -> Node.Operand f ]
let kryptoCardDeck = List.concat ((List.replicate 3 cardsUpTo6) @ (List.replicate 4 cardsUpTo10) @ (List.replicate 2 cardsUpTo17) @ (List.replicate 1 cardsUpTo25))
// The number of card that will be delt for this Krypto puzzle.  Note an additional "Result" card is always drawn as well
let mutable numberOfCardsInPlay = 4
let mutable numberOfOperators = numberOfCardsInPlay - 1
// Here are the Operations in the form of a tuple
let kryptoOperators = [OPlus; OMinus; OTimes; ODivide; OPowerOf; OMod]

/// Actions
let dealKryptoCards = 
    let random = System.Random()
    fun count -> 
        let cards = [for i in 1..count -> List.item (random.Next(kryptoCardDeck.Length)) kryptoCardDeck ]
        cards

let dealAKryptoCard() = 
    let random = System.Random()
    List.item (random.Next(kryptoCardDeck.Length)) kryptoCardDeck 

let rec kryptoCardsAsString cards =
    match cards with
    | h::t -> sprintf "%s %s" (getNodeString h) (kryptoCardsAsString t)
    | [] -> ""
    
//#endregion

//region Krypto Game Run
let mutable CardsInPlay = [for x in 1..numberOfCardsInPlay -> dealAKryptoCard()]
let mutable krytoResultCard = dealAKryptoCard()
//let kryptoCardsAndResultString = sprintf "%s = %s" (kryptoCardsAsString CardsInPlay) (getNodeString krytoResultCard)

//let shuffleKryptoDeck =
//    let random = System.Random()
//    CardsInPlay <- [for i in 1..numberOfCardsInPlay -> List.nth kryptoCardDeck (random.Next(kryptoCardDeck.Length))]
//    krytoResultCard <- List.nth kryptoCardDeck (random.Next(kryptoCardDeck.Length))
//    sprintf "%s = %s" (kryptoCardsAsString CardsInPlay) (getNodeString krytoResultCard)

let myRandom =
    let random = System.Random()
    random.Next(100)
//#endregion