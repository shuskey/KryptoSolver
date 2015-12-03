module FSharpKrypto.Tree

// TODO
// (DONE)Remove redundant parentheses, see: http://stackoverflow.com/questions/18400741/remove-redundant-parentheses-from-an-arithmetic-expression
// Check for and eliminate duplicate answers, perhaps order the equation then do a string compare
//   To do this, a flattenBTreeToGTree is needed, then a call to a sortGTree nodes, then a getGTreeString.
//   The resultant list of strings will be ready for a "solutionStringList |> List.distinct" that will eliminate far
//   more dulpicates that we have inherited because of our crazy 'all permutations' 'all combinations' that we went 
//   through to find ALL answers.

// I am using the "F# Outlining" extension from Victor Milovanov
//#region General Tree, GBranch, Gleaf
// General Tree Reference: 
// http://stackoverflow.com/questions/2815236/f-recursive-collect-and-filter-over-n-ary-tree
// A General Tree is either empty or it is composed of a root element and a list of successors, 
// which are General Trees themselves.

type 'a GeneralTree = Empty | GBranch of 'a * 'a GeneralTree list

type 'a GeneralList = 'a GeneralTree list

// Since a "leaf" node is a branch with no subtrees, it can be useful to define a 
// shorthand function:

let Gleaf x = GBranch (x, []) 
//#endregion
//#region Display General Tree helpers
let rec getGTreeString' parentOperator gt =
    match (Krypto.getNodeString parentOperator), gt with
    | _, GeneralTree.Empty -> ""
    | _, GBranch ( Krypto.Operand(operand), []) -> sprintf "%.0f" operand
    | "#", GBranch ( node, h::t) -> sprintf "%s%s%s" (getGTreeString' node h ) (Krypto.getNodeString node) (String.concat (Krypto.getNodeString node) (List.map (getGTreeString' node) t))
    | "+", GBranch ( node, h::t)  when 
        (Krypto.getNodeString node = "+" ) || (Krypto.getNodeString node = "-" )
        -> sprintf "%s%s%s" (getGTreeString' node h ) (Krypto.getNodeString node) (String.concat (Krypto.getNodeString node) (List.map (getGTreeString' node) t))
    | _,GBranch ( node, h::t) -> sprintf "(%s%s%s)" (getGTreeString' node h ) (Krypto.getNodeString node) (String.concat (Krypto.getNodeString node) (List.map (getGTreeString' node) t))
    | _,GBranch (_, []) -> ""

let rec getGTreeResult gt =
    match gt with
    | GeneralTree.Empty -> 0.0
    | GBranch (Krypto.Operand(operand), []) -> operand
    | GBranch (Krypto.Operator(f,s), gth::gtt) -> List.fold (fun acc elem -> elem |> f acc) (getGTreeResult gth) (List.map getGTreeResult gtt)
    | GBranch (_, []) -> 0.0

let getGTreeString gt =
    sprintf "%s = %.0f" (getGTreeString' Krypto.ONone gt) (getGTreeResult gt) // Top parent, lowest order of operation -> Parentheses not needed

let sortGTFunction gt1 gt2 =
    System.String.Compare((getGTreeString gt1), (getGTreeString gt2))

// Sort only "+" branches and "x" branches
// for "-" branches, only sort the tail
let rec sortEachGBranch (gt :(Krypto.Node GeneralTree))  =
    match gt with
    | GeneralTree.Empty -> GeneralTree.Empty
    | GBranch (operandNode, []) -> Gleaf (operandNode)
    | GBranch (operandNode, gtlist) when   // OKAY TO SORT
        Krypto.getNodeString operandNode = "+" ||
        Krypto.getNodeString operandNode = "x"
        -> GBranch (operandNode, List.sortWith sortGTFunction (gtlist |> List.map sortEachGBranch ))
    | GBranch (operandNode, gth::gtt) when   // OKAY TO SORT Tail ONLY
        Krypto.getNodeString operandNode = "-"
        -> GBranch (operandNode, gth::(List.sortWith sortGTFunction (gtt |> List.map sortEachGBranch )))
    | GBranch (operandNode, gtlist)      // NOT OKAY TO SORT
        -> gt


//#endregion

//#region Binary Tree, Branch leaf
// Binary Tree Reference:
// http://www.fssnip.net/snippet/as/0
// A Binary Tree is either empty or it is composed of a root element and two successors, 
// which are binary trees themselves.
//  tree1
//               +
//           /       \
//          -         x
//         /  \      /  \
//       1.0 2.0  3.0  4.0
//
// In F#, we can characterize binary trees with a type definition: 
type 'a Tree = Empty | Branch of 'a * 'a Tree * 'a Tree

//
// This says that a Tree of type a consists of either an Empty node, or a Branch containing one 
// value of type a with exactly two subtrees of type Krypto.Node.
//  
// Given this definition, the tree in the diagram above would be represented as: 
// 
//let tree1 = Branch (Krypto.Operator((+), "+"), Branch (Krypto.Operator((-), "-"), Branch (Krypto.Operand(1.0), Empty, Empty),
//                                Branch (Krypto.Operand(2.0), Empty, Empty)),
//                            Branch (Krypto.Operator((*), "x"), Branch (Krypto.Operand(3.0), Empty, Empty),
//                                Branch (Krypto.Operand(4.0), Empty, Empty)))

// Since a "leaf" node is a branch with two empty subtrees, it can be useful to define a 
// shorthand function:

let leaf x = Branch (x, Empty, Empty) 
//#endregion
//#region Display Binary Tree helpers

let rec getTreeResult t =
    match t with
    | Empty -> 0.0
    | Branch (Krypto.Operand(operand), Empty, Empty) -> operand
    | Branch (Krypto.Operator(f,s), lt, rt) -> (getTreeResult rt) |> f (getTreeResult lt) 
    | Branch (_, _, _) -> 0.0

let rec getTreeString' (parentOperator :Krypto.Node) t leftOrRight=
    match (Krypto.getNodeString parentOperator), t, leftOrRight with
    | _, Empty, _ -> ""
    | _, Branch ( Krypto.Operand(operand) , Empty, Empty), _ -> sprintf "%.0f" operand
    | "x", Branch ( node, lt, rt), _ when 
        (Krypto.getNodeString node = "x" )
        -> sprintf "%s%s%s" (getTreeString' node lt "L") (Krypto.getNodeString node) (getTreeString' node rt "R")
    | "+", Branch ( node, lt, rt), _ when 
        (Krypto.getNodeString node = "+" ) || (Krypto.getNodeString node = "-" )
        -> sprintf "%s%s%s" (getTreeString' node lt "L") (Krypto.getNodeString node) (getTreeString' node rt "R")
    | "-", Branch ( node, lt, rt), "L" when // (A-B)-C OR (A+B)-C drop paren
        (Krypto.getNodeString node = "-" ) ||  // A-B-C      A+B-C  
        (Krypto.getNodeString node = "+" )
        -> sprintf "%s%s%s" (getTreeString' node lt "L") (Krypto.getNodeString node) (getTreeString' node rt "R")
    | "-", Branch ( node, lt, rt), "R" when  // A-(B-C) drop paren make B+C
        (Krypto.getNodeString node = "-" )   // A-B+C 
        -> sprintf "%s%s%s" (getTreeString' node lt "L") "+" (getTreeString' Krypto.OPlus rt "R")
    | "-", Branch ( node, lt, rt), "R" when   //A-(B+C) drop paren make B-C
        (Krypto.getNodeString node = "+" )   // A-B-C 
        -> sprintf "%s%s%s" (getTreeString' node lt "L") "-" (getTreeString' Krypto.OMinus rt "R")
    | "#", Branch ( node, lt, rt), _ -> sprintf "%s%s%s" (getTreeString' node lt "L") (Krypto.getNodeString node) (getTreeString' node rt "R")
    | _, Branch ( node, lt, rt), _ -> sprintf "(%s%s%s)" (getTreeString' node lt "L") (Krypto.getNodeString node) (getTreeString' node rt "R")

//    | Branch ( Krypto.Operator(f,s,o), lt, rt) when parentOperator > o -> sprintf "(%s%s%s)" (getTreeString' o lt) s (getTreeString' o rt)
//    | Branch ( Krypto.Operator(f,s,o), lt, rt) -> sprintf "%s%s%s" (getTreeString' o lt) s (getTreeString' o rt)

let getTreeString t more =
    sprintf "%s = %.0f  more %s" (getTreeString' Krypto.ONone t "L") (getTreeResult t) more // Top parent, lowest order of operation -> Parentheses not needed

//#region Binary and General Tree functions
// BTreeToGTree Converts a Binary Tree to a General Tree
// Once this is created, I need to create a function to flatten a binary tree into a General Tree perhaps called flattenBTreetoGTree
// the flatening will occur with neighboring nodes that have operations of the same priotiy
// for example a Binary Tree of (1 + (2 + (3 + (4 + 5)))) will flaten into a General Tree of (1 + 2 + 3 + 4 + 5)
let rec BTreeToGTree (btree :(Krypto.Node Tree)) :(Krypto.Node GeneralTree) =
    match btree with
    | Tree.Empty -> GeneralTree.Empty
    | Branch (node, Empty, Empty) -> Gleaf (node)
    | Branch (node, lt, rt) -> GBranch (node, [(BTreeToGTree lt); (BTreeToGTree rt)])
//
//This will return a Gtree list that need to be welded back to a parent node
//Input Parameters are a GTree list and a Parent Order of Operation Priority
let rec GTreeFlatten' (leftOrRight :string) (grandParentOperator :Krypto.Node) (parentOperator :Krypto.Node) (gtl :(Krypto.Node GeneralTree list)) :(Krypto.Node GeneralTree list) =
    match  leftOrRight, (Krypto.getNodeString grandParentOperator), (Krypto.getNodeString parentOperator), gtl with
    // leftOrRight, if "L" then the equation is formatted like (A#B)#C and "R" is A#(B#C)
    //  p = parentOperator, n = node or node of Head Item
    // (AnB)pC  Ap(BnC)
    // when removing parentheses from some Left nodes, we need to know the grandParentOperator
    // gp = grandParentOperator
    // D gp (A-B)+C
    | _,_,_, [] -> []
   // | GBranch (node, []) -> [Gleaf (node)]
    //| GBranch (Krypto.Operator(f,s,o), h::t) when parentOperator > o -> [GBranch (Krypto.Operator(f,s,o), List.concat (h |> List.map (GTreeFlatten' o)) @ (List.map (t |> GTreeFlatten' o)))]
    | _,_,_, [GBranch (Krypto.Node.Empty, _);_] -> []
    | _,_,_, [GBranch (node, [])] -> [Gleaf (node)]
  ////  | _,_,_,GBranch (x, [])::t -> gtl
  ////  | _,_,_,GBranch (x, [])::t -> [GBranch (parentOperator, gtl)]
    //    -> [Gleaf(x)] @ (GTreeFlatten' grandParentOperator parentOperator t "R")
    | _,_,"x", GBranch (node,h)::t when // (AxB)xC -> AxBxC, Ax(BxC) -> AxBxC
        (Krypto.IsTimes node)
        ->  GTreeFlatten' "L" parentOperator node h @ GTreeFlatten' "R" parentOperator node t
    | "R","-","-", GBranch (node,h)::t when // (A - (B+C))  -> A-B-C  works for    (A - (B+C)) - D  but not A - ((B+C) - D)
        (Krypto.IsPlus node)  
        ->  GTreeFlatten' "L" parentOperator Krypto.OMinus h @ GTreeFlatten' "R" parentOperator node t
//    | "R","-","-", GBranch (node,h)::t when    // A-(B+C) -> A-B-C nice!
//        (Krypto.IsPlus node) // We can not mix operations in a GTree||  (Krypto.IsMinus node) // A-(B-C) -> A-B+C
//        ->  GTreeFlatten' "L" parentOperator Krypto.OMinus h @ GTreeFlatten' "R" parentOperator Krypto.OMinus t
    | "L","+","+", GBranch (node,h)::t when // +(A+B)+C -> A+B+C
        (Krypto.IsPlus node) // We can not mix operations in a GTree|| (Krypto.IsMinus node) 
        ->  GTreeFlatten' "L" parentOperator node h @ GTreeFlatten' "R" parentOperator node t
    | "R","+","+", GBranch (node,h)::t when // A+(B+C) -> A+B+C
        (Krypto.IsPlus node) // We can not mix operations in a GTree|| (Krypto.IsMinus node) 
        ->  GTreeFlatten' "L" parentOperator node h @ GTreeFlatten' "R" parentOperator node t
    | "R","+","-", GBranch (node,h)::t when    //  A + (B-(C+D)) -> B-C-D nice!  
        (Krypto.IsPlus node) // We can not mix operations in a GTree||  (Krypto.IsMinus node) // A-(B-C) -> A-B+C
        ->  GTreeFlatten' "L" parentOperator Krypto.OMinus h @ GTreeFlatten' "R" parentOperator Krypto.OMinus t
    | "R","-","+", GBranch (node,h)::t when // - (A+(B+C)) -> A+B+C
        (Krypto.IsPlus node) // We can not mix operations in a GTree|| (Krypto.IsMinus node) 
        ->  GTreeFlatten' "L" parentOperator node h @ GTreeFlatten' "R" parentOperator node t
    | _,_,_, GBranch (node,h)::t 
        -> [GBranch (node, (GTreeFlatten' "L" parentOperator node h))] @ (GTreeFlatten' "R" grandParentOperator parentOperator t)
    //| GBranch (x, [])::t -> [GBranch (parentOperator,  [Gleaf (x)] @ (GTreeFlatten' parentOperator t))]
    ////| _,_,_,GBranch (x, [])::t -> [Gleaf (x)] @ (GTreeFlatten' parentOperator parentOperator t "R")
    | _,_,_,_ -> (GTreeFlatten' "L" Krypto.ONone Krypto.ONone [])
    //| h::t -> (GTreeFlatten' o h) @ List.concat (t |> List.map (GTreeFlatten' o))
    //| GBranch (node, h::t) -> GBranch (node, [(GTreeFlatten h)] @ List.concat [(List.map GTreeFlatten t)])

let GTreeFlatten (gt:(Krypto.Node GeneralTree)) :(Krypto.Node GeneralTree)=
    match gt with
    | GeneralTree.Empty -> GeneralTree.Empty
    | GBranch (node, []) -> Gleaf (node) 
    | GBranch (node, gtlist) -> GBranch (node, GTreeFlatten' "L" Krypto.OPlus node gtlist)

//#endregion
// (12+5) + (11+4) + 12 + 11 + 15 (12+4)
let TwelvePlusFive = Branch (Krypto.OPlus, leaf (Krypto.Operand(12.0)), leaf (Krypto.Operand(5.0)))
let ElevenPlusFour = Branch (Krypto.OPlus, leaf (Krypto.Operand(11.0)), leaf (Krypto.Operand(4.0)))
let TwelvePlusFour = Branch (Krypto.OPlus, leaf (Krypto.Operand(12.0)), leaf (Krypto.Operand(4.0)))
let TwelvePlusEleven = Branch (Krypto.OPlus, leaf (Krypto.Operand(12.0)), leaf (Krypto.Operand(11.0)))
let FifteenPlusAbove = Branch (Krypto.OPlus, leaf (Krypto.Operand(15.0)), TwelvePlusFour)
let FirstTwo = Branch (Krypto.OPlus, TwelvePlusFive, ElevenPlusFour)
let NextOne = Branch (Krypto.OPlus, FirstTwo, TwelvePlusEleven)
let tree1 = Branch (Krypto.OPlus, NextOne, FifteenPlusAbove)

let GTree1 = BTreeToGTree tree1
let GTreeString1 = getGTreeString GTree1
let GSTree1 = sortEachGBranch GTree1
let GSTree1String = getGTreeString GSTree1
let GFTree1 = GTreeFlatten GTree1
let GFSTree1 = sortEachGBranch GFTree1
let GFSTree1String = getGTreeString GFSTree1
let GFTree1String = getGTreeString GFTree1
let treeString1 = getTreeString tree1 GFTree1String
let ts = tree1 |> (fun x -> (getTreeString x ((getGTreeString (GTreeFlatten (BTreeToGTree x))))))
let x = 0
//#region leaf functions on Binary Trees
let rec allTreeLeafList' leafCount operands operators =
    match leafCount, operands, operators with
        | 0, _ , _ -> [Empty]
        | 1, (hand::tand), _ -> [leaf (hand)]
        | 2, (hand::tand), (htor::ttor) -> [Branch(htor, leaf (hand), leaf ((List.nth tand 0)))]
        | _, _, (htor::ttor) -> 
            [ for i=1 to leafCount - 1 do   // was leafCount -1 for all possible, leafCout/2 to get 1/2
                for lt in allTreeLeafList' i (fst (Krypto.split i operands)) (fst (Krypto.split (i-1) ttor)) do
                    for rt in allTreeLeafList' (leafCount - i) (snd (Krypto.split i operands)) (snd (Krypto.split (i-1) ttor)) do
                        yield Branch(htor, lt, rt) ]

let allTreeLeafList operands operators =
    match operands, operators with
    | [], _ -> [Empty]
    | _, [] -> [Empty]
    | _, _ when operands.Length <> (operators.Length + 1) -> [Empty]
    | _, _ -> allTreeLeafList' operands.Length operands operators
//#endregion


let getEquationTreesForTheseCards allPossibleCombinationsOfOperators onePermutationOfCards =
    List.concat (allPossibleCombinationsOfOperators 
    |> List.map 
        (allTreeLeafList onePermutationOfCards))

let getAllTreefromForestMatrix allPermutationsOfCardsInPlay allPossibleOperatorCombinations =
    List.concat (allPermutationsOfCardsInPlay 
    |> List.map
        (getEquationTreesForTheseCards allPossibleOperatorCombinations))

let mutable AllPermutationsOfCardsInPlay = Krypto.getPermutations Krypto.CardsInPlay
let mutable AllPossibleOperatorCombinations = Krypto.getPermutationsWithRepitition Krypto.numberOfOperators Krypto.kryptoOperators

//let kryptoSolutionWithTheseCardsTest ( array : ResizeArray<int>) =
//    sprintf "btree = %s\r\ngtree = %s\r\nftree = %s" treeString1 GTreeString1 GFTree1String

let GetAllButLast list =
    let rec but_last' list' acc =
        match list' with
        | [x]     -> List.rev acc
        | x :: xs -> but_last' xs (x :: acc)
    if List.isEmpty list then [] else but_last' list []

let distanceTo option target =
     abs (target - option)

let closestToTarget (acc :float) (option :float) (target :float) :(float) = 
    if (option.Equals(nan)) then
        acc
    else
        if (distanceTo acc target) < (distanceTo option target) then
            acc
        else
            option

let GetClosestSolution target allTrees= 
    let startValue = getTreeResult (List.item 1 allTrees)
    let pret = List.fold (fun acc elem -> closestToTarget acc (getTreeResult elem) target) startValue allTrees
    pret
      
//#region Krypto Game Solve New
// <summary>
//  kryptoSolutionWithTheseCards provides an interface to this F-Sharp Library from C-Sharp
//  It will evaluate all possible permutations of cards and all combinations of operations
//  along with all combinations of computational order (Parentheses), then determine which
//  equations are equal to the desired Target Solution.
// </summary>
// <param name="array"> This is an array of integer values.  All but the last element in this array are
// the 'Cards in Play'.  The last element of this array is the Target Solution Value.</param>
// <returns>
//   This will return a String which represents a human readable list off all the equations that match the 
//   Target Solution.  This, under some circumstances, will be a list of equations that come CLOSEST to the
//   Target Solution.  This string may also include some cool statistics about the calulations that were needed
//   to solve the Krypto Puzzle.
// </returns>
let kryptoSolutionWithTheseCards ( array : ResizeArray<int>) =
    let kryptoList = Seq.toList array       // Convert this to an F-Sharp List
    let mutable solutionString = ""

    Krypto.numberOfCardsInPlay <- kryptoList.Length - 1
    Krypto.numberOfOperators <- Krypto.numberOfCardsInPlay - 1
    Krypto.CardsInPlay <- kryptoList 
        |> GetAllButLast                         // all but the last, are the cards in play
        |> List.map (fun x -> float x) 
        |> List.map (fun x -> Krypto.Operand x)
   
    Krypto.krytoResultCard <- Krypto.Operand(float array.[kryptoList.Length - 1])
    AllPermutationsOfCardsInPlay <- Krypto.getPermutations Krypto.CardsInPlay
    AllPossibleOperatorCombinations <- Krypto.getPermutationsWithRepitition Krypto.numberOfOperators Krypto.kryptoOperators
    let allTrees = getAllTreefromForestMatrix AllPermutationsOfCardsInPlay AllPossibleOperatorCombinations
    let targetSolution = Krypto.getNodeValue Krypto.krytoResultCard

    let solutionTreeList =
        allTrees
        |> List.filter (fun x -> (getTreeResult x) = targetSolution)

    let closestSolution =
        if (solutionTreeList.Length = 0) then 
            GetClosestSolution targetSolution allTrees
        else
            nan

    let closestSolutionTreeList = 
        if (solutionTreeList.Length = 0) then 
            allTrees |> List.filter (fun x -> (getTreeResult x) = closestSolution)
        else
            []

    let solutionStringList =
        if (solutionTreeList.Length = 0) then // no 'dead on' solution, now try for the closest
            closestSolutionTreeList |> List.map (fun x -> (sprintf "%s" (getGTreeString (sortEachGBranch (GTreeFlatten (BTreeToGTree x))))))
        else 
            solutionTreeList |> List.map (fun x -> (sprintf "%s" (getGTreeString (sortEachGBranch (GTreeFlatten (BTreeToGTree x))))))
            //Long            |> List.map (fun x -> (getTreeString x (sprintf "%s sorted  %s" (getGTreeString (GTreeFlatten (BTreeToGTree x))) (getGTreeString (sortEachGBranch (GTreeFlatten (BTreeToGTree x))))  )))
                
    let secondPassSolutionStringList = solutionStringList |> List.distinct  
    // these next lines are all about returning the output to the calling function
    for aSolutionString in secondPassSolutionStringList do
        solutionString <- sprintf "%s\r\n%s" solutionString aSolutionString

   // solutionString <- sprintf "%s" solutionString
    let mutable Statistics = sprintf "%i Number of Operand Permutations\r\n%i Number of Operator Combinations\r\n%i  Total Number of Equations\r\n%i  Number of Solutions Found\r\n%i  Number of Distinct Solutions Found\r\n" AllPermutationsOfCardsInPlay.Length AllPossibleOperatorCombinations.Length allTrees.Length solutionTreeList.Length secondPassSolutionStringList.Length

    if (solutionTreeList.Length = 0) then 
        sprintf "Stats = %s\r\nCLOSEST SOLUTIONS = %s" Statistics solutionString
    else
        sprintf "Stats = %s\r\nExact Solutions = %s" Statistics solutionString

//#endregion