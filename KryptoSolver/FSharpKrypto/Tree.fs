module FSharpKrypto.Tree
let but_last list =
    let rec but_last' list' acc =
        match list' with
        | [x]     -> List.rev acc
        | x :: xs -> but_last' xs (x :: acc)
    if List.isEmpty list then [] else but_last' list []
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
let rec getGTreeString' gt =
    match gt with
    | GeneralTree.Empty -> ""
    | GBranch ( Krypto.Operand(operand), []) -> sprintf "%.0f" operand
    | GBranch ( Krypto.Operator(f,s,o), h::t) -> sprintf "(%s%s%s)" (getGTreeString' h) s (String.concat s (List.map getGTreeString' t))
    | GBranch (_, []) -> ""

let rec getGTreeResult gt =
    match gt with
    | GeneralTree.Empty -> 0.0
    | GBranch (Krypto.Operand(operand), []) -> operand
    | GBranch (Krypto.Operator(f,s,o), gth::gtt) -> List.fold (fun acc elem -> elem |> f acc) (getGTreeResult gth) (List.map getGTreeResult gtt)
    | GBranch (_, []) -> 0.0

let getGTreeString gt =
    sprintf "%s = %.0f" (getGTreeString' gt) (getGTreeResult gt) // Top parent, lowest order of operation -> Parentheses not needed

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
let rec getTreeString' parentOrderOfOperationPriority t =
    match t with
    | Empty -> ""
    | Branch ( Krypto.Operand(operand) , Empty, Empty) -> sprintf "%.0f" operand
    | Branch ( Krypto.Operator(f,s,o), lt, rt) -> sprintf "(%s%s%s)" (getTreeString' o lt) s (getTreeString' o rt)
//    | Branch ( Krypto.Operator(f,s,o), lt, rt) when parentOrderOfOperationPriority > o -> sprintf "(%s%s%s)" (getTreeString' o lt) s (getTreeString' o rt)
//    | Branch ( Krypto.Operator(f,s,o), lt, rt) -> sprintf "%s%s%s" (getTreeString' o lt) s (getTreeString' o rt)
    | Branch (_, _, _) -> ""

let rec getTreeResult t =
    match t with
    | Empty -> 0.0
    | Branch (Krypto.Operand(operand), Empty, Empty) -> operand
    | Branch (Krypto.Operator(f,s,o), lt, rt) -> (getTreeResult rt) |> f (getTreeResult lt) 
    | Branch (_, _, _) -> 0.0

let getTreeString t =
    sprintf "%s = %.0f" (getTreeString' 0 t) (getTreeResult t) // Top parent, lowest order of operation -> Parentheses not needed

//#endregion

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
let rec GTreeFlatten' (parentOOP :Krypto.Node) (gtl :(Krypto.Node GeneralTree list)) :(Krypto.Node GeneralTree list) =
    match gtl with
    | [] -> []
   // | GBranch (node, []) -> [Gleaf (node)]
    //| GBranch (Krypto.Operator(f,s,o), h::t) when parentOOP > o -> [GBranch (Krypto.Operator(f,s,o), List.concat (h |> List.map (GTreeFlatten' o)) @ (List.map (t |> GTreeFlatten' o)))]
    | [GBranch (Krypto.Node.Empty, _);_] -> []
    | [GBranch (node, [])] -> [Gleaf (node)]
    | GBranch (Krypto.Operator(f,s,o),h)::t when Krypto.IsMinus parentOOP -> [GBranch ((Krypto.Operator(f,s,o)), (GTreeFlatten' (Krypto.Operator(f,s,o)) h))] @ (GTreeFlatten' parentOOP t)
    | GBranch (Krypto.Operator(f,s,o),h)::t when Krypto.IsDivide parentOOP -> [GBranch ((Krypto.Operator(f,s,o)), (GTreeFlatten' (Krypto.Operator(f,s,o)) h))] @ (GTreeFlatten' parentOOP t)
    | GBranch (Krypto.Operator(f,s,o),h)::t when (parentOOP |> Krypto.getNodeOOP) <> o -> [GBranch ((Krypto.Operator(f,s,o)), (GTreeFlatten' (Krypto.Operator(f,s,o)) h))] @ (GTreeFlatten' parentOOP t)
    | GBranch (Krypto.Operator(f,s,o),h)::t -> (GTreeFlatten' (Krypto.Operator(f,s,o)) h) @ (GTreeFlatten' parentOOP t)
    //| GBranch (x, [])::t -> [GBranch (parentOOP,  [Gleaf (x)] @ (GTreeFlatten' parentOOP t))]
    | GBranch (x, [])::t -> [Gleaf (x)] @ (GTreeFlatten' Krypto.ONone t)
    | _ -> (GTreeFlatten' Krypto.ONone [])
    //| h::t -> (GTreeFlatten' o h) @ List.concat (t |> List.map (GTreeFlatten' o))
    //| GBranch (node, h::t) -> GBranch (node, [(GTreeFlatten h)] @ List.concat [(List.map GTreeFlatten t)])

let GTreeFlatten (gt:(Krypto.Node GeneralTree)) :(Krypto.Node GeneralTree)=
    match gt with
    | GeneralTree.Empty -> GeneralTree.Empty
    | GBranch (node, []) -> Gleaf (node) 
    | GBranch (node, gtlist) -> GBranch (node, GTreeFlatten' node gtlist)

//let rec BTreeToFlattenedGTree' parentOrderOfOperationPriority btree gtlist =
//    match btree, gtlist with
//    | Empty, [] -> GeneralTree.Empty
//    | Empty, _ -> GBranch (Krypto.ONone, gtlist)
//    | Branch (node, Empty, Empty), [] -> Gleaf (node)
//    | Branch (node, Empty, Empty), _ -> GBranch (Krypto.ONone, [Gleaf (node)] @ gtlist)
//    | Branch (Krypto.Operator(f,s,o), lt, rt), _ when parentOrderOfOperationPriority > o -> GBranch (Krypto.Operator(f,s,o), [(BTreeToFlattenedGTree' o lt []); (BTreeToFlattenedGTree' o rt [])] @ gtlist)
//    | Branch (Krypto.Operator(f,s,o), lt, rt), _ -> BTreeToFlattenedGTree' o, Empty, ([(BTreeToFlattenedGTree' o lt []); (BTreeToFlattenedGTree' o rt [])] @ gtlist)
    ///GBranch (Krypto.Operator(f,s,o), [(BTreeToFlattenedGTree' o lt []); (BTreeToFlattenedGTree' o rt [])] @ gtlist)

// We need to add in the operator 'order of operation' priotiry for our flatten logic
// We need to switch to a tuple data type that is the BinaryTree and list of GeneralTrees <- these will be combined before returning
// a simple GeneralTree.  GT = BT converted to 2 item list  @ GTL
//let BTreeToFlattenedGTree t =
//    BTreeToFlattenedGTree' 0 t [] // Top parent, lowest order of operation -> Parentheses not needed, [] represents empty list of trees to be combined


// Testing out new GTree functions
let OneTwoThree = [Gleaf (Krypto.Operand(1.0)); Gleaf (Krypto.Operand(2.0)); Gleaf (Krypto.Operand(3.0))]
let Plus123 = GBranch (Krypto.OPlus, OneTwoThree)
let Times123 = GBranch (Krypto.OPlus, OneTwoThree)
let Minus123 = GBranch (Krypto.OPlus, OneTwoThree)
let Divide123 = GBranch (Krypto.OPlus, OneTwoThree)
let GTree123 = GBranch (Krypto.OPlus, [Plus123; Times123; Minus123; Divide123])
let GTree123String = getGTreeString GTree123
let GTree123Result = getGTreeResult GTree123
let GFTree = GTreeFlatten GTree123
let GFTreeString = getGTreeString GFTree
    




let TenDivideTwelve = Branch (Krypto.ODivide, leaf (Krypto.Operand(10.0)), leaf (Krypto.Operand(12.0)))
let TenTimesThree = Branch (Krypto.OTimes, leaf (Krypto.Operand(10.0)), leaf (Krypto.Operand(3.0)))
let tree1 = Branch (Krypto.OTimes, TenDivideTwelve, TenTimesThree)
let treeString1 = getTreeString tree1
let GTree1 = BTreeToGTree tree1
let GTreeString1 = getGTreeString GTree1
let GFTree1 = GTreeFlatten GTree1
let GFTree1String = getGTreeString GFTree1


//let GTree2 = GBranch (Krypto.Operator((+), "+", 1), [GTreeA; GTreeB; GTreeC; GTreeD ])

//#endregion

//#region leaf functions on Binary Trees
let rec allTreeLeafList' leafCount operands operators =
    match leafCount, operands, operators with
        | 0, _ , _ -> [Empty]
        | 1, (hand::tand), _ -> [leaf (hand)]
        | 2, (hand::tand), (htor::ttor) -> [Branch(htor, leaf (hand), leaf ((List.nth tand 0)))]
        | _, _, (htor::ttor) -> 
            [ for i=1 to leafCount / 2 do   // was leafCount -1 for all possible
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

let kryptoSolutionWithTheseCardsTest ( array : ResizeArray<int>) =
    sprintf "btree = %s\r\ngtree = %s\r\nftree = %s" treeString1 GTreeString1 GFTree1String


//#region Krypto Game Solve New
let kryptoSolutionWithTheseCards ( array : ResizeArray<int>) =
    let kryptoList = Seq.toList array
    let mutable solutionString = ""
    Krypto.numberOfCardsInPlay <- kryptoList.Length - 1
    Krypto.numberOfOperators <- Krypto.numberOfCardsInPlay - 1
    let floatList = kryptoList |> List.map (fun x -> float x)
    Krypto.CardsInPlay <- floatList |> but_last |> List.map (fun x -> Krypto.Operand x)
   
//    Krypto.CardsInPlay <- [Krypto.Operand(float array.[0]); Krypto.Operand(float array.[1]); Krypto.Operand(float array.[2]); Krypto.Operand(float array.[3])]
    Krypto.krytoResultCard <- Krypto.Operand(float array.[kryptoList.Length - 1])
    AllPermutationsOfCardsInPlay <- Krypto.getPermutations Krypto.CardsInPlay
    AllPossibleOperatorCombinations <- Krypto.getPermutationsWithRepitition Krypto.numberOfOperators Krypto.kryptoOperators
    let allTrees = getAllTreefromForestMatrix AllPermutationsOfCardsInPlay AllPossibleOperatorCombinations

//    let allTrees = 
//        AllPossibleOperatorCombinations
//        |> List.map
//            (getAllTreefromForestMatrix AllPermutationsOfCardsInPlay) 
    let solutionTreeList =
        allTrees
        |> List.filter (fun x -> (getTreeResult x) = (Krypto.getNodeValue Krypto.krytoResultCard))

    let solutionGTreeList =
        solutionTreeList
        |> List.map BTreeToGTree

    let solutionStringList = 
        solutionGTreeList
        |> List.map getGTreeString

   // let secondPassSolutionStringList = solutionStringList |> List.distinct  
    // these next lines are all about returning the output to the calling function
    for aSolutionString in solutionStringList do
        solutionString <- sprintf "%s\r\n%s" solutionString aSolutionString

    solutionString <- sprintf "%s" solutionString
    
//    let flattenSolutionGTreeList =
//        solutionGTreeList
//        |> List.map GTreeFlatten
//
//    let flattenSolutionStringList = 
//        flattenSolutionGTreeList
//        |> List.map getGTreeString
//
//    for aSolutionString in flattenSolutionStringList do
//        solutionString <- sprintf "%s\r\n%s" solutionString aSolutionString
    
    sprintf "Solutions = %s" solutionString

//#endregion

//#region Krypto Game Solve ORIG
let kryptoSolutionWithTheseCardsOrig ( array : ResizeArray<int>) =
//   let list = List.ofSeq(array)
//   FindMaxInList list
    let mutable solutionString = ""
    let mutable totalCombinationsCount = 0
    let mutable totalOperatorCombinations = 0
    let mutable totalCardPermutations = 0
    let mutable totalTreeLeafListCombinations = 0
    let mutable totalSolutionsCount = 0
    let mutable solutionTreeList = [Empty]
    Krypto.CardsInPlay <- [Krypto.Operand(float array.[0]); Krypto.Operand(float array.[1]); Krypto.Operand(float array.[2]); Krypto.Operand(float array.[3]); Krypto.Operand(float array.[4])]
    Krypto.krytoResultCard <- Krypto.Operand(float array.[5])
    AllPermutationsOfCardsInPlay <- Krypto.getPermutations Krypto.CardsInPlay
    AllPossibleOperatorCombinations <- Krypto.getPermutationsWithRepitition Krypto.numberOfOperators Krypto.kryptoOperators
    for onePermutationOfCards in AllPermutationsOfCardsInPlay do
        totalCardPermutations <- totalCardPermutations + 1
        totalOperatorCombinations <- 0
        for oneCombinationOfOperators in AllPossibleOperatorCombinations do
            totalOperatorCombinations <- totalOperatorCombinations + 1
            totalTreeLeafListCombinations <- 0
            for oneTreeInTheForest in (allTreeLeafList onePermutationOfCards oneCombinationOfOperators) do
                totalTreeLeafListCombinations <- totalTreeLeafListCombinations + 1
                totalCombinationsCount <- totalCombinationsCount + 1
                if (getTreeResult oneTreeInTheForest) = (Krypto.getNodeValue Krypto.krytoResultCard) then
                    totalSolutionsCount <- totalSolutionsCount + 1            
                    solutionTreeList <- List.append solutionTreeList [oneTreeInTheForest]
    // get a list of strings that we can work on to reduce duplicates
    let solutionStringList = List.map getTreeString solutionTreeList
    let secondPassSolutionStringList = solutionStringList |> List.distinct
    
    // these next lines are all about returning the output to the calling function
    for aSolutionString in solutionStringList do
        solutionString <- sprintf "%s\r\n%s" solutionString aSolutionString
    solutionString <- sprintf "%s\r\n===SECOND PASS WITH DUPLICATES REMOVED===" solutionString
    for aSolutionString in secondPassSolutionStringList do
        solutionString <- sprintf "%s\r\n%s" solutionString aSolutionString
    sprintf "Solutions/CardPermutations/OperatorCombinations/ParenthesisCombinations/TotalCombinations = %i/%i/%i/%i/%i\r\nSolutions = %s" totalSolutionsCount totalCardPermutations totalOperatorCombinations totalTreeLeafListCombinations totalCombinationsCount solutionString

//#endregion