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

// Since a "leaf" node is a branch with no subtrees, it can be useful to define a 
// shorthand function:

let Gleaf x = GBranch (x, []) 
//#endregion
//#region Display General Tree helpers
let rec getGTreeString gt =
    match gt with
    | Empty -> ""
    | GBranch ( Krypto.Operand(operand), []) -> sprintf "%.0f" operand
    | GBranch ( Krypto.Operator(f,s,o), h::t) -> sprintf "(%s%s%s)" (getGTreeString h) s (String.concat s (List.map getGTreeString t))
    | GBranch (_, []) -> ""

let rec getGTreeResult gt =
    match gt with
    | Empty -> 0.0
    | GBranch (Krypto.Operand(operand), []) -> operand
    | GBranch (Krypto.Operator(f,s,o), gth::gtt) -> List.fold (fun acc elem -> elem |> f acc) (getGTreeResult gth) (List.map getGTreeResult gtt)
    | GBranch (_, []) -> 0.0
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
    | Branch ( Krypto.Operator(f,s,o), lt, rt) when parentOrderOfOperationPriority > o -> sprintf "(%s%s%s)" (getTreeString' o lt) s (getTreeString' o rt)
    | Branch ( Krypto.Operator(f,s,o), lt, rt) -> sprintf "%s%s%s" (getTreeString' o lt) s (getTreeString' o rt)
    | Branch (_, _, _) -> ""

let getTreeString t =
    getTreeString' 0 t // Top parent, lowest order of operation -> Parentheses not needed

let rec getTreeResult t =
    match t with
    | Empty -> 0.0
    | Branch (Krypto.Operand(operand), Empty, Empty) -> operand
    | Branch (Krypto.Operator(f,s,o), lt, rt) -> (getTreeResult rt) |> f (getTreeResult lt) 
    | Branch (_, _, _) -> 0.0
//#endregion

//#region Binary and General Tree functions
// BTreeToGTree Converts a Binary Tree to a General Tree
// Once this is created, I need to create a function to flatten a binary tree into a General Tree perhaps called flattenBTreetoGTree
// the flatening will occur with neighboring nodes that have operations of the same priotiy
// for example a Binary Tree of (1 + (2 + (3 + (4 + 5)))) will flaten into a General Tree of (1 + 2 + 3 + 4 + 5)
let rec BTreeToGTree btree :Krypto.Node GeneralTree =
    match btree with
    | Empty -> GeneralTree.Empty
    | Branch (node, Empty, Empty) -> Gleaf (node)
    | Branch (node, lt, rt) -> GBranch (node, [(BTreeToGTree lt); (BTreeToGTree rt)])

// Testing out new GTree functions
let tree1 = Branch (Krypto.OPlus, Branch (Krypto.OMinus, leaf (Krypto.Operand(1.0)), leaf (Krypto.Operand(2.0))), Branch (Krypto.OTimes, leaf (Krypto.Operand(3.0)), leaf (Krypto.Operand(4.0))))
let treeString1 = getTreeString tree1
let treeResult1 = getTreeResult tree1
let GTree1 = BTreeToGTree tree1
let GTreeString1 = getGTreeString GTree1
let GTreeResult1 = getGTreeResult GTree1
let OneTwoThree = [Gleaf (Krypto.Operand(1.0)); Gleaf (Krypto.Operand(2.0)); Gleaf (Krypto.Operand(3.0))]
let Plus123 = GBranch (Krypto.OPlus, OneTwoThree)
let Times123 = GBranch (Krypto.OTimes, OneTwoThree)
let Minus123 = GBranch (Krypto.OMinus, OneTwoThree)
let Divide123 = GBranch (Krypto.ODivide, OneTwoThree)
let GTree123 = GBranch (Krypto.OPlus, [Plus123; Times123; Minus123; Divide123])
let GTree123String = getGTreeString GTree123
let GTree123Result = getGTreeResult GTree123
    
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

//#region Krypto Game Solve
let mutable AllPermutationsOfCardsInPlay = Krypto.getPermutations Krypto.CardsInPlay
let mutable AllPossibleOperatorCombinations = Krypto.getPermutationsWithRepitition Krypto.numberOfOperators Krypto.kryptoOperators

let kryptoSolutionWithTheseCards ( array : ResizeArray<int>) =
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