module FSharpKrypto.Tree

// TODO
// (DONE)Remove redundant parentheses, see: http://stackoverflow.com/questions/18400741/remove-redundant-parentheses-from-an-arithmetic-expression
// Check for and eliminate duplicate answers, perhaps order the equation then do a string compare
//   to do this a flattenBTreeToGTree is needed, the a call to a sortGTree nodes, then a getGTreeString
//   It is my beleif that a list of strings that have gone through this flattening and sorting, will be ready
//   for a "solutionStringList |> List.distinct" that will eliminate far more dulpicates that we have inherited because of our
//   crazy 'all permutations' 'all combinations' that we wnet through to find ALL answers

//#region General Tree
//General Tree Reference: http://stackoverflow.com/questions/2815236/f-recursive-collect-and-filter-over-n-ary-tree
type 'a GeneralTree = Empty | GBranch of 'a * 'a GeneralTree list

//#endregion

//#region Binary Tree Branch leaf
/// Taken from Ninety-Nine F# Problems - Problems 54 - 60 - Binary trees
/// http://www.fssnip.net/snippet/as/0
/// A binary tree is either empty or it is composed of a root element and two successors, 
/// which are binary trees themselves.
///  tree1
///               +
///           /       \
///          -         x
///         /  \      /  \
///       1.0 2.0  3.0  4.0
///
/// In F#, we can characterize binary trees with a type definition: 
type 'a Tree = Empty | Branch of 'a * 'a Tree * 'a Tree

// BTreeToGTree Converts a binary tree to a General Tree
// Once this is created, I need to create a function to Flatten a binary tree into a General Tree perhaps called flattenBTreetoGTree
// the flatening will occur with neighboring nodes that have operations of the same priotiy
// for example a binary tree of (1 + (2 + (3 + (4 + 5)))) will flaten into a General Tree of (1 + 2 + 3 + 4 + 5)
//
#if NOTWORKINGYET
let rec BTreeToGTree btree :Krypto.Node GeneralTree =
    match btree with
    | Empty -> GeneralTree.Empty
    | Branch (Krypto.Operand(operand), Empty, Empty) -> GBranch (Krypto.Operand operand, GeneralTree.Empty)
    | Branch (Krypto.Operator(f,s,o), lt, rt) -> GBranch (operand, [ (BTreeToGTree lt); (BTreeToGTree rt)])

#endif
///
/// This says that a Tree of type a consists of either an Empty node, or a Branch containing one 
/// value of type a with exactly two subtrees of type Krypto.Node.
///  
/// Given this definition, the tree in the diagram above would be represented as: 
/// 
//let tree1 = Branch (Krypto.Operator((+), "+"), Branch (Krypto.Operator((-), "-"), Branch (Krypto.Operand(1.0), Empty, Empty),
//                                Branch (Krypto.Operand(2.0), Empty, Empty)),
//                            Branch (Krypto.Operator((*), "x"), Branch (Krypto.Operand(3.0), Empty, Empty),
//                                Branch (Krypto.Operand(4.0), Empty, Empty)))

/// Since a "leaf" node is a branch with two empty subtrees, it can be useful to define a 
/// shorthand function:

let leaf x = Branch (x, Empty, Empty) 
//#endregion

//#region leaf driven Binary Trees
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