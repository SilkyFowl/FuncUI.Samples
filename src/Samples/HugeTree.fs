namespace Samples

module Tree =
    open System

    let rand = Random()

    type Leaf = {| Id: Guid; Name: string |}

    type Node =
        {| Id: Guid
           Name: string
           IsExpand: bool
           Children: Tree seq |}

    and Tree =
        | Leaf of Leaf
        | Node of Node

        member x.Id =
            match x with
            | Leaf l -> l.Id
            | Node n -> n.Id

        member x.Name =
            match x with
            | Leaf l -> l.Name
            | Node n -> n.Name

    let rec cata fLeaf fNode item =
        let recurse = cata fLeaf fNode

        match item with
        | Leaf leaf -> fLeaf leaf
        | Node node ->
            let children' = node.Children |> Seq.map recurse

            fNode
                {| Id = node.Id
                   Name = node.Name
                   IsExpand = node.IsExpand
                   Children' = children' |}


    let update (tree: Tree) (oldTree: Tree) (newTree: Tree) =

        cata
            (fun l ->
                if l.Id = oldTree.Id then
                    newTree
                else
                    Leaf l)
            (fun n' ->
                if n'.Id = oldTree.Id then
                    oldTree
                else
                    {| Id = n'.Id
                       Name = n'.Name
                       IsExpand = n'.IsExpand
                       Children = n'.Children' |}
                    |> Node)
            tree

    let rec remove (trees: Tree seq) (removeTree: Tree) =
        trees
        |> Seq.choose (fun tree ->
            match tree with
            | _ when tree.Id = removeTree.Id -> None
            | Leaf _ as leaf -> Some leaf
            | Node node ->
                {| node with
                    Children = remove node.Children removeTree |}
                |> Node
                |> Some)

// match tree with
// | _ when tree.Id = oldTree.Id -> newTree
// | Leaf _ -> tree
// | Node n ->
//     {| n with
//         Children =
//             seq {
//                 for child in n.Children do
//                     update child oldTree newTree
//             } |}
//     |> Node
