#if INTERACTIVE
// Some code that executes only in FSI
#r "nuget: Bogus"
#else
namespace Samples
#endif

module Tree =
    open System

    type Leaf = { Id: Guid; Name: string }

    type Node =
        { Id: Guid
          Name: string
          SubTree: Tree seq }

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


    let createLeaf name = { Id = Guid.NewGuid(); Name = name }

    let createNode name subTree =
        { Id = Guid.NewGuid()
          Name = name
          SubTree = subTree }

    let rec update (targetId: Guid) (newValue: Tree) (tree: Tree) =
        match tree with
        | _ when tree.Id = targetId -> newValue
        | Leaf _ as leaf -> leaf
        | Node n -> Node { n with SubTree = n.SubTree |> Seq.map (update targetId newValue) }


    let rec remove (trees: Tree seq) (targetId: Guid) =
        trees
        |> Seq.choose (fun tree ->
            match tree with
            | _ when tree.Id = targetId -> None
            | Leaf _ as leaf -> Some leaf
            | Node node ->
                { node with SubTree = remove node.SubTree targetId }
                |> Node
                |> Some)


module Share =
    let fakar = Bogus.Faker "ja"
    let random = System.Random()

    module rec Fakar =
        let fullName () =
            $"{fakar.Name.LastName()} {fakar.Name.FirstName()}"

        let leaf = fullName >> Tree.createLeaf >> Tree.Leaf

        let node subTree =
            subTree
            |> Tree.createNode (fullName ())
            |> Tree.Node

        let generate max =
            let generators = [ leaf; fun () -> generate max |> node ]

            seq {
                for _ = 0 to random.Next max do
                    for _ = 0 to random.Next max do
                        generators[ generators.Length |> random.Next ] ()
            }

    let example = Fakar.generate 3 |> Seq.toList