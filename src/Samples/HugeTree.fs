#if INTERACTIVE
// Some code that executes only in FSI
#r "nuget: Bogus"
#else
namespace Samples
#endif

/// 参考:https://fsharpforfunandprofit.com/posts/recursive-types-and-folds-3b/#tree
type Tree<'LeafData, 'INodeData> =
    | LeafNode of 'LeafData
    | InternalNode of 'INodeData * Tree<'LeafData, 'INodeData> seq

module Tree =

    let rec cata fLeaf fNode (tree: Tree<'LeafData, 'INodeData>) : 'r =
        let recurse = cata fLeaf fNode

        match tree with
        | LeafNode leafInfo -> fLeaf leafInfo
        | InternalNode (nodeInfo, subtrees) -> fNode nodeInfo (subtrees |> Seq.map recurse)

    let rec fold fLeaf fNode acc (tree: Tree<'LeafData, 'INodeData>) : 'r =
        let recurse = fold fLeaf fNode

        match tree with
        | LeafNode leafInfo -> fLeaf acc leafInfo
        | InternalNode (nodeInfo, subtrees) ->
            // determine the local accumulator at this level
            let localAccum = fNode acc nodeInfo
            // thread the local accumulator through all the subitems using Seq.fold
            let finalAccum = subtrees |> Seq.fold recurse localAccum
            // ... and return it
            finalAccum

    let rec map fLeaf fNode (tree: Tree<'LeafData, 'INodeData>) =
        let recurse = map fLeaf fNode

        match tree with
        | LeafNode leafInfo ->
            let newLeafInfo = fLeaf leafInfo
            LeafNode newLeafInfo
        | InternalNode (nodeInfo, subtrees) ->
            let newSubtrees = subtrees |> Seq.map recurse
            let newNodeInfo = fNode nodeInfo
            InternalNode(newNodeInfo, newSubtrees)

    let rec iter fLeaf fNode (tree: Tree<'LeafData, 'INodeData>) =
        let recurse = iter fLeaf fNode

        match tree with
        | LeafNode leafInfo -> fLeaf leafInfo
        | InternalNode (nodeInfo, subtrees) ->
            subtrees |> Seq.iter recurse
            fNode nodeInfo

    let rec filter fLeaf fNode (tree: Tree<'LeafData, 'INodeData> seq) =
        let recurse = filter fLeaf fNode

        tree
        |> Seq.choose (function
            | LeafNode leafInfo when fLeaf leafInfo -> (LeafNode >> Some) leafInfo
            | InternalNode (nodeInfo, subtrees) when fNode nodeInfo ->
                let newSubtrees = recurse subtrees
                InternalNode(nodeInfo, newSubtrees) |> Some
            | _ -> None)

    let rec setSubtrees selector fSubtrees (tree: Tree<'LeafData, 'INodeData>) =
        let recurse = setSubtrees selector fSubtrees

        match tree with
        | InternalNode (nodeInfo, subtrees) when selector nodeInfo ->
            let newSubtrees = fSubtrees subtrees
            InternalNode(nodeInfo, newSubtrees)
        | InternalNode (nodeInfo, subtrees) ->
            let newSubtrees = subtrees |> Seq.map recurse
            InternalNode(nodeInfo, newSubtrees)
        | other -> other

/// ダミーデータ
module Share =
    open System

    let fakar = Bogus.Faker "ja"
    let random = Random()

    type Kind = { Id: Guid; Name: string }

    type Info =
        { Id: Guid
          Name: string
          Description: string }

    type ShareItem = Tree<Info, Kind>

    let fromKind kind subitems : ShareItem = InternalNode(kind, subitems)
    let fromInfo info : ShareItem = LeafNode info

    module rec Fakar =

        let kind () =
            { Id = Guid.NewGuid()
              Name = fakar.Commerce.Categories 1 |> Array.head }

        let info () =
            { Id = Guid.NewGuid()
              Name = fakar.Commerce.ProductName()
              Description = fakar.Commerce.ProductDescription() }

        let generate max =
            let gen =
                [| fun () -> fromKind (kind ()) Seq.empty
                   info >> fromInfo |]

            seq {
                for _ = 0 to random.Next max do
                    (fakar.PickRandomParam gen) ()
            }
            |> Seq.cache


/// Viewで保持するTree型
type HugeTreeViewItem<'leaf, 'node> = Tree<HugeTreeViewLeaf<'leaf>, HugeTreeViewNode<'node>>

and HugeTreeViewLeaf<'leaf> = { Id: System.Guid; Data: 'leaf }

and HugeTreeViewNode<'node> =
    { Id: System.Guid
      Data: 'node
      IsExpand: bool }

/// 実際に表示されるデータ型
type ExpandedItem<'leaf, 'node> =
    { Ids: System.Guid list
      Data: Choice<HugeTreeViewLeaf<'leaf>, HugeTreeViewNode<'node>> }

module HugeTreeViewItem =
    let fromLeaf leaf : HugeTreeViewItem<'leaf, 'node> = LeafNode leaf
    let fromNode node subItem : HugeTreeViewItem<'leaf, 'node> = InternalNode(node, subItem)

    let create<'leaf, 'node> fLeafId fNodeId tree : HugeTreeViewItem<'leaf, 'node> =
        let fLeaf leaf = { Id = fLeafId leaf; Data = leaf }

        let fNode node =
            { Id = fNodeId node
              Data = node
              IsExpand = false }

        Tree.map fLeaf fNode tree

    let update oldValueId fNode fLeaf (tree: HugeTreeViewItem<'leaf, 'node>) : HugeTreeViewItem<'leaf, 'node> =
        tree
        |> Tree.map
            (fun leaf ->
                if leaf.Id = oldValueId then
                    fLeaf leaf
                else
                    leaf)
            (fun node ->
                if node.Id = oldValueId then
                    fNode node
                else
                    node)

    let setExpand isExpand (targetId: System.Guid) (tree: HugeTreeViewItem<'leaf, 'node>) =
        update targetId (fun node -> { node with IsExpand = isExpand }) id tree


    let expand (treeItem: HugeTreeViewItem<'leaf, 'node>) =
        let rec loop state (trees: HugeTreeViewItem<'leaf, 'node> seq) =
            trees
            |> Seq.map (function
                | LeafNode leaf -> seq { { Ids = state; Data = Choice1Of2 leaf } }
                | InternalNode (node, subTree) ->
                    seq {
                        { Ids = state; Data = Choice2Of2 node }

                        if node.IsExpand then
                            yield! loop (state @ List.singleton node.Id) subTree
                    })
            |> Seq.concat

        seq { treeItem } |> loop []

    let getId (tree: HugeTreeViewItem<'leaf, 'node>) =
        match tree with
        | LeafNode leaf -> leaf.Id
        | InternalNode (node, _) -> node.Id

    let setSubtrees
        (targetId: System.Guid)
        (tree: HugeTreeViewItem<'leaf, 'node>)
        (subtrees: HugeTreeViewItem<'leaf, 'node> seq)
        =
        tree
        |> Tree.setSubtrees (fun node -> node.Id = targetId) (fun _ -> subtrees)

#if INTERACTIVE
module Test =

    let root =
        Share.fromKind
            { Id = System.Guid.NewGuid()
              Name = "root" }
            (Share.Fakar.generate 5)

    let item =
        HugeTreeViewItem.create<Share.Info, Share.Kind> (fun info -> info.Id) (fun kind -> kind.Id) root

    let v = HugeTreeViewItem.expand item |> Seq.toList

    let item' = HugeTreeViewItem.setExpand true (HugeTreeViewItem.getId item) item


    let v' = item' |> HugeTreeViewItem.expand |> Seq.toList

    let v'' =
        item'
        |> HugeTreeViewItem.expand
        |> Seq.tryPick (function
            | { Data = Choice2Of2 node } when not node.IsExpand ->
                HugeTreeViewItem.setExpand true node.Id item'
                |> HugeTreeViewItem.expand
                |> Some
            | _ -> None)
        |> Option.defaultValue Seq.empty
        |> Seq.toList
#else


/// 大量のデータを扱えるTreeViewっぽいもの。
/// AvaloniaのTreeViewは仮想化できないので大量のデータがあると遅い。
/// https://github.com/AvaloniaUI/Avalonia/issues/6580
///
/// 対応するには仮想化できるItemRepeaterを使用する。
/// https://github.com/kekekeks/example-avalonia-huge-tree/blob/master/AvaloniaHugeTree/MainWindow.xaml#L19-L36
///
/// この実装はコード量削減のためListBoxを使用。
///
module HugeTree =
    open System

    module Share =
        let toTreeViewItem =
            HugeTreeViewItem.create<Share.Info, Share.Kind> (fun info -> info.Id) (fun kind -> kind.Id)

        let generateTreeViewItems max =
            Share.Fakar.generate max
            |> Seq.map toTreeViewItem

        let initRoot max =
            Share.fromKind { Id = Guid.NewGuid(); Name = "root" } (Share.Fakar.generate max)
            |> toTreeViewItem

        let addSubItems max (node: HugeTreeViewNode<_>) tree =
            generateTreeViewItems max
            |> HugeTreeViewItem.setSubtrees node.Id tree

    open Avalonia
    open Avalonia.Controls
    open Avalonia.Controls.Presenters
    open Avalonia.VisualTree
    open Avalonia.Media
    open Avalonia.Layout
    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL

    let conterStyle = [ Control.verticalAlignment VerticalAlignment.Center ]

    /// リスト要素の表示関数
    let viewFunc
        (root: IWritable<HugeTreeViewItem<Share.Info, Share.Kind>>)
        (max: IWritable<int>)
        (data: ExpandedItem<Share.Info, Share.Kind>)
        =
        let left = 4 + (24 * List.length data.Ids)

        let headerfontSize = 20


        Grid.create [
            Grid.margin (left, 4, 4, 4)
            match data.Data with
            | Choice1Of2 leaf ->
                Grid.columnDefinitions "Auto,*"
                Grid.rowDefinitions "Auto,Auto"

                Grid.children [
                    TextBlock.create [
                        TextBlock.column 1
                        TextBlock.row 0
                        yield! conterStyle
                        Control.margin (0, 0, 0, 4)
                        TextBlock.fontSize headerfontSize
                        TextBlock.text leaf.Data.Name
                    ]
                    TextBlock.create [
                        TextBlock.column 1
                        TextBlock.row 1
                        yield! conterStyle
                        TextBlock.textWrapping TextWrapping.WrapWithOverflow
                        TextBlock.text leaf.Data.Description
                    ]
                ]
            | Choice2Of2 node ->
                let setSubtrees isExpand _ =
                    HugeTreeViewItem.setExpand isExpand node.Id root.Current
                    |> root.Set

                let addSubItems _ =
                    Share.addSubItems max.Current node root.Current
                    |> root.Set

                Grid.columnDefinitions "Auto,Auto,*"

                Grid.children [
                    CheckBox.create [
                        CheckBox.column 0
                        CheckBox.isChecked node.IsExpand
                        CheckBox.onChecked (setSubtrees true)
                        CheckBox.onUnchecked (setSubtrees false)
                    ]
                    Button.create [
                        Button.column 1
                        Button.content "Genetate"
                        Button.onClick addSubItems
                    ]
                    TextBlock.create [
                        TextBlock.column 2
                        yield! conterStyle
                        Control.margin (8, 0, 0, 0)
                        TextBlock.fontSize headerfontSize
                        TextBlock.text node.Data.Name
                    ]
                ]
        ]

    let view =
        Component.create (
            "huge-tree",
            fun ctx ->
                let max = ctx.useState (5000, false)

                let root = Share.initRoot max.Current |> ctx.useState
                let expanded = HugeTreeViewItem.expand root.Current

                Grid.create [
                    Grid.rowDefinitions "Auto,*"
                    Grid.columnDefinitions "Auto,Auto,Auto,*"
                    Grid.children [
                        TextBlock.create [
                            Control.row 0
                            Control.column 0
                            yield! conterStyle
                            TextBlock.text $"Count: {Seq.length expanded}"
                        ]
                        TextBlock.create [
                            Control.row 0
                            Control.column 1
                            Control.margin (8, 0, 0, 0)
                            yield! conterStyle
                            TextBlock.text "Max Generate Items:"
                        ]
                        NumericUpDown.create [
                            Control.row 0
                            Control.column 2
                            Control.margin (8, 0, 0, 0)
                            NumericUpDown.value max.Current
                            NumericUpDown.minimum 1
                            NumericUpDown.onValueChanged (int >> max.Set)
                        ]
                        ListBox.create [
                            ListBox.row 1
                            Control.column 0
                            Control.columnSpan 4
                            ListBox.dataItems expanded
                            viewFunc root max
                            |> DataTemplateView<_>.create
                            |> ListBox.itemTemplate
                        ]
                    ]
                ]
        )
#endif