namespace Samples

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

    let length = fold (fun i _ -> i + 1) (fun i _ -> i + 1) 0

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

/// 大量のデータを扱えるTreeViewっぽいもの。
/// AvaloniaのTreeViewは仮想化できないので大量のデータがあると遅い。
/// https://github.com/AvaloniaUI/Avalonia/issues/6580
///
/// 対応するには仮想化できるItemRepeaterを使用する。
/// https://github.com/kekekeks/example-avalonia-huge-tree/blob/master/AvaloniaHugeTree/MainWindow.xaml#L19-L36
///
/// この実装はコード量削減のためListBoxを使用。
///
module TreeBox =
    open System
    open Avalonia
    open Avalonia.Controls
    open Avalonia.Controls.Primitives
    open Avalonia.Controls.Templates
    open Avalonia.Styling
    open Avalonia.Media
    open Avalonia.Layout
    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL

    type TemplatedControl with
        static member template(viewFunc: ITemplatedControl -> INameScope -> 'view) =
            FuncControlTemplate(fun x scope -> viewFunc x scope |> VirtualDom.VirtualDom.create)
            |> TemplatedControl.template

    type ExpandedTreeNode<'node> = { IsExpand: bool; Node: 'node }

    type ExpandedTreeItem<'leaf, 'node> =
        { Ids: Guid list
          Data: Choice<'leaf, ExpandedTreeNode<'node>> }

    let expandTree fNodeId (expandedSet: Set<Guid>) (trees: Tree<'leaf, 'node> seq) =
        let rec loop state trees' =
            trees'
            |> Seq.map (function
                | LeafNode leaf -> seq { { Ids = state; Data = Choice1Of2 leaf } }
                | InternalNode (node, subTree) ->
                    seq {
                        let nodeId = fNodeId node
                        let isExpand = expandedSet |> Set.contains nodeId

                        { Ids = state
                          Data = Choice2Of2 { IsExpand = isExpand; Node = node } }


                        if expandedSet |> Set.contains nodeId then
                            yield! loop (state @ List.singleton nodeId) subTree
                    })
            |> Seq.concat

        loop [] trees

    let create<'leaf, 'node>
        fNodeId
        fLeaf
        (fNode: _ -> _ -> Types.IView<_>)
        (items: IWritable<Tree<'leaf, 'node> seq>)
        attrs
        =
        Component.create (
            "tree-box",
            fun ctx ->
                let expandIdSet = ctx.useState (Set.empty, false)
                let expandItems = ctx.useState Seq.empty

                ctx.useEffect (
                    (fun _ ->
                        expandTree fNodeId expandIdSet.Current items.Current
                        |> expandItems.Set),
                    [ EffectTrigger.AfterInit
                      EffectTrigger.AfterChange items
                      EffectTrigger.AfterChange expandIdSet ]
                )

                ctx.attrs attrs

                let viewFunc (data: ExpandedTreeItem<'leaf, 'node>) =
                    let left = 4 + (24 * List.length data.Ids)

                    Grid.create [
                        Grid.margin (left, 4, 4, 4)
                        match data.Data with
                        | Choice1Of2 leaf -> Grid.children [ fLeaf data.Ids leaf ]
                        | Choice2Of2 node ->
                            let nodeId = fNodeId node.Node

                            let setExpand isExpand _ =
                                let newValue =
                                    if isExpand then
                                        expandIdSet.Current |> Set.add nodeId
                                    else
                                        expandIdSet.Current |> Set.remove nodeId

                                expandIdSet.Set newValue

                            Grid.columnDefinitions "Auto,Auto,*"

                            Grid.children [
                                ToggleButton.create [
                                    ToggleButton.column 0
                                    ToggleButton.isChecked node.IsExpand
                                    ToggleButton.onChecked (setExpand true)
                                    ToggleButton.onUnchecked (setExpand false)
                                    // 参考
                                    // https://github.com/AvaloniaUI/Avalonia/blob/master/src/Avalonia.Themes.Default/Controls/TreeViewItem.xaml#L43-L58
                                    ToggleButton.template (fun x scope ->
                                        Border.create [
                                            Border.height 14
                                            Border.width 12
                                            Border.verticalAlignment VerticalAlignment.Center
                                            Border.margin (0, 0, 8, 0)
                                            Border.background Brushes.Transparent
                                            Border.child (
                                                Shapes.Path(
                                                    HorizontalAlignment = HorizontalAlignment.Center,
                                                    VerticalAlignment = VerticalAlignment.Center,
                                                    Fill = x.GetValue ContentControl.ForegroundProperty,
                                                    Data = Geometry.Parse "M 0 2 L 4 6 L 0 10 Z"
                                                )
                                            )
                                            if node.IsExpand then
                                                RotateTransform 45.0 |> Border.renderTransform
                                        ])
                                ]
                                Border.create [
                                    Border.column 1
                                    Border.child (fNode data.Ids node)
                                ]
                            ]
                    ]

                ListBox.create [
                    ListBox.dataItems expandItems.Current
                    DataTemplateView<_>.create viewFunc
                    |> ListBox.itemTemplate
                ]
        )

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

    let getId (item: ShareItem) =
        match item with
        | LeafNode info -> info.Id
        | InternalNode (kind, _) -> kind.Id

    let setSubtrees targetId subtrees (tree: ShareItem) =
        tree
        |> Tree.setSubtrees (fun kind -> kind.Id = targetId) (fun _ -> subtrees)

    module Fakar =

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



module HugeTree =
    open Avalonia.Controls
    open Avalonia.Media
    open Avalonia.Layout
    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL

    type ExpandedTreeKind = TreeBox.ExpandedTreeNode<Share.Kind>
    let headerfontSize = 20

    let leafView ids (leaf: Share.Info) =
        Grid.create [
            Grid.rowDefinitions "Auto,Auto"
            Grid.children [
                TextBlock.create [
                    TextBlock.row 0
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    Control.margin (0, 0, 0, 4)
                    TextBlock.fontSize headerfontSize
                    TextBlock.text leaf.Name
                ]
                TextBlock.create [
                    TextBlock.row 1
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.textWrapping TextWrapping.WrapWithOverflow
                    TextBlock.text leaf.Description
                ]
            ]
        ]


    let nodeView addSubItems ids (node: ExpandedTreeKind) =
        Grid.create [
            Grid.columnDefinitions "Auto,*"
            Grid.children [
                Button.create [
                    Button.column 0
                    Button.content "Genetate"
                    Button.onClick (addSubItems ids node)
                ]

                TextBlock.create [
                    TextBlock.column 1
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    Control.margin (8, 0, 0, 0)
                    TextBlock.fontSize headerfontSize
                    TextBlock.text node.Node.Name
                ]
            ]
        ]

    let shareTreeBox addSubItems =
        TreeBox.create<Share.Info, Share.Kind> (fun kind -> kind.Id) leafView (nodeView addSubItems)

    let view =
        Component.create (
            "huge-tree",
            fun ctx ->
                let initItems = 100
                let max = ctx.useState (5000, false)
                let roots = Share.Fakar.generate initItems |> ctx.useState

                let resetRoots _ =
                    Share.Fakar.generate initItems |> roots.Set

                let addSubItems ids (node: ExpandedTreeKind) _ =
                    let targetId =
                        if List.isEmpty ids then
                            node.Node.Id
                        else
                            List.head ids

                    let (rootIdx, root) =
                        roots.Current
                        |> Seq.indexed
                        |> Seq.find (fun (_, i) -> Share.getId i = targetId)


                    let newValue =
                        Share.setSubtrees node.Node.Id (Share.Fakar.generate max.Current) root

                    roots.Current
                    |> Seq.updateAt rootIdx newValue
                    |> roots.Set

                Grid.create [
                    Grid.rowDefinitions "Auto,*"
                    Grid.columnDefinitions "Auto,Auto,Auto,*,Auto"
                    Grid.children [
                        TextBlock.create [
                            Control.row 0
                            Control.column 0
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.text $"Count: {Seq.sumBy Tree.length roots.Current}"
                        ]
                        TextBlock.create [
                            Control.row 0
                            Control.column 1
                            Control.margin (8, 0, 0, 0)
                            TextBlock.verticalAlignment VerticalAlignment.Center
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
                        Button.create [
                            Control.row 0
                            Control.column 4
                            Button.content "Reset"
                            Button.onClick resetRoots
                        ]
                        shareTreeBox
                            addSubItems
                            roots
                            [ Component.row 1
                              Component.column 0
                              Component.columnSpan 5 ]
                    ]
                ]
        )