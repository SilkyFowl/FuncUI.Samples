namespace Samples

/// 参考
/// https://fsharp.github.io/fsharp-compiler-docs/fcs/interactive.html
/// https://github.com/fsprojects/Avalonia.FuncUI/issues/147
module Interactive =
    open System
    open System.IO
    open FSharp.Compiler.Interactive.Shell
    open Avalonia.Controls
    open Avalonia.Media
    open Avalonia.Layout
    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL

    let initText =
        $"""
module Counter =
    open Avalonia.FuncUI
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    open Avalonia.Media

    let view =
        Component.create("Counter",fun ctx ->
            let state = ctx.useState 0
            DockPanel.create [
                DockPanel.verticalAlignment VerticalAlignment.Center
                DockPanel.horizontalAlignment HorizontalAlignment.Center
                DockPanel.children [
                    Button.create [
                        Button.width 64
                        Button.horizontalAlignment HorizontalAlignment.Center
                        Button.horizontalContentAlignment HorizontalAlignment.Center
                        Button.content "Reset"
                        Button.onClick (fun _ -> state.Set 0)
                        Button.dock Dock.Bottom
                    ]
                    Button.create [
                        Button.width 64
                        Button.horizontalAlignment HorizontalAlignment.Center
                        Button.horizontalContentAlignment HorizontalAlignment.Center
                        Button.content "-"
                        Button.onClick (fun _ -> state.Current - 1 |> state.Set)
                        Button.dock Dock.Bottom
                    ]
                    Button.create [
                        Button.width 64
                        Button.horizontalAlignment HorizontalAlignment.Center
                        Button.horizontalContentAlignment HorizontalAlignment.Center
                        Button.content "+"
                        Button.onClick (fun _ -> state.Current + 1 |> state.Set)
                        Button.dock Dock.Bottom
                    ]
                    TextBlock.create [
                        TextBlock.dock Dock.Top
                        TextBlock.foreground Brushes.White
                        TextBlock.fontSize 48.0
                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                        TextBlock.text (string state.Current)
                    ]
                ]
            ]
        )

Counter.view
    """

    let initResult =
        TextBlock.create [
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.horizontalAlignment HorizontalAlignment.Center
            TextBlock.text "Results are displayed here."
        ]
        |> VirtualDom.VirtualDom.create

    let argv = Environment.GetCommandLineArgs()
    let sbOut = new Text.StringBuilder()
    let sbErr = new Text.StringBuilder()
    let inStream = new StringReader("")
    let outStream = new StringWriter(sbOut)
    let errStream = new StringWriter(sbErr)
    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()

    /// F# インタラクティブに、このアプリのアセンブリと依存アセンブリを読み込むための引数。
    let references =
        let asm = Reflection.Assembly.GetEntryAssembly()

        let deps =
            asm.GetReferencedAssemblies()
            |> Seq.map (fun asm -> asm.Name)

        let path = (Directory.GetParent asm.Location).FullName

        Directory.EnumerateFiles(path, "*.dll")
        |> Seq.filter (fun p -> Seq.contains (Path.GetFileNameWithoutExtension p) deps)
        |> Seq.map (fun p -> $"-r:{Path.GetFileName p}")
        |> Seq.append (Seq.singleton $"-r:{asm.GetName().Name}")

    let fsiSession =
        FsiEvaluationSession.Create(
            fsiConfig,
            [| yield "fsi.exe"
               yield! argv
               yield "--noninteractive"
               // 参照が F# インタラクティブ プロセスによってロックされないようにする。
               yield "--shadowcopyreferences+"
               yield! references |],
            inStream,
            outStream,
            errStream
        )

    let view =
        Component.create (
            "interactive",
            fun ctx ->

                let evalText = ctx.useState (initText, false)
                let evalResult = ctx.useState (box initResult)
                let evalWarnings = ctx.useState ([||], false)

                let evalInteraction text =
                    let res, warnings = fsiSession.EvalInteractionNonThrowing(text)

                    warnings
                    |> Array.map (fun w -> box $"Warning {w.Message} at {w.StartLine},{w.StartColumn}")
                    |> evalWarnings.Set

                    match res with
                    | Choice1Of2 (Some value) ->
                        match value.ReflectionValue with
                        | :? Types.IView as view ->
                            VirtualDom.VirtualDom.create view
                            |> evalResult.Set
                        | other -> other |> evalResult.Set
                    | Choice1Of2 None -> "null or no result" |> evalResult.Set
                    | Choice2Of2 (exn: exn) ->
                        [| box $"exception %s{exn.Message}" |]
                        |> evalWarnings.Set

                let tryEvalText text =
                    if not <| String.IsNullOrEmpty text then
                        task { evalInteraction text } |> ignore

                ctx.useEffect (
                    (fun _ ->
                        evalText.Observable
                        |> Observable.subscribe tryEvalText),
                    [ EffectTrigger.AfterInit ]
                )

                Grid.create [
                    Grid.rowDefinitions "*,4,*,Auto"
                    Grid.children [
                        TextBox.create [
                            TextBox.row 0
                            TextBox.acceptsReturn true
                            TextBox.textWrapping TextWrapping.Wrap
                            TextBox.text evalText.Current
                            TextBox.onTextChanged evalText.Set
                            if Array.isEmpty evalWarnings.Current then
                                TextBox.errors evalWarnings.Current
                        ]
                        GridSplitter.create [ Grid.row 1 ]
                        ContentControl.create [
                            Border.row 2
                            ContentControl.content evalResult.Current
                        ]
                        Button.create [
                            Button.row 3
                            Button.content "eval manualy"
                            Button.onClick (fun _ -> tryEvalText evalText.Current)
                        ]
                    ]
                ]
        )