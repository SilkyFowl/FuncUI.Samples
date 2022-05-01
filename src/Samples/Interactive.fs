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
#r "nuget: Avalonia.Desktop"
#r "nuget: JaggerJo.Avalonia.FuncUI"
#r "nuget: JaggerJo.Avalonia.FuncUI.DSL"
#r "nuget: JaggerJo.Avalonia.FuncUI.Elmish"
#r "{Reflection.Assembly.GetEntryAssembly().GetName().Name}.dll"

module Counter =
    open Avalonia.FuncUI
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout

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
                        TextBlock.fontSize 48.0
                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                        TextBlock.text (string state.Current)
                    ]
                ]
            ]
        )


open Avalonia.FuncUI.VirtualDom

Counter.view  |> VirtualDom.create
    """

    let initResult =
        TextBlock.create [
            TextBlock.verticalAlignment VerticalAlignment.Center
            TextBlock.horizontalAlignment HorizontalAlignment.Center
            TextBlock.text "The variable 'view' is displayed as a result of the compile."
        ]
        |> VirtualDom.VirtualDom.create

    let argv = Environment.GetCommandLineArgs()
    let sbOut = new Text.StringBuilder()
    let sbErr = new Text.StringBuilder()
    let inStream = new StringReader("")
    let outStream = new StringWriter(sbOut)
    let errStream = new StringWriter(sbErr)
    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()


    let fsiSession =
        FsiEvaluationSession.Create(
            fsiConfig,
            [| yield "fsi.exe"
               yield! argv
               yield "--noninteractive" |],
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
                    | Choice1Of2 (Some value) -> value.ReflectionValue |> evalResult.Set
                    | Choice1Of2 None -> "null or no result" |> evalResult.Set
                    | Choice2Of2 (exn: exn) ->
                        [| box $"exception %s{exn.Message}" |]
                        |> evalWarnings.Set


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
                            Button.content "eval"
                            Button.onClick (fun _ ->
                                task {
                                    if not <| String.IsNullOrEmpty evalText.Current then
                                        evalInteraction evalText.Current
                                }
                                |> ignore)
                        ]
                    ]
                ]
        )