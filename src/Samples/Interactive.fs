module Samples.Interactive

/// 参考
/// https://fsharp.github.io/fsharp-compiler-docs/fcs/interactive.html
/// https://github.com/fsprojects/Avalonia.FuncUI/issues/147
module FsiSession =
    open System
    open System.IO
    open FSharp.Compiler.Interactive.Shell

    let create () =
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

        FsiEvaluationSession.Create(
            fsiConfig,
            [| yield "fsi.exe"
               yield! argv
               yield "--noninteractive"
               // 参照が F# インタラクティブ プロセスによってロックされないようにする。
               yield "--shadowcopyreferences+"
               yield "-d:PREVIEW"
               yield! references |],
            inStream,
            outStream,
            errStream
        )

open System
open Avalonia.Controls
open Avalonia.Media
open Avalonia.Layout
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL


type StateStore =
    { EvalText: IWritable<string>
      EvalResult: IWritable<obj>
      EvalWarings: IWritable<obj []> }

module StateStore =
    open System.Text.RegularExpressions
    let private fsiSession = FsiSession.create ()

    let isCollectText text =
        not <| String.IsNullOrEmpty text
        && Regex.IsMatch(text, "//#funcuianalyzer")


    let evalInteraction state =
        if isCollectText state.EvalText.Current then
            let res, warnings = fsiSession.EvalInteractionNonThrowing state.EvalText.Current

            warnings
            |> Array.map (fun w -> box $"Warning {w.Message} at {w.StartLine},{w.StartColumn}")
            |> state.EvalWarings.Set

            match res with
            | Choice1Of2 (Some value) ->
                match value.ReflectionValue with
                | :? Types.IView as view ->
                    Avalonia.Threading.Dispatcher.UIThread.Post (fun _ ->
                        VirtualDom.VirtualDom.create view
                        |> state.EvalResult.Set)
                | other -> other |> state.EvalResult.Set
            | Choice1Of2 None -> "null or no result" |> state.EvalResult.Set
            | Choice2Of2 (exn: exn) ->
                [| box $"exception %s{exn.Message}" |]
                |> state.EvalWarings.Set
        else
            [| box "not collect file..." |]
            |> state.EvalWarings.Set

    let evalInteractionAsync state _ =
        async { evalInteraction state }
        |> Async.StartImmediate

    let init () =
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

        { EvalText = new State<_>(initText)
          EvalResult = new State<_>(box initResult)
          EvalWarings = new State<_>([||]) }

let shared = StateStore.init ()

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open System.Threading

/// https://suave.io/index.html
let surve =
    let app =
        PUT
        >=> path "/funcuianalyzer"
        >=> request (fun r ->
            r.form
            |> Map.ofList
            |> Map.tryFind "eval"
            |> Option.flatten
            |> Option.iter shared.EvalText.Set

            OK("PUT Received."))

    let cts = new CancellationTokenSource()
    let conf = { defaultConfig with cancellationToken = cts.Token }
    let listening, server = startWebServerAsync conf app
    Async.Start(server, cts.Token)

    { new IDisposable with
        member _.Dispose() = cts.Cancel() }

let view =

    Component.create (
        "interactive-file",
        fun ctx ->

            let evalText = ctx.usePassed (shared.EvalText, false)
            let evalResult = ctx.usePassed (shared.EvalResult)
            let evalWarnings = ctx.usePassed (shared.EvalWarings)

            ctx.trackDisposable surve

            let evalInteractionAsync _ =
                StateStore.evalInteractionAsync shared ()

            ctx.useEffect (
                (fun _ ->
                    evalText.Observable
                    |> Observable.subscribe evalInteractionAsync),
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
                        if not <| Array.isEmpty evalWarnings.Current then
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
                        Button.onClick evalInteractionAsync
                    ]
                ]
            ]
    )