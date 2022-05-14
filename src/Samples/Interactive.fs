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

    open Avalonia.FuncUI
    open Avalonia.FuncUI.VirtualDom

    /// Evalを行う。
    let evalInteraction
        isCollectText
        (fsiSession: FsiEvaluationSession)
        (log: string -> unit)
        (evalText: IWritable<string>)
        (evalWarings: IWritable<_>)
        (evalResult: IWritable<obj>)
        =
        let time = DateTime.Now.ToString "T"

        if isCollectText evalText.Current then
            let res, warnings = fsiSession.EvalInteractionNonThrowing evalText.Current

            warnings
            |> Array.map (fun w -> box $"Warning {w.Message} at {w.StartLine},{w.StartColumn}")
            |> evalWarings.Set

            match res with
            | Choice1Of2 (Some value) ->

                log $"{time} Eval Success."

                match value.ReflectionValue with
                | :? Types.IView as view ->
                    fun _ ->
                        VirtualDom.create view
                        |> evalResult.Set
                    |> Avalonia.Threading.Dispatcher.UIThread.Post
                | other -> other |> evalResult.Set
            | Choice1Of2 None -> log $"{time} Null or no result."
            | Choice2Of2 (exn: exn) ->
                log $"{time} Eval Failed."

                [| box $"exception %s{exn.Message}" |]
                |> evalWarings.Set
        else
            [| box "not collect file..." |] |> evalWarings.Set
            log $"{time} no collect file.."


module MessagePack =
    open System
    open System.Net.Sockets
    open System.Net.NetworkInformation
    open System.Threading
    open MessagePack
    open Avalonia.FuncUI.Analyzer
    open Avalonia.FuncUI.Analyzer.MessagePack

    let isActiveTcpListener ipAddredd port =
        IPGlobalProperties
            .GetIPGlobalProperties()
            .GetActiveTcpListeners()
        |> Array.exists (fun ep -> ep.Address = ipAddredd && ep.Port = port)

    /// ``FuncUiAnalyzer`への接続が成功するまで`ConnectAsync`を行う。
    /// `SocketException`以外のエラーになった場合は中断。
    let tryConnectAsync (log: string -> unit) (client: TcpClient) retryMilliseconds =
        task {
            let mutable hasConnedted = false

            while not hasConnedted do
                try
                    do! client.ConnectAsync(address = Server.iPAddress, port = Server.port)
                    hasConnedted <- true
                with
                | :? SocketException as e ->
                    log $"{e.SocketErrorCode}: {e.Message}"
                    do! Tasks.Task.Delay(millisecondsDelay = retryMilliseconds)
        }

    /// `FuncUiAnalyzer`からデータを購読するためのクライアント。
    let initClient (log: string -> unit) (setEvalText: string -> unit) =
        let cts = new CancellationTokenSource()

        let token = cts.Token

        task {
            use client = new TcpClient()

            while isActiveTcpListener Server.iPAddress Server.port
                  |> not do
                log "FuncUiAnalyzer server is not Actice..."
                do! Tasks.Task.Delay 1000

            log "start connect..."
            do! tryConnectAsync log client 1000
            log "Connedted!!"
            use reader = new MessagePackStreamReader(client.GetStream())

            while not token.IsCancellationRequested do
                let! result = reader.ReadAsync token
                log $"read: {result}"

                match ValueOption.ofNullable result with
                | ValueSome buff ->
                    let (Code content) =
                        MessagePackSerializer.Deserialize<MessagePack.Msg>(&buff, options)

                    content
                    |> Array.filter (fun line -> line.StartsWith("#r") |> not)
                    |> String.concat "\n"
                    |> setEvalText

                | ValueNone -> ()
        }
        |> ignore

        { new IDisposable with
            member _.Dispose() = cts.Cancel() }


open System
open Avalonia.Controls
open Avalonia.Media
open Avalonia.Layout
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL

type StateStore =
    { EvalText: IWritable<string>
      EvalResult: IWritable<obj>
      EvalWarings: IWritable<obj []>
      Status: IWritable<string> }

module StateStore =
    open System.Text.RegularExpressions
    let private fsiSession = FsiSession.create ()

    /// この文字列が含まれていたらEvalする。
    [<Literal>]
    let MatchText = "//#funcuianalyzer"

    /// Evalするかを判定する。
    let isCollectText text =
        not <| String.IsNullOrEmpty text
        && Regex.IsMatch(text, MatchText)

    /// `state`の情報に基づいてEvalする。
    let evalInteraction state =
        FsiSession.evalInteraction
            isCollectText
            fsiSession
            state.Status.Set
            state.EvalText
            state.EvalWarings
            state.EvalResult

    /// `evalInteraction`の非同期版。
    let evalInteractionAsync state _ =
        async { evalInteraction state }
        |> Async.StartImmediate

    /// `StateStore`を初期化する。
    let init () =
        let initText =
            $"""
{MatchText}
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
          EvalWarings = new State<_>([||])
          Status = new State<_> "" }

/// `Interactive`のStore。
/// ※本来、Storeはアプリケーション一つだけであるのが望ましい。
let shared = StateStore.init ()

let client = MessagePack.initClient shared.Status.Set shared.EvalText.Set

let view =

    Component.create (
        "interactive-file",
        fun ctx ->

            // sharedの購読
            let evalText = ctx.usePassed (shared.EvalText, false)
            let evalResult = ctx.usePassed (shared.EvalResult)
            let evalWarnings = ctx.usePassed (shared.EvalWarings)
            let status = ctx.usePassed shared.Status

            /// `true`ならEvalTextが更新されたら自動でEvalする。
            let autoEval = ctx.useState true
            /// `true`ならEvalTextを表示する。
            let showEvalText = ctx.useState false

            ctx.trackDisposable client

            /// Evalを実行する。
            let evalInteractionAsync _ =
                StateStore.evalInteractionAsync shared ()

            ctx.useEffect (
                (fun _ ->
                    evalText.Observable
                    |> Observable.subscribe (fun _ ->
                        if autoEval.Current then
                            evalInteractionAsync ())),
                [ EffectTrigger.AfterInit ]
            )

            Grid.create [
                Grid.rowDefinitions "Auto,*,4,*,Auto"
                Grid.columnDefinitions "Auto,*,Auto"
                Grid.children [
                    CheckBox.create [
                        CheckBox.row 0
                        CheckBox.column 0
                        CheckBox.content "Show EvalText"
                        CheckBox.isChecked showEvalText.Current
                        CheckBox.onChecked (fun _ -> showEvalText.Set true)
                        CheckBox.onUnchecked (fun _ -> showEvalText.Set false)
                    ]
                    TextBox.create [
                        TextBox.isVisible showEvalText.Current
                        if showEvalText.Current then
                            TextBox.row 1
                            TextBox.column 0
                            TextBox.columnSpan 3
                            TextBox.acceptsReturn true
                            TextBox.textWrapping TextWrapping.Wrap
                            TextBox.text evalText.Current
                            TextBox.onTextChanged evalText.Set

                            if not <| Array.isEmpty evalWarnings.Current then
                                TextBox.errors evalWarnings.Current
                    ]
                    GridSplitter.create [
                        GridSplitter.isVisible showEvalText.Current
                        if showEvalText.Current then
                            GridSplitter.row 2
                            GridSplitter.column 0
                            GridSplitter.columnSpan 3
                    ]
                    ContentControl.create [
                        if showEvalText.Current then
                            Border.row 3
                        else
                            Border.row 1
                            Border.rowSpan 3
                        TextBox.column 0
                        TextBox.columnSpan 3
                        TextBox.column 0
                        ContentControl.content evalResult.Current
                    ]
                    CheckBox.create [
                        CheckBox.row 4
                        CheckBox.column 0
                        CheckBox.content "Auto EvalText"
                        CheckBox.isChecked autoEval.Current
                        CheckBox.onChecked (fun _ -> autoEval.Set true)
                        CheckBox.onUnchecked (fun _ -> autoEval.Set false)
                    ]
                    Button.create [
                        Button.row 4
                        TextBox.column 1
                        Button.horizontalAlignment HorizontalAlignment.Left
                        Button.content "eval manualy"
                        Button.onClick evalInteractionAsync
                    ]
                    TextBlock.create [
                        TextBlock.row 5
                        TextBlock.column 2
                        TextBlock.text status.Current
                    ]
                ]
            ]
    )