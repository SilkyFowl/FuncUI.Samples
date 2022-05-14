namespace Avalonia.FuncUI.Analyzer

module Server =
    open System.Net

    let iPAddress = IPAddress.Loopback
    let port = 8080

module MessagePack =
    open System
    open System.Net.Sockets
    open System.Threading
    open System.Threading.Tasks
    open MessagePack
    open MessagePack.Resolvers
    open MessagePack.FSharp

    /// FuncUiAnalyzer -> クライアントの通信定義。
    [<MessagePackObject>]
    type Msg = Code of content: string []

    let resolver =
        Resolvers.CompositeResolver.Create(FSharpResolver.Instance, StandardResolver.Instance)

    let options = MessagePackSerializerOptions.Standard.WithResolver(resolver)

    /// クライアントへのデータ送信の失敗による例外なのかを判定するアクティブパターン。
    let inline private (|SerializedStreamError|_|) (ex: exn) =

        let msg = "Error occurred while writing the serialized data to the stream."

        match ex with
        | :? AggregateException as es ->
            es.InnerExceptions
            |> Seq.tryPick (function
                | :? MessagePackSerializationException as e when e.Message = msg -> Some e
                | _ -> None)
        | _ -> None

    /// `TcpClient`に`value`を送信する。
    let inline private serializeAsync (client: TcpClient) token value =
        MessagePackSerializer.SerializeAsync(client.GetStream(), value, options, token)
        |> Async.AwaitTask

    /// `AcceptTcpClientAsync`の結果を`Choice1Of2`でラップして`cont`を評価する。
    let inline private acceptTcpClientAsync (listener: TcpListener) cont =
        async {
            return!
                listener.AcceptTcpClientAsync()
                |> Choice1Of2
                |> cont
        }

    /// `serializeAsync`を実行する。
    /// `SerializedStreamError`に該当する例外した場合、復旧処理として`acceptTcpClientAsync`を実行する。
    let inline private trySerializeAsync cont listener client token value =
        async {
            try
                do! serializeAsync client token value
                return! Choice2Of2 client |> cont
            with
            | SerializedStreamError e -> return! acceptTcpClientAsync listener cont
        }

    /// `TcpListener`を起動して、クライアントの接続を待ち受ける。
    let inline private body ipAddress port token (inbox: MailboxProcessor<Msg>) =
        async {
            let listener = TcpListener(ipAddress, port)

            listener.Start()

            use _ =
                { new IDisposable with
                    member _.Dispose() = listener.Stop() }

            let rec loop (state: Choice<Task<TcpClient>, TcpClient>) =
                async {
                    let! msg = inbox.Receive()

                    match state with
                    | Choice1Of2 t when not t.IsCompleted -> return! loop state
                    | Choice1Of2 t -> return! trySerializeAsync loop listener t.Result token msg
                    | Choice2Of2 client -> return! trySerializeAsync loop listener client token msg
                }

            do! acceptTcpClientAsync listener loop
        }

    let cts = new CancellationTokenSource()

    /// `CurrentDomain`が`fsautocomplete`である場合のみサーバを起動する。
    /// TODO 通信に使う型情報を共有プロジェクトとして切り出す。
    /// TODO IPAddressとPortをあとから設定できる方法を考える。
    let private server =
        if AppDomain.CurrentDomain.FriendlyName = "fsautocomplete" then
            MailboxProcessor.Start(body Server.iPAddress Server.port cts.Token, cts.Token)
            |> Some
        else
            None

    /// サーバが起動してたら`msg`を送信する。
    let post msg =
        match server with
        | Some s -> s.Post msg
        | None -> ()

    // `fsautocomplete`が終了するときにサーバも終了する。
    AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> cts.Cancel())



module Lib =
    open FSharp.Analyzers.SDK
    open MessagePack

    /// AnalyzerでIDEによるF#コードの編集にフックできる。
    /// これを利用してLiveViewを実現する。
    [<Analyzer "FuncUiAnalyzer">]
    let optionValueAnalyzer: Analyzer =
        fun ctx ->
            Code ctx.Content |> post
            []