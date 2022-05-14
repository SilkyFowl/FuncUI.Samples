#r "nuget: MessagePack.FSharpExtensions, 3.0.0"

open System
open System.Buffers
open System.Threading
open MessagePack
open MessagePack.Resolvers
open MessagePack.FSharp

[<MessagePackObject>]
type UnionSample =
    | Foo of XYZ: int
    | Bar of OPQ: string list

module UnionSample =
    let dump =
        function
        | Foo x -> printfn "%d" x
        | Bar xs -> printfn "%A" xs

let resolver =
    Resolvers.CompositeResolver.Create(FSharpResolver.Instance, StandardResolver.Instance)

let options = MessagePackSerializerOptions.Standard.WithResolver(resolver)


open System.Net
open System.Net.Sockets


let initServer ipAddress port token : MailboxProcessor<UnionSample> =
    MailboxProcessor.Start(
        (fun inbox ->

            let serializeAsync (client: TcpClient) token value =
                MessagePackSerializer.SerializeAsync(client.GetStream(), value, options, token)
                |> Async.AwaitTask

            let (|SerializedStreamError|_|) (ex: exn) =

                let msg = "Error occurred while writing the serialized data to the stream."

                match ex with
                | :? AggregateException as es ->
                    es.InnerExceptions
                    |> Seq.tryPick (function
                        | :? MessagePackSerializationException as e when e.Message = msg -> Some e
                        | _ -> None)
                | _ -> None

            async {
                let listener = TcpListener(ipAddress, port)
                listener.Start()
                use! __ = Async.OnCancel(fun _ -> listener.Stop())


                let rec loop (client: TcpClient) =
                    async {
                        match! inbox.Receive() with
                        | _ when inbox.CurrentQueueLength <> 0 ->
                            printfn $"Through Message"
                            printfn $"CurrentQueueLength: %A{inbox.CurrentQueueLength}"
                            return! loop client
                        | msg ->
                            printfn "Send Message"
                            printfn $"CurrentQueueLength: %A{inbox.CurrentQueueLength}"

                            try
                                do! serializeAsync client token msg
                                return! loop client
                            with
                            | SerializedStreamError e ->
                                printfn $"%A{e.Message}\nRe-Accept Tcp Client..."
                                client.Dispose()
                                use! client' = listener.AcceptTcpClientAsync() |> Async.AwaitTask
                                return! loop client'
                    }

                use! client = listener.AcceptTcpClientAsync() |> Async.AwaitTask
                do! loop client
            }),
        token
    )



let startClient ipAddress port =
    async {
        use client = new TcpClient()

        do!
            client.ConnectAsync(address = ipAddress, port = port)
            |> Async.AwaitTask

        use reader = new MessagePackStreamReader(client.GetStream())

        let! token = Async.CancellationToken

        let readAsync () =
            task {
                let! result = reader.ReadAsync token

                return
                    match ValueOption.ofNullable result with
                    | ValueSome buff ->
                        MessagePackSerializer.Deserialize<UnionSample>(&buff, options)
                        |> ValueSome
                    | ValueNone -> ValueNone
            }
            |> Async.AwaitTask

        let rec loop () =
            async {
                match! readAsync () with
                | ValueSome v ->
                    UnionSample.dump v
                    return! loop ()
                | ValueNone -> ()
            }

        do! loop ()
    }

let iPAddress = IPAddress.Loopback
let port = 8035

let cts = new CancellationTokenSource()
let cts' = new CancellationTokenSource()

let server = initServer iPAddress port cts.Token

Async.Start(startClient iPAddress port, cts'.Token)
task { return! server.Receive() }

[ Foo 993
  Bar [ "example" ]
  Foo 189872 ]
|> List.iter (fun msg -> server.Post msg)

server
cts'.Cancel()

open System.Net
open System.Net.Sockets
open System.Net.NetworkInformation

IPGlobalProperties
    .GetIPGlobalProperties()
    .GetActiveTcpListeners()
|> Array.tryFind (fun ep -> ep.Address = IPAddress.Loopback && ep.Port = 8080)

let client = new TcpClient()
let c = client.ConnectAsync(IPAddress.Loopback, 8030)
client.Client
