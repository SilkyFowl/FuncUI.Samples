namespace Samples

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI
open Avalonia.Themes.Fluent

/// This is your application you can ose the initialize method to load styles
/// or handle Life Cycle events of your application
type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme(baseUri = null, Mode = FluentThemeMode.Dark))
        this.Styles.Load "avares://Samples/Styles.xaml"

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- Shell.MainWindow()
        | _ -> ()


module Remote =
    open System.Net
    open System.Net.Sockets
    open Avalonia.Controls
    open Avalonia.Controls.Remote
    open Avalonia.Remote.Protocol
    open Avalonia.Threading

    /// 参考:https://github.com/AvaloniaUI/Avalonia/blob/cba80543048c9f566a17f657d93a5bf22b4006fa/samples/RemoteDemo/Program.cs
    /// 画面は出るがまともに動作しない……
    /// とりあえず書き置き
    let remote (args: string []) =
        let _ =
            AppBuilder
                .Configure<App>()
                .UsePlatformDetect()
                .SetupWithoutStarting()

        let l = TcpListener(IPAddress.Loopback, 0)
        l.Start()
        let port = (l.LocalEndpoint :?> IPEndPoint).Port
        l.Stop()
        let transport = new BsonTcpTransport()

        let _ =
            transport.Listen(
                IPAddress.Loopback,
                port,
                fun sc ->
                    fun _ -> (RemoteServer sc).Content <- Shell.view
                    |> Dispatcher.UIThread.Post
            )

        let cts = new System.Threading.CancellationTokenSource()

        let _ =
            task {
                let! t = transport.Connect(IPAddress.Loopback, port)

                fun _ ->
                    let window = Window(Content = RemoteWidget t)
                    window.Closed |> Event.add (fun _ -> cts.Cancel())
                    window.Show()
                |> Dispatcher.UIThread.Post
            }

        Dispatcher.UIThread.MainLoop cts.Token
        0

module Program =

    let debugLogArea: string [] =
        [|
        // "TreeBox"
        |]

    let level, area =
        if Array.isEmpty debugLogArea then
            Logging.LogEventLevel.Error, [||]
        else
            Logging.LogEventLevel.Debug, debugLogArea

    [<EntryPoint>]
    let main (args: string []) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .LogToTrace(level, area)
            .StartWithClassicDesktopLifetime(args)