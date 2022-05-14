#r "paket:
nuget Fake.Core.Target
nuget Fake.Core.Process
nuget Fake.DotNet.Cli
nuget Fake.Core.ReleaseNotes
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.DotNet.Paket
nuget Fake.Tools.Git
nuget Fake.Core.Environment
nuget Fake.Core.UserInput
nuget Fake.IO.FileSystem
nuget Fake.DotNet.MsBuild
nuget Fake.Api.GitHub
nuget Octokit < 0.50  //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.Tools
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Api
open System
open System.IO
open System.IO.Compression


// make ZipFile available
#r "System.IO.Compression.FileSystem.dll"

let releaseNotes = ReleaseNotes.load "RELEASE_NOTES.md"

Target.create "Default" ignore

Target.create "Clean" (fun _ ->
    let exitCode = Shell.Exec("dotnet", "clean")

    if exitCode <> 0 then
        failwith "dotnet clean failed")

Target.create "Build" (fun _ ->
    let exitCode = Shell.Exec("dotnet", "build")

    if exitCode <> 0 then
        failwith "dotnet build failed")

Target.create "ReleaseBuild" (fun _ ->
    let exitCode = Shell.Exec("dotnet", "build -c Release")

    if exitCode <> 0 then
        failwith "dotnet build failed")

Target.create "BuildAnalyzer" (fun _ ->
    let analyzerProject = "src" </> "Avalonia.FuncUI.Analyzer"
    let outputDirectory = __SOURCE_DIRECTORY__ </> "analyzers"

    let args =
        [ "pack"
          "--configuration Release"
          sprintf "/p:PackageVersion=%s" releaseNotes.NugetVersion
          sprintf "/p:PackageReleaseNotes=\"%s\"" (String.concat "\n" releaseNotes.Notes)
          sprintf "--output %s" outputDirectory ]

    Shell.cleanDir outputDirectory

    // create initial nuget package
    let exitCode = Shell.Exec("dotnet", String.concat " " args, analyzerProject)

    if exitCode <> 0 then
        failwith "dotnet pack failed"
    else
        match Shell.Exec("dotnet", "publish --configuration Release --framework net6.0", analyzerProject) with
        | 0 ->
            let nupkg =
                System.IO.Directory.GetFiles outputDirectory
                |> Seq.head
                |> IO.Path.GetFullPath

            let nugetParent = DirectoryInfo(nupkg).Parent.FullName
            let nugetFileName = IO.Path.GetFileNameWithoutExtension(nupkg)

            let publishPath =
                analyzerProject
                </> "bin"
                </> "Release"
                </> "net6.0"
                </> "publish"
            // Unzip the nuget
            ZipFile.ExtractToDirectory(nupkg, nugetParent </> nugetFileName)
            // delete the initial nuget package
            File.Delete nupkg
            // remove stuff from ./lib/net6.0
            Shell.deleteDir (
                nugetParent
                </> nugetFileName
                </> "lib"
                </> "net6.0"
            )
            // move the output of publish folder into the ./lib/net6.0 directory
            Shell.copyDir
                (nugetParent
                 </> nugetFileName
                 </> "lib"
                 </> "net6.0")
                publishPath
                (fun _ -> true)
        // // re-create the nuget package
        // ZipFile.CreateFromDirectory(nugetParent </> nugetFileName, nupkg)
        // // delete intermediate directory
        // Shell.deleteDir(nugetParent </> nugetFileName)
        | _ -> failwith "dotnet publish failed")

"Clean" ==> "Build" ==> "Default"
"Clean" ==> "ReleaseBuild"
"Clean" ==> "BuildAnalyzer"

// start build
Target.runOrDefault "Default"