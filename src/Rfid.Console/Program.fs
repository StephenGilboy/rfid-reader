open Rfid.J4210U
open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks


let backgroundReader (msgQueue: ConcurrentQueue<string>) (queue: ConcurrentQueue<ScanResult>) (ct: CancellationToken) =
    let startReader =
        msgQueue.Enqueue "Starting Reader" |> ignore
        match availablePorts with
        | [||] -> Error "No available ports"
        | ports ->
            msgQueue.Enqueue "Got Available Ports" |> ignore
            // Write the available ports to the console
            ports
            |> Array.iter (fun p -> msgQueue.Enqueue p |> ignore)
            |> ignore

            let port = ports.[0]
            match openPort (57600, port) with
            | Error e -> Error e
            | _ ->
                // Load Settings and then get Reader Info
                match loadSettings with
                | Error e -> Error e
                | Ok bytes ->
                    let readerInfo = getReaderInfo bytes
                    Ok readerInfo
    let loop =
        if ct.IsCancellationRequested then
            Error "Background Reader Stopped"
        else
            match inventory with 
            | 0 -> Error "No Tags Found"
            | n ->
                for i in 0..n-1 do
                    match getResult i with
                    | None -> ()
                    | Some scanResult -> queue.Enqueue scanResult |> ignore
                Ok n
    
    task {
        match startReader with
        | Error e -> return Error e
        | Ok _ ->
            msgQueue.Enqueue "Reader Started" |> ignore
            while not ct.IsCancellationRequested do
                match loop with
                | Error e -> msgQueue.Enqueue e |> ignore
                | Ok n -> msgQueue.Enqueue (sprintf "Processed %d Tags" n) |> ignore
                Thread.Sleep 500 |> ignore
            return Ok "Background Reader Stopped"
    }




[<EntryPoint>]
let main args =
    let msgQueue = ConcurrentQueue<string>()
    let scanQueue = ConcurrentQueue<ScanResult>()
    let cts = new CancellationTokenSource()

    Console.CancelKeyPress.Add(fun arg -> cts.Cancel false |> ignore)

    async {
        backgroundReader msgQueue scanQueue cts.Token
        |> Async.AwaitTask
        |> Async.Ignore
        |> ignore
    } |> Async.Start
    
    while not cts.IsCancellationRequested do
        let msg = msgQueue.TryDequeue()
        match msg with
        | true, m -> Console.WriteLine m |> ignore
        | _ -> ()
        Thread.Sleep 100
    0