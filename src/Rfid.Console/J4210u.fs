namespace Rfid
open System.Runtime.InteropServices
open System.Text

module J4210U =
    open System
    module Native =
        [<Literal>]
        let Library = "j4210u"

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern int16 AvailablePorts(byte[] ports);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte OpenComPort(byte[] port, int baud);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern void CloseComPort();

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte LoadSettings(byte[] buff);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern int Inventory(byte filter);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte GetResult(byte[] scanresult, int index);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte GetTID(byte[] epc, byte epclen, byte[] tid, byte[] tidlen);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte SetPassword(byte[] epc, byte epcLen, byte[] pass, byte size);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte SetKillPassword(byte[] epc, byte epcLen, byte[] pass, byte size);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern void LastError(byte[] buffer);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte Auth(byte[] password, byte size);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte WriteMemWord(byte[] epc, byte epclen, byte[] data, byte windex);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte ReadmemWord(byte[] epc, byte epclen, byte[] data, byte windex);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte SetFilter(int maskAdrInByte, int maskLenInByte, byte[] maskDataByte);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte TagExists(byte[] epc, byte epclen);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte WriteEpcWord(byte[] epc, byte epclen, byte[] data, byte windex);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte GetTagInfo(byte[] tid, byte[] info);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte GetGPI(byte gpino);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte SetGPO(byte gpono);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern void LibVersion(byte[] version);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte SetQ(byte q);

        [<DllImport(Library, CallingConvention = CallingConvention.Cdecl)>]
        extern byte SetSession(byte session);

    // Start the F# Wrapping Functions
    type ScanResult =
        struct
            val Ant: int
            val RSSI: int
            val Count: int
            val EpcLength: int
            val Epc: byte[]
            new (ant: int, rssi: int, count: int, epcLength: int, epc: byte[]) =
                {Ant = ant; RSSI = rssi; Count = count; EpcLength = epcLength; Epc = epc;}
        end

    let getScanResult (bb: byte[]) =
        match Array.length bb < 64 with
        | true -> Error "Given byte array needs to have a size of 64 or greater"
        | _ ->
            let ant = bb.[0] |> int
            let rssi = bb.[1] |> int
            let count = bb.[2] |> int
            let epcLength = bb.[3] |> int
            Ok(ScanResult(
                ant,
                rssi,
                count,
                epcLength,
                bb.[4..16]
            ))
    
    let scanResultToString (sr: ScanResult) =
        let epc = Encoding.UTF8.GetString sr.Epc
        String.Format("Ant: {0}, RSSI: {1}, Count: {2}, EpcLength: {3}, Epc: {4}", sr.Ant, sr.RSSI, sr.Count, sr.EpcLength, epc)

    type ReaderInfo =
        struct
            val Serial: int
            val Major: byte
            val Minor: byte
            val Antenna: byte[]
            val ComAdr: byte[]
            val ReaderType: byte[]
            val Protocol: byte[]
            val Band: byte[]
            val Power: byte[]
            val ScanTime: byte[]
            val BeepOn: byte[]
            val MaxFreq: int
            val MinFreq: int
            val BaudRate: int
            // Create initializing constructor
            new (serial: int, major: byte, minor: byte, antenna: byte[], comAdr: byte[], readerType: byte[], protocol: byte[], band: byte[], power: byte[], scanTime: byte[], beepOn: byte[], maxFreq: int, minFreq: int, baudRate: int) =
                {Serial = serial; Major = major; Minor = minor; Antenna = antenna; ComAdr = comAdr; ReaderType = readerType; Protocol = protocol; Band = band; Power = power; ScanTime = scanTime; BeepOn = beepOn; MaxFreq = maxFreq; MinFreq = minFreq; BaudRate = baudRate;}
        end

    let getReaderInfo (bytes: byte[]) =
        let serial = bytes[0..4] |> BitConverter.ToInt32
        let major = bytes.[4]
        let minor = bytes.[5]
        let antenna = bytes[6..7]
        let comAdr = bytes[7..8]
        let readerType = bytes[8..9]
        let protocol = bytes[9..10]
        let band = bytes[10..11]
        let power = bytes[11..12]
        let scanTime = bytes[12..13]
        let beepOn = bytes[13..14]
        let maxFreq = bytes[16..19] |> BitConverter.ToInt32
        let minFreq = bytes[20..23] |> BitConverter.ToInt32
        let baudRate = bytes[24..27] |> BitConverter.ToInt32
        ReaderInfo(serial, major, minor, antenna, comAdr, readerType, protocol, band, power, scanTime, beepOn, maxFreq, minFreq, baudRate)

    // Returns available COM ports as a string array
    let availablePorts =
        let buff = Array.zeroCreate 2048
        match Native.AvailablePorts(buff) <= int16 0 with
        | true-> Array.empty
        | _ ->
            let str = Encoding.UTF8.GetString buff
            str.Split [|'\n'|]

    // Opens then given COM port at the given baud rate
    let openPort (baud: int, port: string) =
        let bytes = Encoding.ASCII.GetBytes port
        match Array.length bytes <= 0 with
        | true -> Error "Port name is empty"
        | _ -> 
            match Native.OpenComPort(bytes, baud) > byte 0 with
                | true  -> Ok port
                | _ -> Error "Unable to open port"
    
    // Loads the settings from the reader
    let loadSettings =
        let buff = Array.zeroCreate 2048
        match Native.LoadSettings(buff) > byte 0 with
        | true -> Ok buff
        | _ -> Error "Unable to load settings"

    // Number of tags detected or 0 if none.
    let inventory =
        byte 0 |> Native.Inventory

    let getResult index =
        let buff = Array.zeroCreate 64
        match Native.GetResult(buff, index) <= byte 0 with
        | true -> None
        | _ ->
            match getScanResult buff with
            | Error e -> None
            | Ok sr -> Some sr