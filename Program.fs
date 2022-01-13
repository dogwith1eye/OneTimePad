let cts = [
    "BB3A65F6F0034FA957F6A767699CE7FABA855AFB4F2B520AEAD612944A801E";
    "BA7F24F2A35357A05CB8A16762C5A6AAAC924AE6447F0608A3D11388569A1E";
    "A67261BBB30651BA5CF6BA297ED0E7B4E9894AA95E300247F0C0028F409A1E";
    "A57261F5F0004BA74CF4AA2979D9A6B7AC854DA95E305203EC8515954C9D0F";
    "BB3A70F3B91D48E84DF0AB702ECFEEB5BC8C5DA94C301E0BECD241954C831E";
    "A6726DE8F01A50E849EDBC6C7C9CF2B2A88E19FD423E0647ECCB04DD4C9D1E";
    "BC7570BBBF1D46E85AF9AA6C7A9CEFA9E9825CFD5E3A0047F7CD009305A71E"
]

let space = "20"
let t = "74"
let T = "54"

type Byte = {
    Byte: byte 
    Character: char
    Hex: string
    Decimal: int
    Bits: string
    LS: string 
}

let byteToByte (byte:byte) =
    let bits = System.Convert.ToString(byte, 2).PadLeft(8, '0')
    {
        Byte = byte;
        Character = char byte;
        Hex = System.Convert.ToString(byte, 16) 
        Decimal = int byte;
        Bits = bits
        LS = if bits.StartsWith("00") then "L" else "S"
    }

let cipherTextSeq ct =
    Seq.toList ct
    |> List.chunkBySize 2
    |> List.map (fun x -> $"{x[0]}{x[1]}")
    |> List.map (fun x -> System.Convert.ToByte(x, 16))
    |> List.map byteToByte

let xOr xs ys =
    List.zip xs ys 
    |> List.map (fun (x, y) -> x.Byte ^^^ y.Byte)
    |> List.map byteToByte

// check the the first two bits are indeed 00 for xor of letters
let tT =
    xOr <| cipherTextSeq t <| cipherTextSeq T
    |> Seq.toList

let st =
    xOr <| cipherTextSeq space <| cipherTextSeq t
    |> Seq.toList

let Ts =
    xOr <| cipherTextSeq T <| cipherTextSeq space
    |> Seq.toList
    
let xOrCode (bits:string) (code:int) =
    let input = System.Convert.ToByte(bits, 2)
    seq {
        for x in 0 .. 255 do
            let r = (byte x) ^^^ (byte code)
            if r = input then byte x 
    }
    |> Seq.tryHead
    |> Option.map char

let xOrSpace (bits:string) = xOrCode bits 32

let findMatches ctx cty =
    xOr (cipherTextSeq ctx) (cipherTextSeq cty)
    |> Seq.map (fun x -> if x.LS = "L" then None else xOrSpace x.Bits)
    |> Seq.toList

type Guess = Space | Char of char | Letter

let howManySatisfy pred = 
    List.filter pred >> List.length 

let makeGuess (xs:List<Option<char>>) =
    let somes = xs |> List.filter (fun x -> x.IsSome)
    let count = somes |> List.length
    match count with
        | _ when count >= 4 -> Space
        | _ when count > 0 && count < 4 -> Char (somes |> List.head).Value
        | _ -> Letter

let findGuesses (index:int) =
    let item = List.item index cts
    let others = List.removeAt index cts
    others
    |> List.map (fun x -> findMatches item x)
    |> List.transpose
    |> List.map (fun xs -> makeGuess xs)

let printGuess guess =
    match guess with
    | Space -> ' '
    | Char c -> c
    | Letter -> '?'

let joinChars chars = 
    new string [|for c in chars -> c|]

let findCandidate index =
    findGuesses index
    |> List.map (fun x -> printGuess x)
    |> joinChars
        
let cc0 = findCandidate 0
let cgm = "I am planning a secret mission."

let findCgmCode index = 
    int <| cgm[index]

let ccOthers =
    let ct0 = List.item 0 cts
    seq {
        for x in 1 .. 6 do
            let ctx = List.item x cts
            xOr (cipherTextSeq ct0) (cipherTextSeq ctx)
            |> Seq.mapi (fun i x -> xOrCode x.Bits (findCgmCode i))
            |> Seq.map (fun x -> x.Value)
            |> Seq.toList
            |> joinChars
    }
    |> Seq.toList

printfn "%A" ccOthers