// See the file alphabet-cipher.md for detailed information.

type Message = string
type Keyword = string

let alphabet = [|97..122|] |> Array.map (fun i -> System.Convert.ToChar(i))
let letterToNum c = alphabet |> Array.findIndex (fun x -> x = c)
let numToLetter num = alphabet.[num]

let encode (key:Keyword) (message:Message) : Message =
    message
    |> Seq.toArray
    |> Array.indexed
    |> Array.map(fun (curMsgIndex, curChar) ->
        let msgInt = letterToNum(curChar)
        let keyInt = letterToNum(key.[curMsgIndex % key.Length])
        numToLetter((msgInt + keyInt) % 26))
    |> System.String

let decode (key:Keyword) (message:Message) : Message =
    message
    |> Seq.toArray
    |> Array.indexed
    |> Array.map(fun (curMsgIndex, curChar) ->
        let msgInt = letterToNum(curChar)
        let keyInt = letterToNum(key.[curMsgIndex % key.Length])
        let finalIndex =
            if msgInt - keyInt < 0 then
                26 + (msgInt - keyInt)
            else
                msgInt - keyInt
        numToLetter finalIndex)
    |> System.String

let decipher (cipher:Message) (message:Message) : Keyword =
    Array.map2 (fun msgChar cipherChar ->
        let msgCharIndex = letterToNum msgChar
        let keyCharIndexRelativeToMsgChar = (26 - msgCharIndex) + letterToNum cipherChar
        let keyCharIndex =
            if keyCharIndexRelativeToMsgChar > 25 then
                keyCharIndexRelativeToMsgChar - 26
            else
                keyCharIndexRelativeToMsgChar

        numToLetter (keyCharIndex)
    ) (Seq.toArray message) (Seq.toArray cipher)
    |> System.String


#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // verify encoding
    test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>

    // verify decoding
    test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>

    // verify decyphering
    test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

// run the tests
tests ()
