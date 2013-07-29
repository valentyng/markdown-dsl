module RecursiveDescent

open MarkdownDocument

let toString chars = 
    System.String (chars |> Array.ofList)

module Old = 
//Parsing the inline code span
    let rec parseInlineBody acc = function
        | '`'::rest -> Some(List.rev acc, rest)
        | c::chars -> parseInlineBody (c::acc) chars
        | [] -> None

    let parseInline = function
        | '`'::chars -> parseInlineBody [] chars
        | _ -> None


    //Parsing spans using functions


    let rec parseSpans acc chars = seq {
        let emitLiteral = seq {
            if acc <> [] then
                yield acc |> List.rev |> toString |> Literal }

        match parseInline chars, chars with
            | Some(body, chars), _ ->
                yield! emitLiteral
                yield body |> toString |> InlineCode
                yield! parseSpans [] chars
            | _, c::chars ->
                yield! parseSpans (c::acc) chars
            | _, [] -> 
                yield! emitLiteral
        }

(*
#A Helper function that converts list of characters to a string; this is needed when we want to return 
parsed characters.
#B Function that parses text into span elements; It is implemented as a sequence expression that 
takes chars, keeps unprocessed characters in acc, and emits span elements of type 
MarkdownSpan. 
#C Lazy sequence that yields a single Literal span with the characters that were accumulated 
while processing earlier input that did not contain any special formatting command; Nothing is 
emitted if the character list was empty.
#D The main part of the function attempts to parse the input using the parseInline function and 
then pattern matches on the result as well as on the original input. 
#1 The input is an inline code span
#2 Continue processing the input
#3 Emit the last literal
*)


//Implementing Delimited active pattern
let (|StartsWith|_|) prefix input = 
    let rec loop = function 
        | p::prefix, r::rest when p = r -> loop (prefix, rest) 
        | [], rest -> Some(rest) 
        | _ -> None 
    loop (prefix, input) 

let rec parseBracketedBody closing acc = function
    | StartsWith closing (rest) -> 
        Some(List.rev acc, rest)
    | c::chars -> 
        parseBracketedBody closing (c::acc) chars
    | _ -> None 

let parseBracketed opening closing = function 
    | StartsWith opening chars -> 
        parseBracketedBody closing [] chars
    | _ -> None

let (|Delimited|_|) delim = parseBracketed delim delim 



//Parsing Markdown spans using active patterns
let rec parseSpans acc chars = seq {
    let emitLiteral = seq { 
        if acc <> [] then
            yield acc |> List.rev |> toString |> Literal }

    match chars with
        | Delimited ['`'] (body, chars) -> 
            yield! emitLiteral
            yield InlineCode(toString body)
            yield! parseSpans [] chars
        | Delimited ['*'; '*' ] (body, chars) 
        | Delimited ['_'; '_' ] (body, chars) -> 
            yield! emitLiteral
            yield Strong(parseSpans [] body |> List.ofSeq)
            yield! parseSpans [] chars
    
        | Delimited ['*' ] (body, chars)
        | Delimited ['_' ] (body, chars) -> 
            yield! emitLiteral
            yield Emphasis(parseSpans [] body |> List.ofSeq)
            yield! parseSpans [] chars
        | c::chars -> 
            yield! parseSpans (c::acc) chars
        | [] -> 
            yield! emitLiteral 
            
        }


//Exercise 1: Add parsing of hyperlinks
//Exercise 2: Add support for hard line breaks