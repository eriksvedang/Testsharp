module ParserCombinators

// Code from http://www.codeproject.com/Articles/235221/Monadic-Parsing-in-Fsharp


type Parser<'r> = Parser of (char list -> ('r * char list) list)

let parse (Parser p) = p

let (>>=) p f = Parser(fun cs -> List.concat [for (r,cs') in parse p cs -> parse (f r) cs'])


// FUNDAMENTAL PARSERS

// The first simply injects a value into a result without consuming any input characters
let mreturn r = Parser(fun cs -> [(r,cs)])

// The next is a parser that returns an empty result, often denoted by λ, Λ, or ϵ:
let lambda = Parser(fun _ -> [])

// Next is the item parser. This parser consumes a single character unconditionally:
let item = Parser(fun cs ->
    match cs with [] -> [] | c::cs' -> [(c,cs')])

// Using these fundamental parsers, we can define two other fundamental parser. The first is the conditional parser:
let sat cond =
  item >>= fun c -> if cond c then mreturn c else lambda

// The char parser consumes a character if and only if the character matches:'
let pchar c = sat ((=)c)

// The digit parser consumes a character if and only if the character is in the range [0..9]:
let digit = sat (fun c -> (List.tryFind ((=)c) ['0'..'9']).IsSome)

// The alpha parser acts like digit:
let alpha = sat (fun c -> (List.tryFind ((=)c) (List.append ['a'..'z'] ['A'..'Z'])).IsSome)


// CHOICE COMBINATORS

let (<|>) p q = Parser(fun cs ->
  match parse p cs with
  | [] -> parse q cs
  | rs -> rs)

let (++) p q = Parser(fun cs ->
    List.append (parse p cs) (parse q cs))

// The Kleene* and Kleene (0-or-many and 1-or-many repetitions of a parser, respectively) 
// combinators are expressed in terms of each other:
let rec many0 p = many1 p <|> mreturn []
and many1 p = p >>= fun r -> many0 p >>= fun rs -> mreturn (r::rs)

// Sometimes it is necessary to check that not only a single character but an 
// entire string can be parsed, for example when parsing keywords:
//let rec symbol cs =
//  match cs with
//  | [] -> mreturn []
//  | c::cs' -> char c >> symbol cs' >> mreturn cs


// UTILS

// convert a string to a list of characters
let (~&) (str:string) = str.ToCharArray() |> List.ofArray
// convert a list of characters to a string
let (~%) (chars:char list) = new System.String(Array.ofList chars)



//Parse a series of parser p separated by a parser sep and return the list of results from parsing p. For example, parsing the string aba or abababa:
//let sepBy p sep =
//    p >>= fun r ->
//    many0 (sep >> p) >>= fun rs ->
//    mreturn (r::rs)

// Parse p followed by another parser e:
//let endBy p e =
//    p >>= fun r ->
//    e >>
//    mreturn r

// Parse a new line and an end-of-line. Note that "rn" is a newline in Win32 systems.
//let newline = symbol &"\r\n"
//let endofline = many0 (char ' ') >> newline >> many0 (char ' ')

// Parse any number (\geqq1) of spaces, here defined as either '' or 't':
let space = many1 (pchar ' ' <|> pchar '\t')

//Either parse the parser p or nothing. In grammar terms, it is expressed as p - Λ and is the equivalent of 0 or 1 successful applications of p.
let orLambda p = (p >>= fun v -> mreturn [v]) <|> mreturn []

