

#load "Json.fs"
open Json

jsonToString data3
jsonToString data4
jsonToString data5

lookup "a" data4
nth 1 data3

data5 |> lookup "stuff" |> nth 2


#load "ParserCombinators.fs"
open ParserCombinators
open System

parse (pchar 'e') &"erik"

//let rec expr =
//    number >> space >> number >> mreturn
//and number = 
//    many1 digit >>= fun digits ->
//    mreturn (Decimal.Parse (%digits + ","))
//
//parse expr &"213"
//
//
//