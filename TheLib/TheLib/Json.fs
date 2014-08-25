module Json

open System

exception Error of string

type Json = 
    | JNumber of float32
    | JString of string
    | JList of Json list
    | JDict of (string * Json) list
    | JTrue
    | JFalse
    | JNull

let rec jsonToString = function
    | JNumber x -> x.ToString()
    | JString s -> "'" + s + "'"
    | JList l -> "[" + (String.concat ", " (List.map jsonToString l)) + "]"
    | JDict l -> "{" + (String.concat ", " (List.map strKeyValue l)) + "}"
    | JTrue -> "true"
    | JFalse -> "false"
    | JNull -> "null"
and strKeyValue (s, json) = "'" + s + "': " + (jsonToString json)



let rec lookup key jsonDict =
    match jsonDict with
    | JDict ((k, v)::tail) -> if key = k then v else lookup key (JDict tail)
    | JDict [] -> JNull
    | _ -> raise (Error "Can't use 'lookup' function on something that's not a dictionary")

let rec nth n jsonList =
    match jsonList with
    | JList l -> l.Item n
    | _ -> raise (Error "Can't use 'nth' function on something that's not a list")



// Example data
let data1 = JNumber 20.0f
let data2 = JString "hej"
let data3 = JList [data1; data2]
let data4 = JDict [("a", JNumber 2.0f); ("b", JString "jupp")]
let data5 = JDict [("stuff", JList [JNumber 10.0f; JNumber 20.0f; JNumber 30.0f]); ("fisk", JTrue)]

