// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Component1.fs"
open TheLib

#load "Maybe.fs"

open Option
open Maybe

let f x = Some (x / 2)


(Some 10) >>= f