
#load "Maybe.fs"
open Maybe

#load "Json.fs"
open Json

jsonToString data3
jsonToString data4

lookup data4 "a"
nth data3 2

for c in "erik" do
    printfn "c = %A" c

