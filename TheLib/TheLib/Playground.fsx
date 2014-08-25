
#load "Json.fs"
open Json

jsonToString data3
jsonToString data4
jsonToString data5

lookup "a" data4
nth 1 data3

data5 |> lookup "stuff" |> nth 2



