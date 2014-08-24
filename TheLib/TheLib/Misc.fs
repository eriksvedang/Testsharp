module Misc

open UnityEngine

type Mode = Happy | Questioning

let exclaim s = function
    | Happy -> s + "!"
    | Questioning -> s + "?!"

let transformDistance (t1:Transform) (t2:Transform) = Vector3.Distance(t1.position, t2.position)
     
let findClosest tag (transform:Transform) = 
     let objs = Array.map (fun (o:GameObject) -> o.transform) (GameObject.FindGameObjectsWithTag(tag))
     let removedSelf = Array.filter (fun t -> System.Object.ReferenceEquals(t, transform) = false) objs
     let sorted = Array.sortBy (fun t -> transformDistance t transform) removedSelf
     let l = Array.toList sorted
     match l with
        | [] -> None
        | (x::_) -> Some x
