namespace TheLib

open Misc
open UnityEngine
open Things



type Class1() = 
    member this.X = 
        match this.stuff with
            | A -> "F#?!"
            | B -> "C++"
            | C -> "What?"
    member this.stuff = C
    member this.l = [1..10]



type SimpleComponent() =
    inherit MonoBehaviour()

    [<SerializeField>] 
    let mutable changeSpeed = 2.0f

    [<SerializeField>] 
    let mutable mode = A

    [<SerializeField>] 
    let mutable xs = [|10;20;30|]

    [<SerializeField>]
    [<Range(10.0f, 50.0f)>] 
    let mutable slide = 20.0f

    member this.Start () = 
        do let listOfStuff = [1;2;10]
           exclaim (listOfStuff.ToString()) Happy |> Debug.Log;
           Debug.Log("Closest with tag: " + (findClosest "dude" this.transform).ToString())

    member this.Update () =
        let r = Time.time * changeSpeed |> Mathf.Sin |> Mathf.Abs 
        in do this.renderer.material.color <- new Color(r, 0.0f, 1.0f, 1.0f)
              this.transform.localScale <- new Vector3(r, r, r)