namespace TheLib

open UnityEngine
open UnityMagic
open Maybe

type SmartComponent() = 
    inherit MonoBehaviour()
    
    [<SerializeField>]
    let mutable flash : Light = null
    
    [<SerializeField>]
    let mutable stick : Transform = null
    
    [<SerializeField>]
    let mutable pick : Transform = null

    member this.Start() = 
        do this.setupFlashlight()
           stick <- extract $ maybe { let! arm = find "Arm" this.transform 
                                      return! find "Stick" arm }
    
    member this.Update() = () //fetchInput (this.inputHandler)

    member this.inputHandler keyEvent = ()
//        match keyEvent with
//           | (x, Down) -> Debug.Log $ sprintf "%A down" x
//           | (KeyCode.A, Pressed) -> this.transform.Translate $ new Vector3(-10.0f * Time.deltaTime, 0.0f, 0.0f)
//           | (KeyCode.D, Pressed) -> this.transform.Translate $ new Vector3(10.0f * Time.deltaTime, 0.0f, 0.0f)
//           | (KeyCode.W, Up) -> Debug.Log("W was released!")
//           | _ -> ()

    member this.setupFlashlight() = 
        ignore $ maybe { 
            let! flashTransform = find "Flashlight" this.transform
            let! flashLight = get<Light> flashTransform
            flashLight.color <- Color.green
            flashLight.intensity <- Random.Range(0.0f, 100.0f)
            flashLight.transform.Translate $ new Vector3(3.0f, 5.0f, 0.0f)
            flash <- flashLight
        }

    member this.naiveWay() = 
        do match find "Flashlight" this.transform with
           | None -> Debug.Log("No flashlight found")
           | Some l -> 
               match get<Light> l.transform with
               | None -> Debug.Log("No light found in flashlight")
               | Some lc -> do lc.color <- Color.red