﻿namespace TheLib

open UnityEngine
open UnityMagic

type FlightMode = Landed | InAir of float32

type RotationDir = Left | Right

type ControlSignal = 
    | Turn of RotationDir
    | Move

type Truck() =
    inherit MonoBehaviour()

    [<SerializeField>]
    let mutable rotSpeed = 50.0f

    [<SerializeField>]
    let mutable moveSpeed = 10.0f

    let mutable flightMode = InAir 5.0f

    let rotationDirToSignum = function
        | Left -> -1.0f
        | Right -> 1.0f

    member this.Update () = do
        fetchInput this.inputHandler
        this.syncWithFlightMode()

    member this.inputHandler = function
        | (KeyCode.A, Pressed) -> this.control $ Turn Left
        | (KeyCode.D, Pressed) -> this.control $ Turn Right
        | (KeyCode.W, Pressed) -> this.control $ Move
        | (KeyCode.Space, Down) -> this.swapFlightMode()
        | _ -> ()

    member this.control signal =
        match flightMode with
        | Landed -> ()
        | InAir _ -> match signal with
                     | (Turn dir) -> this.transform.Rotate(0.0f, (rotationDirToSignum dir) * rotSpeed * Time.deltaTime, 0.0f)
                     | Move -> this.transform.Translate $ new Vector3(moveSpeed * Time.deltaTime, 0.0f, 0.0f)

    member this.swapFlightMode() =
        flightMode <- match flightMode with
                      | Landed -> InAir 5.0f
                      | _ -> Landed

    member this.syncWithFlightMode () =
        let height = match flightMode with
                     | Landed ->  0.0f
                     | (InAir h) -> h
        this.transform.position <- new Vector3(this.transform.position.x,
                                               height,
                                               this.transform.position.z)
