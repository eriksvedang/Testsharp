module UnityMagic

open UnityEngine

let ($) f g = f g

let error msg =
    Debug.LogError msg
    null

let nullToOption x =
    match x with
    | null -> None
    | _ -> Some x

/// Returns an object or null, depending on if it's wrapped in Some, or is None
let extract x =
    match x with
    | (Some s) -> s
    | None -> null

let find name (parent:Transform) = 
    match parent.Find(name) with
    | null -> None
    | x -> Some x
 
let sureFind name parent =
    match find name parent with 
    | Some child -> child
    | _ -> error ("Could not find " + name + " in " + parent.name)

let get<'t when 't :> Component and 't : null and 't : equality> (g:Component) : Option<'t> =
    let c = g.GetComponent<'t>()
    in if UnityEngine.Object.op_Equality(c, null) then None else (Some c)
   
type ButtonState = 
    | Down
    | Pressed
    | Up

type MouseButton =
    | LeftMouseButton
    | RightMouseButton
    | MiddleMouseButton

type InputType =
    | Keyboard of KeyCode * ButtonState
    | Mouse of MouseButton * ButtonState

let buttonIntToMouseButton = function
    | 0 -> LeftMouseButton
    | 1 -> RightMouseButton
    | 2 -> MiddleMouseButton
    | _ -> LeftMouseButton // ???

let fetchInput handler =
    // Keyboard
    let keysToCheck = Seq.cast<KeyCode>(System.Enum.GetValues(typeof<KeyCode>))
    let sendKeySignal checkingMethod keyCode buttonState = 
        if checkingMethod keyCode then handler $ Keyboard (keyCode, buttonState)
    for keyCode in keysToCheck do
        sendKeySignal (Input.GetKeyDown : (KeyCode -> bool)) keyCode Down
        sendKeySignal (Input.GetKey     : (KeyCode -> bool)) keyCode Pressed
        sendKeySignal (Input.GetKeyUp   : (KeyCode -> bool)) keyCode Up
    // Mouse
    let sendMouseSignal checkingMethod buttonInt mouseButton buttonState =
        if checkingMethod buttonInt then handler $ Mouse (mouseButton, buttonState)
    for buttonInt in [0;1;2] do
        let mouseButton = buttonIntToMouseButton buttonInt
        sendMouseSignal (Input.GetMouseButtonDown : (int -> bool)) buttonInt mouseButton Down
        sendMouseSignal (Input.GetMouseButton     : (int -> bool)) buttonInt mouseButton Pressed
        sendMouseSignal (Input.GetMouseButtonUp   : (int -> bool)) buttonInt mouseButton Up
    