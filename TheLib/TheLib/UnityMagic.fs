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

type KeyState = 
    | Down
    | Pressed
    | Up

let fetchInput handler =
    let keysToCheck = Seq.cast<KeyCode>(System.Enum.GetValues(typeof<KeyCode>))
    let sendSignal checkingMethod keyCode keyState = 
        if checkingMethod keyCode then handler (keyCode, keyState)
    for keyCode in keysToCheck do
        sendSignal (Input.GetKeyDown : (KeyCode -> bool)) keyCode Down
        sendSignal Input.GetKey keyCode Pressed
        sendSignal Input.GetKeyUp keyCode Up