module Maybe

open System

type MaybeBuilder() =
    member this.Return(x) = Some x

    member this.ReturnFrom(m: 'T option) = m

    member this.Bind(m, f) = Option.bind f m

    member this.Zero() = None

    member this.Combine(m, f) = Option.bind f m

    member this.Delay(f: unit -> _) = f

    member this.Run(f) = f()

    member this.TryWith(m, h) =
        try this.ReturnFrom(m)
        with e -> h e

    member this.TryFinally(m, compensation) =
        try this.ReturnFrom(m)
        finally compensation()

    member this.Using(res:#IDisposable, body) =
        this.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

    member this.While(guard, f) =
        if not (guard()) then this.Zero() else
        this.Bind(f(), fun _ -> this.While(guard, f))

    member this.For(sequence:seq<_>, body) =
        this.Using(sequence.GetEnumerator(),
                             fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))

let maybe = MaybeBuilder()
let (>>=) m f = Option.bind f m