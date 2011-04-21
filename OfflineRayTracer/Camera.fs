#light

namespace Jurassic.RayTracer

open System
open Microsoft.FSharp.Math
open Jurassic.RayTracer
open Misc


type Camera(position:vector, lookAt:vector, ?up:vector) = 
    let mutable position = position
    let mutable lookAt = lookAt
    let mutable desiredUp = defaultArg up (vector [ 0.0; 1.0; 0.0; 0.0 ])
                     
    
    let mutable aspectRatio = 4.0 / 3.0
    let mutable horizFoV = Math.PI / 2.0
    
    let backwards = normalize(position - lookAt)
    let right = normalize (Vector.cross4 desiredUp backwards)
    let up = Vector.cross4 backwards right
    let camera2world = (Matrix.translation position) * (Matrix.fromBase right up backwards)
    let x = -sin(horizFoV/2.0)
    let y = -x/aspectRatio

    let mutable boardOrigin = camera2world * vector [ x; y; -1.0; 1.0 ]
    let mutable boardAxisX = right
    let mutable boardAxisY = -up
    let mutable boardWidth = -2.0 * x
    let mutable boardHeight = 2.0 * y

    let update () = 
        let backwards = normalize(position - lookAt)
        let right' = Vector.cross4 desiredUp backwards
        //if backwards has the same direction as desiredUp, we add a little noise to prevent
        //normalizing a zero vector
        let right = if (Vector.norm right' > 0.0)
                        then normalize right'
                    else
                        normalize (Vector.cross4 desiredUp (backwards + vector [epsilon; epsilon; epsilon; 0.0]))
        let up = Vector.cross4 backwards right
        let camera2world = (Matrix.translation position) * (Matrix.fromBase right up backwards)
        let x = -sin(horizFoV/2.0)
        let y = -x/aspectRatio
        boardOrigin <- camera2world * vector [ x; y; -1.0; 1.0 ]
        boardAxisX <- right
        boardAxisY <- -up
        boardWidth <- -2.0 * x
        boardHeight <- 2.0 * y

    member this.Position with get () = position
                         and  set v = position <- v
                                      update()

    member this.LookAtPoint with get () = lookAt
                            and  set v = lookAt <- v
                                         update()

    member this.UpDirection with get () = desiredUp
                            and  set v = desiredUp <- v
                                         update()

    member this.HorizontalFieldOfView with get () = horizFoV
                                      and  set v = horizFoV <- v
                                                   update()

    member this.AspectRatio with get () = aspectRatio
                            and   set v = aspectRatio <- v
                                          update()

    member this.BoardOrigin = boardOrigin
    member this.BoardAxisX = boardAxisX
    member this.BoardAxisY = boardAxisY
    member this.BoardWidth = boardWidth
    member this.BoardHeight = boardHeight