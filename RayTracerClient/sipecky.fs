#light

open System
open System.Drawing
open Jurassic.RayTracer
open Materials
open Misc
open Math

let renderer = new Renderer()

let movieMaker = new MovieMaker("sipecky.avi", 25.0, false)

let room = new Box()
room.ScaleX <- 4.0
room.ScaleY <- 3.0
room.ScaleZ <- 8.0
room.Center <- vector [0.0; room.ScaleY / 2.0; 0.0; 1.0]

let wall = new Box()
wall.ScaleX <- room.ScaleX
wall.ScaleY <- room.ScaleY
wall.ScaleZ <- 0.05
wall.Center <- vector [0.0; room.ScaleY / 2.0; 0.0; 1.0]
let zero = uniform 0.0
wall.TranslucencySurface <- makeCube zero zero zero zero
                                     (scalarTexture "arrows.bmp")
                                     (flipX (scalarTexture "arrows.bmp"))



let circleCenter = vector [ 0.0; wall.ScaleY / 2.0; 2.0; 1.0 ]
let circleRadius = wall.ScaleX * (170.0 / 640.0)

let lightProp = new Sphere()
lightProp.Radius <- 0.1
lightProp.EmissiveSurface <- uniform (fromColor (Color.White))


renderer.SceneElements <- [(room :> SceneElement); (wall :> SceneElement)]

let viewingCameraPos = vector [ 0.0; room.ScaleY / 2.0; -0.3; 1.0 ]
let viewingCameraTarget = vector [ 0.0; room.ScaleY / 2.0; -room.ScaleZ / 2.0; 1.0 ]
let viewingCamera = new Camera(viewingCameraPos, viewingCameraTarget)

let backdoorCameraPos = circleCenter + vector [ 0.0; 1.0; 0.0; 0.0 ] * (room.ScaleY / 2.0 - 0.1)
                            + vector [ 0.0; 0.0; 1.0; 0.0 ] * 1.5
let backdoorCameraTarget = circleCenter + vector [ 0.0; 1.0; 0.0; 0.0 ] * (circleRadius - 0.5)
let backdoorCamera = new Camera(backdoorCameraPos, backdoorCameraTarget)

renderer.Camera <- backdoorCamera


renderer.Width <- 320
renderer.Height <- 240

for frame in 0 .. 179 do
    printfn "frame %d" frame
    
    let angle = float frame * System.Math.PI / 90.0
    
    let lightPos = circleCenter + vector [ 0.0; 1.0; 0.0; 0.0 ] * circleRadius * (cos angle)
                                + vector [ 1.0; 0.0; 0.0; 0.0 ] * circleRadius * (sin angle)
    
    lightProp.Center <- lightPos
    
    renderer.Lights <- Seq.singleton { Color = fromColor (Color.Green);
                                       Position = lightPos; Falloff = Light.NoFalloff;
                                       Prop = (lightProp :> SceneElement) }
    
    if frame = 45 then
        renderer.Camera <- viewingCamera
    
    use bitmap = renderer.Draw()
    
    movieMaker.AddFrame(bitmap)

movieMaker.Close()