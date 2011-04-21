#light

open System
open System.Drawing
open Jurassic.RayTracer
open Materials
open Misc
open Math

let renderer = new Renderer()

let movieMaker = new MovieMaker("mistnostSeStinitkemVideo.avi", 25.0, false)

let room = new Box()
room.ScaleX <- 3.0
room.ScaleY <- 2.4
room.ScaleZ <- 5.0
room.SpecularitySurface <- uniform 0.0
room.Center <- vector [ 0.0; room.ScaleY / 2.0; 0.0; 1.0 ]

let lightColor = fromColor (Color.White)

let lightBulb = new Sphere()
lightBulb.Radius <- 0.1
lightBulb.EmissiveSurface <- uniform lightColor

let lightCover = new Box()
lightCover.Scale <- 2.2 * lightBulb.Radius
lightCover.ScaleY <- 1.5
lightCover.TranslucencySurface <- uniform 0.8
lightCover.ColorSurface <- uniform (fromColor (Color.Orange))


let shader = new Box()
shader.ScaleX <- room.ScaleX / 3.0
shader.ScaleY <- shader.ScaleX
shader.ScaleZ <- 0.1
shader.Center <- vector [0.0; room.ScaleY / 2.0; 0.0; 1.0]
let zero = uniform 0.0
shader.TranslucencySurface <- makeCube zero zero zero zero
                                       (scalarTexture "stinitko.bmp")
                                       (flipX (scalarTexture "stinitko.bmp"))
shader.ColorSurface <- uniform (fromColor Color.White)


let scene = [ (room :> SceneElement); (lightCover :> SceneElement); (shader :> SceneElement) ]

renderer.SceneElements <- scene

let cameraPos = vector [ -room.ScaleX * 0.45; room.ScaleY * 0.9; room.ScaleZ * 0.45; 1.0 ]
let cameraTarget = vector [0.0; room.ScaleY * 0.3; 0.0; 1.0]

renderer.Camera <- new Camera(cameraPos, cameraTarget)

renderer.Width <- 320
renderer.Height <- 240

let lightStartPos = shader.Center +
                    vector [ 0.0; 0.0; 0.1 + lightCover.Scale / 2.0; 0.0 ]
let lightEndPos = vector [ 0.0; room.ScaleY / 2.0;
                           room.ScaleZ / 2.0 - lightCover.Scale / 2.0; 1.0 ]

for frame in 1 .. 125 do
    printfn "frame %d" frame
    
    let t = (float frame - 1.0) / 124.0
    let lightPos = vectorLerp t lightStartPos lightEndPos
    
    let light = { Position = lightPos; Color = lightColor;
                  Falloff = Light.LinearFalloff; Prop = (lightBulb :> SceneElement) }
    
    renderer.Lights <- Seq.singleton light
    
    lightCover.Center <- lightPos
    lightBulb.Center <- lightPos
    
    use bitmap = renderer.Draw()
    
    movieMaker.AddFrame(bitmap)
    
movieMaker.Close()