#light

open System
open System.Drawing
open Jurassic.RayTracer
open Materials
open Misc
open Microsoft.FSharp.Math

let renderer = new Renderer()

let fps = 25.0
let movieMaker = new MovieMaker("zrcadla.avi", fps, false)

let room = new Box()
room.ScaleX <- 6.0
room.ScaleY <- 3.0
room.ScaleZ <- 6.0
room.Center <- vector [0.0; room.ScaleY / 2.0; 0.0; 1.0]
let zero = uniform 0.0
let one = uniform 1.0
room.ReflectivitySurface <- makeCubeSymmetric one zero one
let white = uniform (fromColor Color.White)
let floor = chessboard (int (room.ScaleX * 4.0)) (int (room.ScaleZ * 4.0))
                       (fromColor Color.White) (fromColor Color.Black)
room.ColorSurface <- makeCube white white white floor white white


type FlyingSphere(initVel) =
    inherit Sphere()
        
    let mutable velocity = initVel
    
    member this.Velocity with get () = velocity
                         and  set v = velocity <- v
    
    member this.Update(timeElapsed, sphereList) = 
        let mutable newPos = this.Center + velocity * timeElapsed
        
        if newPos.[1] < this.Radius then
            velocity <- vector [ velocity.[0]; -velocity.[1]; velocity.[2]; 0.0 ]
            newPos <- newPos + vector [ 0.0; 1.0; 0.0; 0.0 ] * (2.0 * (this.Radius - newPos.[1]))
       
        if newPos.[1] > 0.5 * room.ScaleY - this.Radius then
            velocity <- vector [ velocity.[0]; -velocity.[1]; velocity.[2]; 0.0 ]
            newPos <- newPos + vector [ 0.0; -1.0; 0.0; 0.0 ] * (2.0 * (newPos.[1] - (0.5 * room.ScaleY - this.Radius)))
        
        if (newPos.[0] > 0.5 * room.ScaleX - this.Radius) then
            velocity <- vector [ -velocity.[0]; velocity.[1]; velocity.[2]; 0.0 ]
            newPos <- newPos + vector [ -1.0; 0.0; 0.0; 0.0 ] * (2.0 * (newPos.[0] - (0.5 * room.ScaleX - this.Radius)))
        
        if (newPos.[0] < -0.5 * room.ScaleX + this.Radius) then
            velocity <- vector [ -velocity.[0]; velocity.[1]; velocity.[2]; 0.0 ]
            newPos <- newPos + vector [ 1.0; 0.0; 0.0; 0.0 ] * (2.0 * (-0.5 * room.ScaleX + this.Radius - newPos.[0]))
        
        if (newPos.[2] > 0.5 * room.ScaleZ - this.Radius) then
            velocity <- vector [ velocity.[0]; velocity.[1]; -velocity.[2]; 0.0 ]
            newPos <- newPos + vector [ 0.0; 0.0; -1.0; 0.0 ] * (2.0 * (newPos.[2] - (0.5 * room.ScaleZ - this.Radius)))
        
        if (newPos.[2] < -0.5 * room.ScaleZ + this.Radius) then
            velocity <- vector [ velocity.[0]; velocity.[1]; -velocity.[2]; 0.0 ]
            newPos <- newPos + vector [ 0.0; 0.0; 1.0; 0.0 ] * (2.0 * (-0.5 * room.ScaleZ + this.Radius - newPos.[2]))
        
        for sphere in sphereList do
            if sphere <> this then
                let distance = Vector.norm (newPos - sphere.Center)
                if distance < this.Radius + sphere.Radius then
                    let axis = (sphere.Center - newPos) * (1.0 / distance)
                    let projection = Vector.dot velocity axis
                    
                    velocity <- velocity - axis * (2.0 * projection)
                    newPos <- newPos - axis * (2.0 * (this.Radius + sphere.Radius - distance))
        
        this.Center <- newPos


let lightPos = vector [ 0.0; 0.9 * room.ScaleY; 0.0; 1.0 ]

let lightProp = new Sphere()
lightProp.Center <- lightPos
lightProp.Radius <- 0.1
lightProp.EmissiveSurface <- uniform (fromColor (Color.White))


renderer.Lights <- Seq.singleton { Color = fromColor (Color.White);
                                   Position = lightPos; Falloff = Light.NoFalloff;
                                   Prop = (lightProp :> SceneElement) }

renderer.SceneElements <- [(room :> SceneElement)]

let cameraTarget = vector [ 0.0; room.ScaleY * 0.7; 0.0; 1.0 ]



renderer.Width <- 320
renderer.Height <- 240
renderer.MaximumDepthOfRecursion <- 15

let random = new Random()
let red = uniform (fromColor Color.Red)
let blue = uniform (fromColor Color.Blue)
let mutable spheres : FlyingSphere list = []
let gravity = 9.81


for frame in 1 .. 500 do
    printfn "frame %d" frame
    
    if frame % (int fps) = 0  && (frame <= 250)then
        let radius, color, speed =
            if random.NextDouble() < 0.5 then
                0.2, blue, 3.0
            else
                0.3, red, 2.0
        let initVel = vector [ speed * 2.0 * (random.NextDouble() - 0.5); 0.0;
                               speed * 2.0 * (random.NextDouble() - 0.5); 0.0 ]
        let newSphere = new FlyingSphere(initVel)
        newSphere.ColorSurface <- color
        newSphere.Radius <- radius
        newSphere.Center <- vector [ 0.0; room.ScaleY * 0.5; 0.0; 1.0 ]
        spheres <- newSphere :: spheres
        renderer.SceneElements <- Seq.append renderer.SceneElements [(newSphere :> SceneElement)]
    
    for sphere in spheres do
        sphere.Velocity <- sphere.Velocity + vector [ 0.0; -1.0; 0.0; 0.0 ] * gravity * (1.0 / fps)
        sphere.Update(1.0 / fps, spheres)
    
    let cameraAngle = (float frame / (10.0 * fps)) * (2.0 * Math.PI)
    let cameraPos = vector [ 0.45 * room.ScaleX * cos(cameraAngle); room.ScaleY * 0.7; 0.45 * room.ScaleZ * sin(cameraAngle); 1.0 ]
    renderer.Camera <- new Camera(cameraPos, cameraTarget)
    
    use bitmap = renderer.Draw()
    
    movieMaker.AddFrame(bitmap)

do movieMaker.Close()