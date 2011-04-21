#light

namespace Jurassic.RayTracer

module Materials

open System
open System.Drawing
open Microsoft.FSharp.Math
open Jurassic.RayTracer
open Misc


let vectorBitmaps = new HashMultiMap<_,_>()
let scalarBitmaps = new HashMultiMap<_,_>()

let loadVectorBitmap (name:string) = 
    let bitmap = new Bitmap(name)
    let result = Array2D.zeroCreate (bitmap.Width + 1) (bitmap.Height + 1)
    
    for y = 0 to bitmap.Height - 1 do
        for x = 0 to bitmap.Width - 1 do
            result.[x,y] <- fromColor (bitmap.GetPixel(x,y))
    
    for x = 0 to bitmap.Width - 1 do
        result.[x,bitmap.Height] <- result.[x,bitmap.Height - 1]
    
    for y = 0 to bitmap.Height do
        result.[bitmap.Width,y] <- result.[bitmap.Width - 1,y]
    
    vectorBitmaps.[name] <- result
    
    result

let vectorTexture name = 
    let bitmap = if vectorBitmaps.ContainsKey(name) then vectorBitmaps.[name]
                 else loadVectorBitmap name
    
    fun (x,y) ->
        let xScaled = x * (float (bitmap.GetLength(0) - 2));
        let yScaled = y * (float (bitmap.GetLength(1) - 2));
        
        let x0, xFrac = intFrac xScaled
        let y0, yFrac = intFrac yScaled
        
        vectorLerp yFrac (vectorLerp xFrac bitmap.[x0,y0] bitmap.[x0+1,y0])
                         (vectorLerp xFrac bitmap.[x0,y0+1] bitmap.[x0+1,y0+1])


let loadScalarBitmap (name:string) = 
    let bitmap = new Bitmap(name)
    let result = Array2D.zeroCreate (bitmap.Width + 1) (bitmap.Height + 1)
    
    for y = 0 to bitmap.Height - 1 do
        for x = 0 to bitmap.Width - 1 do
            result.[x,y] <- float (bitmap.GetPixel(x,y).GetBrightness())
    
    for x = 0 to bitmap.Width - 1 do
        result.[x,bitmap.Height] <- result.[x,bitmap.Height - 1]
    
    for y = 0 to bitmap.Height do
        result.[bitmap.Width,y] <- result.[bitmap.Width - 1,y]
    
    scalarBitmaps.[name] <- result
    
    result

let scalarTexture name =
    let bitmap = if scalarBitmaps.ContainsKey(name) then scalarBitmaps.[name]
                 else loadScalarBitmap name
    
    fun (x,y) ->
        let xScaled = x * (float (bitmap.GetLength(0) - 2));
        let yScaled = y * (float (bitmap.GetLength(1) - 2));
        
        let x0, xFrac = intFrac xScaled
        let y0, yFrac = intFrac yScaled
        
        lerp yFrac (lerp xFrac bitmap.[x0,y0] bitmap.[x0+1,y0])
                   (lerp xFrac bitmap.[x0,y0+1] bitmap.[x0+1,y0+1])



let chessboard sizeX sizeY light dark = fun (x,y) ->
    let file = int((float sizeX) * x)
    let rank = int((float sizeY) * y)
    
    if (file + rank) % 2 = 0 then light
                             else dark

let classicChessboard = chessboard 8 8 (fromColor Color.White)
                                       (fromColor Color.Black)

let uniform value = fun(x:float,y:float) -> value

let scalarGradientX a b = fun (x:float,y:float) -> lerp x (a(x,y)) (b(x,y))

let scalarGradientY a b = fun (x:float,y:float) -> lerp y (a(x,y)) (b(x,y))

let vectorGradientX a b = fun (x:float,y:float) -> vectorLerp x (a(x,y)) (b(x,y))

let vectorGradientY a b = fun (x:float,y:float) -> vectorLerp y (a(x,y)) (b(x,y))

let scalarInterpolate t a b = fun (x:float, y:float) -> lerp t (a(x,y)) (b(x,y))

let vectorInterpolate t a b = fun (x:float, y:float) -> vectorLerp t (a(x,y)) (b(x,y))


//Cube texture generators

let makeCube posX negX posY negY posZ negZ = fun (x:float,y:float) ->
    let x0, xFrac = intFrac (x * 6.0)
    match x0 with
    | 0 -> posX (xFrac, y)
    | 1 -> negX (xFrac, y)
    | 2 -> posY (xFrac, y)
    | 3 -> negY (xFrac, y)
    | 4 -> posZ (xFrac, y)
    | 5 | 6 -> negZ (xFrac, y)
    | _ -> failwith "x out of range in a cubemap"

let makeCubeSymmetric matX matY matZ = makeCube matX matX matY matY matZ matZ

let makeCubeUniform material = fun (x:float,y:float) -> material((x * 6.0) % 1.0, y)


//mutators

let flipX material = fun (x,y) -> material(1.0 - x,y)

let flipY material = fun (x,y) -> material(x,1.0 - y)

///Rotates a material 90 degrees clockwise
let rotateCW material = fun (x,y) -> material(1.0 - y,x)

///Rotates a material 90 degress counter-clockwise
let rotateCCW material = fun (x,y) -> material(y, 1.0 - x)