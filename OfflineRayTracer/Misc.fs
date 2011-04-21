#light

namespace Jurassic.RayTracer

module Misc

open System
open System.Drawing
open Microsoft.FSharp.Math;

let epsilon = 10.0 ** (-10.0)

module Vector = 
    let map2 f v1 v2 = 
        Array.map2 f (Vector.to_array v1) (Vector.to_array v2) |> Vector.of_array
    
    let cross3 (v1:vector) (v2:vector) = 
        if v1.Length <> 3 || v2.Length <> 3
            then raise (ArgumentException("Vectors must be of length 3."))
        
        vector [ v1.[1]*v2.[2] - v1.[2]*v2.[1];
                 v1.[2]*v2.[0] - v1.[0]*v2.[2];
                 v1.[0]*v2.[1] - v1.[1]*v2.[0] ]
    
    let cross4 (v1:vector) (v2:vector) = 
        if v1.Length <> 4 || v2.Length <> 4
            then raise (ArgumentException("Vectors must be of length 4."))
        vector [ v1.[1]*v2.[2] - v1.[2]*v2.[1];
                 v1.[2]*v2.[0] - v1.[0]*v2.[2];
                 v1.[0]*v2.[1] - v1.[1]*v2.[0]; 0.0 ]
    
    let reflect (incident:vector) (normal:vector) = 
        incident - normal * (Vector.dot incident normal) * 2.0
    
    let reflectRefract (incident:vector) (normal:vector) (ior:float) = 
        let criticalAngle = asin ior
        if (acos (Vector.dot normal (-incident)) > criticalAngle) then
            let reflectedDir = reflect incident normal
            (reflectedDir, reflectedDir)
        else
            let cosTheta1 = Vector.dot normal (-incident)
            let cosTheta2 = sqrt (1.0 - (1.0 - cosTheta1 * cosTheta1) / ior / ior)
            let reflectedDir = incident - normal * cosTheta1 * 2.0
            let refractedDir = incident * (1.0 / ior) +
                               normal * (cosTheta1 / ior - cosTheta2)
            (reflectedDir, refractedDir)

let intFrac (x:float) = 
    let xInt = int x
    (xInt,x - float xInt)

let lerp t x1 x2 =  x1 * (1.0 - t) + x2 * t

let vectorLerp t (v1:vector) (v2:vector) = v1 * (1.0 - t) + v2 * t

let normalize vec = 
    vec * (1.0 / Vector.norm vec)

let saturate x = 
    max (min 1.0 x) 0.0

let saturateVector = Vector.map saturate


let fromColor (c:Color) = vector [ (float c.R / 255.0);
                                   (float c.G / 255.0);
                                   (float c.B / 255.0) ]

let makeColor (c:vector) = 
    Color.FromArgb(int(c.[0] * 255.0), int(c.[1] * 255.0), int(c.[2] * 255.0))

let sphericalToCartesian (r,azimuth,zenith) = 
    vector [r * cos zenith * sin azimuth; r * sin zenith; r * cos zenith * cos azimuth; 1.0]

module Matrix = 
    let swapRows row1 row2 (m:matrix) = 
        if row1 >= m.NumRows || row2 >= m.NumRows || row1 < 0 || row2 < 0 then
            raise (ArgumentException("Invalid row indices."))
        
        Matrix.mapi (fun i j x ->
            if i = row1 then m.[row2,j]
            else if i = row2 then m.[row1,j]
            else x) m
    
    let inverse (m:matrix) =
        if m.NumCols <> m.NumRows
            then raise (ArgumentException("The matrix to be inverted must be square."))
        
        let mutable temp = Matrix.create m.NumRows (2 * m.NumCols) 0.0
        temp <- Matrix.mapi (fun i j x ->
            if j < m.NumCols then m.[i,j]
            else if (j - m.NumCols) = i then 1.0
            else 0.0) temp
        
        for k = 0 to m.NumCols - 1 do
            let rowToSwap,_ = temp.Column k |>
                Vector.foldi (fun i (maxRow, maxAbs) x ->
                    if i >= k && abs(x) > maxAbs
                    then (i, abs(x))
                    else (maxRow, maxAbs)) (k,0.0)
            temp <- swapRows k rowToSwap temp
            
            let diagonalValue = temp.[k,k]
            
            if diagonalValue = 0.0
                then raise (ArgumentException("The matrix to be inverted must be regular."))
            
            for j = 0 to temp.NumCols - 1 do
                temp.[k,j] <- temp.[k,j] / diagonalValue
            
            for i = 0 to temp.NumRows - 1 do
                if i <> k then
                    let coefficient = temp.[i,k]
                    for j = 0 to temp.NumCols - 1 do
                        temp.[i,j] <- temp.[i,j] - coefficient * temp.[k,j]
        
        temp.Region(0, m.NumCols, m.NumRows, m.NumCols)
    
    
    
    let scaling (scaleX:float) (scaleY:float) (scaleZ:float) = 
        Matrix.initDiagonal (vector [scaleX; scaleY; scaleZ; 1.0])
    
    
    let rotationX angle = 
        matrix [ [1.0; 0.0; 0.0; 0.0];
                 [0.0; cos(angle); -sin(angle); 0.0];
                 [0.0; sin(angle); cos(angle); 0.0];
                 [0.0; 0.0; 0.0; 1.0] ]
    
    let rotationY angle = 
        matrix [ [cos(angle); 0.0; sin(angle); 0.0];
                 [0.0; 1.0; 0.0; 0.0];
                 [-sin(angle); 0.0; cos(angle); 0.0];
                 [0.0; 0.0; 0.0; 1.0] ]
    
    let rotationZ angle = 
        matrix [ [cos(angle); -sin(angle); 0.0; 0.0];
                 [sin(angle); cos(angle); 0.0; 0.0];
                 [0.0; 0.0; 1.0; 0.0];
                 [0.0; 0.0; 0.0; 1.0] ]
    
    let rotationEuler h p r = 
        let ch = cos(h)
        let sh = sin(h)
        let cp = cos(p)
        let sp = sin(p)
        let cr = cos(r)
        let sr = sin(r)
        matrix [ [cr*ch - sr*sp*sh; -sr*cp; cr*sh + sr*sp*ch; 0.0];
                 [sr*ch + cr*sp*sh; cr*cp; sr*sh - cr*sp*ch; 0.0];
                 [-cp*sh; sp; cp*ch; 0.0];
                 [0.0; 0.0; 0.0; 1.0] ]
    
    let rotationAxis axis angle = 
        let c = cos(angle)
        let s = sin(angle)
        let axisNormalized = normalize axis
        let x = axisNormalized.[0]
        let y = axisNormalized.[1]
        let z = axisNormalized.[2]
        matrix [ [c+(1.0-c)*x*x; (1.0-c)*x*y-z*s; (1.0-c)*x*z+y*s; 0.0];
                 [(1.0-c)*x*y+z*s; c+(1.0-c)*y*y; (1.0-c)*y*z-x*s; 0.0];
                 [(1.0-c)*x*z-y*s; (1.0-c)*y*z+x*s; c+(1.0-c)*z*z; 0.0];
                 [0.0; 0.0; 0.0; 1.0] ]
    
    let translation (vec:vector) = 
        matrix [[1.0; 0.0; 0.0; vec.[0]];
                [0.0; 1.0; 0.0; vec.[1]];
                [0.0; 0.0; 1.0; vec.[2]];
                [0.0; 0.0; 0.0; 1.0]]

    ///Creates a matrix which converts vectors from the current base
    ///to the passed base.    
    let toBase (axis1:vector) (axis2:vector) (axis3:vector) = 
        matrix [ Vector.to_array axis1; Vector.to_array axis2; Vector.to_array axis3; [|0.0; 0.0; 0.0; 1.0|] ]
    
    ///Creates a matrix which converts vectors from the passed orthogonal
    ///base to the current base.
    let fromBase (axis1:vector) (axis2:vector) (axis3:vector) = 
        Matrix.transpose(toBase axis1 axis2 axis3)