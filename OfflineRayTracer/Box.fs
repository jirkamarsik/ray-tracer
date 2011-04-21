#light

namespace Jurassic.RayTracer

open System
open Microsoft.FSharp.Math
open Jurassic.RayTracer
open Misc

type Box() = 
    inherit SceneElement()
    
    ///0 - positive X
    ///1 - negative X
    ///2 - positive Y
    ///3 - negative Y
    ///4 - positive Z
    ///5 - negative Z
    let surfaceIndexFromPosition (objectSpacePosition:vector) = 
        let absX = abs(objectSpacePosition.[0])
        let absY = abs(objectSpacePosition.[1])
        let absZ = abs(objectSpacePosition.[2])
        
        let surfaceIndex =        
            if ((absX >= absY) && (absX >= absZ))
                then if (objectSpacePosition.[0] > 0.0)
                        then 0
                        else 1
            else if ((absY >= absX) && (absY >= absZ))
                then if (objectSpacePosition.[1] > 0.0)
                        then 2
                        else 3
            else
                if (objectSpacePosition.[2] > 0.0)
                    then 4
                    else 5
                    
        surfaceIndex
    
    default this.TextureMap (objectSpacePosition) = 
        let surfaceIndex = surfaceIndexFromPosition objectSpacePosition
        let (t, u) = 
            match surfaceIndex with
            | 0 -> (-objectSpacePosition.[2] + 0.5, -objectSpacePosition.[1] + 0.5)
            | 1 -> (objectSpacePosition.[2] + 0.5, -objectSpacePosition.[1] + 0.5)
            | 2 -> (objectSpacePosition.[0] + 0.5, objectSpacePosition.[2] + 0.5)
            | 3 -> (objectSpacePosition.[0] + 0.5, -objectSpacePosition.[2] + 0.5)
            | 4 -> (objectSpacePosition.[0] + 0.5, -objectSpacePosition.[1] + 0.5)
            | 5 -> (-objectSpacePosition.[0] + 0.5, -objectSpacePosition.[1] + 0.5)
        
        ((t + float surfaceIndex) / 6.0, u)
    
    default this.Normal (worldSpacePosition) = 
        let objectSpacePosition = this.TransformInverse * worldSpacePosition
        let surfaceIndex = surfaceIndexFromPosition objectSpacePosition
        let objectSpaceNormal = 
            match surfaceIndex with
            | 0 -> vector [ 1.0; 0.0; 0.0; 0.0 ]
            | 1 -> vector [ -1.0; 0.0; 0.0; 0.0 ]
            | 2 -> vector [ 0.0; 1.0; 0.0; 0.0 ]
            | 3 -> vector [ 0.0; -1.0; 0.0; 0.0 ]
            | 4 -> vector [ 0.0; 0.0; 1.0; 0.0 ]
            | 5 -> vector [ 0.0; 0.0; -1.0; 0.0 ]
        
        let transformIT = this.TransformInverse.Transpose
        let normalTransformMatrix = Matrix.init 4 4 (fun i j ->
            if i < 3 && j < 3 then
                transformIT.[i,j]
            else if i = 4 && j = 4 then
                1.0
            else
                0.0)
        normalize(normalTransformMatrix * objectSpaceNormal)
    
    //metoda Slabs z RealTime Rendering, rozbalene cykly kvuli
    //brzkym vystupum a typum if vyrazu
    default this.Intersect (ray) = 
        let o = this.TransformInverse * ray.Start
        let d = this.TransformInverse * ray.Direction
        let mutable tmin = Double.NegativeInfinity
        let mutable tmax = Double.PositiveInfinity
        let p = vector [ 0.0; 0.0; 0.0; 1.0 ] - o
        
        let mutable stop = false
        let mutable e = p.[0]
        let mutable f = d.[0]
        if (abs(f) > Double.Epsilon) then
            let t1 = (e + 0.5) / f
            let t2 = (e - 0.5) / f
            if (t1 > t2) then
                if (t2 > tmin) then
                    tmin <- t2
                if (t1 < tmax) then
                    tmax <- t1
            else
                if (t1 > tmin) then
                    tmin <- t1
                if (t2 < tmax) then
                    tmax <- t2
            if (tmin > tmax) then stop <- true
            else if (tmax < 0.0) then stop <- true
        else if (-e - 0.5 > 0.0) || (-e + 0.5 < 0.0) then stop <- true
        
        if stop then None else
        
        e <- p.[1]
        f <- d.[1]
        if (abs(f) > Double.Epsilon) then
            let t1 = (e + 0.5) / f
            let t2 = (e - 0.5) / f
            if (t1 > t2) then
                if (t2 > tmin) then
                    tmin <- t2
                if (t1 < tmax) then
                    tmax <- t1
            else
                if (t1 > tmin) then
                    tmin <- t1
                if (t2 < tmax) then
                    tmax <- t2
            if (tmin > tmax) then stop <- true
            else if (tmax < 0.0) then stop <- true
        else if (-e - 0.5 > 0.0) || (-e + 0.5 < 0.0) then stop <- true
        
        if stop then None else
        
        e <- p.[2]
        f <- d.[2]
        if (abs(f) > Double.Epsilon) then
            let t1 = (e + 0.5) / f
            let t2 = (e - 0.5) / f
            if (t1 > t2) then
                if (t2 > tmin) then
                    tmin <- t2
                if (t1 < tmax) then
                    tmax <- t1
            else
                if (t1 > tmin) then
                    tmin <- t1
                if (t2 < tmax) then
                    tmax <- t2
            if (tmin > tmax) then stop <- true
            else if (tmax < 0.0) then stop <- true
        else if (-e - 0.5 > 0.0) || (-e + 0.5 < 0.0) then stop <- true
        
        if stop then None else
        
        if (tmin > 0.0) then Some tmin
                        else Some tmax