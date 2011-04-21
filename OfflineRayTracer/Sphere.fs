#light

namespace Jurassic.RayTracer

open System
open Microsoft.FSharp.Math
open Jurassic.RayTracer
open Misc

type Sphere() = 
    inherit SceneElement()
    
    ///Same as Scale.
    member this.Radius with get() = this.Scale
                       and  set v = this.Scale <- v
    
    default this.TextureMap (objectSpacePosition) = 
        let vec = (objectSpacePosition - vector [0.0; 0.0; 0.0; 1.0])
        let azimuth = atan2 vec.[2] vec.[0]
        let zenith = acos vec.[1]
        let u = zenith / Math.PI
        let t = if vec.[2] >= 0.0 then
                    azimuth / (2.0 * Math.PI)
                else
                    (azimuth + 2.0 * Math.PI) / (2.0 * Math.PI)
        (t,u)
    
    default this.Normal (worldSpacePosition) = 
        let objectSpacePosition = this.TransformInverse * worldSpacePosition
        let objectSpaceNormal = objectSpacePosition - vector [ 0.0; 0.0; 0.0; 1.0 ]
        let transformIT = this.TransformInverse.Transpose
        let normalTransformMatrix = Matrix.init 4 4 (fun i j ->
            if i < 3 && j < 3 then
                transformIT.[i,j]
            else if i = 4 && j = 4 then
                1.0
            else
                0.0)
        normalize(normalTransformMatrix * objectSpaceNormal)
    
    default this.Intersect (ray) = 
        let s = this.TransformInverse * ray.Start
        let d = this.TransformInverse * ray.Direction
        let v = s - vector [ 0.0; 0.0; 0.0; 1.0 ]
        let vd = Vector.dot v d
        let vv = Vector.dot v v
        let dd = Vector.dot d d
        let disc = 4.0 * (vd * vd - dd * (vv - 1.0))
        if disc < 0.0 then None
        else
            let discSqrt = sqrt disc
            let t1 = (-2.0 * vd - discSqrt) / (2.0 * dd)
            let t2 = (-2.0 * vd + discSqrt) / (2.0 * dd)
            if t2 < 0.0 then None
            else if t1 < 0.0 then Some t2
            else Some t1