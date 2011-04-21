#light

namespace Jurassic.RayTracer

open Microsoft.FSharp.Math
open Jurassic.RayTracer

type Light = 
    { Position : vector;
      Color : vector;
      Falloff : float -> float;
      Prop : SceneElement; }
    
    static member NoFalloff = fun (d:float) -> 1.0
    
    static member LinearFalloff = fun (d:float) -> 1.0 / d
    
    static member SquareFalloff = fun (d:float) -> 1.0 / (d * d)