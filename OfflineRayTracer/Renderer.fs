#light

namespace Jurassic.RayTracer

open System
open System.Drawing
open System.Drawing.Imaging
open Microsoft.FSharp.Math
open Jurassic.RayTracer
open Misc

type Renderer() = 
    let mutable maxDepth = 5
    let mutable bgColor = fromColor (Color.Black)
    let mutable ambientLight = vector [0.0; 0.0; 0.0]

    let mutable (sceneElements : seq<SceneElement>) = Seq.empty
    let mutable (lights : seq<Light>) = Seq.empty

    let mutable camera = new Camera(vector [ 5.0; 5.0; 5.0; 1.0 ],
                                    vector [ 0.0; 0.0; 0.0; 1.0 ])

    let mutable pixelWidth = 640
    let mutable pixelHeight = 480
    
    let mutable bmpContents:(byte array) = null
    let mutable stride = pixelWidth * 3
    
    member this.MaximumDepthOfRecursion with get() = maxDepth
                                        and  set v = maxDepth <- v
    
    member this.BackgroundColor with get() = bgColor
                                and  set v = bgColor <- v
    
    member this.AmbientLightColor with get() = ambientLight
                                  and  set v = ambientLight <- v
    
    member this.SceneElements with get() = sceneElements
                              and  set v = sceneElements <- v
    
    member this.Lights with get() = lights
                       and  set v = lights <- v
    
    member this.Camera with get() = camera
                       and  set v = camera <- v
    
    ///Width of the resulting picture in pixels.
    member this.Width with get() = pixelWidth
                      and  set v = pixelWidth <- v
    
    ///Height of the resulting picture in pixels.
    member this.Height with get() = pixelHeight
                       and  set v = pixelHeight <- v

    member private this.findClosestIntersection ray = 
        let lightProps = Seq.map (fun light -> light.Prop) lights
        
        let intersections = Seq.map (fun (x:SceneElement) -> match x.Intersect ray with
                                                              | Some t -> Some(t,x)
                                                              | None -> None)
                                       (Seq.append sceneElements lightProps)
        Seq.fold (fun a b ->
                      match (a,b) with
                      | (_,None) -> a
                      | (None,_) -> b
                      | (Some(t1,_),Some(t2,_)) -> if t1 < t2 then a
                                                              else b)
                        None intersections
    
    member private this.collectShadowMask ray maxT = 
        Seq.fold (fun shadowMask (x:SceneElement) ->
            match x.Intersect ray with
            | Some t ->
                if t > maxT then shadowMask
                else
                     let p = ray.Start + ray.Direction * t
                     let normal = x.Normal(p)
                     let entering = (Vector.dot normal ray.Direction) < 0.0
                     if entering then
                          let ray2 = { Start = p + ray.Direction*epsilon; Direction = ray.Direction }
                          let t2 = match x.Intersect ray2 with
                                   | Some num -> num
                                   | None -> Double.PositiveInfinity
                          if (t + t2 - epsilon) < maxT then
                              let p2 = ray2.Start + ray2.Direction * t2
                              shadowMask .* x.Color(p) * x.Translucency(p)
                                         .* x.Color(p2) * x.Translucency(p2)
                          else shadowMask .* x.Color(p) * x.Translucency(p)
                     else shadowMask .* x.Color(p) * x.Translucency(p)
                
            | None -> shadowMask)
            (vector [1.0; 1.0; 1.0])
            sceneElements
    
    member private this.intersect ray depth = 
        if depth > maxDepth then bgColor else
        let closest = this.findClosestIntersection ray
        match closest with
        | None -> bgColor
        | Some (t,entity) ->
            let p = ray.Start + ray.Direction * t
            let rayDir = ray.Direction
            let eye = normalize (camera.Position - p)
            let normal = 
                let modelNormal = entity.Normal(p)
                if (Vector.dot modelNormal rayDir) < 0.0 then
                    entity.Normal(p)
                else
                    -entity.Normal(p)
            let color = entity.Color(p)
            let emissive = entity.Emissive(p)
            let reflectivity = entity.Reflectivity(p)
            let specExp = entity.SpecularExponent(p)
            let specularity = entity.Specularity(p)
            let translucency = entity.Translucency(p)
            let opacity = 1.0 - reflectivity - translucency
            
            let reflectedDir = Vector.reflect rayDir normal
            
            let reflected = if reflectivity > 0.0
                                then color .* (this.intersect {Start = p + reflectedDir*epsilon;
                                                               Direction = reflectedDir}
                                                              (depth+1))
                            else (vector [0.0; 0.0; 0.0])
            
            //no refraction takes place, IOR is constantly 1
            let refractedDir = rayDir
            
            let refracted = if translucency > 0.0
                                then color .* (this.intersect {Start = p + refractedDir*epsilon;
                                                               Direction = refractedDir}
                                                              (depth+1))
                            else (vector [0.0; 0.0; 0.0])
            
            let (diffuse, specular) = 
                Seq.fold (fun (diff, spec) light ->
                    let lightDistance = Vector.norm (light.Position - p)
                    let lightVec = (light.Position - p) * (1.0 / lightDistance)
                    let shadowMask = this.collectShadowMask { Start = p + lightVec*epsilon;
                                                              Direction = lightVec }
                                                            (lightDistance - epsilon)
                    let falloff = saturate (light.Falloff lightDistance)
                    
                    let nDotL = saturate (Vector.dot normal lightVec)
                    let newDiff = color .* light.Color * nDotL .* shadowMask * falloff
                    
                    let reflectedLight = Vector.reflect (-lightVec) normal
                    let rDotE = saturate (Vector.dot reflectedLight eye)
                    let specAmount = (rDotE ** specExp) * specularity
                    let newSpec = (Vector.create 3 specAmount) .* shadowMask * falloff
                    
                    (diff + newDiff, spec + newSpec))
                    (Vector.zero 3, Vector.zero 3)
                    lights
            
            saturateVector((saturateVector (diffuse + specular)) * opacity +
                            reflected * reflectivity + refracted * translucency +
                            ambientLight .* color + emissive)
    
    
    member private this.drawRange (start,finish) = 
        let temp = Array.zeroCreate ((finish - start + 1) * stride)
        
        let mutable i = 0
        
        for y = start to finish do
            for x = 0 to pixelWidth - 1 do
                let result =
                    this.intersect {Start = camera.Position;
                                    Direction = normalize (camera.BoardOrigin
                                        + camera.BoardAxisX * ((float x) / (float pixelWidth) * camera.BoardWidth) 
                                        + camera.BoardAxisY * ((float y) / (float pixelHeight) * camera.BoardHeight)
                                        - camera.Position) } 1
                let color = makeColor result
                temp.[i] <- color.B
                temp.[i+1] <- color.G
                temp.[i+2] <- color.R
                i <- i + 3
        
        lock (bmpContents) (fun () ->
            temp.CopyTo(bmpContents, start*stride))
    
    
    member this.Draw() = 
        let bitmap = new Bitmap(pixelWidth, pixelHeight, PixelFormat.Format24bppRgb)
        
        let bmpData = bitmap.LockBits(Rectangle(0, 0, pixelWidth, pixelHeight),
                                      ImageLockMode.WriteOnly, bitmap.PixelFormat)
        
        let bytes = bmpData.Height * bmpData.Stride
        
        bmpContents <- Array.zeroCreate bytes
        
        stride <- bmpData.Stride
        
        //not large enough to cause overhead trouble, but large enough to be easily
        //distributed for 2 (Hyperthreading), 6 (Xbox 360) or 9 (Cell) hardware threads
        let hwThreads = 18
    
        let step = bitmap.Height / hwThreads
        
        let tasks = List.init hwThreads (fun i ->
            async { do this.drawRange
                            (i*step, if i < hwThreads - 1 then (i+1)*step-1
                                                          else bitmap.Height-1) } )
        
        do Async.RunSynchronously (Async.Parallel tasks) |> ignore
        
        do System.Runtime.InteropServices.Marshal.Copy(bmpContents, 0, bmpData.Scan0, bytes)
        
        bitmap.UnlockBits(bmpData)
        
        bitmap