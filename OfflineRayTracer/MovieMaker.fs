#light

namespace Jurassic.RayTracer

open AviFile

type MovieMaker(filepath:string,fps:float,compressed:bool) = 
    do System.IO.File.Delete(filepath)
    let mutable frames = 0
    let mutable aviManager = null
    let mutable aviStream = null
    
    member this.Compressed with get() = compressed
    
    member this.FramesPerSecond with get() = fps
    
    member this.FilePath with get() = filepath
    
    member this.AddFrame frame = 
        if frames = 0 then
            aviManager <- new AviManager(filepath, false)
            aviStream <- aviManager.AddVideoStream(compressed, fps, frame)
        else
            aviStream.AddFrame(frame)
        frames <- frames + 1
    
    member this.Close() = 
        aviManager.Close()
    
    interface System.IDisposable with
        member this.Dispose() = 
            if (aviManager <> null) then
                aviManager.Close()
                aviManager <- null
            if (aviStream <> null) then
                aviStream.Close()
                aviStream <- null