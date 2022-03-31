// For more information see https://aka.ms/fsharp-console-apps

[<EntryPoint>]
let main args =
  printfn "Hello"
  let cfg: LibFile.config.Config = {
    target_directory = @"..\"
  }
  let body: Resource.Body = {
    kind = Resource.InLocal
    content_type = "test"
    link = "test"
  }
  let user = Setting.Host
  
  LibFile.config.init(cfg)
  let result1 = LibFile.pipe.create_from_zero cfg body user
  LibFile.file.overwrite cfg result1
  let result2 = LibFile.file.search cfg result1.identifier
  printfn "%A" result1
  printfn "%A" result2
  0