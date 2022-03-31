module LibFile

open System.IO
open System.Text.Json

module config =
    type Config = {
        target_directory: string
    }

    let path (config: Config) = Path.Combine(config.target_directory , ".memoData")
    let link_path (config: Config) = Path.Combine(path(config) , "Resource")
    let link_id_path (config: Config) (id: Resource.Id) =
        Path.Combine(link_path(config) , id.idnumber.ToString() + ".json")

    let init (config: Config) =
        let path = path(config)
        Directory.CreateDirectory (Path.Combine(path , "Resource")) |> ignore
        Directory.CreateDirectory (Path.Combine(path , "Word")) |> ignore

    let ready(config: Config) =
        Directory.Exists(path(config))

module pipe =
    let rand = new System.Random()

    let create_from_zero (cfg: config.Config) (body: Resource.Body) (user: Setting.User) =
        let id = rand.Next()
        let content: Resource.Content = {
            info = {
                user = user
                datetime = System.DateTime.Now
                generation = Resource.Nothing
            }
            body = body
        }
        let res: Resource.Resource = { identifier = {idnumber = id} ; content = content }
        res

module file =

    let rand = new System.Random()

    let listup_resource_id (cfg: config.Config): Result<Resource.Id array , string> =
        if config.ready(cfg) then
            let path = config.path(cfg)
            Ok(Directory.GetFiles(path) |> Array.choose Resource.id_or)
        else
            Error("error directory not found")
    
    let search (cfg: config.Config) (id: Resource.Id) =
        File.OpenRead (config.link_id_path cfg id)
        |> JsonSerializer.Deserialize<Json.asJson.Resource.Content>
        |> Json.Deserialize.Resource.Content

    let overwrite (cfg: config.Config) (res: Resource.Resource) =
        let str = res.content |>
                    Json.Serialize.Resource.Content |>
                    JsonSerializer.Serialize<Json.asJson.Resource.Content>
        File.WriteAllText ((config.link_id_path cfg res.identifier) , str)