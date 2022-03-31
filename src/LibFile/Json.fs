module Json

module asJson =
  module Setting =
    type User = {user: string}

  module Resource =
    type Kind = {kind: string}
    type Body = {kind: Kind; content_type: string; link: string}
    type GeneratedBy = {generatedby: string}
    type Info = {user: Setting.User; datetime: string; generation: GeneratedBy}
    type Content = {info: Info; body:Body}
    type Id = {idnumber: int}
    type Resource = {identifier: Id; content: Content}

module Serialize =
  module Setting = 
    let User : Setting.User -> asJson.Setting.User =
      function
      | Setting.Host -> {user = "Host"}
      | Setting.Guest i -> {user = "Guest" + string i} 

  module Resource =
    let Kind : Resource.Kind -> asJson.Resource.Kind =
      function
      | Resource.InWeb -> {asJson.Resource.kind = "Inweb"}
      | Resource.InLocal -> {asJson.Resource.kind = "InLocal"}
      | Resource.InThisDir -> {asJson.Resource.kind = "InThisDir"}

    let Body (body: Resource.Body): asJson.Resource.Body =
      {
        kind = Kind(body.kind)
        content_type = body.content_type
        link = body.link
      }

    let GeneratedBy : Resource.GeneratedBy -> asJson.Resource.GeneratedBy =
      function
      | Resource.Nothing -> {generatedby = "Nothing"}
      | Resource.Creation s -> {generatedby = "Creation:" + s}
      | Resource.Save s -> {generatedby = "Save" + s}

    let Info (info: Resource.Info): asJson.Resource.Info =
      {
        user = Setting.User(info.user)
        datetime = info.datetime.ToString()
        generation = GeneratedBy(info.generation)
      }

    let Content (content: Resource.Content): asJson.Resource.Content =
      {
        info = Info(content.info)
        body = Body(content.body)
      }

    let Id (id: Resource.Id): asJson.Resource.Id =
      {
        idnumber = id.idnumber
      }

    let Resource (resource: Resource.Resource): asJson.Resource.Resource =
      {
        identifier = Id(resource.identifier)
        content = Content(resource.content)
      }

module Deserialize =

  module Setting = 
    let (|Guest|_|) (str: string) =
      match System.Int32.TryParse(str[6..]) with
      | (true , i) -> Some(i)
      | _ -> None
    let User (user: asJson.Setting.User): Result<Setting.User, string> =
      match user.user with
      | "Host" -> Ok(Setting.Host)
      | Guest i -> Ok(Setting.Guest i)
      | _ -> Error("fail to parse user")

  module Resource =
    let Kind (kind: asJson.Resource.Kind): Result<Resource.Kind, string> =
      match kind.kind with
      | "InWeb" -> Ok(Resource.InWeb)
      | "InLocal" -> Ok(Resource.InLocal)
      | "InThisDir" -> Ok(Resource.InThisDir)
      | _ -> Error("fail to parse kind")
  
    let Body (body: asJson.Resource.Body): Result<Resource.Body, string> =
      let f kind: Resource.Body = {kind = kind; content_type = body.content_type; link = body.link}
      Result.map f (Kind body.kind)

    let (|Creation|_|) (str: string) =
      match str[0..7] with
      | "Creation" -> Some(str[8..])
      | _ -> None

    let (|Save|_|) (str: string) =
      match str[0..3] with
      | "Save" -> Some(str[4..])
      | _ -> None

    let GeneratedBy (gen: asJson.Resource.GeneratedBy): Result<Resource.GeneratedBy, string> = 
      match gen.generatedby with
      | "Nothing" -> Ok(Resource.Nothing)
      | Creation str -> Ok(Resource.Creation str)
      | Save str -> Ok(Resource.Save str)
      | _ -> Error("fail to parse generation")

    let (|Date|_|) (str: string) =
      match System.DateTime.TryParse(str) with
      | (true , datetime) -> Some(datetime)
      | _ -> None

    let DateTime (str: string) =
      match str with
      | Date datetime -> Ok(datetime)
      | _ -> Error("fail to parse date")

    let Info (info: asJson.Resource.Info): Result<Resource.Info, string> =
      match Setting.User(info.user) , DateTime(info.datetime) , GeneratedBy(info.generation) with
      | Ok(user) , Ok(datetime) , Ok(gen) -> Ok({user = user; datetime = datetime; generation = gen})
      | _ , _ , _ -> Error("fail to parse info")

    let Content (content: asJson.Resource.Content): Result<Resource.Content, string> =
      match Info(content.info) , Body(content.body) with
      | Ok(info) , Ok(body) -> Ok({info=info; body=body})
      | _ , _ -> Error("fail to parse content")

    let Id (id: asJson.Resource.Id): Result<Resource.Id, string> =
      Ok({idnumber = id.idnumber})
    
    let Resource (resource: asJson.Resource.Resource): Result<Resource.Resource, string> =
      match Id(resource.identifier) , Content(resource.content) with
      | Ok(identifier) , Ok(content) -> Ok({identifier=identifier; content=content})
      | _ , _ -> Error("fail to parse resource")