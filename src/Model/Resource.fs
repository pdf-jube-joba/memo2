module Resource

type Kind =
    | InWeb
    | InLocal
    | InThisDir

type Body = {
    kind: Kind
    content_type : string
    link: string
}

type GeneratedBy =
    | Nothing
    | Creation of string
    | Save of string

type Info = {
    user: Setting.User
    datetime: System.DateTime
    generation: GeneratedBy
}

type Content = {
    info: Info
    body: Body
}

type Id = {
    idnumber: int
}

let id_or (s: string) =
    match System.Int32.TryParse s with
    | (true , v) -> Some {idnumber = v}
    | (false , _) -> None

type Resource = {
    identifier: Id;
    content: Content
}