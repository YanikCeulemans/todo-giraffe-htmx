module Unit42.Todo.App

open System
open System.IO
open System.Collections.Concurrent
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe

// ---------------------------------
// Models
// ---------------------------------
module Primitives =
  module Natural =
    type T = private Natural of uint

    let ofInt =
      function
      | i when i < 0 -> None
      | n -> Some(Natural(uint n))

    let create = Natural

    let value (Natural n) = n

    // TODO: This might not be safe for A LOT of todos?
    let increment (Natural n) = Natural(n + 1u)

  module NonEmptyText =
    type T = private NonEmptyText of string

    let create =
      function
      | candidate when String.IsNullOrWhiteSpace candidate -> None
      | nonEmptyText -> Some(NonEmptyText nonEmptyText)

    let value (NonEmptyText nonEmptyText) = nonEmptyText

module Indexed =
  type T<'a> = {
    Data: 'a
    Index: Primitives.Natural.T
  }


module TodoId =
  type T = private TodoId of Guid

  let create = TodoId

module Todo =
  type T = {
    Id: TodoId.T
    IsDone: bool
    Text: Primitives.NonEmptyText.T
  }

module ModelId =
  type T = private ModelId of Guid

  let create = ModelId

  let value (ModelId modelId) = modelId

module Model =
  type T = {
    NewTodoId: TodoId.T
    Todos: Map<TodoId.T, Todo.T Indexed.T>
  }

  let todos model =
    model.Todos
    |> Map.toList
    |> List.map snd
    |> List.sortByDescending (_.Index >> Primitives.Natural.value)
    |> List.map _.Data

  let createEmpty () = {
    NewTodoId = Guid.NewGuid() |> TodoId.create
    Todos = Map.empty
  }

  let private nextIndexed model x =
    let nextIndex =
      model.Todos
      |> Map.values
      |> List.ofSeq
      |> function
        | [] -> Primitives.Natural.create 0u
        | todoEntries ->
          todoEntries
          |> List.maxBy (_.Index >> Primitives.Natural.value)
          |> _.Index
          |> Primitives.Natural.increment

    {
      Indexed.T.Data = x
      Indexed.T.Index = nextIndex
    }


  let addTodo (todo: Todo.T) (model: T) =
    let indexedTodo = nextIndexed model todo

    {
      model with
          Todos = Map.add todo.Id indexedTodo model.Todos
    }

let models: ConcurrentDictionary<ModelId.T, Model.T> =
  ConcurrentDictionary<ModelId.T, Model.T>()

// ---------------------------------
// Views
// ---------------------------------

module Views =
  open Giraffe.ViewEngine

  let classes cs =
    cs
    |> List.filter snd
    |> List.map fst
    |> String.concat " "

  let layout (content: XmlNode list) =
    html [] [
      head [] [
        title [] [ encodedText "Unit42.Todo" ]
        link [ _rel "stylesheet"; _type "text/css"; _href "/app.css" ]
        script [ _src "https://unpkg.com/htmx.org@1.9.10" ] []
      ]
      body [] content
    ]

  let index (model: Model.T) =
    [
      section [ _class "todoapp" ] [
        header [ _class "header" ] [
          h1 [] [ encodedText "todos" ]
          form [] [
            input [ _type "hidden"; _value ""; _name "id" ]
            input [
              _class "new-todo"
              _placeholder "What needs to be done?"
              _autofocus
              _name "text"
            ]
          ]
        ]
        main [ _class "main" ] [
          div [ _class "toggle-all-container" ] [
            input [ _class "toggle-all"; _type "checkbox" ]
            label [ _class "toggle-all-label"; _for "toggle-all" ] [
              encodedText "Mark all as complete"
            ]
          ]
          ul
            [ _class "todo-list" ]
            (Model.todos model
             |> List.map (fun todo ->
               li [
                 _class (classes [ "view", true; "completed", todo.IsDone ])
               ] [
                 div [ _class "view" ] [
                   input (
                     [ _type "checkbox"; _class "toggle" ]
                     @ if todo.IsDone then [ _checked ] else []
                   )
                   label [] [
                     todo.Text
                     |> Primitives.NonEmptyText.value
                     |> encodedText
                   ]
                   button [ _class "destroy" ] []
                 ]
               ]))
        ]
      ]
    ]
    |> layout

// ---------------------------------
// Web app
// ---------------------------------
let cookieKey = "cid"

type Cookie = {
  Name: string
  Value: string
  Options: CookieOptions
}

let createModelId () =
  let guid = Guid.NewGuid()
  let sidText = guid.ToString()
  let modelId = ModelId.create guid

  sidText, modelId

let createClientIdCookie (ctx: HttpContext) clientId =
  let cookieOptions =
    CookieBuilder(
      Domain = ctx.Request.Host.Host,
      HttpOnly = true,
      SecurePolicy = CookieSecurePolicy.SameAsRequest
    )
      .Build(ctx)

  {
    Name = cookieKey
    Value = clientId
    Options = cookieOptions
  }

let requestScopeItemKeys = {| ModelId = "ModelId" |}

let ensureIdCookie: HttpHandler =
  fun (next: HttpFunc) (ctx: HttpContext) ->
    let logger = ctx.GetLogger()

    match ctx.GetCookieValue cookieKey with
    | Some cidText ->
      match Guid.TryParse(cidText) with
      | true, cidGuid ->
        logger.LogInformation("client has a valid cid cookie")
        let modelId = ModelId.create cidGuid
        ctx.Items[requestScopeItemKeys.ModelId] <- modelId

        models.TryAdd(modelId, Model.createEmpty ())
        |> ignore

      | false, _ ->
        logger.LogInformation(
          "client has a cid cookie '{CidText}', but it doesn't contain a \
            model id, some state corruption must have occurred, creating new state \
            and assigning it to a new cid cookie",
          cidText
        )

        let cidText, modelId = createModelId ()
        ctx.Items[requestScopeItemKeys.ModelId] <- modelId

        models.TryAdd(modelId, Model.createEmpty ())
        |> ignore

        let cookie = createClientIdCookie ctx cidText
        ctx.Response.Cookies.Append(cookie.Name, cookie.Value, cookie.Options)

    | None ->
      logger.LogInformation(
        "client doesn't have a cid cookie yet, creating and assigning one"
      )

      let cidText, modelId = createModelId ()
      ctx.Items[requestScopeItemKeys.ModelId] <- modelId

      models.TryAdd(modelId, Model.createEmpty ())
      |> ignore

      let cookie = createClientIdCookie ctx cidText

      ctx.Response.Cookies.Append(cookie.Name, cookie.Value, cookie.Options)

    next ctx


let indexHandler: HttpHandler =
  fun (next: HttpFunc) (ctx: HttpContext) ->
    let modelId = ctx.Items[requestScopeItemKeys.ModelId] :?> ModelId.T

    let model =
      match models.TryGetValue modelId with
      | false, _ -> failwith "absurd"
      | true, x -> x

    let view = Views.index model
    htmlView view next ctx

let webApp =
  ensureIdCookie
  >=> choose [
    route "/"
    >=> choose [ GET >=> indexHandler ]
    setStatusCode 404 >=> text "Not Found"
  ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex: Exception) (logger: ILogger) =
  logger.LogError(
    ex,
    "An unhandled exception has occurred while executing the request."
  )

  clearResponse
  >=> setStatusCode 500
  >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder: CorsPolicyBuilder) =
  builder
    .WithOrigins("http://localhost:5000", "https://localhost:5001")
    .AllowAnyMethod()
    .AllowAnyHeader()
  |> ignore

let configureApp (app: IApplicationBuilder) =
  let env = app.ApplicationServices.GetService<IWebHostEnvironment>()

  (match env.IsDevelopment() with
   | true -> app.UseDeveloperExceptionPage()
   | false -> app.UseGiraffeErrorHandler(errorHandler).UseHttpsRedirection())
    .UseCors(configureCors)
    .UseStaticFiles()
    .UseGiraffe(webApp)

let configureServices (services: IServiceCollection) =
  services.AddCors() |> ignore
  services.AddGiraffe() |> ignore

let configureLogging (builder: ILoggingBuilder) =
  builder.AddConsole().AddDebug()
  |> ignore

[<EntryPoint>]
let main args =
  let contentRoot = Directory.GetCurrentDirectory()
  let webRoot = Path.Combine(contentRoot, "WebRoot")

  Host
    .CreateDefaultBuilder(args)
    .ConfigureWebHostDefaults(fun webHostBuilder ->
      webHostBuilder
        .UseContentRoot(contentRoot)
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
      |> ignore)
    .Build()
    .Run()

  0
