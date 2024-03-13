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
open Microsoft.FSharp.Reflection
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

    let ofString =
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

  let value (TodoId id) = id

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

module Filter =
  type T =
    | All
    | Active
    | Completed

  let format =
    function
    | All -> ""
    | Active -> "active"
    | Completed -> "completed"

module Model =
  type T = {
    NewTodoId: TodoId.T
    Todos: Map<TodoId.T, Todo.T Indexed.T>
  }

  let todos filter model =
    model.Todos
    |> Map.toList
    |> List.map snd
    |> List.sortByDescending (_.Index >> Primitives.Natural.value)
    |> List.map _.Data
    |> List.filter (fun todo ->
      match filter with
      | Filter.All -> true
      | Filter.Active -> not todo.IsDone
      | Filter.Completed -> todo.IsDone)

  let activeTodos model = todos Filter.Active model

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


  let upsertTodo (todo: Todo.T) (model: T) =
    let changeTodo: Indexed.T<Todo.T> option -> Indexed.T<Todo.T> option =
      function
      | None -> nextIndexed model todo |> Some
      | Some indexedTodo -> { indexedTodo with Data = todo } |> Some

    {
      model with
          Todos = Map.change todo.Id changeTodo model.Todos
    }

  let toggleAll (model: T) =
    let isDone =
      match activeTodos model with
      | [] -> false
      | _ -> true

    {
      model with
          Todos =
            Map.map
              (fun _ todo -> { todo with Data.IsDone = isDone })
              model.Todos
    }

  let deleteTodo (todoId: TodoId.T) (model: T) = {
    model with
        Todos = Map.remove todoId model.Todos
  }

  let createEmpty () =
    let addDummy txt m =
      let todoId = Guid.NewGuid() |> TodoId.create

      upsertTodo
        {
          Todo.T.Id = todoId
          Todo.T.Text =
            Primitives.NonEmptyText.ofString txt
            |> Option.defaultWith (fun _ -> failwith "absurd")
          Todo.T.IsDone = false
        }
        m

    {
      NewTodoId = Guid.NewGuid() |> TodoId.create
      Todos = Map.empty
    }
    |> addDummy "First todo"
    |> addDummy "Second todo"

let models: ConcurrentDictionary<ModelId.T, Model.T> =
  ConcurrentDictionary<ModelId.T, Model.T>()

// ---------------------------------
// Views
// ---------------------------------
let jsonOptions =
  Text.Json.JsonSerializerOptions(Text.Json.JsonSerializerDefaults.Web)

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

  let todoItem (todo: Todo.T) =
    let todoId = todo.Id |> TodoId.value |> _.ToString()

    let todoText =
      todo.Text
      |> Primitives.NonEmptyText.value

    let vals =
      System.Text.Json.JsonSerializer.Serialize(
        {|
          Text = todoText
          IsDone = not todo.IsDone
        |},
        jsonOptions
      )

    li [ _class (classes [ "completed", todo.IsDone ]) ] [
      div [ _class "view" ] [
        input (
          [
            _type "checkbox"
            _class "toggle"
            // TODO: toggling todos and refreshing the page seems to sometimes
            // go wrong where an uncompleted item suddenly has an active
            // checkbox, but not the completed class indicating that the model
            // is correct. It seems the HTML is not replaced correctly or the
            // state of the checkbox is incorrectly set somehow
            attr "hx-put" $"/todos/{todoId}"
            attr "hx-swap" "outerHTML"
            attr "hx-target" "closest <li/>"
            attr "hx-vals" vals
          ]
          @ if todo.IsDone then [ _checked ] else []
        )
        label [] [ encodedText todoText ]
        button [
          _class "destroy"
          $"/todos/{todo.Id |> TodoId.value |> _.ToString()}"
          |> attr "hx-delete"
          attr "hx-target" "closest <li/>"
          attr "hx-swap" "delete"
        ] []
      ]
    ]

  let newTodoForm extraAttrs autofocus newTodoId =
    form
      ([
        _id "new-todo-form"
        attr "hx-put" $"/todos/%s{newTodoId}"
        attr "hx-target" ".todo-list"
        attr "hx-swap" "afterbegin"
        attr "hx-vals" """{ "isDone": false }"""
       ]
       @ extraAttrs)
      [
        input (
          [
            _class "new-todo"
            _placeholder "What needs to be done?"
            _name "text"
          ]
          @ if autofocus then [ _autofocus ] else []
        )
      ]

  let todoCount extraAttrs model =
    span
      ([ _class "todo-count"; _id "todo-count" ]
       @ extraAttrs)
      [
        let activeTodoCount = Model.activeTodos model |> List.length
        let plural = if activeTodoCount = 1 then "" else "s"
        strong [] [ string activeTodoCount |> encodedText ]
        encodedText $" item{plural} left"
      ]

  let todoList filter model =
    ul
      [ _class "todo-list" ]
      (Model.todos filter model
       |> List.map todoItem)

  let filters filter =
    ul [ _class "filters" ] [
      for f in FSharpType.GetUnionCases typeof<Filter.T> do
        let unionF = FSharpValue.MakeUnion(f, [||]) :?> Filter.T

        let path = $"/{Filter.format unionF}"

        li [] [
          a [ _href path; _class (classes [ "selected", filter = unionF ]) ] [
            encodedText f.Name
          ]
        ]
    ]

  let index (filter: Filter.T) (model: Model.T) =
    let newTodoId =
      model.NewTodoId
      |> TodoId.value
      |> _.ToString()

    let toggleAllPath =
      [ Filter.format filter; "toggle-all" ]
      |> List.filter (not << String.IsNullOrEmpty)
      |> String.concat "/"

    [
      section [ _class "todoapp" ] [
        header [ _class "header" ] [
          h1 [] [ encodedText "todos" ]
          newTodoForm [] true newTodoId
        ]
        main [ _class "main" ] [
          div [ _class "toggle-all-container" ] [
            input [ _class "toggle-all"; _type "checkbox" ]
            label [
              _class "toggle-all-label"
              _for "toggle-all"
              attr "hx-put" $"/{toggleAllPath}"
              attr "hx-target" ".todo-list"
              attr "hx-swap" "outerHTML"
            ] [ encodedText "Mark all as complete" ]
          ]
          todoList filter model
        ]
        footer [ _class "footer" ] [
          todoCount [] model
          filters filter
          // TODO: implement clear completed feature
          button [ _class "clear-completed" ] [ encodedText "Clear completed" ]
        ]
      ]
      footer [ _class "info" ] [
        p [] [ encodedText "Double-click to edit a todo" ]
        p [] [ encodedText "Created by Unit42" ]
        p [] [
          encodedText "Part of "
          a [ _href "http://todomvc.com" ] [ encodedText "TodoMVC" ]
        ]
      ]
    ]
    |> layout

  module OutOfBandWrapper =
    let withNewTodoForm newTodoId content = [
      newTodoForm [ attr "hx-swap-oob" "true" ] false newTodoId
      yield! content
    ]

    let withTodoCount model content = [
      todoCount [ attr "hx-swap-oob" "true" ] model
      yield! content
    ]

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


let indexHandler (filter: Filter.T) : HttpHandler =
  fun (next: HttpFunc) (ctx: HttpContext) ->
    let modelId = ctx.Items[requestScopeItemKeys.ModelId] :?> ModelId.T

    let model =
      match models.TryGetValue modelId with
      | false, _ -> failwith "absurd"
      | true, x -> x

    // printfn $"Model: %A{model}"
    let view = Views.index filter model
    htmlView view next ctx

[<CLIMutable>]
type TodoFormData = { Text: string; IsDone: bool }

let upsertTodoHandler (todoGuid: Guid) : HttpHandler =
  fun (next: HttpFunc) (ctx: HttpContext) ->
    let modelId = ctx.Items[requestScopeItemKeys.ModelId] :?> ModelId.T

    let model =
      match models.TryGetValue modelId with
      | false, _ -> failwith "absurd"
      | true, x -> x

    task {
      match! ctx.TryBindFormAsync<TodoFormData>() with
      | Error err -> return! RequestErrors.BAD_REQUEST err next ctx
      | Ok todoFormData ->
        match Primitives.NonEmptyText.ofString todoFormData.Text with
        | None ->
          return!
            RequestErrors.BAD_REQUEST "todo text must not be empty" next ctx
        | Some todoText ->
          let todo: Todo.T = {
            Id = TodoId.create todoGuid
            Text = todoText
            IsDone = todoFormData.IsDone
          }

          let updatedModel = Model.upsertTodo todo model

          match models.TryUpdate(modelId, updatedModel, model) with
          | false -> return! ServerErrors.INTERNAL_ERROR "" next ctx
          | true ->
            let view =
              [ Views.todoItem todo ]
              |> Views.OutOfBandWrapper.withNewTodoForm (
                Guid.NewGuid().ToString()
              )
              |> Views.OutOfBandWrapper.withTodoCount updatedModel
              |> ViewEngine.RenderView.AsString.htmlNodes

            return! htmlString view next ctx
    }

let deleteTodoHandler (todoGuid: Guid) =
  fun (next: HttpFunc) (ctx: HttpContext) ->
    let modelId = ctx.Items[requestScopeItemKeys.ModelId] :?> ModelId.T

    let model =
      match models.TryGetValue modelId with
      | false, _ -> failwith "absurd"
      | true, x -> x

    let todoId = TodoId.create todoGuid
    let updatedModel = Model.deleteTodo todoId model

    models.TryUpdate(modelId, updatedModel, model)
    |> ignore

    task {
      ctx.SetStatusCode 200
      return! ctx.WriteTextAsync "OK"
    }

let toggleAllHandler (filter: Filter.T) : HttpHandler =
  fun (next: HttpFunc) (ctx: HttpContext) ->
    // TODO: We can do better than this, having to fetch the model every time... come on?
    let modelId = ctx.Items[requestScopeItemKeys.ModelId] :?> ModelId.T

    let model =
      match models.TryGetValue modelId with
      | false, _ -> failwith "absurd"
      | true, x -> x

    let updatedModel = Model.toggleAll model

    models.TryUpdate(modelId, updatedModel, model)
    |> ignore

    let view =
      [ Views.todoList filter updatedModel ]
      |> Views.OutOfBandWrapper.withTodoCount updatedModel
      |> ViewEngine.RenderView.AsString.htmlNodes

    htmlString view next ctx


let webApp =
  ensureIdCookie
  >=> choose [
    route "/"
    >=> GET
    >=> indexHandler Filter.All
    subRoute "/toggle-all" (choose [ PUT >=> toggleAllHandler Filter.All ])
    subRoute
      "/todos"
      (choose [
        subRoutef "/%O" (fun todoId ->
          choose [
            PUT >=> upsertTodoHandler todoId
            DELETE >=> deleteTodoHandler todoId
          ])
      ])
    subRoute
      "/active"
      (choose [
        subRoute
          "/toggle-all"
          (choose [ PUT >=> toggleAllHandler Filter.Active ])
        GET >=> indexHandler Filter.Active
      ])
    subRoute
      "/completed"
      (choose [
        subRoute
          "/toggle-all"
          (choose [
            PUT
            >=> toggleAllHandler Filter.Completed
          ])
        GET >=> indexHandler Filter.Completed
      ])
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
