module Unit42.Todo.App

open System
open System.IO
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
    Todos: Map<TodoId.T, Todo.T Indexed.T>
  }

  let todos model =
    model.Todos
    |> Map.toList
    |> List.map snd
    |> List.sortByDescending (_.Index >> Primitives.Natural.value)
    |> List.map _.Data

  let empty = { Todos = Map.empty }

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

let models: Map<ModelId.T, Model.T> = Map.empty

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
          input [
            _class "new-todo"
            _placeholder "What needs to be done?"
            _autofocus
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

let indexHandler =
  let todoText1 =
    Primitives.NonEmptyText.create "First todo"
    |> Option.defaultWith (fun _ -> failwith "absurd")

  let todoText2 =
    Primitives.NonEmptyText.create "Second todo"
    |> Option.defaultWith (fun _ -> failwith "absurd")

  let model =
    Model.empty
    |> Model.addTodo {
      Id = (TodoId.create (Guid.NewGuid()))
      IsDone = true
      Text = todoText1
    }
    |> Model.addTodo {
      Id = (TodoId.create (Guid.NewGuid()))
      IsDone = false
      Text = todoText2
    }

  let view = Views.index model
  htmlView view

let webApp =
  choose [
    GET
    >=> choose [ route "/" >=> indexHandler ]
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
