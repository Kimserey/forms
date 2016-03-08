namespace forms

open WebSharper
open WebSharper.Forms
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Client

[<JavaScript>]
module Client =  

    let sendToBackend displayName age =
        async {
            let msg = sprintf "%s - Age: %i" displayName age
            return Result.Failure [ ErrorMessage.Create(0, "Sent") ];
        }
       
    let customErrorId = 5000

    type Doc with
        static member ShowErrorInline view (rv: Var<_>)=
            View.Through(view, rv) 
            |> View.Map (function Success _ -> Doc.Empty | Failure errs -> errs |> List.map (fun err -> p [ text err.Text ] :> Doc)  |> Doc.Concat) 
            |> Doc.EmbedView
        static member ShowCustomErrors view =
            Doc.ShowErrors view
                (fun  errs ->  errs 
                               |> List.filter (fun err -> err.Id = customErrorId)
                               |> List.map    (fun err -> p [ text err.Text ] :> Doc)  
                               |> Doc.Concat) 
    
    let form =
        Form.Return (fun firstname lastname age -> firstname + " " + lastname, age)
        <*> (Form.Yield "" |> Validation.IsNotEmpty "First name is required.")
        <*> (Form.Yield "" |> Validation.IsNotEmpty "Last name is required.")
        <*> (Form.Yield 18)
        |> Form.MapAsync(fun (displayName, number) -> sendToBackend displayName number)
        |> Form.MapToResult (fun res -> 
            match res with
            | Success s -> Success s
            | Result.Failure _ -> Result.Failure [ ErrorMessage.Create(customErrorId, "Backend failure") ])
        |> Form.WithSubmit
        |> Form.Render(fun name lastname age submit ->
            form [ fieldset [ div [ Doc.Input [] name ]
                              Doc.ShowErrorInline submit.View name
                              div [ Doc.Input [] lastname ]
                              Doc.ShowErrorInline submit.View lastname
                              div [ Doc.IntInputUnchecked [] age ]
                              Doc.Button "Send" [ attr.``type`` "submit" ] submit.Trigger 
                              Doc.ShowCustomErrors submit.View ] ])

    let main =
        form
        |> Doc.RunById "main"
