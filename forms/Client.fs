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
            return Result.Success msg;
//            return Result.Failure [ ErrorMessage.Create(0, "Sent") ];
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
    
    let f =
        Form.Return (fun firstname lastname age -> firstname + " " + lastname, age)
        <*> (Form.Yield "" |> Validation.IsNotEmpty "First name is required.")
        <*> (Form.Yield "" |> Validation.IsNotEmpty "Last name is required.")
        <*> (Form.Yield 18)
        |> Form.MapToAsyncResult(fun (displayName, number) -> 
            async {
                let! res = sendToBackend displayName number
                JS.Alert "F"
                return match res with
                        | Success s -> Success s
                        | Result.Failure _ -> Result.Failure [ ErrorMessage.Create(customErrorId, "Backend failure") ]
            })
        |> Form.WithSubmit
        |> Form.Render(fun name lastname age submit ->
            form [ fieldset [ div [ Doc.Input [] name ]
                              Doc.ShowErrorInline submit.View name
                              div [ Doc.Input [] lastname ]
                              Doc.ShowErrorInline submit.View lastname
                              div [ Doc.IntInputUnchecked [] age ]
                              Doc.Button "Send" [ attr.``type`` "submit" ] submit.Trigger 
                              Doc.ShowCustomErrors submit.View ] ])
    
    let f' =
        Form.Return (fun firstname lastname age -> firstname + " " + lastname, age)
        <*> (Form.Yield "" |> Validation.IsNotEmpty "First name is required.")
        <*> (Form.Yield "" |> Validation.IsNotEmpty "Last name is required.")
        <*> (Form.Yield 18)
        |> Form.WithSubmit
        |> Form.MapToAsyncResult(fun (displayName, number) -> 
            async {
                let! res = sendToBackend displayName number
                JS.Alert "F"
                return match res with
                        | Success s -> Success s
                        | Result.Failure _ -> Result.Failure [ ErrorMessage.Create(customErrorId, "Backend failure") ]
            })
        |> Form.Render(fun name lastname age submit ->
            form [ fieldset [ div [ Doc.Input [] name ]
                              Doc.ShowErrorInline submit.View name
                              div [ Doc.Input [] lastname ]
                              Doc.ShowErrorInline submit.View lastname
                              div [ Doc.IntInputUnchecked [] age ]
                              Doc.Button "Send" [ attr.``type`` "submit" ] submit.Trigger 
                              Doc.ShowCustomErrors submit.View ] ])
    
    //Submitter reproduction
    let rv = Var.Create ""
    let input = Doc.Input [] rv
    let submitter = Var.Create ()    
    let snapAfter = 
        View.SnapshotOn (Result.Success rv.Value) submitter.View (rv.View |> View.MapAsync (fun s -> sendToBackend s 18))
        |> View.Map (fun s -> 
            match s with
            | Success s -> JS.Alert s; Doc.Empty
            | Failure err -> Doc.Empty)
        |> Doc.EmbedView

    let snapBefore = 
        View.SnapshotOn (rv.Value) submitter.View (rv.View)
        |> View.MapAsync (fun s -> sendToBackend s 18)
        |> View.Map (fun s -> 
            match s with
            | Success s -> JS.Alert s; Doc.Empty
            | Failure err -> Doc.Empty)
        |> Doc.EmbedView

    let btn = Doc.Button "test snap" [] (fun () -> submitter.Value <- ())

    let main =
        [ f
          br [] :> Doc
          br [] :> Doc
          f'
          br [] :> Doc
          br [] :> Doc
          input :> Doc
          btn :> Doc
          snapBefore ]
        |> Doc.Concat
        |> Doc.RunById "main"
