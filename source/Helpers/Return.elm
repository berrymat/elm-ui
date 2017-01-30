module Helpers.Return exposing (..)

import Return exposing (Return)


type alias ReturnOut msg out model =
    ( ( model, Cmd msg ), List out )


map : (a -> b) -> ReturnOut msg out a -> ReturnOut msg out b
map f ( ( model, cmd ), out ) =
    ( ( f model, cmd ), out )


mapBoth : (a -> b) -> (c -> d) -> ReturnOut a out c -> ReturnOut b out d
mapBoth f f_ ( ( model, cmd ), out ) =
    ( ( f_ model, Cmd.map f cmd ), out )


singleton : model -> ReturnOut msg out model
singleton model =
    ( ( model, Cmd.none ), [] )


return : model -> Cmd msg -> ReturnOut msg out model
return model cmd =
    ( ( model, cmd ), [] )


out : model -> Cmd msg -> out -> ReturnOut msg out model
out model cmd out =
    ( ( model, cmd ), [ out ] )


wrap : Return a b -> ReturnOut a out b
wrap r =
    ( r, [] )


unwrap : ReturnOut a out b -> Return a b
unwrap ( r, out ) =
    r


andThen : (a -> ReturnOut msg out b) -> ReturnOut msg out a -> ReturnOut msg out b
andThen f ( ( model, cmd ), out ) =
    let
        ( ( model_, cmd_ ), out_ ) =
            f model
    in
        ( model_ ! [ cmd, cmd_ ], out ++ out_ )


mapOut :
    (out -> ReturnOut msg out a -> ReturnOut msg out a)
    -> ReturnOut msg out a
    -> ReturnOut msg out a
mapOut f r =
    let
        ( _, outlist ) =
            r
    in
        List.foldl f r outlist


logStray : x -> model -> ReturnOut msg out model
logStray x model =
    let
        _ =
            Debug.log "Stray found" x
    in
        singleton model


sequence : List (ReturnOut msg out model) -> ReturnOut msg out (List model)
sequence =
    let
        f ( ( model, cmd ), out ) ( ( models, cmds ), outs ) =
            ( (model :: models) ! [ cmd, cmds ], out ++ outs )
    in
        List.foldr f ( ( [], Cmd.none ), [] )


command : Cmd msg -> ReturnOut msg out model -> ReturnOut msg out model
command cmd ( ( model, cmd_ ), out ) =
    ( model ! [ cmd, cmd_ ], out )


outmsg : out -> ReturnOut msg out model -> ReturnOut msg out model
outmsg out ( ( model, cmd ), out_ ) =
    ( ( model, cmd ), out :: out_ )


dropout : ReturnOut msg out model -> ReturnOut msg out model
dropout ( ( model, cmd ), _ ) =
    ( ( model, cmd ), [] )


logout : ReturnOut msg out model -> ReturnOut msg out model
logout return =
    let
        ( ( _, _ ), outs ) =
            return

        x =
            Debug.log "Outs" outs
    in
        return
