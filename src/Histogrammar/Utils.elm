module Histogrammar.Utils exposing (..)


unifyResult : (err -> unified) -> (ok -> unified) -> Result err ok -> unified
unifyResult unifyErr unifyOk result =
    case result of
        Ok ok ->
            unifyOk ok

        Err err ->
            unifyErr err
