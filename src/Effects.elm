module Effects exposing (Effects, apply, applyWithPriority, batch, from, map, none)

{-| In Elm all effects are managed (no side effects) and described with commands and subscriptions (Cmd msg, Sub msg).
Only private organisations/people have access to add/describe new effects.

Effects module helps to manage additional program effects.
For instance effects can be used for communication between programs.
It make sense especially for programs described as a composition of sub-programs.
A program effect should be supplied by a consumer.

-}


{-| Describes a collection of program effects.
-}
type Effects effect
    = Effects (List effect)


{-| Creates no effect. Useful when a program does not produce any effects.
-}
none : Effects effect
none =
    Effects []


{-| Creates an effect. Useful to create an instance of a program effect.
-}
from : effect -> Effects effect
from =
    Effects << List.singleton


{-| Batches multiple effects. Useful when a program produces multiple effects.
-}
batch : List (Effects effect) -> Effects effect
batch =
    Effects << List.concat << List.map toList


{-| Transforms an effect. Useful when combining multiple programs together.
-}
map : (effectA -> effectB) -> Effects effectA -> Effects effectB
map tagger (Effects effects) =
    Effects (List.map tagger effects)


{-| Applies all effects to given initial value.
Useful to apply effects produced by a program.
-}
apply : (effect -> value -> value) -> value -> Effects effect -> value
apply applyEffect initialValue (Effects effects) =
    List.foldl applyEffect initialValue effects


{-| Applies all effects to given initial value by given priority.
Useful to apply effects produced by a program.
Effects with lover priority are applied first.
-}
applyWithPriority : (effect -> value -> value) -> (effect -> Int) -> value -> Effects effect -> value
applyWithPriority applyEffect priority initialValue (Effects effects) =
    effects
        |> List.sortBy priority
        |> List.foldl applyEffect initialValue


{-| Converts effects into a list.
-}
toList : Effects effect -> List effect
toList (Effects effects) =
    effects
