module UndoListExtra exposing (newSafeConcise)

import UndoList exposing (UndoList)


-- from https://erkal.github.io/UndoRedo/index.html

newSafeConcise : state -> UndoList state -> UndoList state
newSafeConcise state { past, present, future } =
    case future of
        [] ->
            { past = present :: past
            , present = state
            , future = []
            }

        _ ->
            { past = present :: List.reverse future ++ present :: past
            , present = state
            , future = []
            }
