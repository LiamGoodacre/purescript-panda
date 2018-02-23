module Panda.Internal.Types where

import Control.Monad.Eff    (Eff)
import DOM.Event.Types      (Event) as DOM
import Data.Lazy            (Lazy)
import Data.Maybe           (Maybe)
import FRP.Event            (Event) as FRP
import Util.Exists          (Exists3)
import Data.Tuple           (Tuple)

import Prelude

-- | Convenience synonym (sorry @jusrin00!)
type Canceller eff = Eff eff Unit

-- | Sum type of all sensible event handlers that can be applied to elements.
data Producer
  = OnAbort
  | OnBlur
  | OnChange
  | OnContextMenu
  | OnClick
  | OnDoubleClick
  | OnDrag
  | OnDragEnd
  | OnDragEnter
  | OnDragExit
  | OnDragLeave
  | OnDragOver
  | OnDragStart
  | OnDrop
  | OnError
  | OnFocus
  | OnFocusIn
  | OnFocusOut
  | OnInput
  | OnInvalid
  | OnKeyDown
  | OnKeyPress
  | OnKeyUp
  | OnLoad
  | OnMouseDown
  | OnMouseEnter
  | OnMouseLeave
  | OnMouseMove
  | OnMouseOver
  | OnMouseOut
  | OnMouseUp
  | OnReset
  | OnScroll
  | OnSelect
  | OnSubmit
  | OnTransitionEnd

-- | `Watching` describes either some fixed `output` that doesn't depend on
-- | an `input`, or some `input` dependent `output`.
-- | If the `output` does depend on `input` then it must provide an `interest`
-- | and possibly some resulting `output`.
data Watching input output
  = IgnoreInput output
  | WatchInput (input -> { interest ∷ Boolean
                         , result ∷ Maybe (Lazy output) })

-- | A `Property` corresponds with either:
-- | * A DOM node attribute, e.g `type="button"`
-- | * A DOM event listener, e.g `onClick` with some listener
-- |
-- | The values associated with each of these property formats
-- | may watch for changes of input - see `Watching`.
data Property input event
  = PString String (Watching input String)
  | PEvent Producer (Watching input (DOM.Event -> Maybe event))

---

-- | A static component is one that has properties and potentially houses other
-- components. These are the things you _actually_ render to the DOM, and get
-- converted to HTML elements. Note that the children of a static component can
-- absolutely be dynamic, and it can even have dynamic properties. All this
-- represents is an HTML element.
newtype ComponentStatic eff update state event
  = ComponentStatic
      { children   ∷ Array (Component eff update state event)
      , properties ∷ Array (Property { update ∷ update, state ∷ state} event)
      , tagName    ∷ String
      }

-- | A watcher component is one that varies according to the state and updates
-- in the application loop. Because a re-render is a destructive process, it is
-- recommended that the more common the watcher's interest, the lower in the
-- component tree it should occur. If an event is likely to be fired every
-- second and that will cause the re-rendering of the entire tree, performance
-- won't be great. Similarly to the `PropertyWatcher`, the `interest` flag is
-- an indication to the renderer: it may be ignored, as is the case in the
-- initial render.
newtype ComponentWatcher eff update state event
  = ComponentWatcher
      ( { update ∷ update
        , state  ∷ state
        }
      → { interest ∷ Boolean
        , renderer ∷ Lazy (Component eff update state event)
        }
      )

-- Applications can be nested arbitrarily, with the proviso that there is some
-- way to translate from "parent" to "child". The actual types of the child are
-- existential, and are thus not carried up the tree: "as long as you can tell
-- me how to convert updates and states for the child, and then 'unconvert'
-- events from the child, I can embed this application".
newtype ComponentDelegate eff update state event subupdate substate subevent
  = ComponentDelegate
      { delegate ∷ Application eff subupdate substate subevent
      , focus
          ∷ { update ∷ update   → Maybe subupdate
            , state  ∷ state    → substate
            , event  ∷ subevent → event
            }
      }


-- | A component is either a "static" element, a watcher, a delegate, or a text
-- element. @TODO: add `CFragment`.
data Component eff update state event
  = CStatic            (ComponentStatic   eff update state event)
  | CWatcher           (ComponentWatcher  eff update state event)
  | CDelegate (Exists3 (ComponentDelegate eff update state event))
  | CText String


-- | A specification for a contained Panda application: it must have a view
-- (what are we going to display?), a subscription (what events are we going to
-- generate and/or be interested in?) and an update method (how are we going to
-- interpret those events into updates for the DOM?)
type Application eff update state event
  = { view         ∷ Component eff update state event
    , subscription ∷ FRP.Event event
    , initial      ∷  { update ∷ update, state ∷ state }
    , update       ∷ ({ update ∷ update, state ∷ state } → Eff eff Unit)
                   →  { event  ∷ event,  state ∷ state }
                   → Eff eff Unit
    }

