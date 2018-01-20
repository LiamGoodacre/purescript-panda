module Panda.Application where

-- There are problems to be had with cyclical dependencies. In an effort to
-- avoid them, this file declares all the relevant types in one go.

import Prelude
import Data.Profunctor (dimap, rmap)
import Data.Lens (ALens)
import FRP.Event (Event)
import Util.Exists2 (Exists4, runExists4, mkExists4)
import Data.Exists (Exists, mkExists, runExists)


-- Configuration for a Panda application. These three values encompass
-- everything required for a Panda application to be fully functioning.

newtype Application' si so ei eo action
  = Application'
      { -- A view is the generated output. Certain components within it can be
        -- configured to listen for a particular event and, with that and the
        -- state, update themselves. Moreover, UI events can trigger _actions_,
        view ∷ si → Component si so ei eo action

        -- A UI can be affected by an event (which itself can be the combination
        -- of many events) that occurs according to some side-effects.
      , subscription ∷ Event eo

        -- When a UI action occurs, this function produces the UI update event.
      , update ∷ si → action → { event ∷ eo, state ∷ so }

      }


newtype Application si so ei eo
  = Application (Exists (Application' si so ei eo))

mkApplication
  ∷ ∀ si so ei eo action
  . Application' si so ei eo action
  → Application si so ei eo

mkApplication app = Application (mkExists app)

runApplication
  ∷ ∀ si so ei eo r
  . (∀ action. Application' si so ei eo action -> r)
  → Application si so ei eo
  → r

runApplication f (Application app)
  = runExists f app



-- A regular element - one with nothing interesting going on - is just a tag
-- name, some properties, and any children underneath. Of course, not all
-- elements will have children.

type Element si so ei eo action
  = { tagName    ∷ String
    , properties ∷ Array (Property action)
    , children   ∷ Array (Component si so ei eo action)
    }


-- A watcher is a component that is dependent on some event predicate. When the
-- predicate passes, the render function is called with the current state to
-- determine what needs to be done with the DOM element. It's quite nifty.

type Watcher si so ei eo action
  = { interest ∷ ei → Boolean
    , renderer ∷ Component si so ei eo action
    }


-- A delegate is a "sub-application" within our larger tree. This is pretty
-- neat because we can write applications that focus on specific parts of our
-- data, and then compose them together to deal with larger structures.

newtype Delegate' si so ei eo xsi xso xei xeo
  = Delegate'
      { focus
          ∷ { state ∷ ALens si so xsi xso
            , event ∷ ALens ei eo xei xeo
            }

      , delegate ∷ Application xsi xso xei xeo
      }


type Delegate si so ei eo = Exists4 (Delegate' si so ei eo)


-- A component is just the stuff from above. :)

data Component si so ei eo action
  = Element  (Element  si so ei eo action)
  | Watcher  (Watcher  si so ei eo action)
  | Delegate (Delegate si so ei eo)
  | Text     String

-- TODO: expand this out _a lot_.

data Property action
  = Property
      { key ∷ String
      , value ∷ String
      }
  | OnClick action -- TODO

derive instance functorProperty :: Functor Property

-- Neat little fold.

component
  ∷ ∀ si so ei eo action result
  . (Element   si so ei eo action → result) 
  → (Watcher   si so ei eo action → result)
  → (Delegate  si so ei eo        → result)
  → (String                       → result)
  →  Component si so ei eo action
  → result

component element watcher delegate text
  = case _ of
      Element  value → element  value
      Watcher  value → watcher  value
      Delegate value → delegate value
      Text     value → text     value

varyApplication
  ∷ ∀ si si' so so' ei ei' eo eo'
  . (si' → si)
  → (so → so')
  → (ei' → ei)
  → (eo -> eo')
  → Application si so ei eo
  → Application si' so' ei' eo'

varyApplication fsi fso fei feo app
  = runApplication onApp app
  where

    onApp
      ∷ ∀ action
      . Application' si so ei eo action
      → Application si' so' ei' eo'

    onApp (Application' {view, subscription, update})
      = mkApplication $ Application'
          { view: dimap fsi (varyComponent fsi fso fei feo) view
          , subscription: feo <$> subscription
          , update: dimap fsi (rmap varyUpdateCod) update
          }

    varyUpdateCod {event, state}
      = { event: feo event
        , state: fso state
        }


varyComponent
  ∷ ∀ si si' so so' ei ei' eo eo' action
  . (si' → si)
  → (so → so')
  → (ei' → ei)
  → (eo → eo')
  → Component si so ei eo action
  → Component si' so' ei' eo' action

varyComponent fsi fso fei feo = go where
  go = case _ of
    Element {tagName, properties, children} →
      Element { tagName
              , properties
              , children: go <$> children
              }

    Watcher {interest, renderer} →
      Watcher { interest: interest <<< fei
              , renderer: go renderer
              }

    Delegate d →
      (runExists4 onDelegate d)

    Text s ->
      Text s

  onDelegate
    ∷ forall a b c d
    . Delegate' si so ei eo a b c d
    → Component si' so' ei' eo' action

  onDelegate (Delegate' {focus: {state, event}, delegate})
    = Delegate (mkExists4 (Delegate' value))
    where
      value = { focus:
                  { state: rmap (dimap fsi fso) state
                  , event: rmap (dimap fei feo) event
                  }

              , delegate
              }


mkElement
  ∷ ∀ si so ei eo action
  . String
  → Array (Property action)
  → Array (Component si so ei eo action)
  → Component si so ei eo action

mkElement tagName properties children = Element
  { tagName
  , properties
  , children
  }

mkButton
  ∷ ∀ si so ei eo action
  . Array (Property action)
  → Array (Component si so ei eo action)
  → Component si so ei eo action

mkButton = mkElement "button"

mkText
  ∷ ∀ si so ei eo action
  . String
  → Component si so ei eo action

mkText = Text

data Act = Noop | Inc

foo :: Application Int Int Boolean Boolean
foo = mkApplication $ Application'
  let view i =
        mkButton
          [ OnClick Inc
          ]

          [ mkText (show i)
          ]

      subscription = pure false

      update state = case _ of
        Noop -> { event: false, state }
        Inc -> { event: true, state: state + 1 }

  in { view
     , subscription
     , update
     }


