-- | Typed results for htmx actions: the route lists every alternative a browser can get back.
module Web.Outcome (Ok (..), Invalid (..), NoBody (..), Rejection (..), HxOutcome (..), HxResponses, HxVerb, HxPost, HxResult, hxAction, hxDone, hxOutcome) where

import Effectful (Eff, (:>))
import Effectful.State.Static.Local qualified as State
import Lucid (ToHtml (..))
import Relude
import Servant (HasStatus (..), StdMethod (POST), UVerb, Union, getResponse, respond)
import Servant.API.UVerb (foldMapUnion)
import Servant.HTML.Lucid (HTML)
import System.Types (HXRedirectDest, HXReswap, RespHeaders, TriggerEvents, XWidgetJSON, addReswap, addRespHeaders, addSuccessToast, addToast, redirectCS)


-- | The action succeeded; the body is the outcome's 'ToHtml'.
newtype Ok a = Ok a
  deriving newtype (ToHtml)


instance HasStatus (Ok a) where type StatusOf (Ok a) = 200


-- | Renders nothing: for outcomes whose effect is a redirect or toast.
newtype NoBody a = NoBody a


instance ToHtml (NoBody a) where
  toHtml _ = mempty
  toHtmlRaw _ = mempty


-- | The request was understood but refused: 422, the reason as an error toast, and no swap, so the form keeps its input.
newtype Invalid e = Invalid e
  deriving (ToHtml) via NoBody (Invalid e)


instance HasStatus (Invalid e) where type StatusOf (Invalid e) = 422


-- | Expected, user-correctable failures, one constructor per case.
class Rejection e where
  rejectionMessage :: e -> Text
  rejectionDetail :: e -> Maybe Text
  rejectionDetail = const Nothing


-- | What a successful outcome tells htmx besides its body.
class ToHtml a => HxOutcome a where
  hxRedirect :: a -> Maybe Text
  hxRedirect = const Nothing
  hxSuccess :: a -> Maybe Text
  hxSuccess = const Nothing


type HxResponses e a = '[RespHeaders (Ok a), RespHeaders (Invalid e)]
type HxVerb method e a = UVerb method '[HTML] (HxResponses e a)
type HxPost e a = HxVerb 'POST e a
type HxResult e a = Union (HxResponses e a)


type HxState es = (State.State HXRedirectDest :> es, State.State HXReswap :> es, State.State TriggerEvents :> es, State.State XWidgetJSON :> es)


-- | Answer with an outcome that cannot be refused.
hxDone :: (HxOutcome a, HxState es) => a -> Eff es (RespHeaders a)
hxDone a = traverse_ redirectCS (hxRedirect a) >> traverse_ (`addSuccessToast` Nothing) (hxSuccess a) >> addRespHeaders a


hxAction :: (HxOutcome a, Rejection e, HxState es) => Eff es (Either e a) -> Eff es (HxResult e a)
hxAction run =
  run >>= \case
    Left e -> addToast "error" (rejectionMessage e) (rejectionDetail e) >> addReswap "none" >> (respond =<< addRespHeaders (Invalid e))
    Right a -> respond . fmap Ok =<< hxDone a


class Settled e a x where settled :: x -> Either e a
instance Settled e a (RespHeaders (Ok a)) where settled = Right . coerce . getResponse
instance Settled e a (RespHeaders (Invalid e)) where settled = Left . coerce . getResponse


-- | Read back which alternative an action answered with.
hxOutcome :: forall e a. HxResult e a -> Either e a
hxOutcome = foldMapUnion (Proxy @(Settled e a)) settled
