{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Tracing (
  Tracing,
  runTracing,
  withSpan,
  withSpan_,
  addEvent,
  addAttribute,
  setStatus,
  SpanStatus (..),
) where

import Data.HashMap.Strict qualified as HM
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import OpenTelemetry.Attributes (Attribute)
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Trace (
  Span,
  SpanKind (..),
  SpanStatus (..),
  TracerProvider,
  defaultSpanArguments,
 )
import OpenTelemetry.Trace qualified as Trace
import Relude hiding (span)


data Tracing :: Effect where
  WithSpan :: Text -> [(Text, Attribute)] -> (Span -> m a) -> Tracing m a
  AddEvent :: Span -> Text -> [(Text, Attribute)] -> Tracing m ()
  AddAttribute :: Span -> Text -> Attribute -> Tracing m ()
  SetStatus :: Span -> SpanStatus -> Tracing m ()

type instance DispatchOf Tracing = 'Dynamic

makeEffect ''Tracing


runTracing :: IOE :> es => TracerProvider -> Eff (Tracing ': es) a -> Eff es a
runTracing tp = interpret $ \env -> \case
  WithSpan name attrs f -> do
    let tracer = Trace.makeTracer tp "apitoolkit-server" $ Trace.TracerOptions Nothing
        attrMap = HM.fromList attrs
    -- Use inSpan which handles the context management
    localSeqUnliftIO env $ \unlift -> liftIO $ do
      sp <- Trace.createSpan tracer Context.empty name (defaultSpanArguments {Trace.kind = Server, Trace.attributes = attrMap})
      result <- unlift (f sp)
      Trace.endSpan sp Nothing
      pure result
  AddEvent span event attrs -> liftIO $ Trace.addEvent span $ Trace.NewEvent event (HM.fromList attrs) Nothing
  AddAttribute span k v -> liftIO $ Trace.addAttribute span k v
  SetStatus span status -> liftIO $ Trace.setStatus span status


withSpan_ :: Tracing :> es => Text -> [(Text, Attribute)] -> Eff es a -> Eff es a
withSpan_ name attrs action = withSpan name attrs $ \_ -> action