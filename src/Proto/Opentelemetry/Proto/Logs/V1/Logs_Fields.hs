{- This file was auto-generated from opentelemetry/proto/logs/v1/logs.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields where
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes as Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing as Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2 as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8 as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
import qualified Proto.Opentelemetry.Proto.Common.V1.Common
import qualified Proto.Opentelemetry.Proto.Resource.V1.Resource
attributes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "attributes" a) =>
  Lens.Family2.LensLike' f s a
attributes = Data.ProtoLens.Field.field @"attributes"
body ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "body" a) =>
  Lens.Family2.LensLike' f s a
body = Data.ProtoLens.Field.field @"body"
droppedAttributesCount ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "droppedAttributesCount" a) =>
  Lens.Family2.LensLike' f s a
droppedAttributesCount
  = Data.ProtoLens.Field.field @"droppedAttributesCount"
flags ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "flags" a) =>
  Lens.Family2.LensLike' f s a
flags = Data.ProtoLens.Field.field @"flags"
logRecords ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "logRecords" a) =>
  Lens.Family2.LensLike' f s a
logRecords = Data.ProtoLens.Field.field @"logRecords"
maybe'body ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'body" a) =>
  Lens.Family2.LensLike' f s a
maybe'body = Data.ProtoLens.Field.field @"maybe'body"
maybe'resource ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'resource" a) =>
  Lens.Family2.LensLike' f s a
maybe'resource = Data.ProtoLens.Field.field @"maybe'resource"
maybe'scope ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'scope" a) =>
  Lens.Family2.LensLike' f s a
maybe'scope = Data.ProtoLens.Field.field @"maybe'scope"
observedTimeUnixNano ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "observedTimeUnixNano" a) =>
  Lens.Family2.LensLike' f s a
observedTimeUnixNano
  = Data.ProtoLens.Field.field @"observedTimeUnixNano"
resource ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "resource" a) =>
  Lens.Family2.LensLike' f s a
resource = Data.ProtoLens.Field.field @"resource"
resourceLogs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "resourceLogs" a) =>
  Lens.Family2.LensLike' f s a
resourceLogs = Data.ProtoLens.Field.field @"resourceLogs"
schemaUrl ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "schemaUrl" a) =>
  Lens.Family2.LensLike' f s a
schemaUrl = Data.ProtoLens.Field.field @"schemaUrl"
scope ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "scope" a) =>
  Lens.Family2.LensLike' f s a
scope = Data.ProtoLens.Field.field @"scope"
scopeLogs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "scopeLogs" a) =>
  Lens.Family2.LensLike' f s a
scopeLogs = Data.ProtoLens.Field.field @"scopeLogs"
severityNumber ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "severityNumber" a) =>
  Lens.Family2.LensLike' f s a
severityNumber = Data.ProtoLens.Field.field @"severityNumber"
severityText ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "severityText" a) =>
  Lens.Family2.LensLike' f s a
severityText = Data.ProtoLens.Field.field @"severityText"
spanId ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "spanId" a) =>
  Lens.Family2.LensLike' f s a
spanId = Data.ProtoLens.Field.field @"spanId"
timeUnixNano ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "timeUnixNano" a) =>
  Lens.Family2.LensLike' f s a
timeUnixNano = Data.ProtoLens.Field.field @"timeUnixNano"
traceId ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "traceId" a) =>
  Lens.Family2.LensLike' f s a
traceId = Data.ProtoLens.Field.field @"traceId"
vec'attributes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'attributes" a) =>
  Lens.Family2.LensLike' f s a
vec'attributes = Data.ProtoLens.Field.field @"vec'attributes"
vec'logRecords ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'logRecords" a) =>
  Lens.Family2.LensLike' f s a
vec'logRecords = Data.ProtoLens.Field.field @"vec'logRecords"
vec'resourceLogs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'resourceLogs" a) =>
  Lens.Family2.LensLike' f s a
vec'resourceLogs = Data.ProtoLens.Field.field @"vec'resourceLogs"
vec'scopeLogs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'scopeLogs" a) =>
  Lens.Family2.LensLike' f s a
vec'scopeLogs = Data.ProtoLens.Field.field @"vec'scopeLogs"