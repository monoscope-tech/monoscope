{- This file was auto-generated from opentelemetry/proto/metrics/v1/metrics.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields where
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
aggregationTemporality ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "aggregationTemporality" a) =>
  Lens.Family2.LensLike' f s a
aggregationTemporality
  = Data.ProtoLens.Field.field @"aggregationTemporality"
asDouble ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "asDouble" a) =>
  Lens.Family2.LensLike' f s a
asDouble = Data.ProtoLens.Field.field @"asDouble"
asInt ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "asInt" a) =>
  Lens.Family2.LensLike' f s a
asInt = Data.ProtoLens.Field.field @"asInt"
attributes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "attributes" a) =>
  Lens.Family2.LensLike' f s a
attributes = Data.ProtoLens.Field.field @"attributes"
bucketCounts ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "bucketCounts" a) =>
  Lens.Family2.LensLike' f s a
bucketCounts = Data.ProtoLens.Field.field @"bucketCounts"
count ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "count" a) =>
  Lens.Family2.LensLike' f s a
count = Data.ProtoLens.Field.field @"count"
dataPoints ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "dataPoints" a) =>
  Lens.Family2.LensLike' f s a
dataPoints = Data.ProtoLens.Field.field @"dataPoints"
description ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "description" a) =>
  Lens.Family2.LensLike' f s a
description = Data.ProtoLens.Field.field @"description"
exemplars ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "exemplars" a) =>
  Lens.Family2.LensLike' f s a
exemplars = Data.ProtoLens.Field.field @"exemplars"
explicitBounds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "explicitBounds" a) =>
  Lens.Family2.LensLike' f s a
explicitBounds = Data.ProtoLens.Field.field @"explicitBounds"
exponentialHistogram ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "exponentialHistogram" a) =>
  Lens.Family2.LensLike' f s a
exponentialHistogram
  = Data.ProtoLens.Field.field @"exponentialHistogram"
filteredAttributes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "filteredAttributes" a) =>
  Lens.Family2.LensLike' f s a
filteredAttributes
  = Data.ProtoLens.Field.field @"filteredAttributes"
flags ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "flags" a) =>
  Lens.Family2.LensLike' f s a
flags = Data.ProtoLens.Field.field @"flags"
gauge ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "gauge" a) =>
  Lens.Family2.LensLike' f s a
gauge = Data.ProtoLens.Field.field @"gauge"
histogram ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "histogram" a) =>
  Lens.Family2.LensLike' f s a
histogram = Data.ProtoLens.Field.field @"histogram"
isMonotonic ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "isMonotonic" a) =>
  Lens.Family2.LensLike' f s a
isMonotonic = Data.ProtoLens.Field.field @"isMonotonic"
max ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "max" a) =>
  Lens.Family2.LensLike' f s a
max = Data.ProtoLens.Field.field @"max"
maybe'asDouble ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'asDouble" a) =>
  Lens.Family2.LensLike' f s a
maybe'asDouble = Data.ProtoLens.Field.field @"maybe'asDouble"
maybe'asInt ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'asInt" a) =>
  Lens.Family2.LensLike' f s a
maybe'asInt = Data.ProtoLens.Field.field @"maybe'asInt"
maybe'data' ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'data'" a) =>
  Lens.Family2.LensLike' f s a
maybe'data' = Data.ProtoLens.Field.field @"maybe'data'"
maybe'exponentialHistogram ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'exponentialHistogram" a) =>
  Lens.Family2.LensLike' f s a
maybe'exponentialHistogram
  = Data.ProtoLens.Field.field @"maybe'exponentialHistogram"
maybe'gauge ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'gauge" a) =>
  Lens.Family2.LensLike' f s a
maybe'gauge = Data.ProtoLens.Field.field @"maybe'gauge"
maybe'histogram ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'histogram" a) =>
  Lens.Family2.LensLike' f s a
maybe'histogram = Data.ProtoLens.Field.field @"maybe'histogram"
maybe'max ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'max" a) =>
  Lens.Family2.LensLike' f s a
maybe'max = Data.ProtoLens.Field.field @"maybe'max"
maybe'min ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'min" a) =>
  Lens.Family2.LensLike' f s a
maybe'min = Data.ProtoLens.Field.field @"maybe'min"
maybe'negative ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'negative" a) =>
  Lens.Family2.LensLike' f s a
maybe'negative = Data.ProtoLens.Field.field @"maybe'negative"
maybe'positive ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'positive" a) =>
  Lens.Family2.LensLike' f s a
maybe'positive = Data.ProtoLens.Field.field @"maybe'positive"
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
maybe'sum ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'sum" a) =>
  Lens.Family2.LensLike' f s a
maybe'sum = Data.ProtoLens.Field.field @"maybe'sum"
maybe'summary ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'summary" a) =>
  Lens.Family2.LensLike' f s a
maybe'summary = Data.ProtoLens.Field.field @"maybe'summary"
maybe'value ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'value" a) =>
  Lens.Family2.LensLike' f s a
maybe'value = Data.ProtoLens.Field.field @"maybe'value"
metadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "metadata" a) =>
  Lens.Family2.LensLike' f s a
metadata = Data.ProtoLens.Field.field @"metadata"
metrics ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "metrics" a) =>
  Lens.Family2.LensLike' f s a
metrics = Data.ProtoLens.Field.field @"metrics"
min ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "min" a) =>
  Lens.Family2.LensLike' f s a
min = Data.ProtoLens.Field.field @"min"
name ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "name" a) =>
  Lens.Family2.LensLike' f s a
name = Data.ProtoLens.Field.field @"name"
negative ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "negative" a) =>
  Lens.Family2.LensLike' f s a
negative = Data.ProtoLens.Field.field @"negative"
offset ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "offset" a) =>
  Lens.Family2.LensLike' f s a
offset = Data.ProtoLens.Field.field @"offset"
positive ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "positive" a) =>
  Lens.Family2.LensLike' f s a
positive = Data.ProtoLens.Field.field @"positive"
quantile ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "quantile" a) =>
  Lens.Family2.LensLike' f s a
quantile = Data.ProtoLens.Field.field @"quantile"
quantileValues ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "quantileValues" a) =>
  Lens.Family2.LensLike' f s a
quantileValues = Data.ProtoLens.Field.field @"quantileValues"
resource ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "resource" a) =>
  Lens.Family2.LensLike' f s a
resource = Data.ProtoLens.Field.field @"resource"
resourceMetrics ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "resourceMetrics" a) =>
  Lens.Family2.LensLike' f s a
resourceMetrics = Data.ProtoLens.Field.field @"resourceMetrics"
scale ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "scale" a) =>
  Lens.Family2.LensLike' f s a
scale = Data.ProtoLens.Field.field @"scale"
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
scopeMetrics ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "scopeMetrics" a) =>
  Lens.Family2.LensLike' f s a
scopeMetrics = Data.ProtoLens.Field.field @"scopeMetrics"
spanId ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "spanId" a) =>
  Lens.Family2.LensLike' f s a
spanId = Data.ProtoLens.Field.field @"spanId"
startTimeUnixNano ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "startTimeUnixNano" a) =>
  Lens.Family2.LensLike' f s a
startTimeUnixNano = Data.ProtoLens.Field.field @"startTimeUnixNano"
sum ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "sum" a) =>
  Lens.Family2.LensLike' f s a
sum = Data.ProtoLens.Field.field @"sum"
summary ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "summary" a) =>
  Lens.Family2.LensLike' f s a
summary = Data.ProtoLens.Field.field @"summary"
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
unit ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "unit" a) =>
  Lens.Family2.LensLike' f s a
unit = Data.ProtoLens.Field.field @"unit"
value ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "value" a) =>
  Lens.Family2.LensLike' f s a
value = Data.ProtoLens.Field.field @"value"
vec'attributes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'attributes" a) =>
  Lens.Family2.LensLike' f s a
vec'attributes = Data.ProtoLens.Field.field @"vec'attributes"
vec'bucketCounts ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'bucketCounts" a) =>
  Lens.Family2.LensLike' f s a
vec'bucketCounts = Data.ProtoLens.Field.field @"vec'bucketCounts"
vec'dataPoints ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'dataPoints" a) =>
  Lens.Family2.LensLike' f s a
vec'dataPoints = Data.ProtoLens.Field.field @"vec'dataPoints"
vec'exemplars ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'exemplars" a) =>
  Lens.Family2.LensLike' f s a
vec'exemplars = Data.ProtoLens.Field.field @"vec'exemplars"
vec'explicitBounds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'explicitBounds" a) =>
  Lens.Family2.LensLike' f s a
vec'explicitBounds
  = Data.ProtoLens.Field.field @"vec'explicitBounds"
vec'filteredAttributes ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'filteredAttributes" a) =>
  Lens.Family2.LensLike' f s a
vec'filteredAttributes
  = Data.ProtoLens.Field.field @"vec'filteredAttributes"
vec'metadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'metadata" a) =>
  Lens.Family2.LensLike' f s a
vec'metadata = Data.ProtoLens.Field.field @"vec'metadata"
vec'metrics ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'metrics" a) =>
  Lens.Family2.LensLike' f s a
vec'metrics = Data.ProtoLens.Field.field @"vec'metrics"
vec'quantileValues ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'quantileValues" a) =>
  Lens.Family2.LensLike' f s a
vec'quantileValues
  = Data.ProtoLens.Field.field @"vec'quantileValues"
vec'resourceMetrics ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'resourceMetrics" a) =>
  Lens.Family2.LensLike' f s a
vec'resourceMetrics
  = Data.ProtoLens.Field.field @"vec'resourceMetrics"
vec'scopeMetrics ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'scopeMetrics" a) =>
  Lens.Family2.LensLike' f s a
vec'scopeMetrics = Data.ProtoLens.Field.field @"vec'scopeMetrics"
zeroCount ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "zeroCount" a) =>
  Lens.Family2.LensLike' f s a
zeroCount = Data.ProtoLens.Field.field @"zeroCount"
zeroThreshold ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "zeroThreshold" a) =>
  Lens.Family2.LensLike' f s a
zeroThreshold = Data.ProtoLens.Field.field @"zeroThreshold"