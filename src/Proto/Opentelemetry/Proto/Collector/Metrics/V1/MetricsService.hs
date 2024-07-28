{- This file was auto-generated from opentelemetry/proto/collector/metrics/v1/metrics_service.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService (
        MetricsService(..), ExportMetricsPartialSuccess(),
        ExportMetricsServiceRequest(), ExportMetricsServiceResponse()
    ) where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Prism as Data.ProtoLens.Prism
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
import qualified Proto.Opentelemetry.Proto.Metrics.V1.Metrics
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields.rejectedDataPoints' @:: Lens' ExportMetricsPartialSuccess Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields.errorMessage' @:: Lens' ExportMetricsPartialSuccess Data.Text.Text@ -}
data ExportMetricsPartialSuccess
  = ExportMetricsPartialSuccess'_constructor {_ExportMetricsPartialSuccess'rejectedDataPoints :: !Data.Int.Int64,
                                              _ExportMetricsPartialSuccess'errorMessage :: !Data.Text.Text,
                                              _ExportMetricsPartialSuccess'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExportMetricsPartialSuccess where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExportMetricsPartialSuccess "rejectedDataPoints" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportMetricsPartialSuccess'rejectedDataPoints
           (\ x__ y__
              -> x__ {_ExportMetricsPartialSuccess'rejectedDataPoints = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExportMetricsPartialSuccess "errorMessage" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportMetricsPartialSuccess'errorMessage
           (\ x__ y__
              -> x__ {_ExportMetricsPartialSuccess'errorMessage = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExportMetricsPartialSuccess where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.collector.metrics.v1.ExportMetricsPartialSuccess"
  packedMessageDescriptor _
    = "\n\
      \\ESCExportMetricsPartialSuccess\DC20\n\
      \\DC4rejected_data_points\CAN\SOH \SOH(\ETXR\DC2rejectedDataPoints\DC2#\n\
      \\rerror_message\CAN\STX \SOH(\tR\ferrorMessage"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        rejectedDataPoints__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "rejected_data_points"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"rejectedDataPoints")) ::
              Data.ProtoLens.FieldDescriptor ExportMetricsPartialSuccess
        errorMessage__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "error_message"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"errorMessage")) ::
              Data.ProtoLens.FieldDescriptor ExportMetricsPartialSuccess
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, rejectedDataPoints__field_descriptor),
           (Data.ProtoLens.Tag 2, errorMessage__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExportMetricsPartialSuccess'_unknownFields
        (\ x__ y__
           -> x__ {_ExportMetricsPartialSuccess'_unknownFields = y__})
  defMessage
    = ExportMetricsPartialSuccess'_constructor
        {_ExportMetricsPartialSuccess'rejectedDataPoints = Data.ProtoLens.fieldDefault,
         _ExportMetricsPartialSuccess'errorMessage = Data.ProtoLens.fieldDefault,
         _ExportMetricsPartialSuccess'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExportMetricsPartialSuccess
          -> Data.ProtoLens.Encoding.Bytes.Parser ExportMetricsPartialSuccess
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "rejected_data_points"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"rejectedDataPoints") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "error_message"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"errorMessage") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ExportMetricsPartialSuccess"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"rejectedDataPoints") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"errorMessage") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((Prelude..)
                            (\ bs
                               -> (Data.Monoid.<>)
                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                       (Prelude.fromIntegral (Data.ByteString.length bs)))
                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            Data.Text.Encoding.encodeUtf8 _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData ExportMetricsPartialSuccess where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExportMetricsPartialSuccess'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExportMetricsPartialSuccess'rejectedDataPoints x__)
                (Control.DeepSeq.deepseq
                   (_ExportMetricsPartialSuccess'errorMessage x__) ()))
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields.resourceMetrics' @:: Lens' ExportMetricsServiceRequest [Proto.Opentelemetry.Proto.Metrics.V1.Metrics.ResourceMetrics]@
         * 'Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields.vec'resourceMetrics' @:: Lens' ExportMetricsServiceRequest (Data.Vector.Vector Proto.Opentelemetry.Proto.Metrics.V1.Metrics.ResourceMetrics)@ -}
data ExportMetricsServiceRequest
  = ExportMetricsServiceRequest'_constructor {_ExportMetricsServiceRequest'resourceMetrics :: !(Data.Vector.Vector Proto.Opentelemetry.Proto.Metrics.V1.Metrics.ResourceMetrics),
                                              _ExportMetricsServiceRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExportMetricsServiceRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExportMetricsServiceRequest "resourceMetrics" [Proto.Opentelemetry.Proto.Metrics.V1.Metrics.ResourceMetrics] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportMetricsServiceRequest'resourceMetrics
           (\ x__ y__
              -> x__ {_ExportMetricsServiceRequest'resourceMetrics = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExportMetricsServiceRequest "vec'resourceMetrics" (Data.Vector.Vector Proto.Opentelemetry.Proto.Metrics.V1.Metrics.ResourceMetrics) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportMetricsServiceRequest'resourceMetrics
           (\ x__ y__
              -> x__ {_ExportMetricsServiceRequest'resourceMetrics = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExportMetricsServiceRequest where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.collector.metrics.v1.ExportMetricsServiceRequest"
  packedMessageDescriptor _
    = "\n\
      \\ESCExportMetricsServiceRequest\DC2Z\n\
      \\DLEresource_metrics\CAN\SOH \ETX(\v2/.opentelemetry.proto.metrics.v1.ResourceMetricsR\SIresourceMetrics"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        resourceMetrics__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "resource_metrics"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Opentelemetry.Proto.Metrics.V1.Metrics.ResourceMetrics)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"resourceMetrics")) ::
              Data.ProtoLens.FieldDescriptor ExportMetricsServiceRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, resourceMetrics__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExportMetricsServiceRequest'_unknownFields
        (\ x__ y__
           -> x__ {_ExportMetricsServiceRequest'_unknownFields = y__})
  defMessage
    = ExportMetricsServiceRequest'_constructor
        {_ExportMetricsServiceRequest'resourceMetrics = Data.Vector.Generic.empty,
         _ExportMetricsServiceRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExportMetricsServiceRequest
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Proto.Opentelemetry.Proto.Metrics.V1.Metrics.ResourceMetrics
             -> Data.ProtoLens.Encoding.Bytes.Parser ExportMetricsServiceRequest
        loop x mutable'resourceMetrics
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'resourceMetrics <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                  (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                     mutable'resourceMetrics)
                      (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t)
                           (Lens.Family2.set
                              (Data.ProtoLens.Field.field @"vec'resourceMetrics")
                              frozen'resourceMetrics x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "resource_metrics"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'resourceMetrics y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'resourceMetrics
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'resourceMetrics <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                           Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'resourceMetrics)
          "ExportMetricsServiceRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                (\ _v
                   -> (Data.Monoid.<>)
                        (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                        ((Prelude..)
                           (\ bs
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                      (Prelude.fromIntegral (Data.ByteString.length bs)))
                                   (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                           Data.ProtoLens.encodeMessage _v))
                (Lens.Family2.view
                   (Data.ProtoLens.Field.field @"vec'resourceMetrics") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ExportMetricsServiceRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExportMetricsServiceRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExportMetricsServiceRequest'resourceMetrics x__) ())
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields.partialSuccess' @:: Lens' ExportMetricsServiceResponse ExportMetricsPartialSuccess@
         * 'Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields.maybe'partialSuccess' @:: Lens' ExportMetricsServiceResponse (Prelude.Maybe ExportMetricsPartialSuccess)@ -}
data ExportMetricsServiceResponse
  = ExportMetricsServiceResponse'_constructor {_ExportMetricsServiceResponse'partialSuccess :: !(Prelude.Maybe ExportMetricsPartialSuccess),
                                               _ExportMetricsServiceResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExportMetricsServiceResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExportMetricsServiceResponse "partialSuccess" ExportMetricsPartialSuccess where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportMetricsServiceResponse'partialSuccess
           (\ x__ y__
              -> x__ {_ExportMetricsServiceResponse'partialSuccess = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ExportMetricsServiceResponse "maybe'partialSuccess" (Prelude.Maybe ExportMetricsPartialSuccess) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportMetricsServiceResponse'partialSuccess
           (\ x__ y__
              -> x__ {_ExportMetricsServiceResponse'partialSuccess = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExportMetricsServiceResponse where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.collector.metrics.v1.ExportMetricsServiceResponse"
  packedMessageDescriptor _
    = "\n\
      \\FSExportMetricsServiceResponse\DC2n\n\
      \\SIpartial_success\CAN\SOH \SOH(\v2E.opentelemetry.proto.collector.metrics.v1.ExportMetricsPartialSuccessR\SOpartialSuccess"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        partialSuccess__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "partial_success"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExportMetricsPartialSuccess)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'partialSuccess")) ::
              Data.ProtoLens.FieldDescriptor ExportMetricsServiceResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, partialSuccess__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExportMetricsServiceResponse'_unknownFields
        (\ x__ y__
           -> x__ {_ExportMetricsServiceResponse'_unknownFields = y__})
  defMessage
    = ExportMetricsServiceResponse'_constructor
        {_ExportMetricsServiceResponse'partialSuccess = Prelude.Nothing,
         _ExportMetricsServiceResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExportMetricsServiceResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser ExportMetricsServiceResponse
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "partial_success"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"partialSuccess") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ExportMetricsServiceResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view
                    (Data.ProtoLens.Field.field @"maybe'partialSuccess") _x
              of
                Prelude.Nothing -> Data.Monoid.mempty
                (Prelude.Just _v)
                  -> (Data.Monoid.<>)
                       (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                       ((Prelude..)
                          (\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                          Data.ProtoLens.encodeMessage _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ExportMetricsServiceResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExportMetricsServiceResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExportMetricsServiceResponse'partialSuccess x__) ())
data MetricsService = MetricsService {}
instance Data.ProtoLens.Service.Types.Service MetricsService where
  type ServiceName MetricsService = "MetricsService"
  type ServicePackage MetricsService = "opentelemetry.proto.collector.metrics.v1"
  type ServiceMethods MetricsService = '["export"]
  packedServiceDescriptor _
    = "\n\
      \\SOMetricsService\DC2\153\SOH\n\
      \\ACKExport\DC2E.opentelemetry.proto.collector.metrics.v1.ExportMetricsServiceRequest\SUBF.opentelemetry.proto.collector.metrics.v1.ExportMetricsServiceResponse\"\NUL"
instance Data.ProtoLens.Service.Types.HasMethodImpl MetricsService "export" where
  type MethodName MetricsService "export" = "Export"
  type MethodInput MetricsService "export" = ExportMetricsServiceRequest
  type MethodOutput MetricsService "export" = ExportMetricsServiceResponse
  type MethodStreamingType MetricsService "export" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \>opentelemetry/proto/collector/metrics/v1/metrics_service.proto\DC2(opentelemetry.proto.collector.metrics.v1\SUB,opentelemetry/proto/metrics/v1/metrics.proto\"y\n\
    \\ESCExportMetricsServiceRequest\DC2Z\n\
    \\DLEresource_metrics\CAN\SOH \ETX(\v2/.opentelemetry.proto.metrics.v1.ResourceMetricsR\SIresourceMetrics\"\142\SOH\n\
    \\FSExportMetricsServiceResponse\DC2n\n\
    \\SIpartial_success\CAN\SOH \SOH(\v2E.opentelemetry.proto.collector.metrics.v1.ExportMetricsPartialSuccessR\SOpartialSuccess\"t\n\
    \\ESCExportMetricsPartialSuccess\DC20\n\
    \\DC4rejected_data_points\CAN\SOH \SOH(\ETXR\DC2rejectedDataPoints\DC2#\n\
    \\rerror_message\CAN\STX \SOH(\tR\ferrorMessage2\172\SOH\n\
    \\SOMetricsService\DC2\153\SOH\n\
    \\ACKExport\DC2E.opentelemetry.proto.collector.metrics.v1.ExportMetricsServiceRequest\SUBF.opentelemetry.proto.collector.metrics.v1.ExportMetricsServiceResponse\"\NULB\164\SOH\n\
    \+io.opentelemetry.proto.collector.metrics.v1B\DC3MetricsServiceProtoP\SOHZ3go.opentelemetry.io/proto/otlp/collector/metrics/v1\170\STX(OpenTelemetry.Proto.Collector.Metrics.V1J\219\ETB\n\
    \\ACK\DC2\EOT\SO\NULN\SOH\n\
    \\200\EOT\n\
    \\SOH\f\DC2\ETX\SO\NUL\DC22\189\EOT Copyright 2019, OpenTelemetry Authors\n\
    \\n\
    \ Licensed under the Apache License, Version 2.0 (the \"License\");\n\
    \ you may not use this file except in compliance with the License.\n\
    \ You may obtain a copy of the License at\n\
    \\n\
    \     http://www.apache.org/licenses/LICENSE-2.0\n\
    \\n\
    \ Unless required by applicable law or agreed to in writing, software\n\
    \ distributed under the License is distributed on an \"AS IS\" BASIS,\n\
    \ WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n\
    \ See the License for the specific language governing permissions and\n\
    \ limitations under the License.\n\
    \\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\DLE\NUL1\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\DC2\NUL6\n\
    \\b\n\
    \\SOH\b\DC2\ETX\DC4\NULE\n\
    \\t\n\
    \\STX\b%\DC2\ETX\DC4\NULE\n\
    \\b\n\
    \\SOH\b\DC2\ETX\NAK\NUL\"\n\
    \\t\n\
    \\STX\b\n\
    \\DC2\ETX\NAK\NUL\"\n\
    \\b\n\
    \\SOH\b\DC2\ETX\SYN\NULD\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX\SYN\NULD\n\
    \\b\n\
    \\SOH\b\DC2\ETX\ETB\NUL4\n\
    \\t\n\
    \\STX\b\b\DC2\ETX\ETB\NUL4\n\
    \\b\n\
    \\SOH\b\DC2\ETX\CAN\NULJ\n\
    \\t\n\
    \\STX\b\v\DC2\ETX\CAN\NULJ\n\
    \\178\SOH\n\
    \\STX\ACK\NUL\DC2\EOT\GS\NUL!\SOH\SUB\165\SOH Service that can be used to push metrics between one Application\n\
    \ instrumented with OpenTelemetry and a collector, or between a collector and a\n\
    \ central collector.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\GS\b\SYN\n\
    \y\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX \STXS\SUBl For performance reasons, it is recommended to keep this RPC\n\
    \ alive for the entire life of the application.\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX \ACK\f\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX \r(\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX 3O\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT#\NUL*\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX#\b#\n\
    \\210\STX\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX)\STXO\SUB\196\STX An array of ResourceMetrics.\n\
    \ For data coming from a single resource this array will typically contain one\n\
    \ element. Intermediary nodes (such as OpenTelemetry Collector) that receive\n\
    \ data from multiple origins typically batch the data before forwarding further and\n\
    \ in that case this array will contain multiple elements.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\EOT\DC2\ETX)\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETX)\v9\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX):J\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX)MN\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT,\NUL=\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX,\b$\n\
    \\148\ACK\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX<\STX2\SUB\134\ACK The details of a partially successful export request.\n\
    \\n\
    \ If the request is only partially accepted\n\
    \ (i.e. when the server accepts only parts of the data and rejects the rest)\n\
    \ the server MUST initialize the `partial_success` field and MUST\n\
    \ set the `rejected_<signal>` with the number of items it rejected.\n\
    \\n\
    \ Servers MAY also make use of the `partial_success` field to convey\n\
    \ warnings/suggestions to senders even when the request was fully accepted.\n\
    \ In such cases, the `rejected_<signal>` MUST have a value of `0` and\n\
    \ the `error_message` MUST be non-empty.\n\
    \\n\
    \ A `partial_success` message with an empty value (rejected_<signal> = 0 and\n\
    \ `error_message` = \"\") is equivalent to it not being set/present. Senders\n\
    \ SHOULD interpret it the same way as in the full success case.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETX<\STX\GS\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX<\RS-\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX<01\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT?\NULN\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX?\b#\n\
    \\149\SOH\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETXD\STX!\SUB\135\SOH The number of rejected data points.\n\
    \\n\
    \ A `rejected_<signal>` field holding a `0` value indicates that the\n\
    \ request was fully accepted.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETXD\STX\a\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETXD\b\FS\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETXD\US \n\
    \\159\ETX\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETXM\STX\ESC\SUB\145\ETX A developer-facing human-readable message in English. It should be used\n\
    \ either to explain why the server rejected parts of the data during a partial\n\
    \ success or to convey warnings/suggestions during a full success. The message\n\
    \ should offer guidance on how users can address such issues.\n\
    \\n\
    \ error_message is an optional field. An error_message with an empty value\n\
    \ is equivalent to it not being set.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ENQ\DC2\ETXM\STX\b\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETXM\t\SYN\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETXM\EM\SUBb\ACKproto3"