{- This file was auto-generated from opentelemetry/proto/collector/trace/v1/trace_service.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService (
        TraceService(..), ExportTracePartialSuccess(),
        ExportTraceServiceRequest(), ExportTraceServiceResponse()
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
import qualified Proto.Opentelemetry.Proto.Trace.V1.Trace
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields.rejectedSpans' @:: Lens' ExportTracePartialSuccess Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields.errorMessage' @:: Lens' ExportTracePartialSuccess Data.Text.Text@ -}
data ExportTracePartialSuccess
  = ExportTracePartialSuccess'_constructor {_ExportTracePartialSuccess'rejectedSpans :: !Data.Int.Int64,
                                            _ExportTracePartialSuccess'errorMessage :: !Data.Text.Text,
                                            _ExportTracePartialSuccess'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExportTracePartialSuccess where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExportTracePartialSuccess "rejectedSpans" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportTracePartialSuccess'rejectedSpans
           (\ x__ y__
              -> x__ {_ExportTracePartialSuccess'rejectedSpans = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExportTracePartialSuccess "errorMessage" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportTracePartialSuccess'errorMessage
           (\ x__ y__ -> x__ {_ExportTracePartialSuccess'errorMessage = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExportTracePartialSuccess where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.collector.trace.v1.ExportTracePartialSuccess"
  packedMessageDescriptor _
    = "\n\
      \\EMExportTracePartialSuccess\DC2%\n\
      \\SOrejected_spans\CAN\SOH \SOH(\ETXR\rrejectedSpans\DC2#\n\
      \\rerror_message\CAN\STX \SOH(\tR\ferrorMessage"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        rejectedSpans__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "rejected_spans"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"rejectedSpans")) ::
              Data.ProtoLens.FieldDescriptor ExportTracePartialSuccess
        errorMessage__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "error_message"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"errorMessage")) ::
              Data.ProtoLens.FieldDescriptor ExportTracePartialSuccess
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, rejectedSpans__field_descriptor),
           (Data.ProtoLens.Tag 2, errorMessage__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExportTracePartialSuccess'_unknownFields
        (\ x__ y__
           -> x__ {_ExportTracePartialSuccess'_unknownFields = y__})
  defMessage
    = ExportTracePartialSuccess'_constructor
        {_ExportTracePartialSuccess'rejectedSpans = Data.ProtoLens.fieldDefault,
         _ExportTracePartialSuccess'errorMessage = Data.ProtoLens.fieldDefault,
         _ExportTracePartialSuccess'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExportTracePartialSuccess
          -> Data.ProtoLens.Encoding.Bytes.Parser ExportTracePartialSuccess
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
                                       "rejected_spans"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"rejectedSpans") y x)
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
          (do loop Data.ProtoLens.defMessage) "ExportTracePartialSuccess"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"rejectedSpans") _x
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
instance Control.DeepSeq.NFData ExportTracePartialSuccess where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExportTracePartialSuccess'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExportTracePartialSuccess'rejectedSpans x__)
                (Control.DeepSeq.deepseq
                   (_ExportTracePartialSuccess'errorMessage x__) ()))
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields.resourceSpans' @:: Lens' ExportTraceServiceRequest [Proto.Opentelemetry.Proto.Trace.V1.Trace.ResourceSpans]@
         * 'Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields.vec'resourceSpans' @:: Lens' ExportTraceServiceRequest (Data.Vector.Vector Proto.Opentelemetry.Proto.Trace.V1.Trace.ResourceSpans)@ -}
data ExportTraceServiceRequest
  = ExportTraceServiceRequest'_constructor {_ExportTraceServiceRequest'resourceSpans :: !(Data.Vector.Vector Proto.Opentelemetry.Proto.Trace.V1.Trace.ResourceSpans),
                                            _ExportTraceServiceRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExportTraceServiceRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExportTraceServiceRequest "resourceSpans" [Proto.Opentelemetry.Proto.Trace.V1.Trace.ResourceSpans] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportTraceServiceRequest'resourceSpans
           (\ x__ y__
              -> x__ {_ExportTraceServiceRequest'resourceSpans = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExportTraceServiceRequest "vec'resourceSpans" (Data.Vector.Vector Proto.Opentelemetry.Proto.Trace.V1.Trace.ResourceSpans) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportTraceServiceRequest'resourceSpans
           (\ x__ y__
              -> x__ {_ExportTraceServiceRequest'resourceSpans = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExportTraceServiceRequest where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.collector.trace.v1.ExportTraceServiceRequest"
  packedMessageDescriptor _
    = "\n\
      \\EMExportTraceServiceRequest\DC2R\n\
      \\SOresource_spans\CAN\SOH \ETX(\v2+.opentelemetry.proto.trace.v1.ResourceSpansR\rresourceSpans"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        resourceSpans__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "resource_spans"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Opentelemetry.Proto.Trace.V1.Trace.ResourceSpans)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"resourceSpans")) ::
              Data.ProtoLens.FieldDescriptor ExportTraceServiceRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, resourceSpans__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExportTraceServiceRequest'_unknownFields
        (\ x__ y__
           -> x__ {_ExportTraceServiceRequest'_unknownFields = y__})
  defMessage
    = ExportTraceServiceRequest'_constructor
        {_ExportTraceServiceRequest'resourceSpans = Data.Vector.Generic.empty,
         _ExportTraceServiceRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExportTraceServiceRequest
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Proto.Opentelemetry.Proto.Trace.V1.Trace.ResourceSpans
             -> Data.ProtoLens.Encoding.Bytes.Parser ExportTraceServiceRequest
        loop x mutable'resourceSpans
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'resourceSpans <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                   mutable'resourceSpans)
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
                              (Data.ProtoLens.Field.field @"vec'resourceSpans")
                              frozen'resourceSpans x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "resource_spans"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'resourceSpans y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'resourceSpans
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'resourceSpans <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'resourceSpans)
          "ExportTraceServiceRequest"
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
                   (Data.ProtoLens.Field.field @"vec'resourceSpans") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ExportTraceServiceRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExportTraceServiceRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExportTraceServiceRequest'resourceSpans x__) ())
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields.partialSuccess' @:: Lens' ExportTraceServiceResponse ExportTracePartialSuccess@
         * 'Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields.maybe'partialSuccess' @:: Lens' ExportTraceServiceResponse (Prelude.Maybe ExportTracePartialSuccess)@ -}
data ExportTraceServiceResponse
  = ExportTraceServiceResponse'_constructor {_ExportTraceServiceResponse'partialSuccess :: !(Prelude.Maybe ExportTracePartialSuccess),
                                             _ExportTraceServiceResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExportTraceServiceResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExportTraceServiceResponse "partialSuccess" ExportTracePartialSuccess where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportTraceServiceResponse'partialSuccess
           (\ x__ y__
              -> x__ {_ExportTraceServiceResponse'partialSuccess = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ExportTraceServiceResponse "maybe'partialSuccess" (Prelude.Maybe ExportTracePartialSuccess) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportTraceServiceResponse'partialSuccess
           (\ x__ y__
              -> x__ {_ExportTraceServiceResponse'partialSuccess = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExportTraceServiceResponse where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.collector.trace.v1.ExportTraceServiceResponse"
  packedMessageDescriptor _
    = "\n\
      \\SUBExportTraceServiceResponse\DC2j\n\
      \\SIpartial_success\CAN\SOH \SOH(\v2A.opentelemetry.proto.collector.trace.v1.ExportTracePartialSuccessR\SOpartialSuccess"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        partialSuccess__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "partial_success"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExportTracePartialSuccess)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'partialSuccess")) ::
              Data.ProtoLens.FieldDescriptor ExportTraceServiceResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, partialSuccess__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExportTraceServiceResponse'_unknownFields
        (\ x__ y__
           -> x__ {_ExportTraceServiceResponse'_unknownFields = y__})
  defMessage
    = ExportTraceServiceResponse'_constructor
        {_ExportTraceServiceResponse'partialSuccess = Prelude.Nothing,
         _ExportTraceServiceResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExportTraceServiceResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser ExportTraceServiceResponse
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
          (do loop Data.ProtoLens.defMessage) "ExportTraceServiceResponse"
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
instance Control.DeepSeq.NFData ExportTraceServiceResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExportTraceServiceResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExportTraceServiceResponse'partialSuccess x__) ())
data TraceService = TraceService {}
instance Data.ProtoLens.Service.Types.Service TraceService where
  type ServiceName TraceService = "TraceService"
  type ServicePackage TraceService = "opentelemetry.proto.collector.trace.v1"
  type ServiceMethods TraceService = '["export"]
  packedServiceDescriptor _
    = "\n\
      \\fTraceService\DC2\145\SOH\n\
      \\ACKExport\DC2A.opentelemetry.proto.collector.trace.v1.ExportTraceServiceRequest\SUBB.opentelemetry.proto.collector.trace.v1.ExportTraceServiceResponse\"\NUL"
instance Data.ProtoLens.Service.Types.HasMethodImpl TraceService "export" where
  type MethodName TraceService "export" = "Export"
  type MethodInput TraceService "export" = ExportTraceServiceRequest
  type MethodOutput TraceService "export" = ExportTraceServiceResponse
  type MethodStreamingType TraceService "export" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \:opentelemetry/proto/collector/trace/v1/trace_service.proto\DC2&opentelemetry.proto.collector.trace.v1\SUB(opentelemetry/proto/trace/v1/trace.proto\"o\n\
    \\EMExportTraceServiceRequest\DC2R\n\
    \\SOresource_spans\CAN\SOH \ETX(\v2+.opentelemetry.proto.trace.v1.ResourceSpansR\rresourceSpans\"\136\SOH\n\
    \\SUBExportTraceServiceResponse\DC2j\n\
    \\SIpartial_success\CAN\SOH \SOH(\v2A.opentelemetry.proto.collector.trace.v1.ExportTracePartialSuccessR\SOpartialSuccess\"g\n\
    \\EMExportTracePartialSuccess\DC2%\n\
    \\SOrejected_spans\CAN\SOH \SOH(\ETXR\rrejectedSpans\DC2#\n\
    \\rerror_message\CAN\STX \SOH(\tR\ferrorMessage2\162\SOH\n\
    \\fTraceService\DC2\145\SOH\n\
    \\ACKExport\DC2A.opentelemetry.proto.collector.trace.v1.ExportTraceServiceRequest\SUBB.opentelemetry.proto.collector.trace.v1.ExportTraceServiceResponse\"\NULB\156\SOH\n\
    \)io.opentelemetry.proto.collector.trace.v1B\DC1TraceServiceProtoP\SOHZ1go.opentelemetry.io/proto/otlp/collector/trace/v1\170\STX&OpenTelemetry.Proto.Collector.Trace.V1J\150\CAN\n\
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
    \\SOH\STX\DC2\ETX\DLE\NUL/\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\DC2\NUL2\n\
    \\b\n\
    \\SOH\b\DC2\ETX\DC4\NULC\n\
    \\t\n\
    \\STX\b%\DC2\ETX\DC4\NULC\n\
    \\b\n\
    \\SOH\b\DC2\ETX\NAK\NUL\"\n\
    \\t\n\
    \\STX\b\n\
    \\DC2\ETX\NAK\NUL\"\n\
    \\b\n\
    \\SOH\b\DC2\ETX\SYN\NULB\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX\SYN\NULB\n\
    \\b\n\
    \\SOH\b\DC2\ETX\ETB\NUL2\n\
    \\t\n\
    \\STX\b\b\DC2\ETX\ETB\NUL2\n\
    \\b\n\
    \\SOH\b\DC2\ETX\CAN\NULH\n\
    \\t\n\
    \\STX\b\v\DC2\ETX\CAN\NULH\n\
    \\245\SOH\n\
    \\STX\ACK\NUL\DC2\EOT\GS\NUL!\SOH\SUB\232\SOH Service that can be used to push spans between one Application instrumented with\n\
    \ OpenTelemetry and a collector, or between a collector and a central collector (in this\n\
    \ case spans are sent/received to/from multiple Applications).\n\
    \\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\GS\b\DC4\n\
    \y\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX \STXO\SUBl For performance reasons, it is recommended to keep this RPC\n\
    \ alive for the entire life of the application.\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX \ACK\f\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX \r&\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX 1K\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT#\NUL*\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX#\b!\n\
    \\208\STX\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX)\STXI\SUB\194\STX An array of ResourceSpans.\n\
    \ For data coming from a single resource this array will typically contain one\n\
    \ element. Intermediary nodes (such as OpenTelemetry Collector) that receive\n\
    \ data from multiple origins typically batch the data before forwarding further and\n\
    \ in that case this array will contain multiple elements.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\EOT\DC2\ETX)\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETX)\v5\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX)6D\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX)GH\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT,\NUL=\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX,\b\"\n\
    \\148\ACK\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX<\STX0\SUB\134\ACK The details of a partially successful export request.\n\
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
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETX<\STX\ESC\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX<\FS+\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX<./\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT?\NULN\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX?\b!\n\
    \\143\SOH\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETXD\STX\ESC\SUB\129\SOH The number of rejected spans.\n\
    \\n\
    \ A `rejected_<signal>` field holding a `0` value indicates that the\n\
    \ request was fully accepted.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETXD\STX\a\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETXD\b\SYN\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETXD\EM\SUB\n\
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