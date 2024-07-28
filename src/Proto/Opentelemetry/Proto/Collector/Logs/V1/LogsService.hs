{- This file was auto-generated from opentelemetry/proto/collector/logs/v1/logs_service.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService (
        LogsService(..), ExportLogsPartialSuccess(),
        ExportLogsServiceRequest(), ExportLogsServiceResponse()
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
import qualified Proto.Opentelemetry.Proto.Logs.V1.Logs
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService_Fields.rejectedLogRecords' @:: Lens' ExportLogsPartialSuccess Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService_Fields.errorMessage' @:: Lens' ExportLogsPartialSuccess Data.Text.Text@ -}
data ExportLogsPartialSuccess
  = ExportLogsPartialSuccess'_constructor {_ExportLogsPartialSuccess'rejectedLogRecords :: !Data.Int.Int64,
                                           _ExportLogsPartialSuccess'errorMessage :: !Data.Text.Text,
                                           _ExportLogsPartialSuccess'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExportLogsPartialSuccess where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExportLogsPartialSuccess "rejectedLogRecords" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportLogsPartialSuccess'rejectedLogRecords
           (\ x__ y__
              -> x__ {_ExportLogsPartialSuccess'rejectedLogRecords = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExportLogsPartialSuccess "errorMessage" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportLogsPartialSuccess'errorMessage
           (\ x__ y__ -> x__ {_ExportLogsPartialSuccess'errorMessage = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExportLogsPartialSuccess where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.collector.logs.v1.ExportLogsPartialSuccess"
  packedMessageDescriptor _
    = "\n\
      \\CANExportLogsPartialSuccess\DC20\n\
      \\DC4rejected_log_records\CAN\SOH \SOH(\ETXR\DC2rejectedLogRecords\DC2#\n\
      \\rerror_message\CAN\STX \SOH(\tR\ferrorMessage"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        rejectedLogRecords__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "rejected_log_records"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"rejectedLogRecords")) ::
              Data.ProtoLens.FieldDescriptor ExportLogsPartialSuccess
        errorMessage__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "error_message"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"errorMessage")) ::
              Data.ProtoLens.FieldDescriptor ExportLogsPartialSuccess
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, rejectedLogRecords__field_descriptor),
           (Data.ProtoLens.Tag 2, errorMessage__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExportLogsPartialSuccess'_unknownFields
        (\ x__ y__ -> x__ {_ExportLogsPartialSuccess'_unknownFields = y__})
  defMessage
    = ExportLogsPartialSuccess'_constructor
        {_ExportLogsPartialSuccess'rejectedLogRecords = Data.ProtoLens.fieldDefault,
         _ExportLogsPartialSuccess'errorMessage = Data.ProtoLens.fieldDefault,
         _ExportLogsPartialSuccess'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExportLogsPartialSuccess
          -> Data.ProtoLens.Encoding.Bytes.Parser ExportLogsPartialSuccess
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
                                       "rejected_log_records"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"rejectedLogRecords") y x)
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
          (do loop Data.ProtoLens.defMessage) "ExportLogsPartialSuccess"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"rejectedLogRecords") _x
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
instance Control.DeepSeq.NFData ExportLogsPartialSuccess where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExportLogsPartialSuccess'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExportLogsPartialSuccess'rejectedLogRecords x__)
                (Control.DeepSeq.deepseq
                   (_ExportLogsPartialSuccess'errorMessage x__) ()))
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService_Fields.resourceLogs' @:: Lens' ExportLogsServiceRequest [Proto.Opentelemetry.Proto.Logs.V1.Logs.ResourceLogs]@
         * 'Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService_Fields.vec'resourceLogs' @:: Lens' ExportLogsServiceRequest (Data.Vector.Vector Proto.Opentelemetry.Proto.Logs.V1.Logs.ResourceLogs)@ -}
data ExportLogsServiceRequest
  = ExportLogsServiceRequest'_constructor {_ExportLogsServiceRequest'resourceLogs :: !(Data.Vector.Vector Proto.Opentelemetry.Proto.Logs.V1.Logs.ResourceLogs),
                                           _ExportLogsServiceRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExportLogsServiceRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExportLogsServiceRequest "resourceLogs" [Proto.Opentelemetry.Proto.Logs.V1.Logs.ResourceLogs] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportLogsServiceRequest'resourceLogs
           (\ x__ y__ -> x__ {_ExportLogsServiceRequest'resourceLogs = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExportLogsServiceRequest "vec'resourceLogs" (Data.Vector.Vector Proto.Opentelemetry.Proto.Logs.V1.Logs.ResourceLogs) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportLogsServiceRequest'resourceLogs
           (\ x__ y__ -> x__ {_ExportLogsServiceRequest'resourceLogs = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExportLogsServiceRequest where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.collector.logs.v1.ExportLogsServiceRequest"
  packedMessageDescriptor _
    = "\n\
      \\CANExportLogsServiceRequest\DC2N\n\
      \\rresource_logs\CAN\SOH \ETX(\v2).opentelemetry.proto.logs.v1.ResourceLogsR\fresourceLogs"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        resourceLogs__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "resource_logs"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Opentelemetry.Proto.Logs.V1.Logs.ResourceLogs)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"resourceLogs")) ::
              Data.ProtoLens.FieldDescriptor ExportLogsServiceRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, resourceLogs__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExportLogsServiceRequest'_unknownFields
        (\ x__ y__ -> x__ {_ExportLogsServiceRequest'_unknownFields = y__})
  defMessage
    = ExportLogsServiceRequest'_constructor
        {_ExportLogsServiceRequest'resourceLogs = Data.Vector.Generic.empty,
         _ExportLogsServiceRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExportLogsServiceRequest
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Proto.Opentelemetry.Proto.Logs.V1.Logs.ResourceLogs
             -> Data.ProtoLens.Encoding.Bytes.Parser ExportLogsServiceRequest
        loop x mutable'resourceLogs
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'resourceLogs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                               (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                  mutable'resourceLogs)
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
                              (Data.ProtoLens.Field.field @"vec'resourceLogs")
                              frozen'resourceLogs x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "resource_logs"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'resourceLogs y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'resourceLogs
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'resourceLogs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'resourceLogs)
          "ExportLogsServiceRequest"
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
                   (Data.ProtoLens.Field.field @"vec'resourceLogs") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ExportLogsServiceRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExportLogsServiceRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExportLogsServiceRequest'resourceLogs x__) ())
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService_Fields.partialSuccess' @:: Lens' ExportLogsServiceResponse ExportLogsPartialSuccess@
         * 'Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService_Fields.maybe'partialSuccess' @:: Lens' ExportLogsServiceResponse (Prelude.Maybe ExportLogsPartialSuccess)@ -}
data ExportLogsServiceResponse
  = ExportLogsServiceResponse'_constructor {_ExportLogsServiceResponse'partialSuccess :: !(Prelude.Maybe ExportLogsPartialSuccess),
                                            _ExportLogsServiceResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExportLogsServiceResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExportLogsServiceResponse "partialSuccess" ExportLogsPartialSuccess where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportLogsServiceResponse'partialSuccess
           (\ x__ y__
              -> x__ {_ExportLogsServiceResponse'partialSuccess = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ExportLogsServiceResponse "maybe'partialSuccess" (Prelude.Maybe ExportLogsPartialSuccess) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportLogsServiceResponse'partialSuccess
           (\ x__ y__
              -> x__ {_ExportLogsServiceResponse'partialSuccess = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExportLogsServiceResponse where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.collector.logs.v1.ExportLogsServiceResponse"
  packedMessageDescriptor _
    = "\n\
      \\EMExportLogsServiceResponse\DC2h\n\
      \\SIpartial_success\CAN\SOH \SOH(\v2?.opentelemetry.proto.collector.logs.v1.ExportLogsPartialSuccessR\SOpartialSuccess"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        partialSuccess__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "partial_success"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExportLogsPartialSuccess)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'partialSuccess")) ::
              Data.ProtoLens.FieldDescriptor ExportLogsServiceResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, partialSuccess__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExportLogsServiceResponse'_unknownFields
        (\ x__ y__
           -> x__ {_ExportLogsServiceResponse'_unknownFields = y__})
  defMessage
    = ExportLogsServiceResponse'_constructor
        {_ExportLogsServiceResponse'partialSuccess = Prelude.Nothing,
         _ExportLogsServiceResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExportLogsServiceResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser ExportLogsServiceResponse
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
          (do loop Data.ProtoLens.defMessage) "ExportLogsServiceResponse"
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
instance Control.DeepSeq.NFData ExportLogsServiceResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExportLogsServiceResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExportLogsServiceResponse'partialSuccess x__) ())
data LogsService = LogsService {}
instance Data.ProtoLens.Service.Types.Service LogsService where
  type ServiceName LogsService = "LogsService"
  type ServicePackage LogsService = "opentelemetry.proto.collector.logs.v1"
  type ServiceMethods LogsService = '["export"]
  packedServiceDescriptor _
    = "\n\
      \\vLogsService\DC2\141\SOH\n\
      \\ACKExport\DC2?.opentelemetry.proto.collector.logs.v1.ExportLogsServiceRequest\SUB@.opentelemetry.proto.collector.logs.v1.ExportLogsServiceResponse\"\NUL"
instance Data.ProtoLens.Service.Types.HasMethodImpl LogsService "export" where
  type MethodName LogsService "export" = "Export"
  type MethodInput LogsService "export" = ExportLogsServiceRequest
  type MethodOutput LogsService "export" = ExportLogsServiceResponse
  type MethodStreamingType LogsService "export" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \8opentelemetry/proto/collector/logs/v1/logs_service.proto\DC2%opentelemetry.proto.collector.logs.v1\SUB&opentelemetry/proto/logs/v1/logs.proto\"j\n\
    \\CANExportLogsServiceRequest\DC2N\n\
    \\rresource_logs\CAN\SOH \ETX(\v2).opentelemetry.proto.logs.v1.ResourceLogsR\fresourceLogs\"\133\SOH\n\
    \\EMExportLogsServiceResponse\DC2h\n\
    \\SIpartial_success\CAN\SOH \SOH(\v2?.opentelemetry.proto.collector.logs.v1.ExportLogsPartialSuccessR\SOpartialSuccess\"q\n\
    \\CANExportLogsPartialSuccess\DC20\n\
    \\DC4rejected_log_records\CAN\SOH \SOH(\ETXR\DC2rejectedLogRecords\DC2#\n\
    \\rerror_message\CAN\STX \SOH(\tR\ferrorMessage2\157\SOH\n\
    \\vLogsService\DC2\141\SOH\n\
    \\ACKExport\DC2?.opentelemetry.proto.collector.logs.v1.ExportLogsServiceRequest\SUB@.opentelemetry.proto.collector.logs.v1.ExportLogsServiceResponse\"\NULB\152\SOH\n\
    \(io.opentelemetry.proto.collector.logs.v1B\DLELogsServiceProtoP\SOHZ0go.opentelemetry.io/proto/otlp/collector/logs/v1\170\STX%OpenTelemetry.Proto.Collector.Logs.V1J\155\CAN\n\
    \\ACK\DC2\EOT\SO\NULN\SOH\n\
    \\200\EOT\n\
    \\SOH\f\DC2\ETX\SO\NUL\DC22\189\EOT Copyright 2020, OpenTelemetry Authors\n\
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
    \\SOH\STX\DC2\ETX\DLE\NUL.\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\DC2\NUL0\n\
    \\b\n\
    \\SOH\b\DC2\ETX\DC4\NULB\n\
    \\t\n\
    \\STX\b%\DC2\ETX\DC4\NULB\n\
    \\b\n\
    \\SOH\b\DC2\ETX\NAK\NUL\"\n\
    \\t\n\
    \\STX\b\n\
    \\DC2\ETX\NAK\NUL\"\n\
    \\b\n\
    \\SOH\b\DC2\ETX\SYN\NULA\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX\SYN\NULA\n\
    \\b\n\
    \\SOH\b\DC2\ETX\ETB\NUL1\n\
    \\t\n\
    \\STX\b\b\DC2\ETX\ETB\NUL1\n\
    \\b\n\
    \\SOH\b\DC2\ETX\CAN\NULG\n\
    \\t\n\
    \\STX\b\v\DC2\ETX\CAN\NULG\n\
    \\245\SOH\n\
    \\STX\ACK\NUL\DC2\EOT\GS\NUL!\SOH\SUB\232\SOH Service that can be used to push logs between one Application instrumented with\n\
    \ OpenTelemetry and an collector, or between an collector and a central collector (in this\n\
    \ case logs are sent/received to/from multiple Applications).\n\
    \\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\GS\b\DC3\n\
    \y\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX \STXM\SUBl For performance reasons, it is recommended to keep this RPC\n\
    \ alive for the entire life of the application.\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX \ACK\f\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX \r%\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX 0I\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT#\NUL*\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX#\b \n\
    \\207\STX\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX)\STXF\SUB\193\STX An array of ResourceLogs.\n\
    \ For data coming from a single resource this array will typically contain one\n\
    \ element. Intermediary nodes (such as OpenTelemetry Collector) that receive\n\
    \ data from multiple origins typically batch the data before forwarding further and\n\
    \ in that case this array will contain multiple elements.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\EOT\DC2\ETX)\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETX)\v3\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX)4A\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX)DE\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT,\NUL=\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX,\b!\n\
    \\148\ACK\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX<\STX/\SUB\134\ACK The details of a partially successful export request.\n\
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
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETX<\STX\SUB\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX<\ESC*\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX<-.\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT?\NULN\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX?\b \n\
    \\149\SOH\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETXD\STX!\SUB\135\SOH The number of rejected log records.\n\
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