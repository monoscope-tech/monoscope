{- This file was auto-generated from opentelemetry/proto/logs/v1/logs.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Opentelemetry.Proto.Logs.V1.Logs (
        LogRecord(), LogRecordFlags(..), LogRecordFlags(),
        LogRecordFlags'UnrecognizedValue, LogsData(), ResourceLogs(),
        ScopeLogs(), SeverityNumber(..), SeverityNumber(),
        SeverityNumber'UnrecognizedValue
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
import qualified Proto.Opentelemetry.Proto.Common.V1.Common
import qualified Proto.Opentelemetry.Proto.Resource.V1.Resource
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.timeUnixNano' @:: Lens' LogRecord Data.Word.Word64@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.observedTimeUnixNano' @:: Lens' LogRecord Data.Word.Word64@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.severityNumber' @:: Lens' LogRecord SeverityNumber@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.severityText' @:: Lens' LogRecord Data.Text.Text@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.body' @:: Lens' LogRecord Proto.Opentelemetry.Proto.Common.V1.Common.AnyValue@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.maybe'body' @:: Lens' LogRecord (Prelude.Maybe Proto.Opentelemetry.Proto.Common.V1.Common.AnyValue)@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.attributes' @:: Lens' LogRecord [Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue]@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.vec'attributes' @:: Lens' LogRecord (Data.Vector.Vector Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue)@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.droppedAttributesCount' @:: Lens' LogRecord Data.Word.Word32@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.flags' @:: Lens' LogRecord Data.Word.Word32@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.traceId' @:: Lens' LogRecord Data.ByteString.ByteString@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.spanId' @:: Lens' LogRecord Data.ByteString.ByteString@ -}
data LogRecord
  = LogRecord'_constructor {_LogRecord'timeUnixNano :: !Data.Word.Word64,
                            _LogRecord'observedTimeUnixNano :: !Data.Word.Word64,
                            _LogRecord'severityNumber :: !SeverityNumber,
                            _LogRecord'severityText :: !Data.Text.Text,
                            _LogRecord'body :: !(Prelude.Maybe Proto.Opentelemetry.Proto.Common.V1.Common.AnyValue),
                            _LogRecord'attributes :: !(Data.Vector.Vector Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue),
                            _LogRecord'droppedAttributesCount :: !Data.Word.Word32,
                            _LogRecord'flags :: !Data.Word.Word32,
                            _LogRecord'traceId :: !Data.ByteString.ByteString,
                            _LogRecord'spanId :: !Data.ByteString.ByteString,
                            _LogRecord'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LogRecord where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LogRecord "timeUnixNano" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LogRecord'timeUnixNano
           (\ x__ y__ -> x__ {_LogRecord'timeUnixNano = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LogRecord "observedTimeUnixNano" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LogRecord'observedTimeUnixNano
           (\ x__ y__ -> x__ {_LogRecord'observedTimeUnixNano = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LogRecord "severityNumber" SeverityNumber where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LogRecord'severityNumber
           (\ x__ y__ -> x__ {_LogRecord'severityNumber = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LogRecord "severityText" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LogRecord'severityText
           (\ x__ y__ -> x__ {_LogRecord'severityText = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LogRecord "body" Proto.Opentelemetry.Proto.Common.V1.Common.AnyValue where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LogRecord'body (\ x__ y__ -> x__ {_LogRecord'body = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField LogRecord "maybe'body" (Prelude.Maybe Proto.Opentelemetry.Proto.Common.V1.Common.AnyValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LogRecord'body (\ x__ y__ -> x__ {_LogRecord'body = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LogRecord "attributes" [Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LogRecord'attributes
           (\ x__ y__ -> x__ {_LogRecord'attributes = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField LogRecord "vec'attributes" (Data.Vector.Vector Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LogRecord'attributes
           (\ x__ y__ -> x__ {_LogRecord'attributes = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LogRecord "droppedAttributesCount" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LogRecord'droppedAttributesCount
           (\ x__ y__ -> x__ {_LogRecord'droppedAttributesCount = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LogRecord "flags" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LogRecord'flags (\ x__ y__ -> x__ {_LogRecord'flags = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LogRecord "traceId" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LogRecord'traceId (\ x__ y__ -> x__ {_LogRecord'traceId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField LogRecord "spanId" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LogRecord'spanId (\ x__ y__ -> x__ {_LogRecord'spanId = y__}))
        Prelude.id
instance Data.ProtoLens.Message LogRecord where
  messageName _
    = Data.Text.pack "opentelemetry.proto.logs.v1.LogRecord"
  packedMessageDescriptor _
    = "\n\
      \\tLogRecord\DC2$\n\
      \\SOtime_unix_nano\CAN\SOH \SOH(\ACKR\ftimeUnixNano\DC25\n\
      \\ETBobserved_time_unix_nano\CAN\v \SOH(\ACKR\DC4observedTimeUnixNano\DC2T\n\
      \\SIseverity_number\CAN\STX \SOH(\SO2+.opentelemetry.proto.logs.v1.SeverityNumberR\SOseverityNumber\DC2#\n\
      \\rseverity_text\CAN\ETX \SOH(\tR\fseverityText\DC2;\n\
      \\EOTbody\CAN\ENQ \SOH(\v2'.opentelemetry.proto.common.v1.AnyValueR\EOTbody\DC2G\n\
      \\n\
      \attributes\CAN\ACK \ETX(\v2'.opentelemetry.proto.common.v1.KeyValueR\n\
      \attributes\DC28\n\
      \\CANdropped_attributes_count\CAN\a \SOH(\rR\SYNdroppedAttributesCount\DC2\DC4\n\
      \\ENQflags\CAN\b \SOH(\aR\ENQflags\DC2\EM\n\
      \\btrace_id\CAN\t \SOH(\fR\atraceId\DC2\ETB\n\
      \\aspan_id\CAN\n\
      \ \SOH(\fR\ACKspanIdJ\EOT\b\EOT\DLE\ENQ"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        timeUnixNano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "time_unix_nano"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Fixed64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"timeUnixNano")) ::
              Data.ProtoLens.FieldDescriptor LogRecord
        observedTimeUnixNano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "observed_time_unix_nano"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Fixed64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"observedTimeUnixNano")) ::
              Data.ProtoLens.FieldDescriptor LogRecord
        severityNumber__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "severity_number"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor SeverityNumber)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"severityNumber")) ::
              Data.ProtoLens.FieldDescriptor LogRecord
        severityText__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "severity_text"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"severityText")) ::
              Data.ProtoLens.FieldDescriptor LogRecord
        body__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "body"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Opentelemetry.Proto.Common.V1.Common.AnyValue)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'body")) ::
              Data.ProtoLens.FieldDescriptor LogRecord
        attributes__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "attributes"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"attributes")) ::
              Data.ProtoLens.FieldDescriptor LogRecord
        droppedAttributesCount__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "dropped_attributes_count"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"droppedAttributesCount")) ::
              Data.ProtoLens.FieldDescriptor LogRecord
        flags__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "flags"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Fixed32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"flags")) ::
              Data.ProtoLens.FieldDescriptor LogRecord
        traceId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "trace_id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"traceId")) ::
              Data.ProtoLens.FieldDescriptor LogRecord
        spanId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "span_id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"spanId")) ::
              Data.ProtoLens.FieldDescriptor LogRecord
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, timeUnixNano__field_descriptor),
           (Data.ProtoLens.Tag 11, observedTimeUnixNano__field_descriptor),
           (Data.ProtoLens.Tag 2, severityNumber__field_descriptor),
           (Data.ProtoLens.Tag 3, severityText__field_descriptor),
           (Data.ProtoLens.Tag 5, body__field_descriptor),
           (Data.ProtoLens.Tag 6, attributes__field_descriptor),
           (Data.ProtoLens.Tag 7, droppedAttributesCount__field_descriptor),
           (Data.ProtoLens.Tag 8, flags__field_descriptor),
           (Data.ProtoLens.Tag 9, traceId__field_descriptor),
           (Data.ProtoLens.Tag 10, spanId__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LogRecord'_unknownFields
        (\ x__ y__ -> x__ {_LogRecord'_unknownFields = y__})
  defMessage
    = LogRecord'_constructor
        {_LogRecord'timeUnixNano = Data.ProtoLens.fieldDefault,
         _LogRecord'observedTimeUnixNano = Data.ProtoLens.fieldDefault,
         _LogRecord'severityNumber = Data.ProtoLens.fieldDefault,
         _LogRecord'severityText = Data.ProtoLens.fieldDefault,
         _LogRecord'body = Prelude.Nothing,
         _LogRecord'attributes = Data.Vector.Generic.empty,
         _LogRecord'droppedAttributesCount = Data.ProtoLens.fieldDefault,
         _LogRecord'flags = Data.ProtoLens.fieldDefault,
         _LogRecord'traceId = Data.ProtoLens.fieldDefault,
         _LogRecord'spanId = Data.ProtoLens.fieldDefault,
         _LogRecord'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LogRecord
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue
             -> Data.ProtoLens.Encoding.Bytes.Parser LogRecord
        loop x mutable'attributes
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'attributes <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                             (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                mutable'attributes)
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
                              (Data.ProtoLens.Field.field @"vec'attributes") frozen'attributes
                              x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        9 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getFixed64 "time_unix_nano"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"timeUnixNano") y x)
                                  mutable'attributes
                        89
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getFixed64
                                       "observed_time_unix_nano"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"observedTimeUnixNano") y x)
                                  mutable'attributes
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "severity_number"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"severityNumber") y x)
                                  mutable'attributes
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "severity_text"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"severityText") y x)
                                  mutable'attributes
                        42
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "body"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"body") y x)
                                  mutable'attributes
                        50
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "attributes"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'attributes y)
                                loop x v
                        56
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "dropped_attributes_count"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"droppedAttributesCount") y x)
                                  mutable'attributes
                        69
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getFixed32 "flags"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"flags") y x)
                                  mutable'attributes
                        74
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "trace_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"traceId") y x)
                                  mutable'attributes
                        82
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "span_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"spanId") y x)
                                  mutable'attributes
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'attributes
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'attributes <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'attributes)
          "LogRecord"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view (Data.ProtoLens.Field.field @"timeUnixNano") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 9)
                      (Data.ProtoLens.Encoding.Bytes.putFixed64 _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"observedTimeUnixNano") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 89)
                         (Data.ProtoLens.Encoding.Bytes.putFixed64 _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view
                            (Data.ProtoLens.Field.field @"severityNumber") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                            ((Prelude..)
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                               Prelude.fromEnum _v))
                   ((Data.Monoid.<>)
                      (let
                         _v
                           = Lens.Family2.view (Data.ProtoLens.Field.field @"severityText") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                               ((Prelude..)
                                  (\ bs
                                     -> (Data.Monoid.<>)
                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                             (Prelude.fromIntegral (Data.ByteString.length bs)))
                                          (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                  Data.Text.Encoding.encodeUtf8 _v))
                      ((Data.Monoid.<>)
                         (case
                              Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'body") _x
                          of
                            Prelude.Nothing -> Data.Monoid.mempty
                            (Prelude.Just _v)
                              -> (Data.Monoid.<>)
                                   (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                                   ((Prelude..)
                                      (\ bs
                                         -> (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (Prelude.fromIntegral (Data.ByteString.length bs)))
                                              (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                      Data.ProtoLens.encodeMessage _v))
                         ((Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                               (\ _v
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                                       ((Prelude..)
                                          (\ bs
                                             -> (Data.Monoid.<>)
                                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                     (Prelude.fromIntegral
                                                        (Data.ByteString.length bs)))
                                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                          Data.ProtoLens.encodeMessage _v))
                               (Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"vec'attributes") _x))
                            ((Data.Monoid.<>)
                               (let
                                  _v
                                    = Lens.Family2.view
                                        (Data.ProtoLens.Field.field @"droppedAttributesCount") _x
                                in
                                  if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                      Data.Monoid.mempty
                                  else
                                      (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt 56)
                                        ((Prelude..)
                                           Data.ProtoLens.Encoding.Bytes.putVarInt
                                           Prelude.fromIntegral _v))
                               ((Data.Monoid.<>)
                                  (let
                                     _v = Lens.Family2.view (Data.ProtoLens.Field.field @"flags") _x
                                   in
                                     if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                         Data.Monoid.mempty
                                     else
                                         (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt 69)
                                           (Data.ProtoLens.Encoding.Bytes.putFixed32 _v))
                                  ((Data.Monoid.<>)
                                     (let
                                        _v
                                          = Lens.Family2.view
                                              (Data.ProtoLens.Field.field @"traceId") _x
                                      in
                                        if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                            Data.Monoid.mempty
                                        else
                                            (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt 74)
                                              ((\ bs
                                                  -> (Data.Monoid.<>)
                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                          (Prelude.fromIntegral
                                                             (Data.ByteString.length bs)))
                                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                                 _v))
                                     ((Data.Monoid.<>)
                                        (let
                                           _v
                                             = Lens.Family2.view
                                                 (Data.ProtoLens.Field.field @"spanId") _x
                                         in
                                           if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                               Data.Monoid.mempty
                                           else
                                               (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 82)
                                                 ((\ bs
                                                     -> (Data.Monoid.<>)
                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                             (Prelude.fromIntegral
                                                                (Data.ByteString.length bs)))
                                                          (Data.ProtoLens.Encoding.Bytes.putBytes
                                                             bs))
                                                    _v))
                                        (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                           (Lens.Family2.view
                                              Data.ProtoLens.unknownFields _x)))))))))))
instance Control.DeepSeq.NFData LogRecord where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LogRecord'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_LogRecord'timeUnixNano x__)
                (Control.DeepSeq.deepseq
                   (_LogRecord'observedTimeUnixNano x__)
                   (Control.DeepSeq.deepseq
                      (_LogRecord'severityNumber x__)
                      (Control.DeepSeq.deepseq
                         (_LogRecord'severityText x__)
                         (Control.DeepSeq.deepseq
                            (_LogRecord'body x__)
                            (Control.DeepSeq.deepseq
                               (_LogRecord'attributes x__)
                               (Control.DeepSeq.deepseq
                                  (_LogRecord'droppedAttributesCount x__)
                                  (Control.DeepSeq.deepseq
                                     (_LogRecord'flags x__)
                                     (Control.DeepSeq.deepseq
                                        (_LogRecord'traceId x__)
                                        (Control.DeepSeq.deepseq
                                           (_LogRecord'spanId x__) ()))))))))))
newtype LogRecordFlags'UnrecognizedValue
  = LogRecordFlags'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data LogRecordFlags
  = LOG_RECORD_FLAGS_DO_NOT_USE |
    LOG_RECORD_FLAGS_TRACE_FLAGS_MASK |
    LogRecordFlags'Unrecognized !LogRecordFlags'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum LogRecordFlags where
  maybeToEnum 0 = Prelude.Just LOG_RECORD_FLAGS_DO_NOT_USE
  maybeToEnum 255 = Prelude.Just LOG_RECORD_FLAGS_TRACE_FLAGS_MASK
  maybeToEnum k
    = Prelude.Just
        (LogRecordFlags'Unrecognized
           (LogRecordFlags'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum LOG_RECORD_FLAGS_DO_NOT_USE
    = "LOG_RECORD_FLAGS_DO_NOT_USE"
  showEnum LOG_RECORD_FLAGS_TRACE_FLAGS_MASK
    = "LOG_RECORD_FLAGS_TRACE_FLAGS_MASK"
  showEnum
    (LogRecordFlags'Unrecognized (LogRecordFlags'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "LOG_RECORD_FLAGS_DO_NOT_USE"
    = Prelude.Just LOG_RECORD_FLAGS_DO_NOT_USE
    | (Prelude.==) k "LOG_RECORD_FLAGS_TRACE_FLAGS_MASK"
    = Prelude.Just LOG_RECORD_FLAGS_TRACE_FLAGS_MASK
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded LogRecordFlags where
  minBound = LOG_RECORD_FLAGS_DO_NOT_USE
  maxBound = LOG_RECORD_FLAGS_TRACE_FLAGS_MASK
instance Prelude.Enum LogRecordFlags where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum LogRecordFlags: "
              (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum LOG_RECORD_FLAGS_DO_NOT_USE = 0
  fromEnum LOG_RECORD_FLAGS_TRACE_FLAGS_MASK = 255
  fromEnum
    (LogRecordFlags'Unrecognized (LogRecordFlags'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ LOG_RECORD_FLAGS_TRACE_FLAGS_MASK
    = Prelude.error
        "LogRecordFlags.succ: bad argument LOG_RECORD_FLAGS_TRACE_FLAGS_MASK. This value would be out of bounds."
  succ LOG_RECORD_FLAGS_DO_NOT_USE
    = LOG_RECORD_FLAGS_TRACE_FLAGS_MASK
  succ (LogRecordFlags'Unrecognized _)
    = Prelude.error
        "LogRecordFlags.succ: bad argument: unrecognized value"
  pred LOG_RECORD_FLAGS_DO_NOT_USE
    = Prelude.error
        "LogRecordFlags.pred: bad argument LOG_RECORD_FLAGS_DO_NOT_USE. This value would be out of bounds."
  pred LOG_RECORD_FLAGS_TRACE_FLAGS_MASK
    = LOG_RECORD_FLAGS_DO_NOT_USE
  pred (LogRecordFlags'Unrecognized _)
    = Prelude.error
        "LogRecordFlags.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault LogRecordFlags where
  fieldDefault = LOG_RECORD_FLAGS_DO_NOT_USE
instance Control.DeepSeq.NFData LogRecordFlags where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.resourceLogs' @:: Lens' LogsData [ResourceLogs]@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.vec'resourceLogs' @:: Lens' LogsData (Data.Vector.Vector ResourceLogs)@ -}
data LogsData
  = LogsData'_constructor {_LogsData'resourceLogs :: !(Data.Vector.Vector ResourceLogs),
                           _LogsData'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show LogsData where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField LogsData "resourceLogs" [ResourceLogs] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LogsData'resourceLogs
           (\ x__ y__ -> x__ {_LogsData'resourceLogs = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField LogsData "vec'resourceLogs" (Data.Vector.Vector ResourceLogs) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _LogsData'resourceLogs
           (\ x__ y__ -> x__ {_LogsData'resourceLogs = y__}))
        Prelude.id
instance Data.ProtoLens.Message LogsData where
  messageName _
    = Data.Text.pack "opentelemetry.proto.logs.v1.LogsData"
  packedMessageDescriptor _
    = "\n\
      \\bLogsData\DC2N\n\
      \\rresource_logs\CAN\SOH \ETX(\v2).opentelemetry.proto.logs.v1.ResourceLogsR\fresourceLogs"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        resourceLogs__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "resource_logs"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ResourceLogs)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"resourceLogs")) ::
              Data.ProtoLens.FieldDescriptor LogsData
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, resourceLogs__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _LogsData'_unknownFields
        (\ x__ y__ -> x__ {_LogsData'_unknownFields = y__})
  defMessage
    = LogsData'_constructor
        {_LogsData'resourceLogs = Data.Vector.Generic.empty,
         _LogsData'_unknownFields = []}
  parseMessage
    = let
        loop ::
          LogsData
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ResourceLogs
             -> Data.ProtoLens.Encoding.Bytes.Parser LogsData
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
          "LogsData"
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
instance Control.DeepSeq.NFData LogsData where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_LogsData'_unknownFields x__)
             (Control.DeepSeq.deepseq (_LogsData'resourceLogs x__) ())
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.resource' @:: Lens' ResourceLogs Proto.Opentelemetry.Proto.Resource.V1.Resource.Resource@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.maybe'resource' @:: Lens' ResourceLogs (Prelude.Maybe Proto.Opentelemetry.Proto.Resource.V1.Resource.Resource)@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.scopeLogs' @:: Lens' ResourceLogs [ScopeLogs]@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.vec'scopeLogs' @:: Lens' ResourceLogs (Data.Vector.Vector ScopeLogs)@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.schemaUrl' @:: Lens' ResourceLogs Data.Text.Text@ -}
data ResourceLogs
  = ResourceLogs'_constructor {_ResourceLogs'resource :: !(Prelude.Maybe Proto.Opentelemetry.Proto.Resource.V1.Resource.Resource),
                               _ResourceLogs'scopeLogs :: !(Data.Vector.Vector ScopeLogs),
                               _ResourceLogs'schemaUrl :: !Data.Text.Text,
                               _ResourceLogs'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ResourceLogs where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ResourceLogs "resource" Proto.Opentelemetry.Proto.Resource.V1.Resource.Resource where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResourceLogs'resource
           (\ x__ y__ -> x__ {_ResourceLogs'resource = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ResourceLogs "maybe'resource" (Prelude.Maybe Proto.Opentelemetry.Proto.Resource.V1.Resource.Resource) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResourceLogs'resource
           (\ x__ y__ -> x__ {_ResourceLogs'resource = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ResourceLogs "scopeLogs" [ScopeLogs] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResourceLogs'scopeLogs
           (\ x__ y__ -> x__ {_ResourceLogs'scopeLogs = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ResourceLogs "vec'scopeLogs" (Data.Vector.Vector ScopeLogs) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResourceLogs'scopeLogs
           (\ x__ y__ -> x__ {_ResourceLogs'scopeLogs = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ResourceLogs "schemaUrl" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResourceLogs'schemaUrl
           (\ x__ y__ -> x__ {_ResourceLogs'schemaUrl = y__}))
        Prelude.id
instance Data.ProtoLens.Message ResourceLogs where
  messageName _
    = Data.Text.pack "opentelemetry.proto.logs.v1.ResourceLogs"
  packedMessageDescriptor _
    = "\n\
      \\fResourceLogs\DC2E\n\
      \\bresource\CAN\SOH \SOH(\v2).opentelemetry.proto.resource.v1.ResourceR\bresource\DC2E\n\
      \\n\
      \scope_logs\CAN\STX \ETX(\v2&.opentelemetry.proto.logs.v1.ScopeLogsR\tscopeLogs\DC2\GS\n\
      \\n\
      \schema_url\CAN\ETX \SOH(\tR\tschemaUrlJ\ACK\b\232\a\DLE\233\a"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        resource__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "resource"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Opentelemetry.Proto.Resource.V1.Resource.Resource)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'resource")) ::
              Data.ProtoLens.FieldDescriptor ResourceLogs
        scopeLogs__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "scope_logs"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ScopeLogs)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"scopeLogs")) ::
              Data.ProtoLens.FieldDescriptor ResourceLogs
        schemaUrl__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "schema_url"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"schemaUrl")) ::
              Data.ProtoLens.FieldDescriptor ResourceLogs
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, resource__field_descriptor),
           (Data.ProtoLens.Tag 2, scopeLogs__field_descriptor),
           (Data.ProtoLens.Tag 3, schemaUrl__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ResourceLogs'_unknownFields
        (\ x__ y__ -> x__ {_ResourceLogs'_unknownFields = y__})
  defMessage
    = ResourceLogs'_constructor
        {_ResourceLogs'resource = Prelude.Nothing,
         _ResourceLogs'scopeLogs = Data.Vector.Generic.empty,
         _ResourceLogs'schemaUrl = Data.ProtoLens.fieldDefault,
         _ResourceLogs'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ResourceLogs
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ScopeLogs
             -> Data.ProtoLens.Encoding.Bytes.Parser ResourceLogs
        loop x mutable'scopeLogs
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'scopeLogs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                               mutable'scopeLogs)
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
                              (Data.ProtoLens.Field.field @"vec'scopeLogs") frozen'scopeLogs x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "resource"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"resource") y x)
                                  mutable'scopeLogs
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "scope_logs"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'scopeLogs y)
                                loop x v
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "schema_url"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"schemaUrl") y x)
                                  mutable'scopeLogs
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'scopeLogs
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'scopeLogs <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                     Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'scopeLogs)
          "ResourceLogs"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'resource") _x
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
             ((Data.Monoid.<>)
                (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                   (\ _v
                      -> (Data.Monoid.<>)
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                           ((Prelude..)
                              (\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Data.ProtoLens.encodeMessage _v))
                   (Lens.Family2.view
                      (Data.ProtoLens.Field.field @"vec'scopeLogs") _x))
                ((Data.Monoid.<>)
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"schemaUrl") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8 _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData ResourceLogs where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ResourceLogs'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ResourceLogs'resource x__)
                (Control.DeepSeq.deepseq
                   (_ResourceLogs'scopeLogs x__)
                   (Control.DeepSeq.deepseq (_ResourceLogs'schemaUrl x__) ())))
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.scope' @:: Lens' ScopeLogs Proto.Opentelemetry.Proto.Common.V1.Common.InstrumentationScope@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.maybe'scope' @:: Lens' ScopeLogs (Prelude.Maybe Proto.Opentelemetry.Proto.Common.V1.Common.InstrumentationScope)@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.logRecords' @:: Lens' ScopeLogs [LogRecord]@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.vec'logRecords' @:: Lens' ScopeLogs (Data.Vector.Vector LogRecord)@
         * 'Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields.schemaUrl' @:: Lens' ScopeLogs Data.Text.Text@ -}
data ScopeLogs
  = ScopeLogs'_constructor {_ScopeLogs'scope :: !(Prelude.Maybe Proto.Opentelemetry.Proto.Common.V1.Common.InstrumentationScope),
                            _ScopeLogs'logRecords :: !(Data.Vector.Vector LogRecord),
                            _ScopeLogs'schemaUrl :: !Data.Text.Text,
                            _ScopeLogs'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ScopeLogs where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ScopeLogs "scope" Proto.Opentelemetry.Proto.Common.V1.Common.InstrumentationScope where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ScopeLogs'scope (\ x__ y__ -> x__ {_ScopeLogs'scope = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ScopeLogs "maybe'scope" (Prelude.Maybe Proto.Opentelemetry.Proto.Common.V1.Common.InstrumentationScope) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ScopeLogs'scope (\ x__ y__ -> x__ {_ScopeLogs'scope = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ScopeLogs "logRecords" [LogRecord] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ScopeLogs'logRecords
           (\ x__ y__ -> x__ {_ScopeLogs'logRecords = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ScopeLogs "vec'logRecords" (Data.Vector.Vector LogRecord) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ScopeLogs'logRecords
           (\ x__ y__ -> x__ {_ScopeLogs'logRecords = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ScopeLogs "schemaUrl" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ScopeLogs'schemaUrl
           (\ x__ y__ -> x__ {_ScopeLogs'schemaUrl = y__}))
        Prelude.id
instance Data.ProtoLens.Message ScopeLogs where
  messageName _
    = Data.Text.pack "opentelemetry.proto.logs.v1.ScopeLogs"
  packedMessageDescriptor _
    = "\n\
      \\tScopeLogs\DC2I\n\
      \\ENQscope\CAN\SOH \SOH(\v23.opentelemetry.proto.common.v1.InstrumentationScopeR\ENQscope\DC2G\n\
      \\vlog_records\CAN\STX \ETX(\v2&.opentelemetry.proto.logs.v1.LogRecordR\n\
      \logRecords\DC2\GS\n\
      \\n\
      \schema_url\CAN\ETX \SOH(\tR\tschemaUrl"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        scope__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "scope"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Opentelemetry.Proto.Common.V1.Common.InstrumentationScope)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'scope")) ::
              Data.ProtoLens.FieldDescriptor ScopeLogs
        logRecords__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "log_records"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor LogRecord)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"logRecords")) ::
              Data.ProtoLens.FieldDescriptor ScopeLogs
        schemaUrl__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "schema_url"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"schemaUrl")) ::
              Data.ProtoLens.FieldDescriptor ScopeLogs
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, scope__field_descriptor),
           (Data.ProtoLens.Tag 2, logRecords__field_descriptor),
           (Data.ProtoLens.Tag 3, schemaUrl__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ScopeLogs'_unknownFields
        (\ x__ y__ -> x__ {_ScopeLogs'_unknownFields = y__})
  defMessage
    = ScopeLogs'_constructor
        {_ScopeLogs'scope = Prelude.Nothing,
         _ScopeLogs'logRecords = Data.Vector.Generic.empty,
         _ScopeLogs'schemaUrl = Data.ProtoLens.fieldDefault,
         _ScopeLogs'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ScopeLogs
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld LogRecord
             -> Data.ProtoLens.Encoding.Bytes.Parser ScopeLogs
        loop x mutable'logRecords
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'logRecords <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                             (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                mutable'logRecords)
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
                              (Data.ProtoLens.Field.field @"vec'logRecords") frozen'logRecords
                              x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "scope"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"scope") y x)
                                  mutable'logRecords
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "log_records"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'logRecords y)
                                loop x v
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "schema_url"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"schemaUrl") y x)
                                  mutable'logRecords
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'logRecords
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'logRecords <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'logRecords)
          "ScopeLogs"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (case
                  Lens.Family2.view (Data.ProtoLens.Field.field @"maybe'scope") _x
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
             ((Data.Monoid.<>)
                (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                   (\ _v
                      -> (Data.Monoid.<>)
                           (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                           ((Prelude..)
                              (\ bs
                                 -> (Data.Monoid.<>)
                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                         (Prelude.fromIntegral (Data.ByteString.length bs)))
                                      (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                              Data.ProtoLens.encodeMessage _v))
                   (Lens.Family2.view
                      (Data.ProtoLens.Field.field @"vec'logRecords") _x))
                ((Data.Monoid.<>)
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"schemaUrl") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                            ((Prelude..)
                               (\ bs
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                          (Prelude.fromIntegral (Data.ByteString.length bs)))
                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                               Data.Text.Encoding.encodeUtf8 _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData ScopeLogs where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ScopeLogs'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ScopeLogs'scope x__)
                (Control.DeepSeq.deepseq
                   (_ScopeLogs'logRecords x__)
                   (Control.DeepSeq.deepseq (_ScopeLogs'schemaUrl x__) ())))
newtype SeverityNumber'UnrecognizedValue
  = SeverityNumber'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data SeverityNumber
  = SEVERITY_NUMBER_UNSPECIFIED |
    SEVERITY_NUMBER_TRACE |
    SEVERITY_NUMBER_TRACE2 |
    SEVERITY_NUMBER_TRACE3 |
    SEVERITY_NUMBER_TRACE4 |
    SEVERITY_NUMBER_DEBUG |
    SEVERITY_NUMBER_DEBUG2 |
    SEVERITY_NUMBER_DEBUG3 |
    SEVERITY_NUMBER_DEBUG4 |
    SEVERITY_NUMBER_INFO |
    SEVERITY_NUMBER_INFO2 |
    SEVERITY_NUMBER_INFO3 |
    SEVERITY_NUMBER_INFO4 |
    SEVERITY_NUMBER_WARN |
    SEVERITY_NUMBER_WARN2 |
    SEVERITY_NUMBER_WARN3 |
    SEVERITY_NUMBER_WARN4 |
    SEVERITY_NUMBER_ERROR |
    SEVERITY_NUMBER_ERROR2 |
    SEVERITY_NUMBER_ERROR3 |
    SEVERITY_NUMBER_ERROR4 |
    SEVERITY_NUMBER_FATAL |
    SEVERITY_NUMBER_FATAL2 |
    SEVERITY_NUMBER_FATAL3 |
    SEVERITY_NUMBER_FATAL4 |
    SeverityNumber'Unrecognized !SeverityNumber'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum SeverityNumber where
  maybeToEnum 0 = Prelude.Just SEVERITY_NUMBER_UNSPECIFIED
  maybeToEnum 1 = Prelude.Just SEVERITY_NUMBER_TRACE
  maybeToEnum 2 = Prelude.Just SEVERITY_NUMBER_TRACE2
  maybeToEnum 3 = Prelude.Just SEVERITY_NUMBER_TRACE3
  maybeToEnum 4 = Prelude.Just SEVERITY_NUMBER_TRACE4
  maybeToEnum 5 = Prelude.Just SEVERITY_NUMBER_DEBUG
  maybeToEnum 6 = Prelude.Just SEVERITY_NUMBER_DEBUG2
  maybeToEnum 7 = Prelude.Just SEVERITY_NUMBER_DEBUG3
  maybeToEnum 8 = Prelude.Just SEVERITY_NUMBER_DEBUG4
  maybeToEnum 9 = Prelude.Just SEVERITY_NUMBER_INFO
  maybeToEnum 10 = Prelude.Just SEVERITY_NUMBER_INFO2
  maybeToEnum 11 = Prelude.Just SEVERITY_NUMBER_INFO3
  maybeToEnum 12 = Prelude.Just SEVERITY_NUMBER_INFO4
  maybeToEnum 13 = Prelude.Just SEVERITY_NUMBER_WARN
  maybeToEnum 14 = Prelude.Just SEVERITY_NUMBER_WARN2
  maybeToEnum 15 = Prelude.Just SEVERITY_NUMBER_WARN3
  maybeToEnum 16 = Prelude.Just SEVERITY_NUMBER_WARN4
  maybeToEnum 17 = Prelude.Just SEVERITY_NUMBER_ERROR
  maybeToEnum 18 = Prelude.Just SEVERITY_NUMBER_ERROR2
  maybeToEnum 19 = Prelude.Just SEVERITY_NUMBER_ERROR3
  maybeToEnum 20 = Prelude.Just SEVERITY_NUMBER_ERROR4
  maybeToEnum 21 = Prelude.Just SEVERITY_NUMBER_FATAL
  maybeToEnum 22 = Prelude.Just SEVERITY_NUMBER_FATAL2
  maybeToEnum 23 = Prelude.Just SEVERITY_NUMBER_FATAL3
  maybeToEnum 24 = Prelude.Just SEVERITY_NUMBER_FATAL4
  maybeToEnum k
    = Prelude.Just
        (SeverityNumber'Unrecognized
           (SeverityNumber'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum SEVERITY_NUMBER_UNSPECIFIED
    = "SEVERITY_NUMBER_UNSPECIFIED"
  showEnum SEVERITY_NUMBER_TRACE = "SEVERITY_NUMBER_TRACE"
  showEnum SEVERITY_NUMBER_TRACE2 = "SEVERITY_NUMBER_TRACE2"
  showEnum SEVERITY_NUMBER_TRACE3 = "SEVERITY_NUMBER_TRACE3"
  showEnum SEVERITY_NUMBER_TRACE4 = "SEVERITY_NUMBER_TRACE4"
  showEnum SEVERITY_NUMBER_DEBUG = "SEVERITY_NUMBER_DEBUG"
  showEnum SEVERITY_NUMBER_DEBUG2 = "SEVERITY_NUMBER_DEBUG2"
  showEnum SEVERITY_NUMBER_DEBUG3 = "SEVERITY_NUMBER_DEBUG3"
  showEnum SEVERITY_NUMBER_DEBUG4 = "SEVERITY_NUMBER_DEBUG4"
  showEnum SEVERITY_NUMBER_INFO = "SEVERITY_NUMBER_INFO"
  showEnum SEVERITY_NUMBER_INFO2 = "SEVERITY_NUMBER_INFO2"
  showEnum SEVERITY_NUMBER_INFO3 = "SEVERITY_NUMBER_INFO3"
  showEnum SEVERITY_NUMBER_INFO4 = "SEVERITY_NUMBER_INFO4"
  showEnum SEVERITY_NUMBER_WARN = "SEVERITY_NUMBER_WARN"
  showEnum SEVERITY_NUMBER_WARN2 = "SEVERITY_NUMBER_WARN2"
  showEnum SEVERITY_NUMBER_WARN3 = "SEVERITY_NUMBER_WARN3"
  showEnum SEVERITY_NUMBER_WARN4 = "SEVERITY_NUMBER_WARN4"
  showEnum SEVERITY_NUMBER_ERROR = "SEVERITY_NUMBER_ERROR"
  showEnum SEVERITY_NUMBER_ERROR2 = "SEVERITY_NUMBER_ERROR2"
  showEnum SEVERITY_NUMBER_ERROR3 = "SEVERITY_NUMBER_ERROR3"
  showEnum SEVERITY_NUMBER_ERROR4 = "SEVERITY_NUMBER_ERROR4"
  showEnum SEVERITY_NUMBER_FATAL = "SEVERITY_NUMBER_FATAL"
  showEnum SEVERITY_NUMBER_FATAL2 = "SEVERITY_NUMBER_FATAL2"
  showEnum SEVERITY_NUMBER_FATAL3 = "SEVERITY_NUMBER_FATAL3"
  showEnum SEVERITY_NUMBER_FATAL4 = "SEVERITY_NUMBER_FATAL4"
  showEnum
    (SeverityNumber'Unrecognized (SeverityNumber'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "SEVERITY_NUMBER_UNSPECIFIED"
    = Prelude.Just SEVERITY_NUMBER_UNSPECIFIED
    | (Prelude.==) k "SEVERITY_NUMBER_TRACE"
    = Prelude.Just SEVERITY_NUMBER_TRACE
    | (Prelude.==) k "SEVERITY_NUMBER_TRACE2"
    = Prelude.Just SEVERITY_NUMBER_TRACE2
    | (Prelude.==) k "SEVERITY_NUMBER_TRACE3"
    = Prelude.Just SEVERITY_NUMBER_TRACE3
    | (Prelude.==) k "SEVERITY_NUMBER_TRACE4"
    = Prelude.Just SEVERITY_NUMBER_TRACE4
    | (Prelude.==) k "SEVERITY_NUMBER_DEBUG"
    = Prelude.Just SEVERITY_NUMBER_DEBUG
    | (Prelude.==) k "SEVERITY_NUMBER_DEBUG2"
    = Prelude.Just SEVERITY_NUMBER_DEBUG2
    | (Prelude.==) k "SEVERITY_NUMBER_DEBUG3"
    = Prelude.Just SEVERITY_NUMBER_DEBUG3
    | (Prelude.==) k "SEVERITY_NUMBER_DEBUG4"
    = Prelude.Just SEVERITY_NUMBER_DEBUG4
    | (Prelude.==) k "SEVERITY_NUMBER_INFO"
    = Prelude.Just SEVERITY_NUMBER_INFO
    | (Prelude.==) k "SEVERITY_NUMBER_INFO2"
    = Prelude.Just SEVERITY_NUMBER_INFO2
    | (Prelude.==) k "SEVERITY_NUMBER_INFO3"
    = Prelude.Just SEVERITY_NUMBER_INFO3
    | (Prelude.==) k "SEVERITY_NUMBER_INFO4"
    = Prelude.Just SEVERITY_NUMBER_INFO4
    | (Prelude.==) k "SEVERITY_NUMBER_WARN"
    = Prelude.Just SEVERITY_NUMBER_WARN
    | (Prelude.==) k "SEVERITY_NUMBER_WARN2"
    = Prelude.Just SEVERITY_NUMBER_WARN2
    | (Prelude.==) k "SEVERITY_NUMBER_WARN3"
    = Prelude.Just SEVERITY_NUMBER_WARN3
    | (Prelude.==) k "SEVERITY_NUMBER_WARN4"
    = Prelude.Just SEVERITY_NUMBER_WARN4
    | (Prelude.==) k "SEVERITY_NUMBER_ERROR"
    = Prelude.Just SEVERITY_NUMBER_ERROR
    | (Prelude.==) k "SEVERITY_NUMBER_ERROR2"
    = Prelude.Just SEVERITY_NUMBER_ERROR2
    | (Prelude.==) k "SEVERITY_NUMBER_ERROR3"
    = Prelude.Just SEVERITY_NUMBER_ERROR3
    | (Prelude.==) k "SEVERITY_NUMBER_ERROR4"
    = Prelude.Just SEVERITY_NUMBER_ERROR4
    | (Prelude.==) k "SEVERITY_NUMBER_FATAL"
    = Prelude.Just SEVERITY_NUMBER_FATAL
    | (Prelude.==) k "SEVERITY_NUMBER_FATAL2"
    = Prelude.Just SEVERITY_NUMBER_FATAL2
    | (Prelude.==) k "SEVERITY_NUMBER_FATAL3"
    = Prelude.Just SEVERITY_NUMBER_FATAL3
    | (Prelude.==) k "SEVERITY_NUMBER_FATAL4"
    = Prelude.Just SEVERITY_NUMBER_FATAL4
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded SeverityNumber where
  minBound = SEVERITY_NUMBER_UNSPECIFIED
  maxBound = SEVERITY_NUMBER_FATAL4
instance Prelude.Enum SeverityNumber where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum SeverityNumber: "
              (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum SEVERITY_NUMBER_UNSPECIFIED = 0
  fromEnum SEVERITY_NUMBER_TRACE = 1
  fromEnum SEVERITY_NUMBER_TRACE2 = 2
  fromEnum SEVERITY_NUMBER_TRACE3 = 3
  fromEnum SEVERITY_NUMBER_TRACE4 = 4
  fromEnum SEVERITY_NUMBER_DEBUG = 5
  fromEnum SEVERITY_NUMBER_DEBUG2 = 6
  fromEnum SEVERITY_NUMBER_DEBUG3 = 7
  fromEnum SEVERITY_NUMBER_DEBUG4 = 8
  fromEnum SEVERITY_NUMBER_INFO = 9
  fromEnum SEVERITY_NUMBER_INFO2 = 10
  fromEnum SEVERITY_NUMBER_INFO3 = 11
  fromEnum SEVERITY_NUMBER_INFO4 = 12
  fromEnum SEVERITY_NUMBER_WARN = 13
  fromEnum SEVERITY_NUMBER_WARN2 = 14
  fromEnum SEVERITY_NUMBER_WARN3 = 15
  fromEnum SEVERITY_NUMBER_WARN4 = 16
  fromEnum SEVERITY_NUMBER_ERROR = 17
  fromEnum SEVERITY_NUMBER_ERROR2 = 18
  fromEnum SEVERITY_NUMBER_ERROR3 = 19
  fromEnum SEVERITY_NUMBER_ERROR4 = 20
  fromEnum SEVERITY_NUMBER_FATAL = 21
  fromEnum SEVERITY_NUMBER_FATAL2 = 22
  fromEnum SEVERITY_NUMBER_FATAL3 = 23
  fromEnum SEVERITY_NUMBER_FATAL4 = 24
  fromEnum
    (SeverityNumber'Unrecognized (SeverityNumber'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ SEVERITY_NUMBER_FATAL4
    = Prelude.error
        "SeverityNumber.succ: bad argument SEVERITY_NUMBER_FATAL4. This value would be out of bounds."
  succ SEVERITY_NUMBER_UNSPECIFIED = SEVERITY_NUMBER_TRACE
  succ SEVERITY_NUMBER_TRACE = SEVERITY_NUMBER_TRACE2
  succ SEVERITY_NUMBER_TRACE2 = SEVERITY_NUMBER_TRACE3
  succ SEVERITY_NUMBER_TRACE3 = SEVERITY_NUMBER_TRACE4
  succ SEVERITY_NUMBER_TRACE4 = SEVERITY_NUMBER_DEBUG
  succ SEVERITY_NUMBER_DEBUG = SEVERITY_NUMBER_DEBUG2
  succ SEVERITY_NUMBER_DEBUG2 = SEVERITY_NUMBER_DEBUG3
  succ SEVERITY_NUMBER_DEBUG3 = SEVERITY_NUMBER_DEBUG4
  succ SEVERITY_NUMBER_DEBUG4 = SEVERITY_NUMBER_INFO
  succ SEVERITY_NUMBER_INFO = SEVERITY_NUMBER_INFO2
  succ SEVERITY_NUMBER_INFO2 = SEVERITY_NUMBER_INFO3
  succ SEVERITY_NUMBER_INFO3 = SEVERITY_NUMBER_INFO4
  succ SEVERITY_NUMBER_INFO4 = SEVERITY_NUMBER_WARN
  succ SEVERITY_NUMBER_WARN = SEVERITY_NUMBER_WARN2
  succ SEVERITY_NUMBER_WARN2 = SEVERITY_NUMBER_WARN3
  succ SEVERITY_NUMBER_WARN3 = SEVERITY_NUMBER_WARN4
  succ SEVERITY_NUMBER_WARN4 = SEVERITY_NUMBER_ERROR
  succ SEVERITY_NUMBER_ERROR = SEVERITY_NUMBER_ERROR2
  succ SEVERITY_NUMBER_ERROR2 = SEVERITY_NUMBER_ERROR3
  succ SEVERITY_NUMBER_ERROR3 = SEVERITY_NUMBER_ERROR4
  succ SEVERITY_NUMBER_ERROR4 = SEVERITY_NUMBER_FATAL
  succ SEVERITY_NUMBER_FATAL = SEVERITY_NUMBER_FATAL2
  succ SEVERITY_NUMBER_FATAL2 = SEVERITY_NUMBER_FATAL3
  succ SEVERITY_NUMBER_FATAL3 = SEVERITY_NUMBER_FATAL4
  succ (SeverityNumber'Unrecognized _)
    = Prelude.error
        "SeverityNumber.succ: bad argument: unrecognized value"
  pred SEVERITY_NUMBER_UNSPECIFIED
    = Prelude.error
        "SeverityNumber.pred: bad argument SEVERITY_NUMBER_UNSPECIFIED. This value would be out of bounds."
  pred SEVERITY_NUMBER_TRACE = SEVERITY_NUMBER_UNSPECIFIED
  pred SEVERITY_NUMBER_TRACE2 = SEVERITY_NUMBER_TRACE
  pred SEVERITY_NUMBER_TRACE3 = SEVERITY_NUMBER_TRACE2
  pred SEVERITY_NUMBER_TRACE4 = SEVERITY_NUMBER_TRACE3
  pred SEVERITY_NUMBER_DEBUG = SEVERITY_NUMBER_TRACE4
  pred SEVERITY_NUMBER_DEBUG2 = SEVERITY_NUMBER_DEBUG
  pred SEVERITY_NUMBER_DEBUG3 = SEVERITY_NUMBER_DEBUG2
  pred SEVERITY_NUMBER_DEBUG4 = SEVERITY_NUMBER_DEBUG3
  pred SEVERITY_NUMBER_INFO = SEVERITY_NUMBER_DEBUG4
  pred SEVERITY_NUMBER_INFO2 = SEVERITY_NUMBER_INFO
  pred SEVERITY_NUMBER_INFO3 = SEVERITY_NUMBER_INFO2
  pred SEVERITY_NUMBER_INFO4 = SEVERITY_NUMBER_INFO3
  pred SEVERITY_NUMBER_WARN = SEVERITY_NUMBER_INFO4
  pred SEVERITY_NUMBER_WARN2 = SEVERITY_NUMBER_WARN
  pred SEVERITY_NUMBER_WARN3 = SEVERITY_NUMBER_WARN2
  pred SEVERITY_NUMBER_WARN4 = SEVERITY_NUMBER_WARN3
  pred SEVERITY_NUMBER_ERROR = SEVERITY_NUMBER_WARN4
  pred SEVERITY_NUMBER_ERROR2 = SEVERITY_NUMBER_ERROR
  pred SEVERITY_NUMBER_ERROR3 = SEVERITY_NUMBER_ERROR2
  pred SEVERITY_NUMBER_ERROR4 = SEVERITY_NUMBER_ERROR3
  pred SEVERITY_NUMBER_FATAL = SEVERITY_NUMBER_ERROR4
  pred SEVERITY_NUMBER_FATAL2 = SEVERITY_NUMBER_FATAL
  pred SEVERITY_NUMBER_FATAL3 = SEVERITY_NUMBER_FATAL2
  pred SEVERITY_NUMBER_FATAL4 = SEVERITY_NUMBER_FATAL3
  pred (SeverityNumber'Unrecognized _)
    = Prelude.error
        "SeverityNumber.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault SeverityNumber where
  fieldDefault = SEVERITY_NUMBER_UNSPECIFIED
instance Control.DeepSeq.NFData SeverityNumber where
  rnf x__ = Prelude.seq x__ ()
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \&opentelemetry/proto/logs/v1/logs.proto\DC2\ESCopentelemetry.proto.logs.v1\SUB*opentelemetry/proto/common/v1/common.proto\SUB.opentelemetry/proto/resource/v1/resource.proto\"Z\n\
    \\bLogsData\DC2N\n\
    \\rresource_logs\CAN\SOH \ETX(\v2).opentelemetry.proto.logs.v1.ResourceLogsR\fresourceLogs\"\195\SOH\n\
    \\fResourceLogs\DC2E\n\
    \\bresource\CAN\SOH \SOH(\v2).opentelemetry.proto.resource.v1.ResourceR\bresource\DC2E\n\
    \\n\
    \scope_logs\CAN\STX \ETX(\v2&.opentelemetry.proto.logs.v1.ScopeLogsR\tscopeLogs\DC2\GS\n\
    \\n\
    \schema_url\CAN\ETX \SOH(\tR\tschemaUrlJ\ACK\b\232\a\DLE\233\a\"\190\SOH\n\
    \\tScopeLogs\DC2I\n\
    \\ENQscope\CAN\SOH \SOH(\v23.opentelemetry.proto.common.v1.InstrumentationScopeR\ENQscope\DC2G\n\
    \\vlog_records\CAN\STX \ETX(\v2&.opentelemetry.proto.logs.v1.LogRecordR\n\
    \logRecords\DC2\GS\n\
    \\n\
    \schema_url\CAN\ETX \SOH(\tR\tschemaUrl\"\243\ETX\n\
    \\tLogRecord\DC2$\n\
    \\SOtime_unix_nano\CAN\SOH \SOH(\ACKR\ftimeUnixNano\DC25\n\
    \\ETBobserved_time_unix_nano\CAN\v \SOH(\ACKR\DC4observedTimeUnixNano\DC2T\n\
    \\SIseverity_number\CAN\STX \SOH(\SO2+.opentelemetry.proto.logs.v1.SeverityNumberR\SOseverityNumber\DC2#\n\
    \\rseverity_text\CAN\ETX \SOH(\tR\fseverityText\DC2;\n\
    \\EOTbody\CAN\ENQ \SOH(\v2'.opentelemetry.proto.common.v1.AnyValueR\EOTbody\DC2G\n\
    \\n\
    \attributes\CAN\ACK \ETX(\v2'.opentelemetry.proto.common.v1.KeyValueR\n\
    \attributes\DC28\n\
    \\CANdropped_attributes_count\CAN\a \SOH(\rR\SYNdroppedAttributesCount\DC2\DC4\n\
    \\ENQflags\CAN\b \SOH(\aR\ENQflags\DC2\EM\n\
    \\btrace_id\CAN\t \SOH(\fR\atraceId\DC2\ETB\n\
    \\aspan_id\CAN\n\
    \ \SOH(\fR\ACKspanIdJ\EOT\b\EOT\DLE\ENQ*\195\ENQ\n\
    \\SOSeverityNumber\DC2\US\n\
    \\ESCSEVERITY_NUMBER_UNSPECIFIED\DLE\NUL\DC2\EM\n\
    \\NAKSEVERITY_NUMBER_TRACE\DLE\SOH\DC2\SUB\n\
    \\SYNSEVERITY_NUMBER_TRACE2\DLE\STX\DC2\SUB\n\
    \\SYNSEVERITY_NUMBER_TRACE3\DLE\ETX\DC2\SUB\n\
    \\SYNSEVERITY_NUMBER_TRACE4\DLE\EOT\DC2\EM\n\
    \\NAKSEVERITY_NUMBER_DEBUG\DLE\ENQ\DC2\SUB\n\
    \\SYNSEVERITY_NUMBER_DEBUG2\DLE\ACK\DC2\SUB\n\
    \\SYNSEVERITY_NUMBER_DEBUG3\DLE\a\DC2\SUB\n\
    \\SYNSEVERITY_NUMBER_DEBUG4\DLE\b\DC2\CAN\n\
    \\DC4SEVERITY_NUMBER_INFO\DLE\t\DC2\EM\n\
    \\NAKSEVERITY_NUMBER_INFO2\DLE\n\
    \\DC2\EM\n\
    \\NAKSEVERITY_NUMBER_INFO3\DLE\v\DC2\EM\n\
    \\NAKSEVERITY_NUMBER_INFO4\DLE\f\DC2\CAN\n\
    \\DC4SEVERITY_NUMBER_WARN\DLE\r\DC2\EM\n\
    \\NAKSEVERITY_NUMBER_WARN2\DLE\SO\DC2\EM\n\
    \\NAKSEVERITY_NUMBER_WARN3\DLE\SI\DC2\EM\n\
    \\NAKSEVERITY_NUMBER_WARN4\DLE\DLE\DC2\EM\n\
    \\NAKSEVERITY_NUMBER_ERROR\DLE\DC1\DC2\SUB\n\
    \\SYNSEVERITY_NUMBER_ERROR2\DLE\DC2\DC2\SUB\n\
    \\SYNSEVERITY_NUMBER_ERROR3\DLE\DC3\DC2\SUB\n\
    \\SYNSEVERITY_NUMBER_ERROR4\DLE\DC4\DC2\EM\n\
    \\NAKSEVERITY_NUMBER_FATAL\DLE\NAK\DC2\SUB\n\
    \\SYNSEVERITY_NUMBER_FATAL2\DLE\SYN\DC2\SUB\n\
    \\SYNSEVERITY_NUMBER_FATAL3\DLE\ETB\DC2\SUB\n\
    \\SYNSEVERITY_NUMBER_FATAL4\DLE\CAN*Y\n\
    \\SOLogRecordFlags\DC2\US\n\
    \\ESCLOG_RECORD_FLAGS_DO_NOT_USE\DLE\NUL\DC2&\n\
    \!LOG_RECORD_FLAGS_TRACE_FLAGS_MASK\DLE\255\SOHBs\n\
    \\RSio.opentelemetry.proto.logs.v1B\tLogsProtoP\SOHZ&go.opentelemetry.io/proto/otlp/logs/v1\170\STX\ESCOpenTelemetry.Proto.Logs.V1J\231E\n\
    \\a\DC2\ENQ\SO\NUL\210\SOH\SOH\n\
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
    \\SOH\STX\DC2\ETX\DLE\NUL$\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\DC2\NUL4\n\
    \\t\n\
    \\STX\ETX\SOH\DC2\ETX\DC3\NUL8\n\
    \\b\n\
    \\SOH\b\DC2\ETX\NAK\NUL8\n\
    \\t\n\
    \\STX\b%\DC2\ETX\NAK\NUL8\n\
    \\b\n\
    \\SOH\b\DC2\ETX\SYN\NUL\"\n\
    \\t\n\
    \\STX\b\n\
    \\DC2\ETX\SYN\NUL\"\n\
    \\b\n\
    \\SOH\b\DC2\ETX\ETB\NUL7\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX\ETB\NUL7\n\
    \\b\n\
    \\SOH\b\DC2\ETX\CAN\NUL*\n\
    \\t\n\
    \\STX\b\b\DC2\ETX\CAN\NUL*\n\
    \\b\n\
    \\SOH\b\DC2\ETX\EM\NUL=\n\
    \\t\n\
    \\STX\b\v\DC2\ETX\EM\NUL=\n\
    \\200\ETX\n\
    \\STX\EOT\NUL\DC2\EOT%\NUL,\SOH\SUB\187\ETX LogsData represents the logs data that can be stored in a persistent storage,\n\
    \ OR can be embedded by other protocols that transfer OTLP logs data but do not\n\
    \ implement the OTLP protocol.\n\
    \\n\
    \ The main difference between this message and collector protocol is that\n\
    \ in this message there will not be any \"control\" or \"metadata\" specific to\n\
    \ OTLP protocol.\n\
    \\n\
    \ When new fields are added into this message, the OTLP request MUST be updated\n\
    \ as well.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX%\b\DLE\n\
    \\173\STX\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX+\STX*\SUB\159\STX An array of ResourceLogs.\n\
    \ For data coming from a single resource this array will typically contain\n\
    \ one element. Intermediary nodes that receive data from multiple origins\n\
    \ typically batch the data before forwarding further and in that case this\n\
    \ array will contain multiple elements.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\EOT\DC2\ETX+\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETX+\v\ETB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX+\CAN%\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX+()\n\
    \8\n\
    \\STX\EOT\SOH\DC2\EOT/\NUL?\SOH\SUB, A collection of ScopeLogs from a Resource.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX/\b\DC4\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\t\DC2\ETX0\STX\DLE\n\
    \\v\n\
    \\EOT\EOT\SOH\t\NUL\DC2\ETX0\v\SI\n\
    \\f\n\
    \\ENQ\EOT\SOH\t\NUL\SOH\DC2\ETX0\v\SI\n\
    \\f\n\
    \\ENQ\EOT\SOH\t\NUL\STX\DC2\ETX0\v\SI\n\
    \r\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX4\STX8\SUBe The resource for the logs in this message.\n\
    \ If this field is not set then resource info is unknown.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETX4\STX*\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX4+3\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX467\n\
    \B\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETX7\STX$\SUB5 A list of ScopeLogs that originate from a resource.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\EOT\DC2\ETX7\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ACK\DC2\ETX7\v\DC4\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETX7\NAK\US\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETX7\"#\n\
    \\245\STX\n\
    \\EOT\EOT\SOH\STX\STX\DC2\ETX>\STX\CAN\SUB\231\STX The Schema URL, if known. This is the identifier of the Schema that the resource data\n\
    \ is recorded in. To learn more about Schema URL see\n\
    \ https://opentelemetry.io/docs/specs/otel/schemas/#schema-url\n\
    \ This schema_url applies to the data in the \"resource\" field. It does not apply\n\
    \ to the data in the \"scope_logs\" field which have their own schema_url field.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ENQ\DC2\ETX>\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\SOH\DC2\ETX>\t\DC3\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ETX\DC2\ETX>\SYN\ETB\n\
    \7\n\
    \\STX\EOT\STX\DC2\EOTB\NULP\SOH\SUB+ A collection of Logs produced by a Scope.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETXB\b\DC1\n\
    \\204\SOH\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETXF\STX?\SUB\190\SOH The instrumentation scope information for the logs in this message.\n\
    \ Semantically when InstrumentationScope isn't set, it is equivalent with\n\
    \ an empty instrumentation scope name (unknown).\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ACK\DC2\ETXF\STX4\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETXF5:\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETXF=>\n\
    \%\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETXI\STX%\SUB\CAN A list of log records.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\EOT\DC2\ETXI\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ACK\DC2\ETXI\v\DC4\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETXI\NAK \n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETXI#$\n\
    \\140\STX\n\
    \\EOT\EOT\STX\STX\STX\DC2\ETXO\STX\CAN\SUB\254\SOH The Schema URL, if known. This is the identifier of the Schema that the log data\n\
    \ is recorded in. To learn more about Schema URL see\n\
    \ https://opentelemetry.io/docs/specs/otel/schemas/#schema-url\n\
    \ This schema_url applies to all logs in the \"logs\" field.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ENQ\DC2\ETXO\STX\b\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\SOH\DC2\ETXO\t\DC3\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\STX\ETX\DC2\ETXO\SYN\ETB\n\
    \;\n\
    \\STX\ENQ\NUL\DC2\EOTS\NULn\SOH\SUB/ Possible values for LogRecord.SeverityNumber.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\ENQ\NUL\SOH\DC2\ETXS\ENQ\DC3\n\
    \N\n\
    \\EOT\ENQ\NUL\STX\NUL\DC2\ETXU\STX\"\SUBA UNSPECIFIED is the default SeverityNumber, it MUST NOT be used.\n\
    \\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NUL\SOH\DC2\ETXU\STX\GS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NUL\STX\DC2\ETXU !\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\SOH\DC2\ETXV\STX\GS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\SOH\SOH\DC2\ETXV\STX\ETB\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\SOH\STX\DC2\ETXV\ESC\FS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\STX\DC2\ETXW\STX\GS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\STX\SOH\DC2\ETXW\STX\CAN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\STX\STX\DC2\ETXW\ESC\FS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\ETX\DC2\ETXX\STX\GS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ETX\SOH\DC2\ETXX\STX\CAN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ETX\STX\DC2\ETXX\ESC\FS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\EOT\DC2\ETXY\STX\GS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\EOT\SOH\DC2\ETXY\STX\CAN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\EOT\STX\DC2\ETXY\ESC\FS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\ENQ\DC2\ETXZ\STX\GS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ENQ\SOH\DC2\ETXZ\STX\ETB\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ENQ\STX\DC2\ETXZ\ESC\FS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\ACK\DC2\ETX[\STX\GS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ACK\SOH\DC2\ETX[\STX\CAN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ACK\STX\DC2\ETX[\ESC\FS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\a\DC2\ETX\\\STX\GS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\a\SOH\DC2\ETX\\\STX\CAN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\a\STX\DC2\ETX\\\ESC\FS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\b\DC2\ETX]\STX\GS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\b\SOH\DC2\ETX]\STX\CAN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\b\STX\DC2\ETX]\ESC\FS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\t\DC2\ETX^\STX\GS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\t\SOH\DC2\ETX^\STX\SYN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\t\STX\DC2\ETX^\ESC\FS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\n\
    \\DC2\ETX_\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\n\
    \\SOH\DC2\ETX_\STX\ETB\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\n\
    \\STX\DC2\ETX_\ESC\GS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\v\DC2\ETX`\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\v\SOH\DC2\ETX`\STX\ETB\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\v\STX\DC2\ETX`\ESC\GS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\f\DC2\ETXa\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\f\SOH\DC2\ETXa\STX\ETB\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\f\STX\DC2\ETXa\ESC\GS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\r\DC2\ETXb\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\r\SOH\DC2\ETXb\STX\SYN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\r\STX\DC2\ETXb\ESC\GS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\SO\DC2\ETXc\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\SO\SOH\DC2\ETXc\STX\ETB\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\SO\STX\DC2\ETXc\ESC\GS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\SI\DC2\ETXd\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\SI\SOH\DC2\ETXd\STX\ETB\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\SI\STX\DC2\ETXd\ESC\GS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\DLE\DC2\ETXe\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\DLE\SOH\DC2\ETXe\STX\ETB\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\DLE\STX\DC2\ETXe\ESC\GS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\DC1\DC2\ETXf\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\DC1\SOH\DC2\ETXf\STX\ETB\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\DC1\STX\DC2\ETXf\ESC\GS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\DC2\DC2\ETXg\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\DC2\SOH\DC2\ETXg\STX\CAN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\DC2\STX\DC2\ETXg\ESC\GS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\DC3\DC2\ETXh\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\DC3\SOH\DC2\ETXh\STX\CAN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\DC3\STX\DC2\ETXh\ESC\GS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\DC4\DC2\ETXi\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\DC4\SOH\DC2\ETXi\STX\CAN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\DC4\STX\DC2\ETXi\ESC\GS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\NAK\DC2\ETXj\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NAK\SOH\DC2\ETXj\STX\ETB\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\NAK\STX\DC2\ETXj\ESC\GS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\SYN\DC2\ETXk\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\SYN\SOH\DC2\ETXk\STX\CAN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\SYN\STX\DC2\ETXk\ESC\GS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\ETB\DC2\ETXl\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ETB\SOH\DC2\ETXl\STX\CAN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\ETB\STX\DC2\ETXl\ESC\GS\n\
    \\v\n\
    \\EOT\ENQ\NUL\STX\CAN\DC2\ETXm\STX\RS\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\CAN\SOH\DC2\ETXm\STX\CAN\n\
    \\f\n\
    \\ENQ\ENQ\NUL\STX\CAN\STX\DC2\ETXm\ESC\GS\n\
    \\217\STX\n\
    \\STX\ENQ\SOH\DC2\ENQx\NUL\129\SOH\SOH\SUB\203\STX LogRecordFlags represents constants used to interpret the\n\
    \ LogRecord.flags field, which is protobuf 'fixed32' type and is to\n\
    \ be used as bit-fields. Each non-zero value defined in this enum is\n\
    \ a bit-mask.  To extract the bit-field, for example, use an\n\
    \ expression like:\n\
    \\n\
    \   (logRecord.flags & LOG_RECORD_FLAGS_TRACE_FLAGS_MASK)\n\
    \\n\
    \\n\
    \\n\
    \\n\
    \\ETX\ENQ\SOH\SOH\DC2\ETXx\ENQ\DC3\n\
    \\149\SOH\n\
    \\EOT\ENQ\SOH\STX\NUL\DC2\ETX{\STX\"\SUB\135\SOH The zero value for the enum. Should not be used for comparisons.\n\
    \ Instead use bitwise \"and\" with the appropriate mask as shown above.\n\
    \\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\NUL\SOH\DC2\ETX{\STX\GS\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\NUL\STX\DC2\ETX{ !\n\
    \1\n\
    \\EOT\ENQ\SOH\STX\SOH\DC2\ETX~\STX1\SUB$ Bits 0-7 are used for trace flags.\n\
    \\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\SOH\SOH\DC2\ETX~\STX#\n\
    \\f\n\
    \\ENQ\ENQ\SOH\STX\SOH\STX\DC2\ETX~&0\n\
    \\156\SOH\n\
    \\STX\EOT\ETX\DC2\ACK\133\SOH\NUL\210\SOH\SOH\SUB\141\SOH A log record according to OpenTelemetry Log Data Model:\n\
    \ https://github.com/open-telemetry/oteps/blob/main/text/logs/0097-log-data-model.md\n\
    \\n\
    \\v\n\
    \\ETX\EOT\ETX\SOH\DC2\EOT\133\SOH\b\DC1\n\
    \\v\n\
    \\ETX\EOT\ETX\t\DC2\EOT\134\SOH\STX\r\n\
    \\f\n\
    \\EOT\EOT\ETX\t\NUL\DC2\EOT\134\SOH\v\f\n\
    \\r\n\
    \\ENQ\EOT\ETX\t\NUL\SOH\DC2\EOT\134\SOH\v\f\n\
    \\r\n\
    \\ENQ\EOT\ETX\t\NUL\STX\DC2\EOT\134\SOH\v\f\n\
    \\199\SOH\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\EOT\139\SOH\STX\GS\SUB\184\SOH time_unix_nano is the time when the event occurred.\n\
    \ Value is UNIX Epoch time in nanoseconds since 00:00:00 UTC on 1 January 1970.\n\
    \ Value of 0 indicates unknown or missing timestamp.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\NUL\ENQ\DC2\EOT\139\SOH\STX\t\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\EOT\139\SOH\n\
    \\CAN\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\EOT\139\SOH\ESC\FS\n\
    \\176\a\n\
    \\EOT\EOT\ETX\STX\SOH\DC2\EOT\156\SOH\STX'\SUB\161\a Time when the event was observed by the collection system.\n\
    \ For events that originate in OpenTelemetry (e.g. using OpenTelemetry Logging SDK)\n\
    \ this timestamp is typically set at the generation time and is equal to Timestamp.\n\
    \ For events originating externally and collected by OpenTelemetry (e.g. using\n\
    \ Collector) this is the time when OpenTelemetry's code observed the event measured\n\
    \ by the clock of the OpenTelemetry code. This field MUST be set once the event is\n\
    \ observed by OpenTelemetry.\n\
    \\n\
    \ For converting OpenTelemetry log data to formats that support only one timestamp or\n\
    \ when receiving OpenTelemetry log data by recipients that support only one timestamp\n\
    \ internally the following logic is recommended:\n\
    \   - Use time_unix_nano if it is present, otherwise use observed_time_unix_nano.\n\
    \\n\
    \ Value is UNIX Epoch time in nanoseconds since 00:00:00 UTC on 1 January 1970.\n\
    \ Value of 0 indicates unknown or missing timestamp.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\SOH\ENQ\DC2\EOT\156\SOH\STX\t\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\SOH\SOH\DC2\EOT\156\SOH\n\
    \!\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\SOH\ETX\DC2\EOT\156\SOH$&\n\
    \o\n\
    \\EOT\EOT\ETX\STX\STX\DC2\EOT\160\SOH\STX%\SUBa Numerical value of the severity, normalized to values described in Log Data Model.\n\
    \ [Optional].\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\STX\ACK\DC2\EOT\160\SOH\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\STX\SOH\DC2\EOT\160\SOH\DC1 \n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\STX\ETX\DC2\EOT\160\SOH#$\n\
    \\138\SOH\n\
    \\EOT\EOT\ETX\STX\ETX\DC2\EOT\164\SOH\STX\ESC\SUB| The severity text (also known as log level). The original string representation as\n\
    \ it is known at the source. [Optional].\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ETX\ENQ\DC2\EOT\164\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ETX\SOH\DC2\EOT\164\SOH\t\SYN\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ETX\ETX\DC2\EOT\164\SOH\EM\SUB\n\
    \\135\STX\n\
    \\EOT\EOT\ETX\STX\EOT\DC2\EOT\169\SOH\STX2\SUB\248\SOH A value containing the body of the log record. Can be for example a human-readable\n\
    \ string message (including multi-line) describing the event in a free form or it can\n\
    \ be a structured data composed of arrays and maps of other values. [Optional].\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\EOT\ACK\DC2\EOT\169\SOH\STX(\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\EOT\SOH\DC2\EOT\169\SOH)-\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\EOT\ETX\DC2\EOT\169\SOH01\n\
    \\198\SOH\n\
    \\EOT\EOT\ETX\STX\ENQ\DC2\EOT\174\SOH\STXA\SUB\183\SOH Additional attributes that describe the specific event occurrence. [Optional].\n\
    \ Attribute keys MUST be unique (it is not allowed to have more than one\n\
    \ attribute with the same key).\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ENQ\EOT\DC2\EOT\174\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ENQ\ACK\DC2\EOT\174\SOH\v1\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ENQ\SOH\DC2\EOT\174\SOH2<\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ENQ\ETX\DC2\EOT\174\SOH?@\n\
    \\f\n\
    \\EOT\EOT\ETX\STX\ACK\DC2\EOT\175\SOH\STX&\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ACK\ENQ\DC2\EOT\175\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ACK\SOH\DC2\EOT\175\SOH\t!\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ACK\ETX\DC2\EOT\175\SOH$%\n\
    \\255\STX\n\
    \\EOT\EOT\ETX\STX\a\DC2\EOT\182\SOH\STX\DC4\SUB\240\STX Flags, a bit field. 8 least significant bits are the trace flags as\n\
    \ defined in W3C Trace Context specification. 24 most significant bits are reserved\n\
    \ and must be set to 0. Readers must not assume that 24 most significant bits\n\
    \ will be zero and must correctly mask the bits when reading 8-bit trace flag (use\n\
    \ flags & LOG_RECORD_FLAGS_TRACE_FLAGS_MASK). [Optional].\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\a\ENQ\DC2\EOT\182\SOH\STX\t\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\a\SOH\DC2\EOT\182\SOH\n\
    \\SI\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\a\ETX\DC2\EOT\182\SOH\DC2\DC3\n\
    \\239\ETX\n\
    \\EOT\EOT\ETX\STX\b\DC2\EOT\195\SOH\STX\NAK\SUB\224\ETX A unique identifier for a trace. All logs from the same trace share\n\
    \ the same `trace_id`. The ID is a 16-byte array. An ID with all zeroes OR\n\
    \ of length other than 16 bytes is considered invalid (empty string in OTLP/JSON\n\
    \ is zero-length and thus is also invalid).\n\
    \\n\
    \ This field is optional.\n\
    \\n\
    \ The receivers SHOULD assume that the log record is not associated with a\n\
    \ trace if any of the following is true:\n\
    \   - the field is not present,\n\
    \   - the field contains an invalid value.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\b\ENQ\DC2\EOT\195\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\b\SOH\DC2\EOT\195\SOH\b\DLE\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\b\ETX\DC2\EOT\195\SOH\DC3\DC4\n\
    \\189\EOT\n\
    \\EOT\EOT\ETX\STX\t\DC2\EOT\209\SOH\STX\NAK\SUB\174\EOT A unique identifier for a span within a trace, assigned when the span\n\
    \ is created. The ID is an 8-byte array. An ID with all zeroes OR of length\n\
    \ other than 8 bytes is considered invalid (empty string in OTLP/JSON\n\
    \ is zero-length and thus is also invalid).\n\
    \\n\
    \ This field is optional. If the sender specifies a valid span_id then it SHOULD also\n\
    \ specify a valid trace_id.\n\
    \\n\
    \ The receivers SHOULD assume that the log record is not associated with a\n\
    \ span if any of the following is true:\n\
    \   - the field is not present,\n\
    \   - the field contains an invalid value.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\t\ENQ\DC2\EOT\209\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\t\SOH\DC2\EOT\209\SOH\b\SI\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\t\ETX\DC2\EOT\209\SOH\DC2\DC4b\ACKproto3"