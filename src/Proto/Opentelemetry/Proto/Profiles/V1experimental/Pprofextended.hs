{- This file was auto-generated from opentelemetry/proto/profiles/v1experimental/pprofextended.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended (
        AggregationTemporality(..), AggregationTemporality(),
        AggregationTemporality'UnrecognizedValue, AttributeUnit(),
        BuildIdKind(..), BuildIdKind(), BuildIdKind'UnrecognizedValue,
        Function(), Label(), Line(), Link(), Location(), Mapping(),
        Profile(), Sample(), ValueType()
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
newtype AggregationTemporality'UnrecognizedValue
  = AggregationTemporality'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data AggregationTemporality
  = AGGREGATION_TEMPORALITY_UNSPECIFIED |
    AGGREGATION_TEMPORALITY_DELTA |
    AGGREGATION_TEMPORALITY_CUMULATIVE |
    AggregationTemporality'Unrecognized !AggregationTemporality'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum AggregationTemporality where
  maybeToEnum 0 = Prelude.Just AGGREGATION_TEMPORALITY_UNSPECIFIED
  maybeToEnum 1 = Prelude.Just AGGREGATION_TEMPORALITY_DELTA
  maybeToEnum 2 = Prelude.Just AGGREGATION_TEMPORALITY_CUMULATIVE
  maybeToEnum k
    = Prelude.Just
        (AggregationTemporality'Unrecognized
           (AggregationTemporality'UnrecognizedValue
              (Prelude.fromIntegral k)))
  showEnum AGGREGATION_TEMPORALITY_UNSPECIFIED
    = "AGGREGATION_TEMPORALITY_UNSPECIFIED"
  showEnum AGGREGATION_TEMPORALITY_DELTA
    = "AGGREGATION_TEMPORALITY_DELTA"
  showEnum AGGREGATION_TEMPORALITY_CUMULATIVE
    = "AGGREGATION_TEMPORALITY_CUMULATIVE"
  showEnum
    (AggregationTemporality'Unrecognized (AggregationTemporality'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "AGGREGATION_TEMPORALITY_UNSPECIFIED"
    = Prelude.Just AGGREGATION_TEMPORALITY_UNSPECIFIED
    | (Prelude.==) k "AGGREGATION_TEMPORALITY_DELTA"
    = Prelude.Just AGGREGATION_TEMPORALITY_DELTA
    | (Prelude.==) k "AGGREGATION_TEMPORALITY_CUMULATIVE"
    = Prelude.Just AGGREGATION_TEMPORALITY_CUMULATIVE
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded AggregationTemporality where
  minBound = AGGREGATION_TEMPORALITY_UNSPECIFIED
  maxBound = AGGREGATION_TEMPORALITY_CUMULATIVE
instance Prelude.Enum AggregationTemporality where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum AggregationTemporality: "
              (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum AGGREGATION_TEMPORALITY_UNSPECIFIED = 0
  fromEnum AGGREGATION_TEMPORALITY_DELTA = 1
  fromEnum AGGREGATION_TEMPORALITY_CUMULATIVE = 2
  fromEnum
    (AggregationTemporality'Unrecognized (AggregationTemporality'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ AGGREGATION_TEMPORALITY_CUMULATIVE
    = Prelude.error
        "AggregationTemporality.succ: bad argument AGGREGATION_TEMPORALITY_CUMULATIVE. This value would be out of bounds."
  succ AGGREGATION_TEMPORALITY_UNSPECIFIED
    = AGGREGATION_TEMPORALITY_DELTA
  succ AGGREGATION_TEMPORALITY_DELTA
    = AGGREGATION_TEMPORALITY_CUMULATIVE
  succ (AggregationTemporality'Unrecognized _)
    = Prelude.error
        "AggregationTemporality.succ: bad argument: unrecognized value"
  pred AGGREGATION_TEMPORALITY_UNSPECIFIED
    = Prelude.error
        "AggregationTemporality.pred: bad argument AGGREGATION_TEMPORALITY_UNSPECIFIED. This value would be out of bounds."
  pred AGGREGATION_TEMPORALITY_DELTA
    = AGGREGATION_TEMPORALITY_UNSPECIFIED
  pred AGGREGATION_TEMPORALITY_CUMULATIVE
    = AGGREGATION_TEMPORALITY_DELTA
  pred (AggregationTemporality'Unrecognized _)
    = Prelude.error
        "AggregationTemporality.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault AggregationTemporality where
  fieldDefault = AGGREGATION_TEMPORALITY_UNSPECIFIED
instance Control.DeepSeq.NFData AggregationTemporality where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.attributeKey' @:: Lens' AttributeUnit Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.unit' @:: Lens' AttributeUnit Data.Int.Int64@ -}
data AttributeUnit
  = AttributeUnit'_constructor {_AttributeUnit'attributeKey :: !Data.Int.Int64,
                                _AttributeUnit'unit :: !Data.Int.Int64,
                                _AttributeUnit'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show AttributeUnit where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField AttributeUnit "attributeKey" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AttributeUnit'attributeKey
           (\ x__ y__ -> x__ {_AttributeUnit'attributeKey = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField AttributeUnit "unit" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _AttributeUnit'unit (\ x__ y__ -> x__ {_AttributeUnit'unit = y__}))
        Prelude.id
instance Data.ProtoLens.Message AttributeUnit where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.profiles.v1experimental.AttributeUnit"
  packedMessageDescriptor _
    = "\n\
      \\rAttributeUnit\DC2#\n\
      \\rattribute_key\CAN\SOH \SOH(\ETXR\fattributeKey\DC2\DC2\n\
      \\EOTunit\CAN\STX \SOH(\ETXR\EOTunit"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        attributeKey__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "attribute_key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"attributeKey")) ::
              Data.ProtoLens.FieldDescriptor AttributeUnit
        unit__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "unit"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"unit")) ::
              Data.ProtoLens.FieldDescriptor AttributeUnit
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, attributeKey__field_descriptor),
           (Data.ProtoLens.Tag 2, unit__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _AttributeUnit'_unknownFields
        (\ x__ y__ -> x__ {_AttributeUnit'_unknownFields = y__})
  defMessage
    = AttributeUnit'_constructor
        {_AttributeUnit'attributeKey = Data.ProtoLens.fieldDefault,
         _AttributeUnit'unit = Data.ProtoLens.fieldDefault,
         _AttributeUnit'_unknownFields = []}
  parseMessage
    = let
        loop ::
          AttributeUnit -> Data.ProtoLens.Encoding.Bytes.Parser AttributeUnit
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
                                       "attribute_key"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"attributeKey") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "unit"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"unit") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "AttributeUnit"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view (Data.ProtoLens.Field.field @"attributeKey") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"unit") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData AttributeUnit where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_AttributeUnit'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_AttributeUnit'attributeKey x__)
                (Control.DeepSeq.deepseq (_AttributeUnit'unit x__) ()))
newtype BuildIdKind'UnrecognizedValue
  = BuildIdKind'UnrecognizedValue Data.Int.Int32
  deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show)
data BuildIdKind
  = BUILD_ID_LINKER |
    BUILD_ID_BINARY_HASH |
    BuildIdKind'Unrecognized !BuildIdKind'UnrecognizedValue
  deriving stock (Prelude.Show, Prelude.Eq, Prelude.Ord)
instance Data.ProtoLens.MessageEnum BuildIdKind where
  maybeToEnum 0 = Prelude.Just BUILD_ID_LINKER
  maybeToEnum 1 = Prelude.Just BUILD_ID_BINARY_HASH
  maybeToEnum k
    = Prelude.Just
        (BuildIdKind'Unrecognized
           (BuildIdKind'UnrecognizedValue (Prelude.fromIntegral k)))
  showEnum BUILD_ID_LINKER = "BUILD_ID_LINKER"
  showEnum BUILD_ID_BINARY_HASH = "BUILD_ID_BINARY_HASH"
  showEnum
    (BuildIdKind'Unrecognized (BuildIdKind'UnrecognizedValue k))
    = Prelude.show k
  readEnum k
    | (Prelude.==) k "BUILD_ID_LINKER" = Prelude.Just BUILD_ID_LINKER
    | (Prelude.==) k "BUILD_ID_BINARY_HASH"
    = Prelude.Just BUILD_ID_BINARY_HASH
    | Prelude.otherwise
    = (Prelude.>>=) (Text.Read.readMaybe k) Data.ProtoLens.maybeToEnum
instance Prelude.Bounded BuildIdKind where
  minBound = BUILD_ID_LINKER
  maxBound = BUILD_ID_BINARY_HASH
instance Prelude.Enum BuildIdKind where
  toEnum k__
    = Prelude.maybe
        (Prelude.error
           ((Prelude.++)
              "toEnum: unknown value for enum BuildIdKind: " (Prelude.show k__)))
        Prelude.id (Data.ProtoLens.maybeToEnum k__)
  fromEnum BUILD_ID_LINKER = 0
  fromEnum BUILD_ID_BINARY_HASH = 1
  fromEnum
    (BuildIdKind'Unrecognized (BuildIdKind'UnrecognizedValue k))
    = Prelude.fromIntegral k
  succ BUILD_ID_BINARY_HASH
    = Prelude.error
        "BuildIdKind.succ: bad argument BUILD_ID_BINARY_HASH. This value would be out of bounds."
  succ BUILD_ID_LINKER = BUILD_ID_BINARY_HASH
  succ (BuildIdKind'Unrecognized _)
    = Prelude.error
        "BuildIdKind.succ: bad argument: unrecognized value"
  pred BUILD_ID_LINKER
    = Prelude.error
        "BuildIdKind.pred: bad argument BUILD_ID_LINKER. This value would be out of bounds."
  pred BUILD_ID_BINARY_HASH = BUILD_ID_LINKER
  pred (BuildIdKind'Unrecognized _)
    = Prelude.error
        "BuildIdKind.pred: bad argument: unrecognized value"
  enumFrom = Data.ProtoLens.Message.Enum.messageEnumFrom
  enumFromTo = Data.ProtoLens.Message.Enum.messageEnumFromTo
  enumFromThen = Data.ProtoLens.Message.Enum.messageEnumFromThen
  enumFromThenTo = Data.ProtoLens.Message.Enum.messageEnumFromThenTo
instance Data.ProtoLens.FieldDefault BuildIdKind where
  fieldDefault = BUILD_ID_LINKER
instance Control.DeepSeq.NFData BuildIdKind where
  rnf x__ = Prelude.seq x__ ()
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.id' @:: Lens' Function Data.Word.Word64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.name' @:: Lens' Function Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.systemName' @:: Lens' Function Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.filename' @:: Lens' Function Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.startLine' @:: Lens' Function Data.Int.Int64@ -}
data Function
  = Function'_constructor {_Function'id :: !Data.Word.Word64,
                           _Function'name :: !Data.Int.Int64,
                           _Function'systemName :: !Data.Int.Int64,
                           _Function'filename :: !Data.Int.Int64,
                           _Function'startLine :: !Data.Int.Int64,
                           _Function'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Function where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Function "id" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Function'id (\ x__ y__ -> x__ {_Function'id = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Function "name" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Function'name (\ x__ y__ -> x__ {_Function'name = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Function "systemName" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Function'systemName
           (\ x__ y__ -> x__ {_Function'systemName = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Function "filename" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Function'filename (\ x__ y__ -> x__ {_Function'filename = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Function "startLine" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Function'startLine (\ x__ y__ -> x__ {_Function'startLine = y__}))
        Prelude.id
instance Data.ProtoLens.Message Function where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.profiles.v1experimental.Function"
  packedMessageDescriptor _
    = "\n\
      \\bFunction\DC2\SO\n\
      \\STXid\CAN\SOH \SOH(\EOTR\STXid\DC2\DC2\n\
      \\EOTname\CAN\STX \SOH(\ETXR\EOTname\DC2\US\n\
      \\vsystem_name\CAN\ETX \SOH(\ETXR\n\
      \systemName\DC2\SUB\n\
      \\bfilename\CAN\EOT \SOH(\ETXR\bfilename\DC2\GS\n\
      \\n\
      \start_line\CAN\ENQ \SOH(\ETXR\tstartLine"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        id__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"id")) ::
              Data.ProtoLens.FieldDescriptor Function
        name__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"name")) ::
              Data.ProtoLens.FieldDescriptor Function
        systemName__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "system_name"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"systemName")) ::
              Data.ProtoLens.FieldDescriptor Function
        filename__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "filename"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"filename")) ::
              Data.ProtoLens.FieldDescriptor Function
        startLine__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "start_line"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"startLine")) ::
              Data.ProtoLens.FieldDescriptor Function
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, id__field_descriptor),
           (Data.ProtoLens.Tag 2, name__field_descriptor),
           (Data.ProtoLens.Tag 3, systemName__field_descriptor),
           (Data.ProtoLens.Tag 4, filename__field_descriptor),
           (Data.ProtoLens.Tag 5, startLine__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Function'_unknownFields
        (\ x__ y__ -> x__ {_Function'_unknownFields = y__})
  defMessage
    = Function'_constructor
        {_Function'id = Data.ProtoLens.fieldDefault,
         _Function'name = Data.ProtoLens.fieldDefault,
         _Function'systemName = Data.ProtoLens.fieldDefault,
         _Function'filename = Data.ProtoLens.fieldDefault,
         _Function'startLine = Data.ProtoLens.fieldDefault,
         _Function'_unknownFields = []}
  parseMessage
    = let
        loop :: Function -> Data.ProtoLens.Encoding.Bytes.Parser Function
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
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "id"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"id") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "name"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"name") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "system_name"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"systemName") y x)
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "filename"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"filename") y x)
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "start_line"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"startLine") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Function"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"id") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"name") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"systemName") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                   ((Data.Monoid.<>)
                      (let
                         _v = Lens.Family2.view (Data.ProtoLens.Field.field @"filename") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                      ((Data.Monoid.<>)
                         (let
                            _v = Lens.Family2.view (Data.ProtoLens.Field.field @"startLine") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral
                                     _v))
                         (Data.ProtoLens.Encoding.Wire.buildFieldSet
                            (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))
instance Control.DeepSeq.NFData Function where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Function'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Function'id x__)
                (Control.DeepSeq.deepseq
                   (_Function'name x__)
                   (Control.DeepSeq.deepseq
                      (_Function'systemName x__)
                      (Control.DeepSeq.deepseq
                         (_Function'filename x__)
                         (Control.DeepSeq.deepseq (_Function'startLine x__) ())))))
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.key' @:: Lens' Label Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.str' @:: Lens' Label Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.num' @:: Lens' Label Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.numUnit' @:: Lens' Label Data.Int.Int64@ -}
data Label
  = Label'_constructor {_Label'key :: !Data.Int.Int64,
                        _Label'str :: !Data.Int.Int64,
                        _Label'num :: !Data.Int.Int64,
                        _Label'numUnit :: !Data.Int.Int64,
                        _Label'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Label where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Label "key" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Label'key (\ x__ y__ -> x__ {_Label'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Label "str" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Label'str (\ x__ y__ -> x__ {_Label'str = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Label "num" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Label'num (\ x__ y__ -> x__ {_Label'num = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Label "numUnit" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Label'numUnit (\ x__ y__ -> x__ {_Label'numUnit = y__}))
        Prelude.id
instance Data.ProtoLens.Message Label where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.profiles.v1experimental.Label"
  packedMessageDescriptor _
    = "\n\
      \\ENQLabel\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\ETXR\ETXkey\DC2\DLE\n\
      \\ETXstr\CAN\STX \SOH(\ETXR\ETXstr\DC2\DLE\n\
      \\ETXnum\CAN\ETX \SOH(\ETXR\ETXnum\DC2\EM\n\
      \\bnum_unit\CAN\EOT \SOH(\ETXR\anumUnit"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor Label
        str__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "str"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"str")) ::
              Data.ProtoLens.FieldDescriptor Label
        num__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "num"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"num")) ::
              Data.ProtoLens.FieldDescriptor Label
        numUnit__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "num_unit"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"numUnit")) ::
              Data.ProtoLens.FieldDescriptor Label
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, str__field_descriptor),
           (Data.ProtoLens.Tag 3, num__field_descriptor),
           (Data.ProtoLens.Tag 4, numUnit__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Label'_unknownFields
        (\ x__ y__ -> x__ {_Label'_unknownFields = y__})
  defMessage
    = Label'_constructor
        {_Label'key = Data.ProtoLens.fieldDefault,
         _Label'str = Data.ProtoLens.fieldDefault,
         _Label'num = Data.ProtoLens.fieldDefault,
         _Label'numUnit = Data.ProtoLens.fieldDefault,
         _Label'_unknownFields = []}
  parseMessage
    = let
        loop :: Label -> Data.ProtoLens.Encoding.Bytes.Parser Label
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
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "str"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"str") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "num"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"num") y x)
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "num_unit"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"numUnit") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Label"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"str") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                ((Data.Monoid.<>)
                   (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"num") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                   ((Data.Monoid.<>)
                      (let
                         _v = Lens.Family2.view (Data.ProtoLens.Field.field @"numUnit") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                      (Data.ProtoLens.Encoding.Wire.buildFieldSet
                         (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))
instance Control.DeepSeq.NFData Label where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Label'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Label'key x__)
                (Control.DeepSeq.deepseq
                   (_Label'str x__)
                   (Control.DeepSeq.deepseq
                      (_Label'num x__)
                      (Control.DeepSeq.deepseq (_Label'numUnit x__) ()))))
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.functionIndex' @:: Lens' Line Data.Word.Word64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.line' @:: Lens' Line Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.column' @:: Lens' Line Data.Int.Int64@ -}
data Line
  = Line'_constructor {_Line'functionIndex :: !Data.Word.Word64,
                       _Line'line :: !Data.Int.Int64,
                       _Line'column :: !Data.Int.Int64,
                       _Line'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Line where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Line "functionIndex" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Line'functionIndex (\ x__ y__ -> x__ {_Line'functionIndex = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Line "line" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Line'line (\ x__ y__ -> x__ {_Line'line = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Line "column" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Line'column (\ x__ y__ -> x__ {_Line'column = y__}))
        Prelude.id
instance Data.ProtoLens.Message Line where
  messageName _
    = Data.Text.pack "opentelemetry.proto.profiles.v1experimental.Line"
  packedMessageDescriptor _
    = "\n\
      \\EOTLine\DC2%\n\
      \\SOfunction_index\CAN\SOH \SOH(\EOTR\rfunctionIndex\DC2\DC2\n\
      \\EOTline\CAN\STX \SOH(\ETXR\EOTline\DC2\SYN\n\
      \\ACKcolumn\CAN\ETX \SOH(\ETXR\ACKcolumn"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        functionIndex__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "function_index"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"functionIndex")) ::
              Data.ProtoLens.FieldDescriptor Line
        line__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "line"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"line")) ::
              Data.ProtoLens.FieldDescriptor Line
        column__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "column"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"column")) ::
              Data.ProtoLens.FieldDescriptor Line
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, functionIndex__field_descriptor),
           (Data.ProtoLens.Tag 2, line__field_descriptor),
           (Data.ProtoLens.Tag 3, column__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Line'_unknownFields
        (\ x__ y__ -> x__ {_Line'_unknownFields = y__})
  defMessage
    = Line'_constructor
        {_Line'functionIndex = Data.ProtoLens.fieldDefault,
         _Line'line = Data.ProtoLens.fieldDefault,
         _Line'column = Data.ProtoLens.fieldDefault,
         _Line'_unknownFields = []}
  parseMessage
    = let
        loop :: Line -> Data.ProtoLens.Encoding.Bytes.Parser Line
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
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "function_index"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"functionIndex") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "line"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"line") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "column"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"column") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Line"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"functionIndex") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"line") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                ((Data.Monoid.<>)
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"column") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData Line where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Line'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Line'functionIndex x__)
                (Control.DeepSeq.deepseq
                   (_Line'line x__) (Control.DeepSeq.deepseq (_Line'column x__) ())))
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.traceId' @:: Lens' Link Data.ByteString.ByteString@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.spanId' @:: Lens' Link Data.ByteString.ByteString@ -}
data Link
  = Link'_constructor {_Link'traceId :: !Data.ByteString.ByteString,
                       _Link'spanId :: !Data.ByteString.ByteString,
                       _Link'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Link where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Link "traceId" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Link'traceId (\ x__ y__ -> x__ {_Link'traceId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Link "spanId" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Link'spanId (\ x__ y__ -> x__ {_Link'spanId = y__}))
        Prelude.id
instance Data.ProtoLens.Message Link where
  messageName _
    = Data.Text.pack "opentelemetry.proto.profiles.v1experimental.Link"
  packedMessageDescriptor _
    = "\n\
      \\EOTLink\DC2\EM\n\
      \\btrace_id\CAN\SOH \SOH(\fR\atraceId\DC2\ETB\n\
      \\aspan_id\CAN\STX \SOH(\fR\ACKspanId"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        traceId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "trace_id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"traceId")) ::
              Data.ProtoLens.FieldDescriptor Link
        spanId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "span_id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"spanId")) ::
              Data.ProtoLens.FieldDescriptor Link
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, traceId__field_descriptor),
           (Data.ProtoLens.Tag 2, spanId__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Link'_unknownFields
        (\ x__ y__ -> x__ {_Link'_unknownFields = y__})
  defMessage
    = Link'_constructor
        {_Link'traceId = Data.ProtoLens.fieldDefault,
         _Link'spanId = Data.ProtoLens.fieldDefault,
         _Link'_unknownFields = []}
  parseMessage
    = let
        loop :: Link -> Data.ProtoLens.Encoding.Bytes.Parser Link
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
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "trace_id"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"traceId") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "span_id"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"spanId") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "Link"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"traceId") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((\ bs
                          -> (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (Prelude.fromIntegral (Data.ByteString.length bs)))
                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"spanId") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData Link where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Link'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Link'traceId x__)
                (Control.DeepSeq.deepseq (_Link'spanId x__) ()))
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.id' @:: Lens' Location Data.Word.Word64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.mappingIndex' @:: Lens' Location Data.Word.Word64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.address' @:: Lens' Location Data.Word.Word64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.line' @:: Lens' Location [Line]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'line' @:: Lens' Location (Data.Vector.Vector Line)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.isFolded' @:: Lens' Location Prelude.Bool@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.typeIndex' @:: Lens' Location Data.Word.Word32@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.attributes' @:: Lens' Location [Data.Word.Word64]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'attributes' @:: Lens' Location (Data.Vector.Unboxed.Vector Data.Word.Word64)@ -}
data Location
  = Location'_constructor {_Location'id :: !Data.Word.Word64,
                           _Location'mappingIndex :: !Data.Word.Word64,
                           _Location'address :: !Data.Word.Word64,
                           _Location'line :: !(Data.Vector.Vector Line),
                           _Location'isFolded :: !Prelude.Bool,
                           _Location'typeIndex :: !Data.Word.Word32,
                           _Location'attributes :: !(Data.Vector.Unboxed.Vector Data.Word.Word64),
                           _Location'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Location where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Location "id" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Location'id (\ x__ y__ -> x__ {_Location'id = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Location "mappingIndex" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Location'mappingIndex
           (\ x__ y__ -> x__ {_Location'mappingIndex = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Location "address" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Location'address (\ x__ y__ -> x__ {_Location'address = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Location "line" [Line] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Location'line (\ x__ y__ -> x__ {_Location'line = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Location "vec'line" (Data.Vector.Vector Line) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Location'line (\ x__ y__ -> x__ {_Location'line = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Location "isFolded" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Location'isFolded (\ x__ y__ -> x__ {_Location'isFolded = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Location "typeIndex" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Location'typeIndex (\ x__ y__ -> x__ {_Location'typeIndex = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Location "attributes" [Data.Word.Word64] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Location'attributes
           (\ x__ y__ -> x__ {_Location'attributes = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Location "vec'attributes" (Data.Vector.Unboxed.Vector Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Location'attributes
           (\ x__ y__ -> x__ {_Location'attributes = y__}))
        Prelude.id
instance Data.ProtoLens.Message Location where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.profiles.v1experimental.Location"
  packedMessageDescriptor _
    = "\n\
      \\bLocation\DC2\SO\n\
      \\STXid\CAN\SOH \SOH(\EOTR\STXid\DC2#\n\
      \\rmapping_index\CAN\STX \SOH(\EOTR\fmappingIndex\DC2\CAN\n\
      \\aaddress\CAN\ETX \SOH(\EOTR\aaddress\DC2E\n\
      \\EOTline\CAN\EOT \ETX(\v21.opentelemetry.proto.profiles.v1experimental.LineR\EOTline\DC2\ESC\n\
      \\tis_folded\CAN\ENQ \SOH(\bR\bisFolded\DC2\GS\n\
      \\n\
      \type_index\CAN\ACK \SOH(\rR\ttypeIndex\DC2\RS\n\
      \\n\
      \attributes\CAN\a \ETX(\EOTR\n\
      \attributes"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        id__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"id")) ::
              Data.ProtoLens.FieldDescriptor Location
        mappingIndex__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapping_index"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"mappingIndex")) ::
              Data.ProtoLens.FieldDescriptor Location
        address__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "address"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"address")) ::
              Data.ProtoLens.FieldDescriptor Location
        line__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "line"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Line)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"line")) ::
              Data.ProtoLens.FieldDescriptor Location
        isFolded__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "is_folded"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"isFolded")) ::
              Data.ProtoLens.FieldDescriptor Location
        typeIndex__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type_index"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"typeIndex")) ::
              Data.ProtoLens.FieldDescriptor Location
        attributes__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "attributes"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"attributes")) ::
              Data.ProtoLens.FieldDescriptor Location
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, id__field_descriptor),
           (Data.ProtoLens.Tag 2, mappingIndex__field_descriptor),
           (Data.ProtoLens.Tag 3, address__field_descriptor),
           (Data.ProtoLens.Tag 4, line__field_descriptor),
           (Data.ProtoLens.Tag 5, isFolded__field_descriptor),
           (Data.ProtoLens.Tag 6, typeIndex__field_descriptor),
           (Data.ProtoLens.Tag 7, attributes__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Location'_unknownFields
        (\ x__ y__ -> x__ {_Location'_unknownFields = y__})
  defMessage
    = Location'_constructor
        {_Location'id = Data.ProtoLens.fieldDefault,
         _Location'mappingIndex = Data.ProtoLens.fieldDefault,
         _Location'address = Data.ProtoLens.fieldDefault,
         _Location'line = Data.Vector.Generic.empty,
         _Location'isFolded = Data.ProtoLens.fieldDefault,
         _Location'typeIndex = Data.ProtoLens.fieldDefault,
         _Location'attributes = Data.Vector.Generic.empty,
         _Location'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Location
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Word.Word64
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Line
                -> Data.ProtoLens.Encoding.Bytes.Parser Location
        loop x mutable'attributes mutable'line
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'attributes <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                             (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                mutable'attributes)
                      frozen'line <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'line)
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
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'line") frozen'line x)))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"id") y x)
                                  mutable'attributes mutable'line
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "mapping_index"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"mappingIndex") y x)
                                  mutable'attributes mutable'line
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "address"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"address") y x)
                                  mutable'attributes mutable'line
                        34
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "line"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'line y)
                                loop x mutable'attributes v
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "is_folded"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"isFolded") y x)
                                  mutable'attributes mutable'line
                        48
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "type_index"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"typeIndex") y x)
                                  mutable'attributes mutable'line
                        56
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        Data.ProtoLens.Encoding.Bytes.getVarInt "attributes"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'attributes y)
                                loop x v mutable'line
                        58
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                    "attributes"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'attributes)
                                loop x y mutable'line
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'attributes mutable'line
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'attributes <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      Data.ProtoLens.Encoding.Growing.new
              mutable'line <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'attributes mutable'line)
          "Location"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"id") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"mappingIndex") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                ((Data.Monoid.<>)
                   (let
                      _v = Lens.Family2.view (Data.ProtoLens.Field.field @"address") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                   ((Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.ProtoLens.encodeMessage _v))
                         (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'line") _x))
                      ((Data.Monoid.<>)
                         (let
                            _v = Lens.Family2.view (Data.ProtoLens.Field.field @"isFolded") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (\ b -> if b then 1 else 0) _v))
                         ((Data.Monoid.<>)
                            (let
                               _v = Lens.Family2.view (Data.ProtoLens.Field.field @"typeIndex") _x
                             in
                               if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                   Data.Monoid.mempty
                               else
                                   (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt 48)
                                     ((Prelude..)
                                        Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral
                                        _v))
                            ((Data.Monoid.<>)
                               (let
                                  p = Lens.Family2.view
                                        (Data.ProtoLens.Field.field @"vec'attributes") _x
                                in
                                  if Data.Vector.Generic.null p then
                                      Data.Monoid.mempty
                                  else
                                      (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt 58)
                                        ((\ bs
                                            -> (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    (Prelude.fromIntegral
                                                       (Data.ByteString.length bs)))
                                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                           (Data.ProtoLens.Encoding.Bytes.runBuilder
                                              (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                 Data.ProtoLens.Encoding.Bytes.putVarInt p))))
                               (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                  (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))))
instance Control.DeepSeq.NFData Location where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Location'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Location'id x__)
                (Control.DeepSeq.deepseq
                   (_Location'mappingIndex x__)
                   (Control.DeepSeq.deepseq
                      (_Location'address x__)
                      (Control.DeepSeq.deepseq
                         (_Location'line x__)
                         (Control.DeepSeq.deepseq
                            (_Location'isFolded x__)
                            (Control.DeepSeq.deepseq
                               (_Location'typeIndex x__)
                               (Control.DeepSeq.deepseq (_Location'attributes x__) ())))))))
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.id' @:: Lens' Mapping Data.Word.Word64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.memoryStart' @:: Lens' Mapping Data.Word.Word64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.memoryLimit' @:: Lens' Mapping Data.Word.Word64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.fileOffset' @:: Lens' Mapping Data.Word.Word64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.filename' @:: Lens' Mapping Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.buildId' @:: Lens' Mapping Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.buildIdKind' @:: Lens' Mapping BuildIdKind@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.attributes' @:: Lens' Mapping [Data.Word.Word64]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'attributes' @:: Lens' Mapping (Data.Vector.Unboxed.Vector Data.Word.Word64)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.hasFunctions' @:: Lens' Mapping Prelude.Bool@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.hasFilenames' @:: Lens' Mapping Prelude.Bool@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.hasLineNumbers' @:: Lens' Mapping Prelude.Bool@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.hasInlineFrames' @:: Lens' Mapping Prelude.Bool@ -}
data Mapping
  = Mapping'_constructor {_Mapping'id :: !Data.Word.Word64,
                          _Mapping'memoryStart :: !Data.Word.Word64,
                          _Mapping'memoryLimit :: !Data.Word.Word64,
                          _Mapping'fileOffset :: !Data.Word.Word64,
                          _Mapping'filename :: !Data.Int.Int64,
                          _Mapping'buildId :: !Data.Int.Int64,
                          _Mapping'buildIdKind :: !BuildIdKind,
                          _Mapping'attributes :: !(Data.Vector.Unboxed.Vector Data.Word.Word64),
                          _Mapping'hasFunctions :: !Prelude.Bool,
                          _Mapping'hasFilenames :: !Prelude.Bool,
                          _Mapping'hasLineNumbers :: !Prelude.Bool,
                          _Mapping'hasInlineFrames :: !Prelude.Bool,
                          _Mapping'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Mapping where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Mapping "id" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Mapping'id (\ x__ y__ -> x__ {_Mapping'id = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Mapping "memoryStart" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Mapping'memoryStart
           (\ x__ y__ -> x__ {_Mapping'memoryStart = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Mapping "memoryLimit" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Mapping'memoryLimit
           (\ x__ y__ -> x__ {_Mapping'memoryLimit = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Mapping "fileOffset" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Mapping'fileOffset (\ x__ y__ -> x__ {_Mapping'fileOffset = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Mapping "filename" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Mapping'filename (\ x__ y__ -> x__ {_Mapping'filename = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Mapping "buildId" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Mapping'buildId (\ x__ y__ -> x__ {_Mapping'buildId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Mapping "buildIdKind" BuildIdKind where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Mapping'buildIdKind
           (\ x__ y__ -> x__ {_Mapping'buildIdKind = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Mapping "attributes" [Data.Word.Word64] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Mapping'attributes (\ x__ y__ -> x__ {_Mapping'attributes = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Mapping "vec'attributes" (Data.Vector.Unboxed.Vector Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Mapping'attributes (\ x__ y__ -> x__ {_Mapping'attributes = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Mapping "hasFunctions" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Mapping'hasFunctions
           (\ x__ y__ -> x__ {_Mapping'hasFunctions = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Mapping "hasFilenames" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Mapping'hasFilenames
           (\ x__ y__ -> x__ {_Mapping'hasFilenames = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Mapping "hasLineNumbers" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Mapping'hasLineNumbers
           (\ x__ y__ -> x__ {_Mapping'hasLineNumbers = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Mapping "hasInlineFrames" Prelude.Bool where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Mapping'hasInlineFrames
           (\ x__ y__ -> x__ {_Mapping'hasInlineFrames = y__}))
        Prelude.id
instance Data.ProtoLens.Message Mapping where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.profiles.v1experimental.Mapping"
  packedMessageDescriptor _
    = "\n\
      \\aMapping\DC2\SO\n\
      \\STXid\CAN\SOH \SOH(\EOTR\STXid\DC2!\n\
      \\fmemory_start\CAN\STX \SOH(\EOTR\vmemoryStart\DC2!\n\
      \\fmemory_limit\CAN\ETX \SOH(\EOTR\vmemoryLimit\DC2\US\n\
      \\vfile_offset\CAN\EOT \SOH(\EOTR\n\
      \fileOffset\DC2\SUB\n\
      \\bfilename\CAN\ENQ \SOH(\ETXR\bfilename\DC2\EM\n\
      \\bbuild_id\CAN\ACK \SOH(\ETXR\abuildId\DC2\\\n\
      \\rbuild_id_kind\CAN\v \SOH(\SO28.opentelemetry.proto.profiles.v1experimental.BuildIdKindR\vbuildIdKind\DC2\RS\n\
      \\n\
      \attributes\CAN\f \ETX(\EOTR\n\
      \attributes\DC2#\n\
      \\rhas_functions\CAN\a \SOH(\bR\fhasFunctions\DC2#\n\
      \\rhas_filenames\CAN\b \SOH(\bR\fhasFilenames\DC2(\n\
      \\DLEhas_line_numbers\CAN\t \SOH(\bR\SOhasLineNumbers\DC2*\n\
      \\DC1has_inline_frames\CAN\n\
      \ \SOH(\bR\SIhasInlineFrames"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        id__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"id")) ::
              Data.ProtoLens.FieldDescriptor Mapping
        memoryStart__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "memory_start"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"memoryStart")) ::
              Data.ProtoLens.FieldDescriptor Mapping
        memoryLimit__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "memory_limit"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"memoryLimit")) ::
              Data.ProtoLens.FieldDescriptor Mapping
        fileOffset__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "file_offset"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"fileOffset")) ::
              Data.ProtoLens.FieldDescriptor Mapping
        filename__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "filename"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"filename")) ::
              Data.ProtoLens.FieldDescriptor Mapping
        buildId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "build_id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"buildId")) ::
              Data.ProtoLens.FieldDescriptor Mapping
        buildIdKind__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "build_id_kind"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor BuildIdKind)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"buildIdKind")) ::
              Data.ProtoLens.FieldDescriptor Mapping
        attributes__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "attributes"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"attributes")) ::
              Data.ProtoLens.FieldDescriptor Mapping
        hasFunctions__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "has_functions"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"hasFunctions")) ::
              Data.ProtoLens.FieldDescriptor Mapping
        hasFilenames__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "has_filenames"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"hasFilenames")) ::
              Data.ProtoLens.FieldDescriptor Mapping
        hasLineNumbers__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "has_line_numbers"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"hasLineNumbers")) ::
              Data.ProtoLens.FieldDescriptor Mapping
        hasInlineFrames__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "has_inline_frames"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BoolField ::
                 Data.ProtoLens.FieldTypeDescriptor Prelude.Bool)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"hasInlineFrames")) ::
              Data.ProtoLens.FieldDescriptor Mapping
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, id__field_descriptor),
           (Data.ProtoLens.Tag 2, memoryStart__field_descriptor),
           (Data.ProtoLens.Tag 3, memoryLimit__field_descriptor),
           (Data.ProtoLens.Tag 4, fileOffset__field_descriptor),
           (Data.ProtoLens.Tag 5, filename__field_descriptor),
           (Data.ProtoLens.Tag 6, buildId__field_descriptor),
           (Data.ProtoLens.Tag 11, buildIdKind__field_descriptor),
           (Data.ProtoLens.Tag 12, attributes__field_descriptor),
           (Data.ProtoLens.Tag 7, hasFunctions__field_descriptor),
           (Data.ProtoLens.Tag 8, hasFilenames__field_descriptor),
           (Data.ProtoLens.Tag 9, hasLineNumbers__field_descriptor),
           (Data.ProtoLens.Tag 10, hasInlineFrames__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Mapping'_unknownFields
        (\ x__ y__ -> x__ {_Mapping'_unknownFields = y__})
  defMessage
    = Mapping'_constructor
        {_Mapping'id = Data.ProtoLens.fieldDefault,
         _Mapping'memoryStart = Data.ProtoLens.fieldDefault,
         _Mapping'memoryLimit = Data.ProtoLens.fieldDefault,
         _Mapping'fileOffset = Data.ProtoLens.fieldDefault,
         _Mapping'filename = Data.ProtoLens.fieldDefault,
         _Mapping'buildId = Data.ProtoLens.fieldDefault,
         _Mapping'buildIdKind = Data.ProtoLens.fieldDefault,
         _Mapping'attributes = Data.Vector.Generic.empty,
         _Mapping'hasFunctions = Data.ProtoLens.fieldDefault,
         _Mapping'hasFilenames = Data.ProtoLens.fieldDefault,
         _Mapping'hasLineNumbers = Data.ProtoLens.fieldDefault,
         _Mapping'hasInlineFrames = Data.ProtoLens.fieldDefault,
         _Mapping'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Mapping
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Word.Word64
             -> Data.ProtoLens.Encoding.Bytes.Parser Mapping
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
                        8 -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"id") y x)
                                  mutable'attributes
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "memory_start"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"memoryStart") y x)
                                  mutable'attributes
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "memory_limit"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"memoryLimit") y x)
                                  mutable'attributes
                        32
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "file_offset"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"fileOffset") y x)
                                  mutable'attributes
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "filename"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"filename") y x)
                                  mutable'attributes
                        48
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "build_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"buildId") y x)
                                  mutable'attributes
                        88
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "build_id_kind"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"buildIdKind") y x)
                                  mutable'attributes
                        96
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        Data.ProtoLens.Encoding.Bytes.getVarInt "attributes"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'attributes y)
                                loop x v
                        98
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                    "attributes"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'attributes)
                                loop x y
                        56
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "has_functions"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"hasFunctions") y x)
                                  mutable'attributes
                        64
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "has_filenames"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"hasFilenames") y x)
                                  mutable'attributes
                        72
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "has_line_numbers"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"hasLineNumbers") y x)
                                  mutable'attributes
                        80
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          ((Prelude./=) 0) Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "has_inline_frames"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"hasInlineFrames") y x)
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
          "Mapping"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"id") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view (Data.ProtoLens.Field.field @"memoryStart") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view (Data.ProtoLens.Field.field @"memoryLimit") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                   ((Data.Monoid.<>)
                      (let
                         _v
                           = Lens.Family2.view (Data.ProtoLens.Field.field @"fileOffset") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 32)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                      ((Data.Monoid.<>)
                         (let
                            _v = Lens.Family2.view (Data.ProtoLens.Field.field @"filename") _x
                          in
                            if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral
                                     _v))
                         ((Data.Monoid.<>)
                            (let
                               _v = Lens.Family2.view (Data.ProtoLens.Field.field @"buildId") _x
                             in
                               if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                   Data.Monoid.mempty
                               else
                                   (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt 48)
                                     ((Prelude..)
                                        Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral
                                        _v))
                            ((Data.Monoid.<>)
                               (let
                                  _v
                                    = Lens.Family2.view
                                        (Data.ProtoLens.Field.field @"buildIdKind") _x
                                in
                                  if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                      Data.Monoid.mempty
                                  else
                                      (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt 88)
                                        ((Prelude..)
                                           ((Prelude..)
                                              Data.ProtoLens.Encoding.Bytes.putVarInt
                                              Prelude.fromIntegral)
                                           Prelude.fromEnum _v))
                               ((Data.Monoid.<>)
                                  (let
                                     p = Lens.Family2.view
                                           (Data.ProtoLens.Field.field @"vec'attributes") _x
                                   in
                                     if Data.Vector.Generic.null p then
                                         Data.Monoid.mempty
                                     else
                                         (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt 98)
                                           ((\ bs
                                               -> (Data.Monoid.<>)
                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                       (Prelude.fromIntegral
                                                          (Data.ByteString.length bs)))
                                                    (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                              (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                 (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                    Data.ProtoLens.Encoding.Bytes.putVarInt p))))
                                  ((Data.Monoid.<>)
                                     (let
                                        _v
                                          = Lens.Family2.view
                                              (Data.ProtoLens.Field.field @"hasFunctions") _x
                                      in
                                        if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                            Data.Monoid.mempty
                                        else
                                            (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt 56)
                                              ((Prelude..)
                                                 Data.ProtoLens.Encoding.Bytes.putVarInt
                                                 (\ b -> if b then 1 else 0) _v))
                                     ((Data.Monoid.<>)
                                        (let
                                           _v
                                             = Lens.Family2.view
                                                 (Data.ProtoLens.Field.field @"hasFilenames") _x
                                         in
                                           if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                               Data.Monoid.mempty
                                           else
                                               (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 64)
                                                 ((Prelude..)
                                                    Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    (\ b -> if b then 1 else 0) _v))
                                        ((Data.Monoid.<>)
                                           (let
                                              _v
                                                = Lens.Family2.view
                                                    (Data.ProtoLens.Field.field @"hasLineNumbers")
                                                    _x
                                            in
                                              if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                                  Data.Monoid.mempty
                                              else
                                                  (Data.Monoid.<>)
                                                    (Data.ProtoLens.Encoding.Bytes.putVarInt 72)
                                                    ((Prelude..)
                                                       Data.ProtoLens.Encoding.Bytes.putVarInt
                                                       (\ b -> if b then 1 else 0) _v))
                                           ((Data.Monoid.<>)
                                              (let
                                                 _v
                                                   = Lens.Family2.view
                                                       (Data.ProtoLens.Field.field
                                                          @"hasInlineFrames")
                                                       _x
                                               in
                                                 if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                                     Data.Monoid.mempty
                                                 else
                                                     (Data.Monoid.<>)
                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt 80)
                                                       ((Prelude..)
                                                          Data.ProtoLens.Encoding.Bytes.putVarInt
                                                          (\ b -> if b then 1 else 0) _v))
                                              (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                                 (Lens.Family2.view
                                                    Data.ProtoLens.unknownFields _x)))))))))))))
instance Control.DeepSeq.NFData Mapping where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Mapping'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Mapping'id x__)
                (Control.DeepSeq.deepseq
                   (_Mapping'memoryStart x__)
                   (Control.DeepSeq.deepseq
                      (_Mapping'memoryLimit x__)
                      (Control.DeepSeq.deepseq
                         (_Mapping'fileOffset x__)
                         (Control.DeepSeq.deepseq
                            (_Mapping'filename x__)
                            (Control.DeepSeq.deepseq
                               (_Mapping'buildId x__)
                               (Control.DeepSeq.deepseq
                                  (_Mapping'buildIdKind x__)
                                  (Control.DeepSeq.deepseq
                                     (_Mapping'attributes x__)
                                     (Control.DeepSeq.deepseq
                                        (_Mapping'hasFunctions x__)
                                        (Control.DeepSeq.deepseq
                                           (_Mapping'hasFilenames x__)
                                           (Control.DeepSeq.deepseq
                                              (_Mapping'hasLineNumbers x__)
                                              (Control.DeepSeq.deepseq
                                                 (_Mapping'hasInlineFrames x__) ()))))))))))))
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.sampleType' @:: Lens' Profile [ValueType]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'sampleType' @:: Lens' Profile (Data.Vector.Vector ValueType)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.sample' @:: Lens' Profile [Sample]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'sample' @:: Lens' Profile (Data.Vector.Vector Sample)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.mapping' @:: Lens' Profile [Mapping]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'mapping' @:: Lens' Profile (Data.Vector.Vector Mapping)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.location' @:: Lens' Profile [Location]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'location' @:: Lens' Profile (Data.Vector.Vector Location)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.locationIndices' @:: Lens' Profile [Data.Int.Int64]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'locationIndices' @:: Lens' Profile (Data.Vector.Unboxed.Vector Data.Int.Int64)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.function' @:: Lens' Profile [Function]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'function' @:: Lens' Profile (Data.Vector.Vector Function)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.attributeTable' @:: Lens' Profile [Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'attributeTable' @:: Lens' Profile (Data.Vector.Vector Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.attributeUnits' @:: Lens' Profile [AttributeUnit]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'attributeUnits' @:: Lens' Profile (Data.Vector.Vector AttributeUnit)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.linkTable' @:: Lens' Profile [Link]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'linkTable' @:: Lens' Profile (Data.Vector.Vector Link)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.stringTable' @:: Lens' Profile [Data.Text.Text]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'stringTable' @:: Lens' Profile (Data.Vector.Vector Data.Text.Text)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.dropFrames' @:: Lens' Profile Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.keepFrames' @:: Lens' Profile Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.timeNanos' @:: Lens' Profile Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.durationNanos' @:: Lens' Profile Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.periodType' @:: Lens' Profile ValueType@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.maybe'periodType' @:: Lens' Profile (Prelude.Maybe ValueType)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.period' @:: Lens' Profile Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.comment' @:: Lens' Profile [Data.Int.Int64]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'comment' @:: Lens' Profile (Data.Vector.Unboxed.Vector Data.Int.Int64)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.defaultSampleType' @:: Lens' Profile Data.Int.Int64@ -}
data Profile
  = Profile'_constructor {_Profile'sampleType :: !(Data.Vector.Vector ValueType),
                          _Profile'sample :: !(Data.Vector.Vector Sample),
                          _Profile'mapping :: !(Data.Vector.Vector Mapping),
                          _Profile'location :: !(Data.Vector.Vector Location),
                          _Profile'locationIndices :: !(Data.Vector.Unboxed.Vector Data.Int.Int64),
                          _Profile'function :: !(Data.Vector.Vector Function),
                          _Profile'attributeTable :: !(Data.Vector.Vector Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue),
                          _Profile'attributeUnits :: !(Data.Vector.Vector AttributeUnit),
                          _Profile'linkTable :: !(Data.Vector.Vector Link),
                          _Profile'stringTable :: !(Data.Vector.Vector Data.Text.Text),
                          _Profile'dropFrames :: !Data.Int.Int64,
                          _Profile'keepFrames :: !Data.Int.Int64,
                          _Profile'timeNanos :: !Data.Int.Int64,
                          _Profile'durationNanos :: !Data.Int.Int64,
                          _Profile'periodType :: !(Prelude.Maybe ValueType),
                          _Profile'period :: !Data.Int.Int64,
                          _Profile'comment :: !(Data.Vector.Unboxed.Vector Data.Int.Int64),
                          _Profile'defaultSampleType :: !Data.Int.Int64,
                          _Profile'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Profile where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Profile "sampleType" [ValueType] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'sampleType (\ x__ y__ -> x__ {_Profile'sampleType = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Profile "vec'sampleType" (Data.Vector.Vector ValueType) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'sampleType (\ x__ y__ -> x__ {_Profile'sampleType = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "sample" [Sample] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'sample (\ x__ y__ -> x__ {_Profile'sample = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Profile "vec'sample" (Data.Vector.Vector Sample) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'sample (\ x__ y__ -> x__ {_Profile'sample = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "mapping" [Mapping] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'mapping (\ x__ y__ -> x__ {_Profile'mapping = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Profile "vec'mapping" (Data.Vector.Vector Mapping) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'mapping (\ x__ y__ -> x__ {_Profile'mapping = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "location" [Location] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'location (\ x__ y__ -> x__ {_Profile'location = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Profile "vec'location" (Data.Vector.Vector Location) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'location (\ x__ y__ -> x__ {_Profile'location = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "locationIndices" [Data.Int.Int64] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'locationIndices
           (\ x__ y__ -> x__ {_Profile'locationIndices = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Profile "vec'locationIndices" (Data.Vector.Unboxed.Vector Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'locationIndices
           (\ x__ y__ -> x__ {_Profile'locationIndices = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "function" [Function] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'function (\ x__ y__ -> x__ {_Profile'function = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Profile "vec'function" (Data.Vector.Vector Function) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'function (\ x__ y__ -> x__ {_Profile'function = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "attributeTable" [Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'attributeTable
           (\ x__ y__ -> x__ {_Profile'attributeTable = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Profile "vec'attributeTable" (Data.Vector.Vector Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'attributeTable
           (\ x__ y__ -> x__ {_Profile'attributeTable = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "attributeUnits" [AttributeUnit] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'attributeUnits
           (\ x__ y__ -> x__ {_Profile'attributeUnits = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Profile "vec'attributeUnits" (Data.Vector.Vector AttributeUnit) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'attributeUnits
           (\ x__ y__ -> x__ {_Profile'attributeUnits = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "linkTable" [Link] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'linkTable (\ x__ y__ -> x__ {_Profile'linkTable = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Profile "vec'linkTable" (Data.Vector.Vector Link) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'linkTable (\ x__ y__ -> x__ {_Profile'linkTable = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "stringTable" [Data.Text.Text] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'stringTable
           (\ x__ y__ -> x__ {_Profile'stringTable = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Profile "vec'stringTable" (Data.Vector.Vector Data.Text.Text) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'stringTable
           (\ x__ y__ -> x__ {_Profile'stringTable = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "dropFrames" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'dropFrames (\ x__ y__ -> x__ {_Profile'dropFrames = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "keepFrames" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'keepFrames (\ x__ y__ -> x__ {_Profile'keepFrames = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "timeNanos" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'timeNanos (\ x__ y__ -> x__ {_Profile'timeNanos = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "durationNanos" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'durationNanos
           (\ x__ y__ -> x__ {_Profile'durationNanos = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "periodType" ValueType where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'periodType (\ x__ y__ -> x__ {_Profile'periodType = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField Profile "maybe'periodType" (Prelude.Maybe ValueType) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'periodType (\ x__ y__ -> x__ {_Profile'periodType = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "period" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'period (\ x__ y__ -> x__ {_Profile'period = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "comment" [Data.Int.Int64] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'comment (\ x__ y__ -> x__ {_Profile'comment = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Profile "vec'comment" (Data.Vector.Unboxed.Vector Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'comment (\ x__ y__ -> x__ {_Profile'comment = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Profile "defaultSampleType" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Profile'defaultSampleType
           (\ x__ y__ -> x__ {_Profile'defaultSampleType = y__}))
        Prelude.id
instance Data.ProtoLens.Message Profile where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.profiles.v1experimental.Profile"
  packedMessageDescriptor _
    = "\n\
      \\aProfile\DC2W\n\
      \\vsample_type\CAN\SOH \ETX(\v26.opentelemetry.proto.profiles.v1experimental.ValueTypeR\n\
      \sampleType\DC2K\n\
      \\ACKsample\CAN\STX \ETX(\v23.opentelemetry.proto.profiles.v1experimental.SampleR\ACKsample\DC2N\n\
      \\amapping\CAN\ETX \ETX(\v24.opentelemetry.proto.profiles.v1experimental.MappingR\amapping\DC2Q\n\
      \\blocation\CAN\EOT \ETX(\v25.opentelemetry.proto.profiles.v1experimental.LocationR\blocation\DC2)\n\
      \\DLElocation_indices\CAN\SI \ETX(\ETXR\SIlocationIndices\DC2Q\n\
      \\bfunction\CAN\ENQ \ETX(\v25.opentelemetry.proto.profiles.v1experimental.FunctionR\bfunction\DC2P\n\
      \\SIattribute_table\CAN\DLE \ETX(\v2'.opentelemetry.proto.common.v1.KeyValueR\SOattributeTable\DC2c\n\
      \\SIattribute_units\CAN\DC1 \ETX(\v2:.opentelemetry.proto.profiles.v1experimental.AttributeUnitR\SOattributeUnits\DC2P\n\
      \\n\
      \link_table\CAN\DC2 \ETX(\v21.opentelemetry.proto.profiles.v1experimental.LinkR\tlinkTable\DC2!\n\
      \\fstring_table\CAN\ACK \ETX(\tR\vstringTable\DC2\US\n\
      \\vdrop_frames\CAN\a \SOH(\ETXR\n\
      \dropFrames\DC2\US\n\
      \\vkeep_frames\CAN\b \SOH(\ETXR\n\
      \keepFrames\DC2\GS\n\
      \\n\
      \time_nanos\CAN\t \SOH(\ETXR\ttimeNanos\DC2%\n\
      \\SOduration_nanos\CAN\n\
      \ \SOH(\ETXR\rdurationNanos\DC2W\n\
      \\vperiod_type\CAN\v \SOH(\v26.opentelemetry.proto.profiles.v1experimental.ValueTypeR\n\
      \periodType\DC2\SYN\n\
      \\ACKperiod\CAN\f \SOH(\ETXR\ACKperiod\DC2\CAN\n\
      \\acomment\CAN\r \ETX(\ETXR\acomment\DC2.\n\
      \\DC3default_sample_type\CAN\SO \SOH(\ETXR\DC1defaultSampleType"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        sampleType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "sample_type"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ValueType)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"sampleType")) ::
              Data.ProtoLens.FieldDescriptor Profile
        sample__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "sample"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Sample)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"sample")) ::
              Data.ProtoLens.FieldDescriptor Profile
        mapping__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "mapping"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Mapping)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"mapping")) ::
              Data.ProtoLens.FieldDescriptor Profile
        location__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "location"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Location)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"location")) ::
              Data.ProtoLens.FieldDescriptor Profile
        locationIndices__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "location_indices"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"locationIndices")) ::
              Data.ProtoLens.FieldDescriptor Profile
        function__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "function"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Function)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"function")) ::
              Data.ProtoLens.FieldDescriptor Profile
        attributeTable__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "attribute_table"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"attributeTable")) ::
              Data.ProtoLens.FieldDescriptor Profile
        attributeUnits__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "attribute_units"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor AttributeUnit)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"attributeUnits")) ::
              Data.ProtoLens.FieldDescriptor Profile
        linkTable__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "link_table"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Link)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"linkTable")) ::
              Data.ProtoLens.FieldDescriptor Profile
        stringTable__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "string_table"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"stringTable")) ::
              Data.ProtoLens.FieldDescriptor Profile
        dropFrames__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "drop_frames"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"dropFrames")) ::
              Data.ProtoLens.FieldDescriptor Profile
        keepFrames__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "keep_frames"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"keepFrames")) ::
              Data.ProtoLens.FieldDescriptor Profile
        timeNanos__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "time_nanos"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"timeNanos")) ::
              Data.ProtoLens.FieldDescriptor Profile
        durationNanos__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "duration_nanos"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"durationNanos")) ::
              Data.ProtoLens.FieldDescriptor Profile
        periodType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "period_type"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ValueType)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'periodType")) ::
              Data.ProtoLens.FieldDescriptor Profile
        period__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "period"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"period")) ::
              Data.ProtoLens.FieldDescriptor Profile
        comment__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "comment"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed (Data.ProtoLens.Field.field @"comment")) ::
              Data.ProtoLens.FieldDescriptor Profile
        defaultSampleType__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "default_sample_type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"defaultSampleType")) ::
              Data.ProtoLens.FieldDescriptor Profile
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, sampleType__field_descriptor),
           (Data.ProtoLens.Tag 2, sample__field_descriptor),
           (Data.ProtoLens.Tag 3, mapping__field_descriptor),
           (Data.ProtoLens.Tag 4, location__field_descriptor),
           (Data.ProtoLens.Tag 15, locationIndices__field_descriptor),
           (Data.ProtoLens.Tag 5, function__field_descriptor),
           (Data.ProtoLens.Tag 16, attributeTable__field_descriptor),
           (Data.ProtoLens.Tag 17, attributeUnits__field_descriptor),
           (Data.ProtoLens.Tag 18, linkTable__field_descriptor),
           (Data.ProtoLens.Tag 6, stringTable__field_descriptor),
           (Data.ProtoLens.Tag 7, dropFrames__field_descriptor),
           (Data.ProtoLens.Tag 8, keepFrames__field_descriptor),
           (Data.ProtoLens.Tag 9, timeNanos__field_descriptor),
           (Data.ProtoLens.Tag 10, durationNanos__field_descriptor),
           (Data.ProtoLens.Tag 11, periodType__field_descriptor),
           (Data.ProtoLens.Tag 12, period__field_descriptor),
           (Data.ProtoLens.Tag 13, comment__field_descriptor),
           (Data.ProtoLens.Tag 14, defaultSampleType__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Profile'_unknownFields
        (\ x__ y__ -> x__ {_Profile'_unknownFields = y__})
  defMessage
    = Profile'_constructor
        {_Profile'sampleType = Data.Vector.Generic.empty,
         _Profile'sample = Data.Vector.Generic.empty,
         _Profile'mapping = Data.Vector.Generic.empty,
         _Profile'location = Data.Vector.Generic.empty,
         _Profile'locationIndices = Data.Vector.Generic.empty,
         _Profile'function = Data.Vector.Generic.empty,
         _Profile'attributeTable = Data.Vector.Generic.empty,
         _Profile'attributeUnits = Data.Vector.Generic.empty,
         _Profile'linkTable = Data.Vector.Generic.empty,
         _Profile'stringTable = Data.Vector.Generic.empty,
         _Profile'dropFrames = Data.ProtoLens.fieldDefault,
         _Profile'keepFrames = Data.ProtoLens.fieldDefault,
         _Profile'timeNanos = Data.ProtoLens.fieldDefault,
         _Profile'durationNanos = Data.ProtoLens.fieldDefault,
         _Profile'periodType = Prelude.Nothing,
         _Profile'period = Data.ProtoLens.fieldDefault,
         _Profile'comment = Data.Vector.Generic.empty,
         _Profile'defaultSampleType = Data.ProtoLens.fieldDefault,
         _Profile'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Profile
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld AttributeUnit
                -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Int.Int64
                   -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Function
                      -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Link
                         -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Location
                            -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Int.Int64
                               -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Mapping
                                  -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Sample
                                     -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ValueType
                                        -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Text.Text
                                           -> Data.ProtoLens.Encoding.Bytes.Parser Profile
        loop
          x
          mutable'attributeTable
          mutable'attributeUnits
          mutable'comment
          mutable'function
          mutable'linkTable
          mutable'location
          mutable'locationIndices
          mutable'mapping
          mutable'sample
          mutable'sampleType
          mutable'stringTable
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'attributeTable <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                 (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                    mutable'attributeTable)
                      frozen'attributeUnits <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                 (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                    mutable'attributeUnits)
                      frozen'comment <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                             mutable'comment)
                      frozen'function <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                           (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                              mutable'function)
                      frozen'linkTable <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                               mutable'linkTable)
                      frozen'location <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                           (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                              mutable'location)
                      frozen'locationIndices <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                  (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                     mutable'locationIndices)
                      frozen'mapping <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                             mutable'mapping)
                      frozen'sample <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                            mutable'sample)
                      frozen'sampleType <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                             (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                mutable'sampleType)
                      frozen'stringTable <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                              (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                 mutable'stringTable)
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
                              (Data.ProtoLens.Field.field @"vec'attributeTable")
                              frozen'attributeTable
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'attributeUnits")
                                 frozen'attributeUnits
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"vec'comment") frozen'comment
                                    (Lens.Family2.set
                                       (Data.ProtoLens.Field.field @"vec'function") frozen'function
                                       (Lens.Family2.set
                                          (Data.ProtoLens.Field.field @"vec'linkTable")
                                          frozen'linkTable
                                          (Lens.Family2.set
                                             (Data.ProtoLens.Field.field @"vec'location")
                                             frozen'location
                                             (Lens.Family2.set
                                                (Data.ProtoLens.Field.field @"vec'locationIndices")
                                                frozen'locationIndices
                                                (Lens.Family2.set
                                                   (Data.ProtoLens.Field.field @"vec'mapping")
                                                   frozen'mapping
                                                   (Lens.Family2.set
                                                      (Data.ProtoLens.Field.field @"vec'sample")
                                                      frozen'sample
                                                      (Lens.Family2.set
                                                         (Data.ProtoLens.Field.field
                                                            @"vec'sampleType")
                                                         frozen'sampleType
                                                         (Lens.Family2.set
                                                            (Data.ProtoLens.Field.field
                                                               @"vec'stringTable")
                                                            frozen'stringTable x))))))))))))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "sample_type"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'sampleType y)
                                loop
                                  x mutable'attributeTable mutable'attributeUnits mutable'comment
                                  mutable'function mutable'linkTable mutable'location
                                  mutable'locationIndices mutable'mapping mutable'sample v
                                  mutable'stringTable
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "sample"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'sample y)
                                loop
                                  x mutable'attributeTable mutable'attributeUnits mutable'comment
                                  mutable'function mutable'linkTable mutable'location
                                  mutable'locationIndices mutable'mapping v mutable'sampleType
                                  mutable'stringTable
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "mapping"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'mapping y)
                                loop
                                  x mutable'attributeTable mutable'attributeUnits mutable'comment
                                  mutable'function mutable'linkTable mutable'location
                                  mutable'locationIndices v mutable'sample mutable'sampleType
                                  mutable'stringTable
                        34
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "location"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'location y)
                                loop
                                  x mutable'attributeTable mutable'attributeUnits mutable'comment
                                  mutable'function mutable'linkTable v mutable'locationIndices
                                  mutable'mapping mutable'sample mutable'sampleType
                                  mutable'stringTable
                        120
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.fromIntegral
                                           Data.ProtoLens.Encoding.Bytes.getVarInt)
                                        "location_indices"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'locationIndices y)
                                loop
                                  x mutable'attributeTable mutable'attributeUnits mutable'comment
                                  mutable'function mutable'linkTable mutable'location v
                                  mutable'mapping mutable'sample mutable'sampleType
                                  mutable'stringTable
                        122
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Prelude.fromIntegral
                                                                       Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                                    "location_indices"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'locationIndices)
                                loop
                                  x mutable'attributeTable mutable'attributeUnits mutable'comment
                                  mutable'function mutable'linkTable mutable'location y
                                  mutable'mapping mutable'sample mutable'sampleType
                                  mutable'stringTable
                        42
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "function"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'function y)
                                loop
                                  x mutable'attributeTable mutable'attributeUnits mutable'comment v
                                  mutable'linkTable mutable'location mutable'locationIndices
                                  mutable'mapping mutable'sample mutable'sampleType
                                  mutable'stringTable
                        130
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "attribute_table"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'attributeTable y)
                                loop
                                  x v mutable'attributeUnits mutable'comment mutable'function
                                  mutable'linkTable mutable'location mutable'locationIndices
                                  mutable'mapping mutable'sample mutable'sampleType
                                  mutable'stringTable
                        138
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "attribute_units"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'attributeUnits y)
                                loop
                                  x mutable'attributeTable v mutable'comment mutable'function
                                  mutable'linkTable mutable'location mutable'locationIndices
                                  mutable'mapping mutable'sample mutable'sampleType
                                  mutable'stringTable
                        146
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "link_table"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'linkTable y)
                                loop
                                  x mutable'attributeTable mutable'attributeUnits mutable'comment
                                  mutable'function v mutable'location mutable'locationIndices
                                  mutable'mapping mutable'sample mutable'sampleType
                                  mutable'stringTable
                        50
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.getText
                                              (Prelude.fromIntegral len))
                                        "string_table"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'stringTable y)
                                loop
                                  x mutable'attributeTable mutable'attributeUnits mutable'comment
                                  mutable'function mutable'linkTable mutable'location
                                  mutable'locationIndices mutable'mapping mutable'sample
                                  mutable'sampleType v
                        56
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "drop_frames"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"dropFrames") y x)
                                  mutable'attributeTable mutable'attributeUnits mutable'comment
                                  mutable'function mutable'linkTable mutable'location
                                  mutable'locationIndices mutable'mapping mutable'sample
                                  mutable'sampleType mutable'stringTable
                        64
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "keep_frames"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"keepFrames") y x)
                                  mutable'attributeTable mutable'attributeUnits mutable'comment
                                  mutable'function mutable'linkTable mutable'location
                                  mutable'locationIndices mutable'mapping mutable'sample
                                  mutable'sampleType mutable'stringTable
                        72
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "time_nanos"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"timeNanos") y x)
                                  mutable'attributeTable mutable'attributeUnits mutable'comment
                                  mutable'function mutable'linkTable mutable'location
                                  mutable'locationIndices mutable'mapping mutable'sample
                                  mutable'sampleType mutable'stringTable
                        80
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "duration_nanos"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"durationNanos") y x)
                                  mutable'attributeTable mutable'attributeUnits mutable'comment
                                  mutable'function mutable'linkTable mutable'location
                                  mutable'locationIndices mutable'mapping mutable'sample
                                  mutable'sampleType mutable'stringTable
                        90
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "period_type"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"periodType") y x)
                                  mutable'attributeTable mutable'attributeUnits mutable'comment
                                  mutable'function mutable'linkTable mutable'location
                                  mutable'locationIndices mutable'mapping mutable'sample
                                  mutable'sampleType mutable'stringTable
                        96
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "period"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"period") y x)
                                  mutable'attributeTable mutable'attributeUnits mutable'comment
                                  mutable'function mutable'linkTable mutable'location
                                  mutable'locationIndices mutable'mapping mutable'sample
                                  mutable'sampleType mutable'stringTable
                        104
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.fromIntegral
                                           Data.ProtoLens.Encoding.Bytes.getVarInt)
                                        "comment"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'comment y)
                                loop
                                  x mutable'attributeTable mutable'attributeUnits v mutable'function
                                  mutable'linkTable mutable'location mutable'locationIndices
                                  mutable'mapping mutable'sample mutable'sampleType
                                  mutable'stringTable
                        106
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Prelude.fromIntegral
                                                                       Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                                    "comment"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'comment)
                                loop
                                  x mutable'attributeTable mutable'attributeUnits y mutable'function
                                  mutable'linkTable mutable'location mutable'locationIndices
                                  mutable'mapping mutable'sample mutable'sampleType
                                  mutable'stringTable
                        112
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "default_sample_type"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"defaultSampleType") y x)
                                  mutable'attributeTable mutable'attributeUnits mutable'comment
                                  mutable'function mutable'linkTable mutable'location
                                  mutable'locationIndices mutable'mapping mutable'sample
                                  mutable'sampleType mutable'stringTable
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'attributeTable mutable'attributeUnits mutable'comment
                                  mutable'function mutable'linkTable mutable'location
                                  mutable'locationIndices mutable'mapping mutable'sample
                                  mutable'sampleType mutable'stringTable
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'attributeTable <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          Data.ProtoLens.Encoding.Growing.new
              mutable'attributeUnits <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                          Data.ProtoLens.Encoding.Growing.new
              mutable'comment <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                   Data.ProtoLens.Encoding.Growing.new
              mutable'function <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                    Data.ProtoLens.Encoding.Growing.new
              mutable'linkTable <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                     Data.ProtoLens.Encoding.Growing.new
              mutable'location <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                    Data.ProtoLens.Encoding.Growing.new
              mutable'locationIndices <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                           Data.ProtoLens.Encoding.Growing.new
              mutable'mapping <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                   Data.ProtoLens.Encoding.Growing.new
              mutable'sample <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                  Data.ProtoLens.Encoding.Growing.new
              mutable'sampleType <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      Data.ProtoLens.Encoding.Growing.new
              mutable'stringTable <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       Data.ProtoLens.Encoding.Growing.new
              loop
                Data.ProtoLens.defMessage mutable'attributeTable
                mutable'attributeUnits mutable'comment mutable'function
                mutable'linkTable mutable'location mutable'locationIndices
                mutable'mapping mutable'sample mutable'sampleType
                mutable'stringTable)
          "Profile"
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
                   (Data.ProtoLens.Field.field @"vec'sampleType") _x))
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
                   (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'sample") _x))
                ((Data.Monoid.<>)
                   (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                      (\ _v
                         -> (Data.Monoid.<>)
                              (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                              ((Prelude..)
                                 (\ bs
                                    -> (Data.Monoid.<>)
                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                            (Prelude.fromIntegral (Data.ByteString.length bs)))
                                         (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                 Data.ProtoLens.encodeMessage _v))
                      (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'mapping") _x))
                   ((Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                         (\ _v
                            -> (Data.Monoid.<>)
                                 (Data.ProtoLens.Encoding.Bytes.putVarInt 34)
                                 ((Prelude..)
                                    (\ bs
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                               (Prelude.fromIntegral (Data.ByteString.length bs)))
                                            (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                    Data.ProtoLens.encodeMessage _v))
                         (Lens.Family2.view
                            (Data.ProtoLens.Field.field @"vec'location") _x))
                      ((Data.Monoid.<>)
                         (let
                            p = Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"vec'locationIndices") _x
                          in
                            if Data.Vector.Generic.null p then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 122)
                                  ((\ bs
                                      -> (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                                           (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                     (Data.ProtoLens.Encoding.Bytes.runBuilder
                                        (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                           ((Prelude..)
                                              Data.ProtoLens.Encoding.Bytes.putVarInt
                                              Prelude.fromIntegral)
                                           p))))
                         ((Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                               (\ _v
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt 42)
                                       ((Prelude..)
                                          (\ bs
                                             -> (Data.Monoid.<>)
                                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                     (Prelude.fromIntegral
                                                        (Data.ByteString.length bs)))
                                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                          Data.ProtoLens.encodeMessage _v))
                               (Lens.Family2.view
                                  (Data.ProtoLens.Field.field @"vec'function") _x))
                            ((Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                  (\ _v
                                     -> (Data.Monoid.<>)
                                          (Data.ProtoLens.Encoding.Bytes.putVarInt 130)
                                          ((Prelude..)
                                             (\ bs
                                                -> (Data.Monoid.<>)
                                                     (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                        (Prelude.fromIntegral
                                                           (Data.ByteString.length bs)))
                                                     (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                             Data.ProtoLens.encodeMessage _v))
                                  (Lens.Family2.view
                                     (Data.ProtoLens.Field.field @"vec'attributeTable") _x))
                               ((Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                     (\ _v
                                        -> (Data.Monoid.<>)
                                             (Data.ProtoLens.Encoding.Bytes.putVarInt 138)
                                             ((Prelude..)
                                                (\ bs
                                                   -> (Data.Monoid.<>)
                                                        (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                           (Prelude.fromIntegral
                                                              (Data.ByteString.length bs)))
                                                        (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                                Data.ProtoLens.encodeMessage _v))
                                     (Lens.Family2.view
                                        (Data.ProtoLens.Field.field @"vec'attributeUnits") _x))
                                  ((Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                        (\ _v
                                           -> (Data.Monoid.<>)
                                                (Data.ProtoLens.Encoding.Bytes.putVarInt 146)
                                                ((Prelude..)
                                                   (\ bs
                                                      -> (Data.Monoid.<>)
                                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                              (Prelude.fromIntegral
                                                                 (Data.ByteString.length bs)))
                                                           (Data.ProtoLens.Encoding.Bytes.putBytes
                                                              bs))
                                                   Data.ProtoLens.encodeMessage _v))
                                        (Lens.Family2.view
                                           (Data.ProtoLens.Field.field @"vec'linkTable") _x))
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
                                                              (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                 bs))
                                                      Data.Text.Encoding.encodeUtf8 _v))
                                           (Lens.Family2.view
                                              (Data.ProtoLens.Field.field @"vec'stringTable") _x))
                                        ((Data.Monoid.<>)
                                           (let
                                              _v
                                                = Lens.Family2.view
                                                    (Data.ProtoLens.Field.field @"dropFrames") _x
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
                                                 _v
                                                   = Lens.Family2.view
                                                       (Data.ProtoLens.Field.field @"keepFrames") _x
                                               in
                                                 if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                                     Data.Monoid.mempty
                                                 else
                                                     (Data.Monoid.<>)
                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt 64)
                                                       ((Prelude..)
                                                          Data.ProtoLens.Encoding.Bytes.putVarInt
                                                          Prelude.fromIntegral _v))
                                              ((Data.Monoid.<>)
                                                 (let
                                                    _v
                                                      = Lens.Family2.view
                                                          (Data.ProtoLens.Field.field @"timeNanos")
                                                          _x
                                                  in
                                                    if (Prelude.==)
                                                         _v Data.ProtoLens.fieldDefault then
                                                        Data.Monoid.mempty
                                                    else
                                                        (Data.Monoid.<>)
                                                          (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                             72)
                                                          ((Prelude..)
                                                             Data.ProtoLens.Encoding.Bytes.putVarInt
                                                             Prelude.fromIntegral _v))
                                                 ((Data.Monoid.<>)
                                                    (let
                                                       _v
                                                         = Lens.Family2.view
                                                             (Data.ProtoLens.Field.field
                                                                @"durationNanos")
                                                             _x
                                                     in
                                                       if (Prelude.==)
                                                            _v Data.ProtoLens.fieldDefault then
                                                           Data.Monoid.mempty
                                                       else
                                                           (Data.Monoid.<>)
                                                             (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                80)
                                                             ((Prelude..)
                                                                Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                Prelude.fromIntegral _v))
                                                    ((Data.Monoid.<>)
                                                       (case
                                                            Lens.Family2.view
                                                              (Data.ProtoLens.Field.field
                                                                 @"maybe'periodType")
                                                              _x
                                                        of
                                                          Prelude.Nothing -> Data.Monoid.mempty
                                                          (Prelude.Just _v)
                                                            -> (Data.Monoid.<>)
                                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                    90)
                                                                 ((Prelude..)
                                                                    (\ bs
                                                                       -> (Data.Monoid.<>)
                                                                            (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                               (Prelude.fromIntegral
                                                                                  (Data.ByteString.length
                                                                                     bs)))
                                                                            (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                               bs))
                                                                    Data.ProtoLens.encodeMessage
                                                                    _v))
                                                       ((Data.Monoid.<>)
                                                          (let
                                                             _v
                                                               = Lens.Family2.view
                                                                   (Data.ProtoLens.Field.field
                                                                      @"period")
                                                                   _x
                                                           in
                                                             if (Prelude.==)
                                                                  _v
                                                                  Data.ProtoLens.fieldDefault then
                                                                 Data.Monoid.mempty
                                                             else
                                                                 (Data.Monoid.<>)
                                                                   (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                      96)
                                                                   ((Prelude..)
                                                                      Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                      Prelude.fromIntegral _v))
                                                          ((Data.Monoid.<>)
                                                             (let
                                                                p = Lens.Family2.view
                                                                      (Data.ProtoLens.Field.field
                                                                         @"vec'comment")
                                                                      _x
                                                              in
                                                                if Data.Vector.Generic.null p then
                                                                    Data.Monoid.mempty
                                                                else
                                                                    (Data.Monoid.<>)
                                                                      (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                         106)
                                                                      ((\ bs
                                                                          -> (Data.Monoid.<>)
                                                                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                  (Prelude.fromIntegral
                                                                                     (Data.ByteString.length
                                                                                        bs)))
                                                                               (Data.ProtoLens.Encoding.Bytes.putBytes
                                                                                  bs))
                                                                         (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                                            (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                                               ((Prelude..)
                                                                                  Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                                  Prelude.fromIntegral)
                                                                               p))))
                                                             ((Data.Monoid.<>)
                                                                (let
                                                                   _v
                                                                     = Lens.Family2.view
                                                                         (Data.ProtoLens.Field.field
                                                                            @"defaultSampleType")
                                                                         _x
                                                                 in
                                                                   if (Prelude.==)
                                                                        _v
                                                                        Data.ProtoLens.fieldDefault then
                                                                       Data.Monoid.mempty
                                                                   else
                                                                       (Data.Monoid.<>)
                                                                         (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                            112)
                                                                         ((Prelude..)
                                                                            Data.ProtoLens.Encoding.Bytes.putVarInt
                                                                            Prelude.fromIntegral
                                                                            _v))
                                                                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                                                   (Lens.Family2.view
                                                                      Data.ProtoLens.unknownFields
                                                                      _x)))))))))))))))))))
instance Control.DeepSeq.NFData Profile where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Profile'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Profile'sampleType x__)
                (Control.DeepSeq.deepseq
                   (_Profile'sample x__)
                   (Control.DeepSeq.deepseq
                      (_Profile'mapping x__)
                      (Control.DeepSeq.deepseq
                         (_Profile'location x__)
                         (Control.DeepSeq.deepseq
                            (_Profile'locationIndices x__)
                            (Control.DeepSeq.deepseq
                               (_Profile'function x__)
                               (Control.DeepSeq.deepseq
                                  (_Profile'attributeTable x__)
                                  (Control.DeepSeq.deepseq
                                     (_Profile'attributeUnits x__)
                                     (Control.DeepSeq.deepseq
                                        (_Profile'linkTable x__)
                                        (Control.DeepSeq.deepseq
                                           (_Profile'stringTable x__)
                                           (Control.DeepSeq.deepseq
                                              (_Profile'dropFrames x__)
                                              (Control.DeepSeq.deepseq
                                                 (_Profile'keepFrames x__)
                                                 (Control.DeepSeq.deepseq
                                                    (_Profile'timeNanos x__)
                                                    (Control.DeepSeq.deepseq
                                                       (_Profile'durationNanos x__)
                                                       (Control.DeepSeq.deepseq
                                                          (_Profile'periodType x__)
                                                          (Control.DeepSeq.deepseq
                                                             (_Profile'period x__)
                                                             (Control.DeepSeq.deepseq
                                                                (_Profile'comment x__)
                                                                (Control.DeepSeq.deepseq
                                                                   (_Profile'defaultSampleType x__)
                                                                   ()))))))))))))))))))
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.locationIndex' @:: Lens' Sample [Data.Word.Word64]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'locationIndex' @:: Lens' Sample (Data.Vector.Unboxed.Vector Data.Word.Word64)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.locationsStartIndex' @:: Lens' Sample Data.Word.Word64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.locationsLength' @:: Lens' Sample Data.Word.Word64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.stacktraceIdIndex' @:: Lens' Sample Data.Word.Word32@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.value' @:: Lens' Sample [Data.Int.Int64]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'value' @:: Lens' Sample (Data.Vector.Unboxed.Vector Data.Int.Int64)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.label' @:: Lens' Sample [Label]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'label' @:: Lens' Sample (Data.Vector.Vector Label)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.attributes' @:: Lens' Sample [Data.Word.Word64]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'attributes' @:: Lens' Sample (Data.Vector.Unboxed.Vector Data.Word.Word64)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.link' @:: Lens' Sample Data.Word.Word64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.timestampsUnixNano' @:: Lens' Sample [Data.Word.Word64]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.vec'timestampsUnixNano' @:: Lens' Sample (Data.Vector.Unboxed.Vector Data.Word.Word64)@ -}
data Sample
  = Sample'_constructor {_Sample'locationIndex :: !(Data.Vector.Unboxed.Vector Data.Word.Word64),
                         _Sample'locationsStartIndex :: !Data.Word.Word64,
                         _Sample'locationsLength :: !Data.Word.Word64,
                         _Sample'stacktraceIdIndex :: !Data.Word.Word32,
                         _Sample'value :: !(Data.Vector.Unboxed.Vector Data.Int.Int64),
                         _Sample'label :: !(Data.Vector.Vector Label),
                         _Sample'attributes :: !(Data.Vector.Unboxed.Vector Data.Word.Word64),
                         _Sample'link :: !Data.Word.Word64,
                         _Sample'timestampsUnixNano :: !(Data.Vector.Unboxed.Vector Data.Word.Word64),
                         _Sample'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show Sample where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField Sample "locationIndex" [Data.Word.Word64] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sample'locationIndex
           (\ x__ y__ -> x__ {_Sample'locationIndex = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Sample "vec'locationIndex" (Data.Vector.Unboxed.Vector Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sample'locationIndex
           (\ x__ y__ -> x__ {_Sample'locationIndex = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Sample "locationsStartIndex" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sample'locationsStartIndex
           (\ x__ y__ -> x__ {_Sample'locationsStartIndex = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Sample "locationsLength" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sample'locationsLength
           (\ x__ y__ -> x__ {_Sample'locationsLength = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Sample "stacktraceIdIndex" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sample'stacktraceIdIndex
           (\ x__ y__ -> x__ {_Sample'stacktraceIdIndex = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Sample "value" [Data.Int.Int64] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sample'value (\ x__ y__ -> x__ {_Sample'value = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Sample "vec'value" (Data.Vector.Unboxed.Vector Data.Int.Int64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sample'value (\ x__ y__ -> x__ {_Sample'value = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Sample "label" [Label] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sample'label (\ x__ y__ -> x__ {_Sample'label = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Sample "vec'label" (Data.Vector.Vector Label) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sample'label (\ x__ y__ -> x__ {_Sample'label = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Sample "attributes" [Data.Word.Word64] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sample'attributes (\ x__ y__ -> x__ {_Sample'attributes = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Sample "vec'attributes" (Data.Vector.Unboxed.Vector Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sample'attributes (\ x__ y__ -> x__ {_Sample'attributes = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Sample "link" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sample'link (\ x__ y__ -> x__ {_Sample'link = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField Sample "timestampsUnixNano" [Data.Word.Word64] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sample'timestampsUnixNano
           (\ x__ y__ -> x__ {_Sample'timestampsUnixNano = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField Sample "vec'timestampsUnixNano" (Data.Vector.Unboxed.Vector Data.Word.Word64) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _Sample'timestampsUnixNano
           (\ x__ y__ -> x__ {_Sample'timestampsUnixNano = y__}))
        Prelude.id
instance Data.ProtoLens.Message Sample where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.profiles.v1experimental.Sample"
  packedMessageDescriptor _
    = "\n\
      \\ACKSample\DC2%\n\
      \\SOlocation_index\CAN\SOH \ETX(\EOTR\rlocationIndex\DC22\n\
      \\NAKlocations_start_index\CAN\a \SOH(\EOTR\DC3locationsStartIndex\DC2)\n\
      \\DLElocations_length\CAN\b \SOH(\EOTR\SIlocationsLength\DC2.\n\
      \\DC3stacktrace_id_index\CAN\t \SOH(\rR\DC1stacktraceIdIndex\DC2\DC4\n\
      \\ENQvalue\CAN\STX \ETX(\ETXR\ENQvalue\DC2H\n\
      \\ENQlabel\CAN\ETX \ETX(\v22.opentelemetry.proto.profiles.v1experimental.LabelR\ENQlabel\DC2\RS\n\
      \\n\
      \attributes\CAN\n\
      \ \ETX(\EOTR\n\
      \attributes\DC2\DC2\n\
      \\EOTlink\CAN\f \SOH(\EOTR\EOTlink\DC20\n\
      \\DC4timestamps_unix_nano\CAN\r \ETX(\EOTR\DC2timestampsUnixNano"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        locationIndex__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "location_index"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"locationIndex")) ::
              Data.ProtoLens.FieldDescriptor Sample
        locationsStartIndex__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "locations_start_index"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"locationsStartIndex")) ::
              Data.ProtoLens.FieldDescriptor Sample
        locationsLength__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "locations_length"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"locationsLength")) ::
              Data.ProtoLens.FieldDescriptor Sample
        stacktraceIdIndex__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "stacktrace_id_index"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"stacktraceIdIndex")) ::
              Data.ProtoLens.FieldDescriptor Sample
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor Sample
        label__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "label"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Label)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked (Data.ProtoLens.Field.field @"label")) ::
              Data.ProtoLens.FieldDescriptor Sample
        attributes__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "attributes"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"attributes")) ::
              Data.ProtoLens.FieldDescriptor Sample
        link__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "link"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"link")) ::
              Data.ProtoLens.FieldDescriptor Sample
        timestampsUnixNano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "timestamps_unix_nano"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Packed
                 (Data.ProtoLens.Field.field @"timestampsUnixNano")) ::
              Data.ProtoLens.FieldDescriptor Sample
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, locationIndex__field_descriptor),
           (Data.ProtoLens.Tag 7, locationsStartIndex__field_descriptor),
           (Data.ProtoLens.Tag 8, locationsLength__field_descriptor),
           (Data.ProtoLens.Tag 9, stacktraceIdIndex__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor),
           (Data.ProtoLens.Tag 3, label__field_descriptor),
           (Data.ProtoLens.Tag 10, attributes__field_descriptor),
           (Data.ProtoLens.Tag 12, link__field_descriptor),
           (Data.ProtoLens.Tag 13, timestampsUnixNano__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _Sample'_unknownFields
        (\ x__ y__ -> x__ {_Sample'_unknownFields = y__})
  defMessage
    = Sample'_constructor
        {_Sample'locationIndex = Data.Vector.Generic.empty,
         _Sample'locationsStartIndex = Data.ProtoLens.fieldDefault,
         _Sample'locationsLength = Data.ProtoLens.fieldDefault,
         _Sample'stacktraceIdIndex = Data.ProtoLens.fieldDefault,
         _Sample'value = Data.Vector.Generic.empty,
         _Sample'label = Data.Vector.Generic.empty,
         _Sample'attributes = Data.Vector.Generic.empty,
         _Sample'link = Data.ProtoLens.fieldDefault,
         _Sample'timestampsUnixNano = Data.Vector.Generic.empty,
         _Sample'_unknownFields = []}
  parseMessage
    = let
        loop ::
          Sample
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Word.Word64
             -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Label
                -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Word.Word64
                   -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Word.Word64
                      -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Unboxed.Vector Data.ProtoLens.Encoding.Growing.RealWorld Data.Int.Int64
                         -> Data.ProtoLens.Encoding.Bytes.Parser Sample
        loop
          x
          mutable'attributes
          mutable'label
          mutable'locationIndex
          mutable'timestampsUnixNano
          mutable'value
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'attributes <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                             (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                mutable'attributes)
                      frozen'label <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'label)
                      frozen'locationIndex <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                   mutable'locationIndex)
                      frozen'timestampsUnixNano <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                     (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                        mutable'timestampsUnixNano)
                      frozen'value <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                        (Data.ProtoLens.Encoding.Growing.unsafeFreeze mutable'value)
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
                              (Lens.Family2.set
                                 (Data.ProtoLens.Field.field @"vec'label") frozen'label
                                 (Lens.Family2.set
                                    (Data.ProtoLens.Field.field @"vec'locationIndex")
                                    frozen'locationIndex
                                    (Lens.Family2.set
                                       (Data.ProtoLens.Field.field @"vec'timestampsUnixNano")
                                       frozen'timestampsUnixNano
                                       (Lens.Family2.set
                                          (Data.ProtoLens.Field.field @"vec'value") frozen'value
                                          x))))))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        8 -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        Data.ProtoLens.Encoding.Bytes.getVarInt "location_index"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'locationIndex y)
                                loop
                                  x mutable'attributes mutable'label v mutable'timestampsUnixNano
                                  mutable'value
                        10
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                    "location_index"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'locationIndex)
                                loop
                                  x mutable'attributes mutable'label y mutable'timestampsUnixNano
                                  mutable'value
                        56
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt
                                       "locations_start_index"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"locationsStartIndex") y x)
                                  mutable'attributes mutable'label mutable'locationIndex
                                  mutable'timestampsUnixNano mutable'value
                        64
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "locations_length"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"locationsLength") y x)
                                  mutable'attributes mutable'label mutable'locationIndex
                                  mutable'timestampsUnixNano mutable'value
                        72
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "stacktrace_id_index"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"stacktraceIdIndex") y x)
                                  mutable'attributes mutable'label mutable'locationIndex
                                  mutable'timestampsUnixNano mutable'value
                        16
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (Prelude.fmap
                                           Prelude.fromIntegral
                                           Data.ProtoLens.Encoding.Bytes.getVarInt)
                                        "value"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'value y)
                                loop
                                  x mutable'attributes mutable'label mutable'locationIndex
                                  mutable'timestampsUnixNano v
                        18
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    (Prelude.fmap
                                                                       Prelude.fromIntegral
                                                                       Data.ProtoLens.Encoding.Bytes.getVarInt)
                                                                    "value"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'value)
                                loop
                                  x mutable'attributes mutable'label mutable'locationIndex
                                  mutable'timestampsUnixNano y
                        26
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "label"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'label y)
                                loop
                                  x mutable'attributes v mutable'locationIndex
                                  mutable'timestampsUnixNano mutable'value
                        80
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        Data.ProtoLens.Encoding.Bytes.getVarInt "attributes"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'attributes y)
                                loop
                                  x v mutable'label mutable'locationIndex mutable'timestampsUnixNano
                                  mutable'value
                        82
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                    "attributes"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'attributes)
                                loop
                                  x y mutable'label mutable'locationIndex mutable'timestampsUnixNano
                                  mutable'value
                        96
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getVarInt "link"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"link") y x)
                                  mutable'attributes mutable'label mutable'locationIndex
                                  mutable'timestampsUnixNano mutable'value
                        104
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        Data.ProtoLens.Encoding.Bytes.getVarInt
                                        "timestamps_unix_nano"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'timestampsUnixNano y)
                                loop
                                  x mutable'attributes mutable'label mutable'locationIndex v
                                  mutable'value
                        106
                          -> do y <- do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                        Data.ProtoLens.Encoding.Bytes.isolate
                                          (Prelude.fromIntegral len)
                                          ((let
                                              ploop qs
                                                = do packedEnd <- Data.ProtoLens.Encoding.Bytes.atEnd
                                                     if packedEnd then
                                                         Prelude.return qs
                                                     else
                                                         do !q <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                                                    Data.ProtoLens.Encoding.Bytes.getVarInt
                                                                    "timestamps_unix_nano"
                                                            qs' <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                                     (Data.ProtoLens.Encoding.Growing.append
                                                                        qs q)
                                                            ploop qs'
                                            in ploop)
                                             mutable'timestampsUnixNano)
                                loop
                                  x mutable'attributes mutable'label mutable'locationIndex y
                                  mutable'value
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'attributes mutable'label mutable'locationIndex
                                  mutable'timestampsUnixNano mutable'value
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'attributes <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                      Data.ProtoLens.Encoding.Growing.new
              mutable'label <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              mutable'locationIndex <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         Data.ProtoLens.Encoding.Growing.new
              mutable'timestampsUnixNano <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                              Data.ProtoLens.Encoding.Growing.new
              mutable'value <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                 Data.ProtoLens.Encoding.Growing.new
              loop
                Data.ProtoLens.defMessage mutable'attributes mutable'label
                mutable'locationIndex mutable'timestampsUnixNano mutable'value)
          "Sample"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                p = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"vec'locationIndex") _x
              in
                if Data.Vector.Generic.null p then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((\ bs
                          -> (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (Prelude.fromIntegral (Data.ByteString.length bs)))
                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         (Data.ProtoLens.Encoding.Bytes.runBuilder
                            (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                               Data.ProtoLens.Encoding.Bytes.putVarInt p))))
             ((Data.Monoid.<>)
                (let
                   _v
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"locationsStartIndex") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 56)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view
                            (Data.ProtoLens.Field.field @"locationsLength") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 64)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                   ((Data.Monoid.<>)
                      (let
                         _v
                           = Lens.Family2.view
                               (Data.ProtoLens.Field.field @"stacktraceIdIndex") _x
                       in
                         if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                             Data.Monoid.mempty
                         else
                             (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt 72)
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                      ((Data.Monoid.<>)
                         (let
                            p = Lens.Family2.view (Data.ProtoLens.Field.field @"vec'value") _x
                          in
                            if Data.Vector.Generic.null p then
                                Data.Monoid.mempty
                            else
                                (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                                  ((\ bs
                                      -> (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt
                                              (Prelude.fromIntegral (Data.ByteString.length bs)))
                                           (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                     (Data.ProtoLens.Encoding.Bytes.runBuilder
                                        (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                           ((Prelude..)
                                              Data.ProtoLens.Encoding.Bytes.putVarInt
                                              Prelude.fromIntegral)
                                           p))))
                         ((Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                               (\ _v
                                  -> (Data.Monoid.<>)
                                       (Data.ProtoLens.Encoding.Bytes.putVarInt 26)
                                       ((Prelude..)
                                          (\ bs
                                             -> (Data.Monoid.<>)
                                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                     (Prelude.fromIntegral
                                                        (Data.ByteString.length bs)))
                                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                          Data.ProtoLens.encodeMessage _v))
                               (Lens.Family2.view (Data.ProtoLens.Field.field @"vec'label") _x))
                            ((Data.Monoid.<>)
                               (let
                                  p = Lens.Family2.view
                                        (Data.ProtoLens.Field.field @"vec'attributes") _x
                                in
                                  if Data.Vector.Generic.null p then
                                      Data.Monoid.mempty
                                  else
                                      (Data.Monoid.<>)
                                        (Data.ProtoLens.Encoding.Bytes.putVarInt 82)
                                        ((\ bs
                                            -> (Data.Monoid.<>)
                                                 (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                    (Prelude.fromIntegral
                                                       (Data.ByteString.length bs)))
                                                 (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                           (Data.ProtoLens.Encoding.Bytes.runBuilder
                                              (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                 Data.ProtoLens.Encoding.Bytes.putVarInt p))))
                               ((Data.Monoid.<>)
                                  (let
                                     _v = Lens.Family2.view (Data.ProtoLens.Field.field @"link") _x
                                   in
                                     if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                         Data.Monoid.mempty
                                     else
                                         (Data.Monoid.<>)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt 96)
                                           (Data.ProtoLens.Encoding.Bytes.putVarInt _v))
                                  ((Data.Monoid.<>)
                                     (let
                                        p = Lens.Family2.view
                                              (Data.ProtoLens.Field.field @"vec'timestampsUnixNano")
                                              _x
                                      in
                                        if Data.Vector.Generic.null p then
                                            Data.Monoid.mempty
                                        else
                                            (Data.Monoid.<>)
                                              (Data.ProtoLens.Encoding.Bytes.putVarInt 106)
                                              ((\ bs
                                                  -> (Data.Monoid.<>)
                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                          (Prelude.fromIntegral
                                                             (Data.ByteString.length bs)))
                                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                                 (Data.ProtoLens.Encoding.Bytes.runBuilder
                                                    (Data.ProtoLens.Encoding.Bytes.foldMapBuilder
                                                       Data.ProtoLens.Encoding.Bytes.putVarInt p))))
                                     (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                        (Lens.Family2.view Data.ProtoLens.unknownFields _x))))))))))
instance Control.DeepSeq.NFData Sample where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_Sample'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_Sample'locationIndex x__)
                (Control.DeepSeq.deepseq
                   (_Sample'locationsStartIndex x__)
                   (Control.DeepSeq.deepseq
                      (_Sample'locationsLength x__)
                      (Control.DeepSeq.deepseq
                         (_Sample'stacktraceIdIndex x__)
                         (Control.DeepSeq.deepseq
                            (_Sample'value x__)
                            (Control.DeepSeq.deepseq
                               (_Sample'label x__)
                               (Control.DeepSeq.deepseq
                                  (_Sample'attributes x__)
                                  (Control.DeepSeq.deepseq
                                     (_Sample'link x__)
                                     (Control.DeepSeq.deepseq
                                        (_Sample'timestampsUnixNano x__) ())))))))))
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.type'' @:: Lens' ValueType Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.unit' @:: Lens' ValueType Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended_Fields.aggregationTemporality' @:: Lens' ValueType AggregationTemporality@ -}
data ValueType
  = ValueType'_constructor {_ValueType'type' :: !Data.Int.Int64,
                            _ValueType'unit :: !Data.Int.Int64,
                            _ValueType'aggregationTemporality :: !AggregationTemporality,
                            _ValueType'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ValueType where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ValueType "type'" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ValueType'type' (\ x__ y__ -> x__ {_ValueType'type' = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ValueType "unit" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ValueType'unit (\ x__ y__ -> x__ {_ValueType'unit = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ValueType "aggregationTemporality" AggregationTemporality where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ValueType'aggregationTemporality
           (\ x__ y__ -> x__ {_ValueType'aggregationTemporality = y__}))
        Prelude.id
instance Data.ProtoLens.Message ValueType where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.profiles.v1experimental.ValueType"
  packedMessageDescriptor _
    = "\n\
      \\tValueType\DC2\DC2\n\
      \\EOTtype\CAN\SOH \SOH(\ETXR\EOTtype\DC2\DC2\n\
      \\EOTunit\CAN\STX \SOH(\ETXR\EOTunit\DC2|\n\
      \\ETBaggregation_temporality\CAN\ETX \SOH(\SO2C.opentelemetry.proto.profiles.v1experimental.AggregationTemporalityR\SYNaggregationTemporality"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        type'__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "type"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"type'")) ::
              Data.ProtoLens.FieldDescriptor ValueType
        unit__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "unit"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"unit")) ::
              Data.ProtoLens.FieldDescriptor ValueType
        aggregationTemporality__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "aggregation_temporality"
              (Data.ProtoLens.ScalarField Data.ProtoLens.EnumField ::
                 Data.ProtoLens.FieldTypeDescriptor AggregationTemporality)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"aggregationTemporality")) ::
              Data.ProtoLens.FieldDescriptor ValueType
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, type'__field_descriptor),
           (Data.ProtoLens.Tag 2, unit__field_descriptor),
           (Data.ProtoLens.Tag 3, aggregationTemporality__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ValueType'_unknownFields
        (\ x__ y__ -> x__ {_ValueType'_unknownFields = y__})
  defMessage
    = ValueType'_constructor
        {_ValueType'type' = Data.ProtoLens.fieldDefault,
         _ValueType'unit = Data.ProtoLens.fieldDefault,
         _ValueType'aggregationTemporality = Data.ProtoLens.fieldDefault,
         _ValueType'_unknownFields = []}
  parseMessage
    = let
        loop :: ValueType -> Data.ProtoLens.Encoding.Bytes.Parser ValueType
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
                                       "type"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"type'") y x)
                        16
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "unit"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"unit") y x)
                        24
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.toEnum
                                          (Prelude.fmap
                                             Prelude.fromIntegral
                                             Data.ProtoLens.Encoding.Bytes.getVarInt))
                                       "aggregation_temporality"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"aggregationTemporality") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "ValueType"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"type'") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 8)
                      ((Prelude..)
                         Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
             ((Data.Monoid.<>)
                (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"unit") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 16)
                         ((Prelude..)
                            Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view
                            (Data.ProtoLens.Field.field @"aggregationTemporality") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 24)
                            ((Prelude..)
                               ((Prelude..)
                                  Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral)
                               Prelude.fromEnum _v))
                   (Data.ProtoLens.Encoding.Wire.buildFieldSet
                      (Lens.Family2.view Data.ProtoLens.unknownFields _x))))
instance Control.DeepSeq.NFData ValueType where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ValueType'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ValueType'type' x__)
                (Control.DeepSeq.deepseq
                   (_ValueType'unit x__)
                   (Control.DeepSeq.deepseq
                      (_ValueType'aggregationTemporality x__) ())))
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \?opentelemetry/proto/profiles/v1experimental/pprofextended.proto\DC2+opentelemetry.proto.profiles.v1experimental\SUB*opentelemetry/proto/common/v1/common.proto\"\191\b\n\
    \\aProfile\DC2W\n\
    \\vsample_type\CAN\SOH \ETX(\v26.opentelemetry.proto.profiles.v1experimental.ValueTypeR\n\
    \sampleType\DC2K\n\
    \\ACKsample\CAN\STX \ETX(\v23.opentelemetry.proto.profiles.v1experimental.SampleR\ACKsample\DC2N\n\
    \\amapping\CAN\ETX \ETX(\v24.opentelemetry.proto.profiles.v1experimental.MappingR\amapping\DC2Q\n\
    \\blocation\CAN\EOT \ETX(\v25.opentelemetry.proto.profiles.v1experimental.LocationR\blocation\DC2)\n\
    \\DLElocation_indices\CAN\SI \ETX(\ETXR\SIlocationIndices\DC2Q\n\
    \\bfunction\CAN\ENQ \ETX(\v25.opentelemetry.proto.profiles.v1experimental.FunctionR\bfunction\DC2P\n\
    \\SIattribute_table\CAN\DLE \ETX(\v2'.opentelemetry.proto.common.v1.KeyValueR\SOattributeTable\DC2c\n\
    \\SIattribute_units\CAN\DC1 \ETX(\v2:.opentelemetry.proto.profiles.v1experimental.AttributeUnitR\SOattributeUnits\DC2P\n\
    \\n\
    \link_table\CAN\DC2 \ETX(\v21.opentelemetry.proto.profiles.v1experimental.LinkR\tlinkTable\DC2!\n\
    \\fstring_table\CAN\ACK \ETX(\tR\vstringTable\DC2\US\n\
    \\vdrop_frames\CAN\a \SOH(\ETXR\n\
    \dropFrames\DC2\US\n\
    \\vkeep_frames\CAN\b \SOH(\ETXR\n\
    \keepFrames\DC2\GS\n\
    \\n\
    \time_nanos\CAN\t \SOH(\ETXR\ttimeNanos\DC2%\n\
    \\SOduration_nanos\CAN\n\
    \ \SOH(\ETXR\rdurationNanos\DC2W\n\
    \\vperiod_type\CAN\v \SOH(\v26.opentelemetry.proto.profiles.v1experimental.ValueTypeR\n\
    \periodType\DC2\SYN\n\
    \\ACKperiod\CAN\f \SOH(\ETXR\ACKperiod\DC2\CAN\n\
    \\acomment\CAN\r \ETX(\ETXR\acomment\DC2.\n\
    \\DC3default_sample_type\CAN\SO \SOH(\ETXR\DC1defaultSampleType\"H\n\
    \\rAttributeUnit\DC2#\n\
    \\rattribute_key\CAN\SOH \SOH(\ETXR\fattributeKey\DC2\DC2\n\
    \\EOTunit\CAN\STX \SOH(\ETXR\EOTunit\":\n\
    \\EOTLink\DC2\EM\n\
    \\btrace_id\CAN\SOH \SOH(\fR\atraceId\DC2\ETB\n\
    \\aspan_id\CAN\STX \SOH(\fR\ACKspanId\"\177\SOH\n\
    \\tValueType\DC2\DC2\n\
    \\EOTtype\CAN\SOH \SOH(\ETXR\EOTtype\DC2\DC2\n\
    \\EOTunit\CAN\STX \SOH(\ETXR\EOTunit\DC2|\n\
    \\ETBaggregation_temporality\CAN\ETX \SOH(\SO2C.opentelemetry.proto.profiles.v1experimental.AggregationTemporalityR\SYNaggregationTemporality\"\132\ETX\n\
    \\ACKSample\DC2%\n\
    \\SOlocation_index\CAN\SOH \ETX(\EOTR\rlocationIndex\DC22\n\
    \\NAKlocations_start_index\CAN\a \SOH(\EOTR\DC3locationsStartIndex\DC2)\n\
    \\DLElocations_length\CAN\b \SOH(\EOTR\SIlocationsLength\DC2.\n\
    \\DC3stacktrace_id_index\CAN\t \SOH(\rR\DC1stacktraceIdIndex\DC2\DC4\n\
    \\ENQvalue\CAN\STX \ETX(\ETXR\ENQvalue\DC2H\n\
    \\ENQlabel\CAN\ETX \ETX(\v22.opentelemetry.proto.profiles.v1experimental.LabelR\ENQlabel\DC2\RS\n\
    \\n\
    \attributes\CAN\n\
    \ \ETX(\EOTR\n\
    \attributes\DC2\DC2\n\
    \\EOTlink\CAN\f \SOH(\EOTR\EOTlink\DC20\n\
    \\DC4timestamps_unix_nano\CAN\r \ETX(\EOTR\DC2timestampsUnixNano\"X\n\
    \\ENQLabel\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\ETXR\ETXkey\DC2\DLE\n\
    \\ETXstr\CAN\STX \SOH(\ETXR\ETXstr\DC2\DLE\n\
    \\ETXnum\CAN\ETX \SOH(\ETXR\ETXnum\DC2\EM\n\
    \\bnum_unit\CAN\EOT \SOH(\ETXR\anumUnit\"\213\ETX\n\
    \\aMapping\DC2\SO\n\
    \\STXid\CAN\SOH \SOH(\EOTR\STXid\DC2!\n\
    \\fmemory_start\CAN\STX \SOH(\EOTR\vmemoryStart\DC2!\n\
    \\fmemory_limit\CAN\ETX \SOH(\EOTR\vmemoryLimit\DC2\US\n\
    \\vfile_offset\CAN\EOT \SOH(\EOTR\n\
    \fileOffset\DC2\SUB\n\
    \\bfilename\CAN\ENQ \SOH(\ETXR\bfilename\DC2\EM\n\
    \\bbuild_id\CAN\ACK \SOH(\ETXR\abuildId\DC2\\\n\
    \\rbuild_id_kind\CAN\v \SOH(\SO28.opentelemetry.proto.profiles.v1experimental.BuildIdKindR\vbuildIdKind\DC2\RS\n\
    \\n\
    \attributes\CAN\f \ETX(\EOTR\n\
    \attributes\DC2#\n\
    \\rhas_functions\CAN\a \SOH(\bR\fhasFunctions\DC2#\n\
    \\rhas_filenames\CAN\b \SOH(\bR\fhasFilenames\DC2(\n\
    \\DLEhas_line_numbers\CAN\t \SOH(\bR\SOhasLineNumbers\DC2*\n\
    \\DC1has_inline_frames\CAN\n\
    \ \SOH(\bR\SIhasInlineFrames\"\252\SOH\n\
    \\bLocation\DC2\SO\n\
    \\STXid\CAN\SOH \SOH(\EOTR\STXid\DC2#\n\
    \\rmapping_index\CAN\STX \SOH(\EOTR\fmappingIndex\DC2\CAN\n\
    \\aaddress\CAN\ETX \SOH(\EOTR\aaddress\DC2E\n\
    \\EOTline\CAN\EOT \ETX(\v21.opentelemetry.proto.profiles.v1experimental.LineR\EOTline\DC2\ESC\n\
    \\tis_folded\CAN\ENQ \SOH(\bR\bisFolded\DC2\GS\n\
    \\n\
    \type_index\CAN\ACK \SOH(\rR\ttypeIndex\DC2\RS\n\
    \\n\
    \attributes\CAN\a \ETX(\EOTR\n\
    \attributes\"Y\n\
    \\EOTLine\DC2%\n\
    \\SOfunction_index\CAN\SOH \SOH(\EOTR\rfunctionIndex\DC2\DC2\n\
    \\EOTline\CAN\STX \SOH(\ETXR\EOTline\DC2\SYN\n\
    \\ACKcolumn\CAN\ETX \SOH(\ETXR\ACKcolumn\"\138\SOH\n\
    \\bFunction\DC2\SO\n\
    \\STXid\CAN\SOH \SOH(\EOTR\STXid\DC2\DC2\n\
    \\EOTname\CAN\STX \SOH(\ETXR\EOTname\DC2\US\n\
    \\vsystem_name\CAN\ETX \SOH(\ETXR\n\
    \systemName\DC2\SUB\n\
    \\bfilename\CAN\EOT \SOH(\ETXR\bfilename\DC2\GS\n\
    \\n\
    \start_line\CAN\ENQ \SOH(\ETXR\tstartLine*\140\SOH\n\
    \\SYNAggregationTemporality\DC2'\n\
    \#AGGREGATION_TEMPORALITY_UNSPECIFIED\DLE\NUL\DC2!\n\
    \\GSAGGREGATION_TEMPORALITY_DELTA\DLE\SOH\DC2&\n\
    \\"AGGREGATION_TEMPORALITY_CUMULATIVE\DLE\STX*<\n\
    \\vBuildIdKind\DC2\DC3\n\
    \\SIBUILD_ID_LINKER\DLE\NUL\DC2\CAN\n\
    \\DC4BUILD_ID_BINARY_HASH\DLE\SOHB\152\SOH\n\
    \.io.opentelemetry.proto.profiles.v1experimentalP\SOHZ6go.opentelemetry.io/proto/otlp/profiles/v1experimental\170\STX+OpenTelemetry.Proto.Profiles.V1ExperimentalJ\157\148\SOH\n\
    \\a\DC2\ENQ6\NUL\131\ETX\SOH\n\
    \\183\DC1\n\
    \\SOH\f\DC2\ETX6\NUL\DC22\218\t Copyright 2023, OpenTelemetry Authors\n\
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
    \ This file includes work covered by the following copyright and permission notices:\n\
    \\n\
    \ Copyright 2016 Google Inc. All Rights Reserved.\n\
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
    \2\207\a Profile is a common stacktrace profile format.\n\
    \\n\
    \ Measurements represented with this format should follow the\n\
    \ following conventions:\n\
    \\n\
    \ - Consumers should treat unset optional fields as if they had been\n\
    \   set with their default value.\n\
    \\n\
    \ - When possible, measurements should be stored in \"unsampled\" form\n\
    \   that is most useful to humans.  There should be enough\n\
    \   information present to determine the original sampled values.\n\
    \\n\
    \ - On-disk, the serialized proto must be gzip-compressed.\n\
    \\n\
    \ - The profile is represented as a set of samples, where each sample\n\
    \   references a sequence of locations, and where each location belongs\n\
    \   to a mapping.\n\
    \ - There is a N->1 relationship from sample.location_id entries to\n\
    \   locations. For every sample.location_id entry there must be a\n\
    \   unique Location with that index.\n\
    \ - There is an optional N->1 relationship from locations to\n\
    \   mappings. For every nonzero Location.mapping_id there must be a\n\
    \   unique Mapping with that index.\n\
    \\n\
    \\b\n\
    \\SOH\STX\DC2\ETX8\NUL4\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX:\NUL4\n\
    \\b\n\
    \\SOH\b\DC2\ETX<\NULH\n\
    \\t\n\
    \\STX\b%\DC2\ETX<\NULH\n\
    \\b\n\
    \\SOH\b\DC2\ETX=\NUL\"\n\
    \\t\n\
    \\STX\b\n\
    \\DC2\ETX=\NUL\"\n\
    \\b\n\
    \\SOH\b\DC2\ETX>\NULG\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX>\NULG\n\
    \\b\n\
    \\SOH\b\DC2\ETX?\NULM\n\
    \\t\n\
    \\STX\b\v\DC2\ETX?\NULM\n\
    \\162\SOH\n\
    \\STX\EOT\NUL\DC2\EOTC\NUL}\SOH\SUB\149\SOH Represents a complete profile, including sample types, samples,\n\
    \ mappings to binaries, locations, functions, string table, and additional metadata.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETXC\b\SI\n\
    \\176\ETX\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETXL\STX%\SUB\162\ETX A description of the samples associated with each Sample.value.\n\
    \ For a cpu profile this might be:\n\
    \   [[\"cpu\",\"nanoseconds\"]] or [[\"wall\",\"seconds\"]] or [[\"syscall\",\"count\"]]\n\
    \ For a heap profile, this might be:\n\
    \   [[\"allocations\",\"count\"], [\"space\",\"bytes\"]],\n\
    \ If one of the values represents the number of events represented\n\
    \ by the sample, by convention it should be at index 0 and use\n\
    \ sample_type.unit == \"count\".\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\EOT\DC2\ETXL\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETXL\v\DC4\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETXL\NAK \n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETXL#$\n\
    \;\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETXN\STX\GS\SUB. The set of samples recorded in this profile.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\EOT\DC2\ETXN\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ACK\DC2\ETXN\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETXN\DC2\CAN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETXN\ESC\FS\n\
    \\140\SOH\n\
    \\EOT\EOT\NUL\STX\STX\DC2\ETXQ\STX\US\SUB\DEL Mapping from address ranges to the image/binary/library mapped\n\
    \ into that address range.  mapping[0] will be the main binary.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\EOT\DC2\ETXQ\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ACK\DC2\ETXQ\v\DC2\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\SOH\DC2\ETXQ\DC3\SUB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\STX\ETX\DC2\ETXQ\GS\RS\n\
    \D\n\
    \\EOT\EOT\NUL\STX\ETX\DC2\ETXS\STX!\SUB7 Locations referenced by samples via location_indices.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\EOT\DC2\ETXS\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ACK\DC2\ETXS\v\DC3\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\SOH\DC2\ETXS\DC4\FS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ETX\ETX\DC2\ETXS\US \n\
    \8\n\
    \\EOT\EOT\NUL\STX\EOT\DC2\ETXU\STX'\SUB+ Array of locations referenced by samples.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EOT\EOT\DC2\ETXU\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EOT\ENQ\DC2\ETXU\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EOT\SOH\DC2\ETXU\DC1!\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\EOT\ETX\DC2\ETXU$&\n\
    \1\n\
    \\EOT\EOT\NUL\STX\ENQ\DC2\ETXW\STX!\SUB$ Functions referenced by locations.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ENQ\EOT\DC2\ETXW\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ENQ\ACK\DC2\ETXW\v\DC3\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ENQ\SOH\DC2\ETXW\DC4\FS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ENQ\ETX\DC2\ETXW\US \n\
    \+\n\
    \\EOT\EOT\NUL\STX\ACK\DC2\ETXY\STXG\SUB\RS Lookup table for attributes.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ACK\EOT\DC2\ETXY\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ACK\ACK\DC2\ETXY\v1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ACK\SOH\DC2\ETXY2A\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\ACK\ETX\DC2\ETXYDF\n\
    \E\n\
    \\EOT\EOT\NUL\STX\a\DC2\ETX[\STX.\SUB8 Represents a mapping between Attribute Keys and Units.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\a\EOT\DC2\ETX[\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\a\ACK\DC2\ETX[\v\CAN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\a\SOH\DC2\ETX[\EM(\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\a\ETX\DC2\ETX[+-\n\
    \&\n\
    \\EOT\EOT\NUL\STX\b\DC2\ETX]\STX \SUB\EM Lookup table for links.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\b\EOT\DC2\ETX]\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\b\ACK\DC2\ETX]\v\SI\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\b\SOH\DC2\ETX]\DLE\SUB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\b\ETX\DC2\ETX]\GS\US\n\
    \m\n\
    \\EOT\EOT\NUL\STX\t\DC2\ETX`\STX#\SUB` A common table for strings referenced by various messages.\n\
    \ string_table[0] must always be \"\".\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\t\EOT\DC2\ETX`\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\t\ENQ\DC2\ETX`\v\DC1\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\t\SOH\DC2\ETX`\DC2\RS\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\t\ETX\DC2\ETX`!\"\n\
    \\178\SOH\n\
    \\EOT\EOT\NUL\STX\n\
    \\DC2\ETXc\STX\CAN\SUB\136\SOH frames with Function.function_name fully matching the following\n\
    \ regexp will be dropped from the samples, along with their successors.\n\
    \\"\SUB Index into string table.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\n\
    \\ENQ\DC2\ETXc\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\n\
    \\SOH\DC2\ETXc\b\DC3\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\n\
    \\ETX\DC2\ETXc\SYN\ETB\n\
    \\160\SOH\n\
    \\EOT\EOT\NUL\STX\v\DC2\ETXf\STX\CAN\SUBw frames with Function.function_name fully matching the following\n\
    \ regexp will be kept, even if it matches drop_frames.\n\
    \\"\SUB Index into string table.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\v\ENQ\DC2\ETXf\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\v\SOH\DC2\ETXf\b\DC3\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\v\ETX\DC2\ETXf\SYN\ETB\n\
    \\167\SOH\n\
    \\EOT\EOT\NUL\STX\f\DC2\ETXl\STX\ETB\SUBE Time of collection (UTC) represented as nanoseconds past the epoch.\n\
    \2S The following fields are informational, do not affect\n\
    \ interpretation of results.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\f\ENQ\DC2\ETXl\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\f\SOH\DC2\ETXl\b\DC2\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\f\ETX\DC2\ETXl\NAK\SYN\n\
    \B\n\
    \\EOT\EOT\NUL\STX\r\DC2\ETXn\STX\FS\SUB5 Duration of the profile, if a duration makes sense.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\r\ENQ\DC2\ETXn\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\r\SOH\DC2\ETXn\b\SYN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\r\ETX\DC2\ETXn\EM\ESC\n\
    \l\n\
    \\EOT\EOT\NUL\STX\SO\DC2\ETXq\STX\GS\SUB_ The kind of events between sampled occurrences.\n\
    \ e.g [ \"cpu\",\"cycles\" ] or [ \"heap\",\"bytes\" ]\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SO\ACK\DC2\ETXq\STX\v\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SO\SOH\DC2\ETXq\f\ETB\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SO\ETX\DC2\ETXq\SUB\FS\n\
    \@\n\
    \\EOT\EOT\NUL\STX\SI\DC2\ETXs\STX\DC4\SUB3 The number of events between sampled occurrences.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SI\ENQ\DC2\ETXs\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SI\SOH\DC2\ETXs\b\SO\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SI\ETX\DC2\ETXs\DC1\DC3\n\
    \\225\STX\n\
    \\EOT\EOT\NUL\STX\DLE\DC2\ETXy\STX\RS\SUB\181\STX Free-form text associated with the profile. The text is displayed as is\n\
    \ to the user by the tools that read profiles (e.g. by pprof). This field\n\
    \ should not be used to store any machine-readable information, it is only\n\
    \ for human-friendly content. The profile must stay functional if this field\n\
    \ is cleaned.\n\
    \\"\FS Indices into string table.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DLE\EOT\DC2\ETXy\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DLE\ENQ\DC2\ETXy\v\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DLE\SOH\DC2\ETXy\DC1\CAN\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DLE\ETX\DC2\ETXy\ESC\GS\n\
    \\146\SOH\n\
    \\EOT\EOT\NUL\STX\DC1\DC2\ETX|\STX!\SUB\132\SOH Index into the string table of the type of the preferred sample\n\
    \ value. If unset, clients should default to the last sample value.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC1\ENQ\DC2\ETX|\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC1\SOH\DC2\ETX|\b\ESC\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\DC1\ETX\DC2\ETX|\RS \n\
    \F\n\
    \\STX\EOT\SOH\DC2\ACK\128\SOH\NUL\133\SOH\SOH\SUB8 Represents a mapping between Attribute Keys and Units.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\SOH\SOH\DC2\EOT\128\SOH\b\NAK\n\
    \(\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\EOT\130\SOH\STX\SUB\SUB\SUB Index into string table.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SOH\STX\NUL\ENQ\DC2\EOT\130\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\EOT\130\SOH\b\NAK\n\
    \\r\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\EOT\130\SOH\CAN\EM\n\
    \(\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\EOT\132\SOH\STX\DC1\SUB\SUB Index into string table.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\SOH\STX\SOH\ENQ\DC2\EOT\132\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\EOT\132\SOH\b\f\n\
    \\r\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\EOT\132\SOH\SI\DLE\n\
    \\150\SOH\n\
    \\STX\EOT\STX\DC2\ACK\137\SOH\NUL\144\SOH\SOH\SUB\135\SOH A pointer from a profile Sample to a trace Span.\n\
    \ Connects a profile sample to a trace span, identified by unique trace and span IDs.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\STX\SOH\DC2\EOT\137\SOH\b\f\n\
    \l\n\
    \\EOT\EOT\STX\STX\NUL\DC2\EOT\140\SOH\STX\NAK\SUB^ A unique identifier of a trace that this linked span is part of. The ID is a\n\
    \ 16-byte array.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\EOT\140\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\EOT\140\SOH\b\DLE\n\
    \\r\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\EOT\140\SOH\DC3\DC4\n\
    \S\n\
    \\EOT\EOT\STX\STX\SOH\DC2\EOT\143\SOH\STX\DC4\SUBE A unique identifier for the linked span. The ID is an 8-byte array.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\STX\STX\SOH\ENQ\DC2\EOT\143\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\EOT\143\SOH\b\SI\n\
    \\r\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\EOT\143\SOH\DC2\DC3\n\
    \\156\SOH\n\
    \\STX\ENQ\NUL\DC2\ACK\148\SOH\NUL\212\SOH\SOH\SUB\141\SOH Specifies the method of aggregating metric values, either DELTA (change since last report)\n\
    \ or CUMULATIVE (total since a fixed start time).\n\
    \\n\
    \\v\n\
    \\ETX\ENQ\NUL\SOH\DC2\EOT\148\SOH\ENQ\ESC\n\
    \W\n\
    \\EOT\ENQ\NUL\STX\NUL\DC2\EOT\150\SOH\STX*\SUBI UNSPECIFIED is the default AggregationTemporality, it MUST not be used. \n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\NUL\SOH\DC2\EOT\150\SOH\STX%\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\NUL\STX\DC2\EOT\150\SOH()\n\
    \\172\t\n\
    \\EOT\ENQ\NUL\STX\SOH\DC2\EOT\176\SOH\STX$\SUB\157\t* DELTA is an AggregationTemporality for a profiler which reports\n\
    \changes since last report time. Successive metrics contain aggregation of\n\
    \values from continuous and non-overlapping intervals.\n\
    \\n\
    \The values for a DELTA metric are based only on the time interval\n\
    \associated with one measurement cycle. There is no dependency on\n\
    \previous measurements like is the case for CUMULATIVE metrics.\n\
    \\n\
    \For example, consider a system measuring the number of requests that\n\
    \it receives and reports the sum of these requests every second as a\n\
    \DELTA metric:\n\
    \\n\
    \1. The system starts receiving at time=t_0.\n\
    \2. A request is received, the system measures 1 request.\n\
    \3. A request is received, the system measures 1 request.\n\
    \4. A request is received, the system measures 1 request.\n\
    \5. The 1 second collection cycle ends. A metric is exported for the\n\
    \number of requests received over the interval of time t_0 to\n\
    \t_0+1 with a value of 3.\n\
    \6. A request is received, the system measures 1 request.\n\
    \7. A request is received, the system measures 1 request.\n\
    \8. The 1 second collection cycle ends. A metric is exported for the\n\
    \number of requests received over the interval of time t_0+1 to\n\
    \t_0+2 with a value of 2. \n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\SOH\SOH\DC2\EOT\176\SOH\STX\US\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\SOH\STX\DC2\EOT\176\SOH\"#\n\
    \\178\r\n\
    \\EOT\ENQ\NUL\STX\STX\DC2\EOT\211\SOH\STX)\SUB\163\r* CUMULATIVE is an AggregationTemporality for a profiler which\n\
    \reports changes since a fixed start time. This means that current values\n\
    \of a CUMULATIVE metric depend on all previous measurements since the\n\
    \start time. Because of this, the sender is required to retain this state\n\
    \in some form. If this state is lost or invalidated, the CUMULATIVE metric\n\
    \values MUST be reset and a new fixed start time following the last\n\
    \reported measurement time sent MUST be used.\n\
    \\n\
    \For example, consider a system measuring the number of requests that\n\
    \it receives and reports the sum of these requests every second as a\n\
    \CUMULATIVE metric:\n\
    \\n\
    \1. The system starts receiving at time=t_0.\n\
    \2. A request is received, the system measures 1 request.\n\
    \3. A request is received, the system measures 1 request.\n\
    \4. A request is received, the system measures 1 request.\n\
    \5. The 1 second collection cycle ends. A metric is exported for the\n\
    \number of requests received over the interval of time t_0 to\n\
    \t_0+1 with a value of 3.\n\
    \6. A request is received, the system measures 1 request.\n\
    \7. A request is received, the system measures 1 request.\n\
    \8. The 1 second collection cycle ends. A metric is exported for the\n\
    \number of requests received over the interval of time t_0 to\n\
    \t_0+2 with a value of 5.\n\
    \9. The system experiences a fault and loses state.\n\
    \10. The system recovers and resumes receiving at time=t_1.\n\
    \11. A request is received, the system measures 1 request.\n\
    \12. The 1 second collection cycle ends. A metric is exported for the\n\
    \number of requests received over the interval of time t_1 to\n\
    \t_1+1 with a value of 1.\n\
    \\n\
    \Note: Even though, when reporting changes since last report time, using\n\
    \CUMULATIVE is valid, it is not recommended. \n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\STX\SOH\DC2\EOT\211\SOH\STX$\n\
    \\r\n\
    \\ENQ\ENQ\NUL\STX\STX\STX\DC2\EOT\211\SOH'(\n\
    \l\n\
    \\STX\EOT\ETX\DC2\ACK\215\SOH\NUL\220\SOH\SOH\SUB^ ValueType describes the type and units of a value, with an optional aggregation temporality.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\ETX\SOH\DC2\EOT\215\SOH\b\DC1\n\
    \(\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\EOT\216\SOH\STX\DC1\"\SUB Index into string table.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\NUL\ENQ\DC2\EOT\216\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\EOT\216\SOH\b\f\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\EOT\216\SOH\SI\DLE\n\
    \(\n\
    \\EOT\EOT\ETX\STX\SOH\DC2\EOT\217\SOH\STX\DC1\"\SUB Index into string table.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\SOH\ENQ\DC2\EOT\217\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\SOH\SOH\DC2\EOT\217\SOH\b\f\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\SOH\ETX\DC2\EOT\217\SOH\SI\DLE\n\
    \\f\n\
    \\EOT\EOT\ETX\STX\STX\DC2\EOT\219\SOH\STX5\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\STX\ACK\DC2\EOT\219\SOH\STX\CAN\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\STX\SOH\DC2\EOT\219\SOH\EM0\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\STX\ETX\DC2\EOT\219\SOH34\n\
    \\128\STX\n\
    \\STX\EOT\EOT\DC2\ACK\226\SOH\NUL\136\STX\SOH\SUB\241\SOH Each Sample records values encountered in some program\n\
    \ context. The program context is typically a stack trace, perhaps\n\
    \ augmented with auxiliary information like the thread-id, some\n\
    \ indicator of a higher level request being handled etc.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\EOT\SOH\DC2\EOT\226\SOH\b\SO\n\
    \\191\SOH\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\EOT\229\SOH\STX%\SUB\176\SOH The indices recorded here correspond to locations in Profile.location.\n\
    \ The leaf is at location_index[0]. [deprecated, superseded by locations_start_index / locations_length]\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\NUL\EOT\DC2\EOT\229\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\NUL\ENQ\DC2\EOT\229\SOH\v\DC1\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\EOT\229\SOH\DC2 \n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\EOT\229\SOH#$\n\
    \\149\SOH\n\
    \\EOT\EOT\EOT\STX\SOH\DC2\EOT\232\SOH\STX#\SUB\134\SOH locations_start_index along with locations_length refers to to a slice of locations in Profile.location.\n\
    \ Supersedes location_index.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\SOH\ENQ\DC2\EOT\232\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\SOH\SOH\DC2\EOT\232\SOH\t\RS\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\SOH\ETX\DC2\EOT\232\SOH!\"\n\
    \\146\SOH\n\
    \\EOT\EOT\EOT\STX\STX\DC2\EOT\235\SOH\STX\RS\SUB\131\SOH locations_length along with locations_start_index refers to a slice of locations in Profile.location.\n\
    \ Supersedes location_index.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\STX\ENQ\DC2\EOT\235\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\STX\SOH\DC2\EOT\235\SOH\t\EM\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\STX\ETX\DC2\EOT\235\SOH\FS\GS\n\
    \s\n\
    \\EOT\EOT\EOT\STX\ETX\DC2\EOT\237\SOH\STX!\SUBe A 128bit id that uniquely identifies this stacktrace, globally. Index into string table. [optional]\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\ETX\ENQ\DC2\EOT\237\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\ETX\SOH\DC2\EOT\237\SOH\t\FS\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\ETX\ETX\DC2\EOT\237\SOH\US \n\
    \\231\STX\n\
    \\EOT\EOT\EOT\STX\EOT\DC2\EOT\244\SOH\STX\ESC\SUB\216\STX The type and unit of each value is defined by the corresponding\n\
    \ entry in Profile.sample_type. All samples must have the same\n\
    \ number of values, the same as the length of Profile.sample_type.\n\
    \ When aggregating multiple samples into a single sample, the\n\
    \ result has a list of values that is the element-wise sum of the\n\
    \ lists of the originals.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\EOT\EOT\DC2\EOT\244\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\EOT\ENQ\DC2\EOT\244\SOH\v\DLE\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\EOT\SOH\DC2\EOT\244\SOH\DC1\SYN\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\EOT\ETX\DC2\EOT\244\SOH\EM\SUB\n\
    \\154\EOT\n\
    \\EOT\EOT\EOT\STX\ENQ\DC2\EOT\254\SOH\STX\ESC\SUB\139\EOT label includes additional context for this sample. It can include\n\
    \ things like a thread id, allocation size, etc.\n\
    \\n\
    \ NOTE: While possible, having multiple values for the same label key is\n\
    \ strongly discouraged and should never be used. Most tools (e.g. pprof) do\n\
    \ not have good (or any) support for multi-value labels. And an even more\n\
    \ discouraged case is having a string label and a numeric label of the same\n\
    \ name on a sample.  Again, possible to express, but should not be used.\n\
    \ [deprecated, superseded by attributes]\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\ENQ\EOT\DC2\EOT\254\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\ENQ\ACK\DC2\EOT\254\SOH\v\DLE\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\ENQ\SOH\DC2\EOT\254\SOH\DC1\SYN\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\ENQ\ETX\DC2\EOT\254\SOH\EM\SUB\n\
    \O\n\
    \\EOT\EOT\EOT\STX\ACK\DC2\EOT\128\STX\STX\"\SUBA References to attributes in Profile.attribute_table. [optional]\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\ACK\EOT\DC2\EOT\128\STX\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\ACK\ENQ\DC2\EOT\128\STX\v\DC1\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\ACK\SOH\DC2\EOT\128\STX\DC2\FS\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\ACK\ETX\DC2\EOT\128\STX\US!\n\
    \C\n\
    \\EOT\EOT\EOT\STX\a\DC2\EOT\131\STX\STX\DC3\SUB5 Reference to link in Profile.link_table. [optional]\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\a\ENQ\DC2\EOT\131\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\a\SOH\DC2\EOT\131\STX\t\r\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\a\ETX\DC2\EOT\131\STX\DLE\DC2\n\
    \\161\SOH\n\
    \\EOT\EOT\EOT\STX\b\DC2\EOT\135\STX\STX,\SUB\146\SOH Timestamps associated with Sample represented in nanoseconds. These timestamps are expected\n\
    \ to fall within the Profile's time range. [optional]\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\b\EOT\DC2\EOT\135\STX\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\b\ENQ\DC2\EOT\135\STX\v\DC1\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\b\SOH\DC2\EOT\135\STX\DC2&\n\
    \\r\n\
    \\ENQ\EOT\EOT\STX\b\ETX\DC2\EOT\135\STX)+\n\
    \\130\SOH\n\
    \\STX\EOT\ENQ\DC2\ACK\140\STX\NUL\155\STX\SOH\SUBt Provides additional context for a sample,\n\
    \ such as thread ID or allocation size, with optional units. [deprecated]\n\
    \\n\
    \\v\n\
    \\ETX\EOT\ENQ\SOH\DC2\EOT\140\STX\b\r\n\
    \'\n\
    \\EOT\EOT\ENQ\STX\NUL\DC2\EOT\141\STX\STX\DLE\"\EM Index into string table\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\NUL\ENQ\DC2\EOT\141\STX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\NUL\SOH\DC2\EOT\141\STX\b\v\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\NUL\ETX\DC2\EOT\141\STX\SO\SI\n\
    \W\n\
    \\EOT\EOT\ENQ\STX\SOH\DC2\EOT\144\STX\STX\DLE\SUB. At most one of the following must be present\n\
    \\"\EM Index into string table\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\SOH\ENQ\DC2\EOT\144\STX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\SOH\SOH\DC2\EOT\144\STX\b\v\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\SOH\ETX\DC2\EOT\144\STX\SO\SI\n\
    \\f\n\
    \\EOT\EOT\ENQ\STX\STX\DC2\EOT\145\STX\STX\DLE\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\STX\ENQ\DC2\EOT\145\STX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\STX\SOH\DC2\EOT\145\STX\b\v\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\STX\ETX\DC2\EOT\145\STX\SO\SI\n\
    \\198\ETX\n\
    \\EOT\EOT\ENQ\STX\ETX\DC2\EOT\154\STX\STX\NAK\SUB\156\ETX Should only be present when num is present.\n\
    \ Specifies the units of num.\n\
    \ Use arbitrary string (for example, \"requests\") as a custom count unit.\n\
    \ If no unit is specified, consumer may apply heuristic to deduce the unit.\n\
    \ Consumers may also  interpret units like \"bytes\" and \"kilobytes\" as memory\n\
    \ units and units like \"seconds\" and \"nanoseconds\" as time units,\n\
    \ and apply appropriate unit conversions to these.\n\
    \\"\EM Index into string table\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\ETX\ENQ\DC2\EOT\154\STX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\ETX\SOH\DC2\EOT\154\STX\b\DLE\n\
    \\r\n\
    \\ENQ\EOT\ENQ\STX\ETX\ETX\DC2\EOT\154\STX\DC3\DC4\n\
    \>\n\
    \\STX\ENQ\SOH\DC2\ACK\158\STX\NUL\166\STX\SOH\SUB0 Indicates the semantics of the build_id field.\n\
    \\n\
    \\v\n\
    \\ETX\ENQ\SOH\SOH\DC2\EOT\158\STX\ENQ\DLE\n\
    \J\n\
    \\EOT\ENQ\SOH\STX\NUL\DC2\EOT\160\STX\STX\SYN\SUB< Linker-generated build ID, stored in the ELF binary notes.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\NUL\SOH\DC2\EOT\160\STX\STX\DC1\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\NUL\STX\DC2\EOT\160\STX\DC4\NAK\n\
    \\195\STX\n\
    \\EOT\ENQ\SOH\STX\SOH\DC2\EOT\165\STX\STX\ESC\SUB\180\STX Build ID based on the content hash of the binary. Currently no particular\n\
    \ hashing approach is standardized, so a given producer needs to define it\n\
    \ themselves and thus unlike BUILD_ID_LINKER this kind of hash is producer-specific.\n\
    \ We may choose to provide a standardized stable hash recommendation later.\n\
    \\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\SOH\SOH\DC2\EOT\165\STX\STX\SYN\n\
    \\r\n\
    \\ENQ\ENQ\SOH\STX\SOH\STX\DC2\EOT\165\STX\EM\SUB\n\
    \\130\SOH\n\
    \\STX\EOT\ACK\DC2\ACK\170\STX\NUL\196\STX\SOH\SUBt Describes the mapping of a binary in memory, including its address range,\n\
    \ file offset, and metadata like build ID\n\
    \\n\
    \\v\n\
    \\ETX\EOT\ACK\SOH\DC2\EOT\170\STX\b\SI\n\
    \?\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\EOT\172\STX\STX\DLE\SUB1 Unique nonzero id for the mapping. [deprecated]\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\NUL\ENQ\DC2\EOT\172\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\EOT\172\STX\t\v\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\EOT\172\STX\SO\SI\n\
    \K\n\
    \\EOT\EOT\ACK\STX\SOH\DC2\EOT\174\STX\STX\SUB\SUB= Address at which the binary (or DLL) is loaded into memory.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\SOH\ENQ\DC2\EOT\174\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\SOH\SOH\DC2\EOT\174\STX\t\NAK\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\SOH\ETX\DC2\EOT\174\STX\CAN\EM\n\
    \H\n\
    \\EOT\EOT\ACK\STX\STX\DC2\EOT\176\STX\STX\SUB\SUB: The limit of the address range occupied by this mapping.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\STX\ENQ\DC2\EOT\176\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\STX\SOH\DC2\EOT\176\STX\t\NAK\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\STX\ETX\DC2\EOT\176\STX\CAN\EM\n\
    \R\n\
    \\EOT\EOT\ACK\STX\ETX\DC2\EOT\178\STX\STX\EM\SUBD Offset in the binary that corresponds to the first mapped address.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\ETX\ENQ\DC2\EOT\178\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\ETX\SOH\DC2\EOT\178\STX\t\DC4\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\ETX\ETX\DC2\EOT\178\STX\ETB\CAN\n\
    \\196\SOH\n\
    \\EOT\EOT\ACK\STX\EOT\DC2\EOT\182\STX\STX\NAK\SUB\154\SOH The object this entry is loaded from.  This can be a filename on\n\
    \ disk for the main binary and shared libraries, or virtual\n\
    \ abstractions like \"[vdso]\".\n\
    \\"\EM Index into string table\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\EOT\ENQ\DC2\EOT\182\STX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\EOT\SOH\DC2\EOT\182\STX\b\DLE\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\EOT\ETX\DC2\EOT\182\STX\DC3\DC4\n\
    \\232\SOH\n\
    \\EOT\EOT\ACK\STX\ENQ\DC2\EOT\186\STX\STX\NAK\SUB\190\SOH A string that uniquely identifies a particular program version\n\
    \ with high probability. E.g., for binaries generated by GNU tools,\n\
    \ it could be the contents of the .note.gnu.build-id field.\n\
    \\"\EM Index into string table\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\ENQ\ENQ\DC2\EOT\186\STX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\ENQ\SOH\DC2\EOT\186\STX\b\DLE\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\ENQ\ETX\DC2\EOT\186\STX\DC3\DC4\n\
    \`\n\
    \\EOT\EOT\ACK\STX\ACK\DC2\EOT\188\STX\STX!\SUBR Specifies the kind of build id. See BuildIdKind enum for more details [optional]\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\ACK\ACK\DC2\EOT\188\STX\STX\r\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\ACK\SOH\DC2\EOT\188\STX\SO\ESC\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\ACK\ETX\DC2\EOT\188\STX\RS \n\
    \O\n\
    \\EOT\EOT\ACK\STX\a\DC2\EOT\190\STX\STX\"\SUBA References to attributes in Profile.attribute_table. [optional]\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\a\EOT\DC2\EOT\190\STX\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\a\ENQ\DC2\EOT\190\STX\v\DC1\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\a\SOH\DC2\EOT\190\STX\DC2\FS\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\a\ETX\DC2\EOT\190\STX\US!\n\
    \N\n\
    \\EOT\EOT\ACK\STX\b\DC2\EOT\192\STX\STX\EM\SUB@ The following fields indicate the resolution of symbolic info.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\b\ENQ\DC2\EOT\192\STX\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\b\SOH\DC2\EOT\192\STX\a\DC4\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\b\ETX\DC2\EOT\192\STX\ETB\CAN\n\
    \\f\n\
    \\EOT\EOT\ACK\STX\t\DC2\EOT\193\STX\STX\EM\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\t\ENQ\DC2\EOT\193\STX\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\t\SOH\DC2\EOT\193\STX\a\DC4\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\t\ETX\DC2\EOT\193\STX\ETB\CAN\n\
    \\f\n\
    \\EOT\EOT\ACK\STX\n\
    \\DC2\EOT\194\STX\STX\FS\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\n\
    \\ENQ\DC2\EOT\194\STX\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\n\
    \\SOH\DC2\EOT\194\STX\a\ETB\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\n\
    \\ETX\DC2\EOT\194\STX\SUB\ESC\n\
    \\f\n\
    \\EOT\EOT\ACK\STX\v\DC2\EOT\195\STX\STX\RS\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\v\ENQ\DC2\EOT\195\STX\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\v\SOH\DC2\EOT\195\STX\a\CAN\n\
    \\r\n\
    \\ENQ\EOT\ACK\STX\v\ETX\DC2\EOT\195\STX\ESC\GS\n\
    \D\n\
    \\STX\EOT\a\DC2\ACK\199\STX\NUL\233\STX\SOH\SUB6 Describes function and line table debug information.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\a\SOH\DC2\EOT\199\STX\b\DLE\n\
    \\140\SOH\n\
    \\EOT\EOT\a\STX\NUL\DC2\EOT\202\STX\STX\DLE\SUB~ Unique nonzero id for the location.  A profile could use\n\
    \ instruction addresses or any integer sequence as ids. [deprecated]\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\ENQ\DC2\EOT\202\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\SOH\DC2\EOT\202\STX\t\v\n\
    \\r\n\
    \\ENQ\EOT\a\STX\NUL\ETX\DC2\EOT\202\STX\SO\SI\n\
    \\167\SOH\n\
    \\EOT\EOT\a\STX\SOH\DC2\EOT\206\STX\STX\ESC\SUB\152\SOH The index of the corresponding profile.Mapping for this location.\n\
    \ It can be unset if the mapping is unknown or not applicable for\n\
    \ this profile type.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\ENQ\DC2\EOT\206\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\SOH\DC2\EOT\206\STX\t\SYN\n\
    \\r\n\
    \\ENQ\EOT\a\STX\SOH\ETX\DC2\EOT\206\STX\EM\SUB\n\
    \\191\STX\n\
    \\EOT\EOT\a\STX\STX\DC2\EOT\212\STX\STX\NAK\SUB\176\STX The instruction address for this location, if available.  It\n\
    \ should be within [Mapping.memory_start...Mapping.memory_limit]\n\
    \ for the corresponding mapping. A non-leaf address may be in the\n\
    \ middle of a call instruction. It is up to display tools to find\n\
    \ the beginning of the instruction if necessary.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\STX\ENQ\DC2\EOT\212\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\a\STX\STX\SOH\DC2\EOT\212\STX\t\DLE\n\
    \\r\n\
    \\ENQ\EOT\a\STX\STX\ETX\DC2\EOT\212\STX\DC3\DC4\n\
    \\161\STX\n\
    \\EOT\EOT\a\STX\ETX\DC2\EOT\220\STX\STX\EM\SUB\146\STX Multiple line indicates this location has inlined functions,\n\
    \ where the last entry represents the caller into which the\n\
    \ preceding entries were inlined.\n\
    \\n\
    \ E.g., if memcpy() is inlined into printf:\n\
    \    line[0].function_name == \"memcpy\"\n\
    \    line[1].function_name == \"printf\"\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ETX\EOT\DC2\EOT\220\STX\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ETX\ACK\DC2\EOT\220\STX\v\SI\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ETX\SOH\DC2\EOT\220\STX\DLE\DC4\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ETX\ETX\DC2\EOT\220\STX\ETB\CAN\n\
    \\189\STX\n\
    \\EOT\EOT\a\STX\EOT\DC2\EOT\226\STX\STX\NAK\SUB\174\STX Provides an indication that multiple symbols map to this location's\n\
    \ address, for example due to identical code folding by the linker. In that\n\
    \ case the line information above represents one of the multiple\n\
    \ symbols. This field must be recomputed when the symbolization state of the\n\
    \ profile changes.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\EOT\ENQ\DC2\EOT\226\STX\STX\ACK\n\
    \\r\n\
    \\ENQ\EOT\a\STX\EOT\SOH\DC2\EOT\226\STX\a\DLE\n\
    \\r\n\
    \\ENQ\EOT\a\STX\EOT\ETX\DC2\EOT\226\STX\DC3\DC4\n\
    \c\n\
    \\EOT\EOT\a\STX\ENQ\DC2\EOT\229\STX\STX\CAN\SUBU Type of frame (e.g. kernel, native, python, hotspot, php). Index into string table.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ENQ\ENQ\DC2\EOT\229\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ENQ\SOH\DC2\EOT\229\STX\t\DC3\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ENQ\ETX\DC2\EOT\229\STX\SYN\ETB\n\
    \O\n\
    \\EOT\EOT\a\STX\ACK\DC2\EOT\232\STX\STX!\SUBA References to attributes in Profile.attribute_table. [optional]\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ACK\EOT\DC2\EOT\232\STX\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ACK\ENQ\DC2\EOT\232\STX\v\DC1\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ACK\SOH\DC2\EOT\232\STX\DC2\FS\n\
    \\r\n\
    \\ENQ\EOT\a\STX\ACK\ETX\DC2\EOT\232\STX\US \n\
    \O\n\
    \\STX\EOT\b\DC2\ACK\236\STX\NUL\243\STX\SOH\SUBA Details a specific line in a source code, linked to a function.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\b\SOH\DC2\EOT\236\STX\b\f\n\
    \N\n\
    \\EOT\EOT\b\STX\NUL\DC2\EOT\238\STX\STX\FS\SUB@ The index of the corresponding profile.Function for this line.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\b\STX\NUL\ENQ\DC2\EOT\238\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\b\STX\NUL\SOH\DC2\EOT\238\STX\t\ETB\n\
    \\r\n\
    \\ENQ\EOT\b\STX\NUL\ETX\DC2\EOT\238\STX\SUB\ESC\n\
    \+\n\
    \\EOT\EOT\b\STX\SOH\DC2\EOT\240\STX\STX\DC1\SUB\GS Line number in source code.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\b\STX\SOH\ENQ\DC2\EOT\240\STX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\b\STX\SOH\SOH\DC2\EOT\240\STX\b\f\n\
    \\r\n\
    \\ENQ\EOT\b\STX\SOH\ETX\DC2\EOT\240\STX\SI\DLE\n\
    \-\n\
    \\EOT\EOT\b\STX\STX\DC2\EOT\242\STX\STX\DC3\SUB\US Column number in source code.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\b\STX\STX\ENQ\DC2\EOT\242\STX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\b\STX\STX\SOH\DC2\EOT\242\STX\b\SO\n\
    \\r\n\
    \\ENQ\EOT\b\STX\STX\ETX\DC2\EOT\242\STX\DC1\DC2\n\
    \\139\SOH\n\
    \\STX\EOT\t\DC2\ACK\247\STX\NUL\131\ETX\SOH\SUB} Describes a function, including its human-readable name, system name,\n\
    \ source file, and starting line number in the source.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\t\SOH\DC2\EOT\247\STX\b\DLE\n\
    \@\n\
    \\EOT\EOT\t\STX\NUL\DC2\EOT\249\STX\STX\DLE\SUB2 Unique nonzero id for the function. [deprecated]\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\NUL\ENQ\DC2\EOT\249\STX\STX\b\n\
    \\r\n\
    \\ENQ\EOT\t\STX\NUL\SOH\DC2\EOT\249\STX\t\v\n\
    \\r\n\
    \\ENQ\EOT\t\STX\NUL\ETX\DC2\EOT\249\STX\SO\SI\n\
    \e\n\
    \\EOT\EOT\t\STX\SOH\DC2\EOT\251\STX\STX\DC1\SUB< Name of the function, in human-readable form if available.\n\
    \\"\EM Index into string table\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\SOH\ENQ\DC2\EOT\251\STX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\t\STX\SOH\SOH\DC2\EOT\251\STX\b\f\n\
    \\r\n\
    \\ENQ\EOT\t\STX\SOH\ETX\DC2\EOT\251\STX\SI\DLE\n\
    \\138\SOH\n\
    \\EOT\EOT\t\STX\STX\DC2\EOT\254\STX\STX\CAN\SUBa Name of the function, as identified by the system.\n\
    \ For instance, it can be a C++ mangled name.\n\
    \\"\EM Index into string table\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\STX\ENQ\DC2\EOT\254\STX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\t\STX\STX\SOH\DC2\EOT\254\STX\b\DC3\n\
    \\r\n\
    \\ENQ\EOT\t\STX\STX\ETX\DC2\EOT\254\STX\SYN\ETB\n\
    \O\n\
    \\EOT\EOT\t\STX\ETX\DC2\EOT\128\ETX\STX\NAK\SUB& Source file containing the function.\n\
    \\"\EM Index into string table\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ETX\ENQ\DC2\EOT\128\ETX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ETX\SOH\DC2\EOT\128\ETX\b\DLE\n\
    \\r\n\
    \\ENQ\EOT\t\STX\ETX\ETX\DC2\EOT\128\ETX\DC3\DC4\n\
    \+\n\
    \\EOT\EOT\t\STX\EOT\DC2\EOT\130\ETX\STX\ETB\SUB\GS Line number in source file.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\t\STX\EOT\ENQ\DC2\EOT\130\ETX\STX\a\n\
    \\r\n\
    \\ENQ\EOT\t\STX\EOT\SOH\DC2\EOT\130\ETX\b\DC2\n\
    \\r\n\
    \\ENQ\EOT\t\STX\EOT\ETX\DC2\EOT\130\ETX\NAK\SYNb\ACKproto3"