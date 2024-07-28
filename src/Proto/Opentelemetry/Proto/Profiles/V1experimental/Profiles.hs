{- This file was auto-generated from opentelemetry/proto/profiles/v1experimental/profiles.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles (
        ProfileContainer(), ProfilesData(), ResourceProfiles(),
        ScopeProfiles()
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
import qualified Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended
import qualified Proto.Opentelemetry.Proto.Resource.V1.Resource
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.profileId' @:: Lens' ProfileContainer Data.ByteString.ByteString@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.startTimeUnixNano' @:: Lens' ProfileContainer Data.Word.Word64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.endTimeUnixNano' @:: Lens' ProfileContainer Data.Word.Word64@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.attributes' @:: Lens' ProfileContainer [Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.vec'attributes' @:: Lens' ProfileContainer (Data.Vector.Vector Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.droppedAttributesCount' @:: Lens' ProfileContainer Data.Word.Word32@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.originalPayloadFormat' @:: Lens' ProfileContainer Data.Text.Text@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.originalPayload' @:: Lens' ProfileContainer Data.ByteString.ByteString@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.profile' @:: Lens' ProfileContainer Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended.Profile@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.maybe'profile' @:: Lens' ProfileContainer (Prelude.Maybe Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended.Profile)@ -}
data ProfileContainer
  = ProfileContainer'_constructor {_ProfileContainer'profileId :: !Data.ByteString.ByteString,
                                   _ProfileContainer'startTimeUnixNano :: !Data.Word.Word64,
                                   _ProfileContainer'endTimeUnixNano :: !Data.Word.Word64,
                                   _ProfileContainer'attributes :: !(Data.Vector.Vector Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue),
                                   _ProfileContainer'droppedAttributesCount :: !Data.Word.Word32,
                                   _ProfileContainer'originalPayloadFormat :: !Data.Text.Text,
                                   _ProfileContainer'originalPayload :: !Data.ByteString.ByteString,
                                   _ProfileContainer'profile :: !(Prelude.Maybe Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended.Profile),
                                   _ProfileContainer'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ProfileContainer where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ProfileContainer "profileId" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProfileContainer'profileId
           (\ x__ y__ -> x__ {_ProfileContainer'profileId = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProfileContainer "startTimeUnixNano" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProfileContainer'startTimeUnixNano
           (\ x__ y__ -> x__ {_ProfileContainer'startTimeUnixNano = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProfileContainer "endTimeUnixNano" Data.Word.Word64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProfileContainer'endTimeUnixNano
           (\ x__ y__ -> x__ {_ProfileContainer'endTimeUnixNano = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProfileContainer "attributes" [Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProfileContainer'attributes
           (\ x__ y__ -> x__ {_ProfileContainer'attributes = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ProfileContainer "vec'attributes" (Data.Vector.Vector Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProfileContainer'attributes
           (\ x__ y__ -> x__ {_ProfileContainer'attributes = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProfileContainer "droppedAttributesCount" Data.Word.Word32 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProfileContainer'droppedAttributesCount
           (\ x__ y__
              -> x__ {_ProfileContainer'droppedAttributesCount = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProfileContainer "originalPayloadFormat" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProfileContainer'originalPayloadFormat
           (\ x__ y__ -> x__ {_ProfileContainer'originalPayloadFormat = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProfileContainer "originalPayload" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProfileContainer'originalPayload
           (\ x__ y__ -> x__ {_ProfileContainer'originalPayload = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ProfileContainer "profile" Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended.Profile where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProfileContainer'profile
           (\ x__ y__ -> x__ {_ProfileContainer'profile = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ProfileContainer "maybe'profile" (Prelude.Maybe Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended.Profile) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProfileContainer'profile
           (\ x__ y__ -> x__ {_ProfileContainer'profile = y__}))
        Prelude.id
instance Data.ProtoLens.Message ProfileContainer where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.profiles.v1experimental.ProfileContainer"
  packedMessageDescriptor _
    = "\n\
      \\DLEProfileContainer\DC2\GS\n\
      \\n\
      \profile_id\CAN\SOH \SOH(\fR\tprofileId\DC2/\n\
      \\DC4start_time_unix_nano\CAN\STX \SOH(\ACKR\DC1startTimeUnixNano\DC2+\n\
      \\DC2end_time_unix_nano\CAN\ETX \SOH(\ACKR\SIendTimeUnixNano\DC2G\n\
      \\n\
      \attributes\CAN\EOT \ETX(\v2'.opentelemetry.proto.common.v1.KeyValueR\n\
      \attributes\DC28\n\
      \\CANdropped_attributes_count\CAN\ENQ \SOH(\rR\SYNdroppedAttributesCount\DC26\n\
      \\ETBoriginal_payload_format\CAN\ACK \SOH(\tR\NAKoriginalPayloadFormat\DC2)\n\
      \\DLEoriginal_payload\CAN\a \SOH(\fR\SIoriginalPayload\DC2N\n\
      \\aprofile\CAN\b \SOH(\v24.opentelemetry.proto.profiles.v1experimental.ProfileR\aprofile"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        profileId__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "profile_id"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"profileId")) ::
              Data.ProtoLens.FieldDescriptor ProfileContainer
        startTimeUnixNano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "start_time_unix_nano"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Fixed64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"startTimeUnixNano")) ::
              Data.ProtoLens.FieldDescriptor ProfileContainer
        endTimeUnixNano__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "end_time_unix_nano"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Fixed64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"endTimeUnixNano")) ::
              Data.ProtoLens.FieldDescriptor ProfileContainer
        attributes__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "attributes"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"attributes")) ::
              Data.ProtoLens.FieldDescriptor ProfileContainer
        droppedAttributesCount__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "dropped_attributes_count"
              (Data.ProtoLens.ScalarField Data.ProtoLens.UInt32Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Word.Word32)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"droppedAttributesCount")) ::
              Data.ProtoLens.FieldDescriptor ProfileContainer
        originalPayloadFormat__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "original_payload_format"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"originalPayloadFormat")) ::
              Data.ProtoLens.FieldDescriptor ProfileContainer
        originalPayload__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "original_payload"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"originalPayload")) ::
              Data.ProtoLens.FieldDescriptor ProfileContainer
        profile__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "profile"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Opentelemetry.Proto.Profiles.V1experimental.Pprofextended.Profile)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'profile")) ::
              Data.ProtoLens.FieldDescriptor ProfileContainer
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, profileId__field_descriptor),
           (Data.ProtoLens.Tag 2, startTimeUnixNano__field_descriptor),
           (Data.ProtoLens.Tag 3, endTimeUnixNano__field_descriptor),
           (Data.ProtoLens.Tag 4, attributes__field_descriptor),
           (Data.ProtoLens.Tag 5, droppedAttributesCount__field_descriptor),
           (Data.ProtoLens.Tag 6, originalPayloadFormat__field_descriptor),
           (Data.ProtoLens.Tag 7, originalPayload__field_descriptor),
           (Data.ProtoLens.Tag 8, profile__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ProfileContainer'_unknownFields
        (\ x__ y__ -> x__ {_ProfileContainer'_unknownFields = y__})
  defMessage
    = ProfileContainer'_constructor
        {_ProfileContainer'profileId = Data.ProtoLens.fieldDefault,
         _ProfileContainer'startTimeUnixNano = Data.ProtoLens.fieldDefault,
         _ProfileContainer'endTimeUnixNano = Data.ProtoLens.fieldDefault,
         _ProfileContainer'attributes = Data.Vector.Generic.empty,
         _ProfileContainer'droppedAttributesCount = Data.ProtoLens.fieldDefault,
         _ProfileContainer'originalPayloadFormat = Data.ProtoLens.fieldDefault,
         _ProfileContainer'originalPayload = Data.ProtoLens.fieldDefault,
         _ProfileContainer'profile = Prelude.Nothing,
         _ProfileContainer'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ProfileContainer
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Proto.Opentelemetry.Proto.Common.V1.Common.KeyValue
             -> Data.ProtoLens.Encoding.Bytes.Parser ProfileContainer
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
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "profile_id"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"profileId") y x)
                                  mutable'attributes
                        17
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getFixed64
                                       "start_time_unix_nano"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"startTimeUnixNano") y x)
                                  mutable'attributes
                        25
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       Data.ProtoLens.Encoding.Bytes.getFixed64 "end_time_unix_nano"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"endTimeUnixNano") y x)
                                  mutable'attributes
                        34
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "attributes"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'attributes y)
                                loop x v
                        40
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (Prelude.fmap
                                          Prelude.fromIntegral
                                          Data.ProtoLens.Encoding.Bytes.getVarInt)
                                       "dropped_attributes_count"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"droppedAttributesCount") y x)
                                  mutable'attributes
                        50
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "original_payload_format"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"originalPayloadFormat") y x)
                                  mutable'attributes
                        58
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "original_payload"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"originalPayload") y x)
                                  mutable'attributes
                        66
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.isolate
                                             (Prelude.fromIntegral len) Data.ProtoLens.parseMessage)
                                       "profile"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"profile") y x)
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
          "ProfileContainer"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"profileId") _x
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
                   _v
                     = Lens.Family2.view
                         (Data.ProtoLens.Field.field @"startTimeUnixNano") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 17)
                         (Data.ProtoLens.Encoding.Bytes.putFixed64 _v))
                ((Data.Monoid.<>)
                   (let
                      _v
                        = Lens.Family2.view
                            (Data.ProtoLens.Field.field @"endTimeUnixNano") _x
                    in
                      if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                          Data.Monoid.mempty
                      else
                          (Data.Monoid.<>)
                            (Data.ProtoLens.Encoding.Bytes.putVarInt 25)
                            (Data.ProtoLens.Encoding.Bytes.putFixed64 _v))
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
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt 40)
                                  ((Prelude..)
                                     Data.ProtoLens.Encoding.Bytes.putVarInt Prelude.fromIntegral
                                     _v))
                         ((Data.Monoid.<>)
                            (let
                               _v
                                 = Lens.Family2.view
                                     (Data.ProtoLens.Field.field @"originalPayloadFormat") _x
                             in
                               if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                                   Data.Monoid.mempty
                               else
                                   (Data.Monoid.<>)
                                     (Data.ProtoLens.Encoding.Bytes.putVarInt 50)
                                     ((Prelude..)
                                        (\ bs
                                           -> (Data.Monoid.<>)
                                                (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                   (Prelude.fromIntegral
                                                      (Data.ByteString.length bs)))
                                                (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                        Data.Text.Encoding.encodeUtf8 _v))
                            ((Data.Monoid.<>)
                               (let
                                  _v
                                    = Lens.Family2.view
                                        (Data.ProtoLens.Field.field @"originalPayload") _x
                                in
                                  if (Prelude.==) _v Data.ProtoLens.fieldDefault then
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
                                           _v))
                               ((Data.Monoid.<>)
                                  (case
                                       Lens.Family2.view
                                         (Data.ProtoLens.Field.field @"maybe'profile") _x
                                   of
                                     Prelude.Nothing -> Data.Monoid.mempty
                                     (Prelude.Just _v)
                                       -> (Data.Monoid.<>)
                                            (Data.ProtoLens.Encoding.Bytes.putVarInt 66)
                                            ((Prelude..)
                                               (\ bs
                                                  -> (Data.Monoid.<>)
                                                       (Data.ProtoLens.Encoding.Bytes.putVarInt
                                                          (Prelude.fromIntegral
                                                             (Data.ByteString.length bs)))
                                                       (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                                               Data.ProtoLens.encodeMessage _v))
                                  (Data.ProtoLens.Encoding.Wire.buildFieldSet
                                     (Lens.Family2.view Data.ProtoLens.unknownFields _x)))))))))
instance Control.DeepSeq.NFData ProfileContainer where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ProfileContainer'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ProfileContainer'profileId x__)
                (Control.DeepSeq.deepseq
                   (_ProfileContainer'startTimeUnixNano x__)
                   (Control.DeepSeq.deepseq
                      (_ProfileContainer'endTimeUnixNano x__)
                      (Control.DeepSeq.deepseq
                         (_ProfileContainer'attributes x__)
                         (Control.DeepSeq.deepseq
                            (_ProfileContainer'droppedAttributesCount x__)
                            (Control.DeepSeq.deepseq
                               (_ProfileContainer'originalPayloadFormat x__)
                               (Control.DeepSeq.deepseq
                                  (_ProfileContainer'originalPayload x__)
                                  (Control.DeepSeq.deepseq
                                     (_ProfileContainer'profile x__) ()))))))))
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.resourceProfiles' @:: Lens' ProfilesData [ResourceProfiles]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.vec'resourceProfiles' @:: Lens' ProfilesData (Data.Vector.Vector ResourceProfiles)@ -}
data ProfilesData
  = ProfilesData'_constructor {_ProfilesData'resourceProfiles :: !(Data.Vector.Vector ResourceProfiles),
                               _ProfilesData'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ProfilesData where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ProfilesData "resourceProfiles" [ResourceProfiles] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProfilesData'resourceProfiles
           (\ x__ y__ -> x__ {_ProfilesData'resourceProfiles = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ProfilesData "vec'resourceProfiles" (Data.Vector.Vector ResourceProfiles) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ProfilesData'resourceProfiles
           (\ x__ y__ -> x__ {_ProfilesData'resourceProfiles = y__}))
        Prelude.id
instance Data.ProtoLens.Message ProfilesData where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.profiles.v1experimental.ProfilesData"
  packedMessageDescriptor _
    = "\n\
      \\fProfilesData\DC2j\n\
      \\DC1resource_profiles\CAN\SOH \ETX(\v2=.opentelemetry.proto.profiles.v1experimental.ResourceProfilesR\DLEresourceProfiles"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        resourceProfiles__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "resource_profiles"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ResourceProfiles)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"resourceProfiles")) ::
              Data.ProtoLens.FieldDescriptor ProfilesData
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, resourceProfiles__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ProfilesData'_unknownFields
        (\ x__ y__ -> x__ {_ProfilesData'_unknownFields = y__})
  defMessage
    = ProfilesData'_constructor
        {_ProfilesData'resourceProfiles = Data.Vector.Generic.empty,
         _ProfilesData'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ProfilesData
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ResourceProfiles
             -> Data.ProtoLens.Encoding.Bytes.Parser ProfilesData
        loop x mutable'resourceProfiles
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'resourceProfiles <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                   (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                      mutable'resourceProfiles)
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
                              (Data.ProtoLens.Field.field @"vec'resourceProfiles")
                              frozen'resourceProfiles x))
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "resource_profiles"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'resourceProfiles y)
                                loop x v
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'resourceProfiles
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'resourceProfiles <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                            Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'resourceProfiles)
          "ProfilesData"
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
                   (Data.ProtoLens.Field.field @"vec'resourceProfiles") _x))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData ProfilesData where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ProfilesData'_unknownFields x__)
             (Control.DeepSeq.deepseq (_ProfilesData'resourceProfiles x__) ())
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.resource' @:: Lens' ResourceProfiles Proto.Opentelemetry.Proto.Resource.V1.Resource.Resource@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.maybe'resource' @:: Lens' ResourceProfiles (Prelude.Maybe Proto.Opentelemetry.Proto.Resource.V1.Resource.Resource)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.scopeProfiles' @:: Lens' ResourceProfiles [ScopeProfiles]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.vec'scopeProfiles' @:: Lens' ResourceProfiles (Data.Vector.Vector ScopeProfiles)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.schemaUrl' @:: Lens' ResourceProfiles Data.Text.Text@ -}
data ResourceProfiles
  = ResourceProfiles'_constructor {_ResourceProfiles'resource :: !(Prelude.Maybe Proto.Opentelemetry.Proto.Resource.V1.Resource.Resource),
                                   _ResourceProfiles'scopeProfiles :: !(Data.Vector.Vector ScopeProfiles),
                                   _ResourceProfiles'schemaUrl :: !Data.Text.Text,
                                   _ResourceProfiles'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ResourceProfiles where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ResourceProfiles "resource" Proto.Opentelemetry.Proto.Resource.V1.Resource.Resource where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResourceProfiles'resource
           (\ x__ y__ -> x__ {_ResourceProfiles'resource = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ResourceProfiles "maybe'resource" (Prelude.Maybe Proto.Opentelemetry.Proto.Resource.V1.Resource.Resource) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResourceProfiles'resource
           (\ x__ y__ -> x__ {_ResourceProfiles'resource = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ResourceProfiles "scopeProfiles" [ScopeProfiles] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResourceProfiles'scopeProfiles
           (\ x__ y__ -> x__ {_ResourceProfiles'scopeProfiles = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ResourceProfiles "vec'scopeProfiles" (Data.Vector.Vector ScopeProfiles) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResourceProfiles'scopeProfiles
           (\ x__ y__ -> x__ {_ResourceProfiles'scopeProfiles = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ResourceProfiles "schemaUrl" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ResourceProfiles'schemaUrl
           (\ x__ y__ -> x__ {_ResourceProfiles'schemaUrl = y__}))
        Prelude.id
instance Data.ProtoLens.Message ResourceProfiles where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.profiles.v1experimental.ResourceProfiles"
  packedMessageDescriptor _
    = "\n\
      \\DLEResourceProfiles\DC2E\n\
      \\bresource\CAN\SOH \SOH(\v2).opentelemetry.proto.resource.v1.ResourceR\bresource\DC2a\n\
      \\SOscope_profiles\CAN\STX \ETX(\v2:.opentelemetry.proto.profiles.v1experimental.ScopeProfilesR\rscopeProfiles\DC2\GS\n\
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
              Data.ProtoLens.FieldDescriptor ResourceProfiles
        scopeProfiles__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "scope_profiles"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ScopeProfiles)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"scopeProfiles")) ::
              Data.ProtoLens.FieldDescriptor ResourceProfiles
        schemaUrl__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "schema_url"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"schemaUrl")) ::
              Data.ProtoLens.FieldDescriptor ResourceProfiles
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, resource__field_descriptor),
           (Data.ProtoLens.Tag 2, scopeProfiles__field_descriptor),
           (Data.ProtoLens.Tag 3, schemaUrl__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ResourceProfiles'_unknownFields
        (\ x__ y__ -> x__ {_ResourceProfiles'_unknownFields = y__})
  defMessage
    = ResourceProfiles'_constructor
        {_ResourceProfiles'resource = Prelude.Nothing,
         _ResourceProfiles'scopeProfiles = Data.Vector.Generic.empty,
         _ResourceProfiles'schemaUrl = Data.ProtoLens.fieldDefault,
         _ResourceProfiles'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ResourceProfiles
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ScopeProfiles
             -> Data.ProtoLens.Encoding.Bytes.Parser ResourceProfiles
        loop x mutable'scopeProfiles
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'scopeProfiles <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                                (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                                   mutable'scopeProfiles)
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
                              (Data.ProtoLens.Field.field @"vec'scopeProfiles")
                              frozen'scopeProfiles x))
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
                                  mutable'scopeProfiles
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "scope_profiles"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append
                                          mutable'scopeProfiles y)
                                loop x v
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "schema_url"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"schemaUrl") y x)
                                  mutable'scopeProfiles
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'scopeProfiles
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'scopeProfiles <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                         Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'scopeProfiles)
          "ResourceProfiles"
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
                      (Data.ProtoLens.Field.field @"vec'scopeProfiles") _x))
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
instance Control.DeepSeq.NFData ResourceProfiles where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ResourceProfiles'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ResourceProfiles'resource x__)
                (Control.DeepSeq.deepseq
                   (_ResourceProfiles'scopeProfiles x__)
                   (Control.DeepSeq.deepseq (_ResourceProfiles'schemaUrl x__) ())))
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.scope' @:: Lens' ScopeProfiles Proto.Opentelemetry.Proto.Common.V1.Common.InstrumentationScope@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.maybe'scope' @:: Lens' ScopeProfiles (Prelude.Maybe Proto.Opentelemetry.Proto.Common.V1.Common.InstrumentationScope)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.profiles' @:: Lens' ScopeProfiles [ProfileContainer]@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.vec'profiles' @:: Lens' ScopeProfiles (Data.Vector.Vector ProfileContainer)@
         * 'Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles_Fields.schemaUrl' @:: Lens' ScopeProfiles Data.Text.Text@ -}
data ScopeProfiles
  = ScopeProfiles'_constructor {_ScopeProfiles'scope :: !(Prelude.Maybe Proto.Opentelemetry.Proto.Common.V1.Common.InstrumentationScope),
                                _ScopeProfiles'profiles :: !(Data.Vector.Vector ProfileContainer),
                                _ScopeProfiles'schemaUrl :: !Data.Text.Text,
                                _ScopeProfiles'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ScopeProfiles where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ScopeProfiles "scope" Proto.Opentelemetry.Proto.Common.V1.Common.InstrumentationScope where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ScopeProfiles'scope
           (\ x__ y__ -> x__ {_ScopeProfiles'scope = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ScopeProfiles "maybe'scope" (Prelude.Maybe Proto.Opentelemetry.Proto.Common.V1.Common.InstrumentationScope) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ScopeProfiles'scope
           (\ x__ y__ -> x__ {_ScopeProfiles'scope = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ScopeProfiles "profiles" [ProfileContainer] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ScopeProfiles'profiles
           (\ x__ y__ -> x__ {_ScopeProfiles'profiles = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ScopeProfiles "vec'profiles" (Data.Vector.Vector ProfileContainer) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ScopeProfiles'profiles
           (\ x__ y__ -> x__ {_ScopeProfiles'profiles = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ScopeProfiles "schemaUrl" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ScopeProfiles'schemaUrl
           (\ x__ y__ -> x__ {_ScopeProfiles'schemaUrl = y__}))
        Prelude.id
instance Data.ProtoLens.Message ScopeProfiles where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.profiles.v1experimental.ScopeProfiles"
  packedMessageDescriptor _
    = "\n\
      \\rScopeProfiles\DC2I\n\
      \\ENQscope\CAN\SOH \SOH(\v23.opentelemetry.proto.common.v1.InstrumentationScopeR\ENQscope\DC2Y\n\
      \\bprofiles\CAN\STX \ETX(\v2=.opentelemetry.proto.profiles.v1experimental.ProfileContainerR\bprofiles\DC2\GS\n\
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
              Data.ProtoLens.FieldDescriptor ScopeProfiles
        profiles__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "profiles"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ProfileContainer)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"profiles")) ::
              Data.ProtoLens.FieldDescriptor ScopeProfiles
        schemaUrl__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "schema_url"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"schemaUrl")) ::
              Data.ProtoLens.FieldDescriptor ScopeProfiles
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, scope__field_descriptor),
           (Data.ProtoLens.Tag 2, profiles__field_descriptor),
           (Data.ProtoLens.Tag 3, schemaUrl__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ScopeProfiles'_unknownFields
        (\ x__ y__ -> x__ {_ScopeProfiles'_unknownFields = y__})
  defMessage
    = ScopeProfiles'_constructor
        {_ScopeProfiles'scope = Prelude.Nothing,
         _ScopeProfiles'profiles = Data.Vector.Generic.empty,
         _ScopeProfiles'schemaUrl = Data.ProtoLens.fieldDefault,
         _ScopeProfiles'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ScopeProfiles
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld ProfileContainer
             -> Data.ProtoLens.Encoding.Bytes.Parser ScopeProfiles
        loop x mutable'profiles
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do frozen'profiles <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                           (Data.ProtoLens.Encoding.Growing.unsafeFreeze
                                              mutable'profiles)
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
                              (Data.ProtoLens.Field.field @"vec'profiles") frozen'profiles x))
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
                                  mutable'profiles
                        18
                          -> do !y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                        (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                            Data.ProtoLens.Encoding.Bytes.isolate
                                              (Prelude.fromIntegral len)
                                              Data.ProtoLens.parseMessage)
                                        "profiles"
                                v <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                       (Data.ProtoLens.Encoding.Growing.append mutable'profiles y)
                                loop x v
                        26
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getText
                                             (Prelude.fromIntegral len))
                                       "schema_url"
                                loop
                                  (Lens.Family2.set (Data.ProtoLens.Field.field @"schemaUrl") y x)
                                  mutable'profiles
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
                                  mutable'profiles
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do mutable'profiles <- Data.ProtoLens.Encoding.Parser.Unsafe.unsafeLiftIO
                                    Data.ProtoLens.Encoding.Growing.new
              loop Data.ProtoLens.defMessage mutable'profiles)
          "ScopeProfiles"
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
                      (Data.ProtoLens.Field.field @"vec'profiles") _x))
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
instance Control.DeepSeq.NFData ScopeProfiles where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ScopeProfiles'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ScopeProfiles'scope x__)
                (Control.DeepSeq.deepseq
                   (_ScopeProfiles'profiles x__)
                   (Control.DeepSeq.deepseq (_ScopeProfiles'schemaUrl x__) ())))
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \:opentelemetry/proto/profiles/v1experimental/profiles.proto\DC2+opentelemetry.proto.profiles.v1experimental\SUB*opentelemetry/proto/common/v1/common.proto\SUB.opentelemetry/proto/resource/v1/resource.proto\SUB?opentelemetry/proto/profiles/v1experimental/pprofextended.proto\"z\n\
    \\fProfilesData\DC2j\n\
    \\DC1resource_profiles\CAN\SOH \ETX(\v2=.opentelemetry.proto.profiles.v1experimental.ResourceProfilesR\DLEresourceProfiles\"\227\SOH\n\
    \\DLEResourceProfiles\DC2E\n\
    \\bresource\CAN\SOH \SOH(\v2).opentelemetry.proto.resource.v1.ResourceR\bresource\DC2a\n\
    \\SOscope_profiles\CAN\STX \ETX(\v2:.opentelemetry.proto.profiles.v1experimental.ScopeProfilesR\rscopeProfiles\DC2\GS\n\
    \\n\
    \schema_url\CAN\ETX \SOH(\tR\tschemaUrlJ\ACK\b\232\a\DLE\233\a\"\212\SOH\n\
    \\rScopeProfiles\DC2I\n\
    \\ENQscope\CAN\SOH \SOH(\v23.opentelemetry.proto.common.v1.InstrumentationScopeR\ENQscope\DC2Y\n\
    \\bprofiles\CAN\STX \ETX(\v2=.opentelemetry.proto.profiles.v1experimental.ProfileContainerR\bprofiles\DC2\GS\n\
    \\n\
    \schema_url\CAN\ETX \SOH(\tR\tschemaUrl\"\197\ETX\n\
    \\DLEProfileContainer\DC2\GS\n\
    \\n\
    \profile_id\CAN\SOH \SOH(\fR\tprofileId\DC2/\n\
    \\DC4start_time_unix_nano\CAN\STX \SOH(\ACKR\DC1startTimeUnixNano\DC2+\n\
    \\DC2end_time_unix_nano\CAN\ETX \SOH(\ACKR\SIendTimeUnixNano\DC2G\n\
    \\n\
    \attributes\CAN\EOT \ETX(\v2'.opentelemetry.proto.common.v1.KeyValueR\n\
    \attributes\DC28\n\
    \\CANdropped_attributes_count\CAN\ENQ \SOH(\rR\SYNdroppedAttributesCount\DC26\n\
    \\ETBoriginal_payload_format\CAN\ACK \SOH(\tR\NAKoriginalPayloadFormat\DC2)\n\
    \\DLEoriginal_payload\CAN\a \SOH(\fR\SIoriginalPayload\DC2N\n\
    \\aprofile\CAN\b \SOH(\v24.opentelemetry.proto.profiles.v1experimental.ProfileR\aprofileB\167\SOH\n\
    \.io.opentelemetry.proto.profiles.v1experimentalB\rProfilesProtoP\SOHZ6go.opentelemetry.io/proto/otlp/profiles/v1experimental\170\STX+OpenTelemetry.Proto.Profiles.V1ExperimentalJ\156D\n\
    \\a\DC2\ENQ\SO\NUL\190\SOH\SOH\n\
    \\200\EOT\n\
    \\SOH\f\DC2\ETX\SO\NUL\DC22\189\EOT Copyright 2023, OpenTelemetry Authors\n\
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
    \\SOH\STX\DC2\ETX\DLE\NUL4\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\DC2\NUL4\n\
    \\t\n\
    \\STX\ETX\SOH\DC2\ETX\DC3\NUL8\n\
    \\t\n\
    \\STX\ETX\STX\DC2\ETX\DC4\NULI\n\
    \\b\n\
    \\SOH\b\DC2\ETX\SYN\NULH\n\
    \\t\n\
    \\STX\b%\DC2\ETX\SYN\NULH\n\
    \\b\n\
    \\SOH\b\DC2\ETX\ETB\NUL\"\n\
    \\t\n\
    \\STX\b\n\
    \\DC2\ETX\ETB\NUL\"\n\
    \\b\n\
    \\SOH\b\DC2\ETX\CAN\NULG\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX\CAN\NULG\n\
    \\b\n\
    \\SOH\b\DC2\ETX\EM\NUL.\n\
    \\t\n\
    \\STX\b\b\DC2\ETX\EM\NUL.\n\
    \\b\n\
    \\SOH\b\DC2\ETX\SUB\NULM\n\
    \\t\n\
    \\STX\b\v\DC2\ETX\SUB\NULM\n\
    \\174\ETB\n\
    \\STX\EOT\NUL\DC2\EOT]\NULd\SOH\SUB\197\ETX ProfilesData represents the profiles data that can be stored in persistent storage,\n\
    \ OR can be embedded by other protocols that transfer OTLP profiles data but do not\n\
    \ implement the OTLP protocol.\n\
    \\n\
    \ The main difference between this message and collector protocol is that\n\
    \ in this message there will not be any \"control\" or \"metadata\" specific to\n\
    \ OTLP protocol.\n\
    \\n\
    \ When new fields are added into this message, the OTLP request MUST be updated\n\
    \ as well.\n\
    \2\217\DC3                Relationships Diagram\n\
    \\n\
    \ \226\148\140\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\144                LEGEND\n\
    \ \226\148\130   ProfilesData   \226\148\130\n\
    \ \226\148\148\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\152            \226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\150\182 embedded\n\
    \   \226\148\130\n\
    \   \226\148\130 1-n                         \226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\150\183 referenced by index\n\
    \   \226\150\188\n\
    \ \226\148\140\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\144\n\
    \ \226\148\130 ResourceProfiles \226\148\130\n\
    \ \226\148\148\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\152\n\
    \   \226\148\130\n\
    \   \226\148\130 1-n\n\
    \   \226\150\188\n\
    \ \226\148\140\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\144\n\
    \ \226\148\130  ScopeProfiles   \226\148\130\n\
    \ \226\148\148\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\152\n\
    \   \226\148\130\n\
    \   \226\148\130 1-n\n\
    \   \226\150\188\n\
    \ \226\148\140\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\144\n\
    \ \226\148\130 ProfileContainer \226\148\130\n\
    \ \226\148\148\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\152\n\
    \   \226\148\130\n\
    \   \226\148\130 1-1\n\
    \   \226\150\188\n\
    \ \226\148\140\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\144\n\
    \ \226\148\130      Profile     \226\148\130\n\
    \ \226\148\148\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\152\n\
    \   \226\148\130                                n-1\n\
    \   \226\148\130 1-n         \226\148\140\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\144\n\
    \   \226\150\188             \226\148\130                                       \226\150\189\n\
    \ \226\148\140\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\144   1-n   \226\148\140\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\144      \226\148\140\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\144\n\
    \ \226\148\130      Sample      \226\148\130 \226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\150\183 \226\148\130   KeyValue   \226\148\130      \226\148\130   Link   \226\148\130\n\
    \ \226\148\148\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\152         \226\148\148\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\152      \226\148\148\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\152\n\
    \   \226\148\130                    1-n       \226\150\179      \226\150\179\n\
    \   \226\148\130 1-n        \226\148\140\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\152      \226\148\130 1-n\n\
    \   \226\150\189            \226\148\130                        \226\148\130\n\
    \ \226\148\140\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\144   n-1   \226\148\140\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\144\n\
    \ \226\148\130     Location     \226\148\130 \226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\150\183 \226\148\130   Mapping    \226\148\130\n\
    \ \226\148\148\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\152         \226\148\148\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\152\n\
    \   \226\148\130\n\
    \   \226\148\130 1-n\n\
    \   \226\150\188\n\
    \ \226\148\140\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\144\n\
    \ \226\148\130       Line       \226\148\130\n\
    \ \226\148\148\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\152\n\
    \   \226\148\130\n\
    \   \226\148\130 1-1\n\
    \   \226\150\189\n\
    \ \226\148\140\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\144\n\
    \ \226\148\130     Function     \226\148\130\n\
    \ \226\148\148\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\128\226\148\152\n\
    \\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX]\b\DC4\n\
    \\177\STX\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETXc\STX2\SUB\163\STX An array of ResourceProfiles.\n\
    \ For data coming from a single resource this array will typically contain\n\
    \ one element. Intermediary nodes that receive data from multiple origins\n\
    \ typically batch the data before forwarding further and in that case this\n\
    \ array will contain multiple elements.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\EOT\DC2\ETXc\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETXc\v\ESC\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETXc\FS-\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETXc01\n\
    \<\n\
    \\STX\EOT\SOH\DC2\EOTh\NULx\SOH\SUB0 A collection of ScopeProfiles from a Resource.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETXh\b\CAN\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\t\DC2\ETXi\STX\DLE\n\
    \\v\n\
    \\EOT\EOT\SOH\t\NUL\DC2\ETXi\v\SI\n\
    \\f\n\
    \\ENQ\EOT\SOH\t\NUL\SOH\DC2\ETXi\v\SI\n\
    \\f\n\
    \\ENQ\EOT\SOH\t\NUL\STX\DC2\ETXi\v\SI\n\
    \w\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETXm\STX8\SUBj The resource for the profiles in this message.\n\
    \ If this field is not set then no resource info is known.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETXm\STX*\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETXm+3\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETXm67\n\
    \F\n\
    \\EOT\EOT\SOH\STX\SOH\DC2\ETXp\STX,\SUB9 A list of ScopeProfiles that originate from a resource.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\EOT\DC2\ETXp\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ACK\DC2\ETXp\v\CAN\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\SOH\DC2\ETXp\EM'\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\SOH\ETX\DC2\ETXp*+\n\
    \\249\STX\n\
    \\EOT\EOT\SOH\STX\STX\DC2\ETXw\STX\CAN\SUB\235\STX The Schema URL, if known. This is the identifier of the Schema that the resource data\n\
    \ is recorded in. To learn more about Schema URL see\n\
    \ https://opentelemetry.io/docs/specs/otel/schemas/#schema-url\n\
    \ This schema_url applies to the data in the \"resource\" field. It does not apply\n\
    \ to the data in the \"scope_profiles\" field which have their own schema_url field.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ENQ\DC2\ETXw\STX\b\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\SOH\DC2\ETXw\t\DC3\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\STX\ETX\DC2\ETXw\SYN\ETB\n\
    \U\n\
    \\STX\EOT\STX\DC2\ENQ{\NUL\137\SOH\SOH\SUBH A collection of ProfileContainers produced by an InstrumentationScope.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX{\b\NAK\n\
    \\208\SOH\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\DEL\STX?\SUB\194\SOH The instrumentation scope information for the profiles in this message.\n\
    \ Semantically when InstrumentationScope isn't set, it is equivalent with\n\
    \ an empty instrumentation scope name (unknown).\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ACK\DC2\ETX\DEL\STX4\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\DEL5:\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\DEL=>\n\
    \Y\n\
    \\EOT\EOT\STX\STX\SOH\DC2\EOT\130\SOH\STX)\SUBK A list of ProfileContainers that originate from an instrumentation scope.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\STX\STX\SOH\EOT\DC2\EOT\130\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\STX\STX\SOH\ACK\DC2\EOT\130\SOH\v\ESC\n\
    \\r\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\EOT\130\SOH\FS$\n\
    \\r\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\EOT\130\SOH'(\n\
    \\152\STX\n\
    \\EOT\EOT\STX\STX\STX\DC2\EOT\136\SOH\STX\CAN\SUB\137\STX The Schema URL, if known. This is the identifier of the Schema that the metric data\n\
    \ is recorded in. To learn more about Schema URL see\n\
    \ https://opentelemetry.io/docs/specs/otel/schemas/#schema-url\n\
    \ This schema_url applies to all profiles in the \"profiles\" field.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\STX\STX\STX\ENQ\DC2\EOT\136\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\STX\STX\STX\SOH\DC2\EOT\136\SOH\t\DC3\n\
    \\r\n\
    \\ENQ\EOT\STX\STX\STX\ETX\DC2\EOT\136\SOH\SYN\ETB\n\
    \|\n\
    \\STX\EOT\ETX\DC2\ACK\140\SOH\NUL\190\SOH\SOH\SUBn A ProfileContainer represents a single profile. It wraps pprof profile with OpenTelemetry specific metadata.\n\
    \\n\
    \\v\n\
    \\ETX\EOT\ETX\SOH\DC2\EOT\140\SOH\b\CAN\n\
    \\159\SOH\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\EOT\145\SOH\STX\ETB\SUB\144\SOH A globally unique identifier for a profile. The ID is a 16-byte array. An ID with\n\
    \ all zeroes is considered invalid.\n\
    \\n\
    \ This field is required.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\NUL\ENQ\DC2\EOT\145\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\EOT\145\SOH\b\DC2\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\EOT\145\SOH\NAK\SYN\n\
    \\236\SOH\n\
    \\EOT\EOT\ETX\STX\SOH\DC2\EOT\151\SOH\STX#\SUB\221\SOH start_time_unix_nano is the start time of the profile.\n\
    \ Value is UNIX Epoch time in nanoseconds since 00:00:00 UTC on 1 January 1970.\n\
    \\n\
    \ This field is semantically required and it is expected that end_time >= start_time.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\SOH\ENQ\DC2\EOT\151\SOH\STX\t\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\SOH\SOH\DC2\EOT\151\SOH\n\
    \\RS\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\SOH\ETX\DC2\EOT\151\SOH!\"\n\
    \\232\SOH\n\
    \\EOT\EOT\ETX\STX\STX\DC2\EOT\157\SOH\STX!\SUB\217\SOH end_time_unix_nano is the end time of the profile.\n\
    \ Value is UNIX Epoch time in nanoseconds since 00:00:00 UTC on 1 January 1970.\n\
    \\n\
    \ This field is semantically required and it is expected that end_time >= start_time.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\STX\ENQ\DC2\EOT\157\SOH\STX\t\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\STX\SOH\DC2\EOT\157\SOH\n\
    \\FS\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\STX\ETX\DC2\EOT\157\SOH\US \n\
    \\194\ENQ\n\
    \\EOT\EOT\ETX\STX\ETX\DC2\EOT\171\SOH\STXA\SUB\179\ENQ attributes is a collection of key/value pairs. Note, global attributes\n\
    \ like server name can be set using the resource API. Examples of attributes:\n\
    \\n\
    \     \"/http/user_agent\": \"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/537.36\"\n\
    \     \"/http/server_latency\": 300\n\
    \     \"abc.com/myattribute\": true\n\
    \     \"abc.com/score\": 10.239\n\
    \\n\
    \ The OpenTelemetry API specification further restricts the allowed value types:\n\
    \ https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/common/README.md#attribute\n\
    \ Attribute keys MUST be unique (it is not allowed to have more than one\n\
    \ attribute with the same key).\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ETX\EOT\DC2\EOT\171\SOH\STX\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ETX\ACK\DC2\EOT\171\SOH\v1\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ETX\SOH\DC2\EOT\171\SOH2<\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ETX\ETX\DC2\EOT\171\SOH?@\n\
    \\247\SOH\n\
    \\EOT\EOT\ETX\STX\EOT\DC2\EOT\176\SOH\STX&\SUB\232\SOH dropped_attributes_count is the number of attributes that were discarded. Attributes\n\
    \ can be discarded because their keys are too long or because there are too many\n\
    \ attributes. If this value is 0, then no attributes were dropped.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\EOT\ENQ\DC2\EOT\176\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\EOT\SOH\DC2\EOT\176\SOH\t!\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\EOT\ETX\DC2\EOT\176\SOH$%\n\
    \\151\SOH\n\
    \\EOT\EOT\ETX\STX\ENQ\DC2\EOT\179\SOH\STX%\SUB\136\SOH Specifies format of the original payload. Common values are defined in semantic conventions. [required if original_payload is present]\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ENQ\ENQ\DC2\EOT\179\SOH\STX\b\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ENQ\SOH\DC2\EOT\179\SOH\t \n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ENQ\ETX\DC2\EOT\179\SOH#$\n\
    \\192\EOT\n\
    \\EOT\EOT\ETX\STX\ACK\DC2\EOT\186\SOH\STX\GS\SUB\177\EOT Original payload can be stored in this field. This can be useful for users who want to get the original payload.\n\
    \ Formats such as JFR are highly extensible and can contain more information than what is defined in this spec.\n\
    \ Inclusion of original payload should be configurable by the user. Default behavior should be to not include the original payload.\n\
    \ If the original payload is in pprof format, it SHOULD not be included in this field.\n\
    \ The field is optional, however if it is present `profile` MUST be present and contain the same profiling information.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ACK\ENQ\DC2\EOT\186\SOH\STX\a\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ACK\SOH\DC2\EOT\186\SOH\b\CAN\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\ACK\ETX\DC2\EOT\186\SOH\ESC\FS\n\
    \h\n\
    \\EOT\EOT\ETX\STX\a\DC2\EOT\189\SOH\STXB\SUBZ This is a reference to a pprof profile. Required, even when original_payload is present.\n\
    \\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\a\ACK\DC2\EOT\189\SOH\STX5\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\a\SOH\DC2\EOT\189\SOH6=\n\
    \\r\n\
    \\ENQ\EOT\ETX\STX\a\ETX\DC2\EOT\189\SOH@Ab\ACKproto3"