{- This file was auto-generated from opentelemetry/proto/collector/profiles/v1experimental/profiles_service.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Opentelemetry.Proto.Collector.Profiles.V1experimental.ProfilesService (
        ProfilesService(..), ExportProfilesPartialSuccess(),
        ExportProfilesServiceRequest(), ExportProfilesServiceResponse()
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
import qualified Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Collector.Profiles.V1experimental.ProfilesService_Fields.rejectedProfiles' @:: Lens' ExportProfilesPartialSuccess Data.Int.Int64@
         * 'Proto.Opentelemetry.Proto.Collector.Profiles.V1experimental.ProfilesService_Fields.errorMessage' @:: Lens' ExportProfilesPartialSuccess Data.Text.Text@ -}
data ExportProfilesPartialSuccess
  = ExportProfilesPartialSuccess'_constructor {_ExportProfilesPartialSuccess'rejectedProfiles :: !Data.Int.Int64,
                                               _ExportProfilesPartialSuccess'errorMessage :: !Data.Text.Text,
                                               _ExportProfilesPartialSuccess'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExportProfilesPartialSuccess where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExportProfilesPartialSuccess "rejectedProfiles" Data.Int.Int64 where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportProfilesPartialSuccess'rejectedProfiles
           (\ x__ y__
              -> x__ {_ExportProfilesPartialSuccess'rejectedProfiles = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField ExportProfilesPartialSuccess "errorMessage" Data.Text.Text where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportProfilesPartialSuccess'errorMessage
           (\ x__ y__
              -> x__ {_ExportProfilesPartialSuccess'errorMessage = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExportProfilesPartialSuccess where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.collector.profiles.v1experimental.ExportProfilesPartialSuccess"
  packedMessageDescriptor _
    = "\n\
      \\FSExportProfilesPartialSuccess\DC2+\n\
      \\DC1rejected_profiles\CAN\SOH \SOH(\ETXR\DLErejectedProfiles\DC2#\n\
      \\rerror_message\CAN\STX \SOH(\tR\ferrorMessage"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        rejectedProfiles__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "rejected_profiles"
              (Data.ProtoLens.ScalarField Data.ProtoLens.Int64Field ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Int.Int64)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"rejectedProfiles")) ::
              Data.ProtoLens.FieldDescriptor ExportProfilesPartialSuccess
        errorMessage__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "error_message"
              (Data.ProtoLens.ScalarField Data.ProtoLens.StringField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional
                 (Data.ProtoLens.Field.field @"errorMessage")) ::
              Data.ProtoLens.FieldDescriptor ExportProfilesPartialSuccess
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, rejectedProfiles__field_descriptor),
           (Data.ProtoLens.Tag 2, errorMessage__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExportProfilesPartialSuccess'_unknownFields
        (\ x__ y__
           -> x__ {_ExportProfilesPartialSuccess'_unknownFields = y__})
  defMessage
    = ExportProfilesPartialSuccess'_constructor
        {_ExportProfilesPartialSuccess'rejectedProfiles = Data.ProtoLens.fieldDefault,
         _ExportProfilesPartialSuccess'errorMessage = Data.ProtoLens.fieldDefault,
         _ExportProfilesPartialSuccess'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExportProfilesPartialSuccess
          -> Data.ProtoLens.Encoding.Bytes.Parser ExportProfilesPartialSuccess
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
                                       "rejected_profiles"
                                loop
                                  (Lens.Family2.set
                                     (Data.ProtoLens.Field.field @"rejectedProfiles") y x)
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
          (do loop Data.ProtoLens.defMessage) "ExportProfilesPartialSuccess"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v
                  = Lens.Family2.view
                      (Data.ProtoLens.Field.field @"rejectedProfiles") _x
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
instance Control.DeepSeq.NFData ExportProfilesPartialSuccess where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExportProfilesPartialSuccess'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExportProfilesPartialSuccess'rejectedProfiles x__)
                (Control.DeepSeq.deepseq
                   (_ExportProfilesPartialSuccess'errorMessage x__) ()))
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Collector.Profiles.V1experimental.ProfilesService_Fields.resourceProfiles' @:: Lens' ExportProfilesServiceRequest [Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles.ResourceProfiles]@
         * 'Proto.Opentelemetry.Proto.Collector.Profiles.V1experimental.ProfilesService_Fields.vec'resourceProfiles' @:: Lens' ExportProfilesServiceRequest (Data.Vector.Vector Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles.ResourceProfiles)@ -}
data ExportProfilesServiceRequest
  = ExportProfilesServiceRequest'_constructor {_ExportProfilesServiceRequest'resourceProfiles :: !(Data.Vector.Vector Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles.ResourceProfiles),
                                               _ExportProfilesServiceRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExportProfilesServiceRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExportProfilesServiceRequest "resourceProfiles" [Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles.ResourceProfiles] where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportProfilesServiceRequest'resourceProfiles
           (\ x__ y__
              -> x__ {_ExportProfilesServiceRequest'resourceProfiles = y__}))
        (Lens.Family2.Unchecked.lens
           Data.Vector.Generic.toList
           (\ _ y__ -> Data.Vector.Generic.fromList y__))
instance Data.ProtoLens.Field.HasField ExportProfilesServiceRequest "vec'resourceProfiles" (Data.Vector.Vector Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles.ResourceProfiles) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportProfilesServiceRequest'resourceProfiles
           (\ x__ y__
              -> x__ {_ExportProfilesServiceRequest'resourceProfiles = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExportProfilesServiceRequest where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.collector.profiles.v1experimental.ExportProfilesServiceRequest"
  packedMessageDescriptor _
    = "\n\
      \\FSExportProfilesServiceRequest\DC2j\n\
      \\DC1resource_profiles\CAN\SOH \ETX(\v2=.opentelemetry.proto.profiles.v1experimental.ResourceProfilesR\DLEresourceProfiles"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        resourceProfiles__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "resource_profiles"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles.ResourceProfiles)
              (Data.ProtoLens.RepeatedField
                 Data.ProtoLens.Unpacked
                 (Data.ProtoLens.Field.field @"resourceProfiles")) ::
              Data.ProtoLens.FieldDescriptor ExportProfilesServiceRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, resourceProfiles__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExportProfilesServiceRequest'_unknownFields
        (\ x__ y__
           -> x__ {_ExportProfilesServiceRequest'_unknownFields = y__})
  defMessage
    = ExportProfilesServiceRequest'_constructor
        {_ExportProfilesServiceRequest'resourceProfiles = Data.Vector.Generic.empty,
         _ExportProfilesServiceRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExportProfilesServiceRequest
          -> Data.ProtoLens.Encoding.Growing.Growing Data.Vector.Vector Data.ProtoLens.Encoding.Growing.RealWorld Proto.Opentelemetry.Proto.Profiles.V1experimental.Profiles.ResourceProfiles
             -> Data.ProtoLens.Encoding.Bytes.Parser ExportProfilesServiceRequest
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
          "ExportProfilesServiceRequest"
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
instance Control.DeepSeq.NFData ExportProfilesServiceRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExportProfilesServiceRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExportProfilesServiceRequest'resourceProfiles x__) ())
{- | Fields :
     
         * 'Proto.Opentelemetry.Proto.Collector.Profiles.V1experimental.ProfilesService_Fields.partialSuccess' @:: Lens' ExportProfilesServiceResponse ExportProfilesPartialSuccess@
         * 'Proto.Opentelemetry.Proto.Collector.Profiles.V1experimental.ProfilesService_Fields.maybe'partialSuccess' @:: Lens' ExportProfilesServiceResponse (Prelude.Maybe ExportProfilesPartialSuccess)@ -}
data ExportProfilesServiceResponse
  = ExportProfilesServiceResponse'_constructor {_ExportProfilesServiceResponse'partialSuccess :: !(Prelude.Maybe ExportProfilesPartialSuccess),
                                                _ExportProfilesServiceResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show ExportProfilesServiceResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField ExportProfilesServiceResponse "partialSuccess" ExportProfilesPartialSuccess where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportProfilesServiceResponse'partialSuccess
           (\ x__ y__
              -> x__ {_ExportProfilesServiceResponse'partialSuccess = y__}))
        (Data.ProtoLens.maybeLens Data.ProtoLens.defMessage)
instance Data.ProtoLens.Field.HasField ExportProfilesServiceResponse "maybe'partialSuccess" (Prelude.Maybe ExportProfilesPartialSuccess) where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _ExportProfilesServiceResponse'partialSuccess
           (\ x__ y__
              -> x__ {_ExportProfilesServiceResponse'partialSuccess = y__}))
        Prelude.id
instance Data.ProtoLens.Message ExportProfilesServiceResponse where
  messageName _
    = Data.Text.pack
        "opentelemetry.proto.collector.profiles.v1experimental.ExportProfilesServiceResponse"
  packedMessageDescriptor _
    = "\n\
      \\GSExportProfilesServiceResponse\DC2|\n\
      \\SIpartial_success\CAN\SOH \SOH(\v2S.opentelemetry.proto.collector.profiles.v1experimental.ExportProfilesPartialSuccessR\SOpartialSuccess"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        partialSuccess__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "partial_success"
              (Data.ProtoLens.MessageField Data.ProtoLens.MessageType ::
                 Data.ProtoLens.FieldTypeDescriptor ExportProfilesPartialSuccess)
              (Data.ProtoLens.OptionalField
                 (Data.ProtoLens.Field.field @"maybe'partialSuccess")) ::
              Data.ProtoLens.FieldDescriptor ExportProfilesServiceResponse
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, partialSuccess__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _ExportProfilesServiceResponse'_unknownFields
        (\ x__ y__
           -> x__ {_ExportProfilesServiceResponse'_unknownFields = y__})
  defMessage
    = ExportProfilesServiceResponse'_constructor
        {_ExportProfilesServiceResponse'partialSuccess = Prelude.Nothing,
         _ExportProfilesServiceResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          ExportProfilesServiceResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser ExportProfilesServiceResponse
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
          (do loop Data.ProtoLens.defMessage) "ExportProfilesServiceResponse"
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
instance Control.DeepSeq.NFData ExportProfilesServiceResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_ExportProfilesServiceResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_ExportProfilesServiceResponse'partialSuccess x__) ())
data ProfilesService = ProfilesService {}
instance Data.ProtoLens.Service.Types.Service ProfilesService where
  type ServiceName ProfilesService = "ProfilesService"
  type ServicePackage ProfilesService = "opentelemetry.proto.collector.profiles.v1experimental"
  type ServiceMethods ProfilesService = '["export"]
  packedServiceDescriptor _
    = "\n\
      \\SIProfilesService\DC2\181\SOH\n\
      \\ACKExport\DC2S.opentelemetry.proto.collector.profiles.v1experimental.ExportProfilesServiceRequest\SUBT.opentelemetry.proto.collector.profiles.v1experimental.ExportProfilesServiceResponse\"\NUL"
instance Data.ProtoLens.Service.Types.HasMethodImpl ProfilesService "export" where
  type MethodName ProfilesService "export" = "Export"
  type MethodInput ProfilesService "export" = ExportProfilesServiceRequest
  type MethodOutput ProfilesService "export" = ExportProfilesServiceResponse
  type MethodStreamingType ProfilesService "export" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \Lopentelemetry/proto/collector/profiles/v1experimental/profiles_service.proto\DC25opentelemetry.proto.collector.profiles.v1experimental\SUB:opentelemetry/proto/profiles/v1experimental/profiles.proto\"\138\SOH\n\
    \\FSExportProfilesServiceRequest\DC2j\n\
    \\DC1resource_profiles\CAN\SOH \ETX(\v2=.opentelemetry.proto.profiles.v1experimental.ResourceProfilesR\DLEresourceProfiles\"\157\SOH\n\
    \\GSExportProfilesServiceResponse\DC2|\n\
    \\SIpartial_success\CAN\SOH \SOH(\v2S.opentelemetry.proto.collector.profiles.v1experimental.ExportProfilesPartialSuccessR\SOpartialSuccess\"p\n\
    \\FSExportProfilesPartialSuccess\DC2+\n\
    \\DC1rejected_profiles\CAN\SOH \SOH(\ETXR\DLErejectedProfiles\DC2#\n\
    \\rerror_message\CAN\STX \SOH(\tR\ferrorMessage2\201\SOH\n\
    \\SIProfilesService\DC2\181\SOH\n\
    \\ACKExport\DC2S.opentelemetry.proto.collector.profiles.v1experimental.ExportProfilesServiceRequest\SUBT.opentelemetry.proto.collector.profiles.v1experimental.ExportProfilesServiceResponse\"\NULB\204\SOH\n\
    \8io.opentelemetry.proto.collector.profiles.v1experimentalB\DC4ProfilesServiceProtoP\SOHZ@go.opentelemetry.io/proto/otlp/collector/profiles/v1experimental\170\STX5OpenTelemetry.Proto.Collector.Profiles.V1ExperimentalJ\217\ETB\n\
    \\ACK\DC2\EOT\SO\NULM\SOH\n\
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
    \\SOH\STX\DC2\ETX\DLE\NUL>\n\
    \\t\n\
    \\STX\ETX\NUL\DC2\ETX\DC2\NULD\n\
    \\b\n\
    \\SOH\b\DC2\ETX\DC4\NULR\n\
    \\t\n\
    \\STX\b%\DC2\ETX\DC4\NULR\n\
    \\b\n\
    \\SOH\b\DC2\ETX\NAK\NUL\"\n\
    \\t\n\
    \\STX\b\n\
    \\DC2\ETX\NAK\NUL\"\n\
    \\b\n\
    \\SOH\b\DC2\ETX\SYN\NULQ\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX\SYN\NULQ\n\
    \\b\n\
    \\SOH\b\DC2\ETX\ETB\NUL5\n\
    \\t\n\
    \\STX\b\b\DC2\ETX\ETB\NUL5\n\
    \\b\n\
    \\SOH\b\DC2\ETX\CAN\NULW\n\
    \\t\n\
    \\STX\b\v\DC2\ETX\CAN\NULW\n\
    \\178\SOH\n\
    \\STX\ACK\NUL\DC2\EOT\FS\NUL \SOH\SUB\165\SOH Service that can be used to push profiles between one Application instrumented with\n\
    \ OpenTelemetry and a collector, or between a collector and a central collector.\n\
    \\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX\FS\b\ETB\n\
    \y\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX\US\STXU\SUBl For performance reasons, it is recommended to keep this RPC\n\
    \ alive for the entire life of the application.\n\
    \\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX\US\ACK\f\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX\US\r)\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX\US4Q\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\"\NUL)\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\"\b$\n\
    \\211\STX\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX(\STX^\SUB\197\STX An array of ResourceProfiles.\n\
    \ For data coming from a single resource this array will typically contain one\n\
    \ element. Intermediary nodes (such as OpenTelemetry Collector) that receive\n\
    \ data from multiple origins typically batch the data before forwarding further and\n\
    \ in that case this array will contain multiple elements.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\EOT\DC2\ETX(\STX\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ACK\DC2\ETX(\vG\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX(HY\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX(\\]\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT+\NUL<\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX+\b%\n\
    \\148\ACK\n\
    \\EOT\EOT\SOH\STX\NUL\DC2\ETX;\STX3\SUB\134\ACK The details of a partially successful export request.\n\
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
    \\ENQ\EOT\SOH\STX\NUL\ACK\DC2\ETX;\STX\RS\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\SOH\DC2\ETX;\US.\n\
    \\f\n\
    \\ENQ\EOT\SOH\STX\NUL\ETX\DC2\ETX;12\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT>\NULM\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX>\b$\n\
    \\146\SOH\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETXC\STX\RS\SUB\132\SOH The number of rejected profiles.\n\
    \\n\
    \ A `rejected_<signal>` field holding a `0` value indicates that the\n\
    \ request was fully accepted.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETXC\STX\a\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETXC\b\EM\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETXC\FS\GS\n\
    \\159\ETX\n\
    \\EOT\EOT\STX\STX\SOH\DC2\ETXL\STX\ESC\SUB\145\ETX A developer-facing human-readable message in English. It should be used\n\
    \ either to explain why the server rejected parts of the data during a partial\n\
    \ success or to convey warnings/suggestions during a full success. The message\n\
    \ should offer guidance on how users can address such issues.\n\
    \\n\
    \ error_message is an optional field. An error_message with an empty value\n\
    \ is equivalent to it not being set.\n\
    \\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ENQ\DC2\ETXL\STX\b\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\SOH\DC2\ETXL\t\SYN\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\SOH\ETX\DC2\ETXL\EM\SUBb\ACKproto3"