module Data.Effectful.CodeReview.Outbox (Scope (..), Receipt (..), Outbox, outboxPath, withOutbox, replay, acknowledge, deliverPhase) where

import Control.Monad (foldM)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AET
import Data.Effectful.CodeReview.Artifacts (ArtifactStore (..), readPrivateArtifact, writePrivateArtifact)
import Data.Effectful.CodeReview.Types
import Data.UUID.V4 qualified as UUID4
import Pkg.DeriveUtils (UUIDId (..))
import Relude
import System.Directory (canonicalizePath, createDirectoryIfMissing, listDirectory, removeFile)
import System.FileLock (SharedExclusive (..), withFileLock)
import System.FilePath (takeExtension, (</>))
import System.IO.Error (userError)
import System.Posix.Files qualified as Posix
import System.Posix.IO qualified as Posix
import System.Posix.Unistd (fileSynchronise)
import UnliftIO.Exception (bracket, throwIO)


data Scope = Advisory (UUIDId "pr_review") | IssueFix (UUIDId "issue_fix") | Admission (UUIDId "issue_fix") | PeerReview (UUIDId "issue_fix")


data Receipt = Call ModelCall | Usage ModelUsage | Allocation AllocationReceipt | Check CheckReceipt | Phase PhaseReceipt
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Outbox = Outbox {path :: FilePath, scope :: Scope}


outboxPath :: Outbox -> FilePath
outboxPath = (.path)


withOutbox :: ArtifactStore -> Scope -> (Outbox -> IO a) -> IO a
withOutbox store scope action = do
  root <- canonicalizePath store.usageDir
  let parts = case scope of
        Advisory run -> [toString run.toText]
        IssueFix run -> ["fixes", toString run.toText]
        Admission run -> ["admissions", toString run.toText]
        PeerReview run -> ["fix-reviews", toString run.toText]
  directory <-
    foldM
      ( \parent part -> do
          let child = parent </> part
          createDirectoryIfMissing False child
          canonicalizePath child >>= ensure . (== child)
          Posix.setFileMode child 0o700
          sync parent
          pure child
      )
      root
      parts
  withFileLock (directory </> ".native.lock") Exclusive $ const $ action $ Outbox directory scope


-- Decode the whole run before any callback; malformed entries stay available for repair.
replay :: Outbox -> (Receipt -> IO (Either Text ())) -> IO (Either Text ())
replay outbox record = do
  files <- filter ((== ".json") . takeExtension) <$> listDirectory outbox.path
  receipts <- traverse (readReceipt outbox) files
  runExceptT $ traverse_ (\receipt -> ExceptT $ acknowledge outbox receipt record) $ sortOn (\receipt -> (rank receipt, receiptId receipt)) receipts
  where
    rank = \case Call _ -> 0 :: Int; Phase _ -> 2; _ -> 1


-- Every producer journals before delivery; the database must accept before unlink/ACK.
acknowledge :: Outbox -> Receipt -> (Receipt -> IO (Either Text ())) -> IO (Either Text ())
acknowledge outbox receipt record = do
  let filename = toString (receiptId receipt) <> ".json"
  stored <- readReceipt outbox filename
  ensure $ AE.toJSON stored == AE.toJSON receipt
  result <- record receipt
  for_ result $ \() -> removeFile (outbox.path </> filename) >> sync outbox.path
  pure result


deliverPhase :: Outbox -> UUIDId "agent_attempt" -> Step -> (Receipt -> IO (Either Text ())) -> IO (Either Text ())
deliverPhase outbox attempt step record = do
  ensure $ case outbox.scope of IssueFix _ -> True; _ -> False
  identifier <- UUIDId <$> UUID4.nextRandom
  let receipt = Phase $ PhaseReceipt identifier attempt step
  writePrivateArtifact outbox.path (toString identifier.toText <> ".json") $ AE.encode receipt <> "\n"
  acknowledge outbox receipt record


readReceipt :: Outbox -> FilePath -> IO Receipt
readReceipt outbox filename = do
  receipt <- either (const invalid) pure . (AE.eitherDecodeStrict >=> AET.parseEither storedReceipt) =<< readPrivateArtifact outbox.path filename (2 * 1048576)
  ensure $ filename == toString (receiptId receipt) <> ".json"
  ensure $ case (outbox.scope, receipt) of
    (IssueFix _, _) -> True
    (_, Phase _) -> False
    (Advisory _, _) -> True
    (_, Check _) -> False
    _ -> True
  pure receipt
  where
    -- Only files accept legacy untagged usage; the live wire remains a tagged sum.
    storedReceipt value = AE.withObject "Stored receipt" (\fields -> if KM.member "tag" fields then AE.parseJSON value else Usage <$> AE.parseJSON value) value


receiptId :: Receipt -> Text
receiptId = \case
  Call receipt -> receipt.id.toText
  Usage receipt -> receipt.id.toText
  Allocation receipt -> (allocationId receipt).toText
  Check receipt -> receipt.id.toText
  Phase receipt -> receipt.id.toText


sync :: FilePath -> IO ()
sync path = bracket (Posix.openFd path Posix.ReadOnly Posix.defaultFileFlags{Posix.nofollow = True, Posix.cloexec = True}) Posix.closeFd fileSynchronise


ensure :: Bool -> IO ()
ensure condition = unless condition invalid


invalid :: IO a
invalid = throwIO $ userError "Invalid durable agent receipt"
