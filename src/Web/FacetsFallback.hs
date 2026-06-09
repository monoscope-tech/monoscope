-- | On-the-fly facets fallback when 'apis.facet_summaries' has no row for a
-- requested field. Lives in its own module because importing
-- @Hasql.Decoders@ / @Hasql.DynamicStatements@ alongside the
-- @DerivingVia DAE.Snake@ machinery in "Web.ApiHandlers" breaks Generics
-- derivation there.
module Web.FacetsFallback (facetsFallback) where

import Relude

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Effectful.Hasql qualified as Hasql
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 qualified
import Hasql.Decoders qualified as HD
import Hasql.DynamicStatements.Snippet (toPreparableStatement)
import Models.Projects.Projects qualified as Projects
import Pkg.Parser.Expr qualified as ParserExpr
import System.Types (ATBaseCtx)


-- | Run a bounded @GROUP BY@ on the live @otel_logs_and_spans@ table for one
-- whitelisted column. Returns @{ <field>: [{value, count}, ...], "source":
-- "fallback" }@ — the @source@ tag lets the CLI surface "computed on-the-fly,
-- the catalog hasn't built up samples yet."
--
-- Defensive: only admits fields already present in the introspected column
-- set (C1) so the user can't pass an arbitrary identifier. Bounded to 50
-- values inside the @from..to@ window already validated by the caller.
facetsFallback
  :: Projects.ProjectId -> Text -> UTCTime -> UTCTime -> ATBaseCtx AE.Value
facetsFallback pid field fromT toT
  | not (field `S.member` ParserExpr.flattenedOtelAttributes)
  , not (field `S.member` ParserExpr.topLevelOtelColumns) =
      -- Reject unknown fields silently (return empty) so a fishing attempt
      -- can't probe for column existence.
      pure (AE.object [])
  | otherwise = do
      -- HI.sql is a QuasiQuoter so we can't splice the column name through
      -- it. The column ref is whitelisted; UUID and ISO 8601 timestamps
      -- stringify into safe SQL literals.
      let col = T.replace "." "___" field
          colRef = "\"" <> col <> "\""
          tsLit t = "'" <> toText (Data.Time.Format.ISO8601.iso8601Show t) <> "'::timestamptz"
          pidLit = "'" <> pid.toText <> "'::uuid"
          snippet =
            "SELECT ("
              <> fromString (toString colRef)
              <> ")::text, COUNT(*)::bigint FROM public.otel_logs_and_spans WHERE project_id = "
              <> fromString (toString pidLit)
              <> " AND timestamp >= "
              <> fromString (toString (tsLit fromT))
              <> " AND timestamp < "
              <> fromString (toString (tsLit toT))
              <> " AND ("
              <> fromString (toString colRef)
              <> ") IS NOT NULL GROUP BY 1 ORDER BY 2 DESC LIMIT 50"
          rowDecoder :: HD.Row (Text, Int64)
          rowDecoder =
            (,)
              <$> HD.column (HD.nonNullable HD.text)
              <*> HD.column (HD.nonNullable HD.int8)
      rows <- Hasql.statement () (toPreparableStatement snippet (HD.rowList rowDecoder))
      let values =
            AE.toJSON
              [AE.object ["value" AE..= v, "count" AE..= c] | (v, c) <- rows]
      pure
        $ AE.object
          [ AEK.fromText field AE..= values
          , "source" AE..= ("fallback" :: Text)
          ]
