-- Delete the replay storage-migration jobs that 0137 seeds.
--
-- The rrweb/ key migration finished (0 of 25,909 sessions on a legacy layout),
-- so the MigrateReplayStorage and MigrateReplayStorageShard constructors were
-- removed from BackgroundJobs.BgJobs. 0137 still INSERTs rows tagged
-- MigrateReplayStorageShard — migrations are append-only, so it cannot be
-- edited — and every fresh database therefore ends up holding rows whose tag no
-- longer decodes. The job runner then fails each one with
--
--   Error in $.tag: parsing BackgroundJobs.BgJobs failed, expected tag field to be one of [...]
--
-- which is what turned CI red across all four shards the moment the
-- constructors went away. Running after 0137 in filename order, this clears
-- them for good; production's rows were deleted by hand before the constructors
-- were removed.
DELETE FROM background_jobs
WHERE payload ->> 'tag' IN ('MigrateReplayStorage', 'MigrateReplayStorageShard');
