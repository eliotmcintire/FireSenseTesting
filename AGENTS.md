# FireSenseTesting — project notes

FireSense ELF fitting runs on a 15-worker tmux fleet. `/home/emcintir/GitHub/FireSenseTesting`.

## Entry points
| File | Purpose |
|---|---|
| `global.R` | `setupProject()`: paths, options, packages, modules, params. Every worker sources it per job. |
| `expt.R` | Launcher. Installs packages ONCE, `preRunSetupProject(upTo = "params")`, builds `expt`, calls `experimentTmux()`. |
| `predictExpt.R`, `multi.R`, `run.R`, `app.R` | Predict-side / other drivers. |

Run `expt.R` **at a command prompt inside tmux**, not from the IDE.

Package installs belong in `expt.R` only. `global.R` sets `spades.useRequire = FALSE` for workers
(`experimentTmux` exports `SPADES_USE_REQUIRE=false`); on 2026-09-09 concurrent installs into the
shared library corrupted SpaDES.tools and killed four workers. Tracked-from-`development` packages
are pak-installed at the top of `expt.R` because `setupProject`'s `packages` list uses version
FLOORS, which never move once satisfied (this is why stale fireSenseUtils/SpaDES.tools silently
persisted for days).

## Paths (global.R ~L121)
- `cachePath = "/mnt/fast/cache"` (`/mnt/shared_cache/cache` commented out), `scratchPath = "/mnt/fast/scratch"`,
  `inputPath = /mnt/fast/inputs/<.ELFind>/...`, `reproducible.destinationPathShared = "/mnt/fast/data"`.
- `outputPath = pathBuild(.ELFind, .samplingRange, .GCM, .SSP, .rep)`; `runName` is that path relative, `/`→`_`.
- Project-local `cache/` (~460M) and `SpaDES/cache/` also exist.
- `reproducible.cloudFolderID = "1oNGYVAV3goXfSzD1dziotKGCdO8P_iV9"`; Drive auth from `~/googledriveAuthentication.R`.
- Cache format qs2. `LandR.assertions = FALSE`, `reproducible.useCOG = FALSE`, `showSimilar` off for batch.

There is deliberately **no** `.studyAreaName` dot in `setupProject()` — a `...` arg referencing another
`...` arg does not resolve. Refer to `.ELFind` directly at each use site. The study area name must stay
the bare ELF id: `fireSense_SpreadFit` keys the shared cloud fit ledger on it.

## Queue mechanics
`experiment_queue_fits_<date>.rds` + a Google Sheet of the same name (no `.rds`), in folder
`1X9-mRjyLMNpgkP_cfqhbr_AQEPOsVCHf`. Data frame, one row per ELF × rep, 14 columns:
`.ELFind`, `.rep`, `status`, `claimed_by`, `started_at`, `finished_at`, `DEoptimElapsedTime`,
`machine_name`, `process_id`, `heartbeat_at`, `heartbeat_iter`, `iterationsTotal`, `interrupted_at`,
`last_error`. Status values: `PENDING`, `RUNNING`, `DONE`, `INTERRUPTED`, `QUARANTINED`.

`experimentTmux` materializes the queue from `expt` **only when `queue_path` does not exist** — reusing an
old name silently keeps its old rows and ignores `expt`. Use a new file name for a new run.

`expt.R` derives work from the cloud ledger: `runELFs(outs, "allNames")` minus
`runELFs(outs, "fittedNamesOnly")`, so re-running cannot re-fit something already done. It then orders
rows (`top`, `problematic`, `dataBlocked`, `firstRuns`).

## Worker fleet and watchers (`R/`)
- `reviveParkedWorkers.sh [session:window] [interval] [max_per_pane]` — a job error parks the worker at an
  R prompt; this **respawns** it (retry re-runs the same failing job). Picks the startup script from the
  live process's `R_PROFILE_USER`, not the pane index (tmux renumbers panes).
- `quarantineFailedELFs.sh [interval] [queue.rds]` — moves rows with a `last_error` that are PENDING/
  INTERRUPTED to QUARANTINED, never touching RUNNING. Triage, not a fix.
- `workerResources.R` → `logs/workerResources_*.csv`; `reclaimStrayClaim.R`, `pruneColdCache.R`,
  `dedupHardlinks.R`, `cleanTerraScratch.R`, `watchRun.sh`, `functions.R`.
- tmux sessions: `fits` (the fleet), `agediag`. Workers run from per-worker library snapshots
  (`worker_respawn.R`), so upgrading the shared library does not affect already-idle sessions.

## Modules
Git submodules under `modules/`: Biomass_{borealDataPrep, core, regeneration, speciesData,
speciesParameters, summary}, burnSummaries, canClimateData, climateYear, fireSense,
fireSense_{dataPrepFit, dataPrepPredict, ELFs, IgnitionFit, IgnitionPredict, SpreadFit, SpreadPredict,
summary}, NRV_summary. Sources are clones under `/home/emcintir/GitHub/`; fix defects in the package or
module repo with a regression test, not in the calling script.

## Defunct modules (Eliot, 2026-09-11)
**fireSense_EscapeFit and fireSense_EscapePredict are essentially defunct and will soon be
ARCHIVED.** Do not spend effort on them: no CI fixes, no pkgdown, no release chasing. Their
known-red checks (EscapePredict's hand-rolled "Render module Rmd" pinned at
`install-Require@v0.2`, achubaty's stale repoint PRs) are moot. EscapeFit 1.0.0 was tagged and
released on 2026-09-11 before this was known — harmless, but no follow-up is needed.

## Known cache trap (verified 2026-09-11)
`reproducible::Cache` digests only the called function's own body, not its callees. A cached call whose
**package-level callee** changed still HITS the stale result. Fix: put the inner function in
`.cacheExtra`, e.g. `.cacheExtra = list(LandR::prepInputs_NTEMS_LCC_FAO)` (Eliot's decision; not package
versions). Separately, `fireSense_dataPrepFit:1338` digested bare `dir()` names — fixed by PR #27
(`moduleCodeDigest()` with `full.names`).

## Session logs
`~/claudeSessions/2026-09-04-spreadfit-elf-cluster/` (fleet, queue, per-ELF failures) and
`~/claudeSessions/completed/2026-09-11-fire-data-1985-rebuild/` (fire years, NFDB/NBAC → fireregimetools, land
cover per data year, ELF fire gate/merges, IgnitionFit folds). `summary.md` is the entry point in each.
