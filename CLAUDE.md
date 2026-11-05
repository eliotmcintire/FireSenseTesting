# FireSenseTesting

## Run scripts
- Every FireSense run uses one pair: `global.R` (one job; workers source it per job) and `expt.R`
  (installs packages, builds the queue, launches tmux or Slurm workers). Both are tracked here. Do not create
  or edit other copies (`*_old.R` are retired). Until 2026-11-05 the pair was `global_draft.R` + `expt_draft.R`; on
  the GPSC those names remain as symlinks to the pair while jobs launched under them finish.
- What differs per machine (basePath, spread-fit hosts, maxWorkersPerHost, mc.cores) is in `site.R`, read from
  the current folder. Run from the project folder; never put host or folder paths into the scripts.
- `~/GitHub/FireSenseTesting_burnable` is a second project folder with its own library, modules/ and outputs;
  its `global.R`, `expt.R` and `site.R` are symlinks to the files here. A branch checkout here changes
  what its workers run.
- The GPSC copy is not a git repo: sync the three files with copy + mv, never edit in place.
- Settings reach a job as queue columns (defaultDots), not environment variables read in `global.R`.
