# FireSenseTesting

## Run scripts
- Every FireSense run uses one pair: `global_draft.R` (one job; workers source it per job) and `expt_draft.R`
  (installs packages, builds the queue, launches tmux or Slurm workers). Both are tracked here. Do not create
  or edit other copies (`global.R`, `expt.R`, `*_old.R` are retired; the pair will later be renamed to them).
- What differs per machine (basePath, spread-fit hosts, maxWorkersPerHost, mc.cores) is in `site.R`, read from
  the current folder. Run from the project folder; never put host or folder paths into the scripts.
- `~/GitHub/FireSenseTesting_burnable` is a second project folder with its own library, modules/ and outputs;
  its `global_draft.R`, `expt_draft.R` and `site.R` are symlinks to the files here. A branch checkout here changes
  what its workers run.
- The GPSC copy is not a git repo: sync the three files with copy + mv, never edit in place.
- Settings reach a job as queue columns (defaultDots), not environment variables read in `global_draft.R`.
