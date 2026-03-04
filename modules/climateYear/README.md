After knitting and rendering `climateYear.Rmd`, overwrite this file with a symlink to `climateYear.md`.

E.g., on Linux/macOS from terminal:

```bash
cd /home/ieddy/git/climateGrowth/modules/climateYear
rm README.md && ln -s climateYear.md README.md
```
