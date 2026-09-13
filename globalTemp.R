options(repos = c(getOption("repos"), PE = "https://predictiveecology.r-universe.dev/"))
if (!require("pak")) install.packages("pak")
pak::pak(c("Require",
           "SpaDES.project"), ask = FALSE)
Require::Require("SpaDES.project", install = FALSE)

# PROJECT LOCATION -----
## please choose where you want the project directory to be placed in your machine.
projLocation <- "~/Documents/tests"


out <- setupProject(
  name = "testProject",
  paths = list(projectPath = file.path(projLocation, "testProject")),
  modules = c(
    "PredictiveEcology/testNestedModule@main/nestedModule"
  ),
  Restart = TRUE,
  useGit = FALSE
)
