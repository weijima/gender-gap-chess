perms <- 1e6L

# Number of parameter combinations (rows) in generate-perm-table-cluster*.R:
# juniors (2) x inactives (2) x rating floors (17: 1000 to 1800 by 50) x metrics (5)
rows <- 2 * 2 * length(seq(1000, 1800, by = 50)) * 5

for (r in 1:rows) {
  jobname <- sprintf("CP%03d.sh", r)
  f <- file(jobname)
  writeLines(c(
    paste("#!/bin/bash"),
    paste("#SBATCH -J", jobname),
    paste("#SBATCH -A naiss2026-4-1561-cpu"),
    paste("#SBATCH -t 02:00:00"),
    paste("#SBATCH --mem=8000"),
    paste("#SBATCH -n 1"),
    paste("# "),
    paste("# Run single task in foreground"),
    paste("module add Miniforge/26.3.2-2-eb"),
    paste("mamba activate R"),
    paste("cd", Sys.getenv("PWD")),
    paste("Rscript code/generate-perm-table-cluster.R", r, perms),
    paste("# "),
    paste("# Script ends here")
  ), f)
  close(f)
  system(paste("sbatch", jobname))
  Sys.sleep(0.1)
}
