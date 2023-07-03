perms <- 1e6L

for (r in 1:60) {
  jobname <- sprintf("CP%02d.sh", r)
  f <- file(jobname)
  writeLines(c(
    paste("#!/bin/bash"),
    paste("#SBATCH -J", jobname),
    paste("#SBATCH -A liu-2018-29"),
    paste("#SBATCH -t 24:00:00"),
    paste("#SBATCH --mem=16000"),
    paste("#SBATCH -n 1"),
    paste("# "),
    paste("# Run single task in foreground"),
    paste("module add R/4.2.2-nsc1-gcc-11.3.0-bare"),
    paste("cd", Sys.getenv("PWD")),
    paste("Rscript code/generate-perm-table-cluster-global.R", r, perms),
    paste("# "),
    paste("# Script ends here")
  ), f)
  close(f)
  system(paste("sbatch", jobname))
  Sys.sleep(0.1)
}
