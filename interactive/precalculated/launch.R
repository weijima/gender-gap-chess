bash_script <- function(jobname, projectID, time, memory, runstr, launch) {
  script <- paste0("#!/bin/bash\n",
                   "#SBATCH -J ", jobname, "\n",
                   "#SBATCH -A ", projectID, "\n",
                   "#SBATCH -t ", time, "\n",
                   "#SBATCH --mem=", memory, "\n",
                   "#SBATCH -n 1\n",
                   "# \n",
                   "# Run single task in foreground\n",
                   "module add R/4.0.3-nsc1-gcc-7.3.0\n",
                   "cd ", Sys.getenv("PWD"), "\n",
                   "Rscript ", runstr, "\n",
                   "# \n",
                   "# Script ends here")
  write(script, jobname)
  if (launch) {
    system(paste("sbatch", jobname))
    Sys.sleep(0.01)
  }
}


df <- data.frame(imin = as.integer(seq(1, 1320, by = 5)))
df$imax = as.integer(df$imin + 4)

for (i in 1:nrow(df)) {
  bash_script(sprintf("GiC%03d.sh", i), "liu-2018-29", "10:00:00", "16000",
              paste("process_data.R", df$imin[i], df$imax[i]), TRUE)
}
