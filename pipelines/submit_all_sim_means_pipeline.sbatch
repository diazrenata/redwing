#!/bin/bash
#SBATCH --job-name=redwing-all   # Job name
#SBATCH --mail-type=ALL               # Mail events (NONE, BEGIN, END, FAIL, AL$
#SBATCH --mail-user=diaz.renata@ufl.edu   # Where to send mail
#SBATCH --account=ewhite
#SBATCH --qos=ewhite-b
#SBATCH --ntasks=1                 # Number of MPI ranks
#SBATCH --cpus-per-task=12            # Number of cores per MPI rank
#SBATCH --mem-per-cpu=16GB
#SBATCH --time=6:00:00       #Time limit hrs:min:sec
#SBATCH --output ../drake_caches/logs/all_sm%j.out
#SBATCH --error ../drake_caches/logs/all_sm%j.err

#This is a generic R submission script
module load R
Rscript pipeline_sim_means_all.R
