#!/bin/bash
#SBATCH --job-name=redwing-actualrs   # Job name
#SBATCH --mail-type=ALL               # Mail events (NONE, BEGIN, END, FAIL, AL$
#SBATCH --mail-user=diaz.renata@ufl.edu   # Where to send mail
#SBATCH --account=ewhite
#SBATCH --qos=ewhite-b
#SBATCH --ntasks=1                 # Number of MPI ranks
#SBATCH --cpus-per-task=2            # Number of cores per MPI rank
#SBATCH --mem-per-cpu=8GB
#SBATCH --time=12:00:00       #Time limit hrs:min:sec
#SBATCH --output logs/pipeline%j.out
#SBATCH --error logs/pipeline%j.err

#This is a generic R submission script
module load R
Rscript pipeline-resim.R
