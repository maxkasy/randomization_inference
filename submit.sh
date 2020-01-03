#!/bin/bash
#SBATCH --nodes=1
#SBATCH --time=24:00:00
#SBATCH --job-name=randomization_inference
#SBATCH --mail-type=ALL
#SBATCH --mail-user=maxkasy@gmail.com
module purge
module load R/3.5.3
Rscript randomization_inference.R