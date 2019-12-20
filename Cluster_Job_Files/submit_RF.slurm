#!/bin/bash
#SBATCH --job-name=rf_training
#SBATCH --time=12:00:00
#SBATCH --mail-user=tureen@umich.edu
#SBATCH --mail-type=END,FAIL,BEGIN
#SBATCH --mem=15g
#SBATCH --cpus-per-task=1

R CMD BATCH --no-save --no-restore train_RF.R
