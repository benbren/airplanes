#!/bin/bash
#SBATCH --job-name=lasso_train
#SBATCH --time=45:00
#SBATCH --mail-user=tureen@umich.edu
#SBATCH --mail-type=END,FAIL,BEGIN
#SBATCH --mem=10g
#SBATCH --cpus-per-task=1

R CMD BATCH --no-save --no-restore train_InterceptLasso.R
