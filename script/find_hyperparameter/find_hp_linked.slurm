#!/bin/bash

#############################
# les directives Slurm vont ici:

# Your job name (displayed by the queue)
#SBATCH -J JSSCOVIDiter10linkTRUE

# walltime (hh:mm::ss)
#SBATCH -t 01:30:00


# change working directory
# SBATCH --chdir=.

### In filenames, %j=jobid, %a=index in job array
#SBATCH -o data/JSSCOVIDiter20linkTRUE_%j_%a.out # standard out goes to this file
#SBATCH -e data/JSSCOVIDiter20linkTRUE_%j_%a.err # standard err goes to this file

#SBATCH --array=1-400

# fin des directives PBS
#############################

# useful informations to print
echo "#############################"
echo "User:" $USER
echo "Date:" `date`
echo "Host:" `hostname`
echo "Directory:" `pwd`
echo "SLURM_JOBID:" $SLURM_JOBID
echo "SLURM_SUBMIT_DIR:" $SLURM_SUBMIT_DIR
echo "SLURM_JOB_NODELIST:" $SLURM_JOB_NODELIST
echo "#############################"

#############################

module load R/4.2.0

Rscript script/find_hyperparameter/esn_find_hp.R
