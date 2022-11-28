#!/bin/bash

#Add Bin bash!!!!! (EDIT: added)
#SBATCH --job-name=dg376_mcfm
#SBATCH -o /scratch/dp032/dc-gill1/run_1_output/stdout_lo_0t1000_5%j
#SBATCH -e /scratch/dp032/dc-gill1/run_1_output/stdout_lo_0t1000_5%j
#SBATCH --account=dp032
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=90
#SBATCH --exclusive
#SBATCH --time=50:00:00
#SBATCH --mail-type=ALL

# Remove all previously loaded modules.
module purge
module load gcc
# Load your desired modules here.


# we set OMP_NUM_THREADS to the number of available cores
./dat2gnu.py ./mcfm dat_0-14000_nocut_dim8O1/ WWqqbr_nlo_PDF4LHC_1.00_1.00_

# we execute the job


# Finish the script and Exit.
exit 0
