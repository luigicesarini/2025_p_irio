#!/bin/bash
#SBATCH --job-name=shocks_array
#SBATCH -e "resultsSLURM/%x-%j.err"
#SBATCH -o "resultsSLURM/%x-%j.out"
#SBATCH --partition=cpuq
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --array=0-581
#SBATCH --cpus-per-task=1
#SBATCH --time=120:00:00
#SBATCH --mem-per-cpu=256GB
#SBATCH --nodelist=cn08
#SBATCH --account=PR_climate

echo "start of job"
# Get the current time in seconds since the epoch and store it in a variable
start_time=$(date +%s)


DAY=1000
echo "eliminating results slurm older than $DAY days"
date +'%Y-%m-%d %H:%M:%S'

DATE=$(date -d "$DAY days ago" +'%Y-%m-%d %H:%M:%S')


export MPLCONFIGDIR=/mnt/beegfs/lcesarini/tmp/mat
WORKDIR=$PWD
cd $WORKDIR
echo $WORKDIR
module purge
conda init bash
source /home/luigi.cesarini/.bashrc
conda activate my_xclim_env

find ./resultsSLURM/* -type f ! -newermt "$DATE" -exec rm {} +


# Load parameters from the list
PARAM_FILE="/mnt/beegfs/lcesarini/2025_p_irio/res/ids_yrs_events.txt"
LINE=$(sed -n "$((SLURM_ARRAY_TASK_ID + 1))p" "$PARAM_FILE")
PARAM1=$(echo $LINE | awk '{print $1}')
PARAM2=$(echo $LINE | awk '{print $2}')

echo ""
echo ----------------------------------------------
echo Calling
echo sbatch /mnt/beegfs/lcesarini/2025_p_irio/src/SLURM/generate_shock.sbatch $PARAM1 $PARAM2
echo ----------------------------------------------

# Submit the inner job with those parameters
sbatch /mnt/beegfs/lcesarini/2025_p_irio/src/SLURM/generate_shock.sbatch $PARAM1 $PARAM2


finish_time=$(date +%s)
echo ""
echo "The job was done in $((finish_time - start_time)) seconds"
echo ""
date
echo "end of job"


