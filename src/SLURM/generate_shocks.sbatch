#!/bin/bash
#SBATCH --job-name=shocks
#SBATCH -e "resultsSLURM/%x-%j.err"
#SBATCH -o "resultsSLURM/%x-%j.out"
#SBATCH --partition=cpuq
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --cpus-per-task=1
#SBATCH --time=120:00:00
#SBATCH --mem-per-cpu=256GB
#SBATCH --nodelist=cn03
#SBATCH --account=PR_climate

echo "start of job"
# Get the current time in seconds since the epoch and store it in a variable
start_time=$(date +%s)


DAY=100
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

id=$1
year=$2


echo ""
echo ----------------------------------------------
echo Calling
echo conda activate r_symi
echo Rscript shocks/create_input.R $year $id
echo 
echo conda activate my_xclim_env
echo ./shocks/create_input.py -id $id -yr $year
echo 
echo rm ../test/*intermediate*
echo ----------------------------------------------

# echo "Processing CSV"
# # # Set the IFS to ',' (comma) for CSV parsing
# IFS=','
# # # Replace 'your_file.csv' with the actual path to your CSV file
# csv_file="../res/event_id.csv"
        
# # Check if the file exists
# if [ ! -f "$csv_file" ]; then
# echo "File not found: $csv_file"
# exit 1
# fi

# Read the CSV file line by line
# while read -r col1 col2; 
# do
# echo srun --ntasks=1 --nodes=1 --cpus-per-task=$SLURM_CPUS_PER_TASK python ./clip_ext_wd.py -id $col1 -yr $col2
# done < "$csv_file"

conda activate r_symi
# The arguments needed for R script are: 1) name of the event file, and region (that could be extracted from the name file, to decide)
Rscript shocks/create_input.R $year $id

# conda deactivate
conda activate my_xclim_env
# The arguments needed for py script are: 1) name of the intermediate file computed at the previous step, and again, the region (that could be extracted from the name file, to decide)
./shocks/create_input.py -id $id -yr $year
# ./shocks/create_input.py -id 40863 -yr 2017

rm ../test/*intermediate*

wait


finish_time=$(date +%s)
echo ""
echo "The job was done in $((finish_time - start_time)) seconds"
echo ""
date
echo "end of job"


