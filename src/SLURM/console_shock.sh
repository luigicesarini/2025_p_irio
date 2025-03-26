##!/bin/bash

id=40327
year=1975

# For each Event, with givend ID, and region we perform the following steps:
# 1. Run the Rscripts that generates the intermediate hit addetti by sector
# 2. Run the py scripts that generates the shopcks in percentage for each sector
# # Set the IFS to ',' (comma) for CSV parsing
IFS=','
# # Replace 'your_file.csv' with the actual path to your CSV file
csv_file="../res/event_id.csv"
        
# Check if the file exists
if [ ! -f "$csv_file" ]; then
echo "File not found: $csv_file"
exit 1
fi

Read the CSV file line by line
while read -r col1 col2; 
do
# echo srun --ntasks=1 --nodes=1 --cpus-per-task=$SLURM_CPUS_PER_TASK python ./clip_ext_wd.py -id $col1 -yr $col2

sbatch SLURM/generate_shocks.sbatch $col1 $col2
done < "$csv_file"











