##!/bin/bash

# id=40327
# year=1975

# # For each Event, with givend ID, and region we perform the following steps:
# # 1. Run the Rscripts that generates the intermediate hit addetti by sector
# # 2. Run the py scripts that generates the shopcks in percentage for each sector
# # # Set the IFS to ',' (comma) for CSV parsing
# IFS=','
# # # Replace 'your_file.csv' with the actual path to your CSV file
# csv_file="../res/events/event_id.csv"
        
# # Check if the file exists
# if [ ! -f "$csv_file" ]; then
# echo "File not found: $csv_file"
# exit 1
# fi

# Read the CSV file line by line
# while read -r col1 col2; 
# do
# # echo srun --ntasks=1 --nodes=1 --cpus-per-task=$SLURM_CPUS_PER_TASK python ./clip_ext_wd.py -id $col1 -yr $col2

# sbatch SLURM/generate_shocks.sbatch $col1 $col2
# done < "$csv_file"





# # Split the file into chunks of 64 lines each
# split -l 64 $csv_file chunk_

# # Process each chunk
# counter=0
# for chunk in chunk_*; do
    
#     cat $chunk | while IFS=, read -r col1 col2; do
#         # echo srun --ntasks=1 --nodes=1 --cpus-per-task=$SLURM_CPUS_PER_TASK python ./clip_ext_wd.py -id $col1 -yr $col2
#         echo sbatch SLURM/generate_shocks.sbatch $col1 $col2 >> SLURM/txt/list_commands_$counter.txt
        
#     done
#     wait
#     rm "$chunk"
#     counter=$((counter + 1))
# done


sbatch SLURM/generate_shocks.sbatch 40001 1950 &
sbatch SLURM/generate_shocks.sbatch 40008 1950 &
sbatch SLURM/generate_shocks.sbatch 40010 1950 &
sbatch SLURM/generate_shocks.sbatch 40006 1950 &
sbatch SLURM/generate_shocks.sbatch 40003 1950 &
sbatch SLURM/generate_shocks.sbatch 40002 1950 &
sbatch SLURM/generate_shocks.sbatch 40007 1950 &
sbatch SLURM/generate_shocks.sbatch 40013 1951 &
sbatch SLURM/generate_shocks.sbatch 40014 1951 &
sbatch SLURM/generate_shocks.sbatch 40015 1951 &
sbatch SLURM/generate_shocks.sbatch 40020 1951 &
sbatch SLURM/generate_shocks.sbatch 40021 1951 &
sbatch SLURM/generate_shocks.sbatch 40018 1951 &
sbatch SLURM/generate_shocks.sbatch 40025 1951 &
sbatch SLURM/generate_shocks.sbatch 40027 1951 &
sbatch SLURM/generate_shocks.sbatch 40017 1951 &
sbatch SLURM/generate_shocks.sbatch 40026 1951 &
sbatch SLURM/generate_shocks.sbatch 40012 1951 &
sbatch SLURM/generate_shocks.sbatch 40011 1951 &
sbatch SLURM/generate_shocks.sbatch 40031 1952 &
sbatch SLURM/generate_shocks.sbatch 40030 1952 &
sbatch SLURM/generate_shocks.sbatch 40037 1952 &
sbatch SLURM/generate_shocks.sbatch 40034 1952 &
sbatch SLURM/generate_shocks.sbatch 40029 1952 &
sbatch SLURM/generate_shocks.sbatch 40038 1952 &
sbatch SLURM/generate_shocks.sbatch 40033 1952 &
sbatch SLURM/generate_shocks.sbatch 40035 1952 &
sbatch SLURM/generate_shocks.sbatch 40036 1952 &
sbatch SLURM/generate_shocks.sbatch 40055 1953 &
sbatch SLURM/generate_shocks.sbatch 40052 1953 &
sbatch SLURM/generate_shocks.sbatch 40050 1953 &
sbatch SLURM/generate_shocks.sbatch 40048 1953 &
sbatch SLURM/generate_shocks.sbatch 40046 1953 &
sbatch SLURM/generate_shocks.sbatch 40040 1953 &
sbatch SLURM/generate_shocks.sbatch 40053 1953 &
sbatch SLURM/generate_shocks.sbatch 40047 1953 &
sbatch SLURM/generate_shocks.sbatch 40045 1953 &
sbatch SLURM/generate_shocks.sbatch 40041 1953 &
sbatch SLURM/generate_shocks.sbatch 40054 1953 &
sbatch SLURM/generate_shocks.sbatch 40042 1953 &
sbatch SLURM/generate_shocks.sbatch 40060 1954 &
sbatch SLURM/generate_shocks.sbatch 40065 1954 &
sbatch SLURM/generate_shocks.sbatch 40067 1954 &
sbatch SLURM/generate_shocks.sbatch 40059 1954 &
sbatch SLURM/generate_shocks.sbatch 40058 1954 &
sbatch SLURM/generate_shocks.sbatch 40066 1954 &
sbatch SLURM/generate_shocks.sbatch 40056 1954 &
sbatch SLURM/generate_shocks.sbatch 40062 1954 &
sbatch SLURM/generate_shocks.sbatch 40063 1954 &
sbatch SLURM/generate_shocks.sbatch 40064 1954 &
sbatch SLURM/generate_shocks.sbatch 40057 1954 &
sbatch SLURM/generate_shocks.sbatch 40068 1955 &
sbatch SLURM/generate_shocks.sbatch 40080 1955 &
sbatch SLURM/generate_shocks.sbatch 40073 1955 &
sbatch SLURM/generate_shocks.sbatch 40069 1955 &
sbatch SLURM/generate_shocks.sbatch 40079 1955 &
sbatch SLURM/generate_shocks.sbatch 40077 1955 &
sbatch SLURM/generate_shocks.sbatch 40072 1955 &
sbatch SLURM/generate_shocks.sbatch 40076 1955 &
sbatch SLURM/generate_shocks.sbatch 40078 1955 &
sbatch SLURM/generate_shocks.sbatch 40086 1956 &
sbatch SLURM/generate_shocks.sbatch 40093 1956 &
sbatch SLURM/generate_shocks.sbatch 40089 1956 &
sbatch SLURM/generate_shocks.sbatch 40084 1956 





wait


finish_time=$(date +%s)
echo ""
echo "The job was done in $((finish_time - start_time)) seconds"
echo ""
date
echo "end of job"