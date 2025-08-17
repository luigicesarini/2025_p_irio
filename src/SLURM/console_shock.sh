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


sbatch SLURM/generate_shocks.sbatch 40799 2013 &
sbatch SLURM/generate_shocks.sbatch 40802 2013 &
sbatch SLURM/generate_shocks.sbatch 40801 2013 &
sbatch SLURM/generate_shocks.sbatch 40800 2013 &
sbatch SLURM/generate_shocks.sbatch 40812 2013 &
sbatch SLURM/generate_shocks.sbatch 40814 2013 &
sbatch SLURM/generate_shocks.sbatch 40815 2013 &
sbatch SLURM/generate_shocks.sbatch 40803 2013 &
sbatch SLURM/generate_shocks.sbatch 40823 2014 &
sbatch SLURM/generate_shocks.sbatch 40829 2014 &
sbatch SLURM/generate_shocks.sbatch 40816 2014 &
sbatch SLURM/generate_shocks.sbatch 40820 2014 &
sbatch SLURM/generate_shocks.sbatch 40828 2014 &
sbatch SLURM/generate_shocks.sbatch 40822 2014 &
sbatch SLURM/generate_shocks.sbatch 40817 2014 &
sbatch SLURM/generate_shocks.sbatch 40825 2014 &
sbatch SLURM/generate_shocks.sbatch 40818 2014 &
sbatch SLURM/generate_shocks.sbatch 40832 2015 &
sbatch SLURM/generate_shocks.sbatch 40831 2015 &
sbatch SLURM/generate_shocks.sbatch 40836 2015 &
sbatch SLURM/generate_shocks.sbatch 40842 2015 &
sbatch SLURM/generate_shocks.sbatch 40833 2015 &
sbatch SLURM/generate_shocks.sbatch 40838 2015 &
sbatch SLURM/generate_shocks.sbatch 40840 2015 &
sbatch SLURM/generate_shocks.sbatch 40834 2015 &
sbatch SLURM/generate_shocks.sbatch 40835 2015 &
sbatch SLURM/generate_shocks.sbatch 40855 2016 &
sbatch SLURM/generate_shocks.sbatch 40845 2016 &
sbatch SLURM/generate_shocks.sbatch 40850 2016 &
sbatch SLURM/generate_shocks.sbatch 40843 2016 &
sbatch SLURM/generate_shocks.sbatch 40852 2016 &
sbatch SLURM/generate_shocks.sbatch 40844 2016 &
sbatch SLURM/generate_shocks.sbatch 40847 2016 &
sbatch SLURM/generate_shocks.sbatch 40849 2016 &
sbatch SLURM/generate_shocks.sbatch 40848 2016 &
sbatch SLURM/generate_shocks.sbatch 40854 2016 &
sbatch SLURM/generate_shocks.sbatch 40856 2016 &
sbatch SLURM/generate_shocks.sbatch 40857 2017 &
sbatch SLURM/generate_shocks.sbatch 40860 2017 &
sbatch SLURM/generate_shocks.sbatch 40859 2017 &
sbatch SLURM/generate_shocks.sbatch 40861 2017 &
sbatch SLURM/generate_shocks.sbatch 40858 2017 &
sbatch SLURM/generate_shocks.sbatch 40868 2017 &
sbatch SLURM/generate_shocks.sbatch 40865 2017 &
sbatch SLURM/generate_shocks.sbatch 40867 2017 &
sbatch SLURM/generate_shocks.sbatch 40863 2017 &
sbatch SLURM/generate_shocks.sbatch 40862 2017 &
sbatch SLURM/generate_shocks.sbatch 40881 2018 &
sbatch SLURM/generate_shocks.sbatch 40871 2018 &
sbatch SLURM/generate_shocks.sbatch 40870 2018 &
sbatch SLURM/generate_shocks.sbatch 40872 2018 &
sbatch SLURM/generate_shocks.sbatch 40877 2018 &
sbatch SLURM/generate_shocks.sbatch 40869 2018 &
sbatch SLURM/generate_shocks.sbatch 40880 2018 &
sbatch SLURM/generate_shocks.sbatch 40875 2018 &
sbatch SLURM/generate_shocks.sbatch 40883 2019 &
sbatch SLURM/generate_shocks.sbatch 40887 2019 &
sbatch SLURM/generate_shocks.sbatch 40890 2019 &
sbatch SLURM/generate_shocks.sbatch 40895 2019 &
sbatch SLURM/generate_shocks.sbatch 40882 2019 &
sbatch SLURM/generate_shocks.sbatch 40889 2019 &
sbatch SLURM/generate_shocks.sbatch 40886 2019 &
sbatch SLURM/generate_shocks.sbatch 40893 2019 &
sbatch SLURM/generate_shocks.sbatch 40906 2020 &
sbatch SLURM/generate_shocks.sbatch 40905 2020 &
sbatch SLURM/generate_shocks.sbatch 40903 2020 &
sbatch SLURM/generate_shocks.sbatch 40901 2020 &
sbatch SLURM/generate_shocks.sbatch 40904 2020 &
sbatch SLURM/generate_shocks.sbatch 40899 2020 &
sbatch SLURM/generate_shocks.sbatch 40898 2020 &
sbatch SLURM/generate_shocks.sbatch 40900 2020 &
sbatch SLURM/generate_shocks.sbatch 40902 2020 &
sbatch SLURM/generate_shocks.sbatch 40896 2020 &
sbatch SLURM/generate_shocks.sbatch 40897 2020





wait


finish_time=$(date +%s)
echo ""
echo "The job was done in $((finish_time - start_time)) seconds"
echo ""
date
echo "end of job"