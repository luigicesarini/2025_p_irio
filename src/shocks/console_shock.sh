##!/bin/bash

id=40327
year=1975

# For each Event, with givend ID, and region we perform the following steps:
# 1. Run the Rscripts that generates the intermediate hit addetti by sector
# 2. Run the py scripts that generates the shopcks in percentage for each sector
conda init bash
source /home/luigi.cesarini/.bashrc
conda activate r_symi
# The arguments needed for R script are: 1) name of the event file, and region (that could be extracted from the name file, to decide)
Rscript shocks/create_input.R $year $id

# conda deactivate
conda activate my_xclim_env
# The arguments needed for py script are: 1) name of the intermediate file computed at the previous step, and again, the region (that could be extracted from the name file, to decide)
./shocks/create_input.py -id $id -yr $year

rm ../test/*intermediate*
echo
echo "--------------------"
echo
echo Done
echo
echo "--------------------"