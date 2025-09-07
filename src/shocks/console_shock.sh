##!/bin/bash

# default values
type_shock=""

usage() {
    echo "Usage: $0 -t type_of_shock"
    #    echo "Scripts that creates repository folder structure at a given location provided as parameter"
#    echo
#    echo "Syntax: scriptTemplate [-h|m|v]"
#    echo "options:"

    exit 1
}

# Parse options
while getopts "t:y:" opt; do
  case $opt in
    t) type_shock="$OPTARG" ;;
    y) year="$OPTARG" ;;
    *) usage ;;
  esac
done

YEAR=$year
echo "--------------------"
echo
echo Processing
echo $YEAR
echo
echo "--------------------"

if [ "$type_shock" = "hazus" ]
then

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

elif [ "$type_shock" = "claims" ]
then 
    for f in /mnt/beegfs/lcesarini/2025_p_irio/out/vector/$YEAR/*Lombardia*
    do
        conda init bash
        source /home/luigi.cesarini/.bashrc
        conda activate r_symi
        # echo Rscript shocks/claimsMarcello/Output_per_BI.R -p "$f"
        final_basename="${f%.gpkg}.rds"
        basename_f=$(basename "$final_basename")
        # echo $basename_f
        # echo $final_basename
        # echo "/mnt/beegfs/lcesarini/2025_p_irio/out/shocks/claims/$year/BI/$basename_f"
        year=$(echo "$basename_f" | awk -F'_' '{print $3}')
        if [ -f "/mnt/beegfs/lcesarini/2025_p_irio/out/shocks/claims/$year/BI/$basename_f" ]
        then
            continue
        else
            Rscript shocks/claimsMarcello/Output_per_BI.R -p "$f"
            # final_basename="${basename_f//.gpkg/.rds}"
            path_to_file_BI="/mnt/beegfs/lcesarini/2025_p_irio/out/shocks/claims/$year/BI/$basename_f"
            Rscript shocks/create_shocks_claims.r  -p "$path_to_file_BI" 
        fi
    done

fi

echo
echo "--------------------"
echo
echo Done
echo
echo "--------------------"