#!/bin/bash
#SBATCH --job-name=shocks_claims
#SBATCH -e "resultsSLURM/%x-%j.err"
#SBATCH -o "resultsSLURM/%x-%j.out"
#SBATCH --partition=cpuq
#SBATCH -N 1
#SBATCH -n 35
#SBATCH --cpus-per-task=1
#SBATCH --time=120:00:00
#SBATCH --mem-per-cpu=256GB
#SBATCH --nodelist=cn01
#SBATCH --account=PR_climate

echo "start of job"
# Get the current time in seconds since the epoch and store it in a variable
start_time=$(date +%s)


DAY=1
echo "eliminating results slurm older than $DAY days"
date +'%Y-%m-%d %H:%M:%S'

DATE=$(date -d "$DAY days ago" +'%Y-%m-%d %H:%M:%S')


export MPLCONFIGDIR=/mnt/beegfs/lcesarini/tmp/mat
WORKDIR=$PWD
echo $WORKDIR
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
# echo rm ../test/*intermediate*
echo ----------------------------------------------

./shocks/console_shock.sh -t claims -y 1950 & 
./shocks/console_shock.sh -t claims -y 1951 &
./shocks/console_shock.sh -t claims -y 1952 &
./shocks/console_shock.sh -t claims -y 1953 &
./shocks/console_shock.sh -t claims -y 1954 &
./shocks/console_shock.sh -t claims -y 1955 &
./shocks/console_shock.sh -t claims -y 1956 &
./shocks/console_shock.sh -t claims -y 1957 &
./shocks/console_shock.sh -t claims -y 1958 &
./shocks/console_shock.sh -t claims -y 1959 &
./shocks/console_shock.sh -t claims -y 1960 &
./shocks/console_shock.sh -t claims -y 1961 &
./shocks/console_shock.sh -t claims -y 1962 &
./shocks/console_shock.sh -t claims -y 1963 &
./shocks/console_shock.sh -t claims -y 1964 &
./shocks/console_shock.sh -t claims -y 1965 &
./shocks/console_shock.sh -t claims -y 1966 &
./shocks/console_shock.sh -t claims -y 1967 &
./shocks/console_shock.sh -t claims -y 1968 &
./shocks/console_shock.sh -t claims -y 1969 &
./shocks/console_shock.sh -t claims -y 1970 &
./shocks/console_shock.sh -t claims -y 1971 &
./shocks/console_shock.sh -t claims -y 1972 &
./shocks/console_shock.sh -t claims -y 1973 &
./shocks/console_shock.sh -t claims -y 1974 &
./shocks/console_shock.sh -t claims -y 1975 &
./shocks/console_shock.sh -t claims -y 1976 &
./shocks/console_shock.sh -t claims -y 1977 &
./shocks/console_shock.sh -t claims -y 1978 &
./shocks/console_shock.sh -t claims -y 1979 &
./shocks/console_shock.sh -t claims -y 1980 &
./shocks/console_shock.sh -t claims -y 1981 &
./shocks/console_shock.sh -t claims -y 1982 &
./shocks/console_shock.sh -t claims -y 1983 &
./shocks/console_shock.sh -t claims -y 1984
./shocks/console_shock.sh -t claims -y 1985 &
./shocks/console_shock.sh -t claims -y 1986 &
./shocks/console_shock.sh -t claims -y 1987 &
./shocks/console_shock.sh -t claims -y 1988 &
./shocks/console_shock.sh -t claims -y 1989 &
./shocks/console_shock.sh -t claims -y 1990 &
./shocks/console_shock.sh -t claims -y 1991 &
./shocks/console_shock.sh -t claims -y 1992 &
./shocks/console_shock.sh -t claims -y 1993 &
./shocks/console_shock.sh -t claims -y 1994 &
./shocks/console_shock.sh -t claims -y 1995 &
./shocks/console_shock.sh -t claims -y 1996 &
./shocks/console_shock.sh -t claims -y 1997 &
./shocks/console_shock.sh -t claims -y 1998 &
./shocks/console_shock.sh -t claims -y 1999 &
./shocks/console_shock.sh -t claims -y 2000 &
./shocks/console_shock.sh -t claims -y 2001 &
./shocks/console_shock.sh -t claims -y 2002 &
./shocks/console_shock.sh -t claims -y 2003 &
./shocks/console_shock.sh -t claims -y 2004 &
./shocks/console_shock.sh -t claims -y 2005 &
./shocks/console_shock.sh -t claims -y 2006 &
./shocks/console_shock.sh -t claims -y 2007 &
./shocks/console_shock.sh -t claims -y 2008 &
./shocks/console_shock.sh -t claims -y 2009 &
# ./shocks/console_shock.sh -t claims -y 2010 & GIA FATTA
./shocks/console_shock.sh -t claims -y 2011 &
./shocks/console_shock.sh -t claims -y 2012 &
./shocks/console_shock.sh -t claims -y 2013 &
./shocks/console_shock.sh -t claims -y 2014 &
./shocks/console_shock.sh -t claims -y 2015 &
./shocks/console_shock.sh -t claims -y 2016 &
./shocks/console_shock.sh -t claims -y 2017 &
./shocks/console_shock.sh -t claims -y 2018 &
./shocks/console_shock.sh -t claims -y 2019 &
./shocks/console_shock.sh -t claims -y 2020

wait


finish_time=$(date +%s)
echo ""
echo "The job was done in $((finish_time - start_time)) seconds"
echo ""
date
echo "end of job"

