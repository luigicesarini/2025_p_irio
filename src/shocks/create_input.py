#!/home/luigi.cesarini/.conda/envs/my_xclim_env/bin/python
import os
import argparse
import numpy as np
import pandas as pd 
import seaborn as sns
from glob import glob
from tqdm import tqdm

import warnings 
warnings.filterwarnings('ignore') 

from functions_python import get_shock_perc,fill_df_shock

def get_unlist(ll:list):
    ul=[]
    for sublist in ll:
        for file in sublist:
            ul.append(file)
    return ul

os.chdir("/mnt/beegfs/lcesarini/2025_p_irio")


"""
PARSER
"""
parser = argparse.ArgumentParser(description="A simple command-line argument parser example.")

# Add command-line arguments
parser.add_argument('-id', type=str, default=False,help='Plot the data')
parser.add_argument('-yr','--year', type=int, default=1,help='Coeff to increase the solar radiation')
# parser.add_argument('--radiation', action='store_true', help='Enable verbose mode')

# Parse the command-line arguments
args = parser.parse_args()
#40504 1989
id_event=args.id
year=args.year

if not os.path.exists(f"out/shocks/hit/{year}"): os.makedirs(f"out/shocks/hit/{year}")
"""
MAIN
""" 


if __name__ == "__main__":

    regioni=pd.read_csv("res/sigle.csv").Regione.unique()

    lf=glob(f"test/tot_addetti_by_sector_intermediate_{id_event}_{year}_*.csv")

    regions=[ os.path.basename(p).split('_')[-1].strip('.csv') for p in lf ]
    
    assert np.isin(regions,regioni).all(), "Some regions are missing"

    # print(lf)

    for reg in regions:
        df_event=pd.read_csv(f"test/tot_addetti_by_sector_intermediate_{id_event}_{year}_{reg}.csv")
        gdf_reg_byateco=pd.read_csv(f"out/shocks/tot/tot_addetti_by_ateco_{reg}.csv")

        df=get_shock_perc(df_event,gdf_reg_byateco)
        fill_df_shock(df).to_csv(f"out/shocks/hit/{year}/tot_addetti_by_ateco_perc_{id_event}_{year}_{reg}.csv",index=False)
        # df_whole=pd.DataFrame(columns=['n_sector','addetti_ul']+[f'{i}' for i in range(1,101)])
        # df_whole['n_sector']=np.arange(3,44)

        # df_whole.columns=df_whole.columns.astype(str)
        # df.columns=df.columns.astype(str)

        # df_whole.iloc[np.where(np.isin(df_whole.n_sector,df.n_sector))[0],np.where(np.isin(df_whole.columns,df.columns))[0][1:]]=df.iloc[:,1:].values

        # actual_dt_col=np.insert(np.where(np.isin(df_whole.columns.astype(str),df.columns.astype(str)))[0][2:]-1,0,0)
        # # print(df)
        # range_to_fill=[np.arange(actual_dt_col[i-1]+1,actual_dt_col[i]) for i in np.arange(1,actual_dt_col.shape[0])]

        # for i_not_na,r_na in zip(actual_dt_col[1:],range_to_fill):
        #     df_whole.loc[np.where(np.isin(df_whole.n_sector,df.n_sector))[0],[f"{i}" for i in r_na]]=np.repeat(df[f'{i_not_na}'].values.reshape(-1,1),axis=1,repeats=r_na.shape[0])

        # # print(df_whole)                
        # df.to_csv(f"out/shocks/hit/{year}/tot_addetti_by_ateco_perc_{id_event}_{year}_{reg}.csv",index=False)