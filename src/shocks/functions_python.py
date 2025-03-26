#!/home/luigi.cesarini/.conda/envs/my_xclim_env/bin/python
import os
import numpy as np
import pandas as pd 
import seaborn as sns
from glob import glob
from tqdm import tqdm

import warnings 
   
warnings.filterwarnings('ignore') 

def get_unlist(ll:list):
    ul=[]
    for sublist in ll:
        for file in sublist:
            ul.append(file)
    return ul

os.chdir("/mnt/beegfs/lcesarini/2025_p_irio")


def get_shock_perc(df,df_reg_byateco):
    """
    Function to get the shock for each event in percentage w.r.t. the workforce of the region

    Parameters
    ----------
    None

    Returns
    -------
    df_perc_joined: pd.DataFrame
    """
    df_to_fill=df.melt(id_vars=['n_sector','tot_addetti_hit'],value_vars=df.columns[1:],var_name="weeks").sort_values(['n_sector','weeks'])
    df_to_fill['flag']=np.concatenate([array for array in df_to_fill.groupby('n_sector').apply(lambda x: np.where(x['value']>0,1,0).cumsum()).values])
    df_to_fill['flag2']=df_to_fill.groupby('n_sector').apply(lambda x: x['flag'].shift(1,fill_value=0)).values
    df_to_fill['value2']=df_to_fill.groupby('n_sector').apply(lambda x: x['value'].shift(1)).values

    df_to_fill['addetti_hit']=np.nan
    df_to_fill.reset_index(drop=True,inplace=True)

    for i in range(df_to_fill.shape[0]):

        if (df_to_fill.loc[i,['flag']].item() == 0) & (df_to_fill.loc[i,['flag2']].item() == 0 ) :
            df_to_fill.loc[i,['addetti_hit']]=df_to_fill['tot_addetti_hit'][i].item() 

        elif (np.isnan(df_to_fill['value2'][i])):
            if (df_to_fill['flag'][i]==1) & (df_to_fill['flag2'][i]==0):
                df_to_fill.loc[i,['addetti_hit']]=df_to_fill['tot_addetti_hit'][i].item() 
            else:
                df_to_fill.loc[i,['addetti_hit']]=df_to_fill['addetti_hit'][i-1].item() 

        elif ~np.isnan(df_to_fill['value2'][i]):
            df_to_fill.loc[i,['addetti_hit']]=df_to_fill['addetti_hit'][i-1].item() - df_to_fill['value2'][i].item()



    df_to_fill['addetti_hit']=np.where(df_to_fill[['addetti_hit']].values.reshape(-1) < 1e-6,0,df_to_fill['addetti_hit'].values.reshape(-1))

    shocks_int=df_to_fill[['n_sector','weeks','addetti_hit']].pivot_table(index='n_sector',columns='weeks',values='addetti_hit',aggfunc='sum').reset_index()

    df_joined=df_reg_byateco.groupby('n_sector').agg({'addetti_ul':'sum'}).reset_index().merge(shocks_int,on='n_sector')
    # print(df_joined)
    df_perc=df_joined.iloc[:,2:].apply(lambda x: x/df_joined['addetti_ul'])
    # print(np.sort(df_perc.columns.values.astype(str)))
    df_perc=df_perc[np.sort(df_perc.columns.values.astype(int)).astype(str)]
    df_perc_joined=pd.concat([df_joined[['n_sector','addetti_ul']],df_perc],axis=1)
    return df_perc_joined



if __name__=="__main__":
    df_event=pd.read_csv("test/tot_addetti_by_sector_intermediate_Basilicata.csv")
    gdf_reg_byateco=pd.read_csv("out/shocks/tot/tot_addetti_by_ateco_Basilicata.csv")

    df=get_shock_perc(df_event,gdf_reg_byateco)
    df.to_csv("test/tot_addetti_by_ateco_perc_Basilicata.csv",index=False)