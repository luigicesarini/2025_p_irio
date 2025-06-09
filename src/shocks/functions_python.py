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
    df_to_fill=df_to_fill.astype({'weeks': 'int32'})
    df_to_fill=df_to_fill.groupby('n_sector').apply(lambda x: x.sort_values(['weeks'],ascending=True)).reset_index(drop=True)

    df_to_fill['flag']=np.concatenate([array for array in df_to_fill.groupby('n_sector').apply(lambda x: np.where(x['value']>0,1,0).cumsum()).values])
    df_to_fill['value2']=df_to_fill.groupby('n_sector').apply(lambda x: x['value'].shift(1)).values
    df_to_fill['flag2']=df_to_fill.groupby('n_sector').apply(lambda x: x['flag'].shift(1,fill_value=0)).values
    df_to_fill.loc[((df_to_fill.weeks==4) + (df_to_fill.weeks==100)),'value2']=df_to_fill[(df_to_fill.weeks==4) + (df_to_fill.weeks==100)]['value']

    #fill NA in column value2 with values in value
    df_to_fill[df_to_fill['n_sector']==32]
    df_to_fill['addetti_hit']=np.nan
    df_to_fill.reset_index(drop=True,inplace=True)


    for i in range(df_to_fill.shape[0]):

        if (df_to_fill.loc[i,['flag']].item() == 0) & (df_to_fill.loc[i,['flag2']].item() == 0 ):
            df_to_fill.loc[i,['addetti_hit']]=df_to_fill['tot_addetti_hit'][i].item() 

        elif (np.isnan(df_to_fill['value2'][i])):
            if (df_to_fill['flag'][i]==1) & (df_to_fill['flag2'][i]==0):
                df_to_fill.loc[i,['addetti_hit']]=df_to_fill['tot_addetti_hit'][i].item() 
            else:
                df_to_fill.loc[i,['addetti_hit']]=df_to_fill['addetti_hit'][i-1].item() 

        elif ~np.isnan(df_to_fill['value2'][i]):

            if df_to_fill['weeks'][i]!=4:
                df_to_fill.loc[i,['addetti_hit']]=df_to_fill['addetti_hit'][i-1].item() - df_to_fill['value2'][i].item()
            else:
                df_to_fill.loc[i,['addetti_hit']]=df_to_fill['tot_addetti_hit'][i].item()


    df_to_fill['addetti_hit']=np.where(df_to_fill[['addetti_hit']].values.reshape(-1) < 1e-6,0,df_to_fill['addetti_hit'].values.reshape(-1))

    shocks_int=df_to_fill[['n_sector','weeks','addetti_hit']].pivot_table(index='n_sector',columns='weeks',values='addetti_hit',aggfunc='sum').reset_index()


    df_joined=df_reg_byateco.groupby('n_sector').agg({'addetti_ul':'sum'}).reset_index().merge(shocks_int,on='n_sector')

    df_perc=df_joined.iloc[:,2:].apply(lambda x: x/df_joined['addetti_ul'])

    df_perc_joined=pd.concat([df_joined[['n_sector','addetti_ul']],df_perc],axis=1)

    return df_perc_joined

def fill_df_shock(df):
    """
    Function to fill the whole 100 weeks 

    Parameters
    ----------
    df: pd.DataFrame
        Dataframe with the shock data

    Returns
    -------
    df_filled: pd.DataFrame
        Dataframe with the shock data filled
    """
    
    #initialize an empty dataframe for the 100 weeks on the columns and n_sectors on the rows 
    df_whole=pd.DataFrame(columns=['n_sector','addetti_ul']+[f'{i}' for i in range(1,101)])
    df_whole['n_sector']=np.arange(3,44)

    #match column names
    df_whole.columns=df_whole.columns.astype(str)
    df.columns=df.columns.astype(str)

    #fill the value in the empty df
    df_whole.iloc[
        np.where(np.isin(df_whole.n_sector,df.n_sector))[0],
        np.where(np.isin(df_whole.columns,df.columns))[0][1:]
        ]=df.iloc[:,1:].values

    #get the actual columns where we have the data
    actual_dt_col=np.insert(np.where(np.isin(df_whole.columns.astype(str),df.columns.astype(str)))[0][2:]-1,0,0)
    #get the range of columns to fill
    range_to_fill=[np.arange(actual_dt_col[i-1]+1,actual_dt_col[i]) for i in np.arange(1,actual_dt_col.shape[0])]
    #fill the columns with the data up to the closest week
    for i_not_na,r_na in zip(actual_dt_col[1:],range_to_fill):
        df_whole.loc[np.where(np.isin(df_whole.n_sector,df.n_sector))[0],[f"{i}" for i in r_na]]=np.repeat(df[f'{i_not_na}'].values.reshape(-1,1),axis=1,repeats=r_na.shape[0])

    # return by filling the NAs with 0
    return df_whole.fillna(0)

if __name__=="__main__":
    region="Veneto"
    id=40183
    year=1964

    df_event=pd.read_csv(f"test/tot_addetti_by_sector_intermediate_{id}_{year}_{region}.csv")
    gdf_reg_byateco=pd.read_csv(f"out/shocks/tot/tot_addetti_by_ateco_{region}.csv")

    df=get_shock_perc(df_event,gdf_reg_byateco)
    df.to_csv(f"test/tot_addetti_by_ateco_perc_{region}.csv",index=False)