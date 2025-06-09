#!/home/luigi.cesarini/.conda/envs/my_xclim_env/bin/python
import os
os.environ['USE_PYGEOS'] = '0'
os.environ["PROJ_LIB"] = "/home/luigi.cesarini/.conda/envs/my_xclim_env/share/proj"

import argparse
import numpy as np
import pandas as pd 
import xarray as xr
import seaborn as sns
from glob import glob
from tqdm import tqdm
import geopandas as gpd
import rioxarray as rxr
from datetime import datetime
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec

import cartopy.crs as ccrs
import cartopy.feature as cfeature

from scipy.interpolate import griddata
from scipy.ndimage import gaussian_filter

import warnings 
   
warnings.filterwarnings('ignore') 

os.chdir("/mnt/beegfs/lcesarini/2025_p_irio/")

"""

The script wants to:
1. Clip the water depth maps, by the extent. (Probably not necessary, the wd are already clipped)
2. Clip each event at the regional level.
3. Intersect each clipped water depth at regional level with the ASIA Database at regional level.
"""

"""
PARSER
"""
parser = argparse.ArgumentParser(description="A simple command-line argument parser example.")

# Add command-line arguments
parser.add_argument('-reg','--region', type=str,help='Region of interest')
parser.add_argument('-id', type=str, default=False,help='Plot the data')
parser.add_argument('-yr','--year', type=int, default=1,help='Coeff to increase the solar radiation')
# parser.add_argument('--radiation', action='store_true', help='Enable verbose mode')

# Parse the command-line arguments
args = parser.parse_args()

df_id_event=pd.read_csv("/mnt/beegfs/lcesarini/2025_p_irio/res/event_id.csv",skiprows=596,header=None)
df_id_event.columns=["event_id","year"]

# name_reg=args.region
# id=args.id
# year=args.year
# print(name_reg,id,year)


"""
END PARSER
"""

shp_reg=gpd.read_file("res/gadm36_ITA.gpkg",layer="gadm36_ITA_1") 

shp_reg=shp_reg.to_crs(3035)
shp_reg.NAME_1[shp_reg.NAME_1=="Sicily"]="Sicilia"
shp_reg.NAME_1[shp_reg.NAME_1=="Apulia"]="Puglia"

asia_ul=gpd.read_file(f"/mnt/beegfs/lcesarini/2024_IRIO_EQ/res/db_asia_geocoded.gpkg")
asia_ul=asia_ul.to_crs(3035)

sigle_prov=pd.read_csv("res/sigle.csv")

# print(np.all([si_pv in sigle_prov for si_pv in asia_ul.denom_prov.unique()]))
# print(asia_ul.denom_prov.unique()[~np.isin(asia_ul.denom_prov.unique(),sigle_prov.sigla)])
path_wd_ext='/mnt/beegfs/lcesarini/HANZE'

for  id, year in tqdm(df_id_event.values):
    
    if not os.path.exists(f"out/vector/{year}"):
        os.makedirs(f"out/vector/{year}")

    wd=rxr.open_rasterio(f'{path_wd_ext}/WD/river/{year}/Event_{id}_IT_{year}_River.tif')
    ext=gpd.read_file(f'{path_wd_ext}/EXTENT/{year}/Event_{id}_IT_{year}_River.shp')

    ext_clip = gpd.overlay(ext, shp_reg, how='intersection',keep_geom_type=False)
    name_regions=ext_clip.NAME_1.unique().tolist()

    for NR in name_regions:
        wd_clip=wd.rio.clip(shp_reg[shp_reg.NAME_1==NR].geometry.values)
        wd_clip=xr.where(wd_clip<0,np.nan,wd_clip).isel(band=0)

        asia_ul_reg=asia_ul[asia_ul.denom_prov.isin(sigle_prov[sigle_prov.Regione==NR].Codice.values)]
        asia_ul_reg['WD']=[wd_clip.sel(x=_x,y=_y, method='nearest').values.item() for _x,_y in zip(asia_ul_reg.geometry.x,asia_ul_reg.geometry.y)]
        
        if np.nansum(~np.isnan(asia_ul_reg.WD))==0:
            continue
        else:
            asia_ul_reg[~np.isnan(asia_ul_reg.WD)].to_file(f"out/vector/{year}/EVENT_{id}_{year}_{NR}_River_ul.gpkg",driver="GPKG")

        # fig,ax=plt.subplots(1,1,figsize=(10,10),
        #                     subplot_kw={'projection': ccrs.LambertAzimuthalEqualArea(
        #                         central_latitude=52,central_longitude=10, false_easting=4321000, false_northing=3210000, globe=None
        #                         )}
        #                     )
        # # ext.plot(ax=ax,color='red')
        # shp_reg[shp_reg.NAME_1==NR].boundary.plot(ax=ax,color='red')
        # # ext_clip[ext_clip.NAME_1==NR].boundary.plot(ax=ax,color='blue')
        # asia_ul.plot(ax=ax,column='WD',cmap="RdBu")
        # ax.coastlines()
        # ax.gridlines()
        # ax.add_feature(cfeature.BORDERS)
        # ax.set_extent(asia_ul[~np.isnan(asia_ul.WD)].total_bounds)#, crs=ccrs.PlateCarree())
        # ax.set_title(f"Flooded UL for {NR}: \n{np.nansum(~np.isnan(asia_ul.WD))}")
        # plt.savefig(f"out/EVENT_{id}_{NR}.png")

        # plt.close()

        # gdalwarp -t_srs EPSG:4326 -r bilinear input_file.tif output_file_3035.tif