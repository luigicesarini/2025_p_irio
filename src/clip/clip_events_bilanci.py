
#!/home/luigi.cesarini/.conda/envs/geo/bin/python
import os
import argparse
import warnings
import folium
from folium.plugins import MarkerCluster
import leafmap
import rasterio
from glob import glob
import geopandas as gpd 


import numpy as np
import xarray as xr
import rioxarray as rxr
import matplotlib.pyplot as plt
import matplotlib.cm as cm

colormap = cm.get_cmap('Blues')

os.chdir("/mnt/beegfs/lcesarini/2025_p_irio/") 

# PATH='/mnt/beegfs/lcesarini/2025_p_irio/test/Event_40769_IT_Veneto_2010_River_4326.tif'
"""
START FUNCTIONS
"""

def get_img_bounds(path):
    """
    Get the image and bounds from a raster file.
    
    Parameters:
    path (str): Path to the raster file.

    Returns:
    image (numpy.ndarray): The raster data.
    bounds (rasterio.coords.BoundingBox): The bounds of the raster.
    """
    # Load the raster data
    with rasterio.open(path) as src:
        image = src.read(1)
        bounds = src.bounds

    return image, bounds


def repro_to_wgs84(path,NR,type_flood):
    """
    Reproject a raster file to EPSG:4326 and clip it to a specific region.
    Eventually will take a s_crs and t_srs as arguments to provide larger usage. 
    Also, eventually might use GDAL utilty from CLI directly.

    Parameters:
    path (str): Path to the raster file.
    NR (str): Name of the region to clip.
    type_flood (str): Type of flood event.

    Returns:
    xarray.DataArray: Clipped and reprojected raster data.
    """
    
    # Load the raster data
    wd=rxr.open_rasterio(path)
    # CLip the raster data to the region of interest
    wd_clip=wd.rio.clip(shp_reg[shp_reg.NAME_1==NR].geometry.values)
    # Put NAs for values lower than 0, and select the first band
    name_dim=wd_clip.dims[0]
    selection={name_dim:0}
    wd_clip=xr.where(wd_clip<0,np.nan,wd_clip).isel(**selection)
    wd_clip.rio.write_crs("EPSG:3035").rio.reproject("EPSG:4326").rio.to_raster(f'test/Event_{id}_IT_{NR}_{year}_{type_flood}_4326.tif')

    # return wd_clip
"""
END FUNCTIONS
"""

"""
PARSER
"""
parser = argparse.ArgumentParser(description="A simple command-line argument parser example.")

# Add command-line arguments
parser.add_argument('-yr','--year', type=int, default=2000,help='Year of the event')
parser.add_argument('-id', type=str, default=False,help='HANZE ID of the event')
parser.add_argument('-reg','--region', type=str,help='Region of interest')
parser.add_argument('-tf','--type_flood', type=str, choices=['River','Coastal','Compound'], help='Enable verbose mode')

# Parse the command-line arguments
args = parser.parse_args()

"""
END PARSER
"""

print(args)

year=args.year #2015
id=args.id#40840
NR=args.region#"Emilia-Romagna"
type_flood="River"


# Load the shapefile of the regions, to clip the big event
# shp_reg=gpd.read_file("res/gadm36_ITA.gpkg",layer="gadm36_ITA_1") 
shp_reg=gpd.read_file("res/gadm36_ITA.gpkg",layer="gadm36_ITA_2") 

shp_reg=shp_reg.to_crs(3035)
shp_reg.NAME_1[shp_reg.NAME_1=="Sicily"]="Sicilia"
shp_reg.NAME_1[shp_reg.NAME_1=="Apulia"]="Puglia"
shp_reg.NAME_1.unique()


# Path to the event
filename_wd = f"/mnt/beegfs/lcesarini/HANZE/WD/{type_flood.lower()}/{year}/Event_{id}_IT_{year}_{type_flood}.tif"


# Reproject the original event in EPSG:4326
repro_to_wgs84(filename_wd,NR,type_flood)



# repro_to_wgs84(filename_wd,'Friuli-Venezia Giulia',type_flood)
# image2, bounds2 = get_img_bounds(f'test/Event_{id}_IT_Friuli-Venezia Giulia_{year}_{type_flood}_4326.tif')

clipped=rxr.open_rasterio(f'test/Event_{id}_IT_{NR}_{year}_{type_flood}_4326.tif')

all_ul=gpd.read_file(f"../2024_IRIO_EQ/res/{NR}_geocoded.gpkg")

# EXtract the water depth at the location of any firm
wd=[clipped.sel(x=lon,y=lat, method="nearest").values.item() for lat,lon in zip(all_ul.geometry.y,all_ul.geometry.x)]
#add it as a new column to the dataframe
all_ul['WD']=wd

print(f"Flooded firms in {NR} for event {id} in {year} are {all_ul[all_ul.WD>0].shape[0]} out of {all_ul.shape[0]}")

if not os.path.exists(f"out/bilanci/{NR}_{year}/") :os.makedirs(f"out/bilanci/{NR}_{year}/")
all_ul.to_file(f"out/bilanci/{NR}_{year}/Event_{id}_IT_{NR}_{year}_{type_flood}.gpkg",layer="WD",driver="GPKG")

# all_ul=gpd.read_file(f"out/bilanci/{NR}_{year}/Event_{id}_IT_{NR}_{year}_{type_flood}.gpkg",layer="WD")
"""
VISUALIZATION
Create the interactive map to save
"""
image, bounds = get_img_bounds(f'test/Event_{id}_IT_{NR}_{year}_{type_flood}_4326.tif')

fig = folium.Figure(width=400, height=300)  # in pixels
# Create a Folium map
m = folium.Map(location=[(bounds.top + bounds.bottom)/2, (bounds.left + bounds.right)/2], 
               zoom_start=10,
               tiles="Cartodb dark_matter")

# Add the raster data as an ImageOverlay
folium.raster_layers.ImageOverlay(
    # image=clipped.isel(band=0).values,
    # bounds=[[clipped.rio.bounds()[1], clipped.rio.bounds()[0]], [clipped.rio.bounds()[3], clipped.rio.bounds()[2]]],
    image=image,
    bounds=[[bounds.bottom, bounds.left], [bounds.top, bounds.right]],
    colormap=lambda x: colormap(x) if x > 0 else (0, 0, 0, 0),
    interactive=True,
    cross_origin=True,
    zindex=1
).add_to(m)

folium.GeoJson(
    # gdf[(gdf.nome_bac.isin(imp_bac)) + (gdf.nome_corso.isin(imp_bac))],
    shp_reg[shp_reg.NAME_1==NR],
    style_function= lambda x: {
        'fillColor': '#ffaf00',
        'color': '#ffaf00',
        'weight': 2,
        'fillOpacity': 0.15
    },
    popup=folium.features.GeoJsonPopup(fields=shp_reg.columns.values.tolist()[:-1]),
    name='region'
).add_to(m)

folium.GeoJson(
    all_ul[~all_ul.WD.isna()],
    style_function= lambda x: {
        'fillColor': '#ffaf00',
        'color': '#ffaf00',
        'weight': 2,
        'fillOpacity': 0.15
    },
    popup=folium.features.GeoJsonPopup(fields=all_ul.columns.values.tolist()[:-2]+['WD']),
    name='flooded ul'
).add_to(m)


# # Create marker cluster
# marker_cluster = MarkerCluster().add_to(m)

# # Add markers to the cluster
# for lat, lon in zip(all_ul[all_ul.WD.isna()].geometry.y, all_ul[all_ul.WD.isna()].geometry.x):
#     folium.Circle([lat, lon],radius=4, fill_color="orange", fill_opacity=0.4, color="black", weight=1).add_to(marker_cluster)



# Display the map
m.save(f"out/bilanci/{NR}_{year}/Event_{id}_IT_{NR}_{year}_{type_flood}.html")