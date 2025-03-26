#!/home/luigi.cesarini/.conda/envs/my_xclim_env/bin/python
import os
os.environ['USE_PYGEOS'] = '0'
os.environ["PROJ_LIB"] = "/home/luigi.cesarini/.conda/envs/my_xclim_env/share/proj"

from pathlib import Path

FILE = Path(__file__).resolve()
ROOT = FILE.parents[0]  # YOLOv5 root directory


DATASETS_DIR = Path(os.getenv("YOLOv5_DATASETS_DIR", ROOT.parent / "datasets"))  # global datasets directory
print(DATASETS_DIR)
