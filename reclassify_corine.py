# %%
# ---------------------------
# 1. Import Required Libraries
# ---------------------------
import os
import numpy as np
import rioxarray as rxr
import matplotlib.pyplot as plt

# %%
# ---------------------------
# 2. Define File Paths
# ---------------------------
corine_path = "/mnt/eo/WilDensity/_data/_corine/CLCplus_2018_010m.tif"
output_dir = "/mnt/eo/WilDensity/output"
os.makedirs(output_dir, exist_ok=True)

# %%
# ---------------------------
# 3. Load CORINE Raster
# ---------------------------
corine_xr = rxr.open_rasterio(corine_path, masked=True).squeeze()
corine_arr = corine_xr.values.copy()

# %%
# ---------------------------
# 4. Define Reclassification Function
# ---------------------------
def reclassify(array, rcl_matrix):
    out = np.full_like(array, fill_value=np.nan, dtype=np.float32)
    for from_val, to_val, new_val in rcl_matrix:
        mask = (array >= from_val) & (array <= to_val)
        out[mask] = new_val
    return out

# %%
# ---------------------------
# 5. Reclassification Matrices
# ---------------------------
rcl_chamois = np.array([
    [1, 1, 2],
    [2, 5, 1],
    [6, 7, 0],
    [8, 9, 1],
    [10, 10, 2],
    [11, 11, 0]
])

rcl_roe_deer = np.array([
    [1, 1, 2],
    [2, 2, 1],
    [3, 3, 1],
    [4, 4, 1],
    [5, 5, 1],
    [6, 6, 1],
    [7, 7, 1],
    [8, 8, 0],
    [9, 9, 0],
    [10, 10, 2],
    [11, 11, 0]
])

rcl_red_deer = np.array([
    [1, 1, 2],
    [2, 2, 1],
    [3, 3, 1],
    [4, 4, 1],
    [5, 5, 1],
    [6, 6, 0],
    [7, 7, 0],
    [8, 8, 0],
    [9, 9, 0],
    [10, 10, 2],
    [11, 11, 0]
])

reclassifications = {
    "binary_chamois": rcl_chamois,
    "binary_roe_deer": rcl_roe_deer,
    "binary_red_deer": rcl_red_deer
}

# %%
# ---------------------------
# 6. Apply Reclassification and Save Rasters
# ---------------------------
for name, rcl in reclassifications.items():
    print(f"Processing {name}...")
    reclass_arr = reclassify(corine_arr, rcl)

    reclass_xr = corine_xr.copy(deep=True)
    reclass_xr.values = reclass_arr

    out_path = os.path.join(output_dir, f"{name}.tif")
    reclass_xr.rio.to_raster(out_path, dtype="float32", compress="LZW", nodata=np.nan)
    print(f"Saved to {out_path}")

# %%
# ---------------------------
# 7. Lazy Load and Plot a Subset of Gämse Raster
# ---------------------------
# Re-open one raster without Dask chunks
chamois_path = os.path.join(output_dir, "binary_chamois.tif")
chamois_xr = rxr.open_rasterio(chamois_path, masked=True).squeeze()

sample = chamois_xr.isel(x=slice(0, 500), y=slice(0, 500))
print("Unique values in small sample:", np.unique(sample.values[~np.isnan(sample.values)]))
# %%
