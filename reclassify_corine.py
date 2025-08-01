
# ---------------------------
# %% Cell 1
# 1. Import Required Libraries
# ---------------------------

import os
import numpy as np
import rioxarray as rxr
import matplotlib.pyplot as plt

# ---------------------------
# %% Cell 2
# 2. Define File Paths
# ---------------------------
# Load CORINE raster via rioxarray
corine_path = "/mnt/eo/WilDensity/_data/_corine/CLCplus_2018_010m.tif"
output_dir = "/mnt/eo/WilDensity/output"
os.makedirs(output_dir, exist_ok=True)

# ---------------------------
# %% Cell 3
# 3. Load CORINE Raster
# ---------------------------
corine_xr = rxr.open_rasterio(corine_path, masked=True).squeeze()
corine_arr = corine_xr.values.copy()

# ---------------------------
# %% Cell 4
# 4. Define Reclassification Function
# ---------------------------
# Reclassification function
def reclassify(array, rcl_matrix):
    out = np.full_like(array, fill_value=np.nan, dtype=np.float32)
    for from_val, to_val, new_val in rcl_matrix:
        mask = (array >= from_val) & (array <= to_val)
        out[mask] = new_val
    return out

# ---------------------------
# %% Cell 5
# 5. Define Reclassification Matrices
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

# ---------------------------
# %% Cell 6
# 6. Apply Reclassification and Save
# ---------------------------
for name, rcl in reclassifications.items():
    print(f"Processing {name}...")
    reclass_arr = reclassify(corine_arr, rcl)

    reclass_xr = corine_xr.copy(deep=True)
    reclass_xr.values = reclass_arr

    out_path = os.path.join(output_dir, f"{name}.tif")
    reclass_xr.rio.to_raster(out_path, dtype="float32", compress="LZW", nodata=np.nan)
    print(f"Saved to {out_path}")
    

# ---------------------------

# %% Cell 7
# 6. Visualise new rasters
# ---------------------------
import rioxarray as rxr
import matplotlib.pyplot as plt

# Lazy Load mit Dask
chamois_xr = rxr.open_rasterio(
    "/mnt/eo/WilDensity/output/binary_chamois.tif",
    masked=True,
    chunks={"x": 1024, "y": 1024}  # Anpassen je nach Server
).squeeze()

# Optional: Lade nur einen kleinen Teil in den Speicher
# subset = chamois_xr.isel(x=slice(0, 2000), y=slice(0, 2000))  # falls vollständiges Plotten zu langsam ist

# Plot
chamois_xr.plot(cmap="viridis", figsize=(10, 8))
plt.title("Chamois Habitat")
plt.show()


# %%
