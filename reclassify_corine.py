# ---------------------------
# 1. Import Required Libraries
# ---------------------------
# %% Cell 1
import os
import numpy as np
import rioxarray as rxr

# ---------------------------
# 2. Define File Paths
# ---------------------------
# %% Cell 2
# Load CORINE raster via rioxarray
corine_path = "/mnt/eo/WilDensity/_data/_corine/CLCplus_2018_010m.tif"
output_dir = "/mnt/eo/WilDensity/output"
os.makedirs(output_dir, exist_ok=True)

# ---------------------------
# 3. Load CORINE Raster
# ---------------------------
# %% Cell 3
corine_xr = rxr.open_rasterio(corine_path, masked=True).squeeze()
corine_arr = corine_xr.values.copy()

# ---------------------------
# 4. Define Reclassification Function
# ---------------------------
# %% Cell 4
# Reclassification function
def reclassify(array, rcl_matrix):
    out = np.full_like(array, fill_value=np.nan, dtype=np.float32)
    for from_val, to_val, new_val in rcl_matrix:
        mask = (array >= from_val) & (array <= to_val)
        out[mask] = new_val
    return out

# ---------------------------
# 5. Define Reclassification Matrices
# ---------------------------
# %% Cell 5
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
# 6. Apply Reclassification and Save
# ---------------------------
# %% Cell 6
for name, rcl in reclassifications.items():
    print(f"Processing {name}...")
    reclass_arr = reclassify(corine_arr, rcl)

    reclass_xr = corine_xr.copy(deep=True)
    reclass_xr.values = reclass_arr

    out_path = os.path.join(output_dir, f"{name}.tif")
    reclass_xr.rio.to_raster(out_path, dtype="float32", compress="LZW", nodata=np.nan)
    print(f"Saved to {out_path}")
    
  
# ---------------------------
# 6. Visualise new rasters
# ---------------------------
# %% Cell 7
# List of raster names
raster_names = ["binary_chamois", "binary_roe_deer", "binary_red_deer"]

for name in raster_names:
    path = os.path.join(output_dir, f"{name}.tif")
    print(f"Loading {name} from: {path}")

    # Load raster
    xr = rxr.open_rasterio(path, masked=True).squeeze()

    # Plot
    plt.figure(figsize=(8, 6))
    xr.plot(cmap="viridis")  # You can change to cmap='tab10', 'gray', etc.
    plt.title(name.replace("_", " ").title())
    plt.axis("off")
    plt.show()
