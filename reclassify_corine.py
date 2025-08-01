# ---------------------------
# 1. Import Required Libraries
# ---------------------------
import os
import numpy as np
import rasterio
import matplotlib.pyplot as plt
from rasterio.enums import Resampling
from rasterio.plot import show
from matplotlib.colors import ListedColormap

# ---------------------------
# 2. Define File Paths
# ---------------------------
corine_path = "/mnt/eo/WilDensity/_data/_corine/CLCplus_2018_010m.tif"
output_dir = "/mnt/eo/WilDensity/output"
os.makedirs(output_dir, exist_ok=True)

# ---------------------------
# 3. Load CORINE Raster
# ---------------------------
with rasterio.open(corine_path) as src:
    corine = src.read(1)
    profile = src.profile.copy()

# ---------------------------
# 4. Define Reclassification Function
# ---------------------------
def reclassify(data, rcl_matrix):
    out = np.full_like(data, fill_value=np.nan, dtype=np.float32)
    for from_val, to_val, new_val in rcl_matrix:
        mask = (data >= from_val) & (data <= to_val)
        out[mask] = new_val
    return out

# ---------------------------
# 5. Define Reclassification Matrices
# ---------------------------
# Format: from, to, becomes
rcl1 = np.array([  # Chamois
    [1, 1, 2],
    [2, 5, 1],
    [6, 7, 0],
    [8, 9, 1],
    [10, 10, 2],
    [11, 11, 0]
])

rcl2 = np.array([  # Roe deer
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

rcl3 = np.array([  # Red deer
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

# ---------------------------
# 6. Apply Reclassification and Save
# ---------------------------
reclass_schemes = {
    "binary_chamois": rcl1,
    "binary_roe_deer": rcl2,
    "binary_red_deer": rcl3
}

profile.update(dtype='float32', count=1, compress='lzw', nodata=np.nan)

for name, matrix in reclass_schemes.items():
    reclassed = reclassify(corine, matrix)
    output_path = os.path.join(output_dir, f"{name}.tif")
    with rasterio.open(output_path, 'w', **profile) as dst:
        dst.write(reclassed, 1)
    print(f"Saved: {output_path}")

# ---------------------------
# 7. Plot Original CORINE Raster
# ---------------------------

# Unique classes in CORINE
classes = sorted(np.unique(corine[~np.isnan(corine)]).astype(int))

# Generate a discrete color map
n_classes = len(classes)
cmap = ListedColormap(plt.cm.get_cmap("tab20", n_classes).colors)
plt.figure(figsize=(10, 8))
im = plt.imshow(corine, cmap=cmap)
plt.title("Original CORINE Land Cover")
plt.colorbar(im, label="CORINE Class Code", ticks=classes)
plt.axis("off")
plt.show()
