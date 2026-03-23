
"""
03-visualize-embeddings.py
--------------------------
Load the pretrained model and visualize embeddings using t-SNE.
"""

from pathlib import Path
import torch
import numpy as np
from sklearn.manifold import TSNE
import matplotlib.pyplot as plt
from tiny_ts_core import TinyTSFoundationModel, generate_sine_data

model = TinyTSFoundationModel()
model_path = Path(__file__).resolve().parent / "tiny_ts_model.pt"
model.load_state_dict(torch.load(model_path, map_location="cpu"))
model.eval()

data = generate_sine_data(num_samples=300, seq_len=128)
data_tensor = torch.tensor(data, dtype=torch.float32)

with torch.no_grad():
    pos_enc = model.pos_encoding[:, :data_tensor.size(1), :]
    x_embed = model.input_proj(data_tensor) + pos_enc
    encoded = model.encoder(x_embed)
    embeddings = encoded[:, -1, :].numpy()

tsne = TSNE(n_components=2, perplexity=30)
proj = tsne.fit_transform(embeddings)

plt.figure(figsize=(8, 6))
plt.scatter(proj[:, 0], proj[:, 1], c=np.arange(len(proj)), cmap="viridis")
plt.title("t-SNE of Time Series Embeddings")
plt.colorbar()
plt.show()
