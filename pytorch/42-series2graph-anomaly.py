"""
42-series2graph-anomaly.py
--------------------------
Series2Graph-style anomaly detection on a univariate time series.

Key ideas
- Build overlapping subsequences via a sliding window.
- Construct a k-NN similarity graph using cosine similarity over subsequences.
- Learn embeddings with a GCN encoder and a simple contrastive loss between
  consecutive subsequences (positives) vs. more distant ones (negatives).
- Score anomalies by measuring changes between consecutive embeddings.

Notes
- This script generates synthetic data; it does not require external datasets.
- Designed to be consistent with other PyTorch graph experiments in this folder.
"""

import torch
import torch.nn as nn
import torch.optim as optim
from torch_geometric.nn import GCNConv
import numpy as np
from sklearn.metrics.pairwise import cosine_similarity

# ==== Generate Univariate Time Series with Anomalies ====
def generate_univariate_anomaly_series(length=500):
    series = np.sin(np.linspace(0, 10 * np.pi, length)) + np.random.randn(length) * 0.1
    labels = np.zeros(length)
    for _ in range(10):  # Inject 10 anomalies
        idx = np.random.randint(50, length - 50)
        series[idx:idx+5] += np.random.uniform(3, 5)
        labels[idx:idx+5] = 1
    return series, labels

# ==== Create subsequences with sliding window ====
def create_subsequences(series, window_size=30, step=1):
    subsequences = []
    for i in range(0, len(series) - window_size + 1, step):
        subsequences.append(series[i:i + window_size])
    return np.array(subsequences)

# ==== Build similarity graph ====
def build_similarity_graph(subsequences, k=5):
    sims = cosine_similarity(subsequences)
    edge_index = []
    for i in range(len(subsequences)):
        top_k = np.argsort(-sims[i])[1:k+1]
        for j in top_k:
            edge_index.append([i, j])
    edge_index = torch.tensor(edge_index, dtype=torch.long).t().contiguous()
    return edge_index

# ==== GCN Encoder ====
class SubsequenceEncoder(nn.Module):
    def __init__(self, window_size, hidden_size):
        super().__init__()
        self.gcn1 = GCNConv(window_size, hidden_size)
        self.gcn2 = GCNConv(hidden_size, hidden_size)

    def forward(self, x, edge_index):
        x = self.gcn1(x, edge_index).relu()
        x = self.gcn2(x, edge_index)
        return x

# ==== Contrastive loss ====
def contrastive_loss(anchor, positive, negative, margin=1.0):
    pos_dist = (anchor - positive).pow(2).sum(1)
    neg_dist = (anchor - negative).pow(2).sum(1)
    return torch.relu(pos_dist - neg_dist + margin).mean()

# ==== Training ====
def train():
    window_size = 30
    step = 1
    hidden_dim = 32

    series, labels = generate_univariate_anomaly_series()
    subsequences = create_subsequences(series, window_size, step)
    edge_index = build_similarity_graph(subsequences, k=5)

    x = torch.tensor(subsequences, dtype=torch.float)
    edge_index = edge_index

    model = SubsequenceEncoder(window_size, hidden_dim)
    optimizer = optim.Adam(model.parameters(), lr=0.01)

    for epoch in range(100):
        model.train()
        optimizer.zero_grad()
        embeddings = model(x, edge_index)

        # Create positive and negative pairs
        anchors = embeddings[:-2]
        positives = embeddings[1:-1]
        negatives = embeddings[2:]

        loss = contrastive_loss(anchors, positives, negatives)
        loss.backward()
        optimizer.step()

        if epoch % 10 == 0:
            print(f"Epoch {epoch}, Loss: {loss.item():.4f}")

    # Anomaly scoring: magnitude of embedding change between consecutive windows
    model.eval()
    with torch.no_grad():
        embeddings = model(x, edge_index)
        diffs = (embeddings[1:] - embeddings[:-1]).pow(2).sum(1)
        scores = torch.zeros(len(series))
        for i in range(len(diffs)):
            scores[i:i+window_size] += diffs[i].item()

        scores = scores / scores.max()
        print("\nTop anomaly scores (sample):")
        for i in torch.topk(scores, 5).indices:
            print(f"Index {i.item()}, Score: {scores[i]:.4f}, Label: {labels[i]:.0f}")

if __name__ == "__main__":
    train()
