"""
41-gcn-anomaly-detection.py
---------------------------
Node-level anomaly detection using a GCN autoencoder-style setup.
Generates synthetic data; no external datasets required.
"""

import torch
import torch.nn as nn
import torch.optim as optim
from torch_geometric.nn import GCNConv
import numpy as np

# ==== Synthetic Anomaly Data Generation ====
def generate_synthetic_anomaly_data(num_nodes=10, seq_len=50, num_samples=200):
    data = []
    labels = []
    for _ in range(num_samples):
        normal = np.sin(np.linspace(0, 2 * np.pi, seq_len))
        noise = np.random.normal(0, 0.05, size=seq_len)
        series = normal + noise
        if np.random.rand() < 0.2:  # 20% anomalies
            anomaly_point = np.random.randint(seq_len)
            series[anomaly_point] += np.random.uniform(3, 5)  # inject spike
            labels.append(1)
        else:
            labels.append(0)
        sample = np.tile(series[:, None], (1, num_nodes)).T
        data.append(sample)
    return np.array(data), np.array(labels)

# ==== Model ====
class GCNEncoder(nn.Module):
    def __init__(self, in_feats, hidden_feats):
        super().__init__()
        self.gcn1 = GCNConv(in_feats, hidden_feats)
        self.gcn2 = GCNConv(hidden_feats, hidden_feats)

    def forward(self, x, edge_index):
        x = self.gcn1(x, edge_index).relu()
        x = self.gcn2(x, edge_index)
        return x

class GCNAnomalyDetector(nn.Module):
    def __init__(self, in_feats, hidden_feats):
        super().__init__()
        self.encoder = GCNEncoder(in_feats, hidden_feats)
        self.decoder = nn.Linear(hidden_feats, in_feats)

    def forward(self, x, edge_index):
        encoded = self.encoder(x, edge_index)
        reconstructed = self.decoder(encoded)
        return reconstructed

# ==== Training ====
def train():
    num_nodes = 10
    seq_len = 50
    samples = 200

    edge_index = []
    for i in range(num_nodes):
        for j in range(num_nodes):
            if i != j:
                edge_index.append([i, j])
    edge_index = torch.tensor(edge_index, dtype=torch.long).t().contiguous()

    data, labels = generate_synthetic_anomaly_data(num_nodes, seq_len, samples)
    data = torch.tensor(data, dtype=torch.float)
    labels = torch.tensor(labels, dtype=torch.long)

    model = GCNAnomalyDetector(in_feats=seq_len, hidden_feats=32)
    optimizer = optim.Adam(model.parameters(), lr=0.01)
    loss_fn = nn.MSELoss()

    for epoch in range(100):
        model.train()
        optimizer.zero_grad()
        x = data.view(-1, seq_len)  # (samples * nodes, seq_len)
        output = model(x, edge_index)
        loss = loss_fn(output, x)
        loss.backward()
        optimizer.step()
        if epoch % 10 == 0:
            print(f"Epoch {epoch}, Loss: {loss.item():.4f}")

    # Inference
    model.eval()
    with torch.no_grad():
        x = data.view(-1, seq_len)
        output = model(x, edge_index)
        reconstruction_error = ((x - output) ** 2).mean(dim=1)
        node_anomaly_scores = reconstruction_error.view(samples, num_nodes).mean(dim=1)

        print("\nSample Anomaly Scores (Top 5):")
        for i in torch.topk(node_anomaly_scores, 5).indices:
            print(f"Sample {i.item()}, Score: {node_anomaly_scores[i]:.4f}, Label: {labels[i].item()}")

if __name__ == "__main__":
    train()
