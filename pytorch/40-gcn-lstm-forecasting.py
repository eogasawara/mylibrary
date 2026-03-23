"""
40-gcn-lstm-forecasting.py
--------------------------
Graph-temporal forecasting example that combines GCN and LSTM.
Generates synthetic data; no external datasets required.
"""

import torch
import torch.nn as nn
import torch.optim as optim
from torch_geometric.nn import GCNConv
from torch_geometric.data import Data
import numpy as np

# ==== Synthetic Data Generation ====
def generate_synthetic_graph(num_nodes=10):
    # Fully connected graph
    edge_index = []
    for i in range(num_nodes):
        for j in range(num_nodes):
            if i != j:
                edge_index.append([i, j])
    edge_index = torch.tensor(edge_index, dtype=torch.long).t().contiguous()
    return edge_index

def generate_synthetic_timeseries(num_nodes=10, seq_len=20, num_samples=100):
    data = []
    for _ in range(num_samples):
        sample = np.sin(np.linspace(0, 3.14, seq_len)) + np.random.randn(seq_len) * 0.1
        sample = np.tile(sample[:, None], (1, num_nodes)).T  # shape: (num_nodes, seq_len)
        data.append(sample)
    return np.array(data)  # shape: (num_samples, num_nodes, seq_len)

# ==== Model Definition ====
class GCNLayer(nn.Module):
    def __init__(self, in_feats, out_feats):
        super(GCNLayer, self).__init__()
        self.gcn = GCNConv(in_feats, out_feats)

    def forward(self, x, edge_index):
        return self.gcn(x, edge_index)

class GCN_LSTM_Model(nn.Module):
    def __init__(self, in_feats, gcn_hidden, lstm_hidden, out_feats):
        super().__init__()
        self.gcn = GCNLayer(in_feats, gcn_hidden)
        self.lstm = nn.LSTM(input_size=gcn_hidden, hidden_size=lstm_hidden, batch_first=True)
        self.fc = nn.Linear(lstm_hidden, out_feats)

    def forward(self, x_seq, edge_index):
        batch_size, T, N, F = x_seq.shape
        x_seq_gcn = []

        for t in range(T):
            xt = x_seq[:, t, :, :].reshape(-1, F)  # (batch * nodes, features)
            gcn_out = self.gcn(xt, edge_index)     # (batch * nodes, gcn_hidden)
            gcn_out = gcn_out.view(batch_size, N, -1)
            x_seq_gcn.append(gcn_out)

        x_seq_gcn = torch.stack(x_seq_gcn, dim=1)  # (batch, time, nodes, gcn_hidden)
        x_seq_gcn = x_seq_gcn.mean(dim=2)  # (batch, time, gcn_hidden)
        lstm_out, _ = self.lstm(x_seq_gcn)
        prediction = self.fc(lstm_out[:, -1, :])  # (batch, out_feats)
        return prediction

# ==== Training Example ====
def train():
    num_nodes = 10
    seq_len = 20
    pred_horizon = 1
    samples = 200

    edge_index = generate_synthetic_graph(num_nodes)
    data = generate_synthetic_timeseries(num_nodes, seq_len + pred_horizon, samples)

    X = data[:, :, :seq_len]    # (samples, nodes, time)
    y = data[:, :, -1]          # predict last time step

    X = torch.tensor(X, dtype=torch.float).permute(0, 2, 1).unsqueeze(-1)  # (samples, time, nodes, 1)
    y = torch.tensor(y, dtype=torch.float).mean(dim=1, keepdim=True)      # (samples, 1)

    model = GCN_LSTM_Model(1, 16, 32, 1)
    optimizer = optim.Adam(model.parameters(), lr=0.01)
    loss_fn = nn.MSELoss()


    for epoch in range(100):
        model.train()
        optimizer.zero_grad()
        y_pred = model(X, edge_index)
        loss = loss_fn(y_pred, y)
        loss.backward()
        optimizer.step()
        if epoch % 10 == 0:
            print(f"Epoch {epoch}, Loss: {loss.item():.4f}")

    # Final results
    model.eval()
    with torch.no_grad():
        y_pred = model(X, edge_index)
        final_loss = loss_fn(y_pred, y)
        print(f"\nFinal MSE Loss: {final_loss.item():.4f}")
        print("\nSample Predictions vs Ground Truth:")
        for i in range(5):
            print(f"Pred: {y_pred[i].item():.4f}, Target: {y[i].item():.4f}")
if __name__ == "__main__":
    train()
