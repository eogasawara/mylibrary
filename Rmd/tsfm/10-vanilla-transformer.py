"""
10-vanilla-transformer.py
------------------------
Minimal vanilla Transformer example for sequence-to-sequence forecasting on
synthetic sine data. Didactic and standalone.
"""

import torch
import torch.nn as nn
from torch.utils.data import Dataset, DataLoader
import numpy as np
import matplotlib.pyplot as plt

# 1. Create a simple sine wave dataset
def generate_sine_data(seq_len=50, pred_len=10, num_samples=1000):
    x = np.linspace(0, 100, num_samples + seq_len + pred_len)
    y = np.sin(x)
    data = []
    for i in range(num_samples):
        input_seq = y[i:i+seq_len]
        target_seq = y[i+seq_len:i+seq_len+pred_len]
        data.append((input_seq, target_seq))
    return data

# 2. PyTorch Dataset
class SineDataset(Dataset):
    def __init__(self, data):
        self.data = data

    def __len__(self):
        return len(self.data)

    def __getitem__(self, idx):
        x, y = self.data[idx]
        return torch.tensor(x, dtype=torch.float32).unsqueeze(-1), torch.tensor(y, dtype=torch.float32).unsqueeze(-1)

# 3. Vanilla Transformer model
class TimeSeriesTransformer(nn.Module):
    def __init__(self, input_dim=1, model_dim=64, num_heads=4, num_layers=2, output_dim=1, seq_len=50, pred_len=10):
        super().__init__()
        self.input_proj = nn.Linear(input_dim, model_dim)
        self.pos_encoding = nn.Parameter(torch.randn(1, seq_len + pred_len, model_dim))
        self.transformer = nn.Transformer(
            d_model=model_dim,
            nhead=num_heads,
            num_encoder_layers=num_layers,
            num_decoder_layers=num_layers,
            batch_first=True
        )
        self.output_proj = nn.Linear(model_dim, output_dim)

    def forward(self, src, tgt):
        B, S, _ = src.shape
        B, T, _ = tgt.shape

        src = self.input_proj(src) + self.pos_encoding[:, :S, :]
        tgt = self.input_proj(tgt) + self.pos_encoding[:, S:S+T, :]

        output = self.transformer(src, tgt)
        return self.output_proj(output)

# 4. Training loop
def train_model(model, train_loader, epochs=10):
    optimizer = torch.optim.Adam(model.parameters(), lr=1e-3)
    loss_fn = nn.MSELoss()

    model.train()
    for epoch in range(epochs):
        total_loss = 0
        for src, tgt in train_loader:
            tgt_input = tgt.clone()
            tgt_input[:, 1:] = tgt[:, :-1]
            tgt_input[:, 0] = 0  # start token

            optimizer.zero_grad()
            out = model(src, tgt_input)
            loss = loss_fn(out, tgt)
            loss.backward()
            optimizer.step()
            total_loss += loss.item()
        print(f"Epoch {epoch+1}, Loss: {total_loss / len(train_loader):.4f}")

# 5. Evaluation
def evaluate(model, sample):
    model.eval()
    src, tgt = sample
    src = src.unsqueeze(0)
    tgt_input = torch.zeros(1, tgt.shape[0], 1)  # Start with zeros
    with torch.no_grad():
        pred = model(src, tgt_input)
    src = src.squeeze().numpy()
    tgt = tgt.squeeze().numpy()
    pred = pred.squeeze().numpy()

    plt.plot(range(len(src)), src, label="Input")
    plt.plot(range(len(src), len(src) + len(tgt)), tgt, label="Target")
    plt.plot(range(len(src), len(src) + len(tgt)), pred, label="Prediction", linestyle="--")
    plt.legend()
    plt.title("Transformer Time Series Forecast")
    plt.show()

# 6. Run everything
if __name__ == "__main__":
    data = generate_sine_data()
    train_data = SineDataset(data[:800])
    test_data = SineDataset(data[800:])

    train_loader = DataLoader(train_data, batch_size=32, shuffle=True)

    model = TimeSeriesTransformer()
    train_model(model, train_loader, epochs=20)

    evaluate(model, test_data[0])
