
"""
02-finetune-forecasting.py
--------------------------
Fine-tune the pretrained tiny foundation model for short-horizon forecasting.
Loads weights saved by 01-pretrain-foundation.py and trains a small decoder.
"""

from pathlib import Path
import torch
import torch.nn as nn
from torch.utils.data import Dataset, DataLoader
import numpy as np
from tiny_ts_core import TinyTSFoundationModel, generate_sine_data

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

def split_forecast_data(data, input_len=100, pred_len=28):
    inputs = data[:, :input_len]
    targets = data[:, input_len:input_len+pred_len]
    return inputs, targets

class ForecastDataset(Dataset):
    def __init__(self, inputs, targets):
        self.x = torch.tensor(inputs, dtype=torch.float32)
        self.y = torch.tensor(targets, dtype=torch.float32)

    def __len__(self):
        return len(self.x)

    def __getitem__(self, idx):
        return self.x[idx], self.y[idx]

class ForecastModel(nn.Module):
    def __init__(self, base_model, pred_len):
        super().__init__()
        self.encoder = base_model.encoder
        self.input_proj = base_model.input_proj
        self.pos_encoding = base_model.pos_encoding
        self.decoder = nn.Linear(base_model.encoder.layers[0].linear2.out_features, 1)
        self.pred_len = pred_len

    def forward(self, x):
        B, T, _ = x.shape
        pos_enc = self.pos_encoding[:, :T, :].to(x.device)
        x_embed = self.input_proj(x) + pos_enc
        encoded = self.encoder(x_embed)
        return self.decoder(encoded[:, -self.pred_len:, :])

if __name__ == "__main__":
    data = generate_sine_data(seq_len=128)
    inputs, targets = split_forecast_data(data, 100, 28)
    dataset = ForecastDataset(inputs, targets)
    loader = DataLoader(dataset, batch_size=64, shuffle=True)

    base_model = TinyTSFoundationModel()
    model_path = Path(__file__).resolve().parent / "tiny_ts_model.pt"
    base_model.load_state_dict(torch.load(model_path, map_location=device))
    model = ForecastModel(base_model, pred_len=28).to(device)

    optimizer = torch.optim.Adam(model.parameters(), lr=1e-3)
    loss_fn = nn.MSELoss()

    for epoch in range(15):
        model.train()
        losses = []
        for x, y in loader:
            x, y = x.to(device), y.to(device)
            pred = model(x)
            loss = loss_fn(pred.squeeze(-1), y.squeeze(-1))
            optimizer.zero_grad()
            loss.backward()
            optimizer.step()
            losses.append(loss.item())
        print(f"[Fine-tune Epoch {epoch+1}] Loss: {np.mean(losses):.4f}")
