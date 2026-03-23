
"""
01-pretrain-foundation.py
-------------------------
Pretrain a tiny time-series foundation model with a masked reconstruction
objective on synthetic sine-wave data. Saves weights to this folder.
"""

from pathlib import Path
import numpy as np
import torch
from torch.utils.data import DataLoader

from tiny_ts_core import (
    set_seed,
    generate_sine_data,
    TimeSeriesDataset,
    TinyTSFoundationModel,
    pretrain_step,
)

set_seed(42)
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

if __name__ == "__main__":
    print("Generating data...")
    data = generate_sine_data()
    dataset = TimeSeriesDataset(data)
    loader = DataLoader(dataset, batch_size=64, shuffle=True)

    model = TinyTSFoundationModel().to(device)
    optimizer = torch.optim.Adam(model.parameters(), lr=1e-3)

    best_loss = float("inf")
    epochs = 20
    print("Starting pretraining...")
    for epoch in range(epochs):
        model.train()
        losses = []
        for batch in loader:
            loss = pretrain_step(model, batch, device)
            optimizer.zero_grad()
            loss.backward()
            optimizer.step()
            losses.append(loss.item())
        avg_loss = np.mean(losses)
        print(f"[Epoch {epoch+1}] Loss: {avg_loss:.4f}")
        if avg_loss < best_loss:
            best_loss = avg_loss
            model_path = Path(__file__).resolve().parent / "tiny_ts_model.pt"
            torch.save(model.state_dict(), model_path)
            print(f"Model checkpoint saved to {model_path}.")
