"""
tiny_ts_core.py
----------------
Shared utilities, dataset generation, and model definitions for tiny time-series
foundation model examples.
"""

from typing import Tuple
import random
import numpy as np
import torch
import torch.nn as nn
from torch.utils.data import Dataset


def set_seed(seed: int = 42) -> None:
    torch.manual_seed(seed)
    np.random.seed(seed)
    random.seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed_all(seed)


def generate_sine_data(num_samples: int = 1000, seq_len: int = 128) -> np.ndarray:
    """Generate simple noisy sine-wave sequences with shape (N, T, 1)."""
    x = np.linspace(0, 50 * np.pi, num_samples + seq_len)
    y = np.sin(x) + 0.1 * np.random.randn(len(x))
    data = [y[i : i + seq_len] for i in range(num_samples)]
    return np.array(data)[:, :, np.newaxis]


class TimeSeriesDataset(Dataset):
    """Minimal dataset wrapper around pre-generated sequences."""

    def __init__(self, data: np.ndarray):
        self.data = torch.tensor(data, dtype=torch.float32)

    def __len__(self) -> int:
        return len(self.data)

    def __getitem__(self, idx: int) -> torch.Tensor:
        return self.data[idx]


class TinyTSFoundationModel(nn.Module):
    """Tiny Transformer encoder for masked sequence reconstruction."""

    def __init__(self, input_dim: int = 1, model_dim: int = 64, num_heads: int = 4, num_layers: int = 3, max_len: int = 256):
        super().__init__()
        self.input_proj = nn.Linear(input_dim, model_dim)
        self.pos_encoding = nn.Parameter(torch.randn(1, max_len, model_dim))
        encoder_layer = nn.TransformerEncoderLayer(d_model=model_dim, nhead=num_heads, batch_first=True)
        self.encoder = nn.TransformerEncoder(encoder_layer, num_layers=num_layers)
        self.output_proj = nn.Linear(model_dim, input_dim)

    def forward(self, x: torch.Tensor, mask: torch.Tensor | None = None) -> torch.Tensor:
        B, T, _ = x.shape
        pos_enc = self.pos_encoding[:, :T, :]
        x_embed = self.input_proj(x) + pos_enc.to(x.device)
        if mask is not None:
            x_embed = x_embed.masked_fill(mask, 0.0)
        encoded = self.encoder(x_embed)
        return self.output_proj(encoded)


def generate_mask(x: torch.Tensor, ratio: float = 0.25) -> torch.Tensor:
    """Boolean mask of shape like x, masking ratio fraction of positions."""
    B, T, D = x.shape
    mask = torch.rand(B, T) < ratio
    for i in range(B):
        if mask[i].all():
            mask[i, np.random.randint(T)] = False
    return mask.unsqueeze(-1).expand(-1, -1, D).to(x.device)


def pretrain_step(model: nn.Module, batch: torch.Tensor, device: torch.device, mask_ratio: float = 0.25) -> torch.Tensor:
    """One masked-reconstruction step returning the MSE loss on masked tokens."""
    x = batch.to(device)
    mask = generate_mask(x, mask_ratio)
    x_masked = x.clone()
    x_masked[mask] = 0.0
    pred = model(x_masked, mask)
    loss = ((pred - x)[mask]).pow(2).mean()
    return loss

