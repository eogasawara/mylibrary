"""
00-utils.py
------------
Lightweight utility helpers used across experiments:
- Reproducible seeding
- PyTorch model save/load wrappers
- DataFrame saving helper

These functions are intentionally minimal and framework-agnostic.
"""

import os
import random
import numpy as np
import torch


def seed_everything(seed: int = 1) -> None:
    """Seed Python, NumPy and PyTorch for reproducibility."""
    random.seed(seed)
    os.environ["PYTHONHASHSEED"] = str(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed(seed)
    torch.backends.cudnn.deterministic = True


def savemodel(model: torch.nn.Module, filename: str) -> None:
    """Save a full PyTorch model to a file path."""
    torch.save(model, filename)


def loadmodel(filename: str) -> torch.nn.Module:
    """Load a full PyTorch model from a file path and set it to eval mode."""
    model = torch.load(filename)
    model.eval()
    return model


def savedf(data, filename: str) -> None:
    """Save a pandas DataFrame to CSV without the index column."""
    data.to_csv(filename, index=False)
