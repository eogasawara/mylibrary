"""Utilities for reproducible random seeds across Python and PyTorch."""

import os
import random

import numpy as np
import torch


def seed_everything(seed=1):
    """Set the random seed for Python, NumPy and PyTorch.

    Parameters
    ----------
    seed : int, default=1
        Seed value used by Python's ``random`` module, NumPy and PyTorch.

    Returns
    -------
    int
        Normalized integer seed value.

    Notes
    -----
    This function also sets ``PYTHONHASHSEED`` and enables deterministic
    cuDNN behavior when available. On GPU-enabled environments it seeds both
    the current CUDA device and all CUDA devices.
    """
    seed = int(seed)

    random.seed(seed)
    os.environ["PYTHONHASHSEED"] = str(seed)
    np.random.seed(seed)

    torch.manual_seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed(seed)
        torch.cuda.manual_seed_all(seed)

    if hasattr(torch.backends, "cudnn"):
        torch.backends.cudnn.deterministic = True
        torch.backends.cudnn.benchmark = False

    return seed
      
