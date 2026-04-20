"""Example of reproducible seeding in Python."""

import pathlib
import sys

import numpy as np
import torch


CURRENT_DIR = pathlib.Path(__file__).resolve().parent
CODE_DIR = CURRENT_DIR.parent
if str(CODE_DIR) not in sys.path:
    sys.path.insert(0, str(CODE_DIR))

from seed import seed_everything


def main():
    seed_everything(123)

    print("python_random_numpy:", np.random.rand(3).tolist())
    print("python_random_torch:", torch.rand(3).tolist())


if __name__ == "__main__":
    main()
