import importlib.util
import os
import pathlib
import random
import unittest

import numpy as np
import torch


ROOT = pathlib.Path(__file__).resolve().parents[1]
MODULE_CANDIDATES = (
    ROOT / "code" / "seed.py",
    ROOT / "seed.py",
)


def resolve_module_path():
    for path in MODULE_CANDIDATES:
        if path.exists():
            return path
    raise FileNotFoundError("seed.py not found in expected locations.")


def load_seed_module():
    spec = importlib.util.spec_from_file_location("seed_module", resolve_module_path())
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class SeedEverythingTests(unittest.TestCase):
    def setUp(self):
        self.seed_module = load_seed_module()

    def test_returns_normalized_integer_seed(self):
        self.assertEqual(self.seed_module.seed_everything("7"), 7)

    def test_sets_pythonhashseed(self):
        self.seed_module.seed_everything(11)
        self.assertEqual(os.environ["PYTHONHASHSEED"], "11")

    def test_reseeding_reproduces_random_sequences(self):
        self.seed_module.seed_everything(123)
        first_random = random.random()
        first_numpy = np.random.rand(4)
        first_torch = torch.rand(4)

        self.seed_module.seed_everything(123)
        second_random = random.random()
        second_numpy = np.random.rand(4)
        second_torch = torch.rand(4)

        self.assertEqual(first_random, second_random)
        np.testing.assert_allclose(first_numpy, second_numpy)
        self.assertTrue(torch.equal(first_torch, second_torch))

    def test_cudnn_flags_are_configured_when_available(self):
        self.seed_module.seed_everything(5)
        if hasattr(torch.backends, "cudnn"):
            self.assertTrue(torch.backends.cudnn.deterministic)
            self.assertFalse(torch.backends.cudnn.benchmark)


if __name__ == "__main__":
    unittest.main()
