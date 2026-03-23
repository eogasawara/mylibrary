import os
import pandas as pd


def read_parquet(filename: str):
    """
    Read a Parquet file into a pandas DataFrame.

    Parameters:
        filename (str): Path or URL to the Parquet file. If a local path is provided,
                        the file must exist on disk. URLs (http/https) are accepted.

    Returns:
        pandas.DataFrame: The loaded data frame.

    Raises:
        FileNotFoundError: If a local file path is provided but does not exist.
        Exception: Propagates pandas read errors for invalid/corrupt files.

    Notes:
        - This small helper keeps Python-specific I/O concerns isolated so R can
          reuse it via reticulate. See the companion R script for orchestration.
    """

    is_url = isinstance(filename, str) and (
        filename.startswith("http://") or filename.startswith("https://")
    )

    if not is_url and not os.path.exists(filename):
        raise FileNotFoundError(f"Parquet file not found: {filename}")

    # pandas handles both local paths and URLs for Parquet.
    return pd.read_parquet(filename)
