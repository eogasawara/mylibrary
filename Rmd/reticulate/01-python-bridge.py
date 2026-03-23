"""
01-python-bridge.py
-------------------
Small Python helpers to be called from R via reticulate.

Functions
- add(x, y): simple arithmetic, used as a connectivity sanity check.
- read_rdata_mem(df): receives a pandas DataFrame from R, computes z = x + y.
- read_rdata_file(path): reads an .RData file (with object 'data'), computes z,
  and writes it back using pyreadr.
"""

from typing import Any
import pyreadr


def add(x: float, y: float) -> float:
    """Return the sum of x and y."""
    return x + y


def read_rdata_mem(df: Any):
    """Add a new column 'z' to a pandas DataFrame where z = x + y.

    The input comes from an R data.frame via reticulate and is converted
    to a pandas DataFrame. The function mutates the DataFrame and returns it
    back to R.
    """
    x = df["x"]
    y = df["y"]
    df["z"] = x + y
    return df


def read_rdata_file(filename: str) -> None:
    """Read an RData file, compute z = x + y, and write back to the same file.

    Expects the RData file to contain a data.frame named 'data'.
    Writes a new RData file with the updated 'data' object using gzip compression.
    """
    result = pyreadr.read_r(filename)
    # Extract the pandas data frame for the object named 'data'
    df1 = result["data"]
    x = df1["x"]
    y = df1["y"]
    df1["z"] = x + y
    pyreadr.write_rdata(filename, df1, df_name="data", compress="gzip")

