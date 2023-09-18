#!/usr/bin/env python
import time
import os
import sys
import logging
from itertools import product


def bytewise_equal(file_a: str, file_b: str, verbose=False) -> bool:
    """Check if two files are equal by comparing them byte by byte.

    The method immediatly returns if there is a difference detected. This
    may make the process of just checking if a difference is present way
    more performant than using tools like `diff`.

    Parameters
    ----------
    file_a : str
        Path of the first file
    file_b : str
        Path of the second file

    Returns
    -------
    bool
        True if both files are equal
    """

    equals = True
    start = time.perf_counter()
    with open(file_a, "rb") as input_a:
        with open(file_b, "rb") as input_b:
            bytes_a, bytes_b = 1, 1
            while bytes_a and bytes_b and equals:
                equals = bytes_a == bytes_b
                bytes_a = input_a.read(32)
                bytes_b = input_b.read(32)
    end = time.perf_counter()
    if verbose:
        logging.info(f"Files are equal: {file_a} - {file_b},"
                     f"took: {end - start}")
    return equals


def main():
    if len(sys.argv) < 2:
        logging.error("Enter a directory as argument")
        return

    target = sys.argv[1]
    files_a = [f for f in os.scandir(target) if f.is_file()]
    files_b = files_a.copy()

    dups = [
        (r[0], r[1])
        for r in product(files_a, files_b)
        if r[0] != r[1] and bytewise_equal(r[0].path, r[1].path, verbose=True)
    ]
    for a, b in dups:
        # Check if both files exist, cause the earlier product contains
        # all combinations like (1, 2) and (2, 1)
        if os.path.exists(a) and os.path.exists(b):
            os.remove(b)


if __name__ == "__main__":
    main()
