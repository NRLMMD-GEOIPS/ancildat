#!/bin/env python

# # # This source code is protected under the license referenced at
# # # https://github.com/NRLMMD-GEOIPS.

"""Build dependencies."""

from subprocess import run
from os.path import dirname


def run_make(setup_kwargs):
    """Build dependencies."""
    retval = run(["make", "-C", dirname(__file__)])
    if retval.returncode != 0:
        exit(retval.returncode)
    return setup_kwargs


if __name__ == "__main__":
    run_make({})
