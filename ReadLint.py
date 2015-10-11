#!/usr/bin/env python3
"""
Official Khan Academy Lint reader
"""
from collections import namedtuple
import csv
import requests

LintEntry = namedtuple("LintEntry", ["date", "url", "text"])

def readLintCSV(filename):
    "Read a KA lint file"
    with open(filename) as lintin:
        reader = csv.reader(lintin, delimiter=',')
        next(reader)  # Skip header
        yield([LintEntry(row[0], row[1], row[2]) for row in reader])

if __name__ == "__main__":
    print(list(readLintCSV("de-lint.csv")))