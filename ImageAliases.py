#!/usr/bin/env python3
import requests
import csv
from collections import defaultdict
from io import StringIO

def readImageAliases(ssid):
    text = StringIO(downloadGDocsCSV(ssid))
    reader = csv.reader(text, delimiter=',')
    next(reader) # Skip header
    aliases = defaultdict(str)
    aliases.update({"![](" + row[0] + ")": "![](" + row[1] + ")" for row in reader})
    return aliases

def downloadGDocsCSV(ssid):
    "Download CSV for a google docs spreadsheet"
    url = "https://docs.google.com/spreadsheets/d/{0}/export?format=csv&id={0}".format(ssid)
    return requests.get(url).text

if __name__ == "__main__":
    print(readImageAliases("177zIAO37SY6xUBUyUE30kn5G_wR7oT-txY8XIN7cecU"))