#!/usr/bin/env python3
import requests
import string
import re
import itertools

rgx = re.compile(r"\{\{tippfehler\|([^\|]+)\|[^\|]+\}\}")

def fetchWikipediaMissspellings(char):
    url = "https://de.wikipedia.org/w/api.php?action=query&prop=revisions&rvprop=content&format=jsonfm&titles=Wikipedia:Liste_von_Tippfehlern/{0}&format=json".format(char)
    response = requests.get(url)
    markup = list(response.json()["query"]["pages"].values())[0]["revisions"][0]["*"]
    return [s.replace("+", " ").replace("*", "") for s in rgx.findall(markup)]

def fetchAllWikipediaMisspellings():
    return itertools.chain(*(fetchWikipediaMissspellings(c) for c in string.ascii_uppercase))

if __name__ == "__main__":
    with open("cache/de-typos.txt", "w") as outfile:
        outfile.write("\n".join(list(fetchAllWikipediaMisspellings())))