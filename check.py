#!/usr/bin/env python3
# coding: utf-8
"""
EXPERIMENTAL

Regular expression rule checker for Khan Academy translations.

Instructions:
 - Download https://crowdin.com/download/project/khanacademy.zip
 - Unzip the 'de' folder.
 - From the directory where the 'de' folder is located, run this script.
"""
import polib
import re
import os
import os.path
from multiprocessing import Pool
from ansicolor import red, black
from jinja2 import Environment, FileSystemLoader

def readPOFiles(directory):
    """
    Read all PO files from a given directory and return
    a dictionary path -> PO object.

    Also supports using a single file as argument.
    """
    if os.path.isfile(directory): #Single file
        poFilenames = [directory]
    else:
        poFilenames = []
        #Recursively iterate directory, ignore everythin except *.po
        for (curdir, _, files) in os.walk(directory):
            for f in files:
                #Ignore non-PO files
                if not f.endswith(".po"): continue
                #Add to list of files to process
                poFilenames.append(os.path.join(curdir, f))
    # Parsing is computationally expensive.
    # Distribute processing amongst distinct processing
    #  if there is a significant number of files
    if len(poFilenames) > 3:
        pool = Pool(None) #As many as CPUs
        parsedFiles = pool.map(polib.pofile, poFilenames)
        return {path: parsedFile
                   for path, parsedFile
                   in zip(poFilenames, parsedFiles)}
    else: #Only a small number of files, process directly
        return {path: polib.pofile(path) for path in poFilenames}

def findByRule(poFiles, msgstrRegexStr):
    """
    In a dictionary of PO objects, find msgstrs that satisfy a given regex.
    """
    #Precompile expressions
    msgstrRegex = re.compile(msgstrRegexStr, re.UNICODE)
    #Iterate over files
    for filename, po in poFiles.items():
        for entry in po:
            searchResult = msgstrRegex.search(entry.msgstr)
            if searchResult:
                yield (entry, searchResult.group(0), filename)

def download():
    import subprocess
    url = "https://crowdin.com/download/project/khanacademy.zip"
    if os.path.isfile("khanacademy.zip"):
        os.remove("khanacademy.zip")
    subprocess.check_output(["wget", url])
    subprocess.check_output(["unzip", "khanacademy.zip", "de/*"], shell=False)

#Coordinate separated by comma instead of |
commaSeparatedCoordinate = r"\$\(\d+\s*\,\s*\d+\)\$"
assert(re.match(commaSeparatedCoordinate, "$(12,3)$"))
#Simple currency value in dollar (matches both comma sep)
simpleDollarCurrency = r"\$\s*\\\\\$\s*\d+([.,]\d+)?\s*\$"
assert(re.match(simpleDollarCurrency, "$\\\\$12$"))
assert(re.match(simpleDollarCurrency, "$\\\\$12.5$"))
assert(re.match(simpleDollarCurrency, "$\\\\$12,5$"))

def hitsToHTML(poFiles, outfile, rule):
    hits = list(findByRule(poFiles, rule))
    #Initialize template engine
    env = Environment(loader=FileSystemLoader('templates'))
    template = env.get_template("template.html")
    with open(outfile, "w") as outfile:
        outfile.write(template.render(hits=hits))
    return len(hits)

if __name__ == "__main__":
    download()
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('directory', help='The directory to look for translation files')
    parser.add_argument('outfile', help='The HTML output file')
    args = parser.parse_args()

    poFiles = readPOFiles(args.directory)
    print(black("Read %d files" % len(poFiles), bold=True))

    ctr = hitsToHTML(poFiles, args.outfile, commaSeparatedCoordinate)

    print ("Found %d rule violations" % ctr)
