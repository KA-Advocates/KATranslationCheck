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

def readPOFiles(startdir="de/2_high_priority_content/"):
    """
    Read all PO files from a given directory and return
    a dictionary path -> PO object
    """
    poFiles = {}
    for (curdir, _, files) in os.walk(startdir):
        for f in files:
            path = os.path.join(curdir, f)
            #Ignore non-PO files
            if not path.endswith(".po"): continue
            poFiles[path] = polib.pofile(path)
    return poFiles

poFiles = readPOFiles()

def findByRule(poFiles, msgstrRegexStr):
    """
    In a dictionary of PO objects, find msgstrs that satisfy a given regex.
    """
    #Precompile expressions
    msgstrRegex = re.compile(msgstrRegexStr, re.UNICODE)
    #Iterate over files
    for filename, po in poFiles.items():
        print(filename)
        for entry in po:
            if msgstrRegex.search(entry.msgstr):
                yield entry
if __name__ == "__main__":
    ctr = 0
    for s in findByRule(poFiles, r"\$\(\d+\s*\,\s*\d+\)\$"):
        ctr += 1
        # s.tcomment contains the URL
        print (s.msgstr)
    print ("Found %d rule violations" % ctr)
