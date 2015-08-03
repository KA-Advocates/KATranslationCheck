#!/usr/bin/env python3
"""
Utilities regarding KA exercises
"""
import re
import os
from collections import defaultdict

def findFilenameToExercisesMapping(lang="de"):
    """
    Find a map from filepath to a set of exercises.
    The links are extracted mainly from the tcomment section.
    """
    linkRegex = re.compile(r"http://translate\.khanacademy\.org/translate/content/items\?exercises=([^\"#]+)")
    fileToExercises = defaultdict(set)
    for (curdir, _, files) in os.walk(lang):
        for f in files:
            #Ignore non-PO files
            if not f.endswith(".po") and not f.endswith(".pot"): continue
            #Add to list of files to process
            path = os.path.join(curdir, f)
            with open(path) as infile:
                for line in infile:
                    for hit in linkRegex.findall(line):
                        fileToExercises[path].add(hit)
    return fileToExercises

if __name__ == "__main__":
    fileToExercises = findFilenameToExercisesMapping()
    print(fileToExercises)
