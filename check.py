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
import json
import itertools
import os
import os.path
import urllib
import datetime
import collections
from multiprocessing import Pool
from ansicolor import red, black, blue
from jinja2 import Environment, FileSystemLoader
from UpdateAllFiles import getTranslationFilemapCache
from de import rules

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
                if not f.endswith(".po") and not f.endswith(".pot"): continue
                #Add to list of files to process
                poFilenames.append(os.path.join(curdir, f))
    # Parsing is computationally expensive.
    # Distribute processing amongst distinct processing
    #  if there is a significant number of files
    if len(poFilenames) > 10:
        pool = Pool(None) #As many as CPUs
        parsedFiles = pool.map(polib.pofile, poFilenames)
        return {path: parsedFile
                   for path, parsedFile
                   in zip(poFilenames, parsedFiles)}
    else: #Only a small number of files, process directly
        return {path: polib.pofile(path) for path in poFilenames}

def genCrowdinSearchString(entry):
    s = entry.msgstr[:100].replace('*', ' ')
    s = s.replace('$', ' ').replace('\\', ' ').replace(',', ' ')
    s = s.replace('.', ' ').replace('?', ' ').replace('!', ' ')
    return urllib.parse.quote(s.replace('☃', ' ').replace("|", " "))

class HTMLHitRenderer(object):
    """
    A state container for the code which applies rules and generates HTML.
    """
    def __init__(self, outdir, rules, lang="de"):
        self.outdir = outdir
        self.rules = rules
        #Initialize template engine
        env = Environment(loader=FileSystemLoader('templates'))
        self.ruleTemplate = env.get_template("template.html")
        self.indexTemplate = env.get_template("index.html")
        # Get timestamp
        self.timestamp = datetime.datetime.now().strftime("%y-%m-%d %H:%M:%S")
        if os.path.isfile("lastdownload.txt"):
            with open("lastdownload.txt") as infile:
                self.downloadTimestamp = infile.read().strip()
        else:
            self.downloadTimestamp = None
        # Initialize translation ID/URL map
        translationFilemapCache = getTranslationFilemapCache()
        self.translationURLs = {
            lang + "/" + v["path"]: "https://crowdin.com/translate" + v["editor_url"]
            for v in translationFilemapCache.values()
        }
    def filepath_to_url(self, filename):
        return filename.replace("/", "_")
    def computeRuleHits(self, po, filename="[unknown filename]"):
        """
        Compute all rule hits for a single parsed PO file
        """
        return {
            rule: list(rule.apply_to_po(po, filename=filename))
            for rule in self.rules
        }
    def computeRuleHitsForFileSet(self, poFiles):
        """
        For each file in the given filename -> PO object dictionary,
        compute the Rule -> Hits dictonary.

        Stores the information in the current instance.
        Does not return anything
        """
        # Compute dict with sorted & prettified filenames
        files = {filename: self.filepath_to_url(filename) for filename in poFiles.keys()}
        self.files = collections.OrderedDict(sorted(files.items()))
        # Apply rules
        self.fileRuleHits = {
            filename: self.computeRuleHits(po, filename)
            for filename, po in poFiles.items()
        }
        # Compute total stats by file
        self.statsByFile = {
            filename: sum((len(hits) for hits in ruleHits.values()))
            for filename, ruleHits in self.fileRuleHits.items()
        }
        # Compute by-rule stats per file
        self.statsByFileAndRule = {
            filename: {rule: len(hits) for rule, hits in ruleHits.items()}
            for filename, ruleHits in self.fileRuleHits.items()
        }
        # Compute total stats per rule
        self.totalStatsByRule = {
            rule: sum((stat[rule] for stat in self.statsByFileAndRule.values()))
            for rule in self.rules
        }
    def writeStatsJSON(self):
        """
        Write a statistics-by-filename JSON to outdir/filestats.sjon
        """
        # Write file
        with open(os.path.join(args.outdir, "filestats.json"), "w") as outfile:
            stats = {
                filename: {"hits": sum([len(hits) for _, hits in ruleHits.items()]),
                           "link": self.filepath_to_url(filename)}
                for filename, ruleHits in self.fileRuleHits.items()
            }
            json.dump(stats, outfile)
    def _renderDirectory(self, ruleHits, ruleStats, directory, filename, filelist={}):
        # Generate output HTML for each rule
        for rule, hits in ruleHits.items():
            # Render hits for individual rule
            outfilePath = os.path.join(directory, "%s.html" % rule.get_machine_name())
            with open(outfilePath, "w") as outfile:
                outfile.write(self.ruleTemplate.render(hits=hits, timestamp=self.timestamp, downloadTimestamp=self.downloadTimestamp, translationURLs=self.translationURLs, urllib=urllib, rule=rule, genCrowdinSearchString=genCrowdinSearchString))
        # Render file index page (no filelist)
        with open(os.path.join(directory, "index.html"), "w") as outfile:
            outfile.write(self.indexTemplate.render(rules=self.rules, timestamp=self.timestamp, files=filelist, statsByFile=self.statsByFile,
                          statsByRule=ruleStats, downloadTimestamp=self.downloadTimestamp, filename=filename, translationURLs=self.translationURLs))
    def hitsToHTML(self):
        """
        Apply a rule and write a directory of output HTML files
        """
        for filename, ruleHits in self.fileRuleHits.items():
            filepath = self.filepath_to_url(filename)
            ruleStats = self.statsByFileAndRule[filename]
            # Ensure output directory is present
            directory = os.path.join(self.outdir, filepath)
            if not os.path.isdir(directory):
                os.mkdir(directory)
            # Perform rendering
            self._renderDirectory(ruleHits, ruleStats, directory, filename, {})
        #####################
        ## Render overview ##
        #####################
        # Compute global hits for every rule
        overviewHits = {
            rule: itertools.chain(*(fileHits[rule] for fileHits in self.fileRuleHits.values()))
            for rule in self.rules
        }
        self._renderDirectory(overviewHits, self.totalStatsByRule, self.outdir, filename="all files", filelist=self.files)

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-d','--download', action='store_true', help='Download or update the directory')
    parser.add_argument('-l','--language', default="de", help='The language directory to use/extract')
    parser.add_argument('outdir', nargs='?', default="output", help='The output directory to use')
    args = parser.parse_args()

    # Download / update if requested
    if args.download:
        download()

    # Create directory
    if not os.path.isdir(args.outdir):
        os.mkdir(args.outdir)


    # Import
    print(black("Reading files from %s folder..." % args.language, bold=True))
    poFiles = readPOFiles(args.language)
    print(black("Read %d files" % len(poFiles), bold=True))

    # Compute hits
    print(black("Computing rules...", bold=True))
    renderer = HTMLHitRenderer(args.outdir, rules)
    renderer.computeRuleHitsForFileSet(poFiles)
    # Ensure the HUGE po stuff goes out of scope ASAP
    poFiles = None

    # Generate HTML
    print(black("Rendering HTML...", bold=True))
    renderer.hitsToHTML()

    print (black("Generating JSON API files...", bold=True))
    renderer.writeStatsJSON()
