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
import os
import os.path
import shutil
import datetime
import collections
from multiprocessing import Pool
from ansicolor import red, black, blue
from jinja2 import Environment, FileSystemLoader
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
    if len(poFilenames) > 3:
        pool = Pool(None) #As many as CPUs
        parsedFiles = pool.map(polib.pofile, poFilenames)
        return {path: parsedFile
                   for path, parsedFile
                   in zip(poFilenames, parsedFiles)}
    else: #Only a small number of files, process directly
        return {path: polib.pofile(path) for path in poFilenames}

def download(lang="de"):
    import subprocess
    url = "https://crowdin.com/download/project/khanacademy.zip"
    #Remove file it it exists
    if os.path.isfile("khanacademy.zip"):
        os.remove("khanacademy.zip")
    #Remove language directory
    if os.path.exists(lang):
        shutil.rmtree(lang)
    #Download using wget. More robust than python solutions.
    subprocess.check_output(["wget", url])
    #Extract
    subprocess.check_output(["unzip", "khanacademy.zip", "%s/*" % lang], shell=False)
    #Now that we have the de folder we don't need the zip any more
    if os.path.isfile("khanacademy.zip"):
        os.remove("khanacademy.zip")
    #Set download timestamp
    timestamp = datetime.datetime.now().strftime("%y-%m-%d %H:%M:%S")
    with open("lastdownload.txt", "w") as outfile:
        outfile.write(timestamp)

class HTMLHitRenderer(object):
    """
    A state container for the code which applies rules and generates HTML.
    """
    def __init__(self, outdir):
        self.outdir = outdir
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
    def hitsToHTML(self, poFiles, write_filelist=True, statsByFile={}):
        """
        Apply a rule and write a directory of output HTML files
        """
        # Stats
        violation_ctr = 0
        # Generate output HTML for each rule
        files = {filename: filepath_to_filename(filename) for filename in poFiles.keys()} if write_filelist else {}
        files = collections.OrderedDict(sorted(files.items()))
        for rule in rules:
            #Run rule
            hits = list(rule.apply_to_po_set(poFiles))
            # Run outfile path
            outfilePath = os.path.join(self.outdir, "%s.html" % rule.get_machine_name())
            with open(outfilePath, "w") as outfile:
                outfile.write(self.ruleTemplate.render(hits=hits, timestamp=self.timestamp, downloadTimestamp=self.downloadTimestamp))
            # Stats
            violation_ctr += len(hits)
            rule.custom_info["numhits"] = len(hits)
        # Render index page
        with open(os.path.join(self.outdir, "index.html"), "w") as outfile:
            outfile.write(self.indexTemplate.render(rules=rules, timestamp=self.timestamp, files=files, statsByFile=statsByFile,
                          downloadTimestamp=self.downloadTimestamp))
        return violation_ctr

def filepath_to_filename(filename):
    return filename.replace("/", "_")

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-d','--download', action='store_true', help='Download or update the directory')
    parser.add_argument('-l','--language', default="de", help='The language directory to use/extract')
    parser.add_argument('--no-individual-reports',  action='store_true', help='Only create overview')
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

    # Initialize renderer
    renderer = HTMLHitRenderer(args.outdir)

    statsByFile = {}
    if not args.no_individual_reports:
        print (black("Generating individual reports...", bold=True))
        for poFilename, poFile in poFiles.items():
            filename = filepath_to_filename(poFilename)
            curOutdir = os.path.join(args.outdir, filename)
            if not os.path.isdir(curOutdir):
                os.mkdir(curOutdir)
            ctr = renderer.hitsToHTML({poFilename: poFile}, write_filelist=False)
            statsByFile[poFilename] = ctr
    ctr = renderer.hitsToHTML(poFiles, args.outdir, statsByFile=statsByFile)

    print ("Found overall %d rule violations" % ctr)
    #Write stats by file
    filestats = {
        filename: {"hits": numHits, "link": filepath_to_filename(filename)}
        for filename, numHits in statsByFile.items()
    }
    with open(os.path.join(args.outdir, "filestats.json"), "w") as outfile:
        json.dump(filestats, outfile)
    print (black("Generated JSON API files", bold=True))
