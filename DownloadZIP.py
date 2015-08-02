#!/usr/bin/env python3
# coding: utf-8
"""
ZIP file downloader for Crowdin.
This is currently not used because it requires manual triggering of ZIP updates
and is therefore unfeasible.

This downloader uses wget and unzip for speed and simplicity.
"""
import shutil
import subprocess
import os

def download(lang="de"):
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
