#!/usr/bin/env python3
"""
Dubbed video mapping code
"""
import requests
import re
import json
from ansicolor import black, red
import functools
from collections import defaultdict
from Languages import findAllLanguages
from multiprocessing import Pool
import csv
try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO

def fetchVideoTranslationsCSV(lang):
    response = requests.get("https://www.khanacademy.org/translations/videos/{0}_all_videos.csv".format(lang))
    sio = StringIO(response.text)
    reader = csv.reader(sio)
    result = []
    for row in reader:
        try:
            slug, orig_vid, vid = row[8], row[5], row[6]
            # Ignore non translated videos
            if not vid: continue
            url_tpl = "https://www.youtube.com/watch?v={0}"
            url = url_tpl.format(vid)
            orig_url = url_tpl.format(orig_vid)
            result.append((slug, url, orig_url))
        except IndexError:
            continue
    return result

if __name__ == "__main__":
    pool = Pool(48)
    languages = list(sorted(list(findAllLanguages())))

    print(black("Fetching language videomaps", bold=True))
    langresults = pool.map(fetchVideoTranslationsCSV, languages)

    videoMap = defaultdict(dict)
    for language, langresult in zip(languages, langresults):
        # Insert results into main language map
        for slug, url, orig_url in langresult:
            videoMap[slug][language] = url
            videoMap[slug]["en"] = orig_url

    print(black("Writing JSON videomap...", bold=True))
    with open("VideoMap.json", "w") as outfile:
        json.dump(videoMap, outfile)
    
