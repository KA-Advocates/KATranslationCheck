#!/usr/bin/env python3
"""
Dubbed video mapping code
"""
import requests
import re
import sys
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

def fetchVideos(lang):
    "Fetch a list of videos for a given language"
    res = requests.get("https://www.khanacademy.org/api/internal/translate_now?lang={0}".format(lang))
    json = res.json()
    return json["nodes"]["videos"]

def findDubbedVideos(lang):
    "Find only videos which are dubbed"
    videos = fetchVideos(lang)
    return [k for k, v in videos.items() if v["dubbed"]]

def fetchOriginalVideoURL(vid):
    "Depreated: Fix original video ID for a slug"
    response = requests.get("https://www.khanacademy.org/api/v1/videos/{0}".format(vid)).json()
    # Strip off extra YT attributes
    return response["url"].partition("&")[0].replace("http:", "https:")

timedtextRE = re.compile(r"^https?://www\.youtube\.com/timedtext_video\?v=(.*)$")

def getTranslatedVideoId(vid, lang):
    "Deprecated: Find redirect for a specific video ID"
    url = "https://www.khanacademy.org/translate/videos/{0}/subtitle?lang={1}&dub=1".format(vid, lang)
    response = requests.get(url, allow_redirects=False)
    location = response.headers["location"]
    match = timedtextRE.match(location)
    if match is not None:
        return "https://www.youtube.com/watch?v={0}".format(match.group(1))
    else: return None

def fetchVideoMap(pool, lang):
    "Deprecated: Fetch individual video dubs"
    print(black("Fetching dubbed video list for lang {0}".format(lang), bold=True))
    dubbedVideoIDs = findDubbedVideos(lang)

    print(black("Fetching {0} dubbed video URLs for lang {1}"
           .format(len(dubbedVideoIDs), lang), bold=True))
    fn = functools.partial(getTranslatedVideoId, lang=lang)
    videoURLs = pool.map(fn, dubbedVideoIDs)
    
    # Remap
    return {videoId: url
             for videoId, url in zip(dubbedVideoIDs, videoURLs)
             if url is not None}

#Domain,Subject,Topic,Tutorial,Transcript,en,ar,title,slug,duration,en_date_added,ar_date_added,TEST,LIVE
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

    # Fetch all video IDs (i.e. all english videos)
    allVideoIDs = list(fetchVideos("de").keys())

    print(black("Fetching language videomaps", bold=True))
    langresults = pool.map(fetchVideoTranslationsCSV, languages)

    videoMap = defaultdict(dict)
    for language, langresult in zip(languages, langresults):
        # Insert results into main language map
        for slug, url, orig_url in langresult:
            videoMap[slug][language] = url
            videoMap[slug]["en"] = orig_url

    with open("VideoMap.json", "w") as outfile:
        json.dump(videoMap, outfile)
    print(black("Writing JSON videomap", bold=True))
    
