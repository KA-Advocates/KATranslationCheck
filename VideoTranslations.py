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
    res = requests.get("https://www.khanacademy.org/api/internal/translate_now?lang={0}".format(lang))
    json = res.json()
    return json["nodes"]["videos"]

def findDubbedVideos(lang):
    videos = fetchVideos(lang)
    return [k for k, v in videos.items() if v["dubbed"]]

def fetchOriginalVideoURL(vid):
    response = requests.get("https://www.khanacademy.org/api/v1/videos/{0}".format(vid)).json()
    # Strip off extra YT attributes
    return response["url"].partition("&")[0].replace("http:", "https:")

timedtextRE = re.compile(r"^https?://www\.youtube\.com/timedtext_video\?v=(.*)$")

def getTranslatedVideoId(vid, lang):
    url = "https://www.khanacademy.org/translate/videos/{0}/subtitle?lang={1}&dub=1".format(vid, lang)
    response = requests.get(url, allow_redirects=False)
    location = response.headers["location"]
    match = timedtextRE.match(location)
    if match is not None:
        return "https://www.youtube.com/watch?v={0}".format(match.group(1))
    else: return None

def fetchVideoMap(pool, lang):
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
    for row in reader:
        try:
            slug = row[8]
            vid = row[6]
            # Ignore non translated videos
            if not vid: continue
            vid = "https://www.youtube.com/watch?v={0}".format()
            print(vid + "," + slug)
            ###yield(vid, slug)
        except IndexError:
            continue

if __name__ == "__main__":
    fetchVideoTranslationsCSV("de")
    sys.exit(1)
    pool = Pool(32)
    languages = findAllLanguages()

    # Fetch all video IDs (i.e. all english videos)
    allVideoIDs = list(fetchVideos("de").keys())

    videoMap = defaultdict(dict)
    for language in languages:
        try:
            langMap = fetchVideoMap(pool, language)
        except:
            print(red("Failed downloading language {0}".format(language), bold=True))
            continue
        # Insert results into main language map
        for vid, url in langMap.items():
            videoMap[vid][language] = url

    # Fetch all english video URLs
    for vid in allVideoIDs:
        try:
            url = fetchOriginalVideoURL(url)
            videoMap[vid]["en"] = url
        except:
            print(red("Failed downloading original video URL {0}".format(vid), bold=True))

    with open("VideoMap.json", "w") as outfile:
        json.dump(videoMap, outfile)
    print(black("Writing JSON videomap", bold=True))
    
