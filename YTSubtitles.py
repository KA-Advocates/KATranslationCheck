#!/usr/bin/env python3
"""
Simple requests-based (non-cached) interface
"""
import requests
from ansicolor import black
import json
from collections import defaultdict
from multiprocessing import Pool

def getExercises():
    return requests.get("https://www.khanacademy.org/api/v1/exercises").json()

def getExerciseVideos(name):
    return requests.get("https://www.khanacademy.org/api/v1/exercises/{0}/videos".format(name)).json()

def hasYTTimedText(v, lang="de"):
    url = "https://www.youtube.com/api/timedtext?caps=asr&key=yttt1&v=%s&lang=%s&fmt=srv2" % (v, lang)
    response = requests.get(url)
    return bool(response.text) # Empty if not available


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

if __name__ == "__main__":
    #Download exercise list
    print(black("Downloading master exercise list...", bold=True))
    exercises = getExercises()
    #Download videos
    print(black("Downloading exercise video list for {0} exercises..."
        .format(len(exercises)), bold=True))
    pool = Pool(32)
    exVideos = pool.map(getExerciseVideos, [e["name"] for e in exercises])
    with open("fo.json","w") as outf:json.dump(exVideos, outf)
    # Perform mapping
    print(black("Mapping videos...", bold=True))
    result = []
    allVideoIDs = set()  # Need that for subtitle mapping
    for exercise, videos in zip(exercises, exVideos):
        current_videos = []
        for video in videos:
            current_videos.append({
                "title": video["title"],
                "youtube_id": video["youtube_id"],
                "duration": video["duration"],
            })
            allVideoIDs.add(video["youtube_id"])
        exercise = {
            "videos": current_videos,
            "id": exercise["name"],
            "title": exercise["title"],
            "url": exercise["ka_url"],
        }
        result.append(exercise)
    allVideoIDs = list(allVideoIDs)  # Need guaranteed order
    # Fetch subtitles for videos (reuse pool)
    print(black("Checking subtitles for {0} videos..."
        .format(len(allVideoIDs)), bold=True))
    hasSubtitles = pool.map(hasYTTimedText, allVideoIDs)
    hasSubtitleMap = {
        v: hasSubs for v, hasSubs in zip(allVideoIDs, hasSubtitles)}
    # Injects
    print(black("Remapping subtitle information to video...", bold=True))
    for exercise in result:
        for video in exercise["videos"]:
            video["has_subtitles"] = hasSubtitleMap[video["youtube_id"]]
    # Write to output file
    print(black("Writing to videos.json...", bold=True))
    with open("videos.json", "w") as outfile:
        json.dump(result, outfile)
