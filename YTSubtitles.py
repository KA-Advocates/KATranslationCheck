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
