#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Official Khan Academy Lint reader
"""
from collections import namedtuple
import csv
import os
import requests
import time
import re
from lxml.html import fromstring
from selenium import webdriver
from ansicolor import black
from html.parser import HTMLParser
from UpdateAllFiles import downloadCrowdinById, getCrowdinSession

LintEntry = namedtuple("LintEntry", ["date", "url", "crid", "text",
                                     "msgid", "msgstr", "comment", "filename"])

def readLintCSV(filename):
    "Read a KA lint file"
    with open(filename) as lintin:
        reader = csv.reader(lintin, delimiter=',')
        return [LintEntry(row[0], row[1], row[1].rpartition("#")[2],
                row[2], None, None, None, None) for row in reader]

def fetchSeleniumFF(url):
    browser = webdriver.Firefox()
    browser.set_window_size(1920, 1080)
    browser.get(url)
    time.sleep(2)
    page_source = browser.page_source
    browser.quit()
    return page_source

def getLatestLintPostURLForLanguage(lang="de"):
    """
    Get the latest lint result URL from google groups for a given language.
    Returns the URL to a Google Groups post.
    """
    page_source = fetchSeleniumFF("https://groups.google.com/a/khanacademy.org/forum/#!forum/i18n-reports")
    frontpage = fromstring(page_source)
    frontpage.make_links_absolute("https://groups.google.com/a/khanacademy.org/forum")
    elements = frontpage.xpath('//a[@class="IVILX2C-p-Q"]') # WTF is this string?
    #with open("out.html","w") as outf:
    #    outf.write(page_source)
    for element in elements:
        txt = element.text
        if txt.endswith("crowdin entries linted for {0}".format(lang)):
            return (element.get("href"))

def getLatestLintDownloadLink(lang="de"):
    page_source = fetchSeleniumFF(getLatestLintPostURLForLanguage(lang))
    frontpage = fromstring(page_source)
    elements = frontpage.xpath('//a[@class="gwt-Anchor"]') # WTF is this string?
    # Return only last link
    latest = None
    for element in elements:
        txt = element.text
        if txt == "Herunterladen":
            latest = element.get("href")
    return latest

def updateLintFromGoogleGroups(lang="de"):
    url = getLatestLintDownloadLink()
    response = requests.get(url)
    filename = os.path.join("cache", "{0}-lint.csv".format(lang))
    with open(filename, "w") as outfile:
        outfile.write(response.text)
    print(black("Updated %s" % filename, bold=True))

def readAndMapLintEntries(filename):
    """
    Enrich a list of lint entries with msgid and msgstr information
    """
    session = getCrowdinSession(domain="https://crowdin.com")
    cnt = 0
    h = HTMLParser()
    for entry in readLintCSV("cache/de-lint.csv"):
        msgid, msgstr, comment, filename = downloadCrowdinById(session, entry.crid)
        #comment = re.sub(__urlRegex, r"<a href=\"\1\">\1</a>", comment)
        msgid = msgid.replace(" ", "⸱").replace("\t", "→")
        msgstr = msgstr.replace(" ", "⸱").replace("\t", "→")
        comment = h.unescape(comment)
        yield LintEntry(entry.date, entry.url,
                        entry.crid, entry.text, msgid, msgstr, comment, filename)
        cnt += 1
        if cnt % 100 == 0:
            print("Mapped {0} lint entries".format(cnt))


if __name__ == "__main__":
    #url = getLatestLintPostURLForLanguage()
    #print(url)
    print(list(getMappedLintEntries("cache/de-lint.json")))
    #print(readLintCSV("de-lint.csv"))