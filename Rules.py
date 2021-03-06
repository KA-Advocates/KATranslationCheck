#!/usr/bin/env python3
# coding: utf-8
import re
import os
import sys
import fnmatch
from collections import defaultdict
from enum import IntEnum
import importlib
from ansicolor import black, red

class Severity(IntEnum):
    # Notice should be used for rules where a significant number of unfixable false-positives are expected
    notice = 1
    # Info should be used for rules that have less impact and more false positives than standard rules.
    info = 2
    # Standard values should have impact on the readability that does not lead to misunderstandins.
    standard = 3
    # Warning values should have clearly visible impact on the readability.
    warning = 4
    # Dangerous values should harm the readability of the text significantly and have virtually no false-positives
    dangerous = 5

if sys.version_info[0] < 3:
    print("This script requires Python version 3.x")
    sys.exit(1)

__cleanupRegex = re.compile(r'<(a|span|div|table)\s*([a-z-]+=("[^"]+"|\'[^\']+\')\s*)*>(.+?)</(a|span|div|table)>\s*', re.MULTILINE)
__cleanupDetectRegex = re.compile(r"<(a|span|div|table)")

def cleanupTranslatedString(s):
    """
    Minor but fast cleanup of the msgstr in order to avoid hits in
    invisible parts like HTML.
    """
    if not __cleanupDetectRegex.search(s):
        return s
    return __cleanupRegex.sub(r"\1", s)

def importRulesForLanguage(lang, basedir="."):
    """Import ruleset from the language-specific python file"""
    moduleName = "rules.{0}".format(lang)
    print(black("Reading rules from {0}".format(moduleName), bold=True))
    langModule = importlib.import_module(moduleName)
    print(black("Found {0} rules for language {1}".format(len(langModule.rules), lang), bold=True))
    return langModule.rules

_extractImgRegex = re.compile(r"(https?://ka-perseus-graphie\.s3\.amazonaws\.com/[0-9a-f]{40,40}\.(png|svg))")

class Rule(object):
    """
    A baseclass for rules.
    Remember to implement __call__(self, msgstr, msgid),
    which must return the hit or None if no hit is found.
    """
    def __init__(self, name, severity=Severity.standard):
        self.name = name
        # If you need to save some state, you can do it here.
        # This MUST NOT be filled by subclasses.
        self.custom_info = {}
        self.severity = severity
    def get_machine_name(self):
        """Get a machine-readable name from a rule name"""
        name = self.name.lower().replace("'", "").replace("\"", "")
        name = name.replace("(", "").replace(")", "").replace("{", "")
        name = name.replace("}", "").replace("\\", "").replace(",", "")
        name = name.replace("*", "").replace("/", "-").replace("%", "")
        name = name.replace("<", "").replace(">", "").replace("/", "")
        name = name.replace("&gt;", "").replace("&lt;", "").replace(":","")
        name = re.sub(r"\s+", "-", name)
        name = re.sub(r"-+", "-", name)
        name = re.sub(r"^-", "", name)
        return name
    def getBootstrapColor(self):
        """Get a bootstrap color class (text-...) depending on the severity"""
        if self.severity == Severity.notice: return "text-muted"
        elif self.severity == Severity.info: return "text-success"
        elif self.severity == Severity.standard: return "text-primary"
        elif self.severity == Severity.warning: return "text-warning"
        elif self.severity == Severity.dangerous: return "text-danger"
        else: return "text-info"
    def __lt__(self, other):
        if self.severity != other.severity:
            return self.severity < other.severity
        return self.name < other.name
    def apply_to_po(self, po, filename="[unknown file]", ignore_untranslated=True):
        """
        Apply to a dictionary of parsed PO files.
        Yields tuples entry, hit, filename
        """
        for entry in po:
            # Ignore empty translations (-> untranslated)
            if not entry.msgstr:
                continue
            # Ignore strings which are the same orig/msgid
            # This accounts for the fact that we don't know how
            if ignore_untranslated and entry.msgstr == entry.msgid:
                continue
            # Translated string cleanup
            msgstr = cleanupTranslatedString(entry.msgstr)
            # Apply the rule
            for hit in self(msgstr, entry.msgid, entry.tcomment, filename=filename):
                #Find images in both original and new string
                origImages = [h[0] for h in _extractImgRegex.findall(entry.msgid)]
                translatedImages = [h[0] for h in _extractImgRegex.findall(entry.msgstr)]
                yield (entry, hit, filename, origImages, translatedImages)

class SimpleRegexRule(Rule):
    """
    A simple rule type that matches a regex to the translated string.
    Partial matches (via re.search) are considered hits.
    """
    def __init__(self, name, regex, severity=Severity.standard, flags=re.UNICODE):
        super().__init__(name, severity)
        self.re = re.compile(regex, flags)
        self.regex_str = regex
    def description(self):
        return "Matches regular expression '%s'" % self.regex_str
    def __call__(self, msgstr, msgid, tcomment="", filename=None):
        hit = self.re.search(msgstr)
        if hit:
            yield hit.group(0)

class SimpleSubstringRule(Rule):
    """
    A simple rule type that hits when a given substring is found in the msgstr.
    """
    def __init__(self, name, substr, severity=Severity.standard, case_insensitive=False):
        super().__init__(name, severity)
        self.substr = substr
        self.ci = case_insensitive
        if self.ci:
            self.substr = self.substr.lower()
    def description(self):
        return "Matches substring '%s'" % self.substr
    def __call__(self, msgstr, msgid, tcomment="", filename=None):
        # Case-insensitive preprocessing
        if self.ci:
            msgstr = msgstr.lower()
        if msgstr.find(self.substr) != -1:
            yield self.substr

class TranslationConstraintRule(Rule):
    """
    Enforces that a certain regex in the original string will
    be translated a certain way

    i.e. the rule hits when regexOrig has >= 1 match in the msgid
    while regexTranslated has 0 machte
    """
    def __init__(self, name, regexOrig, regexTranslated, severity=Severity.standard, flags=re.UNICODE):
        super().__init__(name, severity)
        self.reOrig = re.compile(regexOrig, flags)
        self.reTranslated = re.compile(regexTranslated, flags)
        self.regex_orig_str = regexOrig
        self.regex_translated_str = regexTranslated
    def description(self):
        return "Matches '%s' if translated as '%s'" % (self.regex_orig_str, self.regex_translated_str)
    def __call__(self, msgstr, msgid, tcomment="", filename=None):
        if self.reOrig.search(msgid) and not self.reTranslated.search(msgstr):
            yield "[failed constraint]"

class NegativeTranslationConstraintRule(Rule):
    """
    Enforces that a certain regex in the original string will
    NOT be translated a certain way,

    i.e. the rule hits when regexOrig has >= 1 match in the msgid
    while regexTranslated has a match.
    """
    def __init__(self, name, regexOrig, regexTranslated, severity=Severity.standard, flags=re.UNICODE):
        super().__init__(name, severity)
        self.reOrig = re.compile(regexOrig, flags)
        self.reTranslated = re.compile(regexTranslated, flags)
        self.regex_orig_str = regexOrig
        self.regex_translated_str = regexTranslated
    def description(self):
        return "Matches '%s' if NOT translated as '%s'" % (self.regex_orig_str, self.regex_translated_str)
    def __call__(self, msgstr, msgid, tcomment="", filename=None):
        if self.reOrig.search(msgid) and self.reTranslated.search(msgstr):
            yield "[failed constraint]"

class DynamicTranslationIdentityRule(Rule):
    """
    Enforces that a match to the given regex does is contained in the translated string as-is.
    This rule can also be used as a negative rule to enforce the match is not present
    in the translated string.
    """
    def __init__(self, name, regex, negative=False, group=None, severity=Severity.standard, flags=re.UNICODE):
        super().__init__(name, severity)
        self.regex_str = regex
        self.regex = re.compile(regex, flags)
        self.negative = negative
        self.group = group
    def description(self):
        return "Matches a match for '%s' if %spresent in the translated string" % (self.regex_str, "NOT " if self.negative else "")
    def __call__(self, msgstr, msgid, tcomment="", filename=None):
        matches = self.regex.findall(msgid)
        if not matches: return
        # Apply group filter if enabled
        if self.group is not None:
            matches = [m[self.group] for m in matches]
        # Check individual matches
        if self.negative:
            for match in matches:
                if match in msgstr:
                    yield match
        else:  # Positive rule
            for match in matches:
                if match not in msgstr:
                    yield match

def SimpleGlobRule(name, glob):
    """Rule wrapper that translates a glob-ish rule to a regex rule"""
    return SimpleRegexRule(name, fnmatch.translate(glob))

_whitespaceRegex = re.compile(r"\s+")

class ExactCopyRule(Rule):
    """
    Requires that when a list of regex matches is present in the orignal text,
    the exact same list of matches is also present in the same order.

    This can be used, for example, to ensure GUI elements, numbers or URLs are the same in
    both the translated text and the original.
    """
    def __init__(self, name, regex, severity=Severity.standard, aliases=defaultdict(str), ignore_whitespace=True, group=None):
        super().__init__(name, severity)
        self.regex = re.compile(regex)
        self.regex_str = regex
        self.aliases = aliases
        self.group = group
        self.ignore_whitespace = ignore_whitespace
    def description(self):
        return "Matches if all instances of '%s' are the same (with %d aliases)" % (self.regex_str, len(self.aliases))
    def __call__(self, msgstr, msgid, tcomment="", filename=None):
        origMatches = self.regex.findall(msgid)
        translatedMatches = self.regex.findall(msgstr)
        # Apply aliases
        origMatches = [self.aliases[x] or x for x in origMatches]
        translatedMatches = [self.aliases[x] or x for x in translatedMatches]
        # Apply group if
        if self.group is not None:  # None - Use entire string. No groups must be present in regex
            origMatches = [m[self.group] for m in origMatches]
            translatedMatches = [m[self.group] for m in translatedMatches]
        # Apply whitespace filtering
        if self.ignore_whitespace:
            origMatches = [_whitespaceRegex.sub("", x) or x for x in origMatches]
            translatedMatches = [_whitespaceRegex.sub("", x) or x for x in translatedMatches]
        # Find index of first mismatch
        try:
            idx = next(idx for idx, (x, y) in
                       enumerate(zip(origMatches, translatedMatches)) if x != y)
            yield "[First expression mismatch at occurrence %d]" % (idx + 1)
        except StopIteration:  # No mismatch
            pass

class IgnoreByFilenameRegexWrapper(Rule):
    """
    Ignore a rule (i.e. force zero hits) for a set of filenames defined by a regex.

    If you want to ignore a rule for all filenames starting with "learn.", you'd use:

    """
    def __init__(self, filename_regex, child, invert=False):
        """
        Keyword arguments:
            invert: Set this to true to invert this regex, i.e. mismatches of the regex lead to a ignored entry
        """
        super().__init__(child.name)
        self.child = child
        self.invert = invert
        self.filename_regex = re.compile(filename_regex)
        self.filename_regex_str = filename_regex
        self.severity = child.severity
    def description(self):
        if self.invert:
            return "%s (only applied to filenames matching '%s')" % (self.child.description(), self.filename_regex_str)
        else:
            return "%s (ignored for filenames matching '%s')" % (self.child.description(), self.filename_regex_str)
    def __call__(self, msgstr, msgid, tcomment="", filename=None):
        if bool(self.filename_regex.match(filename)) != self.invert:
            return None
        yield from self.child(msgstr, msgid, tcomment, filename)

class IgnoreByFilenameListWrapper(Rule):
    """
    Ignore a rule (i.e. force zero hits) for a set of filenames defined by a list of exact hits.
    """
    def __init__(self, filenames, child):
        super().__init__(child.name)
        self.child = child
        self.filenames = frozenset(filenames)
        self.severity = child.severity
    def description(self):
        return "%s (ignored for files %s)" % (self.child.description(), str(list(self.filenames)))
    def __call__(self, msgstr, msgid, tcomment="", filename=None):
        if filename in self.filenames:
            return None
        yield from self.child(msgstr, msgid, tcomment, filename)

class IgnoreByMsgidRegexWrapper(Rule):
    """
    Ignore a rule if a regex search in the msgid returns a certain value.

    This can be useful to ignore special cases of translation which
    are distinguishable by the untranslated (english) text, e.g.
    "Green's theorem" as a special case of untranslated "green".

    Note that if a single regex hit is found, the entire string is ignore
    """
    def __init__(self, msgid_regex, child):
        super().__init__(child.name)
        self.child = child
        self.msgid_regex = re.compile(msgid_regex)
        self.msgid_regex_str = msgid_regex
        self.severity = child.severity
    def description(self):
        return "%s (ignored for msgids matching '%s')" % (self.child.description(), self.msgid_regex_str)
    def __call__(self, msgstr, msgid, tcomment="", filename=None):
        if self.msgid_regex.search(msgid):
            return None
        yield from self.child(msgstr, msgid, tcomment, filename)


class IgnoreByMsgstrRegexWrapper(Rule):
    """
    Ignore a rule if a regex search in the msgstr returns a certain value.

    This can be useful to ignore special cases of translation which
    are distinguishable by the untranslated (english) text, e.g.
    "Green's theorem" as a special case of untranslated "green".

    Note that if a single regex hit is found, the entire string is ignore
    """
    def __init__(self, msgstr_regex, child):
        super().__init__(child.name)
        self.child = child
        self.msgstr_regex = re.compile(msgstr_regex)
        self.msgid_regex_str = msgstr_regex
        self.severity = child.severity
    def description(self):
        return "%s (ignored for msgids matching '%s')" % (self.child.description(), self.msgid_regex_str)
    def __call__(self, msgstr, msgid, tcomment="", filename=None):
        if self.msgstr_regex.search(msgstr):
            return None
        yield from self.child(msgstr, msgid, tcomment, filename)

class IgnoreByTcommentRegexWrapper(Rule):
    """
    Ignore a rule if a regex search in the tcomment returns a certain value.

    This can be useful to ignore special cases of translation which
    are distinguishable by the untranslated (english) text, e.g.
    "Green's theorem" as a special case of untranslated "green".

    Note that if a single regex hit is found, the entire string is ignore
    """
    def __init__(self, tcommentRegex, child):
        super().__init__(child.name)
        self.child = child
        self.tcommentRegex = re.compile(tcommentRegex)
        self.tcomment_regex_str = tcommentRegex
        self.severity = child.severity
    def description(self):
        return "%s (ignored for tcomments matching '%s')" % (self.child.description(), self.tcomment_regex_str)
    def __call__(self, msgstr, msgid, tcomment="", filename=None):
        if self.tcommentRegex.search(tcomment):
            return None
        yield from self.child(msgstr, msgid, tcomment, filename)

class TextListRule(Rule):
    """
    A rule that excepts a text list of words (e.g. typos), each of which will generate a
    rule hit. The file is expected to contain one string per line.

    If the file does not exist, this method prints a red bold error message and does not
    generate any rule hits
    """
    def __init__(self, name, filename, severity=Severity.standard, flags=re.UNICODE):
        super().__init__(name, severity)
        self.filename = filename
        self.regexes = []
        # Check if file exists
        if os.path.isfile(filename):
            with open(filename) as infile:
                for line in infile:
                    rgx = line.strip().replace(" ", r"\s+")
                    #Don't match in the middle of a word
                    rgx = "\\b{0}\\b".format(rgx)
                    self.regexes.append(re.compile(rgx, flags=flags))
        else:  # File does not exist
            print(red("Unable to find text list file %s" % filename, bold=True))
    def description(self):
        return "Matches one of the strings in file %s" % self.filename
    def __call__(self, msgstr, msgid, tcomment="", filename=None):
        for regex in self.regexes:
            hit = regex.search(msgstr)
            if hit:
                yield hit.group(0)

def findRule(rules, name):
    "Find a rule by name"
    try:
        next(rule for rule in rules if rule.name == name)
    except StopIteration:
        return None
