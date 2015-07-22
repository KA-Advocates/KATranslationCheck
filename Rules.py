#!/usr/bin/env python3
# coding: utf-8
import re
import sys
import fnmatch

if sys.version_info[0] < 3:
    print("This script requires Python version 3.x")
    sys.exit(1)

class Rule(object):
    """
    A baseclass for rules.
    Remember to implement __call__(self, msgstr, msgid),
    which must return the hit or None if no hit is found.
    """
    def __init__(self, name):
        self.name = name
        # If you need to save some state, you can do it here.
        # This MUST NOT be filled by subclasses.
        self.custom_info = {}
    def get_machine_name(self):
        """Get a machine-readable name from a rule name"""
        return self.name.lower().replace(" ", "-").replace("'", "").replace("\"", "")
    def apply_to_po_set(self, poset, ignore_untranslated=True):
        """
        Apply to a dictionary of parsed PO files.
        Yields tuples entry, hit, filename
        """
        for filename, po in poset.items():
            for entry in po:
                # Ignore strings which are the same orig/msgid
                # This accounts for the fact that we don't know how
                if ignore_untranslated and entry.msgstr == entry.msgid:
                    continue
                # Apply the rule
                hit = self(entry.msgstr, entry.msgid)
                if hit:
                    yield (entry, hit, filename)

class SimpleRegexRule(Rule):
    """
    A simple rule type that matches a regex to the translated string.
    Partial matches (via re.search) are considered hits.
    """
    def __init__(self, name, regex, flags=re.UNICODE):
        super().__init__(name)
        self.re = re.compile(regex, flags)
        self.regex_str = regex
    def __call__(self, msgstr, msgid):
        hit = self.re.search(msgstr)
        if hit:
            return hit.group(0)
        return None

class SimpleSubstringRule(Rule):
    """
    A simple rule type that hits when a given substring is found in the msgstr.
    """
    def __init__(self, name, substr, case_insensitive=False):
        super().__init__(name)
        self.substr = substr
        self.ci = case_insensitive
        if self.ci:
            self.substr = self.substr.lower()
    def __call__(self, msgstr, msgid):
        # Case-insensitive preprocessing
        if self.ci:
            msgstr = msgstr.lower()
        if msgstr.find(self.substr) != -1:
            return self.substr
        return None

class TranslationConstraintRule(Rule):
    """
    Enforces that a certain regex in the original string will
    be translated,

    i.e. the rule hits when regexOrig has >= 1 match in the msgid
    while regexTranslated has 0 machte
    """
    def __init__(self, name, regexOrig, regexTranslated, flags=re.UNICODE):
        super().__init__(name)
        self.reOrig = re.compile(regexOrig, flags)
        self.reTranslated = re.compile(regexTranslated, flags)
        self.regex_orig_str = regexOrig
        self.regex_translated_str = regexTranslated
    def __call__(self, msgstr, msgid):
        if self.reOrig.search(msgid) and not self.reTranslated.search(msgstr):
            return "<failed constraint>"
        return None

def SimpleGlobRule(name, glob):
    """Rule wrapper that translates a glob-ish rule to a regex rule"""
    return SimpleRegexRule(name, fnmatch.translate(glob))

########################
### Initialize rules ###
########################
# Currently hardcoded for DE language
rules = [
    #Coordinate separated by comma instead of |
    SimpleRegexRule("Comma in coordinate", r"\$\(\d+\s*\,\s*\d+\)\$"),
    #The most simple case of using a decimal point instead
    SimpleRegexRule("Simple number with decimal point", r"\$\s*\d+\.\d+\s+\$"),
    #Simple currency value in dollar (matches comma separated and decimal point)
    SimpleRegexRule("Value with embedded dollar symbol", r"\$\s*\\\\\$\s*\d+([.,]\d+)?\s*\$"),
    #Dollar not embedded as a symbol 234$ dollar
    SimpleRegexRule("Value suffixed by dollar", r"\d+\$\s*dollars"),
    #Did not translate "years"
    SimpleRegexRule("Occurrence of untranslated 'year'", r"(?<!%\()[Yy]ear(?!\)s)"), # These are lookbehind/lookhead assertions ;-)
    #Did not translate "time"
    SimpleRegexRule("Occurrence of untranslated 'time'", r"\s+[tT]imes?(?![A-Za-z])"),
    #Did not translate "time"
    SimpleSubstringRule("Occurrence of untranslated 'is'", " is "),
    #word problems no
    TranslationConstraintRule("'word problems' not translated to 'Textaufgaben", r"word\s+problem", "Textaufgabe"),
    #Bing issues
    SimpleRegexRule("Bing (1)", r"!\[\]\s+\("),
    SimpleRegexRule("Bing (1)", r"!\s+\[\]\("),
    SimpleRegexRule("False Bing translation of interactive-graphic", r"[Ii]nteraktive\s+Grafik"),
    SimpleRegexRule("False Bing translation of Radio", r"Radio"),
    SimpleRegexRule("False Bing translation of input-number", r"[Ee]ingabe-Zahl"),
    SimpleRegexRule("False Bing translation of input-number", r"[Ee]ingabe-Nummer"),
    SimpleRegexRule("False Bing translation of numeric-input", r"[Nn]umerische[-\s]+Eingabe"),
    SimpleRegexRule("False Bing translation of numeric-input", r"[Nn]umerische[-\s]+Eingang"),
    SimpleRegexRule("False Bing translation of image", r"☃\s+Bild"),
    SimpleRegexRule("Missing translation of **How", r"\*\*[Hh]ow"),
    SimpleRegexRule("Missing translation of **What", r"\*\*[Ww]hat"),
    SimpleRegexRule("Missing translation of ones", r"\\text\{\s*ones\}\}"),
    SimpleRegexRule("Missing translation of ten(s)", r"\\text\{\s*tens?\}\}"),
    SimpleRegexRule("Missing translation of hundred(s)", r"\\text\{\s*hundreds?\}\}"),

]

def findRule(name):
    "Find a rule by name"
    for rule in rules:
        if rule.name == name:
            return rule
    return None

if __name__ == "__main__":
    #Rule tests. python3 Rules.py to run
    assert(findRule("Comma in coordinate")("$(12,3)$"))
    assert(findRule("Simple number with decimal point")("$(12,3)$"))
    assert(findRule("Value with embedded dollar symbol")("$\\\\$12$"))
    assert(findRule("Value with embedded dollar symbol")("$\\\\$12.5$"))
    assert(findRule("Value with embedded dollar symbol")("$\\\\$12,5$"))
    assert(findRule("Value suffixed by dollar")("$1,234$ dollar"))
