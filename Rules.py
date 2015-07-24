#!/usr/bin/env python3
# coding: utf-8
import re
import sys
import fnmatch

if sys.version_info[0] < 3:
    print("This script requires Python version 3.x")
    sys.exit(1)

aHrefLinkTextRegex = re.compile(r'<a\s+href="[^"]+">(.+?)</a>\s*')

def cleanupTranslatedString(s):
    """Minor but fast cleanup of the msgstr in order to avoid hits in invisible parts"""
    if not "</a>" in s:
        return s
    return aHrefLinkTextRegex.sub(r"\1", s)

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
        return self.name.lower().replace(" ", "-").replace("'", "").replace("\"", "").replace("(","").replace(")","")
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
                # Translated string cleanup
                msgstr = cleanupTranslatedString(entry.msgstr)
                # Apply the rule
                hit = self(msgstr, entry.msgid)
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
    be translated a certain way

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
            return "[failed constraint]"
        return None

class NegativeTranslationConstraintRule(Rule):
    """
    Enforces that a certain regex in the original string will
    NOT be translated a certain way,

    i.e. the rule hits when regexOrig has >= 1 match in the msgid
    while regexTranslated has a match.
    """
    def __init__(self, name, regexOrig, regexTranslated, flags=re.UNICODE):
        super().__init__(name)
        self.reOrig = re.compile(regexOrig, flags)
        self.reTranslated = re.compile(regexTranslated, flags)
        self.regex_orig_str = regexOrig
        self.regex_translated_str = regexTranslated
    def __call__(self, msgstr, msgid):
        if self.reOrig.search(msgid) and self.reTranslated.search(msgstr):
            return "[failed constraint]"
        return None

class BooleanNotRule(Rule):
    """Apply a boolean NOT to a child rule"""
    def __init__(self, child):
        super().__init__(child.name)
        self.child = child
    def __call__(self, msgstr, msgid):
        if self.child(msgstr, msgid):
            return None
        else:
            return "[failed boolean NOT]"

class BooleanAndRule(Rule):
    """Apply a boolean AND to a child rule. Returns the hit of the first child."""
    def __init__(self, name, childA, childB):
        super().__init__(name)
        self.childA = childA
        self.childB = childB
    def __call__(self, msgstr, msgid):
        hitA = self.childA(msgstr, msgid)
        if not hitA: return None # Shortcut-return
        hitB = self.childB(msgstr, msgid)
        if hitB: return hitA
        return None

class BooleanOrRule(Rule):
    """Apply a boolean AND to a child rule. Returns the hit of the first child."""
    def __init__(self, name, childA, childB):
        super().__init__(name)
        self.childA = childA
        self.childB = childB
    def __call__(self, msgstr, msgid):
        hitA = self.childA(msgstr, msgid)
        if hitA: return hitA # Shortcut-return
        return self.childB(msgstr, msgid)


def SimpleGlobRule(name, glob):
    """Rule wrapper that translates a glob-ish rule to a regex rule"""
    return SimpleRegexRule(name, fnmatch.translate(glob))

########################
### Initialize rules ###
########################
# Currently hardcoded for DE language
rules = [
    #Coordinate separated by comma instead of |
    SimpleRegexRule("Comma in integral coordinate", r"\$\(\d+\s*\,\s*\d+\)\$"),
    SimpleRegexRule("Comma in non-integral coordinate", r"\$\(\d+[\.,]\d+\s*\,\s*\d+[\.,]\d+\)\$"),
    #The most simple case of using a decimal point instead
    SimpleRegexRule("Simple number with decimal point instead of comma", r"\$[-]?\s*\d+\.\d+\s+\$"),
    #Simple currency value in dollar (matches comma separated and decimal point)
    SimpleRegexRule("Value with embedded dollar symbol", r"\$\s*\\\\\$\s*\d+([.,]\d+)?\s*\$"),
    #Errors in thousands separation
    SimpleRegexRule("Value with multiple or mixed commata or dots", r"(\d+[.,]){2,}\d+"), #Should be space
    #Dollar not embedded as a symbol 234$ dollar
    SimpleRegexRule("Value suffixed by dollar", r"\d+\$\s*dollars"),
    # Translator missed english-only world
    SimpleRegexRule("Occurrence of untranslated 'year'", r"(?<!%\()[Yy]ear(?!\)s)"), # These are lookbehind/lookhead assertions ;-)
    SimpleRegexRule("Occurrence of untranslated 'time'", r"\s+[tT]imes?(?![A-Za-z])"),
    SimpleRegexRule("Occurrence of untranslated 'is'", r"\b[Ii]s\b"),
    SimpleRegexRule("Occurrence of untranslated 'and'", r"\b[A]nd\b"),
    SimpleRegexRule("Occurrence of untranslated 'great(er)'", r"\b[Gg]reat(er)?\b"),
    SimpleRegexRule("Occurrence of untranslated 'less'", r"\b[L]ess\b"),
    SimpleRegexRule("Occurrence of untranslated 'few(er)'", r"\b[Ff]ew(er)?\b"),
    SimpleRegexRule("Occurrence of untranslated 'equal(s)'", r"\b[Ee]quals?\b"),
    SimpleRegexRule("Occurrence of untranslated 'equivalent", r"\b[Ee]quivalent\b"),
    SimpleRegexRule("Occurrence of untranslated 'piece(s)", r"\b[Pp]ieces?\b"),
    SimpleRegexRule("Occurrence of untranslated 'percent'", r"\b[Pp]ercents?\b"),
    SimpleRegexRule("Occurrence of untranslated 'to'", r"(?<!\\)\b[Tt]o\b"),
    SimpleRegexRule("Occurrence of untranslated 'axis'", r"\b[Aa]xis\b"),
    SimpleRegexRule("Occurrence of untranslated 'multiply'", r"\b[Mm]ultiply\b"),
    SimpleRegexRule("Occurrence of untranslated 'multiplier'", r"\b[Mm]ultiplier\b"),
    SimpleRegexRule("Occurrence of untranslated 'since'", r"\b[Ss]ince\b"),
    SimpleRegexRule("Occurrence of untranslated 'value'", r"\b[Vv]alue\b"),
    SimpleRegexRule("Occurrence of untranslated 'of'", r"\b[Oo]f\b"),
    SimpleRegexRule("Occurrence of untranslated 'numerator'", r"\b[Nn]umerator\b"),
    SimpleRegexRule("Occurrence of untranslated 'denominator'", r"\b[Dd]enominator\b"),
    SimpleRegexRule("Occurrence of untranslated 'Its'", r"\b[Dd]It'?s\b"),
    SimpleRegexRule("Occurrence of untranslated 'inverse' (case-sensitive)", r"\binverse\b"),
    SimpleRegexRule("Occurrence of untranslated 'blue' (not as color specifier)", r"(?<!\\)\b[Bb]lue\b"),
    SimpleRegexRule("Occurrence of untranslated 'purple' (not as color specifier)", r"(?<!\\)\b[Pp]urple\b"),
    SimpleRegexRule("Occurrence of untranslated 'red' (not as color specifier)", r"(?<!\\)\b[Rr]ed\b"),
    SimpleRegexRule("Occurrence of untranslated 'green' (not as color specifier)", r"(?<!\\)\b[Gg]reen\b"),
    SimpleRegexRule("Occurrence of untranslated 'pink' (not as color specifier)", r"(?<!\\)\b[Pp]ink\b"),
    SimpleRegexRule("Occurrence of dollar as string", r"(?<!US-)[Dd]ollars?"), #US-Dollars? allowed
    SimpleSubstringRule("Escaped dollar symbol in formula", r"\\$"),
    #Recommended translations
    TranslationConstraintRule("'word problems' not translated to 'Textaufgaben'", r"word\s+problem", r"textaufgabe", flags=re.UNICODE | re.IGNORECASE),
    TranslationConstraintRule("'Coordinate Plane' not translated to 'Koordinatensystem'", r"coordinate\s+plane", r"Koordinatensystem", flags=re.UNICODE | re.IGNORECASE),
    TranslationConstraintRule("'Inequality' not translated to 'Ungleichung'", r"Inequality", r"Ungleichung", flags=re.UNICODE | re.IGNORECASE),
    #Bing issues
    SimpleRegexRule("Bing (1)", r"!\[\]\s+\("),
    SimpleRegexRule("Bing (2)", r"!\s+\[\]\("),
    NegativeTranslationConstraintRule("False Bing translation of interactive-graphic", r"☃\s+interactive-graph", r"[Ii]nteraktive\s+Grafik"),
    SimpleRegexRule("False Bing translation of Radio", r"Radio\b"),
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
    # Unsorted rules

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
