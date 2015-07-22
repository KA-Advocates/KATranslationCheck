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
    Remember to implement __call__(self, msgstr, msgid=None),
    which must return the hit or None if no hit is found.
    """
    def __init__(self, name):
        self.name = name
    def get_machine_name(self):
        """Get a machine-readable name from a rule name"""
        return self.name.lower().replace(" ", "-")
    def apply_to_po_set(self, poset):
        """
        Apply to a dictionary of parsed PO files.
        Yields tuples entry, hit, filename
        """
        for filename, po in poset.items():
            for entry in po:
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
    def __call__(self, msgstr, msgid=None):
        hit = self.re.search(msgstr)
        if hit:
            return hit.group(0)
        return None

class SimpleSubstringRule(Rule):
    """
    A simple rule type that hits when a given substring is found in the msgstr.
    """
    def __init__(self, name, substr):
        super().__init__(name)
        self.substr = substr
    def __call__(self, msgstr, msgid=None):
        if msgstr.find(self.substr) != -1:
            return self.substr
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
#Simple currency value in dollar (matches comma separated and decimal point)
    SimpleRegexRule("Simple Dollar currency", r"\$\s*\\\\\$\s*\d+([.,]\d+)?\s*\$")
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
    assert(findRule("Simple Dollar currency")("$\\\\$12$"))
    assert(findRule("Simple Dollar currency")("$\\\\$12.5$"))
    assert(findRule("Simple Dollar currency")("$\\\\$12,5$"))
