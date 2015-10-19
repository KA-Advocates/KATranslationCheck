#!/usr/bin/env python3
# coding: utf-8
from Rules import *
from ImageAliases import readImageAliases
from collections import defaultdict

imageAliases = defaultdict(str) #readImageAliases("177zIAO37SY6xUBUyUE30kn5G_wR7oT-txY8XIN7cecU")

########################
### Initialize rules ###
########################
# Currently hardcoded for DE language
rules = [
    #Coordinate separated by comma instead of |
    SimpleRegexRule("Comma in coordinate (| required)", r"\$\(-?\d+([\.,]\d+)?\s*,\s*-?\d+([\.,]\d+)?\)\$", severity=Severity.warning),
    # Translator missed english-only world
    SimpleRegexRule("Occurrence of untranslated 'year'", r"(?<!%\()[Yy]ear(?!\)s)", severity=Severity.standard), # These are lookbehind/lookhead assertions ;-)
    SimpleRegexRule("Occurrence of untranslated 'time'", r"\s+[tT]imes?(?![A-Za-z])(?!-[Oo]ut)(?!_\d)", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'is'", r"(\b|\\n)[Ii]s\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'and'", r"(\b|\\n)[A]nd\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'because'", r"(\b|\\n)[Bb]ecause\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'due'", r"\b[Dd]ue\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'great(er)'", r"\b[Gg]reat(er)?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'less'", r"\b[L]ess\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'few(er)'", r"\b[Ff]ew(er)?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'equal(s)'", r"(?<!-)\b[Ee]quals?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'equivalent", r"\b[Ee]quivalen(t|cy)\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'intercept", r"\b[Ii]ntercepts?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'square", r"(?<!\\)\b[Ss]quares?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'piece(s)", r"\b[Pp]ieces?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'percent'", r"\b[Pp]ercents?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'only'", r"\b[Oo]nly\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'or'", r"\b[Oo]r\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'odd'", r"\b[Oo]dd\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'even'", r"(\b|\\n)[Ee]ven\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'onto'", r"\b[Oo]nto\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'step'", r"(?<![#/])\b[Ss]tep\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'eliminate'", r"\b[Ee]liminate\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'similar'", r"\b[Ss]imilar(ity)?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'pair'", r"\b[Pp]airs?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'she'", r"(\b|\\n)[Ss]he\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'has/had'", r"\b[Hh]a[sd]\b", severity=Severity.standard),
    # Recommended translations
    TranslationConstraintRule("'perpendicular bisector' not translated to 'Mittelsenkrechte'", r"perpendicular\s+bisector", r"mittelsenkrechte", severity=Severity.standard, flags=re.UNICODE | re.IGNORECASE),
    TranslationConstraintRule("'angle bisector' not translated to 'Winkelhalbierende'", r"angle\s+bisector", r"Winkelhalbierende", severity=Severity.standard, flags=re.UNICODE | re.IGNORECASE),
    TranslationConstraintRule("'scalene' not translated to 'ungleichseitig'", r"scalene", r"ungleichseitig", severity=Severity.standard, flags=re.UNICODE | re.IGNORECASE),
    TranslationConstraintRule("'standard quadratic form' not translated to 'allgemeine form'", r"standard\s+quadratic\s+form", r"allgemeiner?\s+form", severity=Severity.standard, flags=re.UNICODE | re.IGNORECASE),
    TranslationConstraintRule("'rectangular coordinates' not translated to 'kartesische Koordinaten'", r"rectangular\s+coordinates?", r"kartesischen?\s+Koordinaten?", severity=Severity.standard, flags=re.UNICODE | re.IGNORECASE),
    TranslationConstraintRule("'interquartile range' not translated to 'Interquartilsabstand'", r"interquartile\s+range", r"Interquartilsabstand", severity=Severity.standard, flags=re.UNICODE | re.IGNORECASE),
    TranslationConstraintRule("'two-way table' not translated to 'Kontingenztafel'", r"two-way\s+table", r"Kontingenztafel", severity=Severity.standard, flags=re.UNICODE | re.IGNORECASE),
    NegativeTranslationConstraintRule("False Bing translation of interactive-graphic", r"☃\s+interactive-graph", r"[Ii]nteraktive\s+Grafik", severity=Severity.dangerous),
    SimpleRegexRule("False Bing translation of Radio", r"☃\s+Radio\b", severity=Severity.dangerous),
    SimpleRegexRule("False Bing translation of input-number", r"[Ee]ingabe-Zahl", severity=Severity.dangerous),
    SimpleRegexRule("False Bing translation of input-number", r"[Ee]ingabe-Nummer", severity=Severity.dangerous),
    SimpleRegexRule("False Bing translation of numeric-input", r"[Nn]umerische[-\s]+Eingabe", severity=Severity.dangerous),
    SimpleRegexRule("False Bing translation of numeric-input", r"[Nn]umerische[-\s]+Eingang", severity=Severity.dangerous),
    SimpleRegexRule("False Bing translation of image", r"☃\s+Bild", severity=Severity.dangerous),
    SimpleRegexRule("Missing translation of **How", r"\*\*[Hh]ow", severity=Severity.dangerous),
    SimpleRegexRule("Missing translation of **What", r"\*\*[Ww]hat", severity=Severity.dangerous),
    SimpleRegexRule("Missing translation of ones", r"\\text\{\s*ones\}\}", severity=Severity.dangerous),
    IgnoreByMsgstrRegexWrapper(r"\d+\^\{\\large\\text\{ten?\}",
        SimpleRegexRule("Missing translation of ten(s)", r"(?<!\d)\^?\{?(\\large)?\\text\{\s*tens?\}\}", severity=Severity.info)),
    SimpleRegexRule("Missing translation of hundred(s)", r"\\text\{\s*hundreds?\}\}", severity=Severity.dangerous),
    # Typos
    SimpleRegexRule("Typo: Multiplikaiton instead of Multiplikation", r"\b[Mm]ultiplikaiton\b", severity=Severity.info),
    # Untranslated stuff directly after \n (not machine translated)
    SimpleRegexRule("Untranslated 'First' after \\n", r"\\nFirst", severity=Severity.standard),
    # Machine-readable stuff must be identical in the translation
    ExactCopyRule("All image URLs must match in order", r"!\[\]\s*\([^\)]+\)", severity=Severity.warning, aliases=imageAliases),
    ExactCopyRule("All GUI elements must match in order", r"\[\[☃\s+[a-z-]+\s*\d*\]\]", severity=Severity.warning),
    # Unsorted stuff
    TranslationConstraintRule("'expression' not translated to 'Term'", r"(?<!polynomial )(?<!quadratic )(?<!☃ )expression", r"term", severity=Severity.notice, flags=re.UNICODE | re.IGNORECASE),
    TranslationConstraintRule("'polynomial expression' not translated to 'Polynom'", r"polynomial expression", r"Polynom", severity=Severity.notice, flags=re.UNICODE | re.IGNORECASE),
]

if __name__ == "__main__":
    print("Counting %d rules" % len(rules))
