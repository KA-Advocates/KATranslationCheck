#!/usr/bin/env python3
# coding: utf-8
from Rules import *

########################
### Initialize rules ###
########################
# Currently hardcoded for DE language
rules = [
    #Coordinate separated by comma instead of |
    SimpleRegexRule("Comma in integral coordinate", r"\$\(\d+\s*\,\s*\d+\)\$"),
    SimpleRegexRule("Comma in non-integral coordinate", r"\$\(\d+[\.,]\d+\s*\,\s*\d+[\.,]\d+\)\$"),
    SimpleRegexRule("{\ } instead of {\,} inside number", r"\d+\{\\\s+\}\d+"),
    #The most simple case of using a decimal point instead
    SimpleRegexRule("Simple number with decimal point instead of comma", r"\$-?\s*\d+\.-?\d+\s*\$"),
    #Simple currency value in dollar (matches comma separated and decimal point)
    SimpleRegexRule("Value with embedded dollar symbol", r"\$\s*\\\\\$\s*\d+([.,]\d+)?\s*\$"),
    #Errors in thousands separation
    SimpleRegexRule("Value with multiple or mixed commata or dots", r"(\d+(\.|\{,\})){2,}\d+"), #Should be space. Comma without {} ignored.
    #Dollar not embedded as a symbol 234$ dollar
    SimpleRegexRule("Value suffixed by dollar", r"\d+\$\s*dollars?"),
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
    SimpleRegexRule("Occurrence of untranslated 'pair'", r"\b[Pp]airs?\b"),
    SimpleRegexRule("Occurrence of untranslated 'has/had'", r"\b[Hh]a[sd]\b"),
    SimpleRegexRule("Occurrence of untranslated 'school'", r"\b[S]chool\b"),
    SimpleRegexRule("Occurrence of untranslated 'shade(d)'", r"(\b|-)[Ss]haded?\b"),    
    SimpleRegexRule("Occurrence of untranslated 'to'", r"(?<!\\)\b[Tt]o\b"),
    SimpleRegexRule("Occurrence of untranslated 'not'", r"(?<!\\)(?<!in)\s*\b[Nn]ot\b"),
    SimpleRegexRule("Occurrence of untranslated 'does", r"(?<!\\)\b[Dd]oesn?\b"),
    SimpleRegexRule("Occurrence of untranslated 'axis'", r"\b[Aa]xis\b"),
    SimpleRegexRule("Occurrence of untranslated 'multiply'", r"\b[Mm]ultiply\b"),
    SimpleRegexRule("Occurrence of untranslated 'multiplier'", r"\b[Mm]ultiplier\b"),
    SimpleRegexRule("Occurrence of untranslated 'since'", r"\b[Ss]ince\b"),
    SimpleRegexRule("Occurrence of untranslated 'value'", r"\b[Vv]alue\b"),
    SimpleRegexRule("Occurrence of untranslated 'of'", r"\b[Oo]f\b"),
    SimpleRegexRule("Occurrence of untranslated 'numerator'", r"\b[Nn]umerator\b"),
    SimpleRegexRule("Occurrence of untranslated 'denominator'", r"\b[Dd]enominator\b"),
    SimpleRegexRule("Occurrence of untranslated 'diameter'", r"\b[Dd]iameter\b"),
    SimpleRegexRule("Occurrence of untranslated 'perimeter'", r"(?<!SHAPE\.)\b[Pp]erimeter\b"),
    SimpleRegexRule("Occurrence of untranslated 'measurement'", r"\b[Mm]easurement\b"),
    SimpleRegexRule("Occurrence of untranslated 'marking'", r"\b[Mm]arkings?\b"),
    SimpleRegexRule("Occurrence of untranslated 'low(er)'", r"\b[Ll]ow(er)?\b"),
    SimpleRegexRule("Occurrence of untranslated 'mass'", r"\b[Mm]ass\b"),
    SimpleRegexRule("Occurrence of untranslated 'high(er)'", r"\b[Hh]igh(er)?\b"),
    SimpleRegexRule("Occurrence of untranslated 'Its'", r"\b[Dd]It'?s\b"),
    SimpleRegexRule("Occurrence of untranslated 'dot(s)", r"\b[Dd]dots?\b"),
    SimpleRegexRule("Occurrence of untranslated '(counter)clockwise", r"\b([Cc]ounter)?-?[Cc]clockwise\b"),
    SimpleRegexRule("Occurrence of untranslated 'inverse' (case-sensitive)", r"\binverse\b"),
    SimpleRegexRule("Occurrence of untranslated 'blue' (not as color specifier)", r"(?<!\\)\b[Bb]lue\b"),
    SimpleRegexRule("Occurrence of untranslated 'purple' (not as color specifier)", r"(?<!\\)\b[Pp]urple\b"),
    SimpleRegexRule("Occurrence of untranslated 'red' (not as color specifier)", r"(?<!\\)\b[Rr]ed\b"),
    IgnoreByMsgidRegexWrapper(r"Green's.*[Tt]heorem",  # Matches 'Green's and Stokes theorem'
        SimpleRegexRule("Occurrence of untranslated 'green' (not as color specifier)",
            r"(?<!\\)\b[Gg]reen\b")),
    SimpleRegexRule("Occurrence of untranslated 'pink' (not as color specifier)", r"(?<!\\)\b[Pp]ink\b"),
    SimpleRegexRule("Occurrence of dollar as string", r"(?<!US-)[Dd]ollars?"), #US-Dollars? allowed
    SimpleSubstringRule("Escaped dollar symbol", r"\\+$"),
    SimpleRegexRule("'Sie' instead of 'Du'", r"\bSie\b"),
    SimpleRegexRule("'Ihre' instead of 'Deine'", r"\bIhre[rms]?\b"),
    #Recommended translations
    TranslationConstraintRule("'word problems' not translated to 'Textaufgaben'", r"word\s+problem", r"textaufgabe", flags=re.UNICODE | re.IGNORECASE),
    TranslationConstraintRule("'Coordinate Plane' not translated to 'Koordinatensystem'", r"coordinate\s+plane", r"Koordinatensystem", flags=re.UNICODE | re.IGNORECASE),
    TranslationConstraintRule("'Inequality' not translated to 'Ungleichung'", r"Inequality", r"Ungleichung", flags=re.UNICODE | re.IGNORECASE),
    TranslationConstraintRule("'inverse function' not translated to 'Umkehrfunktion'", r"inverse\s+function", r"Umkehrfunktion", flags=re.UNICODE | re.IGNORECASE),
    NegativeTranslationConstraintRule("'shaded' translated to 'schraffiert' instead of 'eingefärbt'", r"shaded", r"schraffiert", flags=re.UNICODE | re.IGNORECASE),
    NegativeTranslationConstraintRule("'shaded' translated to 'schattiert' instead of 'eingefärbt'", r"shaded", r"schattiert", flags=re.UNICODE | re.IGNORECASE),
    NegativeTranslationConstraintRule("'scientific notation' translated to 'wissenschaftliche Schreibweise' instead of 'Exponentialschreibweise'", r"scientific\s+notation", r"wissenschaftliche\s+schreibweise", flags=re.UNICODE | re.IGNORECASE),
    NegativeTranslationConstraintRule("'Coach' translated to 'Trainer' instead of 'Coach'", r"coach", r"trainer", flags=re.UNICODE | re.IGNORECASE),
    NegativeTranslationConstraintRule("'Challenge' translated to 'Herausforderung' instead of 'challenge'", r"challenge", r"herausforderung", flags=re.UNICODE | re.IGNORECASE),
    NegativeTranslationConstraintRule("'Post' translated to 'Post' instead of 'Beitrag'", r"post", r"post", flags=re.UNICODE | re.IGNORECASE),
    # E-Mail must be written exactly "E-Mail". Exceptions: {{email}}, %(error_email), %(email), {{ email }}
    SimpleRegexRule("Wrong syntax of E-Mail", r"(?<!%\()(?<!%\(error_)(?<!\{\{)(?<!\{\{ )(eMail|email|Email|EMail|e-Mail|e-mail)s?"),
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
    # Machine-readable stuff must be identical in the translation
    ExactCopyRule("All image URLs must match in order", r"!\[\]\s*\([^\)]+\)"),
    ExactCopyRule("All GUI elements must match in order", r"\[\[☃\s+[a-z-]+\s*\d*\]\]"),
    # Unsorted rules
]

if __name__ == "__main__":
    #Rule tests. python3 Rules.py to run
    assert(findRule(rules, "Comma in integral coordinate")("$(12,3)$", ""))
    assert(findRule(rules, "Simple number with decimal point instead of comma")("$12.3$", ""))
    assert(findRule(rules, "Value with embedded dollar symbol")("$\\\\$12$", ""))
    assert(findRule(rules, "Value with embedded dollar symbol")("$\\\\$12.5$", ""))
    assert(findRule(rules, "Value with embedded dollar symbol")("$\\\\$12,5$", ""))
    assert(findRule(rules, "Value suffixed by dollar")("$1,234$ dollar", ""))
