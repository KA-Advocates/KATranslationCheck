#!/usr/bin/env python3
# coding: utf-8
from Rules import *
import csv
from collections import defaultdict

def readImageAliases():
    with open('de-image-aliases.csv') as csvfile:
        reader = csv.reader(csvfile, delimiter=',')
        aliases = defaultdict(str)
        aliases.update({"![](" + row[0] + ")": "![](" + row[1] + ")" for row in reader})
        return aliases

imageAliases = readImageAliases()

########################
### Initialize rules ###
########################
# Currently hardcoded for DE language
rules = [
    #Coordinate separated by comma instead of |
    SimpleRegexRule("Comma in coordinate (| required)", r"\$\(-?\d+([\.,]\d+)?\s*,\s*-?\d+([\.,]\d+)?\)\$", severity=Severity.warning),
    SimpleRegexRule("Semicolon in coordinate (| required)", r"\$\(-?\d+([\.,]\d+)?\s*\;\s*-?\d+([\.,]\d+)?\)\$", severity=Severity.warning),
    SimpleRegexRule("{\ } instead of {\,} inside number", r"\d+\{\\\s+\}\d+", severity=Severity.notice),
    SimpleRegexRule("'.* *' needs to be '.**', could cause bad formatting", r"\.\s*\*\s+\*", severity=Severity.warning),
    SimpleRegexRule("Decimal dot instead of comma inside number (high sensitivity rule)", r"\d+\.\d+", severity=Severity.standard),
    # Three cases of thin space missing in coordinate
    SimpleRegexRule("Space inserted between **", r"(?<!\*)\* \*(?!\*)", severity=Severity.info),
    SimpleRegexRule("Missing thin space ({\\,}) before or after |-separated coordinate", r"\$?\(\d+([\.,]\d+)?\s*\|\s*\d+([\.,]\d+)?\)\$?", severity=Severity.info),
    #The most simple case of using a decimal point instead
    SimpleRegexRule("Simple number with decimal point instead of comma", r"\$-?\s*\d+\.-?\d+\s*\$", severity=Severity.warning),
    SimpleRegexRule("Wrong or missing space between number and € ({\\,} required)", r"\d+( |  |\{,\}|\{\\ \})?€"),
    IgnoreByMsgidRegexWrapper(r"^[^\$]+$", # No dollar in string
        SimpleRegexRule("Plain comma used instead of {,}", r"\d+,\d+", severity=Severity.info)),
    #Simple currency value in dollar (matches comma separated and decimal point)
    SimpleRegexRule("Value with embedded dollar symbol", r"\$\s*\\\\?\$\s*\d+([.,]\d+)?\s*\$", severity=Severity.info),
    #Errors in thousands separation
    SimpleRegexRule("Value with multiple or mixed commata or dots", r"(\d+(\.|\{,\})){2,}\d+", severity=Severity.dangerous), #Should be space. Comma without {} ignored.
    #Dollar not embedded as a symbol 234$ dollar
    SimpleRegexRule("Value suffixed by dollar", r"\d+\$\s*dollars?", severity=Severity.info),
    SimpleRegexRule("Additional spaces after * for italic word", r"(?<!\*)(?<!\w)\*\s+\w+\s+\*(?!\*)", severity=Severity.info), # Need to avoid hit for *kleiner* oder *größer* etc.
    SimpleRegexRule("Missing thin space before percent (or not escaped correctly) {\\,}\\%", r"(?<!\{\\,\}\\)%\s*\$", severity=Severity.info),
    SimpleRegexRule("Percent symbol in formula not escaped", r"(?<!\\)%\s*\$", severity=Severity.warning),
    # Translator missed english-only world
    SimpleRegexRule("Occurrence of untranslated 'year'", r"(?<!%\()[Yy]ear(?!\)s)", severity=Severity.standard), # These are lookbehind/lookhead assertions ;-)
    SimpleRegexRule("Occurrence of untranslated 'time'", r"\s+[tT]imes?(?![A-Za-z])", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'is'", r"\b[Ii]s\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'and'", r"\b[A]nd\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'great(er)'", r"\b[Gg]reat(er)?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'less'", r"\b[L]ess\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'few(er)'", r"\b[Ff]ew(er)?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'equal(s)'", r"\b[Ee]quals?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'equivalent", r"\b[Ee]quivalent\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'piece(s)", r"\b[Pp]ieces?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'percent'", r"\b[Pp]ercents?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'pair'", r"\b[Pp]airs?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'has/had'", r"\b[Hh]a[sd]\b", severity=Severity.standard),
    IgnoreByFilenameListWrapper(["de/4_low_priority/about.team.pot"],
            SimpleRegexRule("Occurrence of untranslated 'school'", r"(?<!Old-)(?<!High[- ])(?<!Marlborough[- ])(?<!World[- ])\b[S]chool\b", severity=Severity.standard)),
    SimpleRegexRule("Occurrence of untranslated 'shade(d)'", r"(\b|-)[Ss]haded?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'to'", r"(?<!\\)\b[Tt]o\b", severity=Severity.info),
    SimpleRegexRule("Occurrence of untranslated 'not'", r"(?<!\\)(?<!in)\s*\bnot\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'does", r"(?<!\\)(?<!-)\b[Dd]oesn?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'axis'", r"\b[Aa]xis\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'multiply'", r"\b[Mm]ultiply\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'multiplier'", r"\b[Mm]ultiplier\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'since'", r"\b[Ss]ince\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'value'", r"(?<!%\()\b[Vv]alues?\b(?!\)s)(?!=)", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'numerator'", r"\b[Nn]umerator\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'column'", r"\b[Cc]olumn\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'denominator'", r"\b[Dd]enominator\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'diameter'", r"\b[Dd]iameter\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'perimeter'", r"(?<!SHAPE\.)\b[Pp]erimeter\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'measurement'", r"\b[Mm]easurement\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'marking'", r"\b[Mm]arkings?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'low(er)'", r"\b[Ll]ow(er)?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'mass'", r"\b[Mm]ass\b", severity=Severity.standard),
    IgnoreByMsgidRegexWrapper(r"(Ridgemont|Junior|Senior|Riverside)\s+High\b",
        SimpleRegexRule("Occurrence of untranslated 'high(er)'", r"\b[Hh]igh(er)?\b(?!-[Ss]chool)(?! [Ss]chool)(?! Tides)", severity=Severity.info)),
    SimpleRegexRule("Occurrence of untranslated 'Its'", r"\b[Dd]It'?s\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'dot(s)", r"\b[Dd]dots?\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated '(counter)clockwise", r"\b([Cc]ounter)?-?[Cc]clockwise\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'blue' (not as color specifier)", r"(?<!\\color\{)(?<!\\)\b[Bb]lue\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'purple' (not as color specifier)", r"(?<!\\)\b[Pp]urple\b", severity=Severity.standard),
    SimpleRegexRule("Occurrence of untranslated 'red' (not as color specifier)", r"(?<!\\)\b[Rr]ed\b", severity=Severity.standard),
    IgnoreByMsgidRegexWrapper(r"(Summer|Hour|Art|Lots)\s+of\s+(Drawing|Code|Script(ing)?|Webpage|Databases|Problem|Fun)",
        SimpleRegexRule("Occurrence of untranslated 'of'", r"\b[Oo]f\b(?!-)", severity=Severity.info)), #Also allow of inside links etc.
    IgnoreByMsgidRegexWrapper(r"[Gg]reen'?s.+[Tt]heorem",
        SimpleRegexRule("Occurrence of untranslated 'green' (not as color specifier)", r"(?<!\\)\b[Gg]reen\b", severity=Severity.standard)),
    IgnoreByTcommentRegexWrapper("/measuring-and-converting-money-word-problems", # Ignore for conversion exercises 
        SimpleRegexRule("Occurrence of dollar as string", r"(?<!US-)[Dd]ollars?(?!ville)(?!-Schein)", severity=Severity.notice)), #US-Dollars? & Dollarville allowed
    SimpleSubstringRule("Escaped dollar symbol", r"\\+$"),
    IgnoreByFilenameRegexWrapper(r"^de/1_high_priority_platform", SimpleRegexRule("'Sie' instead of 'Du'", r"\bSie\b", severity=Severity.notice), invert=True),
    IgnoreByFilenameRegexWrapper(r"^de/1_high_priority_platform", SimpleRegexRule("'Ihre' instead of 'Deine'", r"\bIhre[rms]?\b", severity=Severity.notice), invert=True),
    # Something was translated that must NOT be translated
    SimpleRegexRule("Occurrence of wrongly translated 'Khan Akademie'", r"[Kk]han\s+Akademie", severity=Severity.dangerous),
    SimpleRegexRule("Translated color in command", r"(\\color\{|\\\\)([Bb]lau|[Rr]ot|[Gg]elb|[Gg]rün|[Vv]iolett|[Ll]ila)", severity=Severity.dangerous),
    # Orthographic rules. Generally low severity
    SimpleRegexRule("daß needs to be written as dass", r"\b[Dd]aß\b", severity=Severity.info),
    SimpleRegexRule("Ausreisser needs to be written as Ausreißer", r"\b[Aa]usreisser\b", severity=Severity.info),
    # Recommended translations
    TranslationConstraintRule("'word problems' not translated to 'Textaufgaben'", r"word\s+problem", r"textaufgabe", severity=Severity.standard, flags=re.UNICODE | re.IGNORECASE),
    TranslationConstraintRule("'Coordinate Plane' not translated to 'Koordinatensystem'", r"coordinate\s+plane", r"Koordinatensystem", severity=Severity.standard, flags=re.UNICODE | re.IGNORECASE),
    TranslationConstraintRule("'Inequality' not translated to 'Ungleichung'", r"Inequality", r"Ungleich(ung|heit|zeichen)", severity=Severity.standard, flags=re.UNICODE | re.IGNORECASE),
    TranslationConstraintRule("'inverse function' not translated to 'Umkehrfunktion'", r"inverse\s+function", r"Umkehrfunktion", severity=Severity.standard, flags=re.UNICODE | re.IGNORECASE),
    NegativeTranslationConstraintRule("'mile(s)' translated to 'Meile(n)' instead of 'Kilometer'", r"miles?", r"(?<!\")meilen?", severity=Severity.standard, flags=re.UNICODE | re.IGNORECASE),
    NegativeTranslationConstraintRule("'shaded' translated to 'schraffiert' instead of 'eingefärbt'", r"shaded", r"schraffiert", severity=Severity.standard, flags=re.UNICODE | re.IGNORECASE),
    NegativeTranslationConstraintRule("'shaded' translated to 'schattiert' instead of 'eingefärbt'", r"shaded", r"schattiert", severity=Severity.standard, flags=re.UNICODE | re.IGNORECASE),
    NegativeTranslationConstraintRule("'scientific notation' translated to 'wissenschaftliche Schreibweise' instead of 'Exponentialschreibweise'", r"scientific\s+notation", r"wissenschaftliche\s+schreibweise", severity=Severity.warning, flags=re.UNICODE | re.IGNORECASE),
    NegativeTranslationConstraintRule("'Coach' translated to 'Trainer' instead of 'Coach'", r"coach", r"trainer", severity=Severity.info, flags=re.UNICODE | re.IGNORECASE),
    NegativeTranslationConstraintRule("'Challenge' translated to 'Herausforderung' instead of 'challenge'", r"challenge", r"herausforderung", severity=Severity.info, flags=re.UNICODE | re.IGNORECASE),
    IgnoreByMsgidRegexWrapper(r"post\s+office",
        NegativeTranslationConstraintRule("'Post' translated to 'Post' instead of 'Beitrag'", r"\bpost\s*(?!card)(?!alCode)(?!man)(?!office)(?!-Money)", r"\bpost(?!karte)(?!amt)(?!en)(?!e)", severity=Severity.info, flags=re.UNICODE | re.IGNORECASE)),
    # E-Mail must be written exactly "E-Mail". Exceptions: {{email}}, %(error_email), %(email), %(coach_email) {{ email }}
    SimpleRegexRule("Wrong syntax of E-Mail", r"(?<!%\()(?<!%\(coach_)(?<!%\(child_)(?<!%\(error_)(?<!\{\{)(?<!\{\{)\s*(eMail|email|Email|EMail|e-Mail|e-mail)s?", severity=Severity.info),
    # Bing issues
    SimpleRegexRule("Space inserted after image URL declaration ('![] (')", r"!\[\]\s+\(", severity=Severity.dangerous),
    SimpleRegexRule("Space inserted before image URL declaration ('! [](')", r"!\s+\[\]\(", severity=Severity.dangerous),
    SimpleRegexRule("Space inserted before & after image URL declaration ('! [] (')", r"!\s+\[\]\s+\(", severity=Severity.dangerous),
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
    IgnoreByMsgidRegexWrapper(r"\d+\^\{\\large\\text\{ten?",
        SimpleRegexRule("Missing translation of ten(s)", r"(?<!\d)\^?\{?(\\large)?\\text\{\s*tens?\}\}", severity=Severity.info)),
    SimpleRegexRule("Missing translation of hundred(s)", r"\\text\{\s*hundreds?\}\}", severity=Severity.dangerous),
    # Machine-readable stuff must be identical in the translation
    ExactCopyRule("All image URLs must match in order", r"!\[\]\s*\([^\)]+\)", severity=Severity.warning, aliases=imageAliases),
    ExactCopyRule("All GUI elements must match in order", r"\[\[☃\s+[a-z-]+\s*\d*\]\]", severity=Severity.warning),
    # Unsorted severityes
]

if __name__ == "__main__":
    #Rule tests. python3 Rules.py to run
    assert(findRule(rules, "Comma in coordinate (| required)")("$(12,3)$", ""))
    assert(findRule(rules, "Simple number with decimal point instead of comma")("$12.3$", ""))
    assert(findRule(rules, "Value with embedded dollar symbol")("$\\\\$12$", ""))
    assert(findRule(rules, "Value with embedded dollar symbol")("$\\\\$12.5$", ""))
    assert(findRule(rules, "Value with embedded dollar symbol")("$\\\\$12,5$", ""))
    assert(findRule(rules, "Value suffixed by dollar")("$1,234$ dollar", ""))
