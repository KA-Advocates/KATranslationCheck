#!/usr/bin/env python3
from UpdateAllFiles import updateTranslations
from check import performRender
from LintReport import updateLintFromGoogleGroups

def updateLintHandler(args):
    updateLintFromGoogleGroups()

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(title="Commands")
    # Generic argument
    parser.add_argument('-l', '--language', default="de", help='The language directory to use/extract (e.g. de, es)')

    update = subparsers.add_parser('update-translations')
    update.add_argument('-j', '--num-processes', default=16, type=int, help='Number of processes to use for parallel download')
    update.add_argument('-f', '--force-filemap-update', action="store_true", help='Force updating the filemap')
    update.set_defaults(func=updateTranslations)

    updateLint = subparsers.add_parser('update-lint')
    update.set_defaults(func=updateLintHandler)

    render = subparsers.add_parser('render')
    render.add_argument('-d', '--download', action='store_true', help='Download or update the directory')
    render.add_argument('outdir', nargs='?', default=None, help='The output directory to use (default: output-<lang>)')
    render.set_defaults(func=performRender)

    args = parser.parse_args()

    try:
        args.func(args)
    except AttributeError:
        parser.print_help()