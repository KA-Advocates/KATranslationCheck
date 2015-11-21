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

    updateTranslationsCmd = subparsers.add_parser('update-translations')
    updateTranslationsCmd.add_argument('-j', '--num-processes', default=16, type=int, help='Number of processes to use for parallel download')
    updateTranslationsCmd.add_argument('-f', '--force-filemap-update', action="store_true", help='Force updating the filemap')
    updateTranslationsCmd.set_defaults(func=updateTranslations)

    updateLint = subparsers.add_parser('update-lint')
    updateLint.set_defaults(func=updateLintHandler)

    render = subparsers.add_parser('render')
    render.add_argument('-d', '--download', action='store_true', help='Download or update the directory')
    render.add_argument('--only-lint', action='store_true', help='Only render the lint hierarchy')
    render.add_argument('outdir', nargs='?', default=None, help='The output directory to use (default: output-<lang>)')
    render.set_defaults(func=performRender)

    args = parser.parse_args()

    # Call args.func, but do not catch AttributeError inside args.func()
    try:
        args.func
        err = False
    except AttributeError:
        parser.print_help()
        err = True
    if not err:
        args.func(args)