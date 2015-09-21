#!/bin/sh

SCRIPT=$(readlink -f "$0")
SCRIPTPATH=$(dirname "$SCRIPT")

cd $SCRIPTPATH/..

rm -f .git/hooks/pre-commit
ln -s $SCRIPTPATH/../tools/pre-commit-hook .git/hooks/pre-commit

rm -f .git/hooks/pre-push
ln -s $SCRIPTPATH/../tools/pre-push-hook .git/hooks/pre-push
