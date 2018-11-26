#!/bin/sh

SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi

cd $SCRIPTPATH/..

rm -f .git/hooks/pre-commit
ln -s $SCRIPTPATH/../tools/pre-commit-hook .git/hooks/pre-commit

rm -f .git/hooks/pre-push
ln -s $SCRIPTPATH/../tools/pre-push-hook .git/hooks/pre-push
