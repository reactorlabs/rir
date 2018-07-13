#!/bin/bash

REPO="reactorlabs/rir"

SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi

TOKENPATH=$SCRIPTPATH/../.travis_token

if [ ! -f $TOKENPATH ]; then
    echo "Please set your Travis API token"
    echo "Go to https://travis-ci.org/profile and copy the API token to the file '.travis_token' in the repository root."
    exit 1
fi

TOKEN=`cat $TOKENPATH`

if [ $# -ne 1 ]; then
    echo "Initiates a Travis debug build for the given job id"
    echo
    echo "Make sure your Travis API token has been set."
    echo "Go to https://travis-ci.org/profile and copy the API token to the file '.travis_token' in the repository root."
    echo "The <job id> will be in the URL: https://travis-ci.org/$REPO/jobs/<job id>"
    echo "(Note: <job id> is not the same as build id!)"
    echo
    echo "usage: $0 <job id>"
    exit 1
fi

curl -s -X POST \
  -H "Content-Type: application/json" \
  -H "Accept: application/json" \
  -H "Travis-API-Version: 3" \
  -H "Authorization: token $TOKEN" \
  -d "{\"quiet\": true}" \
  https://api.travis-ci.org/job/$1/debug
echo ""

echo "If the response has no error, the job has been restarted in debug mode."
echo "Go to https://travis-ci.org/$REPO/jobs/$1 to get the SSH command"
echo "For more information, see https://docs.travis-ci.com/user/running-build-in-debug-mode/"
