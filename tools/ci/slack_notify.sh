#!/bin/bash

SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi

. "${SCRIPTPATH}/../../.local.config"
. "${SCRIPTPATH}/../../.test_results"

SLACK_WEBHOOK_URL="https://hooks.slack.com/services/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"
SLACK_NOTIFIER_USERNAME=cibot
SLACK_NOTIFIER_CHANNEL=""
APPLICATION_NAME="rjit"
CI="TeamCity"
GITHUB="https://github.com/reactorlabs/rjit/compare/"
STATUS="passed"
COLOR="#006400"

if [ -z "$TRAVIS" ]; then
  # since teamcity doesn't track step, we need special way to test it
  # those variables come from local.config
  if [ -z "$SETUP_SUCCESS" ] || [ -z "$INTEGRATION_TESTS_SUCCESS" ]; then
    STATUS="failed"
  fi
  COMMIT_ID=$BUILD_VCS_NUMBER
  GIT_BRANCH=`git rev-parse --abbrev-ref HEAD`
  SLACK_NOTIFIER_ICON_URL="https://secure.gravatar.com/avatar/a08fc43441db4c2df2cef96e0cc8c045?s=140"
  BUILD_URL="https://reactor.ccs.neu.edu:8111/"
else
  # running at travis
  CI="Travis (Ubuntu)"
  if [ "$TRAVIS_OS_NAME" == "osx" ]; then
      CI="Travis (OSX)"
  fi
  GIT_BRANCH=$TRAVIS_BRANCH
  SLACK_NOTIFIER_ICON_URL="https://secure.gravatar.com/avatar/a08fc43441db4c2df2cef96e0cc8c045?s=140"
  if [ "$TRAVIS_TEST_RESULT" != "0" ]; then
      STATUS="*failed*"
      COLOR="#FF0000"
  fi
  COMMIT_ID=$TRAVIS_COMMIT
  BUILD_URL="https://travis-ci.org/reactorlabs/rjit/jobs/$TRAVIS_JOB_ID"
fi

STARTED_BY=`git --no-pager show -s --format="%ae" $COMMIT_ID`

case $STARTED_BY in
    "roman.tsegelskyi@gmail.com")
    SLACK_NOTIFIER_CHANNEL="@romantsegelskyi"
    ;;
    "janvite@yahoo.com")
    SLACK_NOTIFIER_CHANNEL="@j.vitek"
    ;;
    "o@o1o.ch")
    SLACK_NOTIFIER_CHANNEL="@o-"
    ;;
    "peta.maj82@gmail.com")
    SLACK_NOTIFIER_CHANNEL="@peta"
    ;;
    "paleysandbuckets@gmail.com")
    SLACK_NOTIFIER_CHANNEL="@paley"
    ;;
    "ghosn.adrien@gmail.com")
    SLACK_NOTIFIER_CHANNEL="@aghosn"
    ;;
    *)
    ;;
esac

COMMIT_ID_SHORT=${COMMIT_ID:0:7}
MESSAGE="<$BUILD_URL|build> for <$GITHUB$TRAVIS_COMMIT_RANGE|$TRAVIS_COMMIT_RANGE> on branch \`$GIT_BRANCH\` - $STATUS"

send_notification() {
    CHANNEL=$1
    # construct the json
    json="{"

    if [ ! -z "$CHANNEL" ]; then
        json=$json"\"channel\": \"$CHANNEL\","
    fi

    json=$json"
    \"username\": \"$SLACK_NOTIFIER_USERNAME\",
    \"icon_url\":\"$SLACK_NOTIFIER_ICON_URL\",
    \"attachments\":[
      {
        \"title\":\"$CI\",
        \"color\": \"$COLOR\",
        \"text\": \"$MESSAGE\",
        \"mrkdwn_in\": [\"text\"]
      }
    ]
   }"

    # post the result to the slack webhook
    if ! [[ -z "$SLACK_NOTIFIER_CHANNEL" ]]; then
        RESULT=$(curl -d "payload=$json" -s "$SLACK_WEBHOOK_URL" --output "slack_result.txt" -w "%{http_code}")
    fi

    if [ "$RESULT" = "500" ]; then
        if grep -Fqx "No token" "slack_result.txt"; then
            fail "No token is specified."
        fi

        if grep -Fqx "No hooks" "slack_result.txt"; then
            fail "No hook can be found for specified subdomain/token"
        fi

        if grep -Fqx "Invalid channel specified" "slack_result.txt"; then
            fail "Could not find specified channel for subdomain/token."
        fi

        if grep -Fqx "No text specified" "slack_result.txt"; then
            fail "No text specified."
        fi
    fi

    if [ "$RESULT" = "404" ]; then
        fail "Subdomain or token not found."
    fi
}

send_notification $SLACK_NOTIFIER_CHANNEL

if [ "$GIT_BRANCH" == "master" ]; then
    send_notification "#r-on-llvm"
fi
