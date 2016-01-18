#!/bin/bash

SCRIPTPATH=`cd $(dirname "$0") && pwd`
if [ ! -d $SCRIPTPATH ]; then
    echo "Could not determine absolute dir of $0"
    echo "Maybe accessed with symlink"
fi

. "${SCRIPTPATH}/../.local.config"
. "${SCRIPTPATH}/../.test_results"

SLACK_WEBHOOK_URL="https://hooks.slack.com/services/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"
SLACK_NOTIFIER_USERNAME=cibot
APPLICATION_NAME="rjit"
CI="TeamCity"
BITBUCKET="https://bitbucket.org/reactorl/rjit/commits/"
STATUS="passed"

if [ -z "$WERCKER_ROOT"]; then
  # since teamcity doesn't track step, we need special way to test it
  # those variables come from local.config
  if [ -z "$SETUP_SUCCESS"] || [ -z "$INTEGRATION_TESTS_SUCCESS" ]; then
    STATUS="failed"
  fi
  COMMIT_ID=$BUILD_VCS_NUMBER
  GIT_BRANCH=`git rev-parse --abbrev-ref HEAD`
  SLACK_NOTIFIER_ICON_URL="https://secure.gravatar.com/avatar/a08fc43441db4c2df2cef96e0cc8c045?s=140"
  BUILD_URL="https://reactor.ccs.neu.edu:8111/"
else
# running at wercker
  CI="Wercker"
  GIT_BRANCH=$WERCKER_GIT_BRANCH
  SLACK_NOTIFIER_ICON_URL="https://secure.gravatar.com/avatar/a08fc43441db4c2df2cef96e0cc8c045?s=140"
  STATUS=$WERCKER_RESULT
  COMMIT_ID=$WERCKER_GIT_COMMIT
  BUILD_URL=$WERCKER_BUILD_URL
fi

STARTED_BY=`git --no-pager show -s --format="%aN" $COMMIT_ID`

case $STARTED_BY in
    RomanTsegelskyi|"Roman Tsegelskyi")
    SLACK_NOTIFIER_CHANNEL="@romantsegelskyi"
    ;;
    "jan vitek")
    SLACK_NOTIFIER_CHANNEL="@j.vitek"
    ;;
    o--)
    SLACK_NOTIFIER_CHANNEL="@o-"
    ;;
    peta)
    SLACK_NOTIFIER_CHANNEL="@peta"
    ;;
    Pales|"Paley Li")
    SLACK_NOTIFIER_CHANNEL="@paley"
    ;;
    "Adrien Ghosn")
    SLACK_NOTIFIER_CHANNEL="@aghosn"
    ;;
    *)
    ;;
esac

COMMIT_ID_SHORT=${COMMIT_ID:0:7}
MESSAGE="$CI: <$BUILD_URL|build> for <$BITBUCKET$COMMIT_ID|$COMMIT_ID_SHORT> on branch $GIT_BRANCH - $STATUS"

# construct the json
json="{"

# channels are optional, dont send one if it wasnt specified
if [ -n "$SLACK_NOTIFIER_CHANNEL" ]; then 
    json=$json"\"channel\": \"$SLACK_NOTIFIER_CHANNEL\","
fi

json=$json"
    \"username\": \"$SLACK_NOTIFIER_USERNAME\",
    \"icon_url\":\"$SLACK_NOTIFIER_ICON_URL\",
    \"attachments\":[
      {
        \"text\": \"$MESSAGE\",
        \"color\": \"$COLOR\"
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

if [ "$GIT_BRANCH" != "master" ]; then
  exit 0;
fi

MESSAGE="$CI: <$BUILD_URL|build> for <$BITBUCKET$COMMIT_ID|$COMMIT_ID_SHORT> on branch $GIT_BRANCH by $SLACK_NOTIFIER_CHANNEL- $STATUS"

# construct the json
json="{
    \"username\": \"$SLACK_NOTIFIER_USERNAME\",
    \"icon_url\":\"$SLACK_NOTIFIER_ICON_URL\",
    \"attachments\":[
      {
        \"text\": \"$MESSAGE\",
        \"color\": \"$COLOR\"
      }
    ]
}"

# post the result to the slack webhook
RESULT=$(curl -d "payload=$json" -s "$SLACK_WEBHOOK_URL" --output "slack_result.txt" -w "%{http_code}")

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
