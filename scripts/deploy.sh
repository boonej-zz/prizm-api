#!/bin/bash

ENV=$1
ROLLBACK=""
if [ $2 ] 
then
  ROLLBACK=$2
fi
DEV="ec2-54-186-28-238.us-west-2.compute.amazonaws.com"
STAG="ec2-54-200-41-62.us-west-2.compute.amazonaws.com"
TEST="ec2-54-187-131-68.us-west-2.compute.amazonaws.com"
DIR_PATH="/var/www/prism_api"
DEPLOY_USER="ec2-user"
SSHI="$HOME/.ssh/PrismAPIDev.pem"
SERVER=""
SSHCMD=""

deploy() {
  environment
  echo "Creating prism_api rollback"
  create_rollback
  echo "Syncing source files"
  rsync --exclude 'node_modules' --exclude '.git'  -r -e "ssh -i $SSHI" ./* ec2-user@$SERVER:$DIR_PATH

  if [ "$?" -ne "0" ]; then
    echo $?
    echo -n "Error while rsyncing files."
    exit 1
  fi

  echo "Finished syncing source"
  post_deploy
}

post_deploy() {
  echo "Installing NPM modules"
  ssh -i $SSHI ec2-user@$SERVER "cd /var/www/prism_api && npm install"
  echo "Restarting prism service"
  ssh -i $SSHI ec2-user@$SERVER "sudo stop prismapi && sudo start prismapi"
  echo ""
  echo ""
  echo "Verifying service is running"
  sleep 5
  echo ""
  echo ""
  curl -i --insecure -L -X GET "https://$SERVER"
}

environment() {
  if [ $ENV == "dev" ] || [ $ENV == "DEV" ]
    then
      echo "Deploying to Development on https://$DEV"
      echo ""
      SERVER=$DEV
  
  elif [ $ENV == "staging" ] || [ $ENV == "STAGING" ] || [ $ENV == "STAG" ] || [ $ENV == "stag" ]
    then
      echo "Deploying to Staging on https://$STAG"
      echo ""
      SERVER=$STAG
  
  elif [ $ENV == "test" ] || [ $ENV == "TEST" ]
    then
      echo "Deploying to Test on https://$TEST"
      echo ""
      SERVER=$TEST
  fi
}

rollback() {
  environment
  echo ""
  echo "Rolling Back last deployment on $SERVER"
  ssh -i $SSHI ec2-user@$SERVER "cd /var/www && sudo mv ./prism_api_rollback ./prism_api"
  post_deploy
}


create_rollback() {
  ssh -i $SSHI ec2-user@$SERVER "cd /var/www && sudo cp -r ./prism_api/* ./prism_api_rollback"
}

echo ""
echo "--------------------"
echo "Prizm API Deployment"
echo "--------------------"
echo "[`date -u +%Y-%m-%dT%T.%3NZ`]"
echo ""

if [  ${#ENV} == 0 ]
then
  echo "A deployment environment must be supplied"
  exit 0
fi

if [ ${#ROLLBACK} == 0 ]
then
  deploy
else
  if [ $ROLLBACK == "rollback" ]
  then
    rollback
  else
    echo "$ROLLBACK is not a supported command"
    exit 0
  fi
fi

echo ""
echo ""
echo "-----------------------"
echo "End of Prizm Deployment"
echo "-----------------------"

exit 0
