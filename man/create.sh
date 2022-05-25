#! /usr/bin/env bash

# Issue some variables as usual!
HERE=`dirname "$(readlink -f "$BASH_SOURCE")"`
APP_PATH=$(readlink -f $HERE/..)
MDFILE="${HERE}/linebyline.md"
LBL="${HERE}/lbl"

# Build a very own linebyline to use inside this script
stack install --local-bin-path $HERE $APP_PATH


# Grab the application name and version from ../package.yaml
APP_VERSION=$(cat $APP_PATH/package.yaml | $LBL grep version :: split :: :1)
APP_NAME=$(cat $APP_PATH/package.yaml | $LBL grep name :: split :: :1)
APP_AUTHOR=$(cat $APP_PATH/package.yaml | $LBL \
  grep author :: split :: :1~ :: split '"')
APP_MAINTAINER=$(cat $APP_PATH/package.yaml | $LBL \
  grep maintainer :: split :: :1~ :: split '"')
APP_DESCRIPTION=$(cat $APP_PATH/package.yaml | $LBL \
  grep '^description' :: split :: :1~ :: split '"')
DEB_NAME="${APP_NAME}_${APP_VERSION}-${REVISION}-${ARCH}"


SYN=$($LBL --help | tail -n+3 | head -n2 | sed 's/^Usage: //g' | sed 's/^       //g')
DESC=$($LBL --help | tail -n+7)

echo "# ${APP_NAME}

## Name

**${APP_NAME}** - ${APP_DESCRIPTION} 

## Synopsis

${SYN}

## Options

${DESC}

" > $MDFILE


cat README.md >> $MDFILE

md2man $MDFILE \
  -o "${APP_NAME}.1" \
  --author "$APP_AUTHOR" \
  --email "$APP_MAINTAINER" \
  --revision "$APP_VERSION" \
  --section 1 \
  --book "${APP_NAME} manual page"
