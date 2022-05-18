#! /usr/bin/env bash

# Issue some variables as usual!
HERE=`dirname "$(readlink -f "$BASH_SOURCE")"`
APP_PATH=$(readlink -f $HERE/..)
REVISION=1
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

TODAY=`date +"%d %b %Y"`

echo "\
.\" Manpage for ${APP_NAME}.
.\" Contact ${APP_MAINTAINER} to correct errors or typos.
.TH man 1 "${TODAY}" "${APP_VERSION}" "${APP_NAME} man page"
.SH NAME
${APP_NAME} \- ${APP_DESCRIPTION}
.SH SYNOPSIS
$(lbl --help | head -n 4 | tail -n 2 | lbl replace "'Usage: '" '' :: replace "'       '" '') 
.SH DESCRIPTION
${APP_NAME} is high level shell program for adding users to LDAP server.  On Debian, administrators should usually use nuseradd.debian(8) instead.
.SH OPTIONS
The ${APP_NAME} does not take any options. However, you can supply username.
.SH SEE ALSO
useradd(8), passwd(5), ${APP_NAME}.debian(8)
.SH BUGS
No known bugs.
.SH AUTHOR
${APP_AUTHOR} (${APP_MAINTAINER})
" > $HERE/linebyline
