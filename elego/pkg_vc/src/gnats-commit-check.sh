#!/bin/sh

# An appropriate entry in compactrc could look like this:
#commit-check            "gnats-commit-check.sh" # add path if necessary
#external-commit-hook    "!{!commit-check} -id '{?id}' -user '{?user}' -name '{?name}' -pkgname '{?pkgname}' -action '{?action}'"

QUERYPR=${QUERYPR:-nquery-pr} # add path if necessary
ID=''
NAME=''
USER=''
PKGNAME=''
ACTION='' # project-change-set project-release project-snapshot
          # package-commit package-release
QUERYPROPTS=''

if [ -z "$TMPDIR" ] ; then
  if [ -z "$TMP" ] ; then
    if [ -d /tmp ] ; then
      TMPDIR=/tmp
      TMP=/tmp
    elif [ -d /var/tmp ] ; then
      TMPDIR=/var/tmp
      TMP=/var/tmp
    elif [ -d /usr/tmp ] ; then
      TMPDIR=/usr/tmp
      TMP=/usr/tmp
    else 
      TMPDIR=.
      TMP=.
    fi
  else
    TMPDIR=$TMP
  fi
else
  TMP=$TMPDIR
fi

while [ -n "$1" ] ; do
  case $1 in
    -id)       ID="$2"; shift; shift;;
    -name)     NAME="$2"; shift; shift;;
    -user)     USER="$2"; shift; shift;;
    -pkgname)  PKGNAME="$2"; shift; shift;;
    -action)   ACTION="$2"; shift; shift;;
    *)         echo "unknown parameter: ${1}" 1>&2
  esac
done

TMPPR="${TMP}/PR${ID}.$$"

if [ -n "${GNATSSERVER}" ] ; then
  QUERYPROPTS="${QUERYPROPTS} -H ${GNATSSERVER}"
fi
if [ -n "${COMPACTSERVER}" ] ; then
  QUERYPROPTS="${QUERYPROPTS} -H ${COMPACTSERVER}"
fi

# Define your policy concerning different actions below. Default is to
# allow release and snapshot creations without a PR. It could also be 
# reasonable to allow unchecked change sets and package commits.

case "x${ACTION}" in
  xproject-change-set) ;;
  xproject-release) echo "release creation allowed"; exit 0;;
  xproject-snapshot) echo "release creation allowed"; exit 0;;
  xpackage-commit) ;;
  xpackage-release) ;;
  *) echo "internal error: unknown action $ACTION" 1>&2; exit 1;;
esac

if [ -z "$ID" ] ; then
  echo "There seems to be no request id in this log message. Please supply one." 1>&2
  exit 1
fi

${QUERYPR} ${QUERYPROPTS} ${ID} > ${TMPPR}

if [ "$?" != "0" ] ; then
  echo "There seems to be no request with id ${ID}. Please check." 1>&2
  rm -f ${TMPPR}
  exit 1
fi

STATE=$(cat ${TMPPR} | grep '^>State:' | awk '{print $2}')
SYNOPSIS=$(cat ${TMPPR} | grep '^>Synopsis:' | sed -e 's/>Synopsis: *//')

if [ "$STATE" = "closed" ] ; then
  echo "Request id ${ID} seems to be already closed. Please check." 1>&2
  rm -f ${TMPPR}
  exit 1
fi

if [ "$STATE" = "suspended" ] ; then
  echo "Request id ${ID} seems to be suspended. Please check." 1>&2
  rm -f ${TMPPR}
  exit 1
fi

echo "Your ${ACTION} seems to relate to PR ${ID} in state '${STATE}' with the synopsis"
echo "'${SYNOPSIS}'."
echo "Please don't forget to initiate the appropriate state change."

rm -f ${TMPPR}
exit 0
