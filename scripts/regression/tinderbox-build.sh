#!/bin/sh 
# tinderbox-build.sh 

usage () {
  echo "usage: tinderbox-build.sh <build-config>"
}

trap cleanup 1 2 3 6

if [ -z "$1" ]
then
  usage
  exit 1
fi

#source build config
. "$1"
 
if [ -z "${PROJECT}" -o -z "${TREENAME}" ] 
then 
  echo "Parameters missing, see included README for documentation." 
  exit 1 
fi 

# set default parameters:
UNAME_N=`uname -n`
UNAME_S=`uname -s`
UNAME_R=`uname -r | sed -e 's/[^A-Za-z0-9_]/./g'`
UNAME_M=`uname -m`
BUILDNAME=${BUILDNAME:-"${UNAME_N}-${UNAME_S}=${UNAME_R}-${UNAME_M}"}

echo "Building ${PROJECT}."
echo "Tinderbox Tree:   \"${TREENAME}\""
echo "Buildname:        \"${BUILDNAME}\""
echo ""

NAME="build-${PROJECT}-`date '+%Y%m%d-%H%M%S'`" 

tinderbox_header () {
  TREE_NAME="$1"
  BUILD_NAME="$2"
  STATUS="$3"
  STARTTIME="$4"

  if [ -z "${TREE_NAME}" -o -z "${BUILD_NAME}" -o -z "${STATUS}" -o -z "${STARTTIME}" ]
  then
    echo ""
    echo "example:"
    echo "   $0 firefox debian-sarge-i386-gcc-4.1 building 1199624301"
    echo ""
    echo "usage:"
    echo "   $0 <Tinderbox-tree-name> <build-name> <status> <starttime>"
    echo "where"
    echo "   <Tinderbox-tree-name>   a tree-name as defined in the"
    echo "                           tinderbox-configuration"
    echo "   <build-name>            will be the row-title on the"
    echo "                           tinderbox build status page"
    echo "   <status>                one of "building", "build_failed","
    echo "                           "test_failed" or "success""
    echo "   <starttime>             tinderbox-build-number in unix time format,"
    echo "                           get it with:"
    echo "                           date 'date +%s'"
    echo "                           once for each build"
    echo ""
    echo "This script outputs the mail header that can be prepended"
    echo "to the build log when sending the status mail."
    echo ""
    echo "Tinderbox identifies builds by starttime, so if you want to send"
    echo "multiple status messages for the same build, be sure to always"
    echo "use the same starttime"
    echo ""
    echo "ATTENTION: This script can not check validity of parameters."
    echo "           Wrong tree-name or status will make the tinderbox"
    echo "           ignore the status-mail."
    return 1
  fi

  echo ""
  echo tinderbox: tree: $TREE_NAME
  echo tinderbox: starttime: $STARTTIME
  echo tinderbox: timenow: `date -u '+%m/%d/%y %H:%M:%S'`
  echo tinderbox: status: $STATUS
  echo tinderbox: buildname: $BUILD_NAME
  echo tinderbox: errorparser: unix
  echo tinderbox: END
  echo ""
}

mail_buildlog() {
  if [ -z "$1" ]
  then
    return 1
  fi

  TMP_LOG=${BUILDDIR}/mail_temp

  if [ -z "${TMP_LOG}" ]
  then
    echo "Error: Cannot create temp file for mailer."
    cleanup
    exit 4
  fi

  {
    tinderbox_header "${TREENAME}" "${BUILDNAME}" "$1" "${STARTTIME}"
    cat ${LOG}
  }  > "$TMP_LOG"
  
  tinderbox_mailer "${TMP_LOG}"

  if [ $? != 0 ]; then
      echo "Error: Sending buildlog failed!"
      cleanup
      exit 4
  fi

  rm -f "${TMP_LOG}"
}

cleanup() {
  echo "removing build tree ${BUILDDIR_BASE} ..." 
  cd ${BUILDDIR_ROOT}
  rm -rf ${BUILDDIR_BASE} 

  # call build script cleanup
  do_cleanup
}

STARTTIME=`date -u '+%m/%d/%y %H:%M:%S'`

#echo "${NAME}" 
 
# create repository 
BUILDDIR_ROOT="/tmp"
BUILDDIR_BASE="`mktemp -d ${BUILDDIR_ROOT}/${NAME}-XXXXXX`"
BUILDDIR="${BUILDDIR_BASE}/build" 

LOG="${BUILDDIR_BASE}/log.txt"
T="${BUILDDIR_BASE}/tmp-$$"
 
#check builddir
#echo "creating temporary build directory ${BUILDDIR}" 
#if [ -d ${BUILDDIR} ] 
#then 
#    echo "$0: ERROR: Directory already exists: ${BUILDDIR}" 
#    exit 2
#fi 

mkdir ${BUILDDIR} 
 
if [ ! -d ${BUILDDIR} ] 
then 
    echo "$0: ERROR: Cannot create directory: ${BUILDDIR}" 
    exit 3
fi 

# check logfile

echo "creating log file ${LOG}"
touch ${LOG}

if [ ! -w ${LOG} ]
then
  echo "$0: ERROR: Cannot write to ${LOG}"
  cleanup
  exit 5
fi

# starting build
{
  echo "" 
  echo "---" 
  echo "" 
  echo "checkout, compile and test of ${PROJECT} ..." 
  echo "`date '+%Y.%m.%d %H:%M:%S'` -- checkout in progress." 
} 2>&1 | tee -a ${LOG}

mail_buildlog "building"

{
    echo "[start checkout `date '+%Y.%m.%d %H:%M:%S'`]" 
    echo cd ${BUILDDIR} 
} 2>&1 | tee -a ${LOG}

cd ${BUILDDIR} 
do_checkout >"${T}" 2>&1
CHECKOUT_RETURN=$?
cat "${T}" >> ${LOG}

{
  echo cvs return value: ${CHECKOUT_RETURN} 
  echo "[end checkout `date '+%Y.%m.%d %H:%M:%S'`]" 
} | tee -a ${LOG} 2>&1

echo "CHECKOUT_RETURN = ${CHECKOUT_RETURN}"

if [ "${CHECKOUT_RETURN}" != 0 ]; then 
  echo "*** CHECKOUT FAILED" | tee -a ${LOG} 2>&1
  mail_buildlog "build_failed" 
  cleanup
  exit 1 
else
  mail_buildlog "building"
fi
 
{ 
    echo "--"
    echo "`date '+%Y.%m.%d %H:%M:%S'` -- compile in progress." 
 
    echo "[start compile `date '+%Y.%m.%d %H:%M:%S'`]" 
} 2>&1 | tee -a ${LOG}

do_compile >"${T}" 2>&1
COMPILE_RETURN=$? 
cat "${T}" >> ${LOG}

{ 
  echo "compile return value: ${COMPILE_RETURN}" 
  echo "[end compile `date '+%Y.%m.%d %H:%M:%S'`]" 
} 2>&1 | tee -a ${LOG}

echo "COMPILE_RETURN = ${COMPILE_RETURN}"

if [ "${COMPILE_RETURN}" != 0 ]; then 
  echo "*** COMPILE FAILED" | tee -a ${LOG} 2>&1
  mail_buildlog "build_failed" 
  cleanup
  exit 1 
else
  mail_buildlog "building"
fi

 
{ 
    echo "`date '+%Y.%m.%d %H:%M:%S'` -- tests in progress." 
    echo "[start run-tests `date '+%Y.%m.%d %H:%M:%S'`]" 
    echo cd "${BUILDDIR}" 
}  2>&1 | tee -a ${LOG}

cd "${BUILDDIR}" 
do_tests >"${T}" 2>&1
TESTS_RETURN=$?
cat "${T}" >> ${LOG}

{
  echo "[end run-tests `date '+%Y.%m.%d %H:%M:%S'`]" 
} 2>&1 | tee -a ${LOG}

echo "TESTS_RETURN = ${TESTS_RETURN}"

if [ "${TESTS_RETURN}" != 0 ]; then 
  echo "*** TESTS FAILED" | tee -a ${LOG} 2>&1
  mail_buildlog "test_failed" 
  cleanup
  exit 1 
fi

{
  echo "`date '+%Y.%m.%d %H:%M:%S'` -- checkout, compile and test run done." 
  echo "" 
  echo "---" 
  echo "" 
}  2>&1 | tee -a ${LOG}
    
mail_buildlog "success"

#cp -r "${BUILDDIR}/test/01/logs/now" "${LOGDIR}/01" 
#cp -r "${BUILDDIR}/test/03/logs/now" "${LOGDIR}/03" 

cleanup

echo "done." 

