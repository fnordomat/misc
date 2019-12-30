
#!/bin/bash

# Deutsche Bahn / icomera AB offers "free wifi" (ESSID: "WIFIonICE") with internet access via
# mobile networks on some high-speed (ICE) trains.
#
# This takes care of the WIFIonICE captive portal.
#
# Please drop me a note if this script is insecure for some reason. It probably is.
#

DUMMY=http://google.com
LOGIN=http://wifionice.de/de

DOC=$(mktemp)
HDR=$(mktemp)

LOG=$(mktemp /tmp/konnektlog.XXXXXXXXXXX)

# set if you like
UA=''

curl -vsLA "${UA}" -D "${HDR}" -o "${DOC}" "${DUMMY}" --stderr - >> ${LOG}

echo "Headers: ${HDR}"
CSRFToken=$(cat "${DOC}" | perl  -ne '/CSRFToken\" value=\"([a-f0-9]+)\"/ && print $1;')
COOKIE=$(cat "${HDR}" | perl -ne '/^Set-Cookie: ((?:PHPSESSID)|(?:csrf)=[0-9a-z]+;)/ && print "$1";')

echo "CSRFToken: $CSRFToken"
echo "Cookie:    $COOKIE"

REFERER=http://wifionice.de

curl -vLA "${UA}" --post301 --post302 --post303 -e "${REFERER}" "${LOGIN}" -F login=true --form-string CSRFToken="${CSRFToken}" -b "${COOKIE}" --stderr - >> "${LOG}"

echo "Log at ${LOG}"
