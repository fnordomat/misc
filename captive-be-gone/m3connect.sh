#!/bin/bash

curl --stderr - --trace - -v -L -A '' http://google.com | tee step0001

curl --stderr - --trace - -v -L -A '' -c kookie.jar https://portal.m3connect.de/en/login/free | tee step0002

cat kookie.jar

PHPSESSID=$(grep PHPSESSID kookie.jar | perl -ne '/([A-Za-z0-9]*)$/; print $1;')

curl --stderr - --trace - --post301 --post302 --post303 -c kookie.jar -v -L -A 'Mozilla/5.0' -e 'https://portal.m3connect.de/en/login/free' -b "PHPSESSID=${PHPSESSID}" -F submit=Register -F "registration[tariff]=707" -F "registration[terms]=1" https://portal.m3connect.de/en/register/free | tee step0003
