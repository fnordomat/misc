#!/bin/bash

# connect to CDWiFi, then:

# I believe this is the necessary and sufficient step to get internet access on board the Czech long distance trains (e.g. EuroCity which runs from Prague to Kiel)
curl -vLA '' 'http://cdwifi.cz/portal/api/vehicle/gateway/user/authenticate'

# It says "500 internal server error" but that doesn't keep it from working.

# check with this request, answer should contain "authenticated":1
curl -vLA '' 'http://cdwifi.cz/portal/api/vehicle/gateway/user'

# Also nice: GPS data (but seems outdated)
curl -vLA '' 'http://cdwifi.cz/portal/internal/api/x6/position'

# Really nice: show connectivity including the LTE uplinks
curl -vLA '' 'http://www.info.cdwifi.cz/api/jsonp/connectivity'

