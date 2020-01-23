#!/bin/bash
# Can't assign requested address (code=49) is a bug in Mac OS X
# https://www.ovpn.com/en/faq/troubleshooting/mac-cant-assign-requested-address-code49
interface=en0
echo "[INFO] Cleaning the routes for <$interface>"
sudo ifconfig $interface down
sleep 1
sudo route flush
sleep 1
sudo ifconfig $interface up
exit 0