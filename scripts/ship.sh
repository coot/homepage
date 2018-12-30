#!env /bin/bash

tar -caf ./dist.tar.gz --exclude "*.un~" --exclude ".*.sw[pomnrst]" ./dist
scp dist.tar.gz root@coot.me:/var/www/homepage
ssh root@coot.me "tar -taf /var/www/homepage/dist.tar.gz > /dev/null"
ssh root@coot.me "rm /var/www/homepage/dist.tar.gz"
rm dist.tar.gz
