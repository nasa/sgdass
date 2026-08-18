#!/bin/csh -f
cd /d2/gtopo30
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/antarcps.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/e020n40.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/e020n90.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/e020s10.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/e060n40.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/e060n90.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/e060s10.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/e060s60.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/e100n40.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/e100n90.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/e100s10.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/e120s60.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/e140n40.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/e140n90.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/e140s10.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w000s60.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w020n90.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w020s10.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w060n40.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w060n90.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w060s10.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w060s60.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w100n40.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w100n90.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w100s10.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w120s60.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w140n40.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w140n90.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w140s10.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w180n40.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w180n90.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w180s10.tar.gz
wget ftp://edcftp.cr.usgs.gov/data/gtopo30/global/w180s60.tar.gz
#
tar -xzvf antarcps.tar.gz
tar -xzvf e020n40.tar.gz
tar -xzvf e020n90.tar.gz
tar -xzvf e020s10.tar.gz
tar -xzvf e060n40.tar.gz
tar -xzvf e060n90.tar.gz
tar -xzvf e060s10.tar.gz
tar -xzvf e060s60.tar.gz
tar -xzvf e100n40.tar.gz
tar -xzvf e100n90.tar.gz
tar -xzvf e100s10.tar.gz
tar -xzvf e120s60.tar.gz
tar -xzvf e140n40.tar.gz
tar -xzvf e140n90.tar.gz
tar -xzvf e140s10.tar.gz
tar -xzvf w000s60.tar.gz
tar -xzvf w020n90.tar.gz
tar -xzvf w020s10.tar.gz
tar -xzvf w060n40.tar.gz
tar -xzvf w060n90.tar.gz
tar -xzvf w060s10.tar.gz
tar -xzvf w060s60.tar.gz
tar -xzvf w100n40.tar.gz
tar -xzvf w100n90.tar.gz
tar -xzvf w100s10.tar.gz
tar -xzvf w120s60.tar.gz
tar -xzvf w140n40.tar.gz
tar -xzvf w140n90.tar.gz
tar -xzvf w140s10.tar.gz
tar -xzvf w180n40.tar.gz
tar -xzvf w180n90.tar.gz
tar -xzvf w180s10.tar.gz
tar -xzvf w180s60.tar.gz
#
rm *.SCH *.STX *.DMW *.PRJ *.GIF *.SRC
