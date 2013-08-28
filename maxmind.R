# Rmaxmind
# 
# (well, not yet...but it will be a package soon)
# for now it's just maxmind.R
#
# Copyright (c) 2013 Bob Rudis (@hrbrmstr) bob@rudis.net
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

library(data.table)
library(bitops)
library(bit64)
library(rredis)
library(foreach)
library(iterators)

# This product includes GeoLite data created by MaxMind, available from
# http://dev.maxmind.com/geoip/legacy/geolite/
 
# It uses (and auto-downloads):
# * http://geolite.maxmind.com/download/geoip/database/GeoIPCountryCSV.zip
# * http://download.maxmind.com/download/geoip/database/asnum/GeoIPASNum2.zip
# * http://geolite.maxmind.com/download/geoip/database/GeoLiteCity_CSV/GeoLiteCity-latest.zip

# You need to issue a redisConnect() before using most of these functions
# if you expect "redis speed" on subsequent queries. you can set
# the parameter "use.redis" to FALSE if not using redis
# it's hard *not* to use redis tho with the speedup it provides and the fact that
# it's just an "apt-get install redis" on debian-ish systems

# most of these functions also use data.table since it's *alot* faster

# convert IP string to long int for use in searching maxmind dbs
ip2long <- function(ip) {
  # convert string into vector of characters
  parts <- unlist(strsplit(ip, '.', fixed=TRUE))
  # set up a function to bit-shift, then "OR" the octets
  octets <- function(x,y) bitOr(bitShiftL(x, 8), y)
  # Reduce applys a funcution cumulatively left to right
  Reduce(octets, as.integer(parts))
}

# returns a character vector with country code
# by default it wants to use redis
cc.geo.ip <- function(ip, use.redis=TRUE) {
  myip <- ip2long(ip)
  if (use.redis) {
    result <- redisGet(sprintf("cc:ip:%s",myip))
    if (is.null(result)) {
      # all the magic happens here
      result <- as.character(last(geo.cc.dt[geo.cc.dt$sipn<=myip & 
                                              geo.cc.dt$eipn>=myip,]$CC))
      redisSet(sprintf("cc:ip:%s",myip), result)
    }  
  } else {
    result <- as.character(last(geo.cc.dt[geo.cc.dt$sipn<=myip & 
                                            geo.cc.dt$eipn>=myip,]$CC))  
  }
  return(result)
}

# returns a one row data frame of IP location info
# # by default it wants to use redis
city.geo.ip <- function(ip, use.redis=TRUE) {
  myip <- ip2long(ip)
  if (use.redis) {
    result <- redisGet(sprintf("city:ip:%s",myip))
    if (is.null(result)) {
      # all the magic happens here
      loc <- last(geo.blocks.dt[geo.blocks.dt$startIpNum<=myip & 
                                  geo.blocks.dt$endIpNum>=myip,]$locId)
      result <- geo.location.dt[as.integer(loc)]      
      redisSet(sprintf("city:ip:%s",myip), result)
    }  
  } else {
    loc <- last(geo.blocks.dt[geo.blocks.dt$startIpNum<=myip & 
                                geo.blocks.dt$endIpNum>=myip,]$locId)
    result <- geo.location.dt[as.integer(loc)]      
  }
  return(result)
}


# returns a character vector with the ASN # and name
# by default it wants to use redis
asn.ip <- function(ip, use.redis=TRUE) {
  myip <- ip2long(ip)
  if (use.redis) {
    result <- redisGet(sprintf("asn:ip:%s",myip))
    if (is.null(result)) {
      # all the magic happens here
      result <- as.character(last(asn.dt[(asn.dt$sipn<=myip & 
                                            asn.dt$eipn>=myip),]$asnInfo))
      redisSet(sprintf("asn:ip:%s",myip), result)
    }  
  } else {
    result <- as.character(last(asn.dt[(asn.dt$sipn<=myip & 
                                          asn.dt$eipn>=myip),]$asnInfo))
  }
  return(result)
}

# initializes the maxmind asn lookup data table
# NOTE: sets global variable asn.dt
maxmind.asn <- function(refresh=FALSE) {
  
  message("This product includes GeoLite data created by MaxMind, available from")
  message("http://dev.maxmind.com/geoip/legacy/geolite/")
  
  mm.asn.url <- "http://download.maxmind.com/download/geoip/database/asnum/GeoIPASNum2.zip"
  mm.dir <- file.path(path.expand("~"), ".maxmind")
  mm.asn.data.file <- file.path(mm.dir,"GeoIPASNum2.zip")

  dir.create(mm.dir, showWarnings=FALSE)
  
  if (refresh || file.access(mm.asn.data.file)) {
    download.file(mm.asn.url,mm.asn.data.file)
  }
  
  zip.paths = unzip(mm.asn.data.file, exdir=mm.dir)
  asn.source = zip.paths[1]
  
  # sets global variable
  asn.dt <<- fread(asn.source, sep=",", header=FALSE)
  setnames(asn.dt, colnames(asn.dt), c("sipn", "eipn", "asnInfo"))
  
  # makes lookups pretty fast even w/o redis
  setkey(asn.dt, sipn)
}

# initializes the maxmind country lookup data table
# NOTE: only returns country codes at the moment
# NOTE: Sets global variable "geo.cc.dt"
maxmind.cc <- function(refresh=FALSE) {

  message("This product includes GeoLite data created by MaxMind, available from")
  message("http://dev.maxmind.com/geoip/legacy/geolite/")
  
  mm.cc.url <- "http://geolite.maxmind.com/download/geoip/database/GeoIPCountryCSV.zip"
  mm.dir <- file.path(path.expand("~"), ".maxmind")
  mm.cc.data.file <- file.path(mm.dir,"GeoIPCountryCSV.zip")

  dir.create(mm.dir, showWarnings=FALSE)

  if (refresh || file.access(mm.cc.data.file)) {
    download.file(mm.cc.url,mm.cc.data.file)
  }  
  
  zip.paths <- unzip(mm.cc.data.file, exdir=mm.dir)
  cc.source <- zip.paths[1]

  # sets global variable
  geo.cc.dt <<- fread(cc.source, sep=",",header=FALSE)
  setnames(geo.cc.dt, colnames(geo.cc.dt),
           c("sipa", "eipa", "sipn", "eipn", "CC", "Name"))

  # makes lookups pretty fast even w/o redis
  setkey(geo.cc.dt,sipn)
}

# initializes the maxmind city lookup data tables
# NOTE: Sets global variables geo.blocks.dt & geo.location.dt
maxmind.city <- function(refresh=FALSE) {
  
  message("This product includes GeoLite data created by MaxMind, available from")
  message("http://dev.maxmind.com/geoip/legacy/geolite/")
  
  mm.city.url <- "http://geolite.maxmind.com/download/geoip/database/GeoLiteCity_CSV/GeoLiteCity-latest.zip"
  mm.dir <-file.path(path.expand("~"), ".maxmind")
  mm.city.data.file <-  file.path(mm.dir,"GeoLiteCity-latest.zip")
  
  dir.create(mm.dir, showWarnings=FALSE)
  
  if (refresh || file.access(mm.city.data.file)) {
    download.file(mm.city.url,mm.city.data.file)
  }
  
  zip.paths <- unzip(mm.city.data.file, exdir=mm.dir)
  mm.blockSource <- zip.paths[1]
  mm.locationSource <- zip.paths[2]
  
  # Remove Maxmind copyright statement so it doesn't bork fread()
  system(sprintf("sed -i'' -e 1,1d %s",mm.blockSource))
  system(sprintf("sed -i'' -e 1,1d %s",mm.locationSource))
  
  geo.blocks.dt <<- fread(mm.blockSource, sep=",",header=TRUE)
  geo.location.dt <<- fread(mm.blockSource, sep=",",header=TRUE)
  
  # make it even speedier
  setkey(geo.blocks.dt,startIpNum)
  setkey(geo.location.dt,locId)
}

################################################################
# TEST
################################################################

# using alienvault data soley for testing since it has location
# data coded done for us and we can validate our findings
# sets global variable av.dt
av.reputation.dt <- function(refresh=FALSE) {
  
  message("IP Reputation Database Courtesy of the fine folks at AlienVault")
  message("http://labs.alienvault.com/labs/index.php/projects/open-source-ip-reputation-portal/")
  
  av.url <- "http://reputation.alienvault.com/reputation.data"
  av.dir <-file.path(path.expand("~"), ".alienvault")
  av.data.file <-  file.path(av.dir,"reputation.data")
  
  dir.create(av.dir, showWarnings=FALSE)
  
  if (refresh || file.access(av.data.file)) {
    download.file(av.url,av.data.file)
  }
  
  # sets global variable
  av.dt <<- fread(av.data.file, sep="#", header=FALSE)
  setnames(av.dt, colnames(av.dt),
           c("IP", "Reliability", "Risk", "Type", 
             "Country", "Locale", "Coords", "x"))
  
  # makes lookups pretty fast
  setkey(av.dt,IP)
  
}

# start tests/example usage

# load in (downloading, if necessary) the maxmind data
maxmind.asn()
maxmind.cc()
maxmind.city()

# gotta do this for functions to work properly or pass in 
# FALSE to use.redis in asn.ip(), cc.geo.ip() and city.geo.ip()
redisConnect() 

av.reputation.dt()

set.seed(1492)
# random test
for (i in 1:20) {
  ip <- as.character(av.dt[sample(1:nrow(av.dt),1),]$IP)
  print(ip)
  print(asn.ip(ip))
  print(cc.geo.ip(ip))
  print(city.geo.ip(ip))
  print(av.dt[ip,])
  print("==========================")
}

asn.ip("5.135.240.45")
cc.geo.ip("5.135.240.45")

c1 <- city.geo.ip("5.135.240.45")
c2 <- city.geo.ip("147.57.179.249")

# here's how to process a list of IP addresses and get back a data frame
# in the format of "IP","CC". Run it more than once with redis installed to 
# see the power of redis :-)
q <- data.frame(matrix(sapply(av.dt$IP[1:1000], function(x) { c(x, cc.geo.ip(x)) }),ncol=2,byrow=TRUE))
colnames(q) <- c("IP","CC")
