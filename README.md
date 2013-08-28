Rmaxmind
========

Pure R implementation for reading and processing (legacy) MaxMind IP Geolocation data 


While not a full-on R package, the routines in maxmind.R will enable the user to

- download the MaxMind CSV data sets (so, the _legacy_ data format)
- perform ASN, CC & City queries
- optionally cache results (provided you are using redis) IP geolocation

I'll _eventually_ convert this to a package once the book : http://amzn.to/sudabook : that @jayjacobs & I 
are writing comes out :-)

Please direct all inquiries via github or to @hrbrmstr

Simplest way to use this is to clone the repository, read the examples (everyting after "TEST") then blow away
the everything past said "TEST" comment and start using the code.

It requires:

- <code>library(data.table)</code>
- <code>library(bitops)</code>
- <code>library(bit64)</code>
- <code>library(rredis)</code>
- <code>library(foreach)</code>
- <code>library(iterators)</code>

Basic usage is to initialize the library and a connection to redis:

    maxmind.asn()
    maxmind.cc()
    maxmind.city()
    redisConnect()
    
and then geocode all the things:

    print(asn.ip("5.135.240.45"))
    print(cc.ip("5.135.240.45"))
    print(city.geo.ip("5.135.240.45"))
    
NOTE that you can geocode a whole list of IPs pretty easily:

    q <- data.frame(matrix(sapply(list.of.ip.addresses, function(x) {
      c(x, cc.geo.ip(x))
    }),ncol=2,byrow=TRUE))
    colnames(q) <- c("IP","CC")
    
It relies on redis for wicked fast speed (well, after the initial cache miss and fill).
If you don't want to use redis, pass "FALSE" to the maxmind.XXX init routines and omit 
the call to redisConnect().

Given that MaxMind updates their free files each month, it might be a good idea to invalidate
the redis cache and re-build it on that same frequency.
