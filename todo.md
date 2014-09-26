TODO
====

Unify HTTP PULL/PUSH server
---------------------------

### TLS

#### done

* HTTP PULL server
	+ [x] move code to Body.hs
	+ [x] add (pre :: [XmlNode]) argument to makeHttpPull
	+ [x] process pre argument (push to TChan)
	+ [x] make response to pre
	+ [x] make data type for test
	+ [x] make example as test code

#### todo

* HTTP PUSH server
	+ [x] move code to Body.hs
	+ [x] add (pre :: [XmlNode]) argument to makeHttpPull
	+ [x] process pre argument (push to TChan)
	+ [x] make response to pre
	+ [x] make data type for test
	+ [x] make example as test code
* PULL/PUSH server
	+ [x] make module Network.XmlPush.Tls.Server
	+ [x] make data type
	+ [x] make it instance of XmlPush
	+ [ ] make example code
* process TLS original problems
