TODO
====

Unify HTTP PULL/PUSH server
---------------------------

### TLS

#### done

#### todo

* HTTP PULL server
	+ [x] move code to Body.hs
	+ [x] add (pre :: [XmlNode]) argument to makeHttpPull
	+ [x] process pre argument (push to TChan)
	+ [x] make response to pre
	+ [x] make data type for test
	+ [ ] make example as test code
* HTTP PUSH server
	+ [ ] move code to Body.hs
	+ [ ] add (pre :: [XmlNode]) argument to makeHttpPull
	+ [ ] process pre argument (push to TChan)
	+ [ ] make response to pre
	+ [ ] make data type for test
	+ [ ] make example as test code
* PULL/PUSH server
	+ [ ] make module Network.XmlPush.Tls.Server
	+ [ ] make data type
	+ [ ] make it instance of XmlPush
* process TLS original problems
