TODO
====

Unify HTTP PULL/PUSH server
---------------------------

### TLS

#### done

#### todo

* HTTP PULL server
	+ [x] move code to Body.hs
	+ [ ] add (pre :: [XmlNode]) argument to makeHttpPull
	+ [ ] process pre argument (push to TChan)
	+ [ ] make response to pre
* HTTP PUSH server
	+ [ ] move code to Body.hs
	+ [ ] add (pre :: [XmlNode]) argument to makeHttpPull
	+ [ ] process pre argument (push to TChan)
	+ [ ] make response to pre
* PULL/PUSH server
	+ [ ] make module Network.XmlPush.Tls.Server
	+ [ ] make data type
	+ [ ] make it instance of XmlPush
* process TLS original problems
