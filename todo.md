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
	+ [x] make example code
* process TLS original problems

#### todo

* simplify argument
	+ [ ] remove duplicate of TlsArgs for server
		HttpTlsServerArgs (XmlNode -> Mechanism)
			(HttpPullSvArgs h) (HttpPushArgs h)
			Sv.TlsArgs Cl.TlsArgs
