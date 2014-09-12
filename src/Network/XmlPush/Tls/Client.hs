module Network.XmlPush.Tls.Client (
	TlsArgs(..)
	) where

import Data.X509
import Data.X509.CertificateStore
import Network.PeyoTLS.Client

data TlsArgs = TlsArgs {
	serverName :: String,
	cipherSuites :: [CipherSuite],
	certificateAuthorities :: CertificateStore,
	keyChains :: [(CertSecretKey, CertificateChain)]
	}
