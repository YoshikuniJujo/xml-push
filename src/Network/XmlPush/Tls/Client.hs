module Network.XmlPush.Tls.Client (
	TlsArgs(..)
	) where

import Data.X509
import Data.X509.CertificateStore
import Network.PeyoTLS.Client
import Text.XML.Pipe

data TlsArgs = TlsArgs {
	serverName :: String,
	checkCertificate :: XmlNode -> Maybe (SignedCertificate -> Bool),
	cipherSuites :: [CipherSuite],
	certificateAuthorities :: CertificateStore,
	keyChains :: [(CertSecretKey, CertificateChain)]
	}
