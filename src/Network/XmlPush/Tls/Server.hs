module Network.XmlPush.Tls.Server (
	TlsArgs(..)
	) where

import Data.X509
import Data.X509.CertificateStore
import Network.PeyoTLS.Client
import Text.XML.Pipe

data TlsArgs = TlsArgs {
	getClientName :: XmlNode -> Maybe String,
	cipherSuites :: [CipherSuite],
	certificateAuthorities :: Maybe CertificateStore,
	keyChains :: [(CertSecretKey, CertificateChain)]
	}
