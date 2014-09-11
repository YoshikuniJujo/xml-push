module Network.XmlPush.Tls.Server (
	TlsArgs(..)
	) where

import Data.X509
import Data.X509.CertificateStore
import Network.PeyoTLS.Client

data TlsArgs = TlsArgs {
	cipherSuites :: [CipherSuite],
	certificateAuthority :: Maybe CertificateStore,
	keyChain :: [(CertSecretKey, CertificateChain)]
	}
