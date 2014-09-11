module Network.XmlPush.Tls.Client (
	TlsArgs(..)
	) where

import Data.X509
import Data.X509.CertificateStore
import Network.PeyoTLS.Client

data TlsArgs = TlsArgs {
	cipherSuites :: [CipherSuite],
	certificateAuthority :: CertificateStore,
	keyChain :: [(CertSecretKey, CertificateChain)]
	}