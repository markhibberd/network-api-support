module Network.Api.Support.Security (
  checkDomainOnly
) where

import qualified Data.ByteString.Char8 as B8
import Data.Certificate.X509 (X509)

import Network.TLS (TLSCertificateUsage)
import Network.TLS.Extra (certificateVerifyDomain)
import Data.CertificateStore (CertificateStore)

-- | A TLS validator that checks the domain only. Note that this means the validator
-- | will not check the cert chain, and can be used on systems where Data.Certificate.X509
-- | falls over as it does not have access to local root certs.
-- |
-- | ! Use with caution !
checkDomainOnly :: CertificateStore -> B8.ByteString -> [X509] -> IO TLSCertificateUsage
checkDomainOnly _ host' certs = return $ certificateVerifyDomain (B8.unpack host') certs
