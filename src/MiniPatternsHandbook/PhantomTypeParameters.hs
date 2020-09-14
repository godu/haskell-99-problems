module MiniPatternsHandbook.PhantomTypeParameters where

import Data.Binary (Binary)
import Data.ByteString (ByteString)

newtype Key a = Key ByteString

newtype PrivateKey = PrivateKey ByteString

newtype PublicKey = PublicKey ByteString

newtype Signature a = Signature ByteString

-- | Derive public key from secret key.
createPublicKey :: PrivateKey -> PublicKey
createPublicKey = error "Not implemented"

-- | Sign the data using the given 'PrivateKey'.
sign :: Binary a => PrivateKey -> a -> Signature a
sign = error "Not implemented"

-- | Check that the signature is produced by the 'PublicKey', derived for the
-- corresponding 'PrivateKey' that signed the same type of data
verifySignature :: Binary a => PublicKey -> Signature a -> a -> Bool
verifySignature = error "Not implemented"
