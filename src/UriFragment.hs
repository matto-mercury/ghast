module UriFragment where

import Data.Text (Text (..), intercalate, pack)
import qualified Network.URI.Encode as UE

data Parameter = Param Text Text
  deriving stock (Show, Eq)

class UriFragment s where
  render :: s -> Text

instance UriFragment Text where
  render = UE.encodeText

instance UriFragment Parameter where
  render (Param k v) = render k <> pack "=" <> render v

instance UriFragment [Parameter] where
  render [] = pack ""
  render ps = pack "?" <> pstr
    where pstr = intercalate "&" (map render ps)

