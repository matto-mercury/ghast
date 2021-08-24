module UriFragment where

import Data.List (intercalate)
import qualified Network.URI.Encode as UE

data Parameter = Param String String
  deriving stock (Show, Eq)

class UriFragment s where
  render :: s -> String

instance UriFragment String where
  -- probably want some URI-encoding here eventually
  render = UE.encode

instance UriFragment Parameter where
  render (Param k v) = render k <> "=" <> render v

instance UriFragment [Parameter] where
  render [] = ""
  render ps = "?" <> pstr
    where pstr = intercalate "&" (map render ps)

