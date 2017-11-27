{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Library (Library, FromJSON, ToJSON) where

-- Its a good idea to separate each JSON type into a different module and then compose as otherwise you can hit the
-- restriction that you can't have two Haskell records in the same module with the same attribute name. This is because
-- accessor functions are created for the records and you'd end up with two functions with the same name. i.e. if you
-- had the JSON property 'id' in two locations in the doc.

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Book (Book)

-- To decode or encode a value using the generic machinery, we must
-- make the type an instance of the Generic class.
data Library = Library {
                    books :: [Book]
                  } deriving (Show, Generic)

-- While we still have to declare our type as instances of FromJSON
-- and ToJSON, we do *not* need to provide bodies for the instances.
-- Default versions will be supplied for us.

instance FromJSON Library
instance ToJSON Library

-- -- In the case that a property in the JSON schema has a attribute name incompatible with
-- -- Haskell attributes name (i.e. starts with an Upper-case character).
-- -- You can supply options to the generic JSON parsing and writing, one of which is a function to
-- -- apply to the property name, i.e. fromJsonFields below indicates that if you are populating
-- -- 'haskellLowerCaseAttribute' in the record definition then use 'JSONUpperCaseCharacter' from the JSON document.
-- -- The inverse is specified for writing.

-- fromJsonFields :: String -> String
-- fromJsonFields s
--         | s == "haskellLowerCaseAttribute" = "JSONUpperCaseCharacter"
--         | otherwise = s
--
-- -- JSON schema has fields which are not valid in Haskell, need to lower the case
-- instance FromJSON Library where
--   parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = fromJsonFields }
--
-- toJsonFields :: String -> String
-- toJsonFields s
--         | s == "JSONUpperCaseCharacter" = "haskellLowerCaseAttribute"
--         | otherwise = s
--
-- instance ToJSON Library where
--   toJSON = genericToJSON defaultOptions { fieldLabelModifier = toJsonFields }
--   toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = toJsonFields }