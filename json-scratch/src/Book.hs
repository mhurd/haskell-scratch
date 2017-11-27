{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Book (Book, FromJSON, ToJSON) where

-- Its a good idea to separate each JSON type into a different module and then compose as otherwise you can hit the
-- restriction that you can't have two Haskell records in the same module with the same attribute name. This is because
-- accessor functions are created for the records and you'd end up with two functions with the same name. i.e. if you
-- had the JSON property 'id' in two locations in the doc.

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- To decode or encode a value using the generic machinery, we must
-- make the type an instance of the Generic class.
data Book = Book {
                    amazonPageUrl :: String,
                    asin :: String,
                    authors :: Maybe String,
                    binding :: Maybe String,
                    ean :: String,
                    edition :: Maybe String,
                    format :: Maybe String,
                    isbn :: String,
                    largeBookCover :: Maybe String,
                    largeImage :: Maybe String,
                    lastPriceUpdateTimestamp :: Maybe Integer,
                    listPrice :: Maybe Integer,
                    lowestNewPrice :: Maybe Integer,
                    lowestPrice :: Maybe Integer,
                    lowestUsedPrice :: Maybe Integer,
                    mediumImage :: Maybe String,
                    numberOfPages :: Maybe Integer,
                    publicationDate :: Maybe String,
                    publisher :: Maybe String,
                    smallBookCover :: Maybe String,
                    smallImage :: Maybe String,
                    title :: String,
                    totalAvailable :: Integer,
                    totalNew :: Integer,
                    totalUsed :: Integer
                  } deriving (Show, Generic)

-- While we still have to declare our type as instances of FromJSON
-- and ToJSON, we do *not* need to provide bodies for the instances.
-- Default versions will be supplied for us.

instance FromJSON Book
instance ToJSON Book

