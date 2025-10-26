{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Embed (findUrlAndGetEmbed)where

import Text.Parsec
import Data.Text (Text)
import qualified Data.Text as T


data Url = Url {
    uUri :: Text,
    uSubdomain :: Maybe Text,
    uDomain :: Text,
    uPath :: Text
} deriving (Show, Eq)

convertDomain :: Text -> Text
convertDomain domain =
    case T.toLower domain of
        "twitter.com"   -> "fixvx.com"
        "reddit.com"    -> "vxreddit.com"
        "instagram.com" -> "kkinstagram.com"
        "x.com"         -> "fixvx.com"
        _               -> domain

parseUri :: Parsec Text () String
parseUri = try (string "http://") 
            <|> string "https://"

parseSubdomain :: Parsec Text () (Maybe String)
parseSubdomain = optionMaybe 
                 (try (string "www.") 
                   <|> string "old.")

parseUrl :: Parsec Text () Url
parseUrl = do
    uri       <- T.pack <$> parseUri
    subdomain <- fmap T.pack <$> parseSubdomain
    domain    <- T.pack <$> many1 (noneOf "/")
    path      <- T.pack <$> many  (noneOf " \t\n\r")
    return $ Url uri subdomain domain path

findUrlAndGetEmbed :: Text -> Maybe Text
findUrlAndGetEmbed input =
    case parse (manyTill anyChar (lookAhead parseUrl) >> parseUrl) "" input of
        Left _ -> Nothing
        Right (Url uri _ domain path) -> 
            if convertDomain domain /= domain 
            then Just $ uri <> convertDomain domain <> path
            else Nothing
