{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (unless, when, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           Data.List (find)
import           Discord
import           Discord.Types
import qualified Discord.Requests as R
import           Configuration.Dotenv (loadFile, defaultConfig)
import qualified System.Environment
import qualified Data.Map.Strict as M
import           Data.Sequence (Seq(..), (|>))
import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import qualified Data.Sequence as S
import           Data.Foldable (toList)

import           Network.Wreq
import           Control.Lens ((^.), (.~), (&))
import qualified Data.ByteString.Char8 as BS
import           Data.Aeson (FromJSON(..), (.:), (.=), Value(..), object, withObject, eitherDecode)

import          Embed (findUrlAndGetEmbed)

data Msg = Msg {
    msgId   :: MessageId,
    author  :: Text,
    content :: Text,
    images  :: [Text]
} deriving (Show)

data Deleted = Deleted {
    deleted_author  :: Text,
    deleted_content :: Text,
    deleted_images  :: [Text]
} deriving (Show)

data LogMap = LogMap {
    history     :: M.Map ChannelId (Seq Msg),
    lastDeleted :: M.Map ChannelId Deleted
} deriving (Show)

initState :: LogMap
initState = LogMap M.empty M.empty

logChat :: MVar LogMap -> ChannelId -> Msg -> IO ()
logChat st ch m = modifyMVar_ st $ \s ->
  let old = M.findWithDefault S.empty ch (history s)
      new = let o' = if S.length old >= 10 then S.drop 1 old else old
            in o' |> m
  in pure s { history = M.insert ch new (history s) }

setLastDeleted :: MVar LogMap -> ChannelId -> Deleted -> IO ()
setLastDeleted st ch d = modifyMVar_ st $ \s ->
  pure s { lastDeleted = M.insert ch d (lastDeleted s) }

getLastDeleted :: MVar LogMap -> ChannelId -> IO (Maybe Deleted)
getLastDeleted st ch = do
  s <- readMVar st
  pure $ M.lookup ch (lastDeleted s)

getContext :: MVar LogMap -> ChannelId -> IO [Msg]
getContext st ch = do
  s <- readMVar st
  pure . toList $ M.findWithDefault S.empty ch (history s)


narvisActivate :: MVar LogMap -> IO ()
narvisActivate st = do
  loadFile defaultConfig
  token <- System.Environment.getEnv "DISCORD_TOKEN"
  let token_text = pack token
  userFacingError <- runDiscord $ def
    { discordToken   = token_text
    , discordOnEvent = eventHandler st
    , discordOnLog   = \s -> TIO.putStrLn s >> TIO.putStrLn ""
    }
  TIO.putStrLn userFacingError


newtype DSMsg   = DSMsg   { dsContent :: Text } deriving Show
newtype DSChoice= DSChoice{ dsMessage :: DSMsg } deriving Show
newtype DSResp  = DSResp  { dsChoices :: [DSChoice] } deriving Show

instance FromJSON DSMsg where
  parseJSON = withObject "DSMsg" $ \o -> DSMsg <$> o .: "content"

instance FromJSON DSChoice where
  parseJSON = withObject "DSChoice" $ \o -> DSChoice <$> o .: "message"

instance FromJSON DSResp where
  parseJSON = withObject "DSResp" $ \o -> DSResp <$> o .: "choices"

callDeepSeek :: Text -> [Msg] -> Text -> IO Text
callDeepSeek systemPrompt ctx userPrompt = do
  apiKey <- System.Environment.getEnv "DEEPSEEK_API"
  let sys = object ["role" .= String "system", "content" .= String systemPrompt]
      ctxLines = [ author m <> ": " <> content m | m <- ctx ]
      ctxBlock = if null ctx then "" else "Context (most recent last):\n" <> T.intercalate "\n" ctxLines <> "\n---\n"
      usr = object ["role" .= String "user", "content" .= String (ctxBlock <> userPrompt)]
      body = object
        [ "model"    .= String "deepseek-chat"
        , "messages" .= [sys, usr]
        , "stream"   .= False
        ]
      opts = defaults
           & header "Authorization" .~ [BS.pack ("Bearer " <> apiKey)]
           & header "Content-Type"  .~ [BS.pack "application/json"]
  r <- postWith opts "https://api.deepseek.com/chat/completions" body
  let bs = r ^. responseBody
  case eitherDecode bs of
    Left _ -> pure "Sorry, I couldn't reach DeepSeek."
    Right (DSResp choices) ->
      case choices of
        (DSChoice (DSMsg t) : _) -> pure t
        _                        -> pure "narvis deactivated"


eventHandler :: MVar LogMap -> Event -> DiscordHandler ()
eventHandler st (MessageCreate m) = do
  unless (userIsBot $ messageAuthor m) $ do
    let ch   = messageChannelId m
        imgs = [ attachmentUrl a | a <- messageAttachments m ]
        msum = Msg (messageId m) (userName $ messageAuthor m) (messageContent m) imgs
    liftIO $ logChat st ch msum
    case findUrlAndGetEmbed (messageContent m) of
      Just out -> void $ restCall (R.CreateMessage ch out)
      Nothing  -> do
        me <- restCall R.GetCurrentUser
        case me of
          Left _ -> pure ()
          Right botUser -> do
            when (isPing m) $ do
              md <- liftIO $ getLastDeleted st ch
              case md of
                Just d  -> do
                  let extra = if null (deleted_images d) then "" else "\n" <> T.intercalate "\n" (deleted_images d)
                  void $ restCall (R.CreateMessage ch (T.concat [deleted_content d, " - ", deleted_author d, extra]))
                Nothing -> void $ restCall (R.CreateMessage ch "troll")

            case extractPromptAfterMention botUser m of
              Nothing -> pure ()
              Just promptText -> do
                ctx <- liftIO $ getContext st ch
                sysPrompt <- pack <$> liftIO (System.Environment.getEnv "NARVIS_PROMPT")
                let replyFocus = case messageReferencedMessage m of
                                   Just rm -> "\n(Replying to " <> userName (messageAuthor rm) <> ": " <> messageContent rm <> ")\n"
                                   Nothing -> ""
                    fullUserPrompt = replyFocus <> promptText
                ans <- liftIO $ callDeepSeek sysPrompt ctx fullUserPrompt
                void $ restCall (R.CreateMessage ch ans)

eventHandler st (MessageDelete ch mid) = do
  hs <- liftIO $ getContext st ch
  case find (\x -> msgId x == mid) (reverse hs) of
    Just (Msg _ a c imgs) -> liftIO $ setLastDeleted st ch (Deleted a c imgs)
    Nothing -> pure ()
eventHandler st (MessageDeleteBulk ch mids) = do
  let midsL = toList mids
  hs <- liftIO $ getContext st ch
  case find (\x -> msgId x `elem` midsL) (reverse hs) of
    Just (Msg _ a c imgs) -> liftIO $ setLastDeleted st ch (Deleted a c imgs)
    Nothing -> pure ()
eventHandler _ _ = pure ()


extractPromptAfterMention :: User -> Message -> Maybe Text
extractPromptAfterMention me msg =
  let t   = T.stripStart (messageContent msg)
      mid = T.pack (show (userId me))
      m1  = "<@"  <> mid <> ">"
      m2  = "<@!" <> mid <> ">"
      restAfter mention =
        let s = T.stripStart (T.drop (T.length mention) t)
        in if T.null s then Nothing else Just s
  in if      T.isPrefixOf m1 t then restAfter m1
     else if T.isPrefixOf m2 t then restAfter m2
     else Nothing

isPing :: Message -> Bool
isPing = (== "/narvis snipe") . T.toCaseFold . T.strip . messageContent


main :: IO ()
main = do
  putStrLn "Narvis activated"
  st <- newMVar initState
  narvisActivate st

