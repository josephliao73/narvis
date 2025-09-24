{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (when, void, unless)
import           Data.Text (pack)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           Discord
import           Discord.Types
import qualified Discord.Requests as R
import Configuration.Dotenv (loadFile, defaultConfig)
import qualified System.Environment


narvisActivate:: IO ()
narvisActivate = do
  loadFile defaultConfig
  token <- System.Environment.getEnv "DISCORD_TOKEN"
  let token_text = pack token
  print token_text
  userFacingError <- runDiscord $ def {
      discordToken = token_text,
      discordOnEvent = eventHandler,
      discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
    }
  TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler (MessageCreate m) = do
  unless (userIsBot $ messageAuthor m) $ do
    me <- restCall R.GetCurrentUser
    case me of
      Right botUser -> do
        when (botIsMentioned botUser m) $
          void $ restCall (R.CreateMessage (messageChannelId m) "i hate you!")
        when (isPing m) $
          void $ restCall(R.CreateMessage (messageChannelId m) "troll")
      Left _ -> pure ()
eventHandler _ = pure ()

botIsMentioned :: User -> Message -> Bool
botIsMentioned me msg =
  any ((== userId me) . userId) (messageMentions msg)

fromBot :: Message -> Bool 
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = (== "/narvis snipe") . T.toCaseFold . T.strip . messageContent


main :: IO ()
main = narvisActivate
