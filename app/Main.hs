{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.IRC.Client
import Data.Text
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as B
import Network.IRC.CTCP
import Control.Monad.IO.Class

logfn origin bs = putStrLn $ show bs

messageHandler :: Text -> StatefulIRC s Text
messageHandler "" = return ""
messageHandler message = do
  liftIO $ putStrLn $ "Got message: " ++ (show message)
  return $ Data.Text.reverse message

privMSGEvent :: UnicodeEvent -> StatefulIRC s ()
privMSGEvent e = do
  --let msg = _message e
  let msgtext = case (_message e) of
        Privmsg _ (Right text) -> text
        Privmsg _ (Left text) -> (decodeUtf8 $ getUnderlyingByteString text)
        _ -> "I'm sorry, I couldn't understand that"
  let commandtext = case stripPrefix ":g " msgtext of
        Just body -> body -- reply e (Data.Text.reverse body)
        Nothing -> ""
  response <- messageHandler commandtext
  reply e response

  

privMSGHandler :: EventHandler s
privMSGHandler = EventHandler "PRIVMSG" EPrivmsg privMSGEvent

run :: B.ByteString -> Int -> Text -> IO ()
run host port nick = do
  conn <- connectWithTLS' logfn host port 1
  let cfg = defaultIRCConf nick
  let cfg' = cfg { _eventHandlers = privMSGHandler : defaultEventHandlers , 
                   _channels = ["#lucidnonsense"] -- , "#mathematics"] 
                 }
  start conn cfg'

main = do
  run "underrun.starfyre.org" 6697 "Glossop"
