{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.IRC.Client
import Data.Text
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as B
import Network.IRC.CTCP
import Control.Monad.IO.Class
import Interp
import System.Exit

logfn origin bs = putStrLn $ show bs



messageHandler :: Text -> StatefulIRC s Text
messageHandler "" = return ""
messageHandler message = do
  --let msgS
  liftIO $ putStrLn $ "Got message: " ++ (show message)
  let (prefix, body) = breakOn " " message
  evRes <- case prefix of
    "eval" -> liftIO $ evalHS Interp.Run (unpack body)
    "type" -> liftIO $ evalHS Interp.TypeCheck (unpack body)
    "quit" -> liftIO $ exitFailure
    _ -> return $ Right "I don't know how to do that, sorry..."
  liftIO $ putStrLn $ show evRes
  return $ pack $ case evRes of
        Left errors -> Prelude.foldl (++) "" $ take 2 errors
        Right result -> result
  --return $ Data.Text.reverse message

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
                   _channels = ["#lucidnonsense", "#mathematics"] 
                 }
  start conn cfg'

main = do
  run "underrun.starfyre.org" 6697 "Glossop"
