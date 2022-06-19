{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.GI.Base
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as GDK
import System.Directory (getHomeDirectory)
import System.Posix.User (getEffectiveUserName)
import Data.Char (chr)
import Data.Aeson (eitherDecode, encode, eitherDecodeStrict)
import Data.Aeson.Types (ToJSON, FromJSON, Value, parseJSON, parseMaybe)
import GHC.Generics (Generic)
import Data.String ( fromString )
import Data.Char (toLower, isLower, isSpace)
-- import Network.HTTP.Req (JsonResponse, jsonResponse, responseBody, (/:), defaultHttpConfig, (=:), https, runReq, req, NoReqBody, GET)
import Network.HTTP.Req
import qualified Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import qualified Data.ByteString.Lazy as BL

import Data.Text.Encoding
import qualified Data.Text as T
import Data.Text (pack, unpack)
import Text.JSON.Generic (Typeable)
-- import qualified Data.Aeson.Schema as DAS
import Data.Aeson.Schema (schema, Object, get)
import Data.Aeson.Casing.Internal (snakeCase)
import Data.Typeable (typeOf)

import Data.Aeson.Casing (aesonPrefix, pascalCase)
import Data.Time (getCurrentTime)
import Data.Time.Format

import Control.Exception (IOException, catch)
import System.Environment (getEnv)
import Data.List


type YoutubeSchema = [schema|
  {
  items: List {
    contentDetails: {
      relatedPlaylists: {
        uploads: Text
      }
    }
  }
  }
|]

type YoutubeNewSchema = [schema|
  {
  items: List {
    snippet: {
      channelId: Text,
      publishedAt: Text,
      title: Text,
      resourceId: {
        videoId: Text
      },
      videoOwnerChannelTitle: Text
    }
  }
  }
|]


fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "Nothing"

fromIntJust :: Maybe Int -> Int
fromIntJust (Just x) = x
fromIntJust Nothing = 0

-- channelId :: String
-- channelId = "UCVls1GmFKf6WlTraIb_IaJg"

youtubeApi :: String -> IO Value
youtubeApi channelId = do
  youtubeApiKey <- catch (getEnv "YOUTUBE_API_KEY") (const $ pure "none" :: IOException -> IO String)
  response <- runReq defaultHttpConfig $ req
    GET (https "www.googleapis.com" /: "youtube" /: "v3" /: "channels") NoReqBody jsonResponse $
    "key" =: (youtubeApiKey :: String) <>
    "id" =: (channelId :: String) <>
    "part" =: ("contentDetails" :: String)
  return (responseBody response)

youtubeApiNew :: String -> IO Value
youtubeApiNew playlistId = do
  youtubeApiKey <- catch (getEnv "YOUTUBE_API_KEY") (const $ pure "none" :: IOException -> IO String)
  response <- runReq defaultHttpConfig $ req
    GET (https "www.googleapis.com" /: "youtube" /: "v3" /: "playlistItems") NoReqBody jsonResponse $
    "playlistId" =: (playlistId :: String) <>
    "key" =: (youtubeApiKey :: String) <>
    "part" =: ("snippet" :: String) <>
    "maxResults" =: ("7" :: String) <>
    "order" =: ("date" :: String)
  return (responseBody response)


fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON

getYoutubeApi :: String -> IO (Object YoutubeSchema)
getYoutubeApi channelId = do
  payload <- youtubeApi channelId
  let myPayload = Data.Aeson.encode payload
  output <- either fail return $ eitherDecode myPayload :: IO (Object YoutubeSchema)
  return (output)

getYoutubeNewApi :: String -> IO (Object YoutubeNewSchema)
getYoutubeNewApi channelId = do
  json <- getYoutubeApi channelId
  let playlistId = unpack (head [Data.Aeson.Schema.get| json.items[].contentDetails.relatedPlaylists.uploads |])
  payload <- youtubeApiNew playlistId
  let myPayload = Data.Aeson.encode payload
  output <- either fail return $ eitherDecode myPayload :: IO (Object YoutubeNewSchema)
  return (output)

-- styles :: Data.ByteString.Internal.ByteString
styles = mconcat
    [ "button { font-size: large; margin: 2pt 8pt; }"
    , "textview { font-size: 25px; }"
    ]

textViewGetValue tv = do
    buf <- Gtk.textViewGetBuffer tv
    start <- Gtk.textBufferGetStartIter buf
    end <- Gtk.textBufferGetEndIter buf
    value <- Gtk.textBufferGetText buf start end True
    return value

dropNonLetters :: [Char] -> [Char]
dropNonLetters xs = (filter (\x -> isLower x || isSpace x || x == '-')) $ spaceToDash(map toLower xs)
  where
    spaceToDash = map (\x -> if isSpace x then '-' else x)

toCsv :: (T.Text, T.Text, T.Text, T.Text) -> [Char]
toCsv (x,y,z, zz) = dropNonLetters(unpack(zz)) ++ "," ++ unpack(y) ++ "," ++ dropNonLetters (unpack(x)) ++ "," ++ unpack(z) ++ "\n"

perChannel :: String -> IO [Char]
perChannel channelId = do
  videos <- getYoutubeNewApi channelId

  let l1 = [Data.Aeson.Schema.get| videos.items[].snippet.title |]
  let l2 = [Data.Aeson.Schema.get| videos.items[].snippet.publishedAt |]
  let l3 = [Data.Aeson.Schema.get| videos.items[].snippet.resourceId.videoId |]
  let l4 = [Data.Aeson.Schema.get| videos.items[].snippet.videoOwnerChannelTitle |]
  let tuple = [ (e1,e2,e3,e4) | (((e1,e2),e3), e4) <- (zip (zip (zip l1 l2) l3) l4) ]
  -- putStrLn $ foldr (++) "" (map toCsv tuple)
  return ( foldr (++) "" (map toCsv tuple))


f xs = do
  x <- perChannel xs
  -- print x
  return (x)

main :: IO ()
main = do
  Gtk.init Nothing

  home <- getHomeDirectory
  user <- getEffectiveUserName

  win <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.setContainerBorderWidth win 10
  Gtk.setWindowTitle win "Weather"
  Gtk.setWindowResizable win False
  Gtk.setWindowDefaultWidth win 750
  Gtk.setWindowDefaultHeight win 225
  Gtk.setWindowWindowPosition win Gtk.WindowPositionCenter
  Gtk.windowSetDecorated win False

  screen <- maybe (fail "No screen?!") return =<< GDK.screenGetDefault
  css <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData css styles
  Gtk.styleContextAddProviderForScreen screen css (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

  img1 <- Gtk.imageNewFromFile $ home ++ "/.local/img/cancel.png"

  label1 <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup label1 ("<b>" <> "Done" <> "</b>")


  lonLat <- Gtk.textViewNew
  Gtk.textViewSetEditable lonLat True
  textBufferLonLat <- Gtk.getTextViewBuffer lonLat
  Gtk.textBufferSetText textBufferLonLat (( "45.18,-93.32")) (-1)

  data1 <- textViewGetValue lonLat
  print data1

  textView <- Gtk.textViewNew
  Gtk.textViewSetEditable textView False

  textBuffer <- Gtk.getTextViewBuffer textView
  box <- Gtk.boxNew Gtk.OrientationHorizontal 0

  Gtk.setBoxHomogeneous box False
  Gtk.boxPackStart box textView True True 0

  btn1 <- Gtk.buttonNew
  Gtk.buttonSetRelief btn1 Gtk.ReliefStyleNone
  Gtk.buttonSetImage btn1 $ Just img1
  Gtk.widgetSetHexpand btn1 False
  on btn1 #clicked $ do
    putStrLn "User chose: Done"
    Gtk.widgetDestroy win

  on win #keyPressEvent $ \keyEvent -> do
    key <- keyEvent `Data.GI.Base.get` #keyval >>= GDK.keyvalToUnicode
    putStrLn $ "Key pressed: (" ++ show key ++ ")"
    if key == 27 then Gtk.mainQuit else pure ()
    return False

  grid <- Gtk.gridNew
  Gtk.gridSetColumnSpacing grid 10
  Gtk.gridSetRowSpacing grid 10
  Gtk.gridSetColumnHomogeneous grid True

  #attach grid btn1 0 0 1 1
  #attach grid label1 0 1 1 1
  #attach grid box 0 2 1 1
  #attach grid lonLat 0 3 1 1

  #add win grid

  Gtk.onWidgetDestroy win Gtk.mainQuit
  #showAll win

-- #UCQN2DsjnYH60SFBIA6IkNwg
  let myDataList = ["UCVls1GmFKf6WlTraIb_IaJg", "UCZ4AMrDcNrfy3X6nsU8-rPg", "UCMIqrmh2lMdzhlCPK5ahsAg", "UCc-0YpRpqgA5lPTpSQ5uo-Q", "UCcUf33cEPky2GiWBgOP-jQA", "UCt3JiNkefsfbA2N4SgEkoiQ", "UC3xdLFFsqG701QAyGJIPT1g", "UC2cC48A261pBVKztLyzOAnA", "UCQV6O5wfETMrWqQ7Ro9r-0g", "UCld68syR8Wi-GY_n4CaoJGA"]


  -- myData0 <- perChannel "UCVls1GmFKf6WlTraIb_IaJg" --distrotube
  -- myData1 <- perChannel "UCZ4AMrDcNrfy3X6nsU8-rPg" --
  -- myData2 <- perChannel "UCMIqrmh2lMdzhlCPK5ahsAg" --darknet diaries
  -- myData3 <- perChannel "UCc-0YpRpqgA5lPTpSQ5uo-Q" --audit the audit
  -- myData4 <- perChannel "UCcUf33cEPky2GiWBgOP-jQA" --coffeehouse crime
  -- myData5 <- perChannel "UCt3JiNkefsfbA2N4SgEkoiQ" --
  -- myData6 <- perChannel "UC3xdLFFsqG701QAyGJIPT1g"
  -- myData7 <- perChannel "UC2cC48A261pBVKztLyzOAnA"
  -- myData8 <- perChannel "UCQV6O5wfETMrWqQ7Ro9r-0g"
  -- myData9 <- perChannel "UCld68syR8Wi-GY_n4CaoJGA"


  output <- mapM f myDataList


  Gtk.textBufferSetText textBuffer (pack( concat output)) (-1)
  Gtk.main
