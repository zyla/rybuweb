{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import ClassyPrelude hiding (Handler)
import Yesod
import qualified Data.Text as T
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import Network.HTTP.Types.Status
import System.Environment (lookupEnv)
import Text.RawString.QQ

import qualified Codegen as Rybu
import qualified Parser as Rybu
import qualified Err as Rybu

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/compile CompileR POST
|]

instance Yesod App

postCompileR :: Handler String
postCompileR = do
    source <- getSource
    case Rybu.parseModel "" source of
        Right model ->
            case Rybu.generateDedan model of
                Right program -> pure program
                Left err -> compileError ("Error: " ++ Rybu.ppError err)
        
        Left err -> compileError ("Parse Error " ++ show err)

getSource = do
    msource <- lookupPostParam "source"
    T.unpack <$> case msource of
        Just source -> pure source
        Nothing -> T.concat <$> (rawRequestBody $= CT.decodeUtf8 $$ CL.consume)

compileError msg = sendResponseStatus status400 (msg ++ "\n")

example :: Text
example = [r|server sem {
    var state : {up, down};

    { p | state = :up } -> { state = :down }
    { v } -> { state = :up }
}

var s : sem() { state = :up };

process p1() {
    loop { s.p(); }
}

process p2() {
    loop { s.v(); }
}

|]

getHomeR :: Handler Html
getHomeR = defaultLayout $
    [whamlet|
        <h2>Rybu Online
        <p>Wynikowy plik należy załadować do programu Dedan.
        <form action=@{CompileR} method=POST>
            <textarea name="source" rows=25 cols=80>#{example}
            <br>
            <button type=submit>Wyślij
        <p><small>Uwaga: występują limity pamięci i czasu procesora.
    |]

main :: IO ()
main = do
    port <- fromMaybe 3000 . (>>= readMay) <$> lookupEnv "PORT"
    putStrLn $ "Running on port " ++ tshow port
    warp port App
