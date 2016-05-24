{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import ClassyPrelude
import Yesod
import qualified Data.Text as T
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import Network.HTTP.Types.Status
import System.Environment (lookupEnv)

import qualified Codegen as Rybu
import qualified Parser as Rybu
import qualified Err as Rybu

data App = App

mkYesod "App" [parseRoutes|
/compile CompileR POST
|]

instance Yesod App

postCompileR :: Handler String
postCompileR = do
    source <- T.unpack . T.concat <$> (rawRequestBody $= CT.decodeUtf8 $$ CL.consume)
    case Rybu.parseModel "" source of
        Right model ->
            case Rybu.generateDedan model of
                Right program -> pure program
                Left err -> compileError ("Error: " ++ Rybu.ppError err)
        
        Left err -> compileError ("Parse Error " ++ show err)

compileError msg = sendResponseStatus status400 (msg ++ "\n")

main :: IO ()
main = do
    port <- fromMaybe 3000 . (>>= readMay) <$> lookupEnv "PORT"
    putStrLn $ "Running on port " ++ tshow port
    warp port App
