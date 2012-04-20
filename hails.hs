{-# LANGUAGE ScopedTypeVariables #-}
import Hails.HttpServer
import Hails.HttpServer.Auth

import Data.IterIO.Server.TCPServer
import Data.Functor ((<$>))
import Data.Maybe (listToMaybe)
import qualified Data.ByteString.Lazy.Char8 as L8

import System.Environment
import System.Console.GetOpt hiding (Option)
import qualified System.Console.GetOpt as GetOpt
import System.IO (stderr, hPutStrLn)
import System.Exit

import GHC
import GHC.Paths
import DynFlags
import Unsafe.Coerce

import Hails.TCB.Types ( AppName, AppReqHandler )

import Control.Monad (when, void )


version :: String
version = "0.1"

about :: String -> String -> String
about prog ver = "About: " ++ prog ++ " " ++ ver ++
  " \n Simple tool for launching HAILS apps.\
  \ By default, " ++ prog ++ " uses the environment variables\
  \ APP_NAME, PORT, HMAC_KEY, AUTH_URL. Use the command-line \
  \ arguments to override."

--
--
--

-- | Given an application name, return the corresponding computation.
loadApp :: Bool -> AppName -> IO AppReqHandler
loadApp safe appName = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  let dflagsXSafe = if safe
                      then dflags { safeHaskell = Sf_Safe }
                      else dflags
  void $ setSessionDynFlags dflagsXSafe
  target <- guessTarget appName Nothing
  addTarget target
  r <- load LoadAllTargets
  case r of
    Failed -> error "Compilation failed"
    Succeeded -> do
      setContext [IIDecl $ simpleImportDecl (mkModuleName appName)]
      value <- compileExpr (appName ++ ".server") 
      return . unsafeCoerce $ value


main :: IO ()
main = do
  args <- getArgs
  env  <- getEnvironment
  opts <- hailsOpts args env
  when (optAbout opts) $ printAbout
  let appName = optName opts
      port    = optPort opts
      authF   = if optDev opts
                  then basicNoAuth
                  else externalAuth (optKey opts) (optUrl opts)
  func <- loadApp (optSafe opts) appName
  runTCPServer $ secureHttpServer authF (fromInteger port) func

--
-- Helper
--

printAbout :: IO ()
printAbout = do
  prog <- getProgName
  putStrLn $ about prog version
  exitSuccess


--
-- Parsing options
--

data Options = Options
   { optName   :: String        -- ^ App name
   , optPort   :: Integer       -- ^ App port number
   , optAbout  :: Bool          -- ^ About this program
   , optSafe   :: Bool          -- ^ Use -XSafe
   , optDev    :: Bool          -- ^ Development
   , optKey    :: L8.ByteString -- ^ HMAC key
   , optUrl    :: String        -- ^ URL to auth service
   }

defaultOpts :: Options
defaultOpts = Options { optName   = "App"
                      , optPort   = 8080
                      , optAbout  = False
                      , optSafe   = True
                      , optDev    = True
                      , optKey    = L8.empty
                      , optUrl    = "http://127.0.0.1" }

options :: [ OptDescr (Options -> Options) ]
options = 
  [ GetOpt.Option ['s'] ["start"]
      (ReqArg (\n o -> o { optName = n }) "APP_NAME")
      "Start application APP_NAME"
  , GetOpt.Option ['p'] ["port"]
      (ReqArg (\p o -> o { optPort = read p }) "PORT")
      "Run application on port PORT"
  , GetOpt.Option ['k'] ["key", "hmac-key"]
      (ReqArg (\k o -> o { optKey = read k }) "HMAC_KEY")
      "Application authentication HMAC key is HMAC_KEY"
  , GetOpt.Option ['d']    ["dev", "development"]
        (NoArg (\opts -> opts { optDev = True }))
        "Development mode, default (no authentication)"
  , GetOpt.Option ['p']    ["prod", "production"]
        (NoArg (\opts -> opts { optDev = False }))
        "Production mode (external authentication). Must set HMAC-KEY and AUTH_URL."
  , GetOpt.Option ['a'] ["auth-url"]
      (ReqArg (\u o -> o { optUrl = u }) "AUTH_URL")
      "Authentication service URL AUTH_URL"
  , GetOpt.Option []    ["unsafe"]
        (NoArg (\opts -> opts { optSafe = False }))
        "Turn off the -XSafe flag"
  , GetOpt.Option ['h','?']    ["help", "about"]
        (NoArg (\opts -> opts { optAbout = True }))
        "About this program"
  ]

hailsOpts :: [String] -> [(String, String)] -> IO Options
hailsOpts args env = do
  let opts = envOpts defaultOpts env
  case getOpt Permute options args of
    (o,[], []) -> return $ foldl (flip id) opts o
    (_,_,errs) -> do prog <- getProgName
                     hPutStrLn stderr $ concat errs ++
                                        usageInfo (header prog) options
                     exitFailure
    where header prog = "Usage: " ++ prog ++ " [OPTION...]"


envOpts :: Options -> [(String, String)] -> Options
envOpts opts env = 
  opts { optName = maybe (optName opts) id (fromEnv "APP_NAME")
       , optPort = maybe (optPort opts) id (readFromEnv "PORT")
       , optKey  = maybe (optKey  opts) id (L8.pack `fmap` fromEnv "HMAC_KEY")
       , optUrl  = maybe (optUrl  opts) id (fromEnv "AUTH_URL")
       }
    where fromEnv n = lookup n env
          readFromEnv n = lookup n env >>= mRead
          mRead :: Read a => String -> Maybe a
          mRead s = fst <$> (listToMaybe $ reads s)
