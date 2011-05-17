{-# LANGUAGE CPP #-}
#if PRODUCTION
import Controller (withFoundation)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withFoundation $ run 3000
#else
import Controller (withFoundation)
import System.IO (hPutStrLn, stderr)
import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    let port = 3000
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    withFoundation $ run port . debug
#endif
