
import Args
import System.Environment

main :: IO ()
main = do
    getArgs >>= parseOpts >>= resolveOpts
    