
import Args
import System.Environment

main = do
    getArgs >>= parseOpts >>= resolveOpts
    