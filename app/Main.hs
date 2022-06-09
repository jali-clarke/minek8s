import qualified Data.Version as Version
import Hello
import qualified System.Info as Info

main :: IO ()
main = putStrLn $ helloString <> " (from compiler " <> Info.compilerName <> " " <> Version.showVersion Info.compilerVersion <> ")"
