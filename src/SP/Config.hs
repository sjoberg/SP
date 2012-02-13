module SP.Config where

import Data.Char
import Data.ConfigFile                                                          
import Data.Either.Utils                                                        
import SP.ByteString (ByteString, pack)
import Text.Regex

getOpt def sec opt = do
  -- Open config. file.                                                         
  val <- readfile emptyCP "sp.conf"
  let cfgPar = forceEither val                                                  
  return $ if has_option cfgPar sec opt
              then forceEither $ get cfgPar sec opt
              else def

getConfig = do
  depIgnStr <- getOpt "" "Dependency" "ignore"
  posIgnStr <- getOpt "" "POS" "ignore"
  minSmpSizeStr <- getOpt "5" "Preprocess" "samples"
  artSizeStr <- getOpt "50" "Preprocess" "articles"
  let depIgn = optBList depIgnStr
      posIgn = optBList posIgnStr
      minSmpSize = read minSmpSizeStr::Int
      artSize = read artSizeStr::Int
  return $ Config depIgn posIgn minSmpSize artSize

optBList opt = map pack $ splitRegex optListRegex $ map toLower opt
optListRegex = mkRegex ",\\s*"

data Config = Config {depIgn, posIgn::[ByteString], minSmpSize, artSize::Int}

