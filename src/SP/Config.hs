-- | Configuration data types and IO for parsing configuration files.
module SP.Config where

import Data.Char (toLower)
import Data.ConfigFile (emptyCP, has_option, get, readfile)
import Data.Either.Utils (forceEither)
import Database.MongoDB (Host (Host), PortID (PortNumber))
import Text.Regex (mkRegex, splitRegex)


-- | Configuration data type.
data Config = Config
    { ignRels, ignPosTags           :: [String] -- ^ Ignored 
    , minSmpSize, artSize           :: Int      -- ^ Size limits.
    , mongoHost                     :: Host     -- ^ MongoDB host.
    , lemmaIterHt, lemmaIterLt      :: Double   -- ^ Thresholds for iteration scores while merging clusters with same lemma.
    , posCatIterHt, posCatIterLt    :: Double   -- ^ Thresholds for iteration score for main loop.
    , compoundFile                  :: String   -- ^ Filename for pos tag compound patterns.
    , tm, ta, tc, tma, taa, tca     :: Double   -- ^ Operator thresholds, whole and for args.
    , wm, wa, wc                    :: Double   -- ^ Operator weights.
    , tsyn                          :: Double   -- ^ Synonym threshold.
    , useHyponymys                  :: Bool     -- ^ For using hyponymys.
    }

-- | Convert a list option string to a ByteString list.
toList :: String -> [String]
toList = splitRegex (mkRegex ",\\s*") . map toLower

-- | Retrieve an option from the config file.
getConfig :: IO Config
getConfig = do
    val <- readfile emptyCP "sp.conf"
    let parser = forceEither val
        -- Read an option, given the default value, section and option name.
        opt def sec name | has_option parser sec name = forceEither (get parser sec name)  
                         | otherwise = def
        param :: Double -> String -> String -> Double
        param v sec name = read (opt (show v) sec name) 
    -- Return the configuration.            
    return $! Config -- Strict.
        { minSmpSize = read (opt "5" "Preprocess" "samples")    -- Truncate processing of clusters
        , artSize    = read (opt "50" "Preprocess" "documents") -- Number of articles to process
        , ignRels    = toList (opt "" "Dependency" "ignore")    -- Relations to ignore
        , ignPosTags = toList (opt "" "POS" "ignore")           -- POS tags to ignore
        , compoundFile = opt "en.compound" "Preprocess" "compoundFile"
        , lemmaIterHt  = param 6.0 "Iteration" "lemmaIterHt"
        , lemmaIterLt  = param 3.0 "Iteration" "lemmaIterLt"
        , posCatIterHt = param 6.0 "Iteration" "posCatIterHt"
        , posCatIterLt = param 3.0 "Iteration" "posCatIterLt"
        , tm  = param 0.5 "Operator" "tm"
        , ta  = param 0.5 "Operator" "ta"
        , tc  = param 0.5 "Operator" "tc"
        , tma = param 0.5 "Operator" "tma"
        , taa = param 0.5 "Operator" "taa"
        , tca = param 0.5 "Operator" "tca"
        , wm  = param 0.0 "Operator" "wm"
        , wa  = param 0.0 "Operator" "wa"
        , wc  = param 0.0 "Operator" "wc"
        , tsyn = param 0.25 "Operator" "tsyn"
        , useHyponymys = False
        , mongoHost = Host (opt "127.0.0.1" "MongoDB" "host")   -- MongoDB host
                    $ PortNumber . fromIntegral 
                    $ (read (opt "27017" "MongoDB" "port") :: Int)
        }
        