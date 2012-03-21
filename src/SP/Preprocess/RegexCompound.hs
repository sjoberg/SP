module SP.Preprocess.RegexCompound (createRegexCompounds)  where

import Control.Arrow (first, second)
import Control.Parallel.Strategies as P
import Data.Array (elems)
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Char8 (ByteString)
import Data.Either.Utils (forceEither)
import Data.Function (on)
import qualified Data.HashMap.Strict as HashMap
import Data.List.Extras.Argmax (argmax)
import Data.Maybe
import Data.Ord (comparing)
import Data.Tuple
import Text.Regex.Base.RegexLike (MatchArray)
import Text.Regex.TDFA.ByteString
import Text.Regex.TDFA.Common hiding (on)

import qualified SP.ByteString
import SP.Cluster
import SP.Preprocess.Compound
import SP.Preprocess.RegexCompoundConversion (toString, posIdMap, posSet)
import SP.Redirect

-- Stream fusion
import Data.List.Stream
import Prelude hiding ( (++), drop, concat, concatMap, filter, head, length
                      , map, null, tail, take, repeat, replicate, zip, zip3 )

createRegexCompounds :: [Partition] -> [Partition]
--createRegexCompounds :: [Partition] -> [String]
createRegexCompounds partitions =
  let -- Needed for POS lookup.
      posMap :: HashMap.HashMap SP.ByteString.ByteString Int
      posMap = posIdMap . posSet $ partitions

      -- Object Cluster to a one character ByteString.
      objPosSmb :: ObjectCluster -> ByteString
      objPosSmb = ByteString.pack . toString posMap . pos . head . parts
      
      -- POS string to a one character ByteString.
      strPosSmb :: String -> ByteString
      strPosSmb = ByteString.pack . toString posMap . SP.ByteString.pack

      sentenceToString :: [ObjectCluster] -> ByteString
      sentenceToString = ByteString.concat . map objPosSmb

      -- Regexp tuples for replacements.
      rxts = let m = ByteString.pack
                 p = strPosSmb
                 r = SP.ByteString.pack

                 cd  = p"CD"
                 dt  = p"DT"
                 jj  = p"JJ"  -- Adjective.
                 psc = p"IN"  -- Preposition or subordinating conjunction.
                 md  = p"MD"  -- Modal.
                 nn  = p"NN"  -- Noun singular or mass.
                 nnp = p"NNP" -- Proper noun, singular.
                 nns = p"NNS" -- Noun, plural.
                 rb  = p"RB"  -- Negation, e.g. not, 'nt.
                 rp  = p"RP"  -- Particle.
                 to  = p"TO"  -- To.
                 vb  = p"VB"  -- Verb, base form.
                 vbd = p"VBD" -- Verb, past tense.
                 vbg = p"VBG" -- Verb, gerundium.
                 vbz = p"VBZ" -- Verb, present tense, third person singular.
                 vbn = p"VBN" -- Verb, past particle.
                 vbp = p"VBP" -- Verb, present tense, third person plural.
                 
                 (?) = m"?"
                 (*) = m"*"
                 (+) = m"+"
                 (^) = m"^"

                 seq, or :: [ByteString.ByteString] -> ByteString.ByteString
                 seq contents = ByteString.concat (m"(":contents++[m")"])
                 or  contents = ByteString.concat (m"[":contents++[m"]"])
                 
                 -- TO + IN
                 tin = or [psc, to]
                 -- JJ + NN
                 jjnn = or [nn, nnp, nns, jj]
                 -- Maybe rb
                 mrb = seq [rb, (*)]
                 -- TO + VB
                 tovb = seq [to, vb]
             in [ ([nnp, (?), jj, jj, (+), nn],                   r"NN",  2) 
                , ([jj, or [nn, nnp, nns]],                       r"NN",  3)

                , ([vbz, (?), rb, (*), jj, to, (?)],              r"VBZ", 1)
                , ([vbd, (?), rb, (*), jj, to, (?)],              r"VBD", 1)
                , ([vbz, (?), rb, (*), vbn, psc, (+)],            r"VBZ", 1)
                , ([vbz, (?), rb, (*), vbn, to, vb],              r"VBN", 1)     -- has agreed to buy
                , ([vbp, (?), rb, (*), vbn, to, vb],              r"VBN", 1)
                , ([vbp, (?), rb, (*), vbn, psc, (+)],            r"VBP", 1)

                , ([vbz, rb, (*), vbd, psc],                      r"VBZ", 1)     -- is also regularly touted as 
                , ([vbp, rb, (*), vbd, psc],                      r"VBP", 1)     -- are also regularly touted as

                , ([vbz, rb, (*), psc, (?)],                      r"VBZ", 1)     -- is no, is with
                , ([vbp, rb, (*), psc, (?)],                      r"VBP", 1)     -- are no
                , ([vbd, rb, (*), psc, (?)],                      r"VBD", 1)     -- was no, was often in

                , ([vbz, rb, (*), vbg, to, vb, psc, (?)],         r"VBZ", 1)     -- is helping to win over
                , ([vbp, rb, (*), vbg, to, vb, psc, (?)],         r"VBP", 1)     -- are helping to win over
                , ([vbd, rb, (*), vbg, to, vb, psc, (?)],         r"VBD", 1)     -- was helping to win over

                , ([to, rb, (*), vb, psc, (?)],                   r"VB", 1)      -- to take over

                , ([vbz, rb],                                     r"VBZ", 1)     -- makes up
                , ([vbp, rb],                                     r"VBP", 1)     -- make up
                , ([vbd, rb],                                     r"VBD", 1)     -- made up

                , ([rb, (*), vbg, psc],                           r"VBG", 1)     -- looking at

                , ([vbz, tovb, vbg],                              r"VBZ", 1)     -- plans to start preparing
                , ([vbp, tovb, vbg],                              r"VBP", 1)     -- plan to start preparing
                , ([vbd, tovb, vbg],                              r"VBD", 1)     -- planned to start preparing

                , ([vbz, rb, (*), psc, jjnn, to, vb],             r"VBZ", 1)     -- is on track to lift
                , ([vbp, rb, (*), psc, jjnn, to, vb],             r"VBP", 1)     -- are on track to lift
                , ([vbd, rb, (*), psc, jjnn, to, vb],             r"VBD", 1)     -- was on track to lift

                , ([vbz, mrb, vbn, vbg, to, vb],                  r"VBD", 1)     -- has been trying to complete
                , ([vbp, mrb, vbn, vbg, to, vb],                  r"VBD", 1)     -- have been trying to complete
                , ([vbd, mrb, vbn, vbg, to, vb],                  r"VBD", 1)     -- had been trying to complete

                , ([vbz, rb, (*), psc],                           r"VBZ", 1)     -- is before
                , ([vbz, rb, (*), psc, jj, psc],                  r"VBZ", 1)     -- is still not as good as
                , ([vbp, rb, (*), psc],                           r"VBP", 1)     -- are before
                , ([vbp, rb, (*), psc, jj, psc],                  r"VBP", 1)     -- are still not as good as
                , ([vbd, rb, (*), psc],                           r"VBD", 1)     -- was before
                , ([vbd, rb, (*), psc, jj, psc],                  r"VBD", 1)     -- was still not as good as

                , ([ vbz, rb, (*), psc
                   , or [psc, seq [nns, tovb]], (?)],             r"VBZ", 1)     -- is in talks to buy, is in talks with, is in
                , ([ vbp, rb, (*), psc
                   , or [psc, seq [nns, tovb]], (?)],             r"VBP", 1)     -- are in talks to buy, are in talks with, are in

                , ([md, rb, (*), vb, psc, nns, tovb],             r"VB", 2)      -- may be in discussions to buy
                , ([md, rb, (*), vb, psc, (?)],                   r"VB", 1)      -- would not result, would result in
                , ([md, rb, (*), vb, nns, psc],                   r"VBZ", 2)     -- would put funds from 
                , ([md, (?), rb, (*), vb, to, vb],                r"VBZ", 2)     -- would continue to voice


                , ([vbz, rb, (*), vbg, (+), psc, (?)],            r"VBZ", 1)     -- is currently expanding in
                , ([vbd, rb, (*), vbg, (+), psc, (?)],            r"VBZ", 1)     -- was not looking at

                , ([vbz, rb, (*), vbn, vbn],                      r"VBZ", 1)     -- has not been disclosed
                , ([vbp, rb, (*), vbn, vbn],                      r"VBP", 1)     -- have not been disclosed
                , ([vbd, rb, (*), vbn, or [to, psc]],             r"VBD", 1)
                , ([md, vb, or [vbg, to]],                        r"VBZ", 1)
                , ([vbd, to, rb, (*), vb, vbn],                   r"VBD", 1)
                , ([ cd, (?), or [nns, nn]
                   , seq [psc, dt, (?), or [nn, nns]], (?)],      r"NN", 2)      -- 100 shoes per human
                , ([nn, psc, or [nn, nns]],                       r"NN", 2)      -- city of clouds
                , ([nnp, psc, or [nn, nns]],                      r"NNP", 3)     -- city of clouds
                , ([nns, psc, or [nn, nns]],                      r"NNS", 3)     -- cities of clouds
                
                , ([vbz, dt, (?), jjnn, tin],                     r"VBZ", 1)     -- takes control of, is home to
                , ([vbp, dt, (?), jjnn, tin],                     r"VBP", 1)     -- take control of, are homes to
                , ([vbd, dt, (?), jjnn, tin],                     r"VBD", 1)     -- take control of, are homes to
                , ([vbd, mrb, vb, (?), jjnn, tin],                r"VBD", 1)     -- did take control of, took control of
                , ([vbd, mrb, to, vb, seq [jjnn, (?), tin], (?)], r"VBD", 1)     -- decided to take control of, decided to invest in
                , ([vbz, to, vb, seq [jjnn, (?), tin], (?)],      r"VBZ", 1)     -- has to take control of, has to invest in
                , ([vbp, to, vb, seq [jjnn, (?), tin], (?)],      r"VBP", 1)     -- have to take control of, have to invest in

                , ([vbd, rp, vbg],                                r"VBD", 1)     -- ruled out buying
                , ([vbz, rp, vbg],                                r"VBZ", 1)     -- rules out buying
                , ([vbp, rp, vbg],                                r"VBP", 1)     -- rule out buying
                ]


      regexps :: [(Regex, SP.ByteString.ByteString, Int)]
      regexps = let m = ByteString.pack    -- Meta character
                    p = strPosSmb          -- POS tag
                    r = SP.ByteString.pack -- Replacement POS tag
                    comp = let compOption = CompOption { caseSensitive = True
                                                       , multiline = False
                                                       , rightAssoc = True
                                                       , newSyntax = False
                                                       , lastStarGreedy = False
                                                       }
                               execOption = ExecOption { captureGroups = True }
                           in compile compOption execOption
                    conv = forceEither . comp . ByteString.concat
                in map (first $ forceEither . comp . ByteString.concat) rxts

      -- Replacement phrases for a group.
      replPhrases :: [ObjectCluster]
                  -> [Repl]
      replPhrases group = let string = sentenceToString group
                              toMatches :: Regex -> [[ObjectCluster]]
                              toMatches regex = 
                                let exec = forceEither $ execute regex string
                                    fromMaybe m = case m of Just ma -> elems ma 
                                                            Nothing -> []
                                in map (slice group) (fromMaybe exec)
                              zipRegexs (p, r, o) = 
                                zip3 (toMatches p) (repeat r) (repeat o)
                              repls = concatMap zipRegexs regexps
                          in getCmptbl repls

      -- Replacement phrases for all groups.
      allReplPhrases :: [Repl]
      allReplPhrases = concatMap replPhrases $ groups partitions

      grouped :: [[Repl]]
      grouped = let key :: Repl -> [SP.ByteString.ByteString]
                    key = map (form . head . parts) . fst3
                    group = groupBy ((==) `on` key)
                    sort = sortBy $ comparing key
                    limit = filter $ \g -> lenCmp (thd3 $ head g) g
                in group . sort $ allReplPhrases

      -- An object to object list given a replacement tuple.
      objList :: Repl -> [(ObjectCluster, ObjectCluster)]
      objList repl = let new = merge $ fst3 repl 
                         part = (head . parts $ new) {pos = snd3 repl}
                     in zip (fst3 repl) (repeat $ new {parts = [part]})

      objMap :: Partition -> HashMap.HashMap ObjectCluster ObjectCluster
      objMap partition = let ogs :: [[ObjectCluster]]
                             ogs = groups [partition]
                             repls :: [Repl]
                             repls = concatMap replPhrases ogs
                         in HashMap.fromList $ concatMap objList repls


      first f (x,y,z) = (f x,y,z)
      fst3 (x,_,_) = x
      snd3 (_,y,_) = y
      thd3 (_,_,z) = z

      update :: Partition -> Partition
      update partition = redirect partition $ objMap partition
  in P.parMap rseq update partitions
  -- map (intercalate " " . map (SP.ByteString.unpack . text . head . parts) . fst3) $ concat grouped

groups :: [Partition] -> [[ObjectCluster]]
groups partitions = let group :: Partition -> [[ObjectCluster]]
                        group = groupBy ((==) `on` (sntId . head . parts)) . ocs
                    in concatMap group partitions

slice :: [a] -> (Int, Int) -> [a]
slice xs (from, length) = take length . drop from $ xs

type Repl = ([ObjectCluster], SP.ByteString.ByteString, Int)

-- Defunct
mkCompatible :: [Repl] -> [Repl]
mkCompatible []    = []
mkCompatible repls = 
  let go :: [Repl] -> [Repl] -> [Repl]
      go []     bs = bs
      go (x:xs) bs = let r, g :: [Repl]
                         (r, g) = partition (null . intersect (fst3 x) . fst3) xs
                         b :: Repl
                         b = argmax (length . fst3) g
                     in go r (b:bs)
      cmpRpls :: [Repl] -> [Repl] -> Bool
      cmpRpls = (==) `on` length
      newRepls = go repls []
  in if cmpRpls repls newRepls then repls else mkCompatible repls
  
getCmptbl :: [Repl] -> [Repl]
getCmptbl repls = 
  let replList :: Repl -> [(ObjectCluster,[Repl])]
      replList repl = zip (fst3 repl) (repeat [repl])
      replMap :: HashMap.HashMap ObjectCluster [Repl]
      replMap = HashMap.fromListWith (++) $ concatMap replList repls
      ocs :: [ObjectCluster]
      ocs = nub $ concatMap fst3 repls

      toGroups :: HashMap.HashMap Repl Int -> [[Repl]]
      toGroups = HashMap.elems . HashMap.fromListWith (++) 
               . map (second (replicate 1) . swap) . HashMap.toList

      fromMaybes :: [Maybe Int] -> Int -> Int
      fromMaybes []     currId = currId + 1
      fromMaybes (m:ms) currId = case m of Just i  -> i
                                           Nothing -> fromMaybes ms currId
      go :: [ObjectCluster] -> HashMap.HashMap Repl Int -> Int -> [[Repl]]
      go []     groupMap _      = toGroups groupMap
      go (o:os) groupMap lastId = 
        let repls :: [Repl]
            repls = fromJust $ HashMap.lookup o replMap
            groupId :: Int
            groupId = fromMaybes maybeInts lastId
            maybeInts :: [Maybe Int]
            maybeInts = map (flip HashMap.lookup groupMap) repls
            extra, newGroupMap :: HashMap.HashMap Repl Int
            extra = HashMap.fromList $ zip repls $ repeat groupId
            newGroupMap = HashMap.union groupMap extra
        in go os newGroupMap $ max lastId groupId

      groups :: [[Repl]]
      groups = go ocs HashMap.empty 0
  in map (argmax (length . fst3)) groups

                         

  
-- | True iff length xs >= n.
lenCmp n xs | null xs   = n == 0
            | n == 0    = True
            | otherwise = lenCmp (n - 1) (tail xs)

