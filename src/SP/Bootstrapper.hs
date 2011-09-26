{-# LANGUAGE OverloadedStrings #-}
module SP.Bootstrapper (bootstrap) where

import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Lazy as M
import Data.List (elemIndex)
import Data.Maybe
import Database.MongoDB
import SP.Cluster

-- | Get the news items from Mongo, where cb is the callback function that 
-- | will be called upon once the domain has been created.
bootstrap cb = do
  pipe <- runIOE $ connect (host "127.0.0.1")
  e <- access pipe master "articles" (reutersMergers >>= mkDomain cb)
  close pipe
  print e

-- | Find Reuters Mergers news items.
reutersMergers = rest =<< find (select q "reuters.mergers")
  where 
    q = ["status" =: u"ok", "sentences" =: ["$exists" =: True]]

mkDomain cb dArts = liftIO $ cb arts fObjClrs
  where
    arts = zipWith mkArt (take 10 dArts) [1..]
    parts = concatMap sParts $ concatMap aSnts arts
    objClrMap = M.fromList $ zip parts objClrs 
    objClrs = zipWith (mkObjClr objClrMap) parts [1..]
    fObjClrs = filter (not . null . ocArgClrs) objClrs
   
mkArt :: Document -> Int -> Art 
mkArt dArt aId = art
  where 
    art = Art aId parts aTitle aText
    parts = zipWith (mkSnt art) dSnts [1..]
    aTitle = dStr "title" dArt
    aText = dStr "content" dArt
    dSnts = dLst "sentences" dArt

mkSnt :: Art -> Document -> Int -> Snt
mkSnt sArt dSnt sId = snt
  where
    snt = Snt sId sArt parts
    tks = dLst "tokens" dSnt
    sntDeps = map (mkDep parts) $ dLst "deps" dSnt
    parts = zipWith (mkPart snt sntDeps) tks [1..]

mkDep :: [Part] -> Document ->  Dep
mkDep parts doc = Dep rel gov dpt
  where
    rel = dStr "rel" doc
    govIdx = dInt "gov_index" doc
    dptIdx = dInt "dept_index" doc
    gov = parts !! govIdx
    dpt = parts !! dptIdx

mkPart :: Snt -> [Dep] -> Document -> Int -> Part
mkPart pSnt sntDeps tk pId = part
  where
    part = Part pId pSnt form lemma pos word parDeps chdDeps 
    pos = dStr "pos" tk
    lemma = dStr "lemma" tk
    word = dStr "text" tk
    form = BS.concat [pos, pack ":", lemma]
    parDeps = filter (\p -> dpt p == part) sntDeps
    chdDeps = filter (\p -> gov p == part) sntDeps

-- Retrieve typed values from documents.
dStr :: Label -> Document -> ByteString
dStr lbl doc = pack $ typed $ valueAt lbl doc
dLst :: Label -> Document -> [Document]
dLst lbl doc = typed $ valueAt lbl doc
dInt :: Label -> Document -> Int
dInt lbl doc = typed $ valueAt lbl doc

mkObjClr :: M.HashMap Part ObjClr -> Part -> Int -> ObjClr
mkObjClr objClrMap part ocId = oc
  where
    oc = ObjClr ocId [part] parArgClrs chdArgClrs sblArgClrs 
    parArgClrs = [AdjArgClr id oc [arg] Par | (arg, id) <- zip parArgs [1..]]
    chdArgClrs = [AdjArgClr id oc [arg] Chd | (arg, id) <- zip chdArgs [1..]]
    sblArgClrs = [RmtArgClr id oc [arg] Sbl | (arg, id) <- zip sblArgs [1..]]
    parArgs = [AdjArg rel (toOC gov) | (Dep rel gov _) <- parDeps part ]
    chdArgs = [AdjArg rel (toOC dep) | (Dep rel _ dep) <- chdDeps part]
    sblArgs = [RmtArg (dRel pd) (dRel cd) (toOC $ gov cd) (toOC rmtPart) 
              | (pd,cd) <- sblDPs, let rmtPart = dpt pd, rmtPart /= part]
    sblDPs = concat [[(pd,cd) | cd <- chdDeps $ gov pd] | pd <- parDeps part]
    toOC p = fromJust $ M.lookup p objClrMap
