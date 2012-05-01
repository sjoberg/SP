-- {-# LANGUAGE RecordWildCards, StandaloneDeriving, OverloadedStrings, FlexibleContexts, TupleSections, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module SP.Bootstrap.MongoDB (bootstrap) where

import Control.DeepSeq
import Control.Monad.Trans (liftIO)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Database.MongoDB
import Data.Bson
import Data.Bson.Binary
import Data.Array.IArray hiding (elems, index)
import Data.Function (on)
import qualified Data.HashMap.Lazy as HM
import Data.IntMap (keys, findWithDefault, singleton)
import Data.List (foldl', nub)
import Data.Maybe
import Data.UString (u)
import SP.ByteString as B
import SP.Cluster
import SP.Config
import SP.DeepSeq

-- | Get the news items from Mongo. c is the callback function.
bootstrap c = do                                   
  pipe <- runIOE $ connect (Host "127.0.0.1" $ PortNumber 27017)
  cfg <- getConfig
  let order :: Order
      order = [u"generated" =: (1 :: Int)]
      col :: Collection
      col = u"reuters.mergers"
      idx :: Index
      idx = index col order
      selector :: Selector
      selector = [u"status" =: u"ok", u"sentences" =: [u"$exists" =: True]]
      get = rest =<< find (select selector col) {sort = order}
      
  e <- access pipe master (u"articles") (get >>= mkDomain cfg c)
  close pipe
  print e

-- | Find news items.                                           
--findItems = rest =<< find (select q $ u"reuters.mergers")
--  where q = [u"status" =: u"ok", u"sentences" =: [u"$exists" =: True]]

-- Retrieve typed values from documents.                                        
docStr :: String -> Document -> ByteString                                         
docStr lbl doc = pack $ typed $ valueAt (u lbl) doc                                   
docLst :: String -> Document -> [Document]                                         
docLst lbl doc = typed $ valueAt (u lbl) doc                                          
docInt :: String -> Document -> Int                                                
docInt lbl doc = typed $ valueAt (u lbl) doc

mkDomain cfg c arts = liftIO $ c $ mkPtns cfg narts
  where narts = take (artSize cfg) arts -- Limit no. of articles.

mkPtns cfg arts = reverse $ go [] arts 0 0 0
  where
  -- Ignored dependencies and POS tags
  ignDeps = depIgn cfg; ignPos = posIgn cfg

  -- Build the partitions using the worker go.
  go :: [Partition] -> [Document] -> Int -> Int -> Int -> [Partition]
  go ps []          _  _  _  = ps
  go ps (art:narts) pi oi ai = go (p:ps) narts npi noi nai
    where
    -- Partition
    p = Partition pi ocs acs

    -- Object clusters and argument clusters.
    ocs = fst.fst $ osAsOiAi
    acs = nub $ concatMap (map fst.pars) ocs  --snd.fst $ osAsOiAi

    -- New start IDs for the next batch of argument and object clusters.
    npi = 1 + pi -- Next partition index.
    noi = fst.snd $ osAsOiAi -- Next object cluster index.
    nai = snd.snd $ osAsOiAi -- Next argument cluster index.

    -- The object clusters and id of the last argument cluster.
    osAsOiAi :: (([ObjectCluster],[ArgumentCluster]),(Int,Int))
    osAsOiAi = procSnts (docLst "sentences" art) [] [] 0 oi ai

    -- Create argument and object clusters for sentences.
    procSnts :: [Document] -> [ObjectCluster] -> [ArgumentCluster] -> Int -> 
                Int -> Int -> (([ObjectCluster],[ArgumentCluster]),(Int,Int))
    procSnts []     os as _  oi ai = ((os,acs),(oi,ai))
    procSnts (s:ss) os as si oi ai = procSnts ss nos nas (si+1) noi nai
      where
      -- Next argument cluster index.
      nai = ai + length nas
      noi = oi + ufoslen
      -- New argument clusters.
      nas = nub . foldl' (++) [] . HM.elems $ pm
      
      -- Object clusters --with argument clusters.
      nos = os ++ ufos --(HM.keys pm `union` HM.keys cm)
      
      -- Maps from object clusters to 
      pm = HM.fromListWith (++) $ fst ats
      cm = HM.fromListWith (++) $ snd ats

      -- Create argument clusters.
      deps = filter (\d -> docStr "rel" d `notElem` ignDeps) (docLst "deps" s)
      ats = unzip $ zipWith mkArgumentCluster [ai..] deps
      mkArgumentCluster id dep = ((p,[ac]),(c,[ac]))
        where ac = ArgumentCluster id pm cm rm (fromIntegral $ HM.size rm)
              pm = singleton (ocId p) 1; p = oa ! docInt "gov_index" dep
              cm = singleton (ocId c) 1; c = oa ! docInt "dept_index" dep
              rm = HM.singleton (docStr "rel" dep) 1

      -- Unfiltered object clusters, array for faster indexing.
      oa :: Array Int ObjectCluster
      oa = listArray (0, ufoslen - 1) ufos
      ufoslen = length ufos
      ufos = zipWith mkObjectCluster [oi..] (docLst "tokens" s)
      mkObjectCluster id tk = o
        where 
        o = ObjectCluster id [part] pars chdn sbls 1 
        part = Part {partId = id,  artId = pi,       sntId = si,   form = form,  
                     pos = g"pos", lemma = g"lemma", ner = g"ner", text = g"text"}
          where form = B.concat [g"lemma", pack ":", g"pos"]
                g str = docStr str tk

        -- Incidence lists for argument clusters.
        pars = tpls $ HM.lookupDefault [] o cm -- Parents
        chdn = tpls $ HM.lookupDefault [] o pm -- Children
        sbls = concatMap acChdn pars           -- Siblings
        -- TODO Check that siblings uses the right map.
        acChdn pi = [(D2ArgumentCluster par par, 1) | -- Incidence
                     let par = fst pi
                         pidx = (head.keys.parMap) par,
                     chd <- HM.lookupDefault [] ObjectCluster {ocId = pidx} pm,
                     ocId o /= (head.keys.chdMap) chd] -- Don't point to o
        tpls = map $ \e -> (e,1)
