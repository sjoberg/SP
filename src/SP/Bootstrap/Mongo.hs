module SP.Bootstrap.Mongo where


-- | Get the news items from Mongo. callback is the callback function.
bootstrap callback = do                                   
  pipe <- runIOE $ connect (Host "127.0.0.1" $ PortNumber 27017)
  config <- getConfig
  let order :: Order
      order = [u"generated" =: (1 :: Int)]
      collection :: Collection
      collection = u"reuters.mergers"
      idx :: Index
      idx = index collection order
      selector :: Selector
      selector = [u"status" =: u"ok", u"sentences" =: [u"$exists" =: True]]
      get = rest =<< find (select selector collection) {sort = order}
      
  e <- access pipe master (u"articles") (get >>= makeDomain config callback)
  close pipe
  print e

-- Retrieve typed values from documents.                                        
docStr :: String -> Document -> ByteString                                         
docStr lbl doc = pack $ typed $ valueAt (u lbl) doc                                   

docLst :: String -> Document -> [Document]                                         
docLst lbl doc = typed $ valueAt (u lbl) doc                                          

docInt :: String -> Document -> Int                                                
docInt lbl doc = typed $ valueAt (u lbl) doc

-- | Make the domain.
makeDomain :: Configuration -> (([ObjectCluster], [ArgumentCluster]) -> IO ())
           -> [Document] -> IO ()
makeDomain config callback arts = let limit = take $ artSize config
                                      run = callback . makeClusters config
                                  in liftIO . run . limit $ arts

-- | Make clusters in the domain.
makeClusters :: Configuration -> [Document] -> Domain
makeClusters config arts = concatMap 

:: ([a], [b])

procDocument :: Configuration -> Document -> Domain
procDocument config = 

type Domain = ([ObjectCluster], [ArgumentCluster])

