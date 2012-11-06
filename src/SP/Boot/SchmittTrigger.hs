-- | A Schmitt Trigger to be used by the main learning iterations.
module SP.Boot.SchmittTrigger where

import SP.Config

-- | The Schmitt Trigger.
data Trigger = Trigger State Double Double

-- | A trigger state.
data State = High | Low

-- | Update the state of a trigger.
updateState :: Double -> Trigger -> Trigger
updateState input (Trigger _ high low) = Trigger state high low
  where
    state = if input >= high then High else Low

-- | True if the trigger switched from high to low
lowFlankPulse :: Trigger    -- ^ Old trigger. 
              -> Trigger    -- ^ New trigger
              -> Bool       -- ^ True for low flank pulse.
lowFlankPulse (Trigger High _ _) (Trigger Low _ _) = True
lowFlankPulse _                  _                 = False

-- | Create a new Schmitt Trigger for merging same-lemma clusters.
lemmaIterTrigger :: IO Trigger
lemmaIterTrigger = do
    config <- getConfig
    return $ Trigger Low (lemmaIterHt config) (lemmaIterLt config)
    
-- | Create a new Schmitt Trigger for same-POS tag category clusters. 
posCatIterTrigger :: IO Trigger
posCatIterTrigger = do
    config <- getConfig
    return $ Trigger Low (posCatIterHt config) (posCatIterLt config)
    