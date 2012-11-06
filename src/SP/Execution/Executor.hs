-- | Execute operations.
module SP.Execution.Executor where

import SP.Cluster
import SP.Execution.Merger (merge)
import SP.Execution.Abstraction (abstract)
import SP.Execution.Isa (child, parent)
import SP.Execution.Update
import SP.Execution.Redirect
import SP.Scoring.Score

-- | Execute a set of scores on a partition of object clusters.
execute :: [ObjCluster] -> [Score] -> [ObjCluster]
execute clusters = redirect clusters . foldUpdates . map scoreToUpdate

-- | Create an update instruction given a operator score.
scoreToUpdate :: Score -> Update
scoreToUpdate score = case scoreOp score of
    Merge -> merge score
    Abstract -> abstract score
    Child -> child score
    Parent -> parent score
