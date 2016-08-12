module Pipelines.Types where

import           Data.Aeson       ((.:), (.=))
import qualified Data.Aeson       as A
import qualified Data.Aeson.Types as A
import qualified Data.Text        as T
import           List.Transformer

-- | A stringy identifier for a Plan or a Task
type Name = T.Text

-- | A duration for which we can set Task timeouts
type Interval = Int

-- | A stringy identifier for a Task's action in the world
type Action = T.Text

-- | A named unit of work
data Task = Task
  { _taskName    :: Name
  , _taskAction  :: Action
  , _taskTimeout :: Interval
  } deriving (Show, Eq)

instance A.FromJSON Task where
  parseJSON (A.Object m) =
    Task <$>
      m .: "name"    <*>
      m .: "action"  <*>
      m .: "timeout"
  parseJSON invalid = A.typeMismatch "Task" invalid

instance A.ToJSON Task where
  toJSON (Task name action timeout) = A.object
    [ "name" .= name
    , "action" .= action
    , "timeout" .= timeout
    ]

-- | Indicates whether a Plan should loop forever or stop
data Loop
  = StopLoop
  | ContinueLoop
  deriving (Show, Eq)

instance A.FromJSON Loop where
  parseJSON (A.String t) =
    case t of
      "stop" -> return StopLoop
      "continue" -> return ContinueLoop
      _ -> fail ("invalid Loop " ++ T.unpack t)
  parseJSON invalid = A.typeMismatch "Loop" invalid

instance A.ToJSON Loop where
  toJSON StopLoop = A.String "stop"
  toJSON ContinueLoop = A.String "continue"

-- | A sequence of tasks
data Plan = Plan
  { _planName  :: Name
  , _planTasks :: [Task]
  , _planLoop  :: Loop
  } deriving (Show, Eq)

instance A.FromJSON Plan where
  parseJSON (A.Object m) =
    Plan <$>
      m .: "name"  <*>
      m .: "tasks" <*>
      m .: "loop"
  parseJSON invalid = A.typeMismatch "Plan" invalid

instance A.ToJSON Plan where
  toJSON (Plan name tasks loop) = A.object
    [ "name" .= name
    , "tasks" .= tasks
    , "loop" .= loop
    ]

-- | The Result of running a Task
-- TODO(eric) possibly a retry state
data Result = OkResult | FailResult deriving (Show, Eq)

instance A.FromJSON Result where
  parseJSON (A.String t) =
    case t of
      "ok" -> return OkResult
      "fail" -> return FailResult
      _ -> fail ("invalid Result " ++ T.unpack t)
  parseJSON invalid = A.typeMismatch "Result" invalid

instance A.ToJSON Result where
  toJSON OkResult = A.String "ok"
  toJSON FailResult = A.String "fail"

data Watch b c = Watch
  { _watchEvents :: ListT b c
  , _watchStop   :: b ()
  }

-- TODO is <|> fair?
instance Monad b => Monoid (Watch b c) where
  mempty = Watch (ListT (return Nil)) (return ())
  mappend (Watch e1 s1) (Watch e2 s2) = Watch (e1 <|> e2) (s1 >> s2)
