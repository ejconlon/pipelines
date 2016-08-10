import Pipelines
import Test.Tasty
import Test.Tasty.HUnit

simplePlan :: Plan
simplePlan = Plan
  { _planName = "simplePlan"
  , _planTasks =
      [ Task "taskA" "actionA" 1
      , Task "taskB" "actionB" 2
      , Task "taskC" "actionC" 3
      ]
  , _planLoop = StopLoop
  }

tests :: TestTree
tests = testGroup "tests"
  [
  ]

main :: IO ()
main = defaultMain tests
