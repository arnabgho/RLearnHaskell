module Table where
import qualified Data.Map as Map

data Table state action = Table {
  table :: Map.Map (state , action ) Double
}

-- getTable :: Table state action -> Reader (Table state action) 