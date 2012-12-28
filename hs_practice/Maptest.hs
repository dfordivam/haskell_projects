import Prelude
import qualified Data.Map as Map

type Student = String
type Marks = Int
type StudentList = [String]

data MyContainer = MyContainer {
    marks :: Map.Map Student Marks
    , students :: StudentList
} deriving (Show, Eq)


addMarks mc student mark = updated_mc
    where   mks = marks mc
            updated_mc = MyContainer (Map.insert student mark mks) (students mc)
