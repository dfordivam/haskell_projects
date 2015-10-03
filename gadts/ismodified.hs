module Main where

main = do
  let a1 = AStmt 1 2
  if isChanged a1 then putStrLn "Changed" else putStrLn "No"

-- t1 Eq
data AssignmentStmt t1 = AStmt{ lhs::t1, rhs::t1}

data ConditionalStmt t1 t2 = CStmt {lhs1::t1, rhs1::t1, lhs2::t2, rhs2::t2}


-- x = x + 1 -> true, if (cond) x = z + 1; else y = y - 1 -> true

--class Modified a where
--  isModified :: a -> Bool
--
--instance Modified (AssignmentStmt t1) where
--  isModified s = lhs s == rhs s

--instance Modified (ConditionalStmt t1 t2) where
--  isModified s = lhs1 s == rhs1 s || lhs2 s == rhs2 s

isChanged :: Eq a => AssignmentStmt a -> Bool
isChanged s = lhs s == rhs s

type IntAssignment = AssignmentStmt Int

