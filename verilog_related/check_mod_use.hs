-- create a path to Language folder of verilog repository
-- Load this file in ghci 
-- use 'test "file.v"' to check the parser on verilog file
-- do 'checkRun "file.v"' to run the checks on verilog file
--------------------------------------------------------------------------------
import Prelude hiding (catch, lookup)

import Text.PrettyPrint             (render)
import Text.Parsec                  (parse)
import Text.Parsec.ByteString       (parseFromFile)

import Language.Verilog.Parser
import Language.Verilog.PrettyPrint 
import Language.Verilog.Syntax      
import Language.Verilog.Syntax.AST      
import Control.Monad
import Data.Maybe
import Data.List
import Data.Map
--------------------------------------------------------------------------------

test :: FilePath -> IO ()
test fp
  = do x <- run fp
       putStrLn (render (ppVerilog x))

run :: FilePath -> IO Verilog
run fp
  = do x <- parseFromFile verilogFile fp
       case x of
         Left err  -> error (show err)
         Right y   -> return y

checkRun fp = do 
            v <- run fp
            let retVal = getUnusedModules v
            print retVal
            return retVal

-- Return list of Modules / UDPs not used in verilog
getUnusedModules v = (getModuleList v) \\ (getInstModuleList v)
        
-- Return list of all Modules / UDPs in verilog
getModuleList :: Verilog -> [Ident]
getModuleList (Verilog v) = map getDescriptionName v

getDescriptionName (ModuleDescription mod) = modName mod
getDescriptionName (UDPDescription udp) = udpName udp

-- Returns list of Modules being instantiated in Verilog v
getInstModuleList (Verilog v) = nub (map fromJust (concat (catMaybes (map getInstModuleListForDesc v))))
getInstModuleListForDesc ds = (getModuleFromDesc ds) >>= listInstatiatedModules

getModuleFromDesc :: Description -> Maybe Module
getModuleFromDesc (ModuleDescription mod) = Just mod
getModuleFromDesc (UDPDescription _) = Nothing

listInstatiatedModules mod = return (insts)
    where body = (\(Module _ _ b) -> b) mod
          insts = map getInstanceItemName body

getInstanceItemName (InstanceItem i) = Just ((\(Instance x _ _) -> x ) i)
getInstanceItemName _ = Nothing


checkRecursiveModuleInst

traverse_DFS :: Description -> Bool
traverse_DFS (UDPDescription _) = true
traverse_DFS (ModuleDescription mod) = (getInstModuleListForDesc mod) >>= liftM recursive_traverse_DFS

recursive_traverse_DFS :: [Module] -> Bool
recursive_traverse_DFS [] = true
recursive_traverse_DFS [m:ms] = if (traverse_DFS ) then false else recursive_traverse_DFS ms


