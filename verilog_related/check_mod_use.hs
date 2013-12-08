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
import qualified Data.Map.Lazy as Map
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
            let retVal = createModuleMap v
            --let loopInfo = checkRecursiveModuleInst v  
            let loopInfo = checkPortDefs v  
            --print retVal
            return loopInfo

-- Return list of Modules / UDPs not used in verilog
getUnusedModules v = (getModuleList v) \\ (getInstModuleList v)
        
-- Return list of all Modules / UDPs in verilog
getModuleList :: Verilog -> [Ident]
getModuleList (Verilog v) = map getDescriptionName v

getDescriptionName (ModuleDescription mod) = modName mod
getDescriptionName (UDPDescription udp) = udpName udp

-- Returns list of Modules being instantiated in Verilog v
getInstModuleList :: Verilog -> [Ident]
getInstModuleList (Verilog v) = nub (concat (catMaybes (map getInstModuleListForDesc v)))

getInstModuleListForDesc :: Description -> Maybe [Ident]
getInstModuleListForDesc ds =  (liftM listInstatiatedModules) (getModuleFromDesc ds)

getModuleFromDesc :: Description -> Maybe Module
getModuleFromDesc (ModuleDescription mod) = Just mod
getModuleFromDesc (UDPDescription _) = Nothing

listInstatiatedModules :: Module -> [Ident]
listInstatiatedModules mod = insts
    where body = (\(Module _ _ b) -> b) mod
          insts = catMaybes (map getInstanceItemName body)

getInstanceItemName :: Item -> Maybe Ident
getInstanceItemName (InstanceItem i) = Just ((\(Instance x _ _) -> x ) i)
getInstanceItemName _ = Nothing

-- Find recursive Module Instantiation - Uses Depth First Search
checkRecursiveModuleInst :: Verilog -> [Bool]
checkRecursiveModuleInst ver = map (traverse_DFS modDescMap Map.empty) (map (modDescMap Map.!) unusedModList)
    where   modDescMap  = createModuleMap ver
            unusedModList = getUnusedModules ver
        

traverse_DFS :: Map.Map Ident Description -> Map.Map Ident Bool -> Description -> Bool
traverse_DFS _ _ (UDPDescription _) = False
traverse_DFS modDescMap modFlagMap des@(ModuleDescription mod) = if modTraversed then True else fromMaybe False retVal 
    where retVal = (liftM (  recursive_traverse_DFS modDescMap modFlagMapNew)) (getInstModuleListForDesc des)
          modTraversed = Map.member (getDescriptionName des) modFlagMap
          modFlagMapNew = Map.insert (getDescriptionName des) True modFlagMap


recursive_traverse_DFS :: Map.Map Ident Description -> Map.Map Ident Bool -> [Ident] -> Bool
recursive_traverse_DFS _ _ [] = False
recursive_traverse_DFS modDescMap modFlagMap (m:ms) = if (traverse_DFS modDescMap modFlagMap (modDescMap Map.! m )) then True else recursive_traverse_DFS modDescMap modFlagMap ms

-- Creates a map Name -> Description
createModuleMap :: Verilog -> Map.Map Ident Description 
createModuleMap ver@(Verilog v) = Map.fromList modDescList
        where modDescList = zip (getModuleList ver) v


-- Check Port definitions
-- Port names should be unique
-- The type for ports should be defined.
-- input cannot be reg
--
--checkPortDefs :: Verilog -> [Bool]
checkPortDefs (Verilog ver) = map checkPortDefUniqueTop ver 

checkPortDefUniqueTop :: Description -> Maybe Bool
checkPortDefUniqueTop des = do 
                        ports <- (liftM modPorts) (getModuleFromDesc des) 
                        return (checkPortDefUnique ports)
               
checkPortDefUnique :: [Ident] -> Bool
checkPortDefUnique ports = (length (nub ports)) == length(ports)

-- The port should exist in the module definition, 
checkPort :: Description -> Ident -> Bool
checkPort (ModuleDescription mod) port = elem port (modPorts mod)
checkPort (UDPDescription udp) port = (port == (udpOutPort udp)) || (elem port (udpInPorts udp))


checkInstancePortValid :: Verilog -> Module -> Bool
checkInstancePortValid ver mod = True
  where insts = ver
        getInstList (InstanceItem i) = Just ((\(Instance x _ y) -> (x, y) ) i)
        getInstList _       = Nothing

-- Check if the port list is valid for the given instance
-- This assumes the instName is valid
checkPortExist :: Verilog -> Ident -> [Ident] -> Bool
checkPortExist _ _ [] = True
checkPortExist ver modName (port:portList) = 
                                    if (checkPort des port) 
                                    then checkPortExist ver modName portList
                                    else False
  where des             = modDescMap Map.! modName
        modDescMap      = createModuleMap ver





--listInstatiatedModules
