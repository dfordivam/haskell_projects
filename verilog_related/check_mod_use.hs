-- create a path to Language folder of verilog repository
-- Load this file in ghci 
-- use 'test "file.v"' to check the parser on verilog file
-- do 'checkRun "file.v"' to run the checks on verilog file
--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
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
import Control.Monad.Error
import Data.Maybe
import Data.List
import Data.Traversable
import qualified Data.Map.Lazy as Map
--------------------------------------------------------------------------------

main = checkRun "test.v"

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
            --let retVal = createModuleMap v
            --let loopInfo = checkRecursiveModuleInst v  
            --let retVal = map (flip (catchError lintErrorHandler) ) (checkPortDefs v  )
            --let retVal = (checkPortDefs v  )
            --print retVal
            --let r = map (flip catchError lintErrorHandler) retVal
            let r = checkPortDescription v
            --printLintError r
            return (r)

--------------------------------------------------------------------------------
data LintError = 
        NonUniquePort Ident [Ident]
     |  RecursiveModInst Ident
     |  InvalidInstPortList Ident Ident
     |  OtherError String
     deriving Show

instance Error LintError where
  strMsg = OtherError
--  throwError = Left
--  catchError (Right val) _ = Right val
--  catchError (Left err)  h = h err

--lintErrorHandler :: LintError -> Either LintError Bool
lintErrorHandler (NonUniquePort name ports) = Left (NonUniquePort name ports)

printLintError :: Either LintError a -> IO ()
printLintError (Right _) = print ""
printLintError (Left le) = print le
--------------------------------------------------------------------------------
--runChecks :: Verilog -> Either LintError a
runChecks v = do 
            checkPortDefs v
            --checkPortDescription v
            `catchError` lintErrorHandler

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
listInstatiatedModules mod = instNames
    where body = (\(Module _ _ b) -> b) mod
          insts = catMaybes (map getInstanceItem body)
          instNames = map (\(Instance x _ _) -> x) insts 

getInstanceItem :: Item -> Maybe Instance
getInstanceItem (InstanceItem i) = Just i
getInstanceItem _ = Nothing

-- Find recursive Module Instantiation - Uses Depth First Search
checkRecursiveModuleInst :: Verilog -> [Bool]
checkRecursiveModuleInst ver = map (traverse_DFS modDescMap Map.empty) (map (modDescMap Map.!) unusedModList)
    where   modDescMap  = createModuleMap ver
            unusedModList = getUnusedModules ver
        

traverse_DFS :: Map.Map Ident Description -> Map.Map Ident Bool -> Description -> Bool
traverse_DFS _ _ (UDPDescription _) = False
traverse_DFS modDescMap modFlagMap des@(ModuleDescription mod) =    if modTraversed 
                                                                    then True 
                                                                    else fromMaybe False retVal 
    where retVal = (liftM (  recursive_traverse_DFS modDescMap modFlagMapNew)) (getInstModuleListForDesc des)
          modTraversed = Map.member (getDescriptionName des) modFlagMap
          modFlagMapNew = Map.insert (getDescriptionName des) True modFlagMap


recursive_traverse_DFS :: Map.Map Ident Description -> Map.Map Ident Bool -> [Ident] -> Bool
recursive_traverse_DFS _ _ [] = False
recursive_traverse_DFS modDescMap modFlagMap (m:ms) =   if (traverse_DFS modDescMap modFlagMap (modDescMap Map.! m )) 
                                                        then True 
                                                        else recursive_traverse_DFS modDescMap modFlagMap ms


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
checkPortDefs (Verilog ver) = Data.Traversable.mapM checkPortDefUniqueTop ver 

checkPortDefUniqueTop :: Description -> Either LintError Bool
checkPortDefUniqueTop des = if  (length (nub ports)) == length(ports)
                            then Right True
                            else throwError (NonUniquePort desName dupPorts) 
                    where ports = getPorts des
                          getPorts (ModuleDescription des) = modPorts des
                          getPorts (UDPDescription des) = (udpOutPort des) : (udpInPorts des)
                          dupPorts = nub (ports \\ (nub ports))
                          desName = getDescriptionName des
               
checkPortDefUnique :: [Ident] -> Bool
checkPortDefUnique ports = (length (nub ports)) == length(ports)

-- The port should exist in the module definition, 
checkPort :: Description -> NamedConnection -> Bool
checkPort (ModuleDescription mod) (NamedConnection port _) = elem port (modPorts mod)
checkPort (UDPDescription udp) (NamedConnection port _) = (port == (udpOutPort udp)) || (elem port (udpInPorts udp))


checkInstancePortValid :: Verilog -> Module -> Bool
checkInstancePortValid ver mod = True
  where insts = ver
        getInstList (InstanceItem i) = Just ((\(Instance x _ y) -> (x, y) ) i)
        getInstList _       = Nothing

-- Check if the port list is valid for the given instance
-- This assumes the instName is valid
checkPortExist :: Verilog -> Ident -> Ident -> [NamedConnection] -> Either LintError Bool
checkPortExist _ _ _ [] = Right True
checkPortExist ver modName instName (port:portList) = 
                                    if (checkPort des port) 
                                    then checkPortExist ver instName modName portList
                                    else throwError (InvalidInstPortList instName portName)
  where des             = modDescMap Map.! modName
        modDescMap      = createModuleMap ver
        portName        = (\(NamedConnection x _) -> x) port


--checkPortDescription :: Verilog ->c
checkPortDescription v@(Verilog des) = concat (map (getImpData3 v) allInsts)
  where allInsts = concat (map getInstanceDeclarations des)

getInstNameAndPortConn =  (\(Inst x _ y) -> (x,y))
--                        
--getImpData:: Instance -> [(Ident, NamedConnection)]
getImpData (Instance _ _ instList) = map getInstNameAndPortConn instList
--getImpData3 :: Verilog -> Instance -> []
getImpData3 v inst@(Instance modName _ _) =  (getImpData5 v modName) moduleInstancesList
  where moduleInstancesList = getImpData inst

getImpData5 _ _ [] = []
getImpData5 v m (c:cs) = (getImpData4 v m (fst c) (snd c) ): (getImpData5 v m cs)
getImpData4 v m i (NamedConnections d) = checkPortExist v m i d
getImpData4 v m i (Connections d) = Right True

getInstanceDeclarations (UDPDescription _ ) = []
getInstanceDeclarations (ModuleDescription mod) = catMaybes (map getInstanceItem body)  
  where body = (\(Module _ _ b) -> b) mod

--listInstatiatedModules
