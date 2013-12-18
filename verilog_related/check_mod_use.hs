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
            --print retVal
            --let r = map (flip catchError lintErrorHandler) retVal
            let r = runChecks v
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
lintErrorHandler (InvalidInstPortList name ports) = Left (InvalidInstPortList name ports)
lintErrorHandler (RecursiveModInst name ) = Left (RecursiveModInst name )

--printLintError :: Either LintError a -> IO ()
printLintError (Right _) = print ""
printLintError (Left le) = print le
--------------------------------------------------------------------------------
--runChecks :: Verilog -> Either LintError a
runChecks v = do 
            checkRecursiveModuleInst v
            checkPortDefUnique v
            checkPortDescription v
            `catchError` lintErrorHandler

--------------------------------------------------------------------------------
    -- Some Utility Functions
    --
-- Get List of ports of a Module / UDP
getPorts :: Description -> [Ident]
getPorts (ModuleDescription des) = modPorts des
getPorts (UDPDescription des) = (udpOutPort des) : (udpInPorts des)

getDescriptionName (ModuleDescription mod) = modName mod
getDescriptionName (UDPDescription udp) = udpName udp


getInstanceItem :: Item -> Maybe Instance
getInstanceItem (InstanceItem i) = Just i
getInstanceItem _ = Nothing

-- Get Instatiated Modules
getInstanceDeclarations :: Description -> [Instance]
getInstanceDeclarations (UDPDescription _ ) = []
getInstanceDeclarations (ModuleDescription mod) = catMaybes (map getInstanceItem body)  
  where body = (\(Module _ _ b) -> b) mod


-- Creates a map Name -> Description
createModuleMap :: Verilog -> Map.Map Ident Description 
createModuleMap (Verilog des) = Map.fromList modDescList
        where modDescList = zip moduleList des
              moduleList = map getDescriptionName des


getInstModuleListForDesc :: Description -> Maybe [Ident]
getInstModuleListForDesc ds =  (liftM listInstatiatedModules) (getModuleFromDesc ds)
  where getModuleFromDesc (ModuleDescription mod) = Just mod
        getModuleFromDesc (UDPDescription _) = Nothing

        listInstatiatedModules mod = instNames
         where  body = (\(Module _ _ b) -> b) mod
                insts = catMaybes (map getInstanceItem body)
                instNames = map (\(Instance x _ _) -> x) insts
--------------------------------------------------------------------------------

--
-- Return list of Modules / UDPs not used in verilog
getUnusedModules (Verilog des) = moduleList \\ modulesInstantiated
  where moduleList = map getDescriptionName des
        modulesInstantiated = nub (concat (catMaybes (map getInstModuleListForDesc des)))

 

-- Find recursive Module Instantiation - Uses Depth First Search
--checkRecursiveModuleInst :: Verilog -> Either[Bool]
checkRecursiveModuleInst ver = Data.Traversable.mapM (dfs_depth Map.empty) (map getMod unusedModList)
  where modDescMap  = createModuleMap ver
        getMod m = (modDescMap Map.! m)
        unusedModList = getUnusedModules ver

        dfs_depth _ (UDPDescription _) = Right True
        dfs_depth modFlagMap des =   if modTraversed 
                                        then throwError (RecursiveModInst (getDescriptionName des))
                                        else fromMaybe (Right True) retVal 
            where retVal = (liftM ( dfs_width modFlagMapNew)) (getInstModuleListForDesc des)
                  modTraversed = Map.member (getDescriptionName des) modFlagMap
                  modFlagMapNew = Map.insert (getDescriptionName des) True modFlagMap
                  

        dfs_width _ [] = Right True
        dfs_width modFlagMap (m:ms) = r evaluateDepth
            where evaluateDepth = dfs_depth modFlagMap (getMod m)
                  r (Right _) = dfs_width modFlagMap ms
                  r (Left le) = throwError le




-- Check Port definitions
-- Port names should be unique (
-- The type for ports should be defined.
-- input cannot be reg
--
checkPortDefUnique (Verilog ver) = Data.Traversable.mapM loop1 ver 
  where loop1 des = if  (length (nub ports)) == length(ports)
                    then Right True
                    else throwError (NonUniquePort desName dupPorts) 

            where   ports = getPorts des
                    dupPorts = nub (ports \\ (nub ports))
                    desName = getDescriptionName des
               


-- Checks port list of instantiated modules for invalid port name 
-- TODO:
checkPortDescription v@(Verilog des) = (Data.Traversable.sequence (map (loop1) allInsts))
  where allInsts = concat (map getInstanceDeclarations des)

        loop1 inst = Data.Traversable.sequence ((loop2) (fetchInstData inst))
            where   fetchInstData i = map (\(Inst x _ y) -> (x, y)) ((\(Instance _ _ x) -> x) i )

                    loop2 [] = []
                    loop2 (c:cs) = (loop2_2 (fst c) (snd c) ): (loop2 cs)
                    
                    modName = (\(Instance x _ _) -> x) inst
                    loop2_2 i (NamedConnections d) = checkPortExist v modName i d
                    loop2_2 _ (Connections _) = Right True -- This case can throw warnings if ports are unconnected

-- Check if the port list is valid for the given instance
-- This assumes the instName is valid
checkPortExist :: Verilog -> Ident -> Ident -> [NamedConnection] -> Either LintError Bool
checkPortExist _ _ _ [] = Right True
checkPortExist ver modName instName (port:portList) = 
                                    if (checkPort des portName) 
                                    then checkPortExist ver instName modName portList
                                    else throwError (InvalidInstPortList instName portName)
  where des             = modDescMap Map.! modName
        modDescMap      = createModuleMap ver
        portName        = (\(NamedConnection x _) -> x) port
        
        -- The port should exist in the module definition, 
        checkPort (ModuleDescription mod) p = elem p (modPorts mod)
        checkPort (UDPDescription udp) p = (p == (udpOutPort udp)) || (elem p (udpInPorts udp))

