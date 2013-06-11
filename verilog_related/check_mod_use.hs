--------------------------------------------------------------------------------
import Prelude hiding (catch)

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

listInstatiatedModules mod = return (insts)
    where body = (\(Module _ _ b) -> b) mod
          insts = map getInstanceItemName body

getInstanceItemName (InstanceItem i) = Just ((\(Instance x _ _) -> x ) i)
getInstanceItemName _ = Nothing

getModuleFromDesc :: Description -> Maybe Module
getModuleFromDesc (ModuleDescription mod) = Just mod
getModuleFromDesc (UDPDescription _) = Nothing
