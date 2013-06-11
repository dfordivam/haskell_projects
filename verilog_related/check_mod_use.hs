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
            let retVal = checkModInst v
            print retVal
            return retVal

checkModUse :: Verilog -> [Ident]

checkModUse (Verilog v) = unused_mod_list
   where unused_mod_list = map getDescName v

getDescName (ModuleDescription mod) = modName mod
getDescName (UDPDescription udp) = udpName udp

listInstatiatedModules mod = return (insts)
    where body = (\(Module _ _ b) -> b) mod
          insts = map getInstanceItemName body

getInstanceItemName (InstanceItem i) = Just ((\(Instance x _ _) -> x ) i)
getInstanceItemName _ = Nothing

getModule :: Description -> Maybe Module
getModule (ModuleDescription mod) = Just mod
getModule (UDPDescription _) = Nothing

-- Returns list of Modules being instantiated in Verilog v
checkModInst (Verilog v) = nub (map fromJust (concat (catMaybes (map checkModInstReq v))))
checkModInstReq ds = (getModule ds) >>= listInstatiatedModules

