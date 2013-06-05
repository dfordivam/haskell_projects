--------------------------------------------------------------------------------
import Prelude hiding (catch)

import Text.PrettyPrint             (render)
import Text.Parsec                  (parse)
import Text.Parsec.ByteString       (parseFromFile)

import Language.Verilog.Parser
import Language.Verilog.PrettyPrint (ppVerilog)
import Language.Verilog.Syntax      
import Language.Verilog.Syntax.AST      

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
            return retVal

checkModUse :: Verilog -> [Ident]

checkModUse (Verilog v) = unused_mod_list
   where unused_mod_list = map getDescName v

getDescName (ModuleDescription mod) = modName mod
getDescName (UDPDescription udp) = udpName udp

listInstatiatedModules :: Module -> Maybe [Ident]
listInstatiatedModules mod = return (insts)
    where body = (\(Module _ _ b) -> b) mod
          insts = map getInstanceName body

getInstanceName :: Item -> Ident
getInstanceName (InstanceItem i) = (\(Instance x _ _) -> x ) i
getInstanceName _ = Ident "dummy"

getModule :: Description -> Maybe Module
getModule (ModuleDescription mod) = Just mod
getModule (UDPDescription _) = Nothing

--checkModInst :: Verilog -> [Maybe Module]
checkModInst (Verilog v) = (map checkModInstReq v)
checkModInstReq ds = (getModule ds) >>? listInstatiatedModules

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v 

