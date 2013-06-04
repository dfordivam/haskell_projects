--------------------------------------------------------------------------------
import Prelude hiding (catch)

import Text.PrettyPrint             (render)
import Text.Parsec                  (parse)
import Text.Parsec.ByteString       (parseFromFile)

import Language.Verilog.Parser
import Language.Verilog.PrettyPrint (ppVerilog)
import Language.Verilog.Syntax      (Verilog)

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
            let retVal = checkModUse v
            return retVal

checkModUse :: Verilog -> [String]

checkModUse v = unused_mod_list
   where unused_mod_list = ["hello"]         
