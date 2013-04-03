import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.ByteString
import qualified Data.ByteString.Char8 as C

foreign import ccall "math.h sin"
    c_sin :: CDouble -> CDouble

fastsin :: Double -> Double
fastsin x = realToFrac( c_sin (realToFrac x))


foreign import ccall "visitor.h"
    execute :: CInt -> CString -> IO Int

executeVisitor :: Int -> ByteString -> Int
executeVisitor x message = unsafePerformIO ( useAsCString message (\message_1 -> execute (fromIntegral x) message_1  ))

main = (print (executeVisitor 1 myStr))
        where myStr = C.pack "hello"
