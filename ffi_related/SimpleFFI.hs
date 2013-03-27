import Foreign
import Foreign.C.Types

foreign import ccall "math.h sin"
    c_sin :: CDouble -> CDouble

fastsin :: Double -> Double
fastsin x = realToFrac( c_sin (realToFrac x))


foreign import ccall "visitor.h"
    execute :: CInt -> CInt

executeVisitor :: Int -> Int
executeVisitor x = fromIntegral (execute (fromIntegral x) )

main = mapM_ (print.executeVisitor)[1, 2]
