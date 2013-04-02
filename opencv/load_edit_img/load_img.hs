import Foreign
import Foreign.C.Types

foreign import ccall "cv.h imread"
    c_imread :: CString -> CInt -> ()


