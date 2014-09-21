{-# LANGUAGE ForeignFunctionInterface #-} 
module Codec.Image.DDS( CompressionType(..)
                      , withDecompressed
                      , ddsDecompressDXT5
                      , ddsDecompressDXT3
                      , ddsDecompressDXT1
                      ) where

import Data.Word
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

foreign import ccall safe "ddslib.h DDSDecompressDXT5"
  ddsDecompressDXT5 :: Ptr Word8 -> Int -> Int -> Ptr Word8 -> IO ()

foreign import ccall safe "ddslib.h DDSDecompressDXT3"
  ddsDecompressDXT3 :: Ptr Word8 -> Int -> Int -> Ptr Word8 -> IO ()

foreign import ccall safe "ddslib.h DDSDecompressDXT1"
  ddsDecompressDXT1 :: Ptr Word8 -> Int -> Int -> Ptr Word8 -> IO ()

data CompressionType = DXT1 | DXT3 | DXT5 deriving Show

-- withDecompressed :: CompressionType -> BS.ByteString -> Int -> Int -> (Ptr a -> IO b) -> IO b
withDecompressed :: CompressionType
                 -> [Word8]
                 -> Int
                 -> Int
                 -> (Ptr Word8 -> IO b)
                 -> IO b
withDecompressed ctype stream width height action = do
  let dcomp  = case ctype of
                 DXT1 -> ddsDecompressDXT1
                 DXT3 -> ddsDecompressDXT3
                 DXT5 -> ddsDecompressDXT5
  ret <- allocaBytes (width * height * 4) (\pout -> do 
     withArray stream (\pin -> dcomp pin width height pout)
     action pout)
  return ret
