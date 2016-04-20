{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

-- | Buffering abstraction.

module System.IO.ByteBuffer where

import Control.Exception
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import GHC.IO.Handle
import GHC.Ptr
import System.IO

-- A filling function which is capable of writing to a buffer n bytes.
type FillBuffer = Ptr Word8 -> Int -> IO Int

-- A buffer into which bytes can be written.
data ByteBuffer = ByteBuffer
    { byteBufferPtr :: !(Ptr Word8)
    , byteBufferFill :: !FillBuffer
    , byteBufferSize :: !Int
    }

-- | Allocates a new ByteBuffer with a given buffer size filling from
-- the given FillBuffer.
newByteBuffer
    :: Int -- ^ Size of buffer to allocate.
    -> FillBuffer -- ^ The filling function to use.
    -> IO ByteBuffer -- ^ The byte buffer.
newByteBuffer size fillBuffer = do
    ptr <- mallocBytes size
    let !byteBuffer = ByteBuffer ptr fillBuffer size
    return byteBuffer

-- | Free a byte buffer.
freeByteBuffer :: ByteBuffer -> IO ()
freeByteBuffer = free . byteBufferPtr

-- | Fill the byte buffer with more available bytes. You must specify
-- how many bytes you've consumed from the buffer. Unconsumed bytes
-- will be shifted to the beggining of the buffer and more will be
-- filled after it.
fillByteBuffer
    :: ByteBuffer -- ^ Byte buffer to fill.
    -> Int        -- ^ How many bytes consumed by the user of this function.
    -> IO Int     -- ^ How many new bytes available.
fillByteBuffer (ByteBuffer ptr fill size) consumed =
    if consumed == 0 || consumed == size
       then fill ptr size
       else do let unconsumed = size - consumed
               moveBytes ptr (plusPtr ptr consumed) unconsumed
               available <- fill (plusPtr ptr unconsumed) (size - unconsumed)
               return (unconsumed + available)

-- | Make a 'FillBuffer' for a 'Handle'.
handleFillBuffer :: Handle -> FillBuffer
handleFillBuffer h = \ptr size -> hGetBuf h ptr size

-- | Opens the given file in 'ReadMode', provides a 'FillBuffer' for
-- that file, and afterwards closes the file.
withFileFillBuffer :: FilePath -> (FillBuffer -> IO a) -> IO a
withFileFillBuffer fp cont =
    bracket (openFile fp ReadMode)
            hClose
            (cont . handleFillBuffer)

