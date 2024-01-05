module PhotoNegative
( createPhotoNegative
, isCompressed
, isBMPFormat
, isValidBitsPerPixel
, invertPixel
, main
, BMPHeader(..)
) where
  
import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Bits
import Data.Int
import Data.Word
import System.Environment
import System.Exit

-- Constants
headerSize :: Int64
headerSize = fromIntegral 54

-- BMP header structure
data BMPHeader = BMPHeader
  { bitsPerPixel :: Word16
  , compression :: Word32
  } deriving Show

-- Function to parse the BMP header
parseBMPHeader :: Get BMPHeader
parseBMPHeader = do
  -- Skip the remaining header bytes until bits per pixel (28 bytes)
  skip 28

  -- Read the bits per pixel (2 bytes)
  bitsPerPixel <- getWord16le

  -- Read the compression (4 bytes)
  compression <- getWord32le

  return BMPHeader
    { bitsPerPixel = bitsPerPixel
    , compression = compression
    }

-- Function to parse the BMP image header from a byte string
parseBMPHeaderFromBytes :: B.ByteString -> Either String BMPHeader
parseBMPHeaderFromBytes bs =
  case runGetOrFail parseBMPHeader bs of
    Left (_, _, errMsg) -> Left errMsg
    Right (_, _, header) -> Right header

-- Function to check if the BMP image is compressed
isCompressed :: BMPHeader -> Bool
isCompressed header = compression header /= 0

-- Function to check the BMP file format
isBMPFormat :: B.ByteString -> Bool
isBMPFormat bs = B.take 2 bs == B.pack [66, 77]

-- Function to check the bits per pixel
isValidBitsPerPixel :: BMPHeader -> Bool
isValidBitsPerPixel header = bitsPerPixel header == 24

-- Function to invert a single pixel
invertPixel :: Word8 -> Word8
invertPixel pixel = complement pixel

-- Function to create the photo negative from the image data
createPhotoNegative :: B.ByteString -> B.ByteString
createPhotoNegative = B.map invertPixel

-- Function to process the input BMP file
processBMPFile :: B.ByteString -> FilePath -> IO ()
processBMPFile inputContent outputFile = do

  -- Extract the image data
  let imageData = B.drop headerSize inputContent

  -- Create the photo negative
  let negativeData = createPhotoNegative imageData

  -- Create the output file content by combining the header and negative data
  let outputContent = B.append (B.take headerSize inputContent) negativeData

  -- Write the photo negative to the output BMP file
  B.writeFile outputFile outputContent

-- Function to print usage instructions
printUsage :: IO ()
printUsage = putStrLn "Usage: PhotoNegative.exe <input_file.bmp> <output_file.bmp>"

-- Main function
main :: IO ()
main = do
  -- Read the command line arguments
  args <- getArgs

  case args of
    ["--help"] -> printUsage
    [inputFile, outputFile] -> do
      -- Read the BMP image file as a byte string
      inputContent <- B.readFile inputFile

      -- Parse the BMP header
      case parseBMPHeaderFromBytes inputContent of
        Left errMsg -> do
          putStrLn $ "Error parsing BMP header: " ++ errMsg
          exitFailure
        Right header -> do
          -- Check the file format
          let isBMP = isBMPFormat inputContent
          putStrLn $ "Is BMP format: " ++ show isBMP

          if not isBMP
            then do
              putStrLn "Error: File format not supported."
              exitFailure
            else putStrLn "File format is BMP. Continuing..."

          -- Check the bits per pixel
          let validBitsPerPixel = isValidBitsPerPixel header
          putStrLn $ "Valid bits per pixel: " ++ show validBitsPerPixel

          if not validBitsPerPixel
            then do
              putStrLn "Error: Invalid bits per pixel."
              exitFailure
            else putStrLn "24 bits per pixel. Continuing..."

          -- Check if the image is compressed
          let compressed = isCompressed header
          putStrLn $ "Image is compressed: " ++ show compressed

          if compressed
            then do
              putStrLn "Error: Compressed image not supported."
              exitFailure
            else putStrLn "Image is not compressed. Continuing..."

          -- Convert image content to negative and write it to file
          processBMPFile inputContent outputFile
          putStrLn "Successfully created negative image"
    _ -> do
      printUsage
      exitFailure
