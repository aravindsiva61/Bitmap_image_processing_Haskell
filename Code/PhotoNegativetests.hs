import Test.HUnit
import qualified Data.ByteString.Lazy as B
import Data.Word
import PhotoNegative (
    createPhotoNegative,
    isCompressed,
    isBMPFormat,
    isValidBitsPerPixel,
    invertPixel,
    BMPHeader(..)
    )

-- Test cases for isCompressed function
isCompressedTests :: Test
isCompressedTests =
  TestList
    [ -- Test case 1: Compressed BMP header
      TestCase $
        assertBool "Compressed BMP header" (isCompressed (BMPHeader {bitsPerPixel = 24, compression = 1})),
      -- Test case 2: Uncompressed BMP header
      TestCase $
        assertBool "Uncompressed BMP header" (not (isCompressed (BMPHeader {bitsPerPixel = 24, compression = 0})))
    ]

-- Test cases for isBMPFormat function
isBMPFormatTests :: Test
isBMPFormatTests =
  TestList
    [ -- Test case 1: Valid BMP format
      TestCase $
        assertBool "Valid BMP format" (isBMPFormat (B.pack [66, 77])),
      -- Test case 2: Invalid BMP format
      TestCase $
        assertBool "Invalid BMP format" (not (isBMPFormat (B.pack [1, 2])))
    ]

-- Test cases for isValidBitsPerPixel function
isValidBitsPerPixelTests :: Test
isValidBitsPerPixelTests =
  TestList
    [ -- Test case 1: Valid bits per pixel (24)
      TestCase $
        assertBool "Valid bits per pixel (24)" (isValidBitsPerPixel (BMPHeader {bitsPerPixel = 24, compression = 0})),
      -- Test case 2: Invalid bits per pixel (not 24)
      TestCase $
        assertBool "Invalid bits per pixel (not 24)" (not (isValidBitsPerPixel (BMPHeader {bitsPerPixel = 16, compression = 0})))
    ]

-- Test cases for invertPixel function
invertPixelTests :: Test
invertPixelTests =
  TestList
    [ -- Test case 1: Inverting a white pixel
      TestCase $
        assertEqual "Inverted white pixel" 0 (invertPixel 255),
      -- Test case 2: Inverting a black pixel
      TestCase $
        assertEqual "Inverted black pixel" 255 (invertPixel 0),
      -- Test case 3: Inverting a color pixel
      TestCase $
        assertEqual "Inverted color pixel" 128 (invertPixel 127)
    ]

-- Helper function to convert a list of Word8 values to ByteString
toByteString :: [Word8] -> B.ByteString
toByteString = B.pack

-- Test cases for the createPhotoNegative function
photoNegativeTests :: Test
photoNegativeTests =
  test
    [ "Empty image data" ~: createPhotoNegative (toByteString []) ~?= toByteString []
    , "Single pixel" ~: createPhotoNegative (toByteString [255, 0, 127]) ~?= toByteString [0, 255, 128]
    , "Multiple pixels" ~: createPhotoNegative (toByteString [255, 0, 127, 0, 100, 200]) ~?= toByteString [0, 255, 128, 255, 155, 55]
    ]

-- Entry point for running the unit tests
main :: IO ()
main = do
  counts <- runTestTT $ TestList
    [ isCompressedTests
    , isBMPFormatTests
    , isValidBitsPerPixelTests
    , invertPixelTests
    , photoNegativeTests
    ]
  putStrLn $ show counts
