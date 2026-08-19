module Elf
  ( ExecRegion (..)
  , PatchPlan (..)
  , applyPatch
  , planPatch
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.List (partition)
import Data.Maybe (catMaybes)
import Data.Word (Word16, Word32, Word64)
import Roulette (CrashInstruction (..), rngBounded)

data ExecRegion = ExecRegion
  { regionOffset :: Int
  , regionAddress :: Word64
  , regionSize :: Int
  , regionName :: String
  , regionSource :: String
  }
  deriving (Eq, Show)

data ElfInfo = ElfInfo
  { elfOldEntry :: Word64
  , elfRegions :: [ExecRegion]
  }
  deriving (Eq, Show)

data PatchPlan = PatchPlan
  { patchInstruction :: CrashInstruction
  , patchRegion :: ExecRegion
  , patchOffset :: Int
  , patchAddress :: Word64
  , patchOldEntry :: Word64
  }
  deriving (Eq, Show)

planPatch :: BS.ByteString -> CrashInstruction -> Int -> Either String PatchPlan
planPatch bytes instruction state = do
  elf <- parseElf bytes
  let payloadSize = length (crashPayload instruction)
      candidates = filter ((>= payloadSize) . regionSize) (elfRegions elf)
  if null candidates
    then Left "executable region too small for selected payload"
    else do
      (next, regionIndex) <- rngBounded state (length candidates)
      let region = candidates !! regionIndex
          slots = regionSize region - payloadSize + 1
      (_, delta) <- rngBounded next slots
      let address = regionAddress region + fromIntegral delta
      if address < regionAddress region
        then Left "malformed ELF: virtual address overflow"
        else
          Right
            PatchPlan
              { patchInstruction = instruction
              , patchRegion = region
              , patchOffset = regionOffset region + delta
              , patchAddress = address
              , patchOldEntry = elfOldEntry elf
              }

applyPatch :: BS.ByteString -> PatchPlan -> BS.ByteString
applyPatch bytes plan =
  let withPayload = replaceBytes (patchOffset plan) (BS.pack $ crashPayload $ patchInstruction plan) bytes
   in replaceBytes 0x18 (word64LittleEndian $ patchAddress plan) withPayload

replaceBytes :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
replaceBytes offset replacement bytes =
  BS.take offset bytes <> replacement <> BS.drop (offset + BS.length replacement) bytes

word64LittleEndian :: Word64 -> BS.ByteString
word64LittleEndian value = BS.pack [fromIntegral (value `shiftR` shift) | shift <- [0, 8 .. 56]]

parseElf :: BS.ByteString -> Either String ElfInfo
parseElf bytes = do
  validateIdent bytes
  kind <- readWord16 bytes 0x10
  machine <- readWord16 bytes 0x12
  version <- readWord32 bytes 0x14
  entry <- readWord64 bytes 0x18
  programOffset <- readIndex64 bytes 0x20
  sectionOffset <- readIndex64 bytes 0x28
  headerSize <- readWord16 bytes 0x34
  programEntrySize <- readWord16 bytes 0x36
  programCount <- readWord16 bytes 0x38
  sectionEntrySize <- readWord16 bytes 0x3a
  sectionCount <- readWord16 bytes 0x3c
  stringSectionIndex <- readWord16 bytes 0x3e
  if machine /= 62
    then Left "architecture is not x86-64"
    else
      if kind /= 2 && kind /= 3
        then Left "only ET_EXEC and ET_DYN are supported"
        else
          if version /= 1
            then Left "unsupported ELF version"
            else
              if headerSize /= 64
                then Left "malformed ELF header: invalid header size"
                else
                  if kind == 3 && entry == 0
                    then Left "ELF appears to be a shared library"
                    else do
                      validateProgramHeaders bytes programOffset (fromIntegral programEntrySize) (fromIntegral programCount)
                      validateSectionTable sectionOffset (fromIntegral sectionEntrySize) (fromIntegral sectionCount) (BS.length bytes)
                      regions <-
                        if sectionCount > 0
                          then discoverSections bytes sectionOffset (fromIntegral sectionEntrySize) (fromIntegral sectionCount) (fromIntegral stringSectionIndex)
                          else discoverSegments bytes programOffset (fromIntegral programEntrySize) (fromIntegral programCount)
                      if null regions
                        then Left "no executable file-backed region found"
                        else Right $ ElfInfo entry regions

validateIdent :: BS.ByteString -> Either String ()
validateIdent bytes
  | BS.length bytes < 4 = Left "truncated ELF header"
  | BS.take 4 bytes /= BS.pack [0x7f, 0x45, 0x4c, 0x46] = Left "not an ELF file"
  | BS.length bytes < 64 = Left "truncated ELF header"
  | BS.index bytes 4 /= 2 = Left "ELF32 is not supported"
  | BS.index bytes 5 /= 1 = Left "only little-endian ELF is supported"
  | BS.index bytes 6 /= 1 = Left "unsupported ELF identification version"
  | otherwise = Right ()

validateProgramHeaders :: BS.ByteString -> Int -> Int -> Int -> Either String ()
validateProgramHeaders bytes offset entrySize count
  | count == 0 =
      if offset == 0
        then Right ()
        else Left "malformed program header table"
  | entrySize /= 56 = Left "malformed program header table: invalid entry size"
  | offset == 0 = Left "malformed program header table"
  | otherwise = do
      checkedTable offset entrySize count (BS.length bytes) "malformed program header table"
      mapM_ validate [0 .. count - 1]
  where
    validate index = do
      let header = offset + index * entrySize
      kind <- readWord32 bytes header
      fileOffset <- readIndex64 bytes (header + 8)
      fileBytes <- readIndex64 bytes (header + 32)
      memoryBytes <- readWord64 bytes (header + 40)
      if kind == 1 && fromIntegral fileBytes > memoryBytes
        then Left "malformed program header table: p_filesz exceeds p_memsz"
        else
          if fileBytes > 0
            then checkedSpan fileOffset fileBytes (BS.length bytes) "malformed program header table: segment outside file"
            else Right ()

validateSectionTable :: Int -> Int -> Int -> Int -> Either String ()
validateSectionTable offset entrySize count fileSize
  | count == 0 =
      if offset == 0
        then Right ()
        else Left "malformed section header table: extended numbering is unsupported"
  | entrySize /= 64 = Left "malformed section header table: invalid entry size"
  | offset == 0 = Left "malformed section header table"
  | otherwise = checkedTable offset entrySize count fileSize "malformed section header table"

discoverSections :: BS.ByteString -> Int -> Int -> Int -> Int -> Either String [ExecRegion]
discoverSections bytes tableOffset entrySize count stringIndex
  | stringIndex < 0 || stringIndex >= count = Left "malformed section header table: invalid string table index"
  | otherwise = do
      (stringsOffset, stringsSize) <-
        if stringIndex == 0
          then Right (0, 0)
          else do
            let stringsHeader = tableOffset + stringIndex * entrySize
            stringKind <- readWord32 bytes (stringsHeader + 4)
            if stringKind /= 3
              then Left "malformed section header table: names are not a string table"
              else do
                offset <- readIndex64 bytes (stringsHeader + 24)
                size <- readIndex64 bytes (stringsHeader + 32)
                pure (offset, size)
      if stringsSize > 0
        then checkedSpan stringsOffset stringsSize (BS.length bytes) "malformed section header table: string table outside file"
        else Right ()
      regions <- fmap catMaybes $ mapM (readSection stringsOffset stringsSize) [0 .. count - 1]
      let (textRegions, otherRegions) = partition ((== ".text") . regionName) regions
      pure $ textRegions ++ otherRegions
  where
    readSection stringsOffset stringsSize index = do
      let header = tableOffset + index * entrySize
      nameOffset <- readIndex32 bytes header
      kind <- readWord32 bytes (header + 4)
      flags <- readWord64 bytes (header + 8)
      address <- readWord64 bytes (header + 16)
      offset <- readIndex64 bytes (header + 24)
      name <- sectionLabel bytes stringsOffset stringsSize nameOffset index
      if kind == 8
        then Right Nothing
        else do
          size <- readIndex64 bytes (header + 32)
          if size > 0
            then checkedSpan offset size (BS.length bytes) "malformed section header table: section outside file"
            else Right ()
          if size > 0 && flags .&. 4 /= 0
            then Right $ Just $ ExecRegion offset address size name ("section:" ++ name)
            else Right Nothing

discoverSegments :: BS.ByteString -> Int -> Int -> Int -> Either String [ExecRegion]
discoverSegments bytes tableOffset entrySize count = fmap catMaybes $ mapM readSegment [0 .. count - 1]
  where
    readSegment index = do
      let header = tableOffset + index * entrySize
      kind <- readWord32 bytes header
      flags <- readWord32 bytes (header + 4)
      offset <- readIndex64 bytes (header + 8)
      address <- readWord64 bytes (header + 16)
      size <- readIndex64 bytes (header + 32)
      if kind == 1 && flags .&. 1 /= 0 && size > 0
        then
          let name = "PT_LOAD#" ++ show index
           in Right $ Just $ ExecRegion offset address size name ("segment:" ++ name)
        else Right Nothing

sectionLabel :: BS.ByteString -> Int -> Int -> Int -> Int -> Either String String
sectionLabel bytes stringsOffset stringsSize nameOffset index
  | stringsSize == 0 = Right fallback
  | nameOffset < 0 || nameOffset >= stringsSize = Left "malformed section header table: invalid section name"
  | otherwise =
      let available = BS.take (stringsSize - nameOffset) $ BS.drop (stringsOffset + nameOffset) bytes
       in case BS.elemIndex 0 available of
            Nothing -> Left "malformed section header table: unterminated section name"
            Just lengthOfName ->
              let nameBytes = BS.take lengthOfName available
                  name = map (toEnum . fromIntegral) $ BS.unpack nameBytes
               in Right $ if name `elem` knownNames then name else fallback
  where
    fallback = "section#" ++ show index
    knownNames = [".text", ".init", ".fini", ".plt", ".plt.got", ".plt.sec"]

checkedSpan :: Int -> Int -> Int -> String -> Either String ()
checkedSpan offset size fileSize message
  | offset < 0 || size < 0 = Left message
  | offset > fileSize = Left message
  | size > fileSize - offset = Left message
  | otherwise = Right ()

checkedTable :: Int -> Int -> Int -> Int -> String -> Either String ()
checkedTable offset entrySize count fileSize message
  | entrySize <= 0 || count < 0 = Left message
  | offset < 0 || offset > fileSize = Left message
  | count > (fileSize - offset) `div` entrySize = Left message
  | otherwise = Right ()

readIndex32 :: BS.ByteString -> Int -> Either String Int
readIndex32 bytes offset = do
  value <- readWord32 bytes offset
  if value > fromIntegral (maxBound :: Int32)
    then Left "malformed ELF: file offset or size exceeds supported range"
    else Right $ fromIntegral value

readIndex64 :: BS.ByteString -> Int -> Either String Int
readIndex64 bytes offset = do
  value <- readWord64 bytes offset
  if value > fromIntegral (maxBound :: Int32)
    then Left "malformed ELF: file offset or size exceeds supported range"
    else Right $ fromIntegral value

readWord16 :: BS.ByteString -> Int -> Either String Word16
readWord16 bytes offset = do
  checkedSpan offset 2 (BS.length bytes) "malformed ELF: integer outside file"
  pure $ fromIntegral (BS.index bytes offset) .|. (fromIntegral (BS.index bytes $ offset + 1) `shiftL` 8)

readWord32 :: BS.ByteString -> Int -> Either String Word32
readWord32 bytes offset = do
  checkedSpan offset 4 (BS.length bytes) "malformed ELF: integer outside file"
  pure $ fromIntegral $ foldLittleEndian bytes offset 4

readWord64 :: BS.ByteString -> Int -> Either String Word64
readWord64 bytes offset = do
  checkedSpan offset 8 (BS.length bytes) "malformed ELF: integer outside file"
  pure $ foldLittleEndian bytes offset 8

foldLittleEndian :: BS.ByteString -> Int -> Int -> Word64
foldLittleEndian bytes offset width = go 0 0
  where
    go index value
      | index == width = value
      | otherwise = go (index + 1) (value .|. (fromIntegral (BS.index bytes $ offset + index) `shiftL` (index * 8)))
