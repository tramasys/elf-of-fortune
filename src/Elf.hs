module Elf (
    ExecRegion,
    PatchPlan,
    applyPatch,
    patchAddress,
    patchInstruction,
    patchOffset,
    patchOldEntry,
    patchRegion,
    planPatch,
    regionSource,
) where

import Control.Monad (when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (partition)
import Data.Maybe (catMaybes)
import Data.Word (Word16, Word32, Word64, Word8)
import Roulette (CrashInstruction, Rng, crashPayload, uniformIndex)

data ExecRegion = ExecRegion
    { regionOffset :: Int
    , regionAddress :: Word64
    , regionSize :: Int
    , regionName :: String
    , regionSource :: String
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

data ElfInfo = ElfInfo
    { elfOldEntry :: Word64
    , elfRegions :: [ExecRegion]
    }

data ElfHeader = ElfHeader
    { headerEntry :: Word64
    , headerPrograms :: HeaderTable
    , headerSections :: HeaderTable
    , headerStringSection :: Int
    }

data HeaderTable = HeaderTable
    { tableOffset :: Int
    , tableEntrySize :: Int
    , tableEntryCount :: Int
    }

data ProgramHeader = ProgramHeader
    { programIndex :: Int
    , programKind :: Word32
    , programFlags :: Word32
    , programOffset :: Int
    , programAddress :: Word64
    , programFileSize :: Int
    }

data SectionHeader = SectionHeader
    { sectionIndex :: Int
    , sectionNameOffset :: Int
    , sectionKind :: Word32
    , sectionFlags :: Word64
    , sectionAddress :: Word64
    , sectionOffset :: Int
    , sectionFileSize :: Maybe Int
    }

data StringTable = StringTable
    { stringsOffset :: Int
    , stringsSize :: Int
    }

planPatch :: BS.ByteString -> CrashInstruction -> Rng -> Either String PatchPlan
planPatch bytes instruction rng = do
    elf <- parseElf bytes
    let payloadSize = length $ crashPayload instruction
        candidates = filter ((>= payloadSize) . regionSize) $ elfRegions elf
    ensure (not $ null candidates) "executable region too small for selected payload"
    (regionIndex, next) <- uniformIndex (length candidates) rng
    region <- note "internal executable-region index out of range" $ atMay candidates regionIndex
    let slots = regionSize region - payloadSize + 1
    (delta, _) <- uniformIndex slots next
    let address = regionAddress region + fromIntegral delta
    ensure (address >= regionAddress region) "malformed ELF: virtual address overflow"
    pure
        PatchPlan
            { patchInstruction = instruction
            , patchRegion = region
            , patchOffset = regionOffset region + delta
            , patchAddress = address
            , patchOldEntry = elfOldEntry elf
            }

applyPatch :: BS.ByteString -> PatchPlan -> BS.ByteString
applyPatch bytes plan = replaceBytes entryOffset encodedEntry withPayload
  where
    payload = BS.pack $ crashPayload $ patchInstruction plan
    withPayload = replaceBytes (patchOffset plan) payload bytes
    encodedEntry = word64LittleEndian $ patchAddress plan

parseElf :: BS.ByteString -> Either String ElfInfo
parseElf bytes = do
    validateIdent bytes
    header <- readElfHeader bytes
    programs <- readProgramHeaders bytes $ headerPrograms header
    sections <- readSectionHeaders bytes $ headerSections header
    regions <-
        if null sections
            then pure $ discoverSegments programs
            else discoverSections bytes (headerStringSection header) sections
    ensure (not $ null regions) "no executable file-backed region found"
    pure $ ElfInfo (headerEntry header) regions

readElfHeader :: BS.ByteString -> Either String ElfHeader
readElfHeader bytes = do
    kind <- readWord16 bytes 0x10
    machine <- readWord16 bytes 0x12
    version <- readWord32 bytes 0x14
    entry <- readWord64 bytes entryOffset
    programs <- HeaderTable <$> readIndex64 bytes 0x20 <*> readIndex16 bytes 0x36 <*> readIndex16 bytes 0x38
    sections <- HeaderTable <$> readIndex64 bytes 0x28 <*> readIndex16 bytes 0x3a <*> readIndex16 bytes 0x3c
    headerSize <- readWord16 bytes 0x34
    stringSection <- readIndex16 bytes 0x3e
    ensure (machine == elfMachineX86_64) "architecture is not x86-64"
    ensure (kind == elfTypeExecutable || kind == elfTypeDynamic) "only ET_EXEC and ET_DYN are supported"
    ensure (version == elfVersionCurrent) "unsupported ELF version"
    ensure (headerSize == elfHeaderSize) "malformed ELF: invalid header size"
    ensure (kind /= elfTypeDynamic || entry /= 0) "ELF appears to be a shared library"
    pure $ ElfHeader entry programs sections stringSection

validateIdent :: BS.ByteString -> Either String ()
validateIdent bytes
    | BS.length bytes < 4 = Left "truncated ELF header"
    | BS.take 4 bytes /= elfMagic = Left "not an ELF file"
    | BS.length bytes < fromIntegral elfHeaderSize = Left "truncated ELF header"
    | BS.index bytes 4 /= elfClass64 = Left "ELF32 is not supported"
    | BS.index bytes 5 /= elfLittleEndian = Left "only little-endian ELF is supported"
    | BS.index bytes 6 /= fromIntegral elfVersionCurrent = Left "unsupported ELF identification version"
    | otherwise = Right ()

readProgramHeaders :: BS.ByteString -> HeaderTable -> Either String [ProgramHeader]
readProgramHeaders bytes table
    | tableEntryCount table == 0 = do
        ensure (tableOffset table == 0) "malformed ELF: unexpected program header table"
        pure []
    | otherwise = do
        ensure (tableEntrySize table == programHeaderSize) "malformed ELF: invalid program header entry size"
        ensure (tableOffset table /= 0) "malformed ELF: missing program header table"
        checkedTable table (BS.length bytes) "malformed ELF: program header table outside file"
        traverse (readProgramHeader bytes . tableEntryOffset table) [0 .. tableEntryCount table - 1]

readProgramHeader :: BS.ByteString -> (Int, Int) -> Either String ProgramHeader
readProgramHeader bytes (index, offset) = do
    kind <- readWord32 bytes offset
    flags <- readWord32 bytes $ offset + 4
    fileOffset <- readIndex64 bytes $ offset + 8
    address <- readWord64 bytes $ offset + 16
    fileSize <- readIndex64 bytes $ offset + 32
    memorySize <- readWord64 bytes $ offset + 40
    when (kind == segmentLoad) $
        ensure (fromIntegral fileSize <= memorySize) "malformed ELF: p_filesz exceeds p_memsz"
    when (fileSize > 0) $
        checkedSpan fileOffset fileSize (BS.length bytes) "malformed ELF: segment outside file"
    pure $ ProgramHeader index kind flags fileOffset address fileSize

readSectionHeaders :: BS.ByteString -> HeaderTable -> Either String [SectionHeader]
readSectionHeaders bytes table
    | tableEntryCount table == 0 = do
        ensure (tableOffset table == 0) "malformed ELF: extended section numbering is unsupported"
        pure []
    | otherwise = do
        ensure (tableEntrySize table == sectionHeaderSize) "malformed ELF: invalid section header entry size"
        ensure (tableOffset table /= 0) "malformed ELF: missing section header table"
        checkedTable table (BS.length bytes) "malformed ELF: section header table outside file"
        traverse (readSectionHeader bytes . tableEntryOffset table) [0 .. tableEntryCount table - 1]

readSectionHeader :: BS.ByteString -> (Int, Int) -> Either String SectionHeader
readSectionHeader bytes (index, offset) = do
    nameOffset <- readIndex32 bytes offset
    kind <- readWord32 bytes $ offset + 4
    flags <- readWord64 bytes $ offset + 8
    address <- readWord64 bytes $ offset + 16
    fileOffset <- readIndex64 bytes $ offset + 24
    rawSize <- readWord64 bytes $ offset + 32
    fileSize <- if kind == sectionNoBits then pure Nothing else Just <$> wordToIndex rawSize
    case fileSize of
        Just size
            | size > 0 ->
                checkedSpan fileOffset size (BS.length bytes) "malformed ELF: section outside file"
        _ -> pure ()
    pure $ SectionHeader index nameOffset kind flags address fileOffset fileSize

discoverSegments :: [ProgramHeader] -> [ExecRegion]
discoverSegments = map toRegion . filter isExecutableLoad
  where
    isExecutableLoad header =
        programKind header == segmentLoad
            && programFlags header .&. segmentExecutable /= 0
            && programFileSize header > 0

    toRegion header =
        let name = "PT_LOAD#" ++ show (programIndex header)
         in ExecRegion
                { regionOffset = programOffset header
                , regionAddress = programAddress header
                , regionSize = programFileSize header
                , regionName = name
                , regionSource = "segment:" ++ name
                }

discoverSections :: BS.ByteString -> Int -> [SectionHeader] -> Either String [ExecRegion]
discoverSections bytes stringIndex sections = do
    ensure (stringIndex >= 0 && stringIndex < length sections) "malformed ELF: invalid section-name table index"
    stringTable <- readStringTable stringIndex sections
    regions <- catMaybes <$> traverse (toRegion stringTable) sections
    let (textRegions, otherRegions) = partition ((== ".text") . regionName) regions
    pure $ textRegions ++ otherRegions
  where
    toRegion stringTable section = do
        name <- sectionLabel bytes stringTable section
        pure $ case sectionFileSize section of
            Just size
                | size > 0 && sectionFlags section .&. sectionExecutable /= 0 ->
                    Just
                        ExecRegion
                            { regionOffset = sectionOffset section
                            , regionAddress = sectionAddress section
                            , regionSize = size
                            , regionName = name
                            , regionSource = "section:" ++ name
                            }
            _ -> Nothing

readStringTable :: Int -> [SectionHeader] -> Either String (Maybe StringTable)
readStringTable 0 _ = Right Nothing
readStringTable index sections = do
    section <- note "malformed ELF: invalid section-name table index" $ atMay sections index
    ensure (sectionKind section == sectionStringTable) "malformed ELF: section names are not a string table"
    size <- note "malformed ELF: section names are not file-backed" $ sectionFileSize section
    pure $ if size == 0 then Nothing else Just $ StringTable (sectionOffset section) size

sectionLabel :: BS.ByteString -> Maybe StringTable -> SectionHeader -> Either String String
sectionLabel _ Nothing section = Right $ fallbackSectionName section
sectionLabel bytes (Just table) section
    | sectionNameOffset section < 0 || sectionNameOffset section >= stringsSize table =
        Left "malformed ELF: invalid section name"
    | otherwise = do
        let available =
                BS.take (stringsSize table - sectionNameOffset section) $
                    BS.drop (stringsOffset table + sectionNameOffset section) bytes
        nameLength <- note "malformed ELF: unterminated section name" $ BS.elemIndex 0 available
        let name = BSC.unpack $ BS.take nameLength available
        pure $ if name `elem` knownSectionNames then name else fallbackSectionName section

fallbackSectionName :: SectionHeader -> String
fallbackSectionName = ("section#" ++) . show . sectionIndex

replaceBytes :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
replaceBytes offset replacement bytes =
    BS.take offset bytes <> replacement <> BS.drop (offset + BS.length replacement) bytes

word64LittleEndian :: Word64 -> BS.ByteString
word64LittleEndian value = BS.pack [fromIntegral $ value `shiftR` shift | shift <- [0, 8 .. 56]]

readIndex16 :: BS.ByteString -> Int -> Either String Int
readIndex16 bytes offset = fromIntegral <$> readWord16 bytes offset

readIndex32 :: BS.ByteString -> Int -> Either String Int
readIndex32 bytes offset = readWord32 bytes offset >>= wordToIndex . fromIntegral

readIndex64 :: BS.ByteString -> Int -> Either String Int
readIndex64 bytes offset = readWord64 bytes offset >>= wordToIndex

wordToIndex :: Word64 -> Either String Int
wordToIndex value
    | value > fromIntegral (maxBound :: Int) = Left supportedRangeError
    | otherwise = Right $ fromIntegral value

readWord16 :: BS.ByteString -> Int -> Either String Word16
readWord16 bytes offset = fromIntegral <$> readLittleEndian 2 bytes offset

readWord32 :: BS.ByteString -> Int -> Either String Word32
readWord32 bytes offset = fromIntegral <$> readLittleEndian 4 bytes offset

readWord64 :: BS.ByteString -> Int -> Either String Word64
readWord64 = readLittleEndian 8

readLittleEndian :: Int -> BS.ByteString -> Int -> Either String Word64
readLittleEndian width bytes offset = do
    checkedSpan offset width (BS.length bytes) "malformed ELF: integer outside file"
    pure $ BS.foldr addByte 0 $ BS.take width $ BS.drop offset bytes
  where
    addByte byte value = fromIntegral byte .|. (value `shiftL` 8)

checkedSpan :: Int -> Int -> Int -> String -> Either String ()
checkedSpan offset size fileSize message
    | offset < 0 || size < 0 = Left message
    | offset > fileSize = Left message
    | size > fileSize - offset = Left message
    | otherwise = Right ()

checkedTable :: HeaderTable -> Int -> String -> Either String ()
checkedTable table fileSize message
    | tableEntrySize table <= 0 || tableEntryCount table < 0 = Left message
    | tableOffset table < 0 || tableOffset table > fileSize = Left message
    | tableEntryCount table > (fileSize - tableOffset table) `div` tableEntrySize table = Left message
    | otherwise = Right ()

tableEntryOffset :: HeaderTable -> Int -> (Int, Int)
tableEntryOffset table index = (index, tableOffset table + index * tableEntrySize table)

atMay :: [value] -> Int -> Maybe value
atMay values index
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing

ensure :: Bool -> String -> Either String ()
ensure condition message = if condition then Right () else Left message

note :: error -> Maybe value -> Either error value
note errorValue = maybe (Left errorValue) Right

elfMagic :: BS.ByteString
elfMagic = BS.pack [0x7f, 0x45, 0x4c, 0x46]

knownSectionNames :: [String]
knownSectionNames = [".text", ".init", ".fini", ".plt", ".plt.got", ".plt.sec"]

entryOffset, programHeaderSize, sectionHeaderSize :: Int
entryOffset = 0x18
programHeaderSize = 56
sectionHeaderSize = 64

elfHeaderSize, elfMachineX86_64, elfTypeExecutable, elfTypeDynamic :: Word16
elfHeaderSize = 64
elfMachineX86_64 = 62
elfTypeExecutable = 2
elfTypeDynamic = 3

elfVersionCurrent, segmentLoad, segmentExecutable, sectionNoBits, sectionStringTable :: Word32
elfVersionCurrent = 1
segmentLoad = 1
segmentExecutable = 1
sectionNoBits = 8
sectionStringTable = 3

sectionExecutable :: Word64
sectionExecutable = 4

elfClass64, elfLittleEndian :: Word8
elfClass64 = 2
elfLittleEndian = 1

supportedRangeError :: String
supportedRangeError = "malformed ELF: file offset or size exceeds supported range"
