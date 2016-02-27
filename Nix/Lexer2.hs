module Nix.Lexer2 where

import Nix.Expr.Types

data Token
  = IF
  | THEN
  | ELSE
  | ASSERT
  | WITH
  | LET
  | IN
  | REC
  | INHERIT
  | OR_KW
  | ELLIPSIS
  | TBinary NBinaryOp
  | DOLLAR_CURLY
  | LBRACE
  | RBRACE
  | QUOTE
  | IND_STRING_OPEN
  | IND_STRING
  | IND_STRING_CLOSE
  | STR
  deriving (Show,Eq,Ord)

data ParseState = ParseState
  { input :: String
  , curCol :: Int
  , curLn :: Int
  , lastTokLoc :: SrcLoc
  , iParseFilename :: String,
  , iExtensions :: [KnownExtension],
  , iIgnoreLinePragmas :: Bool,
  , stack :: [LexContext]
  , flag :: CtxtFlag
  , comments :: [Comment]
  }

data PState = PState {
        buffer     :: StringBuffer,
        dflags     :: DynFlags,
        messages   :: Messages,
        tab_first  :: Maybe RealSrcSpan, -- pos of first tab warning in the file
        tab_count  :: !Int,              -- number of tab warnings in the file
        last_tk    :: Maybe Token,
        last_loc   :: RealSrcSpan, -- pos of previous token
        last_len   :: !Int,        -- len of previous token
        loc        :: RealSrcLoc,  -- current loc (end of prev token + 1)
        extsBitmap :: !ExtsBitmap,    -- bitmap that determines permitted
                                   -- extensions
        context    :: [LayoutContext],
        lex_state  :: [Int],
        srcfiles   :: [FastString],
        -- Used in the alternative layout rule:
        -- These tokens are the next ones to be sent out. They are
        -- just blindly emitted, without the rule looking at them again:
        alr_pending_implicit_tokens :: [RealLocated Token],
        -- This is the next token to be considered or, if it is Nothing,
        -- we need to get the next token from the input stream:
        alr_next_token :: Maybe (RealLocated Token),
        -- This is what we consider to be the location of the last token
        -- emitted:
        alr_last_loc :: RealSrcSpan,
        -- The stack of layout contexts:
        alr_context :: [ALRContext],
        -- Are we expecting a '{'? If it's Just, then the ALRLayout tells
        -- us what sort of layout the '{' will open:
        alr_expecting_ocurly :: Maybe ALRLayout,
        -- Have we just had the '}' for a let block? If so, than an 'in'
        -- token doesn't need to close anything:
        alr_justClosedExplicitLetBlock :: Bool,

        -- The next three are used to implement Annotations giving the
        -- locations of 'noise' tokens in the source, so that users of
        -- the GHC API can do source to source conversions.
        -- See note [Api annotations] in ApiAnnotation.hs
        annotations :: [(ApiAnnKey,[SrcSpan])],
        comment_q :: [Located AnnotationComment],
        annotations_comments :: [(SrcSpan,[Located AnnotationComment])]
     }
        -- last_loc and last_len are used when generating error messages,
        -- and in pushCurrentContext only.  Sigh, if only Happy passed the
        -- current token to happyError, we could at least get rid of last_len.
        -- Getting rid of last_loc would require finding another way to
        -- implement pushCurrentContext (which is only called from one place).

data ParseStatus a = Ok (ParseState,a) | Failed (SrcLoc,String)
  deriving Show

data LayoutContext
  = NoLayout
  | Layout !Int
  deriving Show

newtype P a = P { runP :: ParseState -> ParseStatus a }
PState -> ParseResult a

IND_STRING,INSIDE_DOLLAR_CURLY,STRING :: Int

binaryOp

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

data AlexInput = AI RealSrcLoc StringBuffer

data RealSrcLoc
  = SrcLoc      FastString              -- A precise location (file name)
                {-# UNPACK #-} !Int     -- line number, begins at 1
                {-# UNPACK #-} !Int     -- column number, begins at 1

data RealSrcSpan
  = RealSrcSpan'
        { srcSpanFile     :: !FastString,
          srcSpanSLine    :: {-# UNPACK #-} !Int,
          srcSpanSCol     :: {-# UNPACK #-} !Int,
          srcSpanELine    :: {-# UNPACK #-} !Int,
          srcSpanECol     :: {-# UNPACK #-} !Int
        }
  deriving (Eq, Typeable)


data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- length
data StringBuffer
 = StringBuffer {
     buf :: {-# UNPACK #-} !(ForeignPtr Word8), -- immutable
     len :: {-# UNPACK #-} !Int,        -- length
     cur :: {-# UNPACK #-} !Int         -- current pos
  }
type Action = RealSrcSpan -> StringBuffer -> Int -> P (RealLocated Token)

getInput :: P AlexInput
getInput = P $ \s@PState{ loc=l, buffer=b } -> POk s (AI l b)

setInput :: AlexInput -> P ()
setInput (AI l b) = P $ \s -> POk s{ loc=l, buffer=b } ()

pushLexState :: Int -> P ()
pushLexState ls = P $ \s@PState{ lex_state=l } -> POk s{lex_state=ls:l} ()

popLexState :: P Int
popLexState = P $ \s@PState{ lex_state=ls:l } -> POk s{ lex_state=l } ls

getLexState :: P Int
getLexState = P $ \s@PState{ lex_state=ls:_ } -> POk s ls

getExts :: P ExtsBitmap
getExts = P $ \s -> POk s (extsBitmap s)

setLastToken :: RealSrcSpan -> Int -> P ()
setLastToken loc len = P $ \s -> POk s {
  last_loc=loc,
  last_len=len
  } ()

setLastTk :: Token -> P ()
setLastTk tk = P $ \s -> POk s { last_tk = Just tk } ()

lexToken :: P (RealLocated Token)
lexToken = do
  inp@(AI loc1 buf) <- getInput
  sc <- getLexState
  exts <- getExts
  case alexScanUser exts inp sc of
    AlexEOF -> do
        let span = mkRealSrcSpan loc1 loc1
        setLastToken span 0
        return (L span ITeof)
    AlexError (AI loc2 buf) ->
        reportLexError loc1 loc2 buf "lexical error"
    AlexSkip inp2 _ -> do
        setInput inp2
        lexToken
    AlexToken inp2@(AI end buf2) _ t -> do
        setInput inp2
        let span = mkRealSrcSpan loc1 end
        let bytes = byteDiff buf buf2
        span `seq` setLastToken span bytes
        lt <- t span buf bytes
        case unLoc lt of
          ITlineComment _  -> return lt
          ITblockComment _ -> return lt
          lt' -> do
            setLastTk lt'
            return lt

-- We can't write the decoder as efficiently as we'd like without
-- resorting to unboxed extensions, unfortunately.  I tried to write
-- an IO version of this function, but GHC can't eliminate boxed
-- results from an IO-returning function.
--
-- We assume we can ignore overflow when parsing a multibyte character here.
-- To make this safe, we add extra sentinel bytes to unparsed UTF-8 sequences
-- before decoding them (see StringBuffer.hs).

{-# INLINE utf8DecodeChar# #-}
utf8DecodeChar# :: Addr# -> (# Char#, Int# #)
utf8DecodeChar# a# =
  let !ch0 = word2Int# (indexWord8OffAddr# a# 0#) in
  case () of
    _ | isTrue# (ch0 <=# 0x7F#) -> (# chr# ch0, 1# #)

      | isTrue# ((ch0 >=# 0xC0#) `andI#` (ch0 <=# 0xDF#)) ->
        let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
        if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then fail 1# else
        (# chr# (((ch0 -# 0xC0#) `uncheckedIShiftL#` 6#) +#
                  (ch1 -# 0x80#)),
           2# #)

      | isTrue# ((ch0 >=# 0xE0#) `andI#` (ch0 <=# 0xEF#)) ->
        let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
        if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then fail 1# else
        let !ch2 = word2Int# (indexWord8OffAddr# a# 2#) in
        if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then fail 2# else
        (# chr# (((ch0 -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                 ((ch1 -# 0x80#) `uncheckedIShiftL#` 6#)  +#
                  (ch2 -# 0x80#)),
           3# #)

     | isTrue# ((ch0 >=# 0xF0#) `andI#` (ch0 <=# 0xF8#)) ->
        let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
        if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then fail 1# else
        let !ch2 = word2Int# (indexWord8OffAddr# a# 2#) in
        if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then fail 2# else
        let !ch3 = word2Int# (indexWord8OffAddr# a# 3#) in
        if isTrue# ((ch3 <# 0x80#) `orI#` (ch3 >=# 0xC0#)) then fail 3# else
        (# chr# (((ch0 -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                 ((ch1 -# 0x80#) `uncheckedIShiftL#` 12#) +#
                 ((ch2 -# 0x80#) `uncheckedIShiftL#` 6#)  +#
                  (ch3 -# 0x80#)),
           4# #)

      | otherwise -> fail 1#
  where
        -- all invalid sequences end up here:
        fail :: Int# -> (# Char#, Int# #)
        fail nBytes# = (# '\0'#, nBytes# #)
        -- '\xFFFD' would be the usual replacement character, but
        -- that's a valid symbol in Haskell, so will result in a
        -- confusing parse error later on.  Instead we use '\0' which
        -- will signal a lexer error immediately.

-- Getting our fingers dirty a little here, but this is performance-critical
{-# INLINE nextChar #-}
nextChar :: StringBuffer -> (Char,StringBuffer)
nextChar (StringBuffer buf len (I# cur#)) =
  inlinePerformIO $ do
    withForeignPtr buf $ \(Ptr a#) -> do
        case utf8DecodeChar# (a# `plusAddr#` cur#) of
          (# c#, nBytes# #) ->
             let cur' = I# (cur# +# nBytes#) in
             return (C# c#, StringBuffer buf len cur')

advanceSrcLoc :: RealSrcLoc -> Char -> RealSrcLoc
advanceSrcLoc (SrcLoc f l _) '\n' = SrcLoc f  (l + 1) 1
advanceSrcLoc (SrcLoc f l c) '\t' = SrcLoc f  l (((((c - 1) `shiftR` 3) + 1) `shiftL` 3) + 1)
advanceSrcLoc (SrcLoc f l c) _    = SrcLoc f  l (c + 1)

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AI loc s)
  | atEnd s   = Nothing
  | otherwise = byte `seq` loc' `seq` s' `seq`
                --trace (show (ord c)) $
                Just (byte, (AI loc' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c
        byte   = fromIntegral $ ord adj_c

        non_graphic     = '\x00'
        upper           = '\x01'
        lower           = '\x02'
        digit           = '\x03'
        symbol          = '\x04'
        space           = '\x05'
        other_graphic   = '\x06'
        suffix          = '\x07'

        adj_c
          | c <= '\x06' = non_graphic
          | c <= '\x7f' = c
          -- Alex doesn't handle Unicode, so when Unicode
          -- character is encountered we output these values
          -- with the actual character value hidden in the state.
          | otherwise =
                -- NB: The logic behind these definitions is also reflected
                -- in basicTypes/Lexeme.hs
                -- Any changes here should likely be reflected there.

                case generalCategory c of
                  UppercaseLetter       -> upper
                  LowercaseLetter       -> lower
                  TitlecaseLetter       -> upper
                  ModifierLetter        -> suffix -- see #10196
                  OtherLetter           -> lower -- see #1103
                  NonSpacingMark        -> other_graphic
                  SpacingCombiningMark  -> other_graphic
                  EnclosingMark         -> other_graphic
                  DecimalNumber         -> digit
                  LetterNumber          -> other_graphic
                  OtherNumber           -> digit -- see #4373
                  ConnectorPunctuation  -> symbol
                  DashPunctuation       -> symbol
                  OpenPunctuation       -> other_graphic
                  ClosePunctuation      -> other_graphic
                  InitialQuote          -> other_graphic
                  FinalQuote            -> other_graphic
                  OtherPunctuation      -> symbol
                  MathSymbol            -> symbol
                  CurrencySymbol        -> symbol
                  ModifierSymbol        -> symbol
                  OtherSymbol           -> symbol
                  Space                 -> space
                  _other                -> non_graphic
