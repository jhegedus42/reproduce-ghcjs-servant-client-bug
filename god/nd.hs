{-# LANGUAGE ForeignFunctionInterface,JavaScriptFFI, CPP, QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators, DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
--{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# OPTIONS_HADDOCK not-home    #-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_HADDOCK not-home       #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances   #-}
#endif
{-# OPTIONS_HADDOCK not-home        #-}
{-# LANGUAGE ConstraintKinds        #-}
module Main (
    main
) where
import           Control.Applicative         ((<$>))
import           Data.ByteString.Char8       as BS (pack, unlines, init)
import           Data.ByteString.Conversion  (ToByteString, toByteString',
                                              FromByteString, fromByteString)
import qualified Data.CaseInsensitive        as CI
import           Data.Proxy
import           GHC.TypeLits                (KnownSymbol, symbolVal)
import qualified Network.HTTP.Types.Header   as HTTP
import           Servant.API.Header          (Header (..))
import           Data.Typeable (Typeable)
import           GHC.TypeLits  (Symbol)
import           Control.Applicative              ((*>), (<*))
import           Control.Arrow                    (left)
import           Control.Monad
import           Data.Aeson                       (FromJSON, ToJSON, encode,
                                                   parseJSON)
import           Data.Aeson.Parser                (value)
import           Data.Aeson.Types                 (parseEither)
import           Data.Attoparsec.ByteString.Char8 (endOfInput, parseOnly,
                                                   skipSpace, (<?>))
import qualified Data.ByteString                  as BS
import           Data.ByteString.Lazy             (ByteString, fromStrict,
                                                   toStrict)
import qualified Data.ByteString.Lazy             as B
import           Data.Monoid
import           Data.String.Conversions          (cs)
import qualified Data.Text                        as TextS
import qualified Data.Text.Encoding               as TextS
import qualified Data.Text.Lazy                   as TextL
import qualified Data.Text.Lazy.Encoding          as TextL
import           Data.Typeable
import           GHC.Exts                         (Constraint)
import qualified Network.HTTP.Media               as M
import           Network.URI                      (escapeURIString,
                                                   isUnreserved, unEscapeString)
import           Data.ByteString (ByteString)
import           Data.Typeable   (Typeable)
import           GHC.TypeLits    (Symbol)
import           Data.Monoid   (Monoid (..))
-
import Data.Typeable (Typeable)
import Control.Monad.Trans.Either
import Data.Proxy
import Data.Aeson
import Data.List
mport GHC.Generics
data Position = Position
  { x :: Int
  , y :: Int
  } deriving (Show, Generic)
instance FromJSON Position
instance ToJSON Position
newtype HelloMessage = HelloMessage { msg :: String }
  deriving (Show, Generic)
instance FromJSON HelloMessage
instance ToJSON HelloMessage
data ClientInfo = ClientInfo
  { name :: String
  , email :: String
  , age :: Int
  , interested_in :: [String]
instance FromJSON ClientInfo
instance ToJSON ClientInfo
data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
instance FromJSON Email
instance ToJSON Email
type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
api :: Proxy API
api = Proxy
main = do
    putStrLn "hello"
    run
position :: Int -- ^ value for "x"
         -> Int -- ^ value for "y"
         -> EitherT ServantError IO Position
hello :: Maybe String -- ^ an optional value for "name"
      -> EitherT ServantError IO HelloMessage
marketing :: ClientInfo -- ^ value for the request body
          -> EitherT ServantError IO Email
position :<|> hello :<|> marketing = client api baseUrl
baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" 8081
queries :: EitherT ServantError IO (Position, HelloMessage, Email)
queries = do
  pos <- position 10 10
  msg <- hello (Just "servant")
  em  <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
  return (pos, msg, em)
run :: IO ()
run = do
  res <- runEitherT queries
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (pos, msg, em) -> do
      print pos
      print msg
      print em
data Get (contentTypes :: [*]) a
  deriving Typeable
data Raw deriving Typeable
data (path :: k) :> a
    deriving (Typeable)
infixr 9 :>
data a :<|> b = a :<|> b
    deriving (Typeable, Eq, Show)
infixr 8 :<|>
instance (Monoid a, Monoid b) => Monoid (a :<|> b) where
    mempty = mempty :<|> mempty
    (a :<|> b) `mappend` (a' :<|> b') = (a `mappend` a') :<|> (b `mappend` b')
data Header (sym :: Symbol) a = Header a
                              | MissingHeader
                              | UndecodableHeader ByteString
    deriving (Typeable, Eq, Show, Functor)
data MatrixParam (sym :: Symbol) a
data MatrixParams (sym :: Symbol) a
data MatrixFlag (sym :: Symbol)
-- * Provided content types
data JSON deriving Typeable
data PlainText deriving Typeable
data FormUrlEncoded deriving Typeable
data OctetStream deriving Typeable
class Accept ctype where
    contentType   :: Proxy ctype -> M.MediaType
-- | @application/json@
instance Accept JSON where
    contentType _ = "application" M.// "json"
-- | @application/x-www-form-urlencoded@
instance Accept FormUrlEncoded where
    contentType _ = "application" M.// "x-www-form-urlencoded"
-- | @text/plain;charset=utf-8@
instance Accept PlainText where
    contentType _ = "text" M.// "plain" M./: ("charset", "utf-8")
-- | @application/octet-stream@
instance Accept OctetStream where
    contentType _ = "application" M.// "octet-stream"
newtype AcceptHeader = AcceptHeader BS.ByteString
    deriving (Eq, Show)
class Accept ctype => MimeRender ctype a where
    mimeRender  :: Proxy ctype -> a -> ByteString
class AllCTRender (list :: [*]) a where
    -- If the Accept header can be matched, returns (Just) a tuple of the
    -- Content-Type and response (serialization of @a@ into the appropriate
    -- mimetype).
    handleAcceptH :: Proxy list -> AcceptHeader -> a -> Maybe (ByteString, ByteString)
instance ( AllMimeRender ctyps a, IsNonEmpty ctyps
         ) => AllCTRender ctyps a where
    handleAcceptH _ (AcceptHeader accept) val = M.mapAcceptMedia lkup accept
      where pctyps = Proxy :: Proxy ctyps
            amrs = allMimeRender pctyps val
            lkup = fmap (\(a,b) -> (a, (fromStrict $ M.renderHeader a, b))) amrs
class Accept ctype => MimeUnrender ctype a where
    mimeUnrender :: Proxy ctype -> ByteString -> Either String a
class (IsNonEmpty list) => AllCTUnrender (list :: [*]) a where
    handleCTypeH :: Proxy list
                 -> ByteString     -- Content-Type header
                 -> ByteString     -- Request body
                 -> Maybe (Either String a)
instance ( AllMimeUnrender ctyps a, IsNonEmpty ctyps
         ) => AllCTUnrender ctyps a where
    handleCTypeH _ ctypeH body = M.mapContentMedia lkup (cs ctypeH)
      where lkup = allMimeUnrender (Proxy :: Proxy ctyps) body
--------------------------------------------------------------------------
-- * Utils (Internal)
-- Check that all elements of list are instances of MimeRender
class AllMimeRender (list :: [*]) a where
    allMimeRender :: Proxy list
                  -> a                              -- value to serialize
                  -> [(M.MediaType, ByteString)]    -- content-types/response pairs
instance ( MimeRender ctyp a ) => AllMimeRender '[ctyp] a where
    allMimeRender _ a = [(contentType pctyp, mimeRender pctyp a)]
        where pctyp = Proxy :: Proxy ctyp
instance ( MimeRender ctyp a
         , AllMimeRender (ctyp' ': ctyps) a
         ) => AllMimeRender (ctyp ': ctyp' ': ctyps) a where
    allMimeRender _ a = (contentType pctyp, mimeRender pctyp a)
                       :(allMimeRender pctyps a)
              pctyps = Proxy :: Proxy (ctyp' ': ctyps)
instance AllMimeRender '[] a where
    allMimeRender _ _ = []
-- Check that all elements of list are instances of MimeUnrender
class AllMimeUnrender (list :: [*]) a where
    allMimeUnrender :: Proxy list
                    -> ByteString
                    -> [(M.MediaType, Either String a)]
instance AllMimeUnrender '[] a where
    allMimeUnrender _ _ = []
instance ( MimeUnrender ctyp a
         , AllMimeUnrender ctyps a
         ) => AllMimeUnrender (ctyp ': ctyps) a where
    allMimeUnrender _ val = (contentType pctyp, mimeUnrender pctyp val)
                           :(allMimeUnrender pctyps val)
              pctyps = Proxy :: Proxy ctyps
type family IsNonEmpty (list :: [*]) :: Constraint where
    IsNonEmpty (x ': xs)   = ()
-- * MimeRender Instances
-- | `encode`
instance ToJSON a => MimeRender JSON a where
    mimeRender _ = encode
-- | @encodeFormUrlEncoded . toFormUrlEncoded@
-- Note that the @mimeUnrender p (mimeRender p x) == Right x@ law only
-- holds if every element of x is non-null (i.e., not @("", "")@)
instance ToFormUrlEncoded a => MimeRender FormUrlEncoded a where
    mimeRender _ = encodeFormUrlEncoded . toFormUrlEncoded
-- | `TextL.encodeUtf8`
instance MimeRender PlainText TextL.Text where
    mimeRender _ = TextL.encodeUtf8
-- | @fromStrict . TextS.encodeUtf8@
instance MimeRender PlainText TextS.Text where
    mimeRender _ = fromStrict . TextS.encodeUtf8
-- | @id@
instance MimeRender OctetStream ByteString where
    mimeRender _ = id
-- | `fromStrict`
instance MimeRender OctetStream BS.ByteString where
    mimeRender _ = fromStrict
-- * MimeUnrender Instances
-- | Like 'Data.Aeson.eitherDecode' but allows all JSON values instead of just
-- objects and arrays.
--
-- Will handle trailing whitespace, but not trailing junk. ie.
-- >>> eitherDecodeLenient "1 " :: Either String Int
-- Right 1
-- >>> eitherDecodeLenient "1 junk" :: Either String Int
-- Left "trailing junk after valid JSON: endOfInput"
eitherDecodeLenient :: FromJSON a => ByteString -> Either String a
eitherDecodeLenient input =
    parseOnly parser (cs input) >>= parseEither parseJSON
  where
    parser = skipSpace
          *> Data.Aeson.Parser.value
          <* skipSpace
          <* (endOfInput <?> "trailing junk after valid JSON")
-- | `eitherDecode`
instance FromJSON a => MimeUnrender JSON a where
    mimeUnrender _ = eitherDecodeLenient
-- | @decodeFormUrlEncoded >=> fromFormUrlEncoded@
instance FromFormUrlEncoded a => MimeUnrender FormUrlEncoded a where
    mimeUnrender _ = decodeFormUrlEncoded >=> fromFormUrlEncoded
-- | @left show . TextL.decodeUtf8'@
instance MimeUnrender PlainText TextL.Text where
    mimeUnrender _ = left show . TextL.decodeUtf8'
-- | @left show . TextS.decodeUtf8' . toStrict@
instance MimeUnrender PlainText TextS.Text where
    mimeUnrender _ = left show . TextS.decodeUtf8' . toStrict
-- | @Right . id@
instance MimeUnrender OctetStream ByteString where
    mimeUnrender _ = Right . id
-- | @Right . toStrict@
instance MimeUnrender OctetStream BS.ByteString where
    mimeUnrender _ = Right . toStrict
-- * FormUrlEncoded
-- | A type that can be converted to @application/x-www-form-urlencoded@
class ToFormUrlEncoded a where
  toFormUrlEncoded :: a -> [(TextS.Text, TextS.Text)]
instance ToFormUrlEncoded [(TextS.Text, TextS.Text)] where
  toFormUrlEncoded = id
-- | A type that can be converted from @application/x-www-form-urlencoded@,
-- with the possibility of failure.
class FromFormUrlEncoded a where
  fromFormUrlEncoded :: [(TextS.Text, TextS.Text)] -> Either String a
instance FromFormUrlEncoded [(TextS.Text, TextS.Text)] where
  fromFormUrlEncoded = return
encodeFormUrlEncoded :: [(TextS.Text, TextS.Text)] -> ByteString
encodeFormUrlEncoded xs =
    let escape :: TextS.Text -> ByteString
        escape = cs . escapeURIString isUnreserved . cs
        encodePair :: (TextS.Text, TextS.Text) -> ByteString
        encodePair (k, "") = escape k
        encodePair (k, v) = escape k <> "=" <> escape v
    in B.intercalate "&" $ map encodePair xs
decodeFormUrlEncoded :: ByteString -> Either String [(TextS.Text, TextS.Text)]
decodeFormUrlEncoded "" = return []
decodeFormUrlEncoded q = do
    let xs :: [TextS.Text]
        xs = TextS.splitOn "&" . cs $ q
        parsePair :: TextS.Text -> Either String (TextS.Text, TextS.Text)
        parsePair p =
            case TextS.splitOn "=" p of
                [k,v] -> return ( unescape k
                                , unescape v
                                )
                [k] -> return ( unescape k, "" )
                _ -> Left $ "not a valid pair: " <> cs p
        unescape :: TextS.Text -> TextS.Text
        unescape = cs . unEscapeString . cs . TextS.intercalate "%20" . TextS.splitOn "+"
    mapM parsePair xs
data Post (contentTypes :: [*]) a
data Capture (sym :: Symbol) a
data QueryParam (sym :: Symbol) a
    deriving Typeable
data QueryParams (sym :: Symbol) a
data QueryFlag (sym :: Symbol)
data Delete (contentTypes :: [*]) a
data Patch (contentTypes :: [*]) a
data Put (contentTypes :: [*]) a
data ReqBody (contentTypes :: [*]) a
-- | Response Header objects. You should never need to construct one directly.
-- Instead, use 'addHeader'.
data Headers ls a = Headers { getResponse :: a
                            -- ^ The underlying value of a 'Headers'
                            , getHeadersHList :: HList ls
                            -- ^ HList of headers.
                            } deriving (Functor)
data HList a where
    HNil  :: HList '[]
    HCons :: Header h x -> HList xs -> HList (Header h x ': xs)
type family HeaderValMap (f :: * -> *) (xs :: [*]) where
    HeaderValMap f '[]                = '[]
    HeaderValMap f (Header h x ': xs) = Header h (f x) ': (HeaderValMap f xs)
class BuildHeadersTo hs where
    buildHeadersTo :: [HTTP.Header] -> HList hs
    -- ^ Note: if there are multiple occurences of a header in the argument,
    -- the values are interspersed with commas before deserialization (see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.2 RFC2616 Sec 4.2>)
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
         BuildHeadersTo '[] where
    buildHeadersTo _ = HNil
         {-# OVERLAPPABLE #-}
         ( FromByteString v, BuildHeadersTo xs, KnownSymbol h, Contains h xs ~ 'False
         ) => BuildHeadersTo ((Header h v) ': xs) where
    buildHeadersTo headers =
      let wantedHeader = CI.mk . pack $ symbolVal (Proxy :: Proxy h)
          matching = snd <$> filter (\(h, _) -> h == wantedHeader) headers
      in case matching of
        [] -> MissingHeader `HCons` buildHeadersTo headers
        xs -> case fromByteString (BS.init $ BS.unlines xs) of
          Nothing -> UndecodableHeader (BS.init $ BS.unlines xs)
             `HCons` buildHeadersTo headers
          Just h   -> Header h `HCons` buildHeadersTo headers
-- * Getting
class GetHeaders ls where
    getHeaders :: ls -> [HTTP.Header]
         GetHeaders (HList '[]) where
    getHeaders _ = []
         ( KnownSymbol h, ToByteString x, GetHeaders (HList xs)
         ) => GetHeaders (HList (Header h x ': xs)) where
    getHeaders hdrs = case hdrs of
        Header val `HCons` rest -> (headerName , toByteString' val):getHeaders rest
        UndecodableHeader h `HCons` rest -> (headerName,  h) : getHeaders rest
        MissingHeader `HCons` rest -> getHeaders rest
      where headerName = CI.mk . pack $ symbolVal (Proxy :: Proxy h)
         GetHeaders (Headers '[] a) where
         ( KnownSymbol h, GetHeaders (HList rest), ToByteString v
         ) => GetHeaders (Headers (Header h v ': rest) a) where
    getHeaders hs = getHeaders $ getHeadersHList hs
-- * Adding
-- We need all these fundeps to save type inference
class AddHeader h v orig new
    | h v orig -> new, new -> h, new -> v, new -> orig where
  addHeader :: v -> orig -> new  -- ^ N.B.: The same header can't be added multiple times
         ( KnownSymbol h, ToByteString v, Contains h (fst ': rest) ~ 'False
         ) => AddHeader h v (Headers (fst ': rest)  a) (Headers (Header h v  ': fst ': rest) a) where
    addHeader a (Headers resp heads) = Headers resp (HCons (Header a) heads)
         ( KnownSymbol h, ToByteString v
         , new ~ (Headers '[Header h v] a)
         ) => AddHeader h v a new where
    addHeader a resp = Headers resp (HCons (Header a) HNil)
type family Contains x xs where
    Contains x ((Header x a) ': xs) = 'True
    Contains x ((Header y a) ': xs) = Contains x xs
    Contains x '[]                  = 'False
-- | Type safe generation of internal links.
-- Given an API with a few endpoints:
-- >>> :set -XDataKinds -XTypeFamilies -XTypeOperators
-- >>> import Servant.API
-- >>> import Servant.Utils.Links
-- >>> import Data.Proxy
-- >>>
-- >>> type Hello = "hello" :> Get '[JSON] Int
-- >>> type Bye   = "bye"   :> QueryParam "name" String :> Delete '[JSON] ()
-- >>> type API   = Hello :<|> Bye
-- >>> let api = Proxy :: Proxy API
-- It is possible to generate links that are guaranteed to be within 'API' with
-- 'safeLink'. The first argument to 'safeLink' is a type representing the API
-- you would like to restrict links to. The second argument is the destination
-- endpoint you would like the link to point to, this will need to end with a
-- verb like GET or POST. Further arguments may be required depending on the
-- type of the endpoint. If everything lines up you will get a 'URI' out the
-- other end.
-- You may omit 'QueryParam's and the like should you not want to provide them,
-- but types which form part of the URL path like 'Capture' must be included.
-- The reason you may want to omit 'QueryParam's is that safeLink is a bit
-- magical: if parameters are included that could take input it will return a
-- function that accepts that input and generates a link. This is best shown
-- with an example. Here, a link is generated with no parameters:
-- >>> let hello = Proxy :: Proxy ("hello" :> Get '[JSON] Int)
-- >>> print (safeLink api hello :: URI)
-- hello
-- If the API has an endpoint with parameters then we can generate links with
-- or without those:
-- >>> let with = Proxy :: Proxy ("bye" :> QueryParam "name" String :> Delete '[JSON] ())
-- >>> print $ safeLink api with "Hubert"
-- bye?name=Hubert
-- >>> let without = Proxy :: Proxy ("bye" :> Delete '[JSON] ())
-- >>> print $ safeLink api without
-- bye
-- If you would like create a helper for generating links only within that API,
-- you can partially apply safeLink if you specify a correct type signature
-- like so:
-- >>> :set -XConstraintKinds
-- >>> :{
-- >>> let apiLink :: (IsElem endpoint API, HasLink endpoint)
-- >>>             => Proxy endpoint -> MkLink endpoint
-- >>>     apiLink = safeLink api
-- >>> :}
-- Attempting to construct a link to an endpoint that does not exist in api
-- will result in a type error like this:
-- >>> let bad_link = Proxy :: Proxy ("hello" :> Delete '[JSON] ())
-- >>> safeLink api bad_link
-- ...
--     Could not deduce (Or
--                         (IsElem' (Delete '[JSON] ()) (Get '[JSON] Int))
--                         (IsElem'
--                            ("hello" :> Delete '[JSON] ())
--                            ("bye" :> (QueryParam "name" String :> Delete '[JSON] ()))))
--       arising from a use of ‘safeLink’
--     In the expression: safeLink api bad_link
--     In an equation for ‘it’: it = safeLink api bad_link
--  This error is essentially saying that the type family couldn't find
--  bad_link under api after trying the open (but empty) type family
--  `IsElem'` as a last resort.
module Servant.Utils.Links (
  -- * Building and using safe links
  --
  -- | Note that 'URI' is Network.URI.URI from the network-uri package.
    safeLink
  , URI(..)
  -- * Adding custom types
  , HasLink(..)
  , linkURI
  , Link
  , IsElem'
  -- * Illustrative exports
  , IsElem
  , Or
import Data.Proxy ( Proxy(..) )
import Data.Text (Text, unpack)
import Data.Monoid ( Monoid(..), (<>) )
#else
import Data.Monoid ( (<>) )
import Network.URI ( URI(..), escapeURIString, isUnreserved )
import GHC.TypeLits ( KnownSymbol, symbolVal )
import GHC.Exts(Constraint)
import Servant.Common.Text
import Servant.API.Capture ( Capture )
import Servant.API.ReqBody ( ReqBody )
import Servant.API.QueryParam ( QueryParam, QueryParams, QueryFlag )
import Servant.API.MatrixParam ( MatrixParam, MatrixParams, MatrixFlag )
import Servant.API.Header ( Header )
import Servant.API.Get ( Get )
import Servant.API.Post ( Post )
import Servant.API.Put ( Put )
import Servant.API.Delete ( Delete )
import Servant.API.Sub ( type (:>) )
import Servant.API.Raw ( Raw )
import Servant.API.Alternative ( type (:<|>) )
-- | A safe link datatype.
-- The only way of constructing a 'Link' is using 'safeLink', which means any
-- 'Link' is guaranteed to be part of the mentioned API.
data Link = Link
  { _segments :: [String] -- ^ Segments of "foo/bar" would be ["foo", "bar"]
  , _queryParams :: [Param Query]
  } deriving Show
-- | If either a or b produce an empty constraint, produce an empty constraint.
type family Or (a :: Constraint) (b :: Constraint) :: Constraint where
    -- This works because of:
    -- https://ghc.haskell.org/trac/ghc/wiki/NewAxioms/CoincidentOverlap
    Or () b       = ()
    Or a ()       = ()
-- | If both a or b produce an empty constraint, produce an empty constraint.
type family And (a :: Constraint) (b :: Constraint) :: Constraint where
    And () ()     = ()
-- | You may use this type family to tell the type checker that your custom
-- type may be skipped as part of a link. This is useful for things like
-- 'QueryParam' that are optional in a URI and do not affect them if they are
-- omitted.
-- >>> data CustomThing
-- >>> type instance IsElem' e (CustomThing :> s) = IsElem e s
-- Note that 'IsElem' is called, which will mutually recurse back to `IsElem'`
-- if it exhausts all other options again.
-- Once you have written a HasLink instance for CustomThing you are ready to
-- go.
type family IsElem' a s :: Constraint
-- | Closed type family, check if endpoint is within api
type family IsElem endpoint api :: Constraint where
    IsElem e (sa :<|> sb)                   = Or (IsElem e sa) (IsElem e sb)
    IsElem (e :> sa) (e :> sb)              = IsElem sa sb
    IsElem sa (Header x :> sb)              = IsElem sa sb
    IsElem sa (ReqBody y x :> sb)           = IsElem sa sb
    IsElem (Capture z y :> sa) (Capture x y :> sb)
                                            = IsElem sa sb
    IsElem sa (QueryParam x y :> sb)        = IsElem sa sb
    IsElem sa (QueryParams x y :> sb)       = IsElem sa sb
    IsElem sa (QueryFlag x :> sb)           = IsElem sa sb
    IsElem sa (MatrixParam x y :> sb)       = IsElem sa sb
    IsElem sa (MatrixParams x y :> sb)      = IsElem sa sb
    IsElem sa (MatrixFlag x :> sb)          = IsElem sa sb
    IsElem (Get ct typ) (Get ct' typ)       = IsSubList ct ct'
    IsElem (Post ct typ) (Post ct' typ)     = IsSubList ct ct'
    IsElem (Put ct typ) (Put ct' typ)       = IsSubList ct ct'
    IsElem (Delete ct typ) (Delete ct' typ) = IsSubList ct ct'
    IsElem e e                              = ()
    IsElem e a                              = IsElem' e a
type family IsSubList a b :: Constraint where
    IsSubList '[] b          = ()
    IsSubList '[x] (x ': xs) = ()
    IsSubList '[x] (y ': ys) = IsSubList '[x] ys
    IsSubList (x ': xs) y    = IsSubList '[x] y `And` IsSubList xs y
-- Phantom types for Param
data Matrix
data Query
-- | Query/Matrix param
data Param a
    = SingleParam    String Text
    | ArrayElemParam String Text
    | FlagParam      String
  deriving Show
addSegment :: String -> Link -> Link
addSegment seg l = l { _segments = _segments l <> [seg] }
addQueryParam :: Param Query -> Link -> Link
addQueryParam qp l =
    l { _queryParams = _queryParams l <> [qp] }
-- Not particularly efficient for many updates. Something to optimise if it's
-- a problem.
addMatrixParam :: Param Matrix -> Link -> Link
addMatrixParam param l = l { _segments = f (_segments l) }
    f [] = []
    f xs = init xs <> [g (last xs)]
    -- Modify the segment at the "top" of the stack
    g :: String -> String
    g seg =
        case param of
            SingleParam k v    -> seg <> ";" <> k <> "=" <> escape (unpack v)
            ArrayElemParam k v -> seg <> ";" <> k <> "[]=" <> escape (unpack v)
            FlagParam k        -> seg <> ";" <> k
linkURI :: Link -> URI
linkURI (Link segments q_params) =
    URI mempty  -- No scheme (relative)
        Nothing -- Or authority (relative)
        (intercalate "/" segments)
        (makeQueries q_params) mempty
    makeQueries :: [Param Query] -> String
    makeQueries [] = ""
    makeQueries xs =
        "?" <> intercalate "&" (fmap makeQuery xs)
    makeQuery :: Param Query -> String
    makeQuery (ArrayElemParam k v) = escape k <> "[]=" <> escape (unpack v)
    makeQuery (SingleParam k v)    = escape k <> "=" <> escape (unpack v)
    makeQuery (FlagParam k)        = escape k
escape :: String -> String
escape = escapeURIString isUnreserved
-- | Create a valid (by construction) relative URI with query params.
-- This function will only typecheck if `endpoint` is part of the API `api`
safeLink
    :: forall endpoint api. (IsElem endpoint api, HasLink endpoint)
    => Proxy api      -- ^ The whole API that this endpoint is a part of
    -> Proxy endpoint -- ^ The API endpoint you would like to point to
    -> MkLink endpoint
safeLink _ endpoint = toLink endpoint (Link mempty mempty)
-- | Construct a toLink for an endpoint.
class HasLink endpoint where
    type MkLink endpoint
    toLink :: Proxy endpoint -- ^ The API endpoint you would like to point to
         -> Link
         -> MkLink endpoint
-- Naked symbol instance
instance (KnownSymbol sym, HasLink sub) => HasLink (sym :> sub) where
    type MkLink (sym :> sub) = MkLink sub
    toLink _ =
        toLink (Proxy :: Proxy sub) . addSegment seg
      where
        seg = symbolVal (Proxy :: Proxy sym)
-- QueryParam instances
instance (KnownSymbol sym, ToText v, HasLink sub)
    => HasLink (QueryParam sym v :> sub) where
    type MkLink (QueryParam sym v :> sub) = v -> MkLink sub
    toLink _ l v =
        toLink (Proxy :: Proxy sub)
             (addQueryParam (SingleParam k (toText v)) l)
        k :: String
        k = symbolVal (Proxy :: Proxy sym)
    => HasLink (QueryParams sym v :> sub) where
    type MkLink (QueryParams sym v :> sub) = [v] -> MkLink sub
    toLink _ l =
        toLink (Proxy :: Proxy sub) .
            foldl' (\l' v -> addQueryParam (ArrayElemParam k (toText v)) l') l
instance (KnownSymbol sym, HasLink sub)
    => HasLink (QueryFlag sym :> sub) where
    type MkLink (QueryFlag sym :> sub) = Bool -> MkLink sub
    toLink _ l False =
        toLink (Proxy :: Proxy sub) l
    toLink _ l True =
        toLink (Proxy :: Proxy sub) $ addQueryParam (FlagParam k) l
-- MatrixParam instances
    => HasLink (MatrixParam sym v :> sub) where
    type MkLink (MatrixParam sym v :> sub) = v -> MkLink sub
        toLink (Proxy :: Proxy sub) $
            addMatrixParam (SingleParam k (toText v)) l
    => HasLink (MatrixParams sym v :> sub) where
    type MkLink (MatrixParams sym v :> sub) = [v] -> MkLink sub
            foldl' (\l' v -> addMatrixParam (ArrayElemParam k (toText v)) l') l
    => HasLink (MatrixFlag sym :> sub) where
    type MkLink (MatrixFlag sym :> sub) = Bool -> MkLink sub
        toLink (Proxy :: Proxy sub) $ addMatrixParam (FlagParam k) l
-- Misc instances
instance HasLink sub => HasLink (ReqBody ct a :> sub) where
    type MkLink (ReqBody ct a :> sub) = MkLink sub
    toLink _ = toLink (Proxy :: Proxy sub)
instance (ToText v, HasLink sub)
    => HasLink (Capture sym v :> sub) where
    type MkLink (Capture sym v :> sub) = v -> MkLink sub
            addSegment (escape . unpack $ toText v) l
instance HasLink sub => HasLink (Header sym a :> sub) where
    type MkLink (Header sym a :> sub) = MkLink sub
-- Verb (terminal) instances
instance HasLink (Get y r) where
    type MkLink (Get y r) = URI
    toLink _ = linkURI
instance HasLink (Post y r) where
    type MkLink (Post y r) = URI
instance HasLink (Put y r) where
    type MkLink (Put y r) = URI
instance HasLink (Delete y r) where
    type MkLink (Delete y r) = URI
instance HasLink Raw where
    type MkLink Raw = URI
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
module Servant.Common.Req where
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Data.ByteString.Lazy hiding (pack, filter, map, null, elem, unpack)
import Data.ByteString.Char8 (unpack, pack)
import qualified Data.ByteString as BS
import Data.IORef
import Data.String
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Typeable
import Data.Primitive.ByteArray
import           Data.Primitive.Addr
import           Data.ByteString.Unsafe (unsafePackAddressLen)
import Network.HTTP.Media
import Network.HTTP.Types
import Network.URI
import Servant.API.ContentTypes
import Servant.Common.BaseUrl
import System.IO.Unsafe
import GHCJS.Foreign (jsTrue)
import GHCJS.Foreign.Callback (Callback (..)
                              , OnBlocked(..)
                              , syncCallback)
import Data.JSString (JSString)
import qualified Data.JSString as JSString
import GHCJS.Marshal
import GHCJS.Prim --hiding (fromJSString, toJSString)
import Control.Concurrent.MVar
import Data.List.Split
import Data.Maybe
import Data.CaseInsensitive
import Data.Char
import Unsafe.Coerce
data ServantError
  = FailureResponse
    { responseStatus            :: Status
    , responseContentType       :: MediaType
    , responseBody              :: ByteString
    }
  | DecodeFailure
    { decodeError               :: String
  | UnsupportedContentType
    { responseContentType       :: MediaType
  | InvalidContentTypeHeader
    { responseContentTypeHeader :: ByteString
  deriving (Show, Typeable)
instance Exception ServantError
data ForeignRetention
  = NeverRetain                   -- ^ do not retain data unless the callback is directly
                                  --   referenced by a Haskell thread.
  | AlwaysRetain                  -- ^ retain references indefinitely, until `freeCallback`
                                  --   is called (the callback will be kept in memory until it's freed)
  | DomRetain JSRef               -- ^ retain data as long as the `JSRef` is a DOM element in
                                  --   `window.document` or in a DOM tree referenced by a Haskell
                                  --    thread.
data Req = Req
  { reqPath   :: String
  , qs        :: QueryText
  , reqBody   :: Maybe (ByteString, MediaType)
  , reqAccept :: [MediaType]
  , headers   :: [(String, Text)]
  }
defReq :: Req
defReq = Req "" [] Nothing [] []
appendToPath :: String -> Req -> Req
appendToPath p req =
  req { reqPath = reqPath req ++ "/" ++ p }
appendToMatrixParams :: String
                     -> Maybe String
                     -> Req
appendToMatrixParams pname pvalue req =
  req { reqPath = reqPath req ++ ";" ++ pname ++ maybe "" ("=" ++) pvalue }
appendToQueryString :: Text       -- ^ param name
                    -> Maybe Text -- ^ param value
                    -> Req
appendToQueryString pname pvalue req =
  req { qs = qs req ++ [(pname, pvalue)]
      }
addHeader :: ToText a => String -> a -> Req -> Req
addHeader name val req = req { headers = headers req
                                      ++ [(name, toText val)]
                             }
setRQBody :: ByteString -> MediaType -> Req -> Req
setRQBody b t req = req { reqBody = Just (b, t) }
displayHttpRequest :: Method -> String
displayHttpRequest httpmethod = "HTTP " ++ cs httpmethod ++ " request"
performRequest :: Method -> Req -> (Int -> Bool) -> BaseUrl
               -> EitherT ServantError IO ( Int, ByteString, MediaType
                                          , [HTTP.Header])
performRequest reqMethod req isWantedStatus reqHost = do
  eResp <- liftIO $ makeRequest reqMethod req isWantedStatus reqHost
  case eResp of
    (Left err) -> left err
    (Right (status_code, hrds, body)) -> do
      ct <- case lookup "Content-Type" hrds of
                 Nothing -> pure $ "application"//"octet-stream"
                 Just t -> case parseAccept t of
                   Nothing -> left $ InvalidContentTypeHeader (cs t) $ fromStrict body
                   Just t' -> pure t'
      return (status_code, fromStrict body, ct, hrds)
performRequestCT :: MimeUnrender ct result =>
  Proxy ct -> Method -> Req -> [Int] -> BaseUrl -> EitherT ServantError IO ([HTTP.Header], result)
performRequestCT ct reqMethod req wantedStatus reqHost = do
  let acceptCT = contentType ct
  (_status, respBody, respCT, hrds) <-
    performRequest reqMethod (req { reqAccept = [acceptCT] }) (`elem` wantedStatus) reqHost
  unless (matches respCT (acceptCT)) $ left $ UnsupportedContentType respCT respBody
  case mimeUnrender ct respBody of
    Left err -> left $ DecodeFailure err respCT respBody
    Right val -> return (hrds, val)
performRequestNoBody :: Method -> Req -> [Int] -> BaseUrl -> EitherT ServantError IO ()
performRequestNoBody reqMethod req wantedStatus reqHost = do
  _ <- performRequest reqMethod req (`elem` wantedStatus) reqHost
  return ()
--data XMLHttpRequest
-- foreign import javascript unsafe "new XMLHttpRequest()"
--   jsXhrRequest :: IO JSRef
--   jsXhrRequestString :: IO JSString
-- foreign import javascript unsafe "$1.open($2, $3, $4)"
--   jsXhrOpen :: JSRef -> JSRef -> JSRef -> JSRef -> IO ()
-- foreign import javascript unsafe "$1.send()"
--   jsXhrSend :: JSRef ->  IO ()
-- foreign import javascript unsafe "$1.send($2)"
--   jsXhrSendWith :: JSRef -> JSRef -> IO ()
-- foreign import javascript unsafe "$1.onreadystatechange = $2"
--   jsXhrOnReadyStateChange:: JSRef -> Callback (IO ()) -> IO ()
-- foreign import javascript unsafe "$1.readyState"
--   jsXhrReadyState:: JSRef -> IO JSRef
-- foreign import javascript unsafe "$1.responseText"
--   jsXhrResponseText:: JSRef -> IO JSString
-- foreign import javascript unsafe "$1.response"
--   jsXhrResponse:: JSRef -> IO JSRef
-- foreign import javascript unsafe "$1.responseType = $2"
--   jsXhrResponseType:: JSRef -> JSString -> IO ()
-- foreign import javascript unsafe "$1.status"
--   jsXhrStatus:: JSRef -> IO JSRef
-- foreign import javascript unsafe "$1.getAllResponseHeaders()"
--   jsXhrResponseHeaders :: JSString -> IO JSRef
-- foreign import javascript unsafe "$1.setRequestHeader($2, $3)"
--   jsXhrSetRequestHeader :: JSRef -> JSString -> JSString -> IO ()
-- foreign import javascript unsafe "$1.statusText"
--   jsXhrGetStatusText :: JSRef -> IO JSString
-- foreign import javascript unsafe "xh = $1"
--   jsDebugXhr :: JSRef -> IO ()
foreign import javascript unsafe "new XMLHttpRequest()"
  jsXhrRequest :: IO JSRef
foreign import javascript unsafe "$1.open($2, $3, $4)"
  jsXhrOpen :: JSRef -> JSString -> JSString -> JSRef -> IO ()
foreign import javascript unsafe "$1.send()"
  jsXhrSend :: JSRef -> IO ()
foreign import javascript unsafe "$1.send($2)"
  jsXhrSendWith :: JSRef -> JSRef -> IO ()
foreign import javascript unsafe "$1.onreadystatechange = $2"
  jsXhrOnReadyStateChange:: JSRef -> Callback (IO ()) -> IO ()
foreign import javascript unsafe "$1.readyState"
  jsXhrReadyState:: JSRef -> IO JSRef
foreign import javascript unsafe "$1.responseText"
  jsXhrResponseText:: JSRef -> IO JSString
foreign import javascript unsafe "$1.response"
  jsXhrResponse:: JSRef -> IO JSRef
foreign import javascript unsafe "$1.responseType = $2"
  jsXhrResponseType:: JSRef -> JSString -> IO ()
foreign import javascript unsafe "$1.status"
  jsXhrStatus:: JSRef -> IO JSRef
foreign import javascript unsafe "$1.getAllResponseHeaders()"
  jsXhrResponseHeaders :: JSRef -> IO JSString
foreign import javascript unsafe "$1.setRequestHeader($2, $3)"
  jsXhrSetRequestHeader :: JSRef -> JSString -> JSString -> IO ()
foreign import javascript unsafe "$1.statusText"
  jsXhrGetStatusText :: JSRef -> IO JSString
foreign import javascript unsafe "xh = $1"
  jsDebugXhr :: JSRef -> IO ()
foreign import javascript safe "h$wrapBuffer($3, true, $1, $2)"
  js_wrapBuffer :: Int -> Int -> JSRef -> IO JSRef
foreign import javascript unsafe "h$release($1)"
  js_release :: Callback (IO ()) -> IO ()
xhrResponseHeaders :: JSRef -> IO [HTTP.Header]
xhrResponseHeaders jReq = do
  (headers :: JSString) <- jsXhrResponseHeaders jReq
  let headersStrings = T.lines . T.pack . JSString.unpack $ headers
  return $ catMaybes $ buildHeader <$> headersStrings
buildHeader :: Text -> Maybe HTTP.Header
buildHeader xs = parseXs $ splitStr xs
  where splitStr = T.splitOn (":")
        parseXs :: [Text] -> Maybe HTTP.Header
        parseXs (c:cs) = Just (mk $ encodeUtf8 $ T.strip c, encodeUtf8 $ T.strip $ T.concat cs)
        parseXs _ = Nothing
bufferByteString :: Int        -- ^ offset from the start in bytes
                 -> Int        -- ^ length in bytes (use zero or a negative number to get the whole ArrayBuffer)
                 -> JSRef
                 -> IO BS.ByteString
bufferByteString offset length buf = do
  (ByteArray ba) <- wrapBuffer offset length buf
  byteArrayByteString ba
byteArrayByteString :: ByteArray# -> IO BS.ByteString
byteArrayByteString arr =
#ifdef ghcjs_HOST_OS
  let ba        = ByteArray arr
      !(Addr a) = byteArrayContents ba
  in  unsafePackAddressLen (sizeofByteArray ba) a
  error "GHCJS.Foreign.byteArrayToByteString: not JS"
wrapBuffer :: Int          -- ^ offset from the start in bytes, if this is not a multiple of 8,
                           --   not all types can be read from the ByteArray#
           -> Int          -- ^ length in bytes (use zero or a negative number to use the whole ArrayBuffer)
           -> JSRef        -- ^ JavaScript ArrayBuffer object
           -> IO ByteArray -- ^ result
wrapBuffer offset size buf = unsafeCoerce <$> js_wrapBuffer offset size buf
{-# INLINE wrapBuffer #-}
makeRequest :: Method -> Req -> (Int -> Bool) -> BaseUrl -> IO (Either ServantError (Int, [HTTP.Header], BS.ByteString))
makeRequest method req isWantedStatus bUrl = do
  jRequest <- jsXhrRequest
  let url = JSString.pack . show  $ buildUrl req bUrl
      methodText = JSString.pack $ unpack method
  jsXhrOpen jRequest methodText url jsTrue
  jsXhrResponseType jRequest "arraybuffer"
  resp <- newEmptyMVar
  cb <- syncCallback ThrowWouldBlock $ do
    r <- jsXhrReadyState jRequest :: IO JSRef
    state <- fromJSRef r
    when ((state :: Maybe Int) == Just 4) $ do
      statusCode <- fromMaybe (-1) <$> (fromJSRef =<< jsXhrStatus jRequest)
      if (statusCode >= 200 && statusCode < 300)
        then do
          bsResp <- bufferByteString 0 0 =<< jsXhrResponse jRequest
          headers <- xhrResponseHeaders jRequest
          putMVar resp $ Right (statusCode, headers, bsResp)
        else do
          bsStatusText <- jsXhrGetStatusText jRequest
          putMVar resp $ Left $ FailureResponse (mkStatus statusCode .
                                                       pack . JSString.unpack $ bsStatusText) undefined undefined
  jsXhrOnReadyStateChange jRequest cb
  case reqBody req of
    Nothing -> jsXhrSend jRequest
    (Just (body, mediaType)) -> do
      jsXhrSetRequestHeader jRequest "Content-Type" $ JSString.pack $ show mediaType
      b <- toJSRef (decodeUtf8 $ toStrict body)
      jsXhrSendWith jRequest b
  res <- takeMVar resp
  release cb
  return res
release :: Callback (IO ()) -- ^ the callback
                 -> IO ()
release = js_release
buildUrl :: Req -> BaseUrl -> URI
buildUrl req@(Req path qText mBody rAccept hs) (BaseUrl scheme host port) =
  nullURI {
    uriScheme = schemeText,
    uriAuthority = Just $ URIAuth "" host portText,
    uriPath = path,
    uriQuery = buildQuery req
  where schemeText = case scheme of
                      Http -> "http:"
                      Https -> "https:"
        portText = ":" <> (show port)
        buildQuery request = unpack $ renderQuery True $ queryTextToQuery qText
module Servant.Common.Text
  ( FromText(..)
  , ToText(..)
  ) where
import           Control.Applicative     ((<$>))
import           Data.Int                (Int16, Int32, Int64, Int8)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           Data.Text.Read          (Reader, decimal, rational, signed)
import           Data.Word               (Word16, Word32, Word64, Word8
        , Word
        )
-- | For getting values from url captures and query string parameters
-- Instances should obey:
-- > fromText (toText a) == Just a
class FromText a where
  fromText :: Text -> Maybe a
-- | For putting values in paths and query string parameters
class ToText a where
  toText :: a -> Text
instance FromText Text where
  fromText = Just
instance ToText Text where
  toText = id
instance FromText String where
  fromText = Just . cs
instance ToText String where
  toText = cs
-- |
-- >>> fromText ("true"::Text) :: Maybe Bool
-- Just True
-- >>> fromText ("false"::Text) :: Maybe Bool
-- Just False
-- >>> fromText ("anything else"::Text) :: Maybe Bool
-- Nothing
instance FromText Bool where
  fromText "true"  = Just True
  fromText "false" = Just False
  fromText _       = Nothing
-- >>> toText True
-- "true"
-- >>> toText False
-- "false"
instance ToText Bool where
  toText True  = "true"
  toText False = "false"
instance FromText Int where
  fromText = runReader (signed decimal)
instance ToText Int where
  toText = cs . show
instance FromText Int8 where
instance ToText Int8 where
instance FromText Int16 where
instance ToText Int16 where
instance FromText Int32 where
instance ToText Int32 where
instance FromText Int64 where
instance ToText Int64 where
instance FromText Word where
  fromText = runReader decimal
instance ToText Word where
instance FromText Word8 where
instance ToText Word8 where
instance FromText Word16 where
instance ToText Word16 where
instance FromText Word32 where
instance ToText Word32 where
instance FromText Word64 where
instance ToText Word64 where
instance FromText Integer where
instance ToText Integer where
instance FromText Double where
  fromText x = fromRational <$> runReader rational x
instance ToText Double where
instance FromText Float where
  -- Double is more practically accurate due to weird rounding when using
  -- rational. We convert to double and then convert to Float.
instance ToText Float where
runReader :: Reader a -> Text -> Maybe a
runReader reader t = either (const Nothing) (Just . fst) $ reader t
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ViewPatterns       #-}
module Servant.Common.BaseUrl (
  -- * types
    BaseUrl (..)
  , InvalidBaseUrlException
  , Scheme (..)
  -- * functions
  , parseBaseUrl
  , showBaseUrl
import Control.Monad.Catch (MonadThrow, throwM, Exception)
import GHC.Generics
import Safe
import Text.Read
-- | URI scheme to use
data Scheme =
    Http  -- ^ http://
  | Https -- ^ https://
  deriving (Show, Eq, Ord, Generic)
-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
data BaseUrl = BaseUrl
  { baseUrlScheme :: Scheme -- ^ URI scheme to use
  , baseUrlHost :: String   -- ^ host (eg "haskell.org")
  , baseUrlPort :: Int      -- ^ port (eg 80)
  } deriving (Show, Eq, Ord, Generic)
showBaseUrl :: BaseUrl -> String
showBaseUrl (BaseUrl urlscheme host port) =
  schemeString ++ "//" ++ host ++ portString
    where
      schemeString = case urlscheme of
        Http  -> "http:"
        Https -> "https:"
      portString = case (urlscheme, port) of
        (Http, 80) -> ""
        (Https, 443) -> ""
        _ -> ":" ++ show port
data InvalidBaseUrlException = InvalidBaseUrlException String deriving (Show, Typeable)
instance Exception InvalidBaseUrlException
parseBaseUrl :: MonadThrow m => String -> m BaseUrl
parseBaseUrl s = case parseURI (removeTrailingSlash s) of
  -- This is a rather hacky implementation and should be replaced with something
  -- implemented in attoparsec (which is already a dependency anyhow (via aeson)).
  Just (URI "http:" (Just (URIAuth "" host (':' : (readMaybe -> Just port)))) "" "" "") ->
    return (BaseUrl Http host port)
  Just (URI "http:" (Just (URIAuth "" host "")) "" "" "") ->
    return (BaseUrl Http host 80)
  Just (URI "https:" (Just (URIAuth "" host (':' : (readMaybe -> Just port)))) "" "" "") ->
    return (BaseUrl Https host port)
  Just (URI "https:" (Just (URIAuth "" host "")) "" "" "") ->
    return (BaseUrl Https host 443)
  _ -> if "://" `isInfixOf` s
    then throwM (InvalidBaseUrlException $ "Invalid base URL: " ++ s)
    else parseBaseUrl ("http://" ++ s)
 where
  removeTrailingSlash str = case lastMay str of
    Just '/' -> init str
    _ -> str
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE OverlappingInstances #-}
-- | This module provides 'client' which can automatically generate
-- querying functions for each endpoint just from the type representing your
-- API.
module Servant.Client
  ( client
  , HasClient(..)
  , ServantError(..)
  , module Servant.Common.BaseUrl
import           Control.Applicative        ((<$>))
import           Control.Monad.Trans.Either
import           Data.ByteString.Lazy       (ByteString)
import           Data.List
import           Data.String.Conversions
import           Data.Text                  (unpack)
import           GHC.TypeLits
import           Network.HTTP.Media
import qualified Network.HTTP.Types         as H
import qualified Network.HTTP.Types.Header  as HTTP
import           Servant.API
import           Servant.Common.BaseUrl
import           Servant.Common.Req
-- * Accessing APIs as a Client
-- | 'client' allows you to produce operations to query an API from a client.
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody '[JSON] Book :> Post Book -- POST /books
-- >
-- > myApi :: Proxy MyApi
-- > myApi = Proxy
-- > getAllBooks :: EitherT String IO [Book]
-- > postNewBook :: Book -> EitherT String IO Book
-- > (getAllBooks :<|> postNewBook) = client myApi host
-- >   where host = BaseUrl Http "localhost" 8080
client :: HasClient layout => Proxy layout -> BaseUrl -> Client layout
client p baseurl = clientWithRoute p defReq baseurl
-- | This class lets us define how each API combinator
-- influences the creation of an HTTP request. It's mostly
-- an internal class, you can just use 'client'.
class HasClient layout where
  type Client layout :: *
  clientWithRoute :: Proxy layout -> Req -> BaseUrl -> Client layout
{-type Client layout = Client layout-}
-- | A client querying function for @a ':<|>' b@ will actually hand you
--   one function for querying @a@ and another one for querying @b@,
--   stitching them together with ':<|>', which really is just like a pair.
instance (HasClient a, HasClient b) => HasClient (a :<|> b) where
  type Client (a :<|> b) = Client a :<|> Client b
  clientWithRoute Proxy req baseurl =
    clientWithRoute (Proxy :: Proxy a) req baseurl :<|>
    clientWithRoute (Proxy :: Proxy b) req baseurl
-- | If you use a 'Capture' in one of your endpoints in your API,
-- the corresponding querying function will automatically take
-- an additional argument of the type specified by your 'Capture'.
-- That function will take care of inserting a textual representation
-- of this value at the right place in the request path.
-- You can control how values for this type are turned into
-- text by specifying a 'ToText' instance for your type.
-- Example:
-- > type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
-- > getBook :: Text -> EitherT String IO Book
-- > getBook = client myApi host
-- > -- then you can just use "getBook" to query that endpoint
instance (KnownSymbol capture, ToText a, HasClient sublayout)
      => HasClient (Capture capture a :> sublayout) where
  type Client (Capture capture a :> sublayout) =
    a -> Client sublayout
  clientWithRoute Proxy req baseurl val =
    clientWithRoute (Proxy :: Proxy sublayout)
                    (appendToPath p req)
                    baseurl
    where p = unpack (toText val)
-- | If you have a 'Delete' endpoint in your API, the client
-- side querying function that is created when calling 'client'
-- will just require an argument that specifies the scheme, host
-- and port to send the request to.
  -- See https://downloads.haskell.org/~ghc/7.8.2/docs/html/users_guide/type-class-extensions.html#undecidable-instances
  (MimeUnrender ct a, cts' ~ (ct ': cts)) => HasClient (Delete cts' a) where
  type Client (Delete cts' a) = EitherT ServantError IO a
    snd <$> performRequestCT (Proxy :: Proxy ct) H.methodDelete req [200, 202] baseurl
-- | If you have a 'Delete xs ()' endpoint, the client expects a 204 No Content
-- HTTP header.
  HasClient (Delete cts ()) where
  type Client (Delete cts ()) = EitherT ServantError IO ()
    void $ performRequestNoBody H.methodDelete req [204] baseurl
-- | If you have a 'Delete xs (Headers ls x)' endpoint, the client expects the
-- corresponding headers.
  ( MimeUnrender ct a, BuildHeadersTo ls, cts' ~ (ct ': cts)
  ) => HasClient (Delete cts' (Headers ls a)) where
  type Client (Delete cts' (Headers ls a)) = EitherT ServantError IO (Headers ls a)
  clientWithRoute Proxy req baseurl = do
    (hdrs, resp) <- performRequestCT (Proxy :: Proxy ct) H.methodDelete req [200, 202] baseurl
    return $ Headers { getResponse = resp
                     , getHeadersHList = buildHeadersTo hdrs
                     }
-- | If you have a 'Get' endpoint in your API, the client
  (MimeUnrender ct result) => HasClient (Get (ct ': cts) result) where
  type Client (Get (ct ': cts) result) = EitherT ServantError IO result
    snd <$> performRequestCT (Proxy :: Proxy ct) H.methodGet req [200, 203] baseurl
-- | If you have a 'Get xs ()' endpoint, the client expects a 204 No Content
-- HTTP status.
  HasClient (Get (ct ': cts) ()) where
  type Client (Get (ct ': cts) ()) = EitherT ServantError IO ()
    performRequestNoBody H.methodGet req [204] baseurl
-- | If you have a 'Get xs (Headers ls x)' endpoint, the client expects the
  ( MimeUnrender ct a, BuildHeadersTo ls
  ) => HasClient (Get (ct ': cts) (Headers ls a)) where
  type Client (Get (ct ': cts) (Headers ls a)) = EitherT ServantError IO (Headers ls a)
    (hdrs, resp) <- performRequestCT (Proxy :: Proxy ct) H.methodGet req [200, 203, 204] baseurl
-- | If you use a 'Header' in one of your endpoints in your API,
-- an additional argument of the type specified by your 'Header',
-- wrapped in Maybe.
-- That function will take care of encoding this argument as Text
-- in the request headers.
-- All you need is for your type to have a 'ToText' instance.
-- > newtype Referer = Referer { referrer :: Text }
-- >   deriving (Eq, Show, Generic, FromText, ToText)
-- >            -- GET /view-my-referer
-- > type MyApi = "view-my-referer" :> Header "Referer" Referer :> Get '[JSON] Referer
-- > viewReferer :: Maybe Referer -> EitherT String IO Book
-- > viewReferer = client myApi host
-- > -- then you can just use "viewRefer" to query that endpoint
-- > -- specifying Nothing or e.g Just "http://haskell.org/" as arguments
instance (KnownSymbol sym, ToText a, HasClient sublayout)
      => HasClient (Header sym a :> sublayout) where
  type Client (Header sym a :> sublayout) =
    Maybe a -> Client sublayout
  clientWithRoute Proxy req baseurl mval =
                    (maybe req
                           (\value -> Servant.Common.Req.addHeader hname value req)
                           mval
                    )
    where hname = symbolVal (Proxy :: Proxy sym)
-- | If you have a 'Post' endpoint in your API, the client
  (MimeUnrender ct a) => HasClient (Post (ct ': cts) a) where
  type Client (Post (ct ': cts) a) = EitherT ServantError IO a
    snd <$> performRequestCT (Proxy :: Proxy ct) H.methodPost req [200,201] baseurl
-- | If you have a 'Post xs ()' endpoint, the client expects a 204 No Content
  HasClient (Post (ct ': cts) ()) where
  type Client (Post (ct ': cts) ()) = EitherT ServantError IO ()
    void $ performRequestNoBody H.methodPost req [204] baseurl
-- | If you have a 'Post xs (Headers ls x)' endpoint, the client expects the
  ) => HasClient (Post (ct ': cts) (Headers ls a)) where
  type Client (Post (ct ': cts) (Headers ls a)) = EitherT ServantError IO (Headers ls a)
    (hdrs, resp) <- performRequestCT (Proxy :: Proxy ct) H.methodPost req [200, 201] baseurl
-- | If you have a 'Put' endpoint in your API, the client
  (MimeUnrender ct a) => HasClient (Put (ct ': cts) a) where
  type Client (Put (ct ': cts) a) = EitherT ServantError IO a
    snd <$> performRequestCT (Proxy :: Proxy ct) H.methodPut req [200,201] baseurl
-- | If you have a 'Put xs ()' endpoint, the client expects a 204 No Content
  HasClient (Put (ct ': cts) ()) where
  type Client (Put (ct ': cts) ()) = EitherT ServantError IO ()
    void $ performRequestNoBody H.methodPut req [204] baseurl
-- | If you have a 'Put xs (Headers ls x)' endpoint, the client expects the
  ) => HasClient (Put (ct ': cts) (Headers ls a)) where
  type Client (Put (ct ': cts) (Headers ls a)) = EitherT ServantError IO (Headers ls a)
    (hdrs, resp) <- performRequestCT (Proxy :: Proxy ct) H.methodPut req [200, 201] baseurl
-- | If you have a 'Patch' endpoint in your API, the client
  (MimeUnrender ct a) => HasClient (Patch (ct ': cts) a) where
  type Client (Patch (ct ': cts) a) = EitherT ServantError IO a
    snd <$> performRequestCT (Proxy :: Proxy ct) H.methodPatch req [200,201] baseurl
-- | If you have a 'Patch xs ()' endpoint, the client expects a 204 No Content
  HasClient (Patch (ct ': cts) ()) where
  type Client (Patch (ct ': cts) ()) = EitherT ServantError IO ()
    void $ performRequestNoBody H.methodPatch req [204] baseurl
-- | If you have a 'Patch xs (Headers ls x)' endpoint, the client expects the
  ) => HasClient (Patch (ct ': cts) (Headers ls a)) where
  type Client (Patch (ct ': cts) (Headers ls a)) = EitherT ServantError IO (Headers ls a)
    (hdrs, resp) <- performRequestCT (Proxy :: Proxy ct) H.methodPatch req [200, 201, 204] baseurl
-- | If you use a 'QueryParam' in one of your endpoints in your API,
-- an additional argument of the type specified by your 'QueryParam',
-- enclosed in Maybe.
-- If you give Nothing, nothing will be added to the query string.
-- If you give a non-'Nothing' value, this function will take care
-- of inserting a textual representation of this value in the query string.
-- You can control how values for your type are turned into
-- > type MyApi = "books" :> QueryParam "author" Text :> Get '[JSON] [Book]
-- > getBooksBy :: Maybe Text -> EitherT String IO [Book]
-- > getBooksBy = client myApi host
-- > -- then you can just use "getBooksBy" to query that endpoint.
-- > -- 'getBooksBy Nothing' for all books
-- > -- 'getBooksBy (Just "Isaac Asimov")' to get all books by Isaac Asimov
      => HasClient (QueryParam sym a :> sublayout) where
  type Client (QueryParam sym a :> sublayout) =
  -- if mparam = Nothing, we don't add it to the query string
  clientWithRoute Proxy req baseurl mparam =
                           (flip (appendToQueryString pname) req . Just)
                           mparamText
    where pname  = cs pname'
          pname' = symbolVal (Proxy :: Proxy sym)
          mparamText = fmap toText mparam
-- | If you use a 'QueryParams' in one of your endpoints in your API,
-- an additional argument, a list of values of the type specified
-- by your 'QueryParams'.
-- If you give an empty list, nothing will be added to the query string.
-- Otherwise, this function will take care
-- of inserting a textual representation of your values in the query string,
-- under the same query string parameter name.
-- > type MyApi = "books" :> QueryParams "authors" Text :> Get '[JSON] [Book]
-- > getBooksBy :: [Text] -> EitherT String IO [Book]
-- > -- 'getBooksBy []' for all books
-- > -- 'getBooksBy ["Isaac Asimov", "Robert A. Heinlein"]'
-- > --   to get all books by Asimov and Heinlein
      => HasClient (QueryParams sym a :> sublayout) where
  type Client (QueryParams sym a :> sublayout) =
    [a] -> Client sublayout
  clientWithRoute Proxy req baseurl paramlist =
                    (foldl' (\ req' -> maybe req' (flip (appendToQueryString pname) req' . Just))
                            req
                            paramlist'
          paramlist' = map (Just . toText) paramlist
-- | If you use a 'QueryFlag' in one of your endpoints in your API,
-- an additional 'Bool' argument.
-- If you give 'False', nothing will be added to the query string.
-- Otherwise, this function will insert a value-less query string
-- parameter under the name associated to your 'QueryFlag'.
-- > type MyApi = "books" :> QueryFlag "published" :> Get '[JSON] [Book]
-- > getBooks :: Bool -> EitherT String IO [Book]
-- > getBooks = client myApi host
-- > -- then you can just use "getBooks" to query that endpoint.
-- > -- 'getBooksBy False' for all books
-- > -- 'getBooksBy True' to only get _already published_ books
instance (KnownSymbol sym, HasClient sublayout)
      => HasClient (QueryFlag sym :> sublayout) where
  type Client (QueryFlag sym :> sublayout) =
    Bool -> Client sublayout
  clientWithRoute Proxy req baseurl flag =
                    (if flag
                       then appendToQueryString paramname Nothing req
                       else req
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
-- | If you use a 'MatrixParam' in one of your endpoints in your API,
-- an additional argument of the type specified by your 'MatrixParam',
-- > type MyApi = "books" :> MatrixParam "author" Text :> Get '[JSON] [Book]
      => HasClient (MatrixParam sym a :> sublayout) where
  type Client (MatrixParam sym a :> sublayout) =
                           (flip (appendToMatrixParams pname . Just) req)
    where pname = symbolVal (Proxy :: Proxy sym)
          mparamText = fmap (cs . toText) mparam
-- | If you use a 'MatrixParams' in one of your endpoints in your API,
-- the corresponding querying function will automatically take an
-- additional argument, a list of values of the type specified by your
-- 'MatrixParams'.
-- Otherwise, this function will take care of inserting a textual
-- representation of your values in the path segment string, under the
-- same matrix string parameter name.
-- You can control how values for your type are turned into text by
-- specifying a 'ToText' instance for your type.
-- > type MyApi = "books" :> MatrixParams "authors" Text :> Get '[JSON] [Book]
      => HasClient (MatrixParams sym a :> sublayout) where
  type Client (MatrixParams sym a :> sublayout) =
                    (foldl' (\ req' value -> maybe req' (flip (appendToMatrixParams pname) req' . Just . cs) value)
-- | If you use a 'MatrixFlag' in one of your endpoints in your API,
-- additional 'Bool' argument.
-- If you give 'False', nothing will be added to the path segment.
-- Otherwise, this function will insert a value-less matrix parameter
-- under the name associated to your 'MatrixFlag'.
-- > type MyApi = "books" :> MatrixFlag "published" :> Get '[JSON] [Book]
      => HasClient (MatrixFlag sym :> sublayout) where
  type Client (MatrixFlag sym :> sublayout) =
                       then appendToMatrixParams paramname Nothing req
-- | Pick a 'Method' and specify where the server you want to query is. You get
-- back the full `Response`.
instance HasClient Raw where
  type Client Raw = H.Method -> EitherT ServantError IO (Int, ByteString, MediaType, [HTTP.Header])
  clientWithRoute :: Proxy Raw -> Req -> BaseUrl -> Client Raw
  clientWithRoute Proxy req baseurl httpMethod = do
    performRequest httpMethod req (const True) baseurl
-- | If you use a 'ReqBody' in one of your endpoints in your API,
-- an additional argument of the type specified by your 'ReqBody'.
-- That function will take care of encoding this argument as JSON and
-- of using it as the request body.
-- All you need is for your type to have a 'ToJSON' instance.
-- > type MyApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
-- > addBook :: Book -> EitherT String IO Book
-- > addBook = client myApi host
-- > -- then you can just use "addBook" to query that endpoint
instance (MimeRender ct a, HasClient sublayout)
      => HasClient (ReqBody (ct ': cts) a :> sublayout) where
  type Client (ReqBody (ct ': cts) a :> sublayout) =
  clientWithRoute Proxy req baseurl body =
                    (let ctProxy = Proxy :: Proxy ct
                     in setRQBody (mimeRender ctProxy body)
                                  (contentType ctProxy)
                                  req
-- | Make the querying function append @path@ to the request path.
instance (KnownSymbol path, HasClient sublayout) => HasClient (path :> sublayout) where
  type Client (path :> sublayout) = Client sublayout
     clientWithRoute (Proxy :: Proxy sublayout)
                     (appendToPath p req)
                     baseurl
    where p = symbolVal (Proxy :: Proxy path)
