{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Machine.Types where

import           Data.Aeson
import qualified Data.HashMap.Strict as Hm
import           Data.Text           (Text)
import           Data.Time

data Wrapped ty
  = Ok ty
  | NotOk Text
    deriving ( Eq, Ord, Show, Read, Functor )

instance FromJSON ty => FromJSON (Wrapped ty) where
  parseJSON = withObject "wrapper" $ \o -> do
    ok <- o .: "ok"
    if ok
      then Ok <$> parseJSON (Object (Hm.delete "ok" o))
      else NotOk <$> (o .: "error")

newtype Account
  = Account Text
  deriving ( Eq, Ord, Show, Read, FromJSON, ToJSON )

newtype Symbol
  = Symbol Text
  deriving ( Eq, Ord, Show, Read, FromJSON, ToJSON )

newtype Venue
  = Venue Text
  deriving ( Eq, Ord, Show, Read, FromJSON, ToJSON )

newtype Price
  = Price Int
  deriving ( Eq, Ord, Show, Read, Num, Enum, Real, Integral, FromJSON, ToJSON )

newtype Size
  = Size Int
  deriving ( Eq, Ord, Show, Read, Num, Enum, Real, Integral, FromJSON, ToJSON )

newtype OrderId
  = OrderId Int
  deriving ( Eq, Ord, Show, Read, Num, Enum, Real, Integral, FromJSON, ToJSON )

newtype StandingId
  = StandingId Int
  deriving ( Eq, Ord, Show, Read, Num, Enum, Real, Integral, FromJSON, ToJSON )

newtype IncomingId
  = IncomingId Int
  deriving ( Eq, Ord, Show, Read, Num, Enum, Real, Integral, FromJSON, ToJSON )

data Quote
  = Quote
    { quoteSymbol    :: Symbol
    , quoteVenue     :: Venue
    , quoteBid       :: Price
    , quoteAsk       :: Price
    , quoteBidSize   :: Size
    , quoteAskSize   :: Size
    , quoteBidDepth  :: Size
    , quoteAskDepth  :: Size
    , quoteLastPrice :: Price
    , quoteLastSize  :: Size
    , quoteLastTrade :: UTCTime
    , quoteTime      :: UTCTime
    }
  deriving ( Eq, Ord, Show, Read )

instance FromJSON Quote where
  parseJSON = withObject "quote" $ \o ->
    Quote
    <$> o .: "symbol"
    <*> o .: "venue"
    <*> o .: "bid"
    <*> o .: "ask"
    <*> o .: "bidSize"
    <*> o .: "askSize"
    <*> o .: "bidDepth"
    <*> o .: "askDepth"
    <*> o .: "last"
    <*> o .: "lastSize"
    <*> o .: "lastTrade"
    <*> o .: "quoteTime"

data OrderDirection
  = Buy | Sell
  deriving ( Eq, Ord, Show, Read )

instance FromJSON OrderDirection where
  parseJSON = withText "order direction" $ \text ->
    case text of
      "buy" -> pure Buy
      "sell" -> pure Sell
      _ -> fail "invalid order direction"

instance ToJSON OrderDirection where
  toJSON Buy = String "buy"
  toJSON Sell = String "sell"

data OrderType
  = Limit | Market | FillOrKill | ImmediateOrCancel
  deriving ( Eq, Ord, Show, Read )

instance FromJSON OrderType where
  parseJSON = withText "order direction" $ \text ->
    case text of
      "limit" -> pure Limit
      "market" -> pure Market
      "fill-or-kill" -> pure FillOrKill
      "immediate-or-cancel" -> pure ImmediateOrCancel
      _ -> fail "invalid order type"

instance ToJSON OrderType where
  toJSON ty = case ty of
    Limit -> String "limit"
    Market -> String "market"
    FillOrKill -> String "fill-or-kill"
    ImmediateOrCancel -> String "immediate-or-cancel"

data NewOrder
  = NewOrder
    { newOrderAccount   :: Account
    , newOrderVenue     :: Venue
    , newOrderSymbol    :: Symbol
    , newOrderQuantity  :: Size
    , newOrderDirection :: OrderDirection
    , newOrderType      :: OrderType
    }
  deriving ( Eq, Ord, Show, Read )

instance ToJSON NewOrder where
  toJSON o =
    object [ "account" .= newOrderAccount o
           , "venue" .= newOrderVenue o
           , "stock" .= newOrderSymbol o
           , "qty" .= newOrderQuantity o
           , "direction" .= newOrderDirection o
           , "orderType" .= newOrderType o
           ]

data Fill
  = Fill
    { fillPrice     :: Price
    , fillQuantity  :: Size
    , fillTimestamp :: UTCTime
    }
  deriving ( Eq, Ord, Show, Read )

instance FromJSON Fill where
  parseJSON = withObject "fill" $ \o ->
    Fill
    <$> o .: "price"
    <*> o .: "qty"
    <*> o .: "ts"

data Order
  = Order
    { orderSymbol           :: Symbol
    , orderVenue            :: Venue
    , orderDirection        :: OrderDirection
    , orderOriginalQuantity :: Size
    , orderQuantity         :: Size
    , orderPrice            :: Price
    , orderType             :: OrderType
    , orderId               :: OrderId
    , orderAccount          :: Account
    , orderTimestamp        :: UTCTime
    , orderFills            :: [Fill]
    , orderTotalFilled      :: Size
    , orderOpen             :: Bool
    }
  deriving ( Eq, Ord, Show, Read )

instance FromJSON Order where
  parseJSON = withObject "order" $ \o ->
    Order
    <$> o .: "symbol"
    <*> o .: "venue"
    <*> o .: "direction"
    <*> o .: "originalQty"
    <*> o .: "qty"
    <*> o .: "price"
    <*> o .: "orderType"
    <*> o .: "id"
    <*> o .: "account"
    <*> o .: "ts"
    <*> o .: "fills"
    <*> o .: "totalFilled"
    <*> o .: "open"

data QuoteWSMessage
  = QuoteWSMessage
    { quoteWsMsgQuote :: Quote }
  deriving ( Eq, Ord, Show, Read )

instance FromJSON QuoteWSMessage where
  parseJSON = withObject "quote ws message" $ \o ->
    QuoteWSMessage <$> o .: "quote"

data ExecutionWSMessage
  = ExecutionWSMessage
    { executionWsMsgAccount          :: Account
    , executionWsMsgVenue            :: Venue
    , executionWsMsgSymbol           :: Symbol
    , executionWsMsgOrder            :: Wrapped Order
    , executionWsMsgStandingId       :: StandingId
    , executionWsMsgIncomingId       :: IncomingId
    , executionWsMsgPrice            :: Price
    , executionWsMsgFilled           :: Size
    , executionWsMsgFilledAt         :: UTCTime
    , executionWsMsgStandingComplete :: Bool
    , executionWsMsgIncomingComplete :: Bool
    }
  deriving ( Eq, Ord, Show, Read )

instance FromJSON ExecutionWSMessage where
  parseJSON = withObject "execution ws message" $ \o ->
    ExecutionWSMessage
    <$> o .: "account"
    <*> o .: "venue"
    <*> o .: "symbol"
    <*> o .: "order"
    <*> o .: "standingId"
    <*> o .: "incomingId"
    <*> o .: "price"
    <*> o .: "filled"
    <*> o .: "filledAt"
    <*> o .: "standingComplete"
    <*> o .: "incomingComplete"
