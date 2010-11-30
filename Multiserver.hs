module Multiserver where

import Router
import Processor



data (Router a) => ServerPair a = ServerPair {
      router      :: a
    , smallServer :: SimpleServer
    , largeServer :: SimpleServer
    } 

instance (Router a) => Processor (ServerPair a) where
    newProcessor = ServerPair (newRouter 1) newProcessor newProcessor

    step = undefined
