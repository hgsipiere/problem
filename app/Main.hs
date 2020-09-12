{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE TypeOperators #-}

module Main where

import           Calamity
import           Calamity.Cache.InMemory
import           Calamity.Commands
import qualified Calamity.Commands.Context                  as CommandContext
import           Calamity.Metrics.Eff
import           Calamity.Metrics.Internal
import           Calamity.Metrics.Noop

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad

import           Data.HashMap.Lazy                          as LH
import           Data.IORef
import           Data.Text                                  ( Text )
import           Data.Text.Lazy.Lens


import           GHC.Generics

import qualified DiPolysemy as D
import qualified Polysemy                                   as P
import qualified Polysemy.Async                             as P
import qualified Polysemy.AtomicState                       as P
import qualified Polysemy.Embed                             as P
import qualified Polysemy.Fail                              as P

import           Prelude                                    hiding ( error )
import qualified Calamity.HTTP                     as K
import           System.Environment

import           TextShow

main :: IO ()
main = do
  token <- view packed <$> getEnv "BOT_TOKEN"
  void . P.runFinal . P.embedToFinal. D.runDiNoop . runCacheInMemory . runMetricsNoop . useConstantPrefix "!"
    $ runBotIO (BotToken token) $ do
    addCommands $ do
      helpCommand
