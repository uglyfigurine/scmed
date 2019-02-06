{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           GI.Gtk.Declarative.App.Simple

import           Hello

main :: IO ()
main = run App {view = view', update = update', inputs = [], initialState = ()}
