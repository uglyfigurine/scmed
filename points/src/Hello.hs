{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Hello where

import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

type State = ()

data Event = Closed

view' :: State -> AppView Gtk.Window Event
view' _ = bin
  Gtk.Window
  [ #title := "Point'em"
  , on #deleteEvent (const (True, Closed))
  ]
  $ widget Gtk.Label [#label := "Add some points."]

update' :: State -> Event -> Transition State Event
update' _ Closed = Exit

