{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Data.List
import           XMonad
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Combo
import           XMonad.Layout.Decoration
import           XMonad.Layout.Master
import           XMonad.Layout.SimpleDecoration
import           XMonad.Layout.Tabbed (tabbed, def, Shrinker(..))
import           XMonad.Layout.TwoPane
import           XMonad.Layout.WindowNavigation
import           XMonad.StackSet (shift)
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig

import           XMonad.Layout.Simplest ( Simplest(Simplest) )

myLayout = avoidStruts $
    dualTabs ||| topTabs ||| leftTabbed (3/20)
  where
    dualTabs = windowNavigation (combineTwo (TwoPane delta (1/2)) topTabs topTabs)
    topTabs = tabbed DropBoring def
    nmaster = 2 -- The default number of windows in the master pane
    delta   = 3/100 -- Percent of screen to increment by when resizing panes

forceDesktopOrFloat = composeAll $
                 (className =? "firefox-bin"                    --> doF (shift "2")) :
                 [ fmap ( c `isInfixOf`) className --> doFloat | c <- myMatchAnywhereFloatsC ]

myMatchAnywhereFloatsC = ["Pinentry"]

main = xmonad $ defaults
        `additionalKeysP`
        [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
          |   (key, scr) <- zip "oeu" [1,0,2]
            , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
         ]
        `additionalKeysP`
        [ (mask ++ "M-" ++ [key], whenJust (Just ws) (windows . action))
        |   (key, ws) <- zip "!@#$%^&*()" $ map show [1..10]
        , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
         ]
        `additionalKeysP`
        [ ("M--", spawn "xscreensaver-command -lock")
        , ("M-s", spawn "xscreensaver-command -suspend && systemctl suspend")
        , ("M-,", sendMessage (Move L))
        , ("M-.", sendMessage (Move R))
        ]

defaults = defaultConfig {
      -- simple stuff
        modMask = mod4Mask,
        terminal           = "alacritty",
        layoutHook         = myLayout,
        manageHook         = forceDesktopOrFloat <+>
                             manageDocks <+> manageHook defaultConfig,
        handleEventHook = docksEventHook <+> handleEventHook defaultConfig
}


-- | xmonad-contrib now has a version of side tabs, but it makes each
-- tab tall enough that the set of tabs fill the screen vertically.
-- For some reason the text shrinking compares text length to (width -
-- height/2), so these tall tabs cause window titles to be shortened
-- to ~ 6 letters!  Keep this version, which makes each tab the same
-- height as the top tabs.

data TabLocation = RHS | LHS | Top | Bottom
                 deriving (Show, Read)

-- instance LayoutClass VertTabs a where
--   pureLayout (VertTabs loc frac) r s = [(W.focus s, master)]
--     where master = case loc of
--             RHS -> fst $ splitHorizontallyBy frac r
--             LHS -> snd $ splitHorizontallyBy (1-frac) r

--   pureMessage (VertTabs loc frac) m =
--     msum [fmap resize     (fromMessage m)]
--     where resize Shrink             = VertTabs loc (max 0 $ frac - 3/100)
--           resize Expand             = VertTabs loc (min 1 $ frac + 3/100)

leftTabbed :: Rational -> ModifiedLayout (Decoration TabbedDecoration DropBoring) Simplest Window
leftTabbed  frac = decoration DropBoring defaultTheme (Tabbed LHS Always frac) Simplest

rightTabbed :: Rational -> ModifiedLayout (Decoration TabbedDecoration DropBoring) Simplest Window
rightTabbed frac = decoration DropBoring defaultTheme (Tabbed RHS Always frac) Simplest


data TabbedDecoration a = Tabbed TabLocation TabbarShown Rational
                        deriving (Read, Show)

data TabbarShown = Always | WhenPlural deriving (Read, Show, Eq)

instance Eq a => DecorationStyle TabbedDecoration a where
    describeDeco (Tabbed Top _ _ ) = "Tabbed"
    describeDeco (Tabbed Bottom _ _ ) = "Tabbed Bottom"
    describeDeco (Tabbed RHS _ _ ) = "Tabbed Right"
    describeDeco (Tabbed LHS _ _ ) = "Tabbed Left"

    decorationEventHook _ ds ButtonEvent { ev_window     = ew
                                         , ev_event_type = et
                                         , ev_button     = eb }
        | et == buttonPress
        , Just ((w,_),_) <-findWindowByDecoration ew ds =
           if eb == button2
               then killWindow w
               else focus w
    decorationEventHook _ _ _ = return ()

    pureDecoration (Tabbed lc sh frac) _ ht _ s wrs (w,r@(Rectangle x y wh hh))
        = if (sh == Always && numWindows > 0) || numWindows > 1
          then Just $ case lc of
                        Top -> upperTab
                        Bottom -> lowerTab
                        LHS -> leftTab
                        RHS -> rightTab
          else Nothing
        where ws = filter (`elem` map fst (filter ((==r) . snd) wrs)) (W.integrate s)
              loc i = x + fi ((wh * fi i) `div` max 1 (fi $ length ws))
              wid = fi $ maybe x (\i -> loc (i+1) - loc i) $ w `elemIndex` ws
              nx  = maybe x loc $ w `elemIndex` ws
              yloc i = y + fi (ht * fi i)
              ny  = maybe y yloc $ w `elemIndex` ws
              vwid = truncate $ fi wh * frac
              upperTab = Rectangle nx y wid (fi ht)
              lowerTab = Rectangle nx (y+fi(hh-ht)) wid (fi ht)
              leftTab  = Rectangle x ny vwid ht
              rightTab = Rectangle (x+fi (wh-vwid)) ny vwid ht
              numWindows = length ws
    shrink (Tabbed loc _ frac) (Rectangle _ _ _ dh) (Rectangle x y w h)
        = case loc of
            Top -> Rectangle x (y + fi dh) w (h - dh)
            Bottom -> Rectangle x y w (h - dh)
            LHS -> Rectangle (x+fi vwid) y (w-vwid) h
            RHS -> Rectangle x y (w-vwid) h
         where vwid = truncate $ fi w * frac

data DropBoring = DropBoring
    deriving (Show, Read)

instance Shrinker DropBoring where
    shrinkIt _ = trailingDirs . replacePrefix "http://" ""
      where
        replacePrefix from to s = case stripPrefix from s of
            Nothing -> s
            Just s' -> to ++ s'
        dropSuffix xxx = reverse . replacePrefix (reverse xxx) "" . reverse
        splitDirs s = case break (=='/') s of
            (_, []) -> [s]
            (_, "/") -> [s]
            (d, _: ds) -> d : splitDirs ds
        -- file names are more interesting than directory locations
        trailingDirs ('/' : s) = let
            directories = splitDirs (replacePrefix "home/bergey" "~" s)
            filename = reverse . inits . last $ directories
            in (map (intercalate "/") . init . tails $ directories) ++ filename
        trailingDirs s = reverse . inits . dropSuffix " - Chromium" $ s
