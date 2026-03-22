import System.IO (hClose)
import XMonad
  ( Default (def),
    Dimension,
    Full (Full),
    KeyMask,
    KeySym,
    ManageHook,
    Mirror (Mirror),
    Tall (Tall),
    X,
    XConfig
      ( XConfig,
        borderWidth,
        clickJustFocuses,
        focusFollowsMouse,
        focusedBorderColor,
        handleEventHook,
        layoutHook,
        manageHook,
        modMask,
        normalBorderColor,
        startupHook,
        terminal,
        workspaces
      ),
    className,
    composeAll,
    controlMask,
    doFloat,
    doIgnore,
    io,
    mod4Mask,
    resource,
    shiftMask,
    spawn,
    stringProperty,
    xK_F1,
    xK_Return,
    xK_b,
    xK_l,
    xK_p,
    xK_s,
    xmonad,
    (-->),
    (.|.),
    (<+>),
    (=?),
    (|||),
  )
import XMonad.Actions.DynamicProjects
  ( Project (..),
    dynamicProjects,
  )
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers
  ( composeOne,
    doFullFloat,
    isFullscreen,
    (-?>),
  )
import XMonad.Hooks.Minimize (minimizeEventHook)
import XMonad.Hooks.Place (placeHook, smart)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.UrgencyHook
  ( NoUrgencyHook (NoUrgencyHook),
    withUrgencyHook,
  )
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.Grid (Grid (Grid))
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.MultiColumns (multiCol)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Util.Hacks (fixSteamFlicker)
import XMonad.Util.NamedActions
  ( NamedAction,
    addDescrKeys,
    addName,
    showKm,
    subtitle,
    (^++^),
  )
import XMonad.Util.Run (hPutStr, spawnPipe)

--------------------------------------------------------------------------------
-- Main                                                                       --
--------------------------------------------------------------------------------
main :: IO ()
main = do
  xmonad
    . dynamicProjects projects
    . withUrgencyHook NoUrgencyHook
    . ewmhFullscreen
    . ewmh
    . docks
    . addDescrKeys ((myModMask, xK_F1), showKeybindings) myKeys
    $ myConfig

--------------------------------------------------------------------------------
-- Personal settings                                                          --
--------------------------------------------------------------------------------
myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "kitty"

myLauncher :: String
myLauncher = "rofi -show drun -modi drun -location 0 -drun-display-format \"{name}\""

myBrowser :: String
myBrowser = "google-chrome-stable"

myScreenLocker :: String
myScreenLocker = "dm-tool lock"

myBorderWidth :: Dimension
myBorderWidth = 1

mySpacing :: Int
mySpacing = 3

--------------------------------------------------------------------------------
-- Theme                                                                      --
--------------------------------------------------------------------------------
type Identifier = String

type Hex = String

defaultColor :: Hex
defaultColor = "#000000"

nord :: Identifier -> Hex
nord "nord0" = "#2E3440"
nord "nord1" = "#3B4252"
nord "nord2" = "#434C5E"
nord "nord3" = "#4C566A"
nord "nord4" = "#D8DEE9"
nord "nord5" = "#E5E9F0"
nord "nord6" = "#ECEFF4"
nord "nord7" = "#8FBCBB"
nord "nord8" = "#88C0D0"
nord "nord9" = "#81A1C1"
nord "nord10" = "#5E81AC"
nord "nord11" = "#BF616A"
nord "nord12" = "#D08770"
nord "nord13" = "#EBCB8B"
nord "nord14" = "#A3BE8C"
nord _ = defaultColor

--------------------------------------------------------------------------------
-- Workspaces                                                                 --
--------------------------------------------------------------------------------

type CodePoint = String

wsGeneral :: CodePoint
wsGeneral = "\xe712"

wsWork :: CodePoint
wsWork = "\xe777"

wsStudy :: CodePoint
wsStudy = "\xf0fc9"

wsNetwork :: CodePoint
wsNetwork = "\xf06f3"

wsMusic :: CodePoint
wsMusic = "\xf075a"

wsNote :: CodePoint
wsNote = "\xf00bd"

wsMonitor :: CodePoint
wsMonitor = "\xf1fe"

wsSetting :: CodePoint
wsSetting = "\xe615"

wsGame :: CodePoint
wsGame = "\xf11b"

myWorkspaces :: [CodePoint]
myWorkspaces =
  [ wsWork,
    wsStudy,
    wsNetwork,
    wsMusic,
    wsNote,
    wsGame,
    wsMonitor,
    wsSetting,
    wsGeneral
  ]

--------------------------------------------------------------------------------
-- Layout                                                                     --
--------------------------------------------------------------------------------
myLayouts = minimize . boringWindows $ perWS

-- layout per workspace
perWS =
  onWorkspace wsGeneral myTMT3F $
    onWorkspace wsGame myFT $
      onWorkspace
        wsMonitor
        myMT3GS
        myAll

myTMT3F = myTall ||| myMirrorTall ||| my3Col ||| myFull

myFT = myFull ||| myTall

myMT3GS = myMirrorTall ||| my3Col ||| myGrid ||| mySpiral

myAll = myTall ||| myMirrorTall ||| my3Col ||| myMultiCol ||| myGrid ||| mySpiral ||| myFull

spaced = spacing mySpacing

myTall = spaced $ Tall 1 (3 / 100) (1 / 2)

myMirrorTall = spaced $ Mirror (Tall 1 (3 / 100) (3 / 5))

my3Col = spaced $ ThreeColMid 1 (3 / 100) (1 / 2)

myMultiCol = spaced $ multiCol [1] 1 0.01 (-0.5)

myGrid = spaced Grid

mySpiral = spaced $ spiral (6 / 7)

myFull = noBorders Full

--------------------------------------------------------------------------------
-- Dynamic Projects                                                           --
--------------------------------------------------------------------------------
projects :: [Project]
projects =
  [ mkProject "Workspace" "~/Workspace" [myTerminal],
    mkProject "Study" "~/Documents/Books" ["nemo .", myTerminal],
    mkProject "NixOS" "~/Codex/github.com/cariandrum22/configuration.nix" ["code ."],
    mkProject "dotfiles" "~/Codex/github.com/cariandrum22/dotfiles" ["code .", myNewTmux],
    mkProject "xmonad" "~/Codex/github.com/cariandrum22/dotfiles/xmonad" ["code .", myNewTmux],
    mkProject "System" "~/Documents/" [myTerminal <> " htop", myTerminal <> " dstat"]
  ]
  where
    mkProject name directory commands =
      Project
        { projectName = name,
          projectDirectory = directory,
          projectStartHook = Just $ mapM_ spawn commands
        }

--------------------------------------------------------------------------------
-- Keybindings                                                                --
--------------------------------------------------------------------------------
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $
  io $ do
    h <- spawnPipe "zenity --text-info --font=Fira Code Nerd Fond 12"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

myKeys :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
myKeys XConfig {XMonad.modMask = modm} =
  keySet
    "Launchers"
    [ key "Open Terminal" (modm .|. shiftMask, xK_Return) $ spawn myNewTmux,
      key "Open rofi" (modm, xK_p) $ spawn myLauncher,
      key "Lock screen" (modm .|. controlMask, xK_l) $ spawn myScreenLocker,
      key "Sleep" (modm .|. controlMask, xK_s) $ spawn "systemctl suspend"
    ]
    ^++^ keySet
      "Applications"
      [ key "Open Google Chrome" (modm, xK_b) $ spawn myBrowser,
        key "Open Slack" (modm, xK_s) $ spawn "slack"
      ]
  where
    keySet s ks = subtitle s : ks
    key n k a = (k, addName n a)

myNewTmux :: String
myNewTmux = myTerminal <> " tmux new-session"

--------------------------------------------------------------------------------
-- ManageHooks                                                                --
--------------------------------------------------------------------------------
myManageHook :: ManageHook
myManageHook =
  composeAll
    [ resource =? "desktop_window" --> doIgnore,
      role =? "pop-up" --> doFloat,
      className =? "feh" --> doFloat
    ]
    <+> composeOne [isFullscreen -?> doFullFloat]
  where
    role = stringProperty "WM_WINDOW_ROLE"

--------------------------------------------------------------------------------
-- StartupHooks                                                               --
--------------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  -- For Java GUI Programs
  setWMName "LG3D"
  spawn "nm-applet"
  spawn "polybar -c ~/.config/polybar/config.ini top"

--------------------------------------------------------------------------------
-- Config                                                                     --
--------------------------------------------------------------------------------
myConfig =
  def
    { terminal = myTerminal,
      layoutHook = avoidStruts myLayouts,
      manageHook =
        placeHook (smart (0.5, 0.5))
          <+> manageDocks
          <+> myManageHook,
      handleEventHook = fixSteamFlicker <+> minimizeEventHook,
      startupHook = myStartupHook,
      focusFollowsMouse = False,
      clickJustFocuses = False,
      borderWidth = myBorderWidth,
      normalBorderColor = nord "nord0",
      focusedBorderColor = nord "nord6",
      workspaces = myWorkspaces,
      modMask = myModMask
    }
