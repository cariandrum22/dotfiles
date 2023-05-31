import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D
import Data.Ratio ((%))
import Graphics.X11.ExtraTypes.XF86 ()
import System.Exit ()
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
        logHook,
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
import XMonad.Actions.CycleWS ()
import XMonad.Actions.DynamicProjects
  ( Project (..),
    dynamicProjects,
  )
import XMonad.Hooks.DynamicLog
  ( PP
      ( ppCurrent,
        ppHidden,
        ppOutput,
        ppSep,
        ppTitle,
        ppUrgent,
        ppVisible,
        ppWsSep
      ),
    def,
    dynamicLogWithPP,
    shorten,
    wrap,
  )
import XMonad.Hooks.EwmhDesktops
  ( ewmh,
    ewmhFullscreen,
  )
import XMonad.Hooks.ManageDocks
  ( avoidStruts,
    docksEventHook,
    manageDocks,
  )
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
import XMonad.Layout.FixedColumn (FixedColumn (FixedColumn))
import XMonad.Layout.Grid (Grid (Grid))
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.Magnifier (magnifiercz')
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.MultiColumns (multiCol)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed ()
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.Tabbed (def)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Prompt
  ( XPConfig
      ( bgColor,
        bgHLight,
        borderColor,
        fgColor,
        fgHLight,
        font,
        height,
        position,
        promptBorderWidth
      ),
    XPPosition (Top),
  )
import XMonad.Util.EZConfig ()
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
  dbus <- D.connectSession
  D.requestName
    dbus
    (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  xmonad $
    dynamicProjects projects $
      withUrgencyHook NoUrgencyHook $
        ewmhFullscreen $
          ewmh $
            addDescrKeys ((myModMask, xK_F1), showKeybindings) myKeys $
              myConfig {logHook = dynamicLogWithPP (myLogHook dbus)}

--------------------------------------------------------------------------------
-- Personal settings                                                          --
--------------------------------------------------------------------------------
myModMask :: KeyMask
myModMask = mod4Mask

myTerminal = "kitty"

myLauncher = "rofi -show drun -modi drun -location 0 -drun-display-format \"{name}\""

myBrowser = "google-chrome"

myScreenLocker = "dm-tool lock"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth :: Dimension
myBorderWidth = 1

mySpacing :: Int
mySpacing = 3

myLargeSpacing :: Int
myLargeSpacing = 15

noSpacing :: Int
noSpacing = 0

--------------------------------------------------------------------------------
-- Theme                                                                      --
--------------------------------------------------------------------------------
prompt = 24

-- Nord
-- https://www.nordtheme.com/
--
-- Polar Night
nord0 = "#2E3440"

nord1 = "#3B4252"

nord2 = "#434C5E"

nord3 = "#4C566A"

-- Snow Storm
nord4 = "#D8DEE9"

nord5 = "#E5E9F0"

nord6 = "#ECEFF4"

-- Frost
nord7 = "#8FBCBB"

nord8 = "#88C0D0"

nord9 = "#81A1C1"

nord10 = "#5E81AC"

-- Aurora
nord11 = "#BF616A"

nord12 = "#D08770"

nord13 = "#EBCB8B"

nord14 = "#A3BE8C"

nord15 = "#B48EAD"

-- Fonts
fontsize :: String
fontsize = "12"

monospace :: String
monospace = "xft:FiraCode Nerd Font Mono:size=" ++ fontsize ++ "style=Retina"

-- Prompt theme
myPromptTheme :: XPConfig
myPromptTheme =
  def
    { font = monospace,
      bgColor = nord0,
      fgColor = nord4,
      fgHLight = nord6,
      bgHLight = nord3,
      borderColor = nord4,
      promptBorderWidth = 0,
      height = prompt,
      position = Top
    }

--------------------------------------------------------------------------------
-- Workspaces                                                                 --
--------------------------------------------------------------------------------
wsGeneral = "\xe712"

wsWork = "\xe777"

wsStudy = "\xf0fc9"

wsNetwork = "\xf06f3"

wsMusic = "\xf075a"

wsNote = "\xf00bd"

wsMonitor = "\xf1fe"

wsSetting = "\xe615"

wsGame = "\xf11b"

myWorkspaces :: [String]
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
myLayouts = avoidStruts . minimize . boringWindows $ perWS

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

myTall = spacing mySpacing $ Tall 1 (3 / 100) (1 / 2)

myMirrorTall = spacing mySpacing $ Mirror (Tall 1 (3 / 100) (3 / 5))

my3Col = spacing mySpacing $ ThreeColMid 1 (3 / 100) (1 / 2)

myMultiCol = spacing mySpacing $ multiCol [1] 1 0.01 (-0.5)

myGrid = spacing mySpacing $ Grid

mySpiral = spacing mySpacing $ spiral (6 / 7)

myFull = spacing 0 $ noBorders Full

--------------------------------------------------------------------------------
-- Dynamic Projects                                                           --
--------------------------------------------------------------------------------
projects :: [Project]
projects =
  [ Project
      { projectName = "Workspace",
        projectDirectory = "~/Workspace",
        projectStartHook = Just $ do
          spawn myTerminal
      },
    Project
      { projectName = "Study",
        projectDirectory = "~/Documents/Books",
        projectStartHook = Just $ do
          spawn "nemo ."
          spawn myTerminal
      },
    Project
      { projectName = "NixOS",
        projectDirectory = "~/Codex/github.com/cariandrum22/configuration.nix",
        projectStartHook = Just $ do
          spawn "code ."
      },
    Project
      { projectName = "dotfiles",
        projectDirectory = "~/Codex/github.com/cariandrum22/dotfiles",
        projectStartHook = Just $ do
          spawn "code ."
          spawn $ myTerminal <> " tmux"
      },
    Project
      { projectName = "xmonad",
        projectDirectory = "~/Codex/github.com/cariandrum22/dotfiles/xmonad",
        projectStartHook = Just $ do
          spawn "code ."
          spawn "kitty tmux"
      },
    Project
      { projectName = "System",
        projectDirectory = "~/Documents/",
        projectStartHook = Just $ do
          spawn $ myTerminal <> " htop"
          spawn $ myTerminal <> " dstat"
      }
  ]

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
    [ key "Open Terminal" (modm .|. shiftMask, xK_Return) $ spawn $ myTerminal <> " tmux",
      key "Open rofi" (modm, xK_p) $ spawn myLauncher,
      key "Lock screen" (modm .|. controlMask, xK_l) $ spawn myScreenLocker,
      key "Sleep" (modm .|. controlMask, xK_l) $ spawn "systemctl suspend"
    ]
    ^++^ keySet
      "Applications"
      [ key "Open Google Chrome" (modm, xK_b) $ spawn myBrowser,
        key "Open Slack" (modm, xK_s) $ spawn "slack"
      ]
  where
    keySet s ks = subtitle s : ks
    key n k a = (k, addName n a)

--------------------------------------------------------------------------------
-- ManageHooks                                                                --
--------------------------------------------------------------------------------
myManageHook =
  composeAll
    [ resource =? "desktop_window" --> doIgnore,
      role =? "pop-up" --> doFloat,
      className =? "feh" --> doFloat
    ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

myManageHook' :: ManageHook
myManageHook' = composeOne [isFullscreen -?> doFullFloat]

--------------------------------------------------------------------------------
-- LogHooks                                                                   --
--------------------------------------------------------------------------------
myLogHook :: D.Client -> PP
myLogHook dbus =
  def
    { ppOutput = dbusOutput dbus,
      ppCurrent = wrap ("%{F" ++ nord7 ++ "} ") " %{F-}",
      ppVisible = wrap ("%{F" ++ nord10 ++ "} ") " %{F-}",
      ppUrgent = wrap ("%{F" ++ nord11 ++ "} ") " %{F-}",
      ppHidden = wrap " " " ",
      ppWsSep = "",
      ppSep = " | ",
      ppTitle = shorten 24 . wrap ("%{F" ++ nord14 ++ "} ") " %{F-}"
    }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal =
        (D.signal objectPath interfaceName memberName)
          { D.signalBody = [D.toVariant $ UTF8.decodeString str]
          }
  D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

--------------------------------------------------------------------------------
-- StartupHooks                                                               --
--------------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  -- For Java GUI Programs
  setWMName "LG3D"
  spawn "nm-applet &"
  spawn "polybar -c ~/.config/polybar/config.ini top &"

--------------------------------------------------------------------------------
-- Config                                                                     --
--------------------------------------------------------------------------------
myConfig =
  def
    { terminal = myTerminal,
      layoutHook = myLayouts,
      manageHook =
        placeHook (smart (0.5, 0.5))
          <+> manageDocks
          <+> myManageHook
          <+> myManageHook'
          <+> manageHook def,
      handleEventHook = minimizeEventHook,
      startupHook = myStartupHook,
      focusFollowsMouse = False,
      clickJustFocuses = False,
      borderWidth = myBorderWidth,
      normalBorderColor = nord0,
      focusedBorderColor = nord6,
      workspaces = myWorkspaces,
      modMask = myModMask
    }
