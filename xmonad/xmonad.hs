import System.IO
import System.Exit
import Control.Monad
import List
import Control.Concurrent
import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdatePointer
import XMonad.Actions.CopyWindow
import XMonad.Actions.GridSelect
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.WindowArranger
import XMonad.Layout.Master
import XMonad.Layout.Grid
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.WindowNavigation
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutHints
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Loggers
import WorkspaceCompareUpgrade
import XMonad.Layout.PerWorkspace
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Maybe
import XMonad.Layout.IM
import Data.Ratio ((%))
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Window
import XMonad.Prompt.AppendFile
import XMonad.ManageHook
import XMonad.Util.NamedScratchpad
import XMonad.Actions.CopyWindow
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog

bigXPConfig = defaultXPConfig
	{ font = "xft:terminal-50"
	, fgColor = "#a8f7a3"
	, bgColor = "#3f3c6d"
	, fgHLight = "#3fc1c32"
	, bgHLight = "#8BA314"
  , promptBorderWidth = 0
	, borderColor =  "#3f3c6d"
	, height = 100
	}

smallXPConfig = bigXPConfig
 {
  font = "xft:terminal-8"
  , height = 20
  }

scratchpads = [
     NS "notes" "gvim --role notes -c 'set autoread' -c'set wrap' -c 'au FocusLost * :wa' -c 'colorscheme slate' -c 'Note'" (role =? "notes")
         (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
 ] where role = stringProperty "WM_WINDOW_ROLE"

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. controlMask, xK_l     ), spawn "gnome-screensaver-command -l")
    , ((controlMask, xK_space     ), spawn "gnome-do")
    , ((modMask .|. shiftMask, xK_x     ), changeDir defaultXPConfig)

    , ((modMask .|. shiftMask, xK_c     ), kill1)
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_f ), sendMessage $ Toggle "Full")
    , ((modMask,               xK_n     ), refresh)
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask .|. shiftMask,               xK_Tab   ), windows W.swapDown)
    , ((modMask,               xK_m     ), windows W.focusMaster  )
    , ((modMask,               xK_Return), windows W.swapMaster)
    , ((modMask,               xK_bracketleft     ), sendMessage Shrink)
    , ((modMask,               xK_bracketright     ), sendMessage Expand)
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ), restart "xmonad" True)
    , ((modMask, xK_g), goToSelected defaultGSConfig)
    , ((modMask .|. shiftMask, xK_g), bringSelected defaultGSConfig)
    , ((modMask, xK_b), sendMessage ToggleStruts)
    , ((modMask              , xK_BackSpace), focusUrgent)
    , ((modMask .|. controlMask, xK_y), defaultCommands >>= runCommand)
    , ((modMask,                 xK_Right), sendMessage $ Go R)
    , ((modMask,                 xK_Left), sendMessage $ Go L)
    , ((modMask,                 xK_Up), sendMessage $ Go U)
    , ((modMask,                 xK_Down), sendMessage $ Go D)
    , ((modMask,                 xK_l), sendMessage $ Go R)
    , ((modMask,                 xK_h), sendMessage $ Go L)
    , ((modMask,                 xK_k), sendMessage $ Go U)
    , ((modMask,                 xK_j), sendMessage $ Go D)
    , ((modMask .|. shiftMask, xK_Right), sendMessage $ Swap R)
    , ((modMask .|. shiftMask, xK_Left), sendMessage $ Swap L)
    , ((modMask .|. shiftMask, xK_Up), sendMessage $ Swap U)
    , ((modMask .|. shiftMask, xK_Down), sendMessage $ Swap D)
    , ((modMask .|. shiftMask, xK_l), sendMessage $ Swap R)
    , ((modMask .|. shiftMask, xK_h), sendMessage $ Swap L)
    , ((modMask .|. shiftMask, xK_k), sendMessage $ Swap U)
    , ((modMask .|. shiftMask, xK_j), sendMessage $ Swap D)

    , ((modMask, xK_i), SM.submap . M.fromList $
            [((modMask, xK_n), namedScratchpadAction scratchpads "notes")])
    , ((modMask, xK_o), SM.submap . M.fromList $
            [ ((modMask, xK_e), spawn "gvim")
            , ((modMask, xK_b), spawn "google-chrome")
            , ((modMask, xK_v), spawn "vlc")
            , ((modMask, xK_t), spawn $ XMonad.terminal conf)
            , ((modMask, xK_f), spawn "nautilus")
            ])
    , ((modMask, xK_a), SM.submap . M.fromList $
            [ ((modMask, xK_n), appendFilePrompt smallXPConfig "~/Dropbox/notes/Everything")
            ])
    , ((modMask, xK_d), SM.submap . M.fromList $
            zip (zip (repeat (modMask)) [xK_a..xK_z]) (map (withWorkspaceByInitial W.greedyView) ['a'..'z'])
            ++
            zip (zip (repeat (modMask .|. shiftMask)) [xK_a..xK_z]) (map (withWorkspaceByInitial (liftM2 (.) W.view W.shift)) ['a'..'z'])
            ++
            zip (zip (repeat (modMask .|. controlMask)) [xK_a..xK_z]) (map (withWorkspaceByInitial (liftM2 (.) W.view copy)) ['a'..'z'])
            ++
            [
            ((modMask, xK_space), renameWorkspace bigXPConfig)
            , ((modMask, xK_BackSpace), removeWorkspace)
            , ((modMask, xK_Left), prevWS)
            , ((modMask, xK_Right), nextWS)
            , ((modMask, xK_Return), toggleWS)
            , ((modMask .|. shiftMask, xK_Return), shiftToPreviousWorkspace)
            ]
      )
    , ((modMask, xK_s), SM.submap . M.fromList $
            [
            ((modMask, xK_Return), swapNextScreen),
            ((modMask, xK_Left), viewScreen 0),
            ((modMask, xK_Right), viewScreen 1),
            ((modMask, xK_Up), viewScreen 0),
            ((modMask, xK_Down), viewScreen 1),
            ((modMask .|. shiftMask, xK_Left), do
             sendToScreen 0
             viewScreen 0),
            ((modMask .|. shiftMask, xK_Right), do
             sendToScreen 1
             viewScreen 1),
            ((modMask .|. shiftMask, xK_Up), do
             sendToScreen 0
             viewScreen 0),
            ((modMask .|. shiftMask, xK_Down), do
             sendToScreen 1
             sendToScreen 1),
            ((modMask, xK_h), viewScreen 0),
            ((modMask, xK_l), viewScreen 1),
            ((modMask, xK_k), viewScreen 0),
            ((modMask, xK_j), viewScreen 1),
            ((modMask .|. shiftMask, xK_h), do
             sendToScreen 0
             viewScreen 0),
            ((modMask .|. shiftMask, xK_l), do
             sendToScreen 1
             viewScreen 1),
            ((modMask .|. shiftMask, xK_k), do
             sendToScreen 0
             viewScreen 0),
            ((modMask .|. shiftMask, xK_j), do
             sendToScreen 1
             sendToScreen 1)
            ]
            )
            ]

withWorkspaceByInitial job fstLet =
  do ws <- gets (map W.tag . W.workspaces . windowset)
     case find (\w -> fstLet == w !! 0) ws of
              Just w -> windows $ job w
              Nothing -> promptWorkspaceNameAndJob fstLet job

promptWorkspaceNameAndJob fstLet job = minimalPrompt bigXPConfig {defaultText = [fstLet]} ?+ selectOrAddWorkspaceAndJob job

selectOrAddWorkspaceAndJob job name = do s <- gets windowset
                                         if W.tagMember name s
                                           then windows $ job name
                                           else addWorkspaceAndJob job name
                                         return ()

addWorkspaceAndJob job name = do addHiddenWorkspace name
                                 windows $ job name
                                 return ()

selectOrAddWorkspaceAndMoveThere = selectOrAddWorkspaceAndJob W.greedyView
data MinimalPrompt = MinimalPrompt String

instance XPrompt MinimalPrompt where
  showXPrompt (MinimalPrompt s) = ""

minimalPrompt c = mkXPromptWithReturn (MinimalPrompt undefined) c (const (return [])) return

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

defaultLayout = layoutHintsToCenter (tiled)
            ||| layoutHintsToCenter (Mirror tiled)
  where
     tiled  = Tall 1 (3 / 100) (1 / 2)

myLayout = toggleLayouts Full $ workspaceDir "" $ windowNavigation $ avoidStruts
        $ onWorkspace "im" (withIM (1%7) (Role "buddy_list") defaultLayout)
        $ onWorkspace "procurement" (workspaceDir "~/supply-chain/sc-proc" defaultLayout)
        $ onWorkspace "ui" (workspaceDir "~/supply-chain/sc-proc-ui" defaultLayout)
        $ onWorkspace "fulfillment" (workspaceDir "~/supply-chain/sc-fulfillment" defaultLayout)
        $ onWorkspace "supplier" (workspaceDir "~/supply-chain/sc-supplier" defaultLayout)
        $ onWorkspace "erp" (workspaceDir "~/supply-chain/" defaultLayout)
        $ onWorkspace "core" (workspaceDir "~/supply-chain/sc-core" defaultLayout)
        $ onWorkspace "oms" (workspaceDir "~/supply-chain/sc-oms" defaultLayout)
        $ onWorkspace "qb" (workspaceDir "~/git/qb" defaultLayout)
        $ onWorkspace "wall" defaultLayout
        $ defaultLayout


myManageHook = composeAll .concat $ [[namedScratchpadManageHook scratchpads, manageDocks], [className =? "Do" --> doIgnore ]]

main = xmonad $ ewmh defaultConfig {
        focusFollowsMouse  = True,
        terminal  = "rxvt",
        modMask            = mod4Mask,
        workspaces         = ["im", "supplier", "core", "procurement", "fulfillment", "ui", "oms", "wall"],
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        focusedBorderColor = "#00FF00",
        normalBorderColor = "#000000"
        , borderWidth        = 3
        , manageHook         = manageHook defaultConfig <+> myManageHook
        , logHook            = do
                       dynamicLog
                       updatePointer (Relative 0.5 0.5)
        , startupHook        = do
                  setWMName "LG3D"
                  startupHook defaultConfig
                  spawn "killall xflux; ~/xflux  -l 12.9833 -g 77.5833"
                  spawn "pidgin"
                  spawn "killall parcellite; parcellite"
                  spawn "dropbox start"
        , layoutHook         = windowArrange $ smartBorders $ myLayout
  }

shiftToPreviousWorkspace = do
    hs <- gets (W.hidden . windowset)
    unless (null hs) (windows . (liftM2 (.) W.view W.shift) . W.tag $ head hs)
