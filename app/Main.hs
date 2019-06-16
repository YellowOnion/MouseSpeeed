{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Graphics.Win32
import System.Win32.DLL (getModuleHandle)
import Control.Exception (SomeException, catch)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Exit (ExitCode(ExitSuccess), exitWith)
import Foreign
import Data.Bits

import System.IO

import Debug.Trace

import Graphics.Win32.Key
import qualified Language.C.Inline as C


flush = hFlush stdout

C.include "windows.h"


data RawMouse = RawMouse {
  rmX :: Int,
  rmY :: Int,
  rmTime :: Int
} deriving (Show)

createWindow :: Int -> Int -> Graphics.Win32.WindowClosure -> IO Graphics.Win32.HWND
createWindow width height wndProc = do
  let winClass = Graphics.Win32.mkClassName "Hello"
  icon         <- Graphics.Win32.loadIcon   Nothing Graphics.Win32.iDI_APPLICATION
  cursor       <- Graphics.Win32.loadCursor Nothing Graphics.Win32.iDC_ARROW
  bgBrush      <- Graphics.Win32.createSolidBrush (Graphics.Win32.rgb 0 0 255)
  mainInstance <- getModuleHandle Nothing
  Graphics.Win32.registerClass
      ( Graphics.Win32.cS_VREDRAW + Graphics.Win32.cS_HREDRAW
    , mainInstance
    , Just icon
    , Just cursor
    , Just bgBrush
    , Nothing
    , winClass
    )
  w <- Graphics.Win32.createWindow
       winClass
     "Hello, World example"
     Graphics.Win32.wS_OVERLAPPEDWINDOW
     Nothing Nothing -- leave it to the shell to decide the position
          -- at where to put the window initially
                 (Just width)
     (Just height)
     (Just $ Graphics.Win32.castUINTPtrToPtr (-3)) -- message pump only. 
     Nothing      -- no menu handle
     mainInstance
     wndProc
  Graphics.Win32.showWindow w Graphics.Win32.sW_SHOWNORMAL
  Graphics.Win32.updateWindow w
  return w

wndProc ::
           TQueue RawMouse
        -> Graphics.Win32.HWND
        -> Graphics.Win32.WindowMessage
        -> Graphics.Win32.WPARAM
        -> Graphics.Win32.LPARAM
        -> IO Graphics.Win32.LRESULT
wndProc rawChan hwnd wmsg wParam lParam 
 | wmsg == Graphics.Win32.wM_DESTROY = do
     Graphics.Win32.sendMessage hwnd Graphics.Win32.wM_QUIT 1 0
     return 0
 | wmsg == 0x00FF = do -- WM_INPUT
    alloca $ \(x :: Ptr C.CInt) ->
      alloca $ \(y :: Ptr C.CInt) -> do
        [C.block| 
          void
          {
            UINT dwSize;
            GetRawInputData((HRAWINPUT)$(intptr_t lParam), RID_INPUT, NULL, &dwSize, 
                              sizeof(RAWINPUTHEADER));
            BYTE lpb[dwSize];

            if (lpb == NULL) 
              {
                  return;
              } 
          
            GetRawInputData((HRAWINPUT)$(intptr_t lParam), RID_INPUT, lpb, &dwSize, 
                            sizeof(RAWINPUTHEADER));
                 
            RAWINPUT* raw = (RAWINPUT*)lpb;
    
            if (raw->header.dwType == RIM_TYPEMOUSE && raw->data.mouse.usFlags == 0) 
            {
              *$(int* x) = raw->data.mouse.lLastX;
              *$(int* y) = raw->data.mouse.lLastY;
            }
          }
        |]
        x' <- peek x
        y' <- peek y
        atomically $ writeTQueue rawChan $ RawMouse (fromIntegral x') (fromIntegral y') 0
    return 0

 | otherwise = Graphics.Win32.defWindowProc (Just hwnd) wmsg wParam lParam

msgPump :: Graphics.Win32.HWND
        -> IO ()
msgPump hwnd = Graphics.Win32.allocaMessage $ \ msg ->
  let pump = do
        Graphics.Win32.getMessage msg (Just hwnd)
          `catch` \(_::SomeException) -> exitWith ExitSuccess
        Graphics.Win32.translateMessage msg
        Graphics.Win32.dispatchMessage msg
        pump
  in pump

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Hello World"

  finalOutVar <- atomically $ newTVar (0) :: IO (TVar Float)
  lastRawVar  <- atomically $ newTVar (RawMouse 0 0 0) :: IO (TVar RawMouse)
  rawChan  <- atomically $ newTQueue :: IO (TQueue RawMouse)

  forkIO . forever $ do
    (raw, last) <- atomically $ do
      raw <- readTQueue rawChan
      last <- swapTVar lastRawVar raw
      return (raw, last)
    atomically $ do
      let speed (RawMouse x1 y1 _) (RawMouse x2 y2 _) = sqrt $ ((x2' - x1')**2) + ((y2' - y1')**2)
            where [x1', x2', y1', y2'] = map ((*0.6299216) . fromIntegral) [x1, x2, y1, y2]
      swapTVar finalOutVar $ speed last raw
      return ()
  putStrLn "Processing thread started"    

  forkIO . forever $ do
    x <- atomically $ readTVar finalOutVar
    putStrLn (show x)
    threadDelay (1000000)
  putStrLn "Output Thread Started"    

  hWnd <- createWindow 300 400 (wndProc rawChan)
  putStrLn "Window Created"    
  [C.block| void  {
    RAWINPUTDEVICE Rid[1];
    Rid[0].usUsagePage = 0x01; 
    Rid[0].usUsage = 0x02; 
    Rid[0].dwFlags = RIDEV_INPUTSINK | RIDEV_NOLEGACY;   
    Rid[0].hwndTarget = $(void *hWnd);
    RegisterRawInputDevices(Rid, 1, sizeof(Rid[0]));
  }|]
  putStrLn "Raw device registered"
  msgPump hWnd

