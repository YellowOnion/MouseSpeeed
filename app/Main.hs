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

import qualified Linear as L

import Graphics.Win32.Key
import qualified Language.C.Inline as C

import Text.Printf
import qualified Data.Text as T
import Options.Applicative
import Data.Semigroup ((<>))

import qualified Network.WebSockets as WS

C.include "windows.h"

data Config = Config
  { cDecay     :: Float
  , cDpi       :: Int
  }

config :: Parser Config
config = Config
      <$> option auto
          ( long "decay"
          <> short 'c'
          <> help "the amount in m subtracted from the peak speed every second" )
      <*> option auto
          ( long "dpi"
          <> short 'd'
          <> help "the DPI/CPI of your mouse" )

type RawMouse = L.V2 Int

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
     Graphics.Win32.wS_DISABLED
     Nothing Nothing 
     (Just width)
     (Just height)
     (Just $ Graphics.Win32.castUINTPtrToPtr (-3)) -- message pump only. 
     Nothing
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
        atomically $ writeTQueue rawChan $ L.V2 (fromIntegral x') (fromIntegral y')
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

opts :: ParserInfo Config 
opts = info (config <**> helper) 
         ( fullDesc
         <> progDesc "mouse speed measurement tool")

main :: IO ()
main = do
  configs <- execParser $ opts 

  hSetBuffering stdout LineBuffering

  lastRawVar  <- atomically $ newTVar (0.0) :: IO (TVar Float)
  rawChan  <- atomically $ newTQueue :: IO (TQueue RawMouse)
  outputChan <- atomically $ newTChan :: IO (TChan String)

  let dpi = 1600
      dpm = (fromIntegral $ cDpi configs) * 39.3701
      mpd = 1 / dpm
      hz  = 15.625
      mcdelay = floor $ 1 / hz * 1000000
  --h <- openFile (cFile configs) WriteMode
  --hSetBuffering h LineBuffering
  forkIO . forever $ do
    atomically $ do
      rawLs <- flushTQueue rawChan
      last <- readTVar lastRawVar
      let last' = last - ((/hz) $ cDecay configs)
          raw = foldr (+) (L.V2 0 0) rawLs
          last'' = if last' < 0.0 then 0.0 else last'
          speed x = ((L.distance (L.V2 0 0) x) * mpd) * hz
          lastFinal = max last'' $ speed $ fmap fromIntegral raw
      writeTVar lastRawVar lastFinal
      writeTChan outputChan $ printf "%2.3f m/s " lastFinal

    threadDelay mcdelay

  let outputThreadDispatcher f = do
        chan <- atomically $ dupTChan outputChan
        f chan
      
  mapM_ (outputThreadDispatcher) [
    (\chan -> forkIO . forever $ do
      x <- atomically $ readTChan chan
      putStr $ "\r" ++ x
      hFlush stdout
    ),
    (\chan ->
      forkIO . WS.runServer ("127.0.0.1") 9160
       $ \pending -> do
         conn <- WS.acceptRequest pending
         forkIO . forever $ do 
          msg :: T.Text <- WS.receiveData conn
          return ()

         forever $ do 
          -- _ <- WS.receiveData conn
          msg <- atomically $ readTChan chan
          WS.sendTextData conn $ T.pack msg
    )
    ]
  
  forkOS $ do
    hWnd <- createWindow 300 400 (wndProc rawChan)
    [C.block| void  {
      RAWINPUTDEVICE Rid[1];
      Rid[0].usUsagePage = 0x01; 
      Rid[0].usUsage = 0x02; 
      Rid[0].dwFlags = RIDEV_INPUTSINK | RIDEV_NOLEGACY;   
      Rid[0].hwndTarget = $(void *hWnd);
      RegisterRawInputDevices(Rid, 1, sizeof(Rid[0]));
    }|]
    msgPump hWnd
  _ <- getLine
  return ()
