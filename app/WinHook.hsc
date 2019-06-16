module WinHook where
import System.Win32.Types (maybePtr, DWORD, WPARAM, LPARAM, LRESULT, HANDLE, HINSTANCE, MbHINSTANCE)
import Foreign
import Foreign.C.Types

#include "windows.h"

type KBHookProc = Int -> WPARAM -> Ptr KBDLLHOOKSTRUCT -> IO LRESULT

data KBDLLHOOKSTRUCT = KBDLLHOOKSTRUCT { vkCode :: DWORD
                                       , scanCode :: DWORD
                                       , flags :: DWORD
                                       , time :: DWORD
                                       , dwExtraInfo :: DWORD } -- don't use this.


instance Storable KBDLLHOOKSTRUCT where
  sizeOf _    = (#size KBDLLHOOKSTRUCT)
  alignment _ = alignment (undefined :: DWORD)
  peek ptr = do
    vkcode <- (#peek KBDLLHOOKSTRUCT, vkCode) ptr
    scancode <- (#peek KBDLLHOOKSTRUCT, scanCode) ptr
    flags <- (#peek KBDLLHOOKSTRUCT, flags) ptr
    time <- (#peek KBDLLHOOKSTRUCT, time) ptr
    extra <- (#peek KBDLLHOOKSTRUCT, dwExtraInfo) ptr
    return KBDLLHOOKSTRUCT { vkCode = vkcode
                           , scanCode = scancode
                           , flags = flags
                           , time = time
                           , dwExtraInfo = extra }
  poke ptr (KBDLLHOOKSTRUCT vk scan flags time extra) = do
    (#poke KBDLLHOOKSTRUCT, vkCode) ptr vk
    (#poke KBDLLHOOKSTRUCT, scanCode) ptr scan
    (#poke KBDLLHOOKSTRUCT, flags) ptr flags
    (#poke KBDLLHOOKSTRUCT, time) ptr time
    (#poke KBDLLHOOKSTRUCT, dwExtraInfo) ptr extra
                       
wH_MOUSE_LL = 14 :: Int
wH_KEYBOARD_LL = 13 :: Int

foreign import ccall "wrapper"
  mkKBHookClosure :: KBHookProc -> IO (FunPtr KBHookProc)

setWindowsHookEx :: Int -> KBHookProc -> MbHINSTANCE -> DWORD -> IO ()
setWindowsHookEx nCode hookFunc hMod t = do
  fn <- mkKBHookClosure hookFunc
  c_setWindowsHookEx nCode fn (maybePtr hMod) t

foreign import ccall unsafe "user32.h SetWindowsHookExW"
  c_setWindowsHookEx :: Int -> FunPtr KBHookProc -> HINSTANCE -> DWORD -> IO ()


callNextHookEx :: Int -> WPARAM -> Ptr KBDLLHOOKSTRUCT -> IO LRESULT
callNextHookEx nCode wp lp =
  c_callNextHookEx 0 nCode wp lp

foreign import ccall unsafe "user32.h CallNextHookEx"
  c_callNextHookEx :: Int -> Int -> WPARAM -> Ptr KBDLLHOOKSTRUCT -> IO LRESULT
