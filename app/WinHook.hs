{-# LINE 1 "WinHook.hsc" #-}
module WinHook where
{-# LINE 2 "WinHook.hsc" #-}
import System.Win32.Types (maybePtr, DWORD, WPARAM, LPARAM, LRESULT, HANDLE, HINSTANCE, MbHINSTANCE)
import Foreign


{-# LINE 6 "WinHook.hsc" #-}

type KBHookProc = Int -> WPARAM -> Ptr KBDLLHOOKSTRUCT -> IO LRESULT

data KBDLLHOOKSTRUCT = KBDLLHOOKSTRUCT { vkCode :: DWORD
                                       , scanCode :: DWORD
                                       , flags :: DWORD
                                       , time :: DWORD
                                       , dwExtraInfo :: DWORD } -- don't use this.


instance Storable KBDLLHOOKSTRUCT where
  sizeOf _    = ((24))
{-# LINE 18 "WinHook.hsc" #-}
  alignment _ = alignment (undefined :: DWORD)
  peek ptr = do
    vkcode <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 21 "WinHook.hsc" #-}
    scancode <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 22 "WinHook.hsc" #-}
    flags <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 23 "WinHook.hsc" #-}
    time <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 24 "WinHook.hsc" #-}
    extra <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 25 "WinHook.hsc" #-}
    return KBDLLHOOKSTRUCT { vkCode = vkcode
                           , scanCode = scancode
                           , flags = flags
                           , time = time
                           , dwExtraInfo = extra }
  poke ptr (KBDLLHOOKSTRUCT vk scan flags time extra) = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr vk
{-# LINE 32 "WinHook.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr scan
{-# LINE 33 "WinHook.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr flags
{-# LINE 34 "WinHook.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr time
{-# LINE 35 "WinHook.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr extra
{-# LINE 36 "WinHook.hsc" #-}
                       
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
