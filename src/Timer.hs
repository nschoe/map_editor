module Timer (
               Timer
             , defaultTimer
             , start
             , pause
             , resume
             , stop
             , isStarted
             , isPaused
             , getTimerTicks
             ) where

import Data.Word (Word32)

import qualified Graphics.UI.SDL.Time as SdlTime


data TimerStatus = Ticking | Paused | Stopped
                   deriving (Show, Eq)


data Timer = Timer {
      startTicks   :: Word32
    , pausedTicks  :: Word32
    , status       :: TimerStatus
    } deriving (Show)


defaultTimer :: Timer
defaultTimer = Timer {startTicks=0, pausedTicks=0, status=Stopped}


-- Starts a timer with the current ticks (pausedTicks set to 0 because I dislike trayling values)
start :: Timer -> IO Timer
start timer = SdlTime.getTicks >>= \currTicks -> return $ timer {startTicks=currTicks, pausedTicks=0, status=Ticking}


-- Completely stops a timer, sets its ticks values to 0 (same reason)
stop :: Timer -> Timer
stop timer = timer {startTicks=0, pausedTicks=0, status=Stopped}


-- Pauses a timer: temporarily stops it, saving the ticks for later resume
pause :: Timer -> IO Timer
pause timer@Timer {status=Ticking, startTicks=ticks} = SdlTime.getTicks >>= \currTicks -> return $ timer {status=Paused, pausedTicks=(currTicks - ticks)}
pause timer = return timer


-- Resumes a previously paused timer
resume :: Timer -> IO Timer
resume timer@Timer {status=Paused, pausedTicks=ticks} = SdlTime.getTicks >>= \currTicks -> return $ timer {status=Ticking, startTicks=(currTicks - ticks), pausedTicks=0}
resume timer = return timer


isStarted :: Timer -> Bool
isStarted timer = not $ ((status timer) == Stopped)

isPaused :: Timer -> Bool
isPaused timer = ((status timer) == Paused)


-- Gives back for how many ticks the timer has been ticking
getTimerTicks :: Timer -> IO Word32
getTimerTicks timer@Timer {status=Stopped} = return 0
getTimerTicks timer@Timer {status=Paused, pausedTicks=ticks} = return ticks
getTimerTicks timer@Timer {status=Ticking, startTicks=ticks} = SdlTime.getTicks >>= \currTicks -> return (currTicks - ticks)