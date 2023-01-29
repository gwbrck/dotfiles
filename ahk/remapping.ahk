#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
SendMode Input
SetWorkingDir %A_ScriptDir%

#SingleInstance force
SetStoreCapsLockMode, off
LayerActive := false
holdFlag := false
holdTime := 150

#w::#Down
#Down::#w
#d::#q
#q::Send !{F4}
#m::run outlook.exe

CapsLock::
	global LayerActive := true
	global holdFlag := false
	SetTimer, holdTimer, %holdTime%
        KeyWait, CapsLock
return
CapsLock up::
	SetTimer, holdTimer, Off
	if (holdFlag = false)
        {
		Send {Esc}
		global LayerActive := false
		global holdFlag := false
        }
	else
  	{
		global LayerActive := false
		global holdFlag := false
        }

		
return

;#
$SC02B::
	global LayerActive := true
	global holdFlag := false
	SetTimer, holdTimer, %holdTime%
        KeyWait, SC02B	
return
$SC02B up::
	SetTimer, holdTimer, Off
	if (holdFlag = false)
        {
		Send {#}
		global LayerActive := false
		global holdFlag := false
        }
	else
  	{
		global LayerActive := false
		global holdFlag := false
        }		
return


$-::
	global holdFlag := false
	SetTimer, holdTimer, %holdTime%
	Send {Ctrl DownR} 	
        KeyWait, -   
return
$- up::
	SetTimer, holdTimer, Off
	if (holdFlag = false)
        {
		Send {Ctrl up}
		Send {-}
		global holdFlag := false
        }
	else
  	{
		Send {Ctrl up}
		global holdFlag := false
        }		
return

$<::
	global holdFlag := false
	SetTimer, holdTimer, %holdTime%
	Send {Ctrl DownR} 	
        KeyWait, -   
return
$< up::
	SetTimer, holdTimer, Off
	if (holdFlag = false)
        {
		Send {Ctrl up}
		Send {<}
		global holdFlag := false
        }
	else
  	{
		Send {Ctrl up}
		global holdFlag := false
        }		
return

holdTimer:
	global holdFlag := true
return

#If LayerActive = true
  h::Left
  j::Down
  k::Up
  l::Right
  u::Send {{}
  i::Send {}}
  m::Send {[}
  SC033::Send {]}
  q::Send {@}
  <::Send {|}
  +::Send {~}
  e::Send {€}
#If