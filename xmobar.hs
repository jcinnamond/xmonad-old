Config {
     font = "xft:Noto Sans Mono:size=10:antialias=true"
     , borderColor = "#707880"
     , border = BottomB
     , bgColor = "#282a2e"
     , fgColor = "#c5c8c6"
     , position = Top
     , pickBroadest = True
     , commands = [ Run Date "%A %_d %B %H:%M" "date" 60
		  , Run StdinReader
                  , Run Com "/bin/bash" ["-c", "~/bin/media"] "media" 2
                  , Run Com "/bin/bash" ["-c", "~/bin/volume"] "vol" 1
                  , Run Com "/bin/bash" ["-c", "~/bin/battery"] "bat" 1
                  ]
     , template = " %StdinReader%}{%media%%vol%%bat%%date% "
     }
