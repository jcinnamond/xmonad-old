Config {
     font = "xft:Input Mono:size=12:antialias=true"
     , borderColor = "#b4b1b0"
     , border = BottomB
     , bgColor = "#020202"
     , fgColor = "#b4b1b0"
     , position = Top
     , pickBroadest = True
     , commands = [ Run Date "%A %_d %B %H:%M" "date" 60
		  , Run StdinReader
                  , Run Com "/bin/bash" ["-c", "~/bin/media.sh"] "media" 2
		  , Run Com "/bin/bash" ["-c", "~/bin/getvolume.sh"] "vol" 1
		  , Run Com "/bin/bash" ["-c", "~/bin/wifi.sh"] "wifi" 15
		  , Run Battery ["-t", "<acstatus>: <left>%", "--", "-O", "AC", "-o", "Battery", "-l", "red"] 10
                  ]
     , template = " %StdinReader%}{%media% | %wifi% | %battery% | %vol% | %date% "
     }
