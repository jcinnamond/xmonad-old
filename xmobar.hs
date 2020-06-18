Config {
     font = "xft:NotoSans Nerd Font:size=10:antialias=true:hinting=true,NotoEmoji Nerd Font:size=10:antialias=true:hinting=true,Noto Sans CJK JP:size=10:antialias:true:hinting:true,Noto Sans CJK KR:size=10:antialias:true:hinting:true,Noto Sans CJK SC:size=10:antialias:true:hinting:true"
     , borderColor = "#707880"
     , border = BottomB
     , bgColor = "#282a2e"
     , fgColor = "#c5c8c6"
     , position = Top
     , pickBroadest = True
     , commands = [ Run Date "%A %_d %B %H:%M" "date" 60
                  , Run StdinReader
                  , Run Battery ["-t", "<fc=#707880>  </fc> <left><fc=#81a2be>%</fc>",
                                "-l", "red",
                                "-h", "#81a2be"
                                ] 600
                  , Run Com "/bin/bash" ["-c", "~/bin/media"] "media" 2
                  , Run Com "/bin/bash" ["-c", "~/bin/volume"] "vol" 1
                  , Run Com "/bin/sh" ["-c", "~/bin/current_source"] "mic" 1
                  ]
     , template = "  %StdinReader%}{%mic%%media%%vol%%battery%   <fc=#707880></fc>  %date%  "
     }
