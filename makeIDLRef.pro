PRO makeIDLRef
; This program builds the ezcaIDLRef.html from the source files
files = ['catcher_v1.pro','view2d.pro','plot1d.pro']
mk_html_help, files, 'catcher.html'
end
