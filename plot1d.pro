@PS_open.pro


PRO catch1d_get_pvtcolor,i,color
; 24 bits
	catch1d_get_pvtct,red,green,blue
	color = red(i) + green(i)*256L + blue(i)*256L ^2
;	plot,indgen(10),color=color
END

PRO catch1d_save_pvtct
	tvlct,red,green,blue,/get
	save,red,green,blue,file='catch1d.tbl'
END

PRO catch1d_get_pvtct,red,green,blue
	file = 'catch1d.tbl'
	found = findfile(file)
	if found(0) eq '' then begin
		file =getenv('EPICS_EXTENSIONS_PVT')+'/bin/'+getenv('HOST_ARCH')+'/catch1d.tbl'
		found1 = findfile(file)
		if found1(0) eq '' then $
		file =getenv('EPICS_EXTENSIONS')+'/bin/'+getenv('HOST_ARCH')+'/catch1d.tbl'
	end
	restore,file
	tvlct,red,green,blue
END

PRO plot1d_replot,state
COMMON Colors,r_orig,g_orig,b_orig,r_curr,g_curr,b_curr

	state.xsize = !d.x_size
	state.ysize = !d.y_size
	cl = state.color
	psym = state.symbol
	thick = state.thick
	line = state.linestyle
	y = state.y
	s = size(y)

; set xy margin

	xmgin = state.xmargin
	ymgin = state.ymargin 

; one dim array 

if !d.name ne 'PS' then WSET,state.winDraw
!p.multi = [0,1,0,0,0]
erase

if !d.name eq 'PS' then cl = 0

color = cl 
if !d.n_colors eq 16777216 then catch1d_get_pvtcolor,cl,color
if s(0) eq 1 then $
	PLOT,state.x,state.y, COLOR=color, $
		xrange=state.xrange, yrange = state.yrange, $
		ylog=state.ylog, xlog=state.xlog, psym=psym, $
		thick=thick, xthick=thick, ythick=thick,$
		xstyle = state.xstyle, ystyle = state.ystyle, $
		linestyle=line, $
		xmargin=xmgin, ymargin=ymgin, $
		title=state.title, xtitle=state.xtitle, ytitle=state.ytitle

; two dim array - multiple curves

if s(0) eq 2 and s(2) gt 1 then begin
in_color = make_array(s(2),/int,value=cl)
in_line = make_array(s(2),/int)
in_symbol = make_array(s(2),/int)
in_color(0)= cl
in_line(0)= line
in_symbol(0)=psym
	PLOT,state.x,state.y,COLOR=color, $
		xrange=state.xrange, yrange = state.yrange, $
		ylog=state.ylog, xlog=state.xlog, $
		thick=thick, xthick=thick, ythick=thick,$
		linestyle=line, psym=psym, $
		xmargin=xmgin, ymargin=ymgin, $
		title=state.title, xtitle=state.xtitle, ytitle=state.ytitle
	
;	dcl = state.color / 16
	dcl = !d.table_size-2
	ncv = 7
	colorlevel = dcl / ncv
	for i= 1 ,s(2) - 1 do begin
		if state.autocolor eq 1 then begin
		ii = i / ncv
		im = i MOD ncv
		cl = dcl -ii -im*colorlevel
		in_color(i) = cl
		color = cl
		; if 24 bits use cl_val
        	if !d.n_colors eq 16777216 then begin
                	catch1d_get_pvtcolor,cl,t_color
                	color = t_color
                	end

		end
		if psym gt 0 then psym = psym+1
		if psym lt 0 then psym = psym-1

; the symbol 2 is too light, skip it
if i eq 2 and psym lt 0 then psym=psym-1
if i eq 2 and psym gt 0 then psym=psym+1

		if line gt 0 then line = line+1
		in_line(i) = line
		in_symbol(i) = psym 
		z = y(0:s(1)-1,i)

		if state.curvfit then begin
			psym=7      ; if curve fitting is true
		end
		OPLOT,state.x,z,COLOR=color,linestyle=line, psym=psym mod 7, $
			thick=thick 
		psym = in_symbol(i)
; print,i,psym,cl,line
	end
end

; draw footnote comment

if state.footnote ne 0 then begin

	real_xl = 0.01*state.xsize
	real_dy = !d.y_ch_size     ; character pixel height
	real_yl = (state.footnote+1)*real_dy
	for i=0,state.footnote -1 do begin
	xyouts,real_xl,(real_yl-i*real_dy), state.comment(i), /DEVICE
	end
end

; draw stamp comment
	
if state.stamp ne 0 then begin
	st = systime(0)
	xyouts,0.01*state.xsize, 1, st, /device
	xyouts,0.75*state.xsize, 1, $
		 'User Name:  '+getenv('USER'), /device
end

; draw legend

if s(0) eq 2 and state.legendon gt 0 then begin

	real_x1 = state.xylegend(0)*(!x.crange(1)-!x.crange(0)) + $
		!x.crange(0)
	real_y1 = state.xylegend(1)*(!y.crange(1)-!y.crange(0)) + $
		!y.crange(0)
	real_dy = 0.1 * (!y.crange(1)-!y.crange(0))

	real_xl = real_x1 + 0.16*(!x.crange(1)-!x.crange(0))
	real_yl = real_y1 - 0.5*real_dy

	xyouts,real_x1,real_y1,'LEGEND'
	oplot,[real_x1,real_xl],[real_yl,real_yl],thick=2

	real_xl = real_x1 + 0.075*(!x.crange(1)-!x.crange(0))
	real_xr = real_x1 + 0.1*(!x.crange(1)-!x.crange(0))

	for i=0,n_elements(state.legend)-1 do begin

	real_yl = real_y1 - (i*0.5+1)*real_dy

	x=[real_x1,real_xl]
	y=[real_yl,real_yl]
	color = in_color(i)
        ; if 24 bits use cl_val
	if !d.n_colors eq 16777216 then begin
	catch1d_get_pvtcolor,in_color(i),t_color
	color = t_color
	end

	if psym ne 0 then $
	oplot,x,y,linestyle=in_line(i),color=color,thick=2
	oplot,x,y,linestyle=in_line(i),color=color,psym=in_symbol(i),thick=2

	xyouts,real_xr,real_yl, state.legend(i)
	end
end

END

PRO plot1d_event,ev 

; resize event

;WIDGET_CONTROL, ev.top, GET_UVALUE = state, /NO_COPY
WIDGET_CONTROL, ev.top, GET_UVALUE = state

IF (ev.id EQ ev.top) then begin
; plot1d the draw widget and redraw its plot;
	WIDGET_CONTROL,state.id_draw, SCR_XSIZE=ev.x, SCR_YSIZE=ev.y

	; if device is X
	if !d.name ne 'PS' then  WSET,state.winDraw

	plot1d_replot, state

	WIDGET_CONTROL,ev.top,SET_UVALUE=state,/NO_COPY
	return
ENDIF

WIDGET_CONTROL,ev.Id,GET_UVALUE=B_ev
CASE B_ev OF
'PLOT1D_PRINT': Begin
	PS_open, 'idl.ps'
	linestyle = state.linestyle
	state.autocolor = 0
	state.linestyle = 1
	plot1d_replot, state
	PS_close
	PS_print, 'idl.ps'
	state.autocolor = 1
	state.linestyle = linestyle 
	end
'PLOT1D_CLOSE': begin
	WIDGET_CONTROL,ev.top,BAD=bad,/DESTROY
	end
ENDCASE
END

PRO plot1d, x, y, id_tlb, windraw, $
	title=title,xtitle=xtitle,ytitle=ytitle,color=color, $
	symbol=symbol, thick=thick, linestyle=linestyle, $
        xrange=xrange, yrange=yrange, xlog=xlog, ylog=ylog, $
	xmargin=xmargin, ymargin=ymargin, stamp=stamp,$
	legend=legend, xylegend=xylegend, $
	width=width, height=height, $
	comment=comment, cleanup=cleanup, $
	curvfit=curvfit, $
	xstyle=xstyle, ystyle=ystyle, $
	wtitle=wtitle, button=button, GROUP=GROUP
;+
; NAME:
;       PLOT1D
;
; PURPOSE:
;       This routine provides a general purpose flexible cartesion plot
;       package.  It provides simple to use automatic feature of labels,
;       legend, comment, line style, symbols, and color options on plot.
;
;       The window generated by this routine will be resizable by the 
;       window manager. 
;
;       Depress the 'Print' button will generate a postscript copy of the
;       graph.
;
;       Normally it accepts two parameters X and Y. If the first parameter
;       is not used then the data array is plotted on the ordinate versus 
;       the point number on the abscissa.  Multiple curves (or variables) 
;       can be stacked into the second parameter as a two dimensional array, 
;       the first dimension gives the number of data points in each curve,
;       the second dimension gives the number of curves to be plotted.
;  
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;
;       PLOT1D, [X,] Y [,ID_TLB]  [,ID_DRAW]
;
; INPUTS:
;       X:      The vector array for X abscissa.
;
;       Y:      The Y data array for curve plot. The Y array can contain
;               more than one curve, the first dimension gives the number
;               of data to be plotted for the curve, the second dimension
;               gives the number of curves to be plotted.
;	
; KEYWORD PARAMETERS:
;       TITLE:  Set this keyword to specify the plot title string.
;
;       XTITLE: Set this keyword to specify the xtitle string.
;
;       YTITLE: Set this keyword to specify the ytitle string.
;
;       COLOR:  Set this keyword to specify the color number used
;               in the plot routine.
;
;      CURVFIT: Set this keyword if two curves are plotted, first curve
;               is the fitted curve, the second curve is data to be fitted. 
;
;       SYMBOL: Set this keyword to specify data plotted as symbol, set to -1
;               data plot as symbol and connected with line.
;
;       XLOG:   Set this keyword to specify a logrithmic X axis.
;
;       YLOG:   Set this keyword to specify a logrithmic Y axis.
;
;       XRANGE: Set this keyword to specify the desired data range for 
;               the X axis.
;
;       YRANGE: Set this keyword to specify the desired data range for 
;               the Y axis.
;
;       XMARGIN: Set this keyword to specify the left and right margin, 
;                default xmargin=[10,3]
;
;       YMARGIN: Set this keyword to specify the bottom and top margin 
;                default ymargin=[5,3]
;
;       THICK:  Set this keyword to specify the line thickness for the
;               axes and the line plot. 
;
;       LINESTYLE:  Set this keyword to turn on different line style used. 
;
;       XSTYLE:  Set this keyword to control x axis in IDL plot routine. 
;
;       YSTYLE:  Set this keyword to control y axis in IDL plot routine. 
;
;       LEGEND:  Set the legend strings corresponding to curves drawn.
;
;       XYLEGEND: Set the x,y location of the legend strings, % from the
;                 lower left corner from the graph window, default 
;                 xylegend=[0.75, 0.35].
;
;       COMMENT:  Set this keyword to write any footnotes on the graph.
;
;       STAMP:  Set this keyword to put the time stamp and user ID on the page. 
;
;       WTITLE: Set this keyword to specify the window title string,
;               default to 'Plot1d'.
;
;       WIDTH:  The initial window width at the creation time, which 
;               default to 350 pixel.
;  
;       HEIGHT: The initial window height at the creation time, which 
;               default to 350 pixel.
;
;       GROUP:  The widget ID of the group leader of the widget. If this
;               keyword is specified, the death of the group leader results 
;               in the death of PLOT1D.
;
;       BUTTON: Set this keyword if no print and close buttons are desired
;               for the PLOT1D widget.
;
;       CLEANUP: Set this keyword if the created window can no be closed by the
;                window manager is desired.
;
; OPTIONAL_OUTPUTS:
;       ID_TLB: The widget ID of the top level base returned by the PLOT1D. 
;
;       ID_DRAW: The widget ID of the drawing area used by the PLOT1D. 
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       If more than one curves are stored in the Y array, it automatically
;       uses different color for each curve. If the color keyword is set
;       by the user, then the specified color will be used for the whole plot. 
;
; RESTRICTIONS:
;       It is assumed that the X position array is the same for multiple
;       curve plot. 
;
; EXAMPLES:
;       Create a resizable line plot without any title or label 
;       specification.
;
;           x = !pi * indgen(100)/25
;           PLOT1D, x, sin(x)
;
;       Create a resizable line plot with title specifications. 
;
;           PLOT1D, x, sin(x), title='title', xtitle='xtitle', ytitle='ytitle'
;
;       Plot two curves with different linestyle and legend at default location.
;
;           x=indgen(100)
;           y=make_array(100,2)
;           y(0,0)=sin(x * !pi / 50)
;           y(0,1)=cos(x * !pi / 50)
;           PLOT1D,x,y,legend=['line1','line2'],/linestyle
;
;       Same as the above example plus symbol and a specified legend location.
;
;           x=indgen(100)
;           y=make_array(100,2)
;           y(0,0)=sin(x * !pi / 50)
;           y(0,1)=cos(x * !pi / 50)
;           PLOT1D,x,y,/linestyle,symbol=-1, $
;              legend=['line1','line2'], xylegend=[0.5,0.9]
;
;       Plot x,y array plus two lines of comment and a time stamp on the graph.
;     
;           PLOT1D,x,y,comment=['Comment line1','Comment line2'],/stamp
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, Mar. 7, 1996.
;
;       04-26-96 bkc   Add the window cleanup keyword 
;       10-28-96 bkc   Add the xstyle and ystyle keywords 
;       07-01-97 bkc   Comment out LOADCT,39 inherit color from parent process 
;       08-11-97 bkc   Add the curvfit support, only two curves allowed 
;       12-22-97 bkc   Add the 24 bit color visual device support 
;-

;LOADCT,39

; check any data provided

n1 = n_elements(x)
if n1 lt 2 then begin
	print,'Error: No data specified.'
	return
	end
xa = x

if n_elements(y) eq 0 then begin
	ya = xa
	s = size(ya)
	n1 = s(1)
	xa = indgen(n1)
endif else ya = y

; check for input labels
xsize=350
ysize=350
xl = ''
yl =''
ti = ''
wti='Plot1d'
cl = !d.table_size - 1
leg =['']
footnote = ''
add_line=0
if keyword_set(title) then ti = string(title)
if keyword_set(xtitle) then xl = string(xtitle)
if keyword_set(ytitle) then yl = string(ytitle)
if keyword_set(wtitle) then wti = string(wtitle)
if keyword_set(legend) then leg = string(legend)
if keyword_set(comment) then footnote = string(comment)

state = { $
	id_draw:0L, $
	winDraw:0L,$
	autocolor: 1, $ 	; automatic use different color for each curve
	color:cl, $
	symbol: 0, $
	curvfit: 0, $		; whether data is from curve fitting
	xtitle:xl, $
	ytitle:yl, $
	title:ti, $
	xstyle:0,$
	ystyle:0,$
	xsize:0,$
	ysize:0,$
	xlog: 0, $
	ylog: 0, $
	xmargin: [10,3], $
	ymargin: [5,3], $
	stamp: 0, $
	footnote: 0, $
	comment: footnote, $
        xrange: [min(xa),max(xa)], $
        yrange: [min(ya),max(ya)], $
	legendon: 0, $
	legend: leg, $
	xylegend: [.75,0.35], $
	thick: 2, $
	linestyle: 0, $
	x: xa, $
	y: ya $
	}

if keyword_set(xstyle) then state.xstyle = xstyle 
if keyword_set(ystyle) then state.ystyle = ystyle 

if keyword_set(color) then begin
	cl = long(color)
	state.color = cl
	state.autocolor = 0   ; use fixed color
	end
if keyword_set(symbol) then begin
	state.symbol = symbol
	end
	psym = state.symbol
if keyword_set(xlog) then state.xlog=1
if keyword_set(ylog) then state.ylog=1
if keyword_set(thick) then state.thick= thick
if keyword_set(linestyle) then state.linestyle=1 
if keyword_set(stamp) then state.stamp=1 
if keyword_set(legend) then state.legendon = 1
if keyword_set(xylegend) then begin 
	if n_elements(xylegend) eq 2 then state.xylegend=xylegend
	end
if keyword_set(xrange) then begin 
	if n_elements(xrange) eq 2 then state.xrange=xrange
	end
if keyword_set(yrange) then begin 
	if n_elements(yrange) eq 2 then state.yrange=yrange
	end
if keyword_set(xmargin) then begin 
	if n_elements(xmargin) eq 2 then state.xmargin=xmargin
	end
if keyword_set(comment) then begin
	add_line = n_elements(comment)
	state.footnote= add_line
	ymargin=[5+add_line, 3]
	end
if keyword_set(ymargin) then begin 
	if n_elements(ymargin) eq 2 then state.ymargin=ymargin
	end

if keyword_set(curvfit) then state.curvfit = 1

if keyword_set(width) then xsize=width
if xsize lt 350 then xsize=350
if keyword_set(height) then ysize=height
if ysize lt 350 then ysize=350

; drawing with data 

if keyword_set(CLEANUP) then $
id_tlb=WIDGET_BASE(Title=wti,/COLUMN, /TLB_SIZE_EVENTS, TLB_FRAME_ATTR=8) $
else $
id_tlb=WIDGET_BASE(Title=wti,/COLUMN, /TLB_SIZE_EVENTS)
;WIDGET_CONTROL,id_tlb,default_font='-*-Helvetica-Bold-R-Normal--*-120-*75-*'
id_draw=WIDGET_DRAW(id_tlb,xsize=xsize,ysize=ysize, RETAIN=2)

if keyword_set(button) eq 0 then begin
id_tlb_row=WIDGET_BASE(id_tlb,/ROW)
id_tlb_print = WIDGET_BUTTON(id_tlb_row,VALUE='Print',UVALUE='PLOT1D_PRINT')
id_tlb_close = WIDGET_BUTTON(id_tlb_row,VALUE='Close',UVALUE='PLOT1D_CLOSE')
end

g_tlb=WIDGET_INFO(id_tlb,/geometry)

WIDGET_CONTROL,id_tlb, /realize
WIDGET_CONTROL,id_draw,get_value=windraw
	state.winDraw = windraw
	state.id_draw = id_draw
	state.xsize = g_tlb.scr_xsize
	state.ysize = g_tlb.scr_ysize
	if !d.name ne 'PS' then WSET,windraw
	
	plot1d_replot, state

WIDGET_CONTROL,id_tlb,set_uvalue=state,/no_copy
xmanager,'plot1d',id_tlb,  GROUP_LEADER=GROUP, $
	EVENT_HANDLER="plot1d_event"

END
