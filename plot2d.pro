;
; Auto Save File For plot2d.pro
;
;  Wed Oct 23 10:43:38 CDT 1996
;  a=findgen(40)
;  a=sin(a/5) / exp(a/50)
;  data = a#a
;


PRO plot2d_setupContourLevels_Event, Event
COMMON PLOT2d_BLOCK,plot2d_state

  WIDGET_CONTROL,Event.top,GET_UVALUE=info

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'TEXT16': BEGIN
      END
  'BUTTON18': BEGIN
      Print, 'Event for Accept'
	WIDGET_CONTROL,info.textID,get_value=st
	nlevels = n_elements(st)
	if nlevels gt 12 then begin
		res = dialog_message('Error: only 12 entry allowed!',/Error)
		return
	end
	is=0
	info.levels=make_array(12,/string)
	for i=0,n_elements(st)-1 do begin
		if strtrim(st(i),2) ne '' then begin 
		info.levels(is)=st(i)
;	print,'Level',is, ' = ',st(i)
		if i eq 0 then levels = float(st(i)) else $
		levels= [levels,float(st(i))]
		is = is+1
		end
	end
	info.nlevels = is
	plot2d_state.nlevels=is
	plot2d_state.levels = levels
      END
  'BUTTON19': BEGIN
	WIDGET_CONTROL,Event.top, /DESTROY
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END plot2d_setupContourLevels
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO plot2d_setupContourLevels, GROUP=Group, c_levels


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  plot2d_setupContourLevels = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, title='plot2d_levels',$
      MAP=1, $
      UVALUE='plot2d_setupContourLevels')

  BASE14 = WIDGET_BASE(plot2d_setupContourLevels, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE14')

  LABEL15 = WIDGET_LABEL( BASE14, $
      UVALUE='LABEL15', $
      VALUE='Contour Levels')

	nlevels = 12
	levels = make_array(nlevels,/string)
	if n_elements(c_levels) gt 0 then begin
	for i=0,n_elements(c_levels)-1 do begin
		levels(i) = strtrim(c_levels(i),2)
		end
	end
  TEXT16 = WIDGET_TEXT( BASE14,VALUE=levels(0:n_elements(c_levels)-1), $
      EDITABLE=1, $
      UVALUE='TEXT16', $
      XSIZE=12, $
      YSIZE=12)

  BASE17 = WIDGET_BASE(BASE14, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE17')

  BUTTON18 = WIDGET_BUTTON( BASE17, $
      UVALUE='BUTTON18', $
      VALUE='Accept')

  BUTTON19 = WIDGET_BUTTON( BASE17, $
      UVALUE='BUTTON19', $
      VALUE='Done')

  info = { $
	base:plot2d_setupContourLevels, $
	textID:TEXT16, $
	nlevels: nlevels, $
	levels: levels $
	}

  WIDGET_CONTROL, plot2d_setupContourLevels, set_uvalue=info 
  WIDGET_CONTROL, plot2d_setupContourLevels, /REALIZE

  XMANAGER, 'plot2d_setupContourLevels', plot2d_setupContourLevels, Event_Handler='plot2d_setupContourLevels_event'

END




PRO plot2d_setupMain13_Event, Event
COMMON PLOT2d_BLOCK,plot2d_state

  WIDGET_CONTROL,Event.top,GET_UVALUE=setup_info
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'BGROUP19': BEGIN
      IF Event.Select THEN Sel = 'On' ELSE Sel = 'Off'
      CASE Event.Value OF
      0: plot2d_state.xlog = Event.Select  
      1: plot2d_state.ylog = Event.Select 
      2: plot2d_state.zlog = Event.Select
      3: plot2d_state.lego = Event.Select 
      4: plot2d_state.shade = Event.Select 
      5: plot2d_state.stamp = Event.Select 
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  'plot2d_setupLevels': BEGIN
	plot2d_setupContourLevels, plot2d_state.levels, GROUP=Event.Top 
      END
  'plot2d_setupCharsize': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.charsize = ch(0) 
      END
  'plot2d_setupContourCsize': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.c_charsize = ch(0) 
      END
  'plot2d_setupAx': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	WIDGET_CONTROL,setup_info.Axslider,SET_VALUE=ch
	plot2d_state.Ax = ch(0) 
      END
  'plot2d_setupSLIDER3': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	WIDGET_CONTROL,setup_info.Ax,SET_VALUE=ch
	plot2d_state.Ax =  float(ch)
      END
  'plot2d_setupAz': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	WIDGET_CONTROL,setup_info.Azslider,SET_VALUE=ch
	plot2d_state.Az = ch(0) 
      END
  'plot2d_setupSLIDER4': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	WIDGET_CONTROL,setup_info.Az,SET_VALUE=ch
	plot2d_state.Az =  float(ch)
      END
  'plot2d_setupXleft': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.xmargin1 = ch(0) 
      END
  'plot2d_setupXright': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.xmargin2 = ch(0) 
      END
  'plot2d_setupYbottom': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.ymargin1 = ch(0) 
      END
  'plot2d_setupYTop': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.ymargin2 = ch(0) 
      END
  'plot2d_setupZoomin': BEGIN
	plot2d_state.zoom = plot2d_state.zoom * 1.25
      END
  'plot2d_setupZoomout': BEGIN
	plot2d_state.zoom = plot2d_state.zoom / 1.25 
      END
  'plot2d_setupXtitle': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.xtitle = ch(0) 
      END
  'plot2d_setupYtitle': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.ytitle = ch(0) 
      END
  'plot2d_setupTitle': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.title = ch(0) 
      END
  'plot2d_setupLocX': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.xloc= ch(0) 
      END
  'plot2d_setupLocY': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.yloc= ch(0) 
      END
  'plot2d_setupComment': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.footnote = n_elements(ch)
	plot2d_state.comment = ch (0:n_elements(ch)-1)
;	plot2d_state.comment(0) = plot2d_state.comment(0)+ $
;	  ' (Max='+strtrim(plot2d_state.max,2) + ', Min='+strtrim(plot2d_state.min,2)+')'

      END
  'plot2d_setupCurLevels': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.levels = ch 
      END
  'BUTTON50': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
      END
  ENDCASE
	plot2d_replot, plot2d_state
END


; DO NOT REMOVE THIS COMMENT: END plot2d_setupMain13
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO plot2d_setup, GROUP=Group
COMMON PLOT2d_BLOCK,plot2d_state

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  plot2d_setupMain13 = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      TITLE='plot2d_options', $
      MAP=1, $
      UVALUE='plot2d_setupMain13')

  BASE20 = WIDGET_BASE(plot2d_setupMain13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE20')

  Btns4929 = [ $
    'Xlog', $
    'Ylog', $
    'Zlog', $
    'Lego', $
    'Shade', $
    'Stamp' $
     ]
  BGROUP19 = CW_BGROUP( BASE20, Btns4929, $
      ROW=1, $
      NONEXCLUSIVE=1, $
      LABEL_LEFT='Options:', $
      UVALUE='BGROUP19')

vals = [plot2d_state.xlog, plot2d_state.ylog, plot2d_state.zlog, $
	plot2d_state.lego, plot2d_state.shade, plot2d_state.stamp ]
WIDGET_CONTROL,BGROUP19,set_value=vals

  BASE21 = WIDGET_BASE(plot2d_setupMain13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE21')

  FieldVal4932 = plot2d_state.charsize
  plot2d_setupCharsize = CW_FIELD( BASE21,VALUE=FieldVal4932, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='Charsize', $
      UVALUE='plot2d_setupCharsize', $
      XSIZE=5)

  FieldVal4934 = plot2d_state.c_charsize
  plot2d_setupContourCsize = CW_FIELD( BASE21,VALUE=FieldVal4934, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='Contour-charsize', $
      UVALUE='plot2d_setupContourCsize', $
      XSIZE=5)

  plot2d_setupLevels = WIDGET_BUTTON(BASE21,VALUE='C_Levels ...', $
	UVALUE='plot2d_setupLevels')

  BASE44 = WIDGET_BASE(plot2d_setupMain13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE44')

;  FieldVal4959 = [ $
;    '' ]
;  plot2d_setupCurLevels = CW_FIELD( BASE44,VALUE=FieldVal4959, $
;      ROW=1, $
;      STRING=1, $
;      RETURN_EVENTS=1, $
;      TITLE='Contour-Levels', $
;      UVALUE='plot2d_setupCurLevels', $
;      XSIZE=70)


  BASE24 = WIDGET_BASE(plot2d_setupMain13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE24')

  FieldVal4937 = plot2d_state.ax
  plot2d_setupAx = CW_FIELD( BASE24,VALUE=FieldVal4937, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='Rotation Ax', $
      UVALUE='plot2d_setupAx', $
      XSIZE=5)

  plot2d_setupSLIDER3 = WIDGET_SLIDER( BASE24, $
      MAXIMUM=180, $
      MINIMUM=-180, $
      UVALUE='plot2d_setupSLIDER3', $
      VALUE=plot2d_state.ax, $
      XSIZE=180)

  FieldVal4939 = plot2d_state.az
  plot2d_setupAz = CW_FIELD( BASE24,VALUE=FieldVal4939, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='Az', $
      UVALUE='plot2d_setupAz', $
      XSIZE=5)

  plot2d_setupSLIDER4 = WIDGET_SLIDER( BASE24, $
      MAXIMUM=180, $
      MINIMUM=-180, $
      UVALUE='plot2d_setupSLIDER4', $
      VALUE=plot2d_state.az, $
      XSIZE=180)

  BASE27 = WIDGET_BASE(plot2d_setupMain13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE27')

  FieldVal4942 = plot2d_state.xmargin1
  plot2d_setupXleft = CW_FIELD( BASE27,VALUE=FieldVal4942, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Xmargin Left', $
      UVALUE='plot2d_setupXleft', $
      XSIZE=2)

  FieldVal4944 = plot2d_state.xmargin2
  plot2d_setupXright = CW_FIELD( BASE27,VALUE=FieldVal4944, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Right', $
      UVALUE='plot2d_setupXright', $
      XSIZE=2)

  FieldVal4946 = plot2d_state.ymargin1
  plot2d_setupYbottom = CW_FIELD( BASE27,VALUE=FieldVal4946, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Ymargin Bottom', $
      UVALUE='plot2d_setupYbottom', $
      XSIZE=2)

  FieldVal4948 = plot2d_state.ymargin2
  plot2d_setupYTop = CW_FIELD( BASE27,VALUE=FieldVal4948, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Top', $
      UVALUE='plot2d_setupYTop', $
      XSIZE=2)

  plot2d_setupZoomin = WIDGET_BUTTON( BASE27,VALUE='Zi', $
	UVALUE='plot2d_setupZoomin')
  plot2d_setupZoomout = WIDGET_BUTTON( BASE27,VALUE='Zo', $
	UVALUE='plot2d_setupZoomout')

  BASE36 = WIDGET_BASE(plot2d_setupMain13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE36')

  FieldVal4951 = plot2d_state.xtitle
  plot2d_setupXtitle = CW_FIELD( BASE36,VALUE=FieldVal4951, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Xtitle', $
      UVALUE='plot2d_setupXtitle', $
      XSIZE=20)

  FieldVal4953 = plot2d_state.ytitle
  plot2d_setupYtitle = CW_FIELD( BASE36,VALUE=FieldVal4953, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Ytitle', $
      UVALUE='plot2d_setupYtitle', $
      XSIZE=20)


  BASE41 = WIDGET_BASE(plot2d_setupMain13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE41')

  FieldVal4956 = plot2d_state.title
  plot2d_setupTitle = CW_FIELD( BASE41,VALUE=FieldVal4956, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Title', $
      UVALUE='plot2d_setupTitle', $
      XSIZE=40)

  BASE42 = WIDGET_BASE(BASE41, $
      ROW=1, FRAME=1, $
      MAP=1, $
      UVALUE='BASE42')

  cmt = plot2d_state.comment(0:plot2d_state.footnote-1)
  plot2d_setupComment = CW_FIELD( BASE42,VALUE=cmt, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Comment', $
      UVALUE='plot2d_setupComment', $
      XSIZE=40,YSIZE=5)

  BASE42_2 = WIDGET_BASE(BASE42, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE42_2')
;  plot2d_setupLoc = WIDGET_LABEL(BASE42_2,value='Location')
  plot2d_setupLocX = CW_FIELD( BASE42_2,VALUE=plot2d_state.xloc, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='rXLoc', $
      UVALUE='plot2d_setupLocX', $
      XSIZE=5)
  plot2d_setupLocY = CW_FIELD( BASE42_2,VALUE=plot2d_state.yloc, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='rYLoc', $
      UVALUE='plot2d_setupLocY', $
      XSIZE=5)

  BASE47 = WIDGET_BASE(plot2d_setupMain13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE47')

  BUTTON50 = WIDGET_BUTTON( BASE47, $
      UVALUE='BUTTON50', $
      VALUE='Close')

  setup_info = { $
	Ax: plot2d_setupAx, $
	Az: plot2d_setupAz, $
	Axslider: plot2d_setupSLIDER3, $
	Azslider: plot2d_setupSLIDER4 $
	}

  WIDGET_CONTROL, plot2d_setupMAIN13, set_uvalue=setup_info
  WIDGET_CONTROL, plot2d_setupMain13, /REALIZE

  XMANAGER, 'plot2d_setupMain13', plot2d_setupMain13
END

PRO plot2d_replot, plot2d_state

if !d.name ne 'PS' then WSET,plot2d_state.win
!p.multi = [0,1,0,0,0]
erase


s  = size(plot2d_state.data)
no = s(0)
if no gt 0 then begin
dim = make_array(no)
dim = s(1:no)
end
type = s(n_elements(s)-2)

		; byte type treat as string

                if type eq 1 then begin
                plot,[-1,-1],XStyle=4,YStyle=4
                yloc = 0.9*!d.y_size
                        for j=0,dim(1)-1 do begin
                        yloc = yloc - !d.y_ch_size
                        if yloc ge 0 then begin
                        str=plot2d_state.data(0:dim(0)-1,j)
                        xyouts, 0.2*!d.x_size, yloc, string(str),/DEVICE
                        end
                        end
		return
		end

maxvl = plot2d_state.max
minvl = plot2d_state.min
xmargin = plot2d_state.zoom *[plot2d_state.xmargin1,plot2d_state.xmargin2]
ymargin = plot2d_state.zoom *[plot2d_state.ymargin1,plot2d_state.ymargin2]

CASE plot2d_state.plottype OF
    0: begin

        ncolors = !d.table_size
        xrange=[plot2d_state.xarr(0), plot2d_state.xarr(dim(0)-1)]
        yrange=[plot2d_state.yarr(0), plot2d_state.yarr(dim(1)-1)]
 
	left=!d.x_size * 0.2
	right=!d.x_size * 0.1
	bottom=!d.y_size *0.1
	top=!d.y_size *0.1
	width=!d.x_size-left-right
	height=!d.y_size-top-bottom
	data = plot2d_state.data
	if !d.name ne 'PS' then data = CONGRID(plot2d_state.data,width,height)
        TVSCL,data,left,bottom,xsize=width,ysize=height
 
        plot,xrange=xrange,yrange=yrange,/nodata,[-1,-1],/noerase, $
                pos=[float(left)/!d.x_size, float(bottom)/!d.y_size, $
                 (!d.x_size-float(right))/!d.x_size, $
		 (!d.y_size-float(top))/!d.y_size], $
                xstyle=1, ystyle=1, xtitle=plot2d_state.xtitle, $
		ytitle=plot2d_state.ytitle, $
                title=plot2d_state.title

	plot2d_notes,plot2d_state
	end
    3: begin
	shades = (plot2d_state.data-minvl)/(maxvl-minvl)*!d.table_size
	shade_surf,plot2d_state.data, plot2d_state.xarr, plot2d_state.yarr, $
		xlog=plot2d_state.xlog,ylog=plot2d_state.ylog, $
		charsize=plot2d_state.charsize, $
		ax = plot2d_state.ax, az=plot2d_state.az, $
		shades=shades, $
		xmargin=xmargin, ymargin=ymargin, $
                title=plot2d_state.title, xtitle=plot2d_state.xtitle, $
		ytitle=plot2d_state.ytitle
	plot2d_notes,plot2d_state
	end
    1: begin
	if plot2d_state.shade then begin 
	shades = (plot2d_state.data-minvl)/(maxvl-minvl)*!d.table_size
	surface,plot2d_state.data, plot2d_state.xarr, plot2d_state.yarr, $
		xlog=plot2d_state.xlog,ylog=plot2d_state.ylog, $
		zlog=plot2d_state.zlog, $
		charsize=plot2d_state.charsize, $
		lego=plot2d_state.lego, $
		ax = plot2d_state.ax, az=plot2d_state.az, $
		shades=shades, $
		xmargin=xmargin, ymargin=ymargin, $
                title=plot2d_state.title, xtitle=plot2d_state.xtitle, $
		ytitle=plot2d_state.ytitle
	endif else begin
	surface,plot2d_state.data, plot2d_state.xarr, plot2d_state.yarr, $
		xlog=plot2d_state.xlog,ylog=plot2d_state.ylog, $
		zlog=plot2d_state.zlog, $
		charsize=plot2d_state.charsize, $
		lego=plot2d_state.lego, $
		ax = plot2d_state.ax, az=plot2d_state.az, $
		xmargin=xmargin, ymargin=ymargin, $
                title=plot2d_state.title, xtitle=plot2d_state.xtitle, $
		ytitle=plot2d_state.ytitle
	end
	plot2d_notes,plot2d_state
	end
    2: begin

	contour,plot2d_state.data, plot2d_state.xarr, plot2d_state.yarr, $
		title=plot2d_state.title,xtitle=plot2d_state.xtitle, $
		ytitle=plot2d_state.ytitle, $
		xmargin=xmargin, ymargin=ymargin, $
		max_value= maxvl, min_value=minvl, $
		charsize=1, c_charsize=1.2, $
		c_colors=plot2d_state.colorI,$
		levels=plot2d_state.levels(0:plot2d_state.nlevels-1), $
		NLevels=plot2d_state.nlevels, /Follow
	plot2d_notes,plot2d_state
	end
    ELSE: print,'No plot supported for this case'
ENDCASE
END

PRO plot2d_notes,plot2d_state
; draw footnote comment

if plot2d_state.footnote ne 0 then begin

        real_xl = plot2d_state.xloc * !d.x_size
        real_dy = !d.y_ch_size     ; character pixel height
	real_yl = plot2d_state.yloc * !d.y_size
        for i=0,plot2d_state.footnote -1 do begin
        xyouts,real_xl,(real_yl-i*real_dy), plot2d_state.comment(i), /DEVICE
	end
end
; draw stamp comment

if plot2d_state.stamp ne 0 then begin
        if strtrim(plot2d_state.timestamp,2) lt 2 then st = systime(0) else $
		st = plot2d_state.timestamp
        xyouts,0.01*!d.x_size, 1, st, /device
        xyouts,0.75*!d.x_size, 1, $
                 'User Name:  '+getenv('USER'), /device
end

END

;
; inorder to support TV,SURFACE,CONTOUR, the common block is required
;
PRO MAIN13_Event, Event
COMMON PLOT2D_BLOCK,plot2d_state
;  WIDGET_CONTROL,Event.top,GET_UVALUE=plot2d_state

; if top resize  event plot2d the base widget and redraw its plot;
IF (Event.id EQ Event.top) THEN BEGIN
        WIDGET_CONTROL,plot2d_state.id_draw, SCR_XSIZE=Event.x, SCR_YSIZE=Event.y

        ; if device is X
        if !d.name ne 'PS' then  WSET,plot2d_state.win

        plot2d_replot, plot2d_state

;        WIDGET_CONTROL,Event.top,SET_UVALUE=plot2d_state
        return
ENDIF

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'DRAW3': BEGIN
      Print, 'Event for plot2d_area'
      END
  'BGROUP2': BEGIN
      CASE Event.Value OF
      0: plot2d_state.plottype= 0  	;tv
      1: plot2d_state.plottype= 1  	;surface
      2: plot2d_state.plottype= 2  	;contour
      3: plot2d_state.plottype= 3  	;shade_surf
	ELSE: print,'error'
      ENDCASE
      plot2d_replot, plot2d_state
      END
  'BGROUP6': BEGIN
      CASE Event.Value OF
      0: begin
	 set_plot,'PS'
        !P.FONT=-1     ; maintain consistent labelling across all IDL devices
        device,/Courier,/Bold, /color, bits=8
        plot2d_replot, plot2d_state
        !P.FONT=-1
        device,/close
        spawn,'lpr idl.ps'
        print,'Print idl.ps'
        set_plot,'X'
	end
      1: begin
	xloadct, GROUP=Event.top
	end
      2: begin
	plot2d_setup, GROUP=Event.top
	end
      3: begin
	WIDGET_CONTROL,Event.top,BAD=bad,/DESTROY
	catch,error_status
	if error_status eq 0 then $
	WSET,plot2d_state.old_win
	end
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END MAIN13
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO plot2d,data,tlb,win, width=width, height=height, $
	charsize=charsize, lego=lego, ax=ax, az=az, shade=shade, $
	title=title, xtitle=xtitle, ytitle=ytitle, ztitle=ztitle, $
	xarr=xarr,yarr=yarr, $
	rxloc=rxloc, ryloc=ryloc, comment=comment, $
	stamp=stamp, GROUP=Group

COMMON PLOT2D_BLOCK,plot2d_state
;+
; NAME:
;       PLOT2D
;
; PURPOSE:
;       This routine provides a general purpose, flexible generic 2D plot
;       package.  It provides simple to use and various features of 
;       automatic generating 2D TV, SURFACE, CONTOUR, and SHADE_SURF plot.
;
;       The window generated by this routine will be resizable by the
;       window manager.
;
;       Depress the 'Print' button will generate a postscript copy of the
;       graph.
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;
;       PLOT2D, DATA,  TLB, WIN
;
; INPUTS:
;       DATA:   The 2D array to be plotted.
;
; KEYWORD PARAMETERS:
;       XARR:   Set this keyword to specify the corresponding x vector values.
;
;       YARR:   Set this keyword to specify the corresponding y vector values.
;
;       TITLE:  Set this keyword to specify the plot title string.
;
;       XTITLE: Set this keyword to specify the xtitle string.
;
;       YTITLE: Set this keyword to specify the ytitle string.
;
;       ZTITLE: Set this keyword to specify the ztitle string.
;
;       CHARSIZE: Set this keyword to specify the plot charsize, default 1.
;
;       COMMENT:  Set this keyword to write any notes on the graph.
;
;       RXLOC:  Set notes X ratio to plot device width, default 0.01.
;
;       RYLOC:  Set notes Y ratio to plot device height, default 0.98.
;
;       STAMP:  Print the time stamp on the graph.
;
;       WTITLE: Set this keyword to specify the window title string,
;               default to 'Plot2d'.
;
;       WIDTH:  The initial window width at the creation time, which
;               default to 600 pixel.
; 
;       HEIGHT: The initial window height at the creation time, which
;               default to 450 pixel.
;
;       AX:     This keyword specifies the rotated angle about the x-axis.
;               AX>0 toward the viewer, AX<0 away from the viewer, default
;               +30 degree (Surface plot)
;
;       AZ:     This keyword specifies the rotated angle about the z-axis,
;               default 30 degree. (Surface plot)
;
;       LEGO:   This keyword specifies the z value as stacked histogram
;               style plot. (Surface plot)
;
;       SHADE:  This keyword specifies the color shade for the surface plot. 
;
;       GROUP:  The widget ID of the group leader of the widget. If this
;               keyword is specified, the death of the group leader results
;               in the death of PLOT2D.
;
; OPTIONAL_OUTPUTS:
;       TLB: The widget ID of the top level base returned by the PLOT2D.
;
;       WIN: The window ID of the drawing area used by the PLOT2D.
;
; COMMON BLOCKS:
;       COMMON PLOT2D_BLOCK plot2d_state.
;
; SIDE EFFECTS:
;       The max and min value will be shown as the default comment.
;
; RESTRICTIONS:
;
; EXAMPLES:
;       Create a resizable 2D plot without any title or label
;       specification.
;
;	PLOT2D, data
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Dec 16, 1998.
;       12-22-1998      Add zoom in/out button to control X, Y margins
;
;-


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

xsize=600
ysize=450
xl = ''
yl =''
zl =''
ti = ''			; plot title
wti='Plot2d'		; window title
cl = !d.n_colors
footnote = make_array(10,/string)
timestamp=''
xloc = 0.01
yloc = 0.98

if keyword_set(width) then xsize=width
if xsize lt 350 then xsize=350
if keyword_set(height) then ysize=height
if ysize lt 350 then ysize=350

if keyword_set(rxloc) then xloc = float(rxloc)
if keyword_set(ryloc) then yloc = float(ryloc)
if keyword_set(title) then ti = string(title)
if keyword_set(xtitle) then xl = string(xtitle)
if keyword_set(ytitle) then yl = string(ytitle)
if keyword_set(wtitle) then wti = string(wtitle)
if keyword_set(comment) then footnote = string(comment)
if keyword_set(stamp) then timestamp = strtrim(stamp,2)

; set x,y array 
sz = size(data)
xdim = sz(1)
ydim = sz(2)
xarray = indgen(xdim)
yarray = indgen(ydim)
if keyword_set(xarr) then xarray = xarr
if keyword_set(yarr) then yarray = yarr

  plot2d_state = { $
	data:data, $
	max:0, $
	min:0, $
	x:1, $
	y:1, $
	xarr:xarray, $
	yarr:yarray, $
	plottype:1, $     	;surface
	charsize:1, $
	c_charsize:1.2, $
	xlog:0, $
	ylog:0, $
	zlog:0, $
	ax:30, $
	az:30, $
	lego:0, $
	shade:0, $
	title:ti, $
	xtitle:xl, $
	ytitle:yl, $
	ztitle:zl, $
	footnote: 1, $
	comment:footnote, $
	xloc:xloc, $
	yloc:yloc, $
	zoom:1.0, $
	xmargin1:10, $
	xmargin2:5, $
	ymargin1:5, $
	ymargin2:5, $
	stamp: 0, $
	timestamp: timestamp, $
	nlevels: 12, $
	levels: make_array(12), $
	colorI: indgen(12), $
	base:0L, $
	id_draw:0L, $
	win:0, $
	old_win: !d.window, $
	xsize: xsize, $
	ysize: ysize $
	}

	maxvl = max(data,min=minvl)
	plot2d_state.max = maxvl
	plot2d_state.min = minvl

; 12 number of curves allowed
	nlevels = plot2d_state.nlevels
	dlevels=(maxvl-minvl)/nlevels
	levels = make_array(nlevels)
	for i=0,nlevels-1 do begin
	levels(i) = minvl + dlevels * i
	end
	plot2d_state.levels = levels

; set colors of line
        dcl = !d.table_size - 2
        ncv = 12  ; 4
        colorlevel = dcl / ncv
        for i=0,11 do begin
        ii = i / ncv
        im = i mod ncv
        plot2d_state.colorI(i) = dcl - ii - im * colorlevel
        end

	xmaxvl = max(data,min=xminvl)
	ymaxvl = max(data,min=yminvl)
if keyword_set(xlog) and xminvl gt 0  then plot2d_state.xlog=xlog
if keyword_set(ylog) and yminvl gt 0  then plot2d_state.ylog=ylog
if keyword_set(zlog) and minvl gt 0  then plot2d_state.zlog=zlog
if keyword_set(charsize) then plot2d_state.charsize=charsize
if keyword_set(az) then plot2d_state.az=az
if keyword_set(ax) then plot2d_state.ax=ax
if keyword_set(lego) then plot2d_state.lego=lego
if keyword_set(comment) then begin
        add_line = n_elements(comment)
        plot2d_state.footnote= add_line
        end
        plot2d_state.comment(0) = plot2d_state.comment(0)+ $
		' (Max='+strtrim(maxvl,2) + ', Min='+strtrim(minvl,2)+')'

  MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      /TLB_SIZE_EVENTS, $	; resizable window
      /COLUMN, $
      MAP=1, $
      TITLE='PLOT2D', $
      UVALUE='MAIN13')

  BASE1 = WIDGET_BASE(MAIN13, /ROW)
  Btns111 = [ $
    'TV', $
    'SURFACE', $
    'CONTOUR', $
    'SHADE_SURF'] 
  BGROUP2 = CW_BGROUP( BASE1, Btns111, $
      ROW=1, UVALUE= 'BGROUP2') 

  DRAW3 = WIDGET_DRAW( MAIN13, XSIZE=xsize, YSIZE=ysize, RETAIN=2)

  BASE2 = WIDGET_BASE(MAIN13, /ROW)

  Btns361 = [ $
    'Print', $
    'Colors', $
    'Plot Options ...', $ 
    'Done' ]
  BGROUP6 = CW_BGROUP( BASE2, Btns361, $
      ROW=1, $
      UVALUE='BGROUP6')

  g_tlb = WIDGET_INFO(MAIN13,/geometry)

  WIDGET_CONTROL, MAIN13, /REALIZE

  ; Get drawable window index

  WIDGET_CONTROL, DRAW3, GET_VALUE=drawWin
  ; WSET, drawWin

  ;surface, data

  tlb = MAIN13
  win = DRAW3
	
	plot2d_state.base = tlb 
	plot2d_state.id_draw = DRAW3
	plot2d_state.win = drawWin 
	plot2d_state.xsize = g_tlb.scr_xsize
	plot2d_state.ysize = g_tlb.scr_ysize

  	plot2d_replot, plot2d_state

  WIDGET_CONTROL, MAIN13, SET_UVALUE=plot2d_state

  XMANAGER, 'MAIN13', MAIN13
END
