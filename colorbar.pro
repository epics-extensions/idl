;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************

PRO setfont,fname,bold=bold,italic=italic,width=width,space=space
	if n_elements(width) eq 0 then width = 9
	if n_elements(space) eq 0 then space = 12 

	charsp = [9,12]
	if fname eq 'Default' or fname eq '' then begin
		!P.FONT = -1   
		DEVICE,SET_CHARACTER_SIZE=charsp
		return
	end

	charsp = [width,space]

	!P.FONT = 1   
	; 1 - true type , 0 - device font, -1 built in Hershey
	fontname = 'Helvetica'
	if n_elements(fname) then fontname=strtrim(fname,2)
	if keyword_set(bold) then fontname = fontname+' BOLD' 
	if keyword_set(italic) then fontname = fontname+' ITALIC' 
	DEVICE,SET_FONT=fontname,SET_CHARACTER_SIZE=charsp,/tt_font

END

PRO putchars,string,x=x,y=y,csize=csize,font=font
	ofont = !p.font
	if keyword_set(font) then begin
	!p.font=1
	DEVICE,SET_FONT=font,/tt_font
	end
	size = 1
	if keyword_set(csize) then size = csize
	if keyword_set(x) then xyouts,x,y,string,charsize=csize else $
	xyouts,string
	!p.font = ofont
END

PRO colorbar_init,colorbar_data

  colorbar_data = { base:0L, $
	xwid : 0L, $
	ywid : 0L, $
	wdwid : 0L, $
	htwid : 0L, $
	minwid : 0L, $
	maxwid : 0L, $
	fmtwid : 0L, $
	caller : '', $
	x : 385, $
	y : 10, $
	width : 20, $
	height : 320, $
	min : 0., $
	max : 100., $
	wid : !d.window, $
	format: 'G10.5', $
	nlabel: 9, $
	horiz: 0 $
	}

END




PRO colorbar_main13_Event, Event
COMMON COLORBAR, colorbar_data

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev
;  WIDGET_CONTROL,Event.Top,GET_UVALUE=colorbar_data

  CASE Ev OF 

  'COLORBAR_POSX': BEGIN
	WIDGET_CONTROL,Event.id,get_value=x
	colorbar_data.x = x(0)	
      END
  'COLORBAR_POSY': BEGIN
	WIDGET_CONTROL,Event.id,get_value=x
	colorbar_data.y = x(0)	
      END
  'COLORBAR_WIDTH': BEGIN
	WIDGET_CONTROL,Event.id,get_value=x
	colorbar_data.width = x(0)	
      END
  'COLORBAR_HEIGHT': BEGIN
	WIDGET_CONTROL,Event.id,get_value=x
	colorbar_data.height = x(0)	
      END
  'COLORBAR_MIN': BEGIN
      END
  'COLORBAR_MAX': BEGIN
      END
  'COLORBAR_FORMAT': BEGIN
	WIDGET_CONTROL,Event.id,get_value=x
	colorbar_data.format = x(0)	
      END
  'COLORBAR_NLABELS': BEGIN
      CASE Event.Value OF
      0: colorbar_data.nlabel = -1 ; Print,'Button 0 Pressed'
      1: colorbar_data.nlabel = 3 ; Print,'Button 3 Pressed'
      2: colorbar_data.nlabel = 5 ; Print,'Button 5 Pressed'
      3: colorbar_data.nlabel = 9 ; Print,'Button 9 Pressed'
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  'COLORBAR_ALIGN': BEGIN
      CASE Event.Value OF
      0: colorbar_data.horiz = 0 ; Print,'Button False Pressed'
      1: colorbar_data.horiz = 1 ; Print,'Button True Pressed'
      ELSE: Message,'Unknown button pressed'
      ENDCASE
	WIDGET_CONTROL,colorbar_data.xwid,GET_VALUE=x
	colorbar_data.x = x(0)
	WIDGET_CONTROL,colorbar_data.ywid,GET_VALUE=x
	colorbar_data.y = x(0)
	WIDGET_CONTROL,colorbar_data.wdwid,GET_VALUE=x
	colorbar_data.width = x(0)
	WIDGET_CONTROL,colorbar_data.htwid,GET_VALUE=x
	colorbar_data.height = x(0)
	WIDGET_CONTROL,colorbar_data.minwid,GET_VALUE=x
	colorbar_data.min = x(0)
	WIDGET_CONTROL,colorbar_data.maxwid,GET_VALUE=x
	colorbar_data.max = x(0)
	WIDGET_CONTROL,colorbar_data.fmtwid,GET_VALUE=x
	colorbar_data.format = x(0)
      END
  'COLORBAR_DONE': BEGIN
	WIDGET_CONTROL,Event.top,/DESTROY
	return
      END
  ENDCASE

   WIDGET_CONTROL,Event.Top,SET_UVALUE=colorbar_data
   r = execute(colorbar_data.caller)

END

PRO colorbar_config, wid=wid, caller=caller, GROUP=Group
COMMON COLORBAR, colorbar_data
;
;  caller is the 'REPLOT' routine name which call color plot
;

  if n_elements(colorbar_data) eq 0 then colorbar_init,colorbar_data
  colorbar_data.caller = ''
  if keyword_set(caller) then colorbar_data.caller = string(caller)

  if keyword_set(wid) eq 0 then begin 
	wid = !d.window
	if wid eq -1 then begin 
		window,0
		wid = !d.window
	end
	colorbar_data.wid = wid
  end	

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  colorbar_main13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, TITLE='Color Bar Dialog', $
      MAP=1, $
      UVALUE='colorbar_main13')
  colorbar_data.base = colorbar_main13


  BASE2 = WIDGET_BASE(colorbar_main13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL3', $
      VALUE='Color Bar Configuration')

  BASE5 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE5')

  FIELD6 = CW_FIELD( BASE5,VALUE=colorbar_data.x, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Position X:', $
      UVALUE='COLORBAR_POSX', $
      XSIZE=5)
  colorbar_data.xwid = FIELD6

  FIELD7 = CW_FIELD( BASE5,VALUE=colorbar_data.y, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Position Y:', $
      UVALUE='COLORBAR_POSY', $
      XSIZE=5)
  colorbar_data.ywid = FIELD7

  FIELD8 = CW_FIELD( BASE5,VALUE=colorbar_data.width, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Width:', $
      UVALUE='COLORBAR_WIDTH', $
      XSIZE=5)
  colorbar_data.wdwid = FIELD8

  FIELD9 = CW_FIELD( BASE5,VALUE=colorbar_data.height, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Height:', $
      UVALUE='COLORBAR_HEIGHT', $
      XSIZE=5)
  colorbar_data.htwid = FIELD9

  BASE19 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE19')

  FIELD41 = CW_FIELD( BASE19,VALUE=colorbar_data.min, $
      ROW=1, $
      FLOAT=1, /NOEDIT, $
      RETURN_EVENTS=1, $
      TITLE='Value Min:', $
      UVALUE='COLORBAR_MIN', $
      XSIZE=12)
  colorbar_data.minwid = FIELD41

  FIELD44 = CW_FIELD( BASE19,VALUE=colorbar_data.max, $
      ROW=1, $
      FLOAT=1, /NOEDIT, $
      RETURN_EVENTS=1, $
      TITLE='Value Max:', $
      UVALUE='COLORBAR_MAX', $
      XSIZE=12)
  colorbar_data.maxwid = FIELD44

  FIELD43 = CW_FIELD( BASE19,VALUE=colorbar_data.format, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Format:', $
      UVALUE='COLORBAR_FORMAT', $
      XSIZE=10)
  colorbar_data.fmtwid = FIELD43


  BASE26 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE26')

  Btns2673 = [ $
    '0', $
    '3', $
    '5', $
    '9' ]
  BGROUP27 = CW_BGROUP( BASE26, Btns2673, $
      ROW=1, $
      EXCLUSIVE=1, $
      FRAME=1, $
      LABEL_LEFT='# of Labels:', $
      UVALUE='COLORBAR_NLABELS')
  WIDGET_CONTROL,BGROUP27,SET_VALUE=3
  if colorbar_data.nlabel eq -1 then WIDGET_CONTROL,BGROUP27,SET_VALUE=0
  if colorbar_data.nlabel eq 3 then WIDGET_CONTROL,BGROUP27,SET_VALUE=1
  if colorbar_data.nlabel eq 5 then WIDGET_CONTROL,BGROUP27,SET_VALUE=2

  Btns2870 = [ $
    'False', $
    'True' ]
  BGROUP30 = CW_BGROUP( BASE26, Btns2870, $
      ROW=1, $
      EXCLUSIVE=1, $
      FRAME=1, $
      LABEL_LEFT='Horizontal:', $
      UVALUE='COLORBAR_ALIGN')
  WIDGET_CONTROL,BGROUP30,SET_VALUE=0
  if colorbar_data.horiz eq 1 then WIDGET_CONTROL,BGROUP30,SET_VALUE=1

  BASE34 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE34')

  BUTTON36 = WIDGET_BUTTON( BASE34, $
      UVALUE='COLORBAR_DONE', $
      VALUE='Done')


  WIDGET_CONTROL, colorbar_main13, /REALIZE
;  WIDGET_CONTROL, colorbar_main13, set_uvalue = colorbar_data

  XMANAGER, 'colorbar_main13', colorbar_main13
END


PRO colorbar,yrange,width,height,horizontal=horizontal,x=x,y=y,wid=wid,ncap=ncap,format=format,PSfact=PSfact,reverse=reverse,ncolors=ncolors
;    width - colorbar width
;    height - colorbar height
;+
; NAME:
;       COLORBAR
;
; PURPOSE:
;       This routine provides the user with the flexibility of specifying 
;       the size and the place of the colorbar to be drawn on the plot. 
;
; CALLING SEQUENCE:
;
;       COLORBAR, Zrange [,Width] [,Height] [,X=x] [,Y=y] [,Wid=wid]
;
; INPUTS:
;       Zrange: [Zmin,Zmax] specifies the minimum and maximum value of
;               data corresponding to the first and last entry of color 
;               table.
;
; OPTIONAL INPUTS:
;       Width:  specify the width of the color bar, default to 20 pixels
;       Height: specify the height of the color bar, default to 320 pixels
;	
; KEYWORD PARAMETERS:
;       X:      specify X location of the lower left corner of the color bar,
;               default to 70 pixels from right boundary for X device
;       Y:      specify Y location of the lower left corner of the color bar,
;               default to 100 pixels from bottom boundary for X device
;       Wid:    specify the destination window the color bar to be drawn,
;               default to current plot device 
;       Ncap:   specify the number of value labels
;       Format: specify the color bar label format
;       PSfact: specify the PS color bar scaling factor, default is 30
;       Reverse: use black color for color bar text color
;       Ncolors: specify the number of colors used from the table
;
; OUTPUTS:
;       None.
;
; RESTRICTION:
;       The Width, Height are absolute pixel numbers respect to the actual
;       plot device. The location X, and Y pixels respect to the plot device
;       origin.
;
; EXAMPLE:
;       Example 1 - Place a colorbar at the default location 70,100 pixels from 
;       the lower right corner of the plot area
;
;		colorbar,[min(z),max(z)]	
;
;       Example 2 - Place a colorbar at the location 10,100 pixels from 
;       the lower left corner of the plot area
;
;		colorbar,[min(z),max(z)],X=10,Y=100	
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, April 9, 1999
;       xx-xx-xxxx  xxx	Comment
;-

if keyword_set(ncolors) eq 0 then ncolors=!d.table_size

	if n_elements(width) eq 0 then width=20
	if n_elements(height) eq 0 then height=320
	if keyword_set(wid) then wset,wid
	fmt = 'G10.5'
	if keyword_set(format) then fmt=format
	tcolor=ncolors -1 ;!d.table_size-1
	if keyword_set(reverse) then tcolor=0

	xsize = !d.x_vsize
	ysize = !d.y_vsize
	nc = ncolors;!d.table_size
	setnc = fix(getenv('IDL_NCOLORS'))
	if setnc gt 0 then nc=setnc	
	ns = 16
	
	fact = 1
	if !d.NAME eq 'PS' then begin
		fact= 30
		tcolor=0
	end
	if keyword_set(PSfact) then fact = PSfact

	; horizontal colorbar
	if keyword_set(horizontal) then begin
	if n_elements(x) eq 0 then x = 60*fact 
	if n_elements(y) eq 0 then y = 25*fact 
	beh = height;    20
	bew = width/ns ;    (xsize-x*2)/ns
	if bew lt 0 then begin
		r = dialog_message('Error: X position too big for horizontal colorbar',/Error)
		return
	end

	dval = (yrange(1)-yrange(0))/ns
	for j=0,ns do begin
	color = (nc/ns)*j 
	if color ge nc then color = nc - 1
	tv,replicate(color,bew,beh), x+j*bew, y
	endfor
	for j=0,ns,8 do begin
	x1=x+j*bew
	str = string(format='('+fmt+')',j*dval+yrange(0))
		xcor=[x1, x1, x1+bew, x1+bew]  ;, x1]
		ycor=[y, y+beh, y+beh, y]  ;, y]
		plots,xcor,ycor ,/device
		xyouts,x+j*bew, y-1.5*!d.y_ch_size, strtrim(str,2),/device,color=tcolor
	endfor
	return
	end

	; vertical colorbar

	bew = width  ;20
	beh = height/ns  ;(ysize-y*2)/ns
	detcap = 2
	if keyword_set(ncap) then detcap = 16 / (ncap-1)	
	if n_elements(x) eq 0 then x = xsize - 70 else x=x
	if n_elements(y) eq 0 then y = 100 else y=y

	dval = (yrange(1)-yrange(0))/ns

	for j=0,ns do begin
	color = (nc/ns)*j 
	if color ge nc then color = nc - 1
	tv,replicate(color,bew,beh),x,y+j*beh,xsize=bew,ysize=beh
	if detcap gt 1 then begin
	jj = j mod detcap ; 2
	if jj eq 0 then begin
	str = string(format='('+fmt+')',j*dval+yrange(0))
		 xyouts,x+1.25*bew,y+j*beh, strtrim(str,2),/device,color=tcolor
		end
	end
	endfor
END	
