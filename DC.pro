; $Id: DC.pro,v 1.54 2013/06/04 19:33:01 rivers Exp $

pro my_box_cursor, x0, y0, nx, ny, INIT = init, FIXED_SIZE = fixed_size, $
	MESSAGE = message
;+
; NAME:
;	BOX_CURSOR
;
; PURPOSE:
;	Emulate the operation of a variable-sized box cursor (also known as
;	a "marquee" selector).
;
; CATEGORY:
;	Interactive graphics.
;
; CALLING SEQUENCE:
;	BOX_CURSOR, x0, y0, nx, ny [, INIT = init] [, FIXED_SIZE = fixed_size]
;
; INPUTS:
;	No required input parameters.
;
; OPTIONAL INPUT PARAMETERS:
;	x0, y0, nx, and ny give the initial location (x0, y0) and 
;	size (nx, ny) of the box if the keyword INIT is set.  Otherwise, the 
;	box is initially drawn in the center of the screen.
;
; KEYWORD PARAMETERS:
;	INIT:  If this keyword is set, x0, y0, nx, and ny contain the initial
;	parameters for the box.
;
;	FIXED_SIZE:  If this keyword is set, nx and ny contain the initial
;	size of the box.  This size may not be changed by the user.
;
;	MESSAGE:  If this keyword is set, print a short message describing
;	operation of the cursor.
;
; OUTPUTS:
;	x0:  X value of lower left corner of box.
;	y0:  Y value of lower left corner of box.
;	nx:  width of box in pixels.
;	ny:  height of box in pixels. 
;
;	The box is also constrained to lie entirely within the window.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	A box is drawn in the currently active window.  It is erased
;	on exit.
;
; RESTRICTIONS:
;	Works only with window system drivers.
;
; PROCEDURE:
;	The graphics function is set to 6 for eXclusive OR.  This
;	allows the box to be drawn and erased without disturbing the
;	contents of the window.
;
;	Operation is as follows:
;	Left mouse button:   Move the box by dragging.
;	Middle mouse button: Resize the box by dragging.  The corner
;		nearest the initial mouse position is moved.
;	Right mouse button:  Exit this procedure, returning the 
;			     current box parameters.
;
; MODIFICATION HISTORY:
;	DMS, April, 1990.
;	DMS, April, 1992.  Made dragging more intutitive.
;	June, 1993 - Bill Thompson
;			prevented the box from having a negative size.
;       04-18-96   bkc  Made the box color more visible.
;       05-28-98   bkc  Reset bounding box color 
;-

device, get_graphics = old, set_graphics = 6  ;Set xor
col = !d.table_size - 2

if keyword_set(message) then begin
	st = [$,
	"Drag Left button to move box.",$
	"Drag Middle button near a corner to resize box.",$
	"Right button when done."]
	res=WIDGET_MESSAGE(st)
	endif

if keyword_set(init) eq 0 then begin  ;Supply default values for box:
	if keyword_set(fixed_size) eq 0 then begin
		nx = !d.x_size/8   ;no fixed size.
		ny = !d.x_size/8
		endif
	x0 = !d.x_size/2 - nx/2
	y0 = !d.y_size/2 - ny/2
	endif

	if nx lt 0 then begin
		x0 = x0 + nx
		nx = -nx
	endif
	if ny lt 0 then begin
		y0 = y0 + ny
		ny = -ny
	endif

	x0 = x0 > 0
	y0 = y0 > 0
	x0 = x0 < (!d.x_size-1 - nx)	;Never outside window
	y0 = y0 < (!d.y_size-1 - ny)

	px = [x0, x0 + nx, x0 + nx, x0, x0] ;X points
	py = [y0, y0, y0 + ny, y0 + ny, y0] ;Y values

	plots,px, py, col=col, /dev, thick=3, lines=0  ;Draw the box

	cursor, x, y, 2, /dev	;Wait for a button

button = 0
while 1 do begin
	old_button = button
	cursor, x, y, 2, /dev	;Wait for a button
	button = !err
	if (old_button eq 0) and (button ne 0) then begin
		mx0 = x		;For dragging, mouse locn...
		my0 = y		
		x00 = x0	;Orig start of ll corner
		y00 = y0
		endif
	if !err eq 1 then begin  ;Drag entire box?
		x0 = x00 + x - mx0
		y0 = y00 + y - my0
		endif
	if (!err eq 2) and (keyword_set(fixed_size) eq 0) then begin ;New size?
		if old_button eq 0 then begin	;Find closest corner
			mind = 1e6
			for i=0,3 do begin
				d = float(px(i)-x)^2 + float(py(i)-y)^2
				if d lt mind then begin
					mind = d
					corner = i
					endif
			   endfor
			nx0 = nx	;Save sizes.
		   	ny0 = ny
			endif
		dx = x - mx0 & dy = y - my0	;Distance dragged...
		case corner of
		0: begin x0 = x00 + dx & y0 = y00 + dy
			nx = nx0 -dx & ny = ny0 - dy & endcase
		1: begin y0 = y00 + dy
			nx = nx0 + dx & ny = ny0 - dy & endcase
		2: begin nx = nx0 + dx & ny = ny0 + dy & endcase
		3: begin x0 = x00 + dx
			nx = nx0 -  dx & ny = ny0 + dy & endcase
		endcase
		endif
	plots, px, py, col=col, /dev, thick=3, lines=0	;Erase previous box
	empty				;Decwindow bug

	if !err eq 4 then begin  ;Quitting?
		device,set_graphics = old
		return
		endif
middle:
	if nx lt 0 then begin
		x0 = x0 + nx
		nx = -nx
	endif
	if ny lt 0 then begin
		y0 = y0 + ny
		ny = -ny
	endif

	x0 = x0 > 0
	y0 = y0 > 0
	x0 = x0 < (!d.x_size-1 - nx)	;Never outside window
	y0 = y0 < (!d.y_size-1 - ny)

	px = [x0, x0 + nx, x0 + nx, x0, x0] ;X points
	py = [y0, y0, y0 + ny, y0 + ny, y0] ;Y values

	plots,px, py, col=col, /dev, thick=3, lines=0  ;Draw the box

	wait, .1		;Dont hog it all
	endwhile
end
;
; Auto Save File For xy_coord.pro
;
;  Wed Aug  2 15:51:05 CDT 1995
;


PRO xycoord, clean=clean
COMMON CATCH1D_COM, widget_ids, scanData
COMMON XY_COORD_BLOCK, xy_id, xy_wid
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

; wipe out the old value
if xy_id.plot eq 1 then begin
    if w_plotspec_id.log ne 1 then begin
	oplot,[xy_id.x0,xy_id.x0],!y.crange, line = 1, color=0
	oplot,!x.crange,[xy_id.y0,xy_id.y0], line = 1, color=0
    endif else begin    ; YLOG
	oplot,[xy_id.x0,xy_id.x0],10^!y.crange, line = 1, color=0
	oplot,!x.crange,[xy_id.y0,xy_id.y0], line = 1, color=0
    end
end
if keyword_set(clean) then return

cursor,x,y,0,/normal
x = (x - !x.window(0)) / (!x.window(1)-!x.window(0)) * $
	(!x.crange(1)-!x.crange(0)) + !x.crange(0)

y = (y - !y.window(0)) / (!y.window(1)-!y.window(0)) * $
	(!y.crange(1)-!y.crange(0)) + !y.crange(0)

if w_plotspec_id.log eq 1 then begin    ; YLOG
	y = 10^y
	oplot,[x,x],10^!y.crange, line = 1
	oplot,!x.crange,[y,y], line = 1
endif else begin
	oplot,[x,x],!y.crange, line = 1
	oplot,!x.crange,[y,y], line = 1
end

st = 'Cursor X Y value @ button 1 pressed: '+string(x) +string(y)
if scanData.debug gt 0 then print,st

xy_id.x0 = x
xy_id.y0 = y
xy_id.x1 = x
xy_id.y1 = y
xy_id.plot = 1
;xy_id.st = st

if xy_wid.base ne 0 then begin
WIDGET_CONTROL,	xy_wid.x, SET_VALUE = strtrim(xy_id.x0,2)
WIDGET_CONTROL,	xy_wid.y, SET_VALUE = strtrim(xy_id.y1,2) 
WIDGET_CONTROL,	xy_wid.motor, SET_VALUE =  'Ref Positioner # '+strtrim(w_plotspec_id.xcord+1,2)
end

END

PRO xycoord_setmotor_confirmed
COMMON GOTO_BLOCK,goto_n,goto_pv,goto_val
	pv = goto_pv(0:goto_n-1)
	val = goto_val(0,0:goto_n-1)
	id = caputArray(pv,val)
	if id ne 0 then w_warningtext,'Error: in Goto setting !',40,3 
END

PRO xycoord_setmotor,val,scanpv=scanpv
; if scanpv set then the current scan record PV names setting is used otherwise
; the read in positioner PV name is used 
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus
COMMON w_warningtext_block,w_warningtext_ids
COMMON GOTO_BLOCK,goto_n,goto_pv,goto_val

   x_axis = w_plotspec_id.x_axis_u

	num_pts =  1 > (scanData.act_npts - 1)
	
		x1 = MAX((*scanData.pa)[0:num_pts,w_plotspec_id.xcord])
		x0 = MIN((*scanData.pa)[0:num_pts,w_plotspec_id.xcord])
	if x1 eq x0 then begin
		w_warningtext,'Error: Invalid request !'
		return
		end
	f1 = (val - x0) / (x1 - x0)
	if x_axis eq 1 then f1 = val / num_pts

	goto_val = make_array(1,4,/double)
	goto_pv = w_plotspec_id.goto_pv  ;make_array(4,/string)

	def = scanData.p_def

        piname=scanData.pv+['.P1PV','.P2PV','.P3PV','.P4PV']
	ti = where (def > 0)
	if ti(0) eq -1 then return
	piname = piname(ti)

	if keyword_set(SCANPV) then begin
	ln = cagetArray(piname, goto_pv, /string)
	if ln lt 0 then return 
	end

	k=0
	for i=0,3 do begin
		if def(i) then begin
		s1 = goto_pv(i)
		if strtrim(s1,2) ne '' then begin
		xmax = MAX((*scanData.pa)(0:num_pts,i))
		xmin = MIN((*scanData.pa)(0:num_pts,i))
		goto_val(0,i) = xmin + f1 * (xmax - xmin)	
		k=k+1
		end
		end
	end

	if k lt 1 then return      ; none defined
	goto_n = k
	st = 'Set New Positions:'
	for i=0,goto_n-1 do begin
	st = [st,goto_pv(i)+ ' --> ' + string(goto_val(0,i))]	
	end

	w_warningtext,st,45,5,'Set Positioner Locations',title='GoTo ...',quest='GoTo'
	return
	
END

PRO XYCOORD_BASE_Event, Event
COMMON XY_COORD_BLOCK, xy_id, xy_wid

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'XY_COORD_FIELD4': BEGIN
	WIDGET_CONTROL,xy_wid.x,GET_VALUE=xvalue
      END
  'XY_COORD_FIELD7': BEGIN
;	WIDGET_CONTROL,xy_wid.y,GET_VALUE=yvalue
      END
  'BUTTON9': BEGIN
	WIDGET_CONTROL,xy_wid.x,GET_VALUE=xvalue
	val = float(xvalue(0))
	xycoord_setmotor,val
	WIDGET_CONTROL, xy_wid.base,/DESTROY,BAD_ID=bad
      END
  'BUTTON10': BEGIN
	WIDGET_CONTROL,Event.top,/DESTROY
	xy_wid.base = 0L
	xycoord,/CLEAN
;	UPDATE_PLOT,1
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END XYCOORD_BASE
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO xy_coord, GROUP=Group
COMMON XY_COORD_BLOCK, xy_id, xy_wid
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

if XRegistered('XYCOORD_BASE') NE 0 then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

if n_elements(xy_id) eq 0 then begin
xy_id = { $
        plot : 0, $
        x0 : 0., $
        x1 : 0., $
        y0 : 0., $
        y1 : 0., $
        st : '' $
        }
        end

  XYCOORD_BASE = WIDGET_BASE(GROUP_LEADER=Group, $
	/COLUMN, MAP=1, $
      TITLE='XY-COORD', $
      UVALUE='XYCOORD_BASE')

  XY_COORD_MOTOR = WIDGET_LABEL(XYCOORD_BASE, $
	VALUE='Ref Positioner # '+strtrim(w_plotspec_id.xcord+1,2))

; x value 
  BASE3 = WIDGET_BASE(XYCOORD_BASE, ROW=1, MAP=1, UVALUE='BASE3')

  label_p1 = WIDGET_LABEL(BASE3,VALUE='X: ')
  XY_COORD_FIELD4 = WIDGET_TEXT( BASE3,VALUE='', $
      EDITABLE=1, UVALUE='XY_COORD_FIELD4', XSIZE=20)

; y value 
  BASE4 = WIDGET_BASE(XYCOORD_BASE, ROW=1, MAP=1, UVALUE='BASE4')
  label_d1 = WIDGET_LABEL(BASE4,VALUE='Y: ')
  XY_COORD_FIELD7 = WIDGET_TEXT( BASE4,VALUE='', $
      EDITABLE=1, UVALUE='XY_COORD_FIELD7', XSIZE=20)

; close button
  BASE5 = WIDGET_BASE(XYCOORD_BASE, ROW=1, MAP=1, UVALUE='BASE5')
  BUTTON9 = WIDGET_BUTTON( BASE5, $
      UVALUE='BUTTON9', $
      VALUE='GoTo ...')
  BUTTON10 = WIDGET_BUTTON( BASE5, $
      UVALUE='BUTTON10', $
      VALUE='Close')


xy_wid = { $
	base : XYCOORD_BASE, $ 
	motor : XY_COORD_MOTOR, $
	x : XY_COORD_FIELD4, $
	y : XY_COORD_FIELD7 $
	}

DEVICE,GET_SCREEN_SIZE=ssize

  WIDGET_CONTROL, XYCOORD_BASE, /REALIZE, $
	TLB_SET_XOFFSET= ssize(0)-200, TLB_SET_YOFFSET= 100

  XMANAGER, 'XYCOORD_BASE', XYCOORD_BASE
END

PRO w_warningtext_quest
COMMON w_warningtext_block,w_warningtext_ids

	WIDGET_CONTROL,w_warningtext_ids.text,GET_VALUE=ans
	w_warningtext_ids.answer = strtrim(strupcase(ans(0)),2)
	WIDGET_CONTROL,w_warningtext_ids.base,BAD_ID=bad,/DESTROY
	if w_warningtext_ids.answer eq 'Y' then begin
		if w_warningtext_ids.quest eq 'GoTo' then $
			xycoord_setmotor_confirmed
		if w_warningtext_ids.quest eq 'Get Scan Data and Save' then begin
			catch1dReadScanRecordAppendFile 
			end
	endif else begin   ; 'N'
		if w_warningtext_ids.quest eq 'APPEND' then $
			catch1d_append
	end
END

PRO w_warningtext_event,event
COMMON w_warningtext_block,w_warningtext_ids

WIDGET_CONTROL, event.id, GET_UVALUE = eventval
CASE eventval OF
        "WARNINGTEXT_GET" : BEGIN
		WIDGET_CONTROL,event.id,GET_VALUE=ans
		w_warningtext_ids.answer = strtrim(strupcase(ans(0)),2)
		w_warningtext_quest
		END
        "WARNINGTEXT_Y" : BEGIN
                WIDGET_CONTROL,w_warningtext_ids.text,SET_VALUE='Y'
                END
        "WARNINGTEXT_N" : BEGIN
                WIDGET_CONTROL,w_warningtext_ids.text,SET_VALUE='N'
                END
        "WARNINGTEXT_OK" : BEGIN
		w_warningtext_quest
		END
        "WARNINGTEXT_CLOSE" : BEGIN
                WIDGET_CONTROL,event.top,BAD_ID=bad,/DESTROY
                END
ENDCASE
END


PRO w_warningtext, str,width,height,heading,title=title,quest=quest,xloc=xloc,yloc=yloc, GROUP = GROUP
COMMON w_warningtext_block,w_warningtext_ids

if XRegistered('w_warningtext') ne 0 then begin
	WIDGET_CONTROL,w_warningtext_ids.base,/DESTROY
	end
wtitle = 'scanSee Messages'
dtitle = ''
if n_elements(width) eq 0 then width = 80
if n_elements(height) eq 0 then height = 5 
if n_elements(heading) gt 0 then dtitle=string(heading)
if n_elements(title) gt 0 then wtitle=string(title)

w_warningtext_ids = { $
	base : 0L, $
	text : 0L, $
	quest : '', $
	answer : 'Y' $
	}

w_warningtext_base=WIDGET_BASE(TITLE = wtitle, $
	TLB_FRAME_ATTR = 2, $
	/COLUMN)
w_warningtext_ids.base = w_warningtext_base
w_warningtext_title = WIDGET_LABEL(w_warningtext_base,VALUE=dtitle)

list = WIDGET_TEXT(w_warningtext_base,VALUE=str,UVALUE='LIST', $
	XSIZE =width, $
	YSIZE=height,/SCROLL)

if n_elements(quest) ne 0 then begin
w_warningtext_ids.quest = string(quest)
w_warningtext_row =WIDGET_BASE(w_warningtext_base, /ROW, /FRAME)
w_warningtext_lab = WIDGET_LABEL(w_warningtext_row,VALUE=string(quest)+' (Y/N) ?')
w_warningtext_text = WIDGET_TEXT(w_warningtext_row,VALUE='Y', $
	EDITABLE=1, UVALUE='WARNINGTEXT_GET', XSIZE=2)
w_warningtext_ids.text = w_warningtext_text 

w_warningtext_y = WIDGET_BUTTON(w_warningtext_row,VALUE='Y', $
	UVALUE='WARNINGTEXT_Y')
w_warningtext_n = WIDGET_BUTTON(w_warningtext_row,VALUE='N', $
	UVALUE='WARNINGTEXT_N')

w_warningtext_actrow =WIDGET_BASE(w_warningtext_base, /ROW)
w_warningtext_ok = WIDGET_BUTTON(w_warningtext_actrow,VALUE=' Accept ', $
	UVALUE='WARNINGTEXT_OK')
close = WIDGET_BUTTON(w_warningtext_actrow, $
                        VALUE = ' Cancel ', $
                        UVALUE = 'WARNINGTEXT_CLOSE')

endif else begin
close = WIDGET_BUTTON(w_warningtext_base, $
                        VALUE = ' Close ', $
                        UVALUE = 'WARNINGTEXT_CLOSE')
end

if keyword_set(xloc) then begin
	if n_elements(yloc) eq 0 then yloc = 300
	WIDGET_CONTROL, w_warningtext_base,/REALIZE, $
	TLB_SET_XOFFSET= xloc, TLB_SET_YOFFSET= yloc 
endif else $
	WIDGET_CONTROL, w_warningtext_base,/REALIZE


XMANAGER,'w_warningtext',w_warningtext_base, GROUP_LEADER = GROUP,/NO_BLOCK


END

;
; this routine does an auto-scaled plot of the selected waveforms
;
PRO UPDATE_PLOT, auto, st

   COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON w_statistic_block,w_statistic_ids
COMMON CATCH1D_2D_COM, data_2d, gD

    WIDGET_CONTROL, widget_ids.wf_select, GET_VALUE = wf_sel
    if scanData.svers then scanData.wf_sel = wf_sel(15:88) else $
    scanData.wf_sel = wf_sel

;   if total(scanData.wf_sel) eq 0 then return

   x_axis = w_plotspec_id.x_axis_u

win_state = WIDGET_INFO(widget_ids.plot_wid, /GEOMETRY)

   plotSubTitle = ''
   plotTitle=''
   plotYTitle=''
   plotXTitle =''

   ;Now draw the axis and plot the selected waveforms
   num_pts = 1 > (scanData.act_npts-1)

   ;extract valid data from global arrays

   catch1d_check_xaxis,num_pts,p1,xmin,xmax

; check any data available 
   if scanData.lastPlot lt 0 then begin 
	auto = 1
	end
 
if w_plotspec_id.log eq 0 and auto ne 0 then auto=1   
if w_plotspec_id.log eq 2 and auto ne 0 then auto=2   ;  Y > 0

;y_descs = strtrim(y_descs,2)

	; check for read or scan case
	id_def = realtime_id.def(*)
        if scanData.y_scan eq 0 and w_plotspec_id.scan eq 0 then begin
                dim =*(*gD).dim
		pv = *(*gD).pv
		scanH = 0
		if strpos(pv(0),'scanH') ge 0 then  scanH = 1
		if dim ge 2 then begin
		id_def = (*(*gD).id_def)(*,dim-2)
		if dim eq 2 then begin
		if scanH and scanData.scanH eq 0 then $
                id_def = (*(*gD).id_def)(*,dim-1) 
		end
		endif else id_def = (*(*gD).id_def)(*,0)
	if scanH then w_plotspec_array(0)=pv(1) 
	if dim eq 2 and scanH and scanData.scanH then w_plotspec_array(0)=pv(0) 
        end
	
     if n_elements(w_plotspec_array) ne 0 then begin 
	if strlen(strtrim(w_plotspec_array(0))) gt 1 then $
	plotTitle = strtrim( w_plotspec_array(0))
	if strlen(w_plotspec_array(2)) gt 1 then $
	plotYTitle = strtrim(w_plotspec_array(2))
	end

   scanData.lastPlot = auto
   y_zero = 0
   if auto eq 2 then y_zero = 1.e-7    ; exclude zero for auto scale

npd = scanData.nd
if scanData.svers then npd = 70
sz = size(*scanData.da)
nd = sz(2)
if sz(0) eq 1 then nd=1

   IF (auto gt 0) THEN BEGIN     ;  auto scale
!y.style = 1
!x.style = 1

     ;if autoscale, determine appropriate Y values
     ;depending on which waveforms will be plotted
     pos_ymin = 1.e20
     ymin = 1.e20
     ymax = -1.e20
     err_dy = 0

for i=0,nd - 1 do begin

     IF (scanData.wf_sel(i) EQ 1) THEN  BEGIN

	d1 = (*scanData.da)[0:num_pts,i]

         IF (MIN(d1) LT ymin) THEN ymin = MIN(d1)
         IF (MAX(d1) GT ymax) THEN ymax = MAX(d1)
	 if w_plotspec_id.log eq 1 and ymin le 0. then begin
		for j=0,num_pts-1 do begin
		  if d1(j) gt 0. and d1(j) lt pos_ymin then pos_ymin=d1(j)
		end
	 endif else pos_ymin = ymin
	if sqrt(abs(ymax)) gt err_dy then err_dy = sqrt(abs(ymax))
	if sqrt(abs(ymin)) gt err_dy then err_dy = sqrt(abs(ymin))
     END

end

; add the support postioner as Y

for i=npd,npd-1+4 do begin
     IF (scanData.wf_sel(i) EQ 1) THEN  BEGIN
	d1 = (*scanData.pa)[0:num_pts,i-npd]
         IF (MIN(d1) LT ymin) THEN ymin = MIN(d1)
         IF (MAX(d1) GT ymax) THEN ymax = MAX(d1)
	 if w_plotspec_id.log eq 1 and ymin le 0. then begin
		for j=0,num_pts-1 do begin
		  if d1(j) gt 0. and d1(j) lt pos_ymin then pos_ymin=d1(j)
		end
	 endif else pos_ymin = ymin
	if sqrt(abs(ymax)) gt err_dy then err_dy = sqrt(abs(ymax))
	if sqrt(abs(ymin)) gt err_dy then err_dy = sqrt(abs(ymin))
     END
end

; if error bar is on ajust ymin,ymax accordingly

	if w_plotspec_id.errbars  eq 1 then begin
		ymax = ymax + err_dy
		ymin = ymin - err_dy
		end

;
;  increase the xmin,xmax by +5%
;
	if auto gt 0 then view1d_adjust_ranges,xmin,xmax

   ENDIF ELSE BEGIN
;
; user scale auto=0
;
   ; if not autoscale, get limits from entry widgets. 

!y.style = 1
!x.style = 1
xmin = w_plotspec_limits(0)
xmax = w_plotspec_limits(1)
ymin = w_plotspec_limits(2)
ymax = w_plotspec_limits(3)
pos_ymin = ymin
ENDELSE
     
     ;now determine xmin and xmax depending on x-axis selection
     IF (x_axis EQ 0) THEN BEGIN
       plotXTitle = strtrim(w_plotspec_array(1))
     ENDIF ELSE BEGIN
	if scanData.zoom eq 0 then begin
        xmin = 0
	xmax=num_pts
	if auto eq 1 then view1d_adjust_ranges,xmin,xmax
	end
       plotXTitle = 'Step #'        
     ENDELSE

     if total(scanData.wf_sel) eq 0 then  plotXTitle = 'Nothing Selected' 

if !d.n_colors le !d.table_size then begin
TVLCT,o_red,o_green,o_blue,/get
;restore,file='/usr/local/epics/extensions/idllib/catch1d.tbl'
restore,file=scanData.tblfile  
TVLCT,red,green,blue
endif else device,decomposed=1


if !d.name ne 'PS' then WSET, widget_ids.plot_area
;   ERASE

   ;fake out PLOT to plot an empty axis
   junk = ['5','6']

   ; If plotting before p1 was read ...
    IF (ymax LE ymin) AND  (MIN(p1) eq MAX(p1) ) THEN  BEGIN
	p1=indgen(num_pts+1)
	xmin=0
	xmax=num_pts
	if auto eq 1 then view1d_adjust_ranges,xmin,xmax
   ENDIF

   ;Plot the axis w/o any waveforms

	POS=[0.15,0.2,0.75,0.85] 
	xticklen = w_plotspec_id.xticklen
	yticklen = w_plotspec_id.yticklen
	gridstyle = w_plotspec_id.gridstyle

;if scanData.act_npts ge scanData.req_npts then begin
; 
; linear plot
;
if w_plotspec_id.log ne 1 then begin


; 10 % margin

	if auto gt 0 then begin
        dy = 0.1 *(ymax-ymin)
        if dy eq 0 then begin
                if ymax eq 0 then  dy = 10 else dy = 0.05 * ymax
                end
        ymax = ymax + dy
        ymin = ymin - dy
	end

; auto scale but only plot y> 0 case

	if auto gt 1 then ymin = 0.

   PLOT, XRANGE = [xmin,xmax],             $
         YRANGE = [ymin,ymax],             $
         XTITLE = plotXTitle,               $
         YTITLE = plotYTitle,               $
	YNOZERO = y_zero, $
	XTICKLEN = xticklen, $
	YTICKLEN = yticklen, $
	XGRIDSTYLE = gridstyle, YGRIDSTYLE= gridstyle, $
	XMINOR = 10, $
        YMINOR = 10, $
         TITLE = plotTitle,               $
         SUBTITLE = plotSubTitle,               $
	POS=pos, $
         MAX_VALUE = 0, junk
end

;
; log plot
;
if w_plotspec_id.log eq 1 then begin
if ymax le 0. then begin
	w_warningtext,'Data not suitable for YLOG plot.'
;	plotoptionsmenu_set_string,18,19
;	w_plotspec_id.log = 0
	return
	end

	yrange = [ymin,ymax]
	if ymin le 0. then begin
		ymin = pos_ymin
		yrange = [ymin,ymax*10]
		end
   PLOT, XRANGE = [xmin,xmax],             $
         YRANGE =  yrange,            $
         XTITLE = plotXTitle,               $
         YTITLE = plotYTitle,               $
	XTICKLEN = xticklen, $
	YTICKLEN = yticklen, $
	XGRIDSTYLE = gridstyle, YGRIDSTYLE= gridstyle, $
         TITLE = plotTitle,               $
         SUBTITLE = plotSubTitle,               $
	 XMINOR = 10, $
;	YMINOR=9,$
	YTYPE=1,$
	POS=pos, $
         MAX_VALUE = 0, junk
end

st='Scan #: ' + strtrim(scanData.scanno)

npd = scanData.nd
if scanData.svers then npd = 70

is = 0
for i=0,nd-1 do begin
   IF (scanData.wf_sel(i) EQ 1) THEN begin
	if w_plotspec_id.scan then ii=1 else ii=0
	d1 = (*scanData.da)[ii:num_pts,i]
if w_plotspec_id.statistic eq 3 then begin
	getStatisticDeviation_1d,i,d1,moments,sdev,mdev,st1
        st = [st, st1]
	statis_value = [sdev,mdev,moments(0),moments(1)]
endif else if w_plotspec_id.statistic gt 0 then begin
	getStatistic_1d,i,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,st1
        st = [st, st1]
	statis_value = [xpeak,c_mass,FWHM,ypeak]
end

     if n_elements(statis_value) gt 0 then $
       view1d_legends,pos,is,i,p1,d1,num_pts,x_axis,statis_value else $
       view1d_legends,pos,is,i,p1,d1,num_pts,x_axis
	is = is + 1
   END
end

for i=npd,npd+3 do begin
   IF (scanData.wf_sel(i) EQ 1 and id_def(i-npd) gt 0) THEN begin
        d1 = (*scanData.pa)[0:num_pts,i-npd]
if w_plotspec_id.statistic eq 3 then begin
        getStatisticDeviation_1d,i,d1,moments,sdev,mdev,st1
        st = [st, st1]
        statis_value = [sdev,mdev,moments(0),moments(1)]
endif else if w_plotspec_id.statistic gt 0 then begin
        getStatistic_1d,i,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,st1
        st = [st, st1]
        statis_value = [xpeak,c_mass,FWHM,ypeak]
end
       view1d_legends,pos,is,i,p1,d1,num_pts,x_axis,statis_value
        is = is + 1
        end
end

if auto eq 1 and n_elements(st) gt 0  and widget_ids.statistic gt 1 then begin
	WIDGET_CONTROL,widget_ids.statistic,SET_VALUE=st,BAD_ID=bad_id,/NO_COPY
	if bad_id ne 0 then widget_ids.statistic = 0L
        end


;
; plot scan number + filename
;
	filenamepath,scanData.trashcan,F,P

	header_note='data file: ' + F

	xdis = 0.01 * !d.x_size
	ydis = !d.y_size - 1.2 * !d.y_ch_size
	xyouts,xdis,ydis,header_note,/device

	if scanData.dim gt 1 then begin
	xdis = 0.45 * !d.x_size
	ydis = !d.y_size - 1.2 * !d.y_ch_size

	if scanData.dim eq 3 then $
	header_note = '3D SCAN # : '+string(scanData.scanno_2d) else $
	header_note = '2D SCAN # : '+string(scanData.scanno_2d) 
	xyouts,xdis,ydis,header_note,/device
	end

	if scanData.dim eq 3 then $
	header_note =  '2D SCAN # : ' + string(scanData.scanno) else $
	header_note =  '1D SCAN # : ' + string(scanData.scanno) 
	xdis = 0.75 * !d.x_size
	ydis = !d.y_size - 1.2 * !d.y_ch_size
	xyouts,xdis,ydis,header_note,/device

	footer_note = strtrim(w_plotspec_array(4),2)
	xdis = 0.01 * !d.x_size
	ydis = 1.2*!d.y_ch_size
	xyouts,xdis,ydis,footer_note,/device


	footer_note= 'comment: ' + strtrim(w_plotspec_array(5))
	view1d_ydist,(.01-pos(1)),ydis	
	xdis = 0.01 * !d.x_size
	ydis = 0.1*!d.y_ch_size
	xyouts,xdis,ydis,footer_note,/device

if !d.n_colors le !d.table_size then TVLCT,o_red,o_green,o_blue else $
device,decomposed=0
END


PRO catch1d_check_xaxis,num_pts,p1,xmin,xmax,scatter=scatter
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

; select the x-axis for plot

	i = w_plotspec_id.xcord
	if w_plotspec_id.scan then is=1 else is=0
	if w_plotspec_id.xcord lt 4 then $
	   p1 = (*scanData.pa)[is:num_pts,i] else $
	   p1 = (*scanData.da)[is:num_pts,i-4]
;	if keyword_set(scatter) then begin
	   xmin = MIN(p1)
	   xmax = MAX(p1)
;	endif else begin
;	   xmin = p1(0)
;	   xmax = p1(num_pts)
;	end
END


PRO view1d_xticks,xmin,xmax,XVAL

  XVAL = make_array(!X.TICKS+1,/float)
  DXVAL = (xmax - xmin)/ !X.TICKS 
  for i=0,!X.TICKS do begin
  XVAL(i) = xmin + i*DXVAL
  end
END
 
PRO view1d_yticks,ymin,ymax,YVAL
  YVAL = make_array(!Y.TICKS+1,/float)
  DYVAL = (ymax - ymin)/ !Y.TICKS 
  for i=0,!Y.TICKS do begin
  YVAL(i) = ymin + i*DYVAL
  end
END

PRO view1d_xdist,fact,xval
	dx = !x.window(1) - !x.window(0)
	if fact gt (1.-!x.window(0)) then begin
		print,'Error: ',-!x.window(0),' < fact < ',1 -!x.window(0)
		return
		end
	xval = !x.crange(0) + fact/dx * (!x.crange(1) - !x.crange(0)) 
END

PRO view1d_ydist,fact,yval
	dy = !y.window(1) - !y.window(0)
	if fact gt (1.-!y.window(0)) then begin
		print,'Error: ',-!y.window(0),' < fact < ',1 -!y.window(0)
		return
		end
	yval = !y.crange(0) + fact/dy * (!y.crange(1) - !y.crange(0)) 
END

PRO view1d_set_range,xmin,xmax,no
print,xmin,xmax,no
dx = (xmax-xmin)/no
xmin = xmin - 0.5 * dx
xmax = xmax + 0.5 * dx
i1 = fix(xmin/dx) - 1
i2 = fix(xmax/dx) + 1
xmin = i1*dx
xmax = i2*dx
print,xmin,xmax,no
END

PRO view1d_adjust_ranges,xmin,xmax
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
 
;
;  increase the xmin,xmax by +5%
;
        dx = 0.05 *(xmax-xmin)
	if xmin eq xmax then dx = 1. 
          xmax = xmax + dx
          xmin = xmin - dx

;	view1d_round_xrange,xmin,xmax

	w_plotspec_limits(0:1) = [xmin,xmax]
END

;
; round the xrange to integer if total width > 5
;
PRO view1d_round_xrange,xmin,xmax
if (xmax - xmin) le 5. then return 
xmax = floor(xmax) + 1
xmin = floor(xmin)

END

PRO view1d_power10_max,x,newx,no
newx = x
if x lt 2. then return
v = fix(x)
;if (x-v) gt 0 then v = v+1

in1 = v / 10 + 1
ir1 = v - in1 *10
p = 1
if ir1 eq 0 then begin
	newx = v
	return
	end

if abs(in1) lt 10 then begin
	newx = in1 * 10^p + (1+ir1) *10^(p-1)
	no = p
	return
	end

REP:
        in2 = in1 /10
	ir2 = in1 - in2 *10
	p = p + 1
	if abs(in2) lt 10 then begin
		newx = in2 * 10^p + (1+ir2) *10^(p-1)
		no = p
		return
		end
	in1 = in2
	ir1 = ir2	
	goto, REP

END

PRO view1d_legends,pos,id1,id,p1,d1,num_pts,x_axis,statis_value

   COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

	view1d_xdist,(0.01 + pos(2)-pos(0)),xdis	
	view1d_xdist,(0.075+pos(2)-pos(0)),xdis2	

	ino = 5*id1
	ch_ratio = float(!d.y_ch_size) / !d.y_size
	view1d_ydist,(pos(3)-pos(1)-5*id1*ch_ratio),lydis	

	color = w_plotspec_id.colorI(id)   ; use fix detector color
	if !d.n_colors gt !d.table_size then  color = w_plotspec_id.colorV(id)

	if w_plotspec_id.color eq 0 then begin
		color= !d.n_colors-1
		if !d.name eq 'PS' then color = 0
	end

	; 24 bit visual case
	thick = 2
	if !d.name eq 'PS' then thick=5

	line = id1
	if w_plotspec_id.solid eq 1 and w_plotspec_id.color eq 1 then line = 0

npd = scanData.nd
if scanData.svers then npd = 70
detname = scanData.DI
if scanData.svers then detname= detname(15:scanData.num_det-1)

if w_plotspec_id.type eq 0 then begin
	IF x_axis eq 1 then $ 
		OPLOT,d1,color=color,LINE=line,thick=thick else $
      		OPLOT,p1,d1,color=color,LINE=line,thick=thick
	; write legend 
	if w_plotspec_id.log ne 1 then $ 
	oplot,[xdis,xdis2],[lydis,lydis],color=color,thick=thick,LINE=line,/noclip else $
	oplot,[xdis,xdis2],[10^lydis,10^lydis],color=color,thick=thick,LINE=line,/noclip
	xdis = 0.8*!d.x_size
	xdis2 = 0.835*!d.x_size
	ydis = pos(3) * !d.y_size - 5 *id1*!d.y_ch_size
	if id lt npd then begin
	  xyouts,xdis2,ydis,detname(id)+': '+scanData.ydescs(id), /device
;	  xyouts,xdis2,ydis,detname(id)+': '+y_descs(id), /device
	end
	if id ge npd then begin
	   idd = id-npd
	   xdescs = scanData.xdescs
	   if strlen(descs(idd)) gt 1 then $
		xyouts,xdis2,ydis,'  '+xdescs(idd), /device else $
		xyouts,xdis2,ydis,'  Encoder P'+strtrim(idd+1,2), /device
	end
endif else begin
	sym = id1+1
	if w_plotspec_id.type eq 2 then sym = -(id1+1)
		IF x_axis eq 1 then $
			OPLOT, d1,color=color, PSYM = sym else $
			OPLOT, p1, d1,color=color, PSYM = sym 

	if w_plotspec_id.log ne 1 then $ 
		oplot,[xdis,xdis],[lydis,lydis],color=color,thick=thick,PSYM=sym,/noclip else $
		oplot,[xdis,xdis],[10^lydis,10^lydis],color=color,thick=thick,PSYM=sym,/noclip
	; write legend
	xdis = 0.8*!d.x_size
	xdis2 = 0.835*!d.x_size
	ydis = pos(3) * !d.y_size - 5 *id1*!d.y_ch_size
	if id lt npd then begin
	  xyouts,xdis2,ydis,detname(id)+': '+scanData.ydescs(id), /device
;	  xyouts,xdis2,ydis,detname(id)+': '+y_descs(id), /device
	end
	if id ge npd then begin
	   idd = id-npd
	   xdescs = scanData.xdescs
	   if strlen(xdescs(idd)) gt 1 then $
		xyouts,xdis2,ydis,'  '+xdescs(idd), /device else $
		xyouts,xdis2,ydis,'  Encoder '+detname(id,1), /device
	end
end

if w_plotspec_id.errbars eq 1 then begin 
	d_err = sqrt(abs(d1))
	for i=0, num_pts do begin
	x2 = p1(i)
	ny1 = d1(i) - d_err(i)
	ny2 = d1(i) + d_err(i)
	IF x_axis eq 1 then $
		OPLOT,color=color,[i,i], [ny1,ny2]  else $
		OPLOT,color=color, [x2,x2],[ny1,ny2]
	end
end


if w_plotspec_id.statistic gt 0 then begin

	xpeak = statis_value(0)
	c_mass = statis_value(1)
	FWHM = statis_value(2)
	peak = statis_value(3)

desc_legend = make_array(4,/string)
if w_plotspec_id.statistic eq 3 then begin
	desc_legend(0) = 'Std Dev '
	desc_legend(1) = 'Ave Dev '
	desc_legend(2) = '  Mean  '
	desc_legend(3) = '  Vari  '
endif else begin
	desc_legend(0) = '  Peak @'
	desc_legend(1) = '  Cntr @'
	desc_legend(2) = '  FWHM '
	desc_legend(3) = '  Peak '
end

if n_elements(xpeak) gt 0 then begin
	ydis = pos(3) * !d.y_size - (ino+1)*!d.y_ch_size
	xyouts,xdis,ydis,desc_legend(0)+strtrim(xpeak,1),/device
	end

if n_elements(c_mass) gt 0 then begin
	ydis = pos(3) * !d.y_size - (ino+2)*!d.y_ch_size
	xyouts,xdis,ydis,desc_legend(1)+strtrim(c_mass,1) ,/device 
	end

if n_elements(FWHM) gt 0 then begin
	ydis = pos(3) * !d.y_size - (ino+3)*!d.y_ch_size
	xyouts,xdis,ydis,desc_legend(2)+strtrim(FWHM,1) ,/device
	end

if n_elements(peak) gt 0 then begin
	ydis = pos(3) * !d.y_size - (ino+4)*!d.y_ch_size
	xyouts,xdis,ydis,desc_legend(3)+strtrim(peak,1) ,/device
	end
end

END



PRO w_statistic_event,event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_statistic_block,w_statistic_ids
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

WIDGET_CONTROL, event.id, GET_UVALUE = eventval
CASE eventval OF
        "STATISTIC_CLOSE" : BEGIN
                WIDGET_CONTROL,event.top,/DESTROY
		widget_ids.statistic = 0L
		w_plotspec_id.statistic = 0
                END
ENDCASE
END


PRO w_statistic, str,width,height,title,quest=quest, GROUP = GROUP
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_statistic_block,w_statistic_ids

if n_elements(str) eq 0 then return
if XRegistered('w_statistic') ne 0 then begin
	WIDGET_CONTROL,w_statistic_ids.base,/DESTROY
	end
if n_elements(width) eq 0 then width = 80
if n_elements(height) eq 0 then height = 5 
if n_elements(title) eq 0 then title=''

w_statistic_base=WIDGET_BASE(TITLE = 'scanSee '+ title, $
	TLB_FRAME_ATTR = 2, $
	 /COLUMN)
w_statistic_title = WIDGET_LABEL(w_statistic_base,VALUE=title)

list = WIDGET_TEXT(w_statistic_base,VALUE=str,UVALUE='LIST', $
	XSIZE =width, $
	YSIZE=height,/SCROLL)

close = WIDGET_BUTTON(w_statistic_base, $
                        VALUE = ' Close ', $
                        UVALUE = 'STATISTIC_CLOSE')

WIDGET_CONTROL, w_statistic_base,/REALIZE, $
	 TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 400

widget_ids.statistic = list 
XMANAGER, 'w_statistic',w_statistic_base, GROUP_LEADER = GROUP

w_statistic_ids = { base : w_statistic_base }

END
@fit_statistic.pro

PRO  getStatisticDeviation_1d,id1,y,mean,sdev,mdev,st
	mean=0.
	sdev=0.
	mdev=0.
	no = n_elements(y)
	if no eq 0 then return 
	mean = total(y)/no
	if no eq 1 then return
	index = where(y gt mean, count)      ; check for constant function 
	mean = [mean,0.,0.,0.]
	if count gt 0 then mean = MOMENT(y,mdev=mdev,sdev=sdev)

st = [' Detector '+strtrim(id1+1,1)]
st= [st+' ']
        st = [st, '   Mean         = '+string(mean(0))]
        st = [st, '   Standard Dev = '+string(sdev)]
        st = [st, '   Mean Abs Dev = '+string(mdev)]
        st = [st, '   Variance     = '+string(mean(1))]
        st = [st, '   Skewness     = '+string(mean(2))]
        st = [st, '   Kurtosis     = '+string(mean(3))]
END

PRO  getStatistic_1d,id1,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,st

; call statistic_1d

        statistic_1d,p1,d1,c_mass,x_peak,y_peak,y_hpeak,FWHM

st = [' Detector '+strtrim(id1+1,1)]
st= [st+' ']
        st = [st, '   Peak  X='+strtrim(x_peak,1)+'  Y='+strtrim(y_peak,1)]
;       st = [st, '   H-Peak  Y='+strtrim(y_hpeak)]
        st = [st, '   Centroid  '+ strtrim(c_mass,1)]
        st = [st, '   FWHM      '+strtrim(FWHM,1)]

if n_elements(x_peak) gt 0 then begin
	largest = max(y_peak)
	i_largest = 0
	for i=0,n_elements(x_peak)-1 do begin
		if y_peak(i) ge largest then begin 
		i_largest = i
		goto, write_peak
		end
		end
	write_peak:
	xpeak = x_peak(i_largest)
	ypeak = y_peak(i_largest)
	end

END



PRO zoom_checkAxis,Event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved


	if Event.Index eq 0 and w_plotspec_id.x_axis_u eq 0 then begin
		xmin = w_plotspec_limits(0)
		xmax = w_plotspec_limits(1)
		p1 = (*scanData.pa)(0:scanData.act_npts-1,w_plotspec_id.xcord) 
		dl = (xmax-xmin)/1.1
		f = (p1(scanData.act_npts-1)-p1(0))/dl
		if f lt 0 then id = where(p1 le xmin and p1 ge xmax) else $
		id = where(p1 le xmax and p1 ge xmin)
		if id(0) ne -1 then begin
		nid = n_elements(id)
		if id(0) gt 0 then w_plotspec_limits(0) = id(0)-1 else $
			w_plotspec_limits(0) = id(0)
		w_plotspec_limits(1) = id(nid-1) 
		endif else begin
			w_plotspec_limits(0) = 0
			w_plotspec_limits(1) = scanData.act_npts-1
		end
		dl = (w_plotspec_limits(1) - w_plotspec_limits(0)) / f
		dl = abs(.05*dl)
		w_plotspec_limits(0) = w_plotspec_limits(0)-dl
		w_plotspec_limits(1) = w_plotspec_limits(1)+dl
		w_plotspec_id.x_axis_u = 1
	end
	if Event.Index gt 0 then begin
	  if w_plotspec_id.x_axis_u eq 1 then begin
	  	w_plotspec_id.x_axis_u = 0
		w_plotspec_id.xcord = Event.Index - 1
		xmin = w_plotspec_limits(0)
		xmax = w_plotspec_limits(1)
		p1 = (*scanData.pa)(0:scanData.act_npts-1,w_plotspec_id.xcord) 
		il = fix(xmin)
		ir = fix(xmax)
		if xmin lt 0 then il = 0 
		if xmax gt scanData.act_npts or xmax lt 0 then ir = scanData.act_npts-1
		f = 1.1*(ir-il)/(xmax-xmin)
		dl = (p1(ir)-p1(il))/f/2
		w_plotspec_limits(0) = p1(il) - dl
		w_plotspec_limits(1) = p1(ir) + dl
	  endif else begin
	  	w_plotspec_id.xcord = Event.Index - 1
	  	w_plotspec_id.x_axis_u = 0
	  end
	end
END


PRO zoom_to_box
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_warningtext_block,w_warningtext_ids

tx = ['Mouse buttons :', $
	'    Left :   drag box ', $
	'    Middle:  resize box ', $
	'    Right:   zoom to box']
w_warningtext,tx,40,5,'Zoom to box'

	win_state = WIDGET_INFO(widget_ids.base,/GEOMETRY)
	if win_state.xoffset gt 300 then $
	WIDGET_CONTROL,w_warningtext_ids.base, $
		TLB_SET_XOFFSET=10, $
		TLB_SET_YOFFSET=300 $
	else $
	WIDGET_CONTROL,w_warningtext_ids.base, $
		TLB_SET_XOFFSET=10+win_state.xoffset+win_state.xsize, $
		TLB_SET_YOFFSET=300 


IF scanData.lastPlot eq -1 then return 
;        WIDGET_CONTROL, widget_ids.plot_area, SENSITIVE = 0

WSET, widget_ids.plot_area

        MY_BOX_CURSOR,x,y,xs,ys
        WIDGET_CONTROL,widget_ids.plot_wid,/CLEAR_EVENTS 
        WIDGET_CONTROL,w_warningtext_ids.base,/DESTROY
d=convert_coord([x,x+xs],[y,y+ys],/DEVICE,/TO_DATA)
w_plotspec_limits(0) = d(0,0)
w_plotspec_limits(1) = d(0,1)
w_plotspec_limits(2) = d(1,0)
w_plotspec_limits(3) = d(1,1)
        WAIT, .2
        WIDGET_CONTROL,widget_ids.plot_area , SENSITIVE = 1
        UPDATE_PLOT, 0

END

PRO zoom_box,x1,y1,x2,y2
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

IF scanData.lastPlot eq -1 then return 

WSET, widget_ids.plot_area

w_plotspec_limits(0) = x1
w_plotspec_limits(1) = x2
w_plotspec_limits(2) = y1
w_plotspec_limits(3) = y2
        WAIT, .2
;        WIDGET_CONTROL,widget_ids.plot_area , SENSITIVE = 1
        UPDATE_PLOT, 0

END


PRO zoom_in_out
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_warningtext_block,w_warningtext_ids
tx = ['Mouse buttons :', $
	'    Left :   zoom in ', $
	'    Middle:  zoom out ', $
	'    Right:   quit zoom in/out mode']
w_warningtext,tx,40,5,'Zoom In/Out'

	win_state = WIDGET_INFO(widget_ids.base,/GEOMETRY)
	if win_state.xoffset gt 300 then $
	WIDGET_CONTROL,w_warningtext_ids.base, $
		TLB_SET_XOFFSET=10, $
		TLB_SET_YOFFSET=300 $
	else $
	WIDGET_CONTROL,w_warningtext_ids.base, $
		TLB_SET_XOFFSET=10+win_state.xoffset+win_state.xsize, $
		TLB_SET_YOFFSET=300 

WSET, widget_ids.plot_area

WHILE 1 do begin
;cursor,x,y,1,/normal
cursor,x,y,0,/normal

if !err eq 2 then begin            ; zoom out
x = (x - !x.window(0)) / (!x.window(1)-!x.window(0)) * $
        (!x.crange(1)-!x.crange(0)) + !x.crange(0)

y = (y - !y.window(0)) / (!y.window(1)-!y.window(0)) * $
        (!y.crange(1)-!y.crange(0)) + !y.crange(0)
;st = 'Cursor X Y value @ button 1 pressed: '+string(x) +string(y)
;print,st

dx = 1. * (!x.crange(1)-!x.crange(0))
dy = 1. * (!y.crange(1)-!y.crange(0))
x1 = x - dx
x2 = x + dx
y1 = y - dy
y2 = y + dy
zoom_box,x1,y1,x2,y2
end

if !err eq 1 then begin            ; zoom in 
x = (x - !x.window(0)) / (!x.window(1)-!x.window(0)) * $
        (!x.crange(1)-!x.crange(0)) + !x.crange(0)

y = (y - !y.window(0)) / (!y.window(1)-!y.window(0)) * $
        (!y.crange(1)-!y.crange(0)) + !y.crange(0)
;st = 'Cursor X Y value @ button 1 pressed: '+string(x) +string(y)
;print,st

dx = 0.25 * (!x.crange(1)-!x.crange(0))
dy = 0.25 * (!y.crange(1)-!y.crange(0))
x1 = x - dx 
x2 = x + dx 
y1 = y - dy 
y2 = y + dy 
;if x2 gt !x.crange(1) then x2 = !x.crange(1)
;if x1 lt !x.crange(0) then x1 = !x.crange(0)
;if y2 gt !y.crange(1) then y2 = !y.crange(1)
;if y1 lt !y.crange(0) then y1 = !y.crange(0)
zoom_box,x1,y1,x2,y2
end

if !err eq 4 then begin 		; stop zoom in/out
	WIDGET_CONTROL,w_warningtext_ids.base,BAD=bad,/DESTROY
	WIDGET_CONTROL,widget_ids.plot_wid,/CLEAR_EVENTS
	return
	end

end
END

PRO zoom_out
COMMON w_warningtext_block,w_warningtext_ids

tx = ['ZOOM_OUT MODE', '    LMB stays in zoom out mode', $
	'    MMB refresh the drawing area', $
	'    RMB stops zoom out mode']
w_warningtext,tx,40,5
WHILE 1 do begin
;cursor,x,y,1,/normal
cursor,x,y,0,/normal
if !err eq 2 then begin
	UPDATE_PLOT,1
end
if !err eq 1 then begin
x = (x - !x.window(0)) / (!x.window(1)-!x.window(0)) * $
        (!x.crange(1)-!x.crange(0)) + !x.crange(0)

y = (y - !y.window(0)) / (!y.window(1)-!y.window(0)) * $
        (!y.crange(1)-!y.crange(0)) + !y.crange(0)
st = 'Cursor X Y value @ button 1 pressed: '+string(x) +string(y)
;print,st

dx = 1. * (!x.crange(1)-!x.crange(0))
dy = 1. * (!y.crange(1)-!y.crange(0))
x1 = x - dx 
x2 = x + dx 
y1 = y - dy 
y2 = y + dy 
zoom_box,x1,y1,x2,y2
end
if !err eq 4 then begin
	WIDGET_CONTROL,w_warningtext_ids.base,BAD=bad,/DESTROY
	return
	end
end
END

PRO draw_dragLine,clean=clean,x,y,slope
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_warningtext_block,w_warningtext_ids
COMMON w_statistic_block,w_statistic_ids

tx = ['Mouse buttons :', $
	'    Left :   pick start point ', $
	'    Middle:  pick end point ', $
	'    Right:   quit slope calc mode']
w_warningtext,tx,40,5,'Pick Slope Line'

	win_state = WIDGET_INFO(widget_ids.base,/GEOMETRY)
	if win_state.xoffset gt 300 then $
	WIDGET_CONTROL,w_warningtext_ids.base, $
		TLB_SET_XOFFSET=10,TLB_SET_YOFFSET=300 $
	else $
	WIDGET_CONTROL,w_warningtext_ids.base, $
		TLB_SET_XOFFSET=10+win_state.xoffset+win_state.xsize, $
		TLB_SET_YOFFSET=300


!Err = 0
if keyword_set(clean) then begin
        if n_params() lt 2 then begin
                print,'Usage: draw_dragLine,x,y,[/clean,slope]
                return
                end
        if n_elements(x) eq n_elements(y) and n_elements(x) gt 1 then $
        oplot,x,y,color=0
        return
        end

; need to be drawing area

WSET, widget_ids.plot_area

LOOP0:
cursor,x1,y1,/down
x2=x1 & y2=y1
LOOP:
while (!err ne 2) do begin
        oplot,[x1,x2],[y1,y2], color=0
        cursor,x2,y2,/nowait
        oplot,[x1,x2],[y1,y2], color = !d.n_colors - 2
wait,0.001
endwhile
	
	slope=0
	x=[x1,x2]
	y=[y1,y2]
	if  x(1) ne x(0) then slope = (y(1)-y(0)) /(x(1) -x(0))
	st = ''
	st = [st,'X1 = '+string(x1)]
	st = [st,'Y1 = '+string(y1)]
	st = [st,'X2 = '+string(x2)]
	st = [st,'Y2 = '+string(y2)]
	st = [st,'','Slope = '+string(slope)]	

	if !err eq 2 then begin 		; whether stop the mode 
	WIDGET_CONTROL,widget_ids.plot_wid,/CLEAR_EVENTS

	w_statistic,st,25,10,'Slope Calc'
	win_state = WIDGET_INFO(widget_ids.base,/GEOMETRY)
	if win_state.xoffset gt 300 then $
	WIDGET_CONTROL,w_statistic_ids.base, $
		TLB_SET_XOFFSET=10, $
		TLB_SET_YOFFSET=500 $
	else $
	WIDGET_CONTROL,w_statistic_ids.base, $
		TLB_SET_XOFFSET=10+win_state.xoffset+win_state.xsize, $
		TLB_SET_YOFFSET=500


	cursor,x1,y1,/down

	if !err eq 4 then begin
		oplot,x,y,color=0
		oplot,x,y
		WIDGET_CONTROL,w_warningtext_ids.base,BAD=bad,/DESTROY
		WIDGET_CONTROL,w_statistic_ids.base,BAD=bad,/DESTROY
		UPDATE_PLOT,scanData.lastPlot
		return
		end

	if !err eq 1 then begin
		oplot,x,y,color=0
		goto,LOOP
		end
	end

	if !err eq 2 then begin
		oplot,x,y,color=0
		goto,LOOP0
	end
END




PRO zoompan_Event, Event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_warningtext_block,w_warningtext_ids

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'ZOOM_PAN_SEL2': BEGIN
      CASE Event.Value OF
      0: begin
	dx = 0.05*(!x.crange(1)-!x.crange(0))
	x1 = !x.crange(0) - dx
	x2 = !x.crange(1) - dx
	y1 = !y.crange(0)
	y2 = !y.crange(1)
	zoom_box,x1,y1,x2,y2
	end
      1: begin
	dx = 0.05*(!x.crange(1)-!x.crange(0))
	x1 = !x.crange(0) + dx
	x2 = !x.crange(1) + dx
	y1 = !y.crange(0)
	y2 = !y.crange(1)
	zoom_box,x1,y1,x2,y2
	end
      2: begin
	dy = 0.05*(!y.crange(1)-!y.crange(0))
	y1 = !y.crange(0) - dy
	y2 = !y.crange(1) - dy
	x1 = !x.crange(0)
	x2 = !x.crange(1)
	zoom_box,x1,y1,x2,y2
	end
      3: begin
	dy = 0.05*(!y.crange(1)-!y.crange(0))
	y1 = !y.crange(0) + dy
	y2 = !y.crange(1) + dy
	x1 = !x.crange(0)
	x2 = !x.crange(1)
	zoom_box,x1,y1,x2,y2
	end
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  'ZOOM_PAN_SEL1': BEGIN
      CASE Event.Value OF
      0: begin
	x = 0.5*(!x.crange(1)+!x.crange(0))
	y = 0.5*(!y.crange(1)+!y.crange(0))

dx = 0.25 * (!x.crange(1)-!x.crange(0))
dy = 0.25 * (!y.crange(1)-!y.crange(0))
x1 = x - dx
x2 = x + dx
y1 = y - dy
y2 = y + dy
if x2 gt !x.crange(1) then x2 = !x.crange(1)
if x1 lt !x.crange(0) then x1 = !x.crange(0)
if y2 gt !y.crange(1) then y2 = !y.crange(1)
if y1 lt !y.crange(0) then y1 = !y.crange(0)
zoom_box,x1,y1,x2,y2
	end

      1: begin
	x = 0.5*(!x.crange(1)+!x.crange(0))
	y = 0.5*(!y.crange(1)+!y.crange(0))
dx = 1. * (!x.crange(1)-!x.crange(0))
dy = 1. * (!y.crange(1)-!y.crange(0))
x1 = x - dx
x2 = x + dx
y1 = y - dy
y2 = y + dy
zoom_box,x1,y1,x2,y2
	end
      2: begin
	scanData.lastPlot = 1
        UPDATE_PLOT, 1
	end
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  'ZOOM_PAN_CLOSE': BEGIN
	scanData.zoom = 0
	widget_control,Event.top,/destroy
      END
  ENDCASE
END




PRO zoompan_Event, Event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_warningtext_block,w_warningtext_ids

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'ZOOM_PAN_SEL2': BEGIN
      CASE Event.Value OF
      0: begin
	dx = 0.05*(!x.crange(1)-!x.crange(0))
	x1 = !x.crange(0) - dx
	x2 = !x.crange(1) - dx
	y1 = !y.crange(0)
	y2 = !y.crange(1)
	zoom_box,x1,y1,x2,y2
	end
      1: begin
	dx = 0.05*(!x.crange(1)-!x.crange(0))
	x1 = !x.crange(0) + dx
	x2 = !x.crange(1) + dx
	y1 = !y.crange(0)
	y2 = !y.crange(1)
	zoom_box,x1,y1,x2,y2
	end
      2: begin
	dy = 0.05*(!y.crange(1)-!y.crange(0))
	y1 = !y.crange(0) - dy
	y2 = !y.crange(1) - dy
	x1 = !x.crange(0)
	x2 = !x.crange(1)
	zoom_box,x1,y1,x2,y2
	end
      3: begin
	dy = 0.05*(!y.crange(1)-!y.crange(0))
	y1 = !y.crange(0) + dy
	y2 = !y.crange(1) + dy
	x1 = !x.crange(0)
	x2 = !x.crange(1)
	zoom_box,x1,y1,x2,y2
	end
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  'ZOOM_PAN_SEL1': BEGIN
      CASE Event.Value OF
      0: begin
	x = 0.5*(!x.crange(1)+!x.crange(0))
	y = 0.5*(!y.crange(1)+!y.crange(0))

dx = 0.25 * (!x.crange(1)-!x.crange(0))
dy = 0.25 * (!y.crange(1)-!y.crange(0))
x1 = x - dx
x2 = x + dx
y1 = y - dy
y2 = y + dy
if x2 gt !x.crange(1) then x2 = !x.crange(1)
if x1 lt !x.crange(0) then x1 = !x.crange(0)
if y2 gt !y.crange(1) then y2 = !y.crange(1)
if y1 lt !y.crange(0) then y1 = !y.crange(0)
zoom_box,x1,y1,x2,y2
	end

      1: begin
	x = 0.5*(!x.crange(1)+!x.crange(0))
	y = 0.5*(!y.crange(1)+!y.crange(0))
dx = 1. * (!x.crange(1)-!x.crange(0))
dy = 1. * (!y.crange(1)-!y.crange(0))
x1 = x - dx
x2 = x + dx
y1 = y - dy
y2 = y + dy
zoom_box,x1,y1,x2,y2
	end
      2: begin
	scanData.lastPlot = 1
	scanData.zoom = 0
        UPDATE_PLOT, 1
	scanData.zoom = 1
	end
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  'ZOOM_PAN_CLOSE': BEGIN
	scanData.zoom = 0
	UPDATE_PLOT,1
	widget_control,Event.top,/destroy
      END
  ENDCASE
END




PRO zoom_in_out1, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  zoompan = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      TITLE='ZOOM_PAN', $
      UVALUE='zoompan')

  BASE2 = WIDGET_BASE(zoompan, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  Btns320 = [ $
    'left ', $
    'right ', $
    'down', $
    'up ' ]
  BGROUP3 = CW_BGROUP( BASE2, Btns320, $
      ROW=1, $
      LABEL_LEFT='PAN: ', $
      UVALUE='ZOOM_PAN_SEL2')

  Btns409 = [ $
    '* 2', $
    '/ 2', $ 
    'Auto Scale' ]
  BGROUP4 = CW_BGROUP( BASE2, Btns409, $
      ROW=1, $
      LABEL_LEFT='ZOOM:', $
      UVALUE='ZOOM_PAN_SEL1')

  BUTTON5 = WIDGET_BUTTON( BASE2, $
      UVALUE='ZOOM_PAN_CLOSE', $
      VALUE='Close')


  WIDGET_CONTROL, zoompan, /REALIZE

  XMANAGER, 'zoompan', zoompan
END


PRO scan_field_set,pv,print=print
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
if n_params() eq 0 then begin
	w_warningtext,"usage:  scan_field_set,'scan_pvname',/print
	return
	end

scan_field_init,pv

s1 = n_elements(field_value)
;
;  Note the last .CPT field can not be set
; 
no = s1 - 1
if keyword_set(print) then begin
	for i=0,no-1 do print,field_name_array(i),'    ',field_value(i)
	end
ret = caputArray(field_name_array(0:no-1),field_value(0:no-1))
if ret ne 0 then w_warningtext,'scan_field_set failed in array put'
END

PRO scan_field_get,pv,print=print
 COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids

if n_params() eq 0 then begin
	w_warningtext,"usage:  scan_field_get,'scan_pvname',/print
	return
	end

scan_field_init,pv

s = size(field_name)

no = s(1)
field_value = make_array(no,/string,value=string(replicate(32b,40)))

	ln = cagetArray(field_name_array,y,/string)
	field_value = y
	y = 0

	if keyword_set(print) then begin
	for i=0, no-1  do print,field_name_array(i), '  ', field_value(i)
	end

END


PRO scan_field_init,pv,print=print
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids

if n_elements(pv) ne 0 then begin
	s = size(field_name)
	no = s(1)

field_name_array = make_array(no,/string,value=string(replicate(32b,30)))

	for i=0, no-1  do begin 
		 field_name_array(i)  = pv + field_name(i)
		end
	
	if keyword_set(print) then begin
	for i=0, no-1  do print,field_name_array(i)
	end
end
END


PRO w_scanfield_close
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
	if XRegistered('w_scanfield') ne 0 then $
	WIDGET_CONTROL, w_scanfield_ids.base, /DESTROY
END


PRO w_scanfield_event,event
WIDGET_CONTROL, event.id, GET_UVALUE = eventval
CASE eventval OF
	"SCANFIELD_OK" : BEGIN
		WIDGET_CONTROL,event.top,/DESTROY
		END
	ENDCASE
END

PRO w_scanfield, GROUP = GROUP
COMMON CATCH1D_COM, widget_ids, scanData
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids

if XRegistered('w_scanfield') ne 0 then return 

w_scanfield_base=WIDGET_BASE(TITLE = 'Scan Fields Widget', /COLUMN)
w_scanfield_title = WIDGET_LABEL(w_scanfield_base,VALUE='SCAN Record Set(Formated as 03/27/95)')

if scanData.pv ne '' then scan_field_init,scanData.pv

s1 = size(field_name_array)
s2 = size(field_value)
no = s1(1) - 1
str = make_array(s1(1),/string,value=string(replicate(32b,80)))
for i=0,no do begin
str(i) = field_name_array(i) + '        ' + field_value(i)
end

list = WIDGET_TEXT(w_scanfield_base,VALUE=str,UVALUE='LIST', $
	XSIZE =60, $
	YSIZE=20,/SCROLL)

close = WIDGET_BUTTON(w_scanfield_base, $
                        VALUE = ' Close ', $
                        UVALUE = 'SCANFIELD_OK')

w_scanfield_ids = { base: w_scanfield_base }

WIDGET_CONTROL, w_scanfield_base,/REALIZE

XMANAGER, 'w_scanfield',w_scanfield_base, GROUP_LEADER = GROUP

END


;
; catch1d_optionmenu.pro
;

PRO plotoptionsmenu_sensitive,i,on_off
COMMON PLOTMENU_OPTION_BLOCK,ids,r_names
        WIDGET_CONTROL,ids(i),SENSITIVE=on_off
END

PRO plotoptionsmenu_set_string,i,j,k,l,m
COMMON PLOTMENU_OPTION_BLOCK,ids,r_names
        WIDGET_CONTROL,ids(i),SET_VALUE=r_names(i)
        len = strlen(r_names(j))-1
        WIDGET_CONTROL,ids(j),SET_VALUE=' '+strmid(r_names(j),1,len)
if n_params() eq 2 then return
        len = strlen(r_names(k))-1
        WIDGET_CONTROL,ids(k),SET_VALUE=' '+strmid(r_names(k),1,len)
if n_params() eq 3 then return
        len = strlen(r_names(l))-1
        WIDGET_CONTROL,ids(l),SET_VALUE=' '+strmid(r_names(l),1,len)
if n_params() eq 4 then return
        len = strlen(r_names(m))-1
        WIDGET_CONTROL,ids(m),SET_VALUE=' '+strmid(r_names(m),1,len)
END




PRO plotoptionsmenu_Event, Event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON PLOTMENU_OPTION_BLOCK,ids,r_names

  WIDGET_CONTROL,EVENT.Id,GET_UVALUE=Ev

WSET,widget_ids.plot_area

if w_plotspec_id.scan eq 0 and realtime_id.ind eq -1 then $
   scanData.act_npts = scanData.readin_npts

  CASE Event.Value OF
; Color Curve 
    2: begin
	plotoptionsmenu_set_string,2,3
	w_plotspec_id.color = 1
	end
    3: begin
	plotoptionsmenu_set_string,3,2
	w_plotspec_id.color = 0
	end
; solid / dotted/ dashed
      5: begin
	plotoptionsmenu_set_string,5,6
	w_plotspec_id.solid = 0
	end
      6: begin
	plotoptionsmenu_set_string,6,5
	w_plotspec_id.solid = 1
	end
; plot style line,point,both
      8: begin
	plotoptionsmenu_set_string,8,9,10
	w_plotspec_id.type = 0
	end
      9: begin
	plotoptionsmenu_set_string,9,10,8
	w_plotspec_id.type = 1
	end
      10: begin
	plotoptionsmenu_set_string,10,8,9
	w_plotspec_id.type = 2
	end
; Grid off/on
     12: begin
	plotoptionsmenu_set_string,12,13
	w_plotspec_id.xticklen = 0.04
	w_plotspec_id.yticklen = 0.02
	w_plotspec_id.gridstyle= 0
	w_plotspec_id.grid = 0
	end
     13: begin
	plotoptionsmenu_set_string,13,12
	w_plotspec_id.xticklen = 0.5
	w_plotspec_id.yticklen = 0.5
	w_plotspec_id.gridstyle= 1
	w_plotspec_id.grid = 1
	end
; Errbar off/on
     15: begin
	plotoptionsmenu_set_string,15,16
	w_plotspec_id.errbars = 0
	end
     16: begin
	plotoptionsmenu_set_string,16,15
	w_plotspec_id.errbars = 1
	end
; Y scale linear, Y > 0, log
      18: begin
	plotoptionsmenu_set_string,18,19,20
	w_plotspec_id.log = 0
	end
     19: begin
	plotoptionsmenu_set_string,19,18,20
	w_plotspec_id.log = 2
	end
     20: begin
	plotoptionsmenu_set_string,20,18,19
	w_plotspec_id.log = 1
	end
; Plot ranges 
     21: begin
        user_scale, GROUP= event.top
        return
	end
; Plot labels 
     22: begin
 	if realtime_id.ind eq 1 then return
        w_plotspec, GROUP= event.top
        return
	end
  ELSE:
  ENDCASE

if realtime_id.ind eq 1 then begin
	realtime_id.axis = 1
endif else $
   UPDATE_PLOT,scanData.lastPlot

END

FUNCTION plotOptions,parent,UVALUE=uvalue
COMMON PLOTMENU_OPTION_BLOCK,ids,r_names
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

  junk   = { CW_PDMENU_S, flags:0, name:'' }

; replot menu
  MenuOptions = [ $
      { CW_PDMENU_S,       3, 'Plot Options' }, $ ;        0
        { CW_PDMENU_S,       1, 'Colors' }, $ ;        1
          { CW_PDMENU_S,       0, '* Colors' }, $ ;        2
          { CW_PDMENU_S,       2, '  Black&White' }, $ ;        3
        { CW_PDMENU_S,       1, 'Lines' }, $ ;        4
          { CW_PDMENU_S,       0, '  Solid/Dotted/etc ' }, $ ;        5
          { CW_PDMENU_S,       2, '* Solid Only' }, $ ;        6
        { CW_PDMENU_S,       1, 'Symbols' }, $ ;        7
          { CW_PDMENU_S,       0, '* Line Only' }, $ ;        8
          { CW_PDMENU_S,       0, '  Symbol Only' }, $ ;        9
          { CW_PDMENU_S,       2, '  Both' }, $ ;        10
        { CW_PDMENU_S,       1, 'Grid' }, $ ;        11
          { CW_PDMENU_S,       0, '* Off' }, $ ;       12
          { CW_PDMENU_S,       2, '  On' }, $ ;        13
        { CW_PDMENU_S,       1, 'Err Bars' }, $ ;        14
          { CW_PDMENU_S,       0, '* Off' }, $ ;        15
          { CW_PDMENU_S,       2, '  On' }, $ ;       16 
        { CW_PDMENU_S,       1, 'Y Scale' }, $ ;        17
          { CW_PDMENU_S,       0, '* Linear' }, $ ;       18 
          { CW_PDMENU_S,       0, '  Linear (Y>0)' }, $ ;       19 
          { CW_PDMENU_S,       2, '  Log' }, $ ;       20 
      { CW_PDMENU_S,       0, 'Ranges ...' }, $ ;        21
      { CW_PDMENU_S,       0, 'Labels ...' } $ ;       22 
  ]

ids = make_array(23,value=0L)
r_names = [ '', $
	'', '* Colors', '* Black&White', $
	'', '* Solid/Dotted/etc', '* Solid Only', $
	'', '* Line Only','* Symbol Only','* Both', $
	'', '* Off', '* On', $
	'', '* Off', '* On', $
	'', '* Linear',  '* Linear (Y>0)', '* Log', $
	'', '']


  PLOTOPTIONSMENU = CW_PDMENU( parent, MenuOptions, $
	IDS=ids, $
 	RETURN_ID = r_id, $
	RETURN_NAME = r_name, $
      UVALUE=uvalue)

	return, PLOTOPTIONSMENU
END

PRO setupoptionsmenu_sensitive,i,on_off
COMMON SETUPMENU_OPTION_BLOCK,ids,r_names
        WIDGET_CONTROL,ids(i),SENSITIVE=on_off
END

PRO setupoptionsmenu_set_string,i,j
COMMON SETUPMENU_OPTION_BLOCK,ids,r_names
        WIDGET_CONTROL,ids(i),SET_VALUE=r_names(i)
        len = strlen(r_names(j))-1
        WIDGET_CONTROL,ids(j),SET_VALUE=' '+strmid(r_names(j),1,len)
END

PRO setupOptionsMenu_event,Event
COMMON SETUPMENU_OPTION_BLOCK,ids,r_names
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

  WIDGET_CONTROL,EVENT.Id,GET_UVALUE=Ev

  CASE Event.Value OF
      2: begin
	WIDGET_CONTROL,ids(2),SET_VALUE=r_names(2)
	len = strlen(r_names(3))-1
	WIDGET_CONTROL,ids(3),SET_VALUE=' '+strmid(r_names(3),1,len)
	scanData.option = 0
        return
	end
      3: begin
	WIDGET_CONTROL,ids(3),SET_VALUE=r_names(3)
	len = strlen(r_names(2))-1
	WIDGET_CONTROL,ids(2),SET_VALUE=' '+strmid(r_names(2),1,len)
	scanData.option = 1
        return
	end
      5: begin
	WIDGET_CONTROL,ids(5),SET_VALUE=r_names(5)
	len = strlen(r_names(6))-1
	WIDGET_CONTROL,ids(6),SET_VALUE=' '+strmid(r_names(6),1,len)
        w_plotspec_id.autosave = 1
;        WIDGET_CONTROL,widget_ids.savelabel,SET_VALUE='  No Save'
        return
	end
      6: begin
	WIDGET_CONTROL,ids(6),SET_VALUE=r_names(6)
	len = strlen(r_names(5))-1
	WIDGET_CONTROL,ids(5),SET_VALUE=' '+strmid(r_names(5),1,len)
        w_plotspec_id.autosave = 0
;        WIDGET_CONTROL,widget_ids.savelabel,SET_VALUE='          '
	catch1d_check_seqno, scanData.trashcan
        return
	end
      8: begin
	WIDGET_CONTROL,ids(8),SET_VALUE=r_names(8)
	len = strlen(r_names(9))-1
	WIDGET_CONTROL,ids(9),SET_VALUE=' '+strmid(r_names(9),1,len)
        w_plotspec_id.realtime = 0
        if w_plotspec_id.scan then realtime_close
        return
	end
      9: begin
	WIDGET_CONTROL,ids(9),SET_VALUE=r_names(9)
	len = strlen(r_names(8))-1
	WIDGET_CONTROL,ids(8),SET_VALUE=' '+strmid(r_names(8),1,len)
        w_plotspec_id.realtime = 1
        if w_plotspec_id.scan then realtime_init
        return
	end
      11: begin
	WIDGET_CONTROL,ids(11),SET_VALUE=r_names(11)
	len = strlen(r_names(12))-1
	WIDGET_CONTROL,ids(12),SET_VALUE=' '+strmid(r_names(12),1,len)
        scanData.showlist = 0
                if widget_ids.terminal ne 0 then begin
                        WIDGET_CONTROL,widget_ids.TERMINAL,BAD_ID=bad
                        if bad eq 0 then $
                        WIDGET_CONTROL,widget_ids.TERMINAL,/DESTROY
                        widget_ids.terminal = 0L
                        end
        return
	end
      12: begin
	WIDGET_CONTROL,ids(12),SET_VALUE=r_names(12)
	len = strlen(r_names(11))-1
	WIDGET_CONTROL,ids(11),SET_VALUE=' '+strmid(r_names(11),1,len)
        scanData.showlist = 1
                widget_ids.terminal = CW_TERM(Event.top, $
                                        TITLE=scanData.pv, $
;                                        BGROUP_NAMES=names, $
;                                        BGEVENT_FUNCT='CWTERM_event', $
                                        /FRAME, $
                                        XSIZE=100, YSIZE=20, /SCROLL)
        return
	end
      14: begin
	WIDGET_CONTROL,ids(14),SET_VALUE=r_names(14)
	len = strlen(r_names(15))-1
	WIDGET_CONTROL,ids(15),SET_VALUE=' '+strmid(r_names(15),1,len)
	scanData.debug = 0
        return
	end
      15: begin
	WIDGET_CONTROL,ids(15),SET_VALUE=r_names(15)
	len = strlen(r_names(14))-1
	WIDGET_CONTROL,ids(14),SET_VALUE=' '+strmid(r_names(14),1,len)
	scanData.debug = 1
        return
	end
      16: begin
	catcher_setup,GROUP=event.top
	end
      17: begin
	found = findfile('pvtcolors.dat')
        if found(0) eq '' then begin
        str = 'Error: Private color table never been saved before'
        r = dialog_message(str,/error)
        endif else begin
        restore,'pvtcolors.dat'
        TVLCT,red,green,blue
        end
	xloadct, GROUP= event.top
	end
      18: begin
	add_caPendEvent,timer=0.1
	print,'add_caPendEvent,timer=0.1'
	end
   ELSE:
   ENDCASE

END



FUNCTION setupOptions,parent,UVALUE=uvalue
COMMON SETUPMENU_OPTION_BLOCK,ids,r_names
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  MenuSetup = [ $
      { CW_PDMENU_S,       3, 'Setup' }, $ ;        0
        { CW_PDMENU_S,       1, 'Acquisition' }, $ ;        1
          { CW_PDMENU_S,       0, '  Off' }, $ ;        2
          { CW_PDMENU_S,       2, '* On' }, $ ;        3
        { CW_PDMENU_S,       1, 'AutoSave' }, $ ;        1
          { CW_PDMENU_S,       0, '  Off' }, $ ;        2
          { CW_PDMENU_S,       2, '* On' }, $ ;        3
        { CW_PDMENU_S,       1, 'Realtime' }, $ ;        1
          { CW_PDMENU_S,       0, '  Off' }, $ ;        2
          { CW_PDMENU_S,       2, '* On' }, $ ;        3
        { CW_PDMENU_S,       1, 'TextWin' }, $ ;        1
          { CW_PDMENU_S,       0, '* Off' }, $ ;        2
          { CW_PDMENU_S,       2, '  On' }, $ ;        3
        { CW_PDMENU_S,       1, 'Debug' }, $ ;        1
          { CW_PDMENU_S,       0, '* Off' }, $ ;        2
          { CW_PDMENU_S,       2, '  On' }, $ ;        3
        { CW_PDMENU_S,       0, 'Scan ...' }, $ ;        2
        { CW_PDMENU_S,       0, 'Color ...' }, $ ;        2
        { CW_PDMENU_S,       0, 'CaPE Timer' } $ ;        2
  ]

;  PDMENU_setup = CW_PDMENU( BASE68, MenuSetup, /RETURN_FULL_NAME, $
;      UVALUE='SETUPMENU')

ids = make_array(18,value=0L)
r_names = ['', '', '* Off','* On', $
	'', '* Off', '* On', $
	'', '* Off', '* On', $
	'', '* Off', '* On', $
	'', '* Off', '* On', $
	'', $
	'']

  SETUPSMENU = CW_PDMENU( parent, MenuSetup, $
	IDS=ids, $
 	RETURN_ID = r_id, $
;	RETURN_NAME = r_name, $
      UVALUE=uvalue)


	return, SETUPSMENU
END

PRO user_scale_event,event
COMMON CATCH1D_COM, widget_ids, scanData
COMMON user_scale_block,user_scale_ids
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

WIDGET_CONTROL, event.id, GET_UVALUE = eventval
CASE eventval OF
	"USER_SCALE_SLDR1" : BEGIN
		WIDGET_CONTROL,user_scale_ids.slider1,GET_VALUE=s1
		val = 10.D^s1
		WIDGET_CONTROL,user_scale_ids.xmin, $
			SET_VALUE=strtrim(string(val,format='(g20.10)'),2)
		w_plotspec_limits(0) = val
		END
	"USER_SCALE_SLDR2" : BEGIN
		WIDGET_CONTROL,user_scale_ids.slider2,GET_VALUE=s1
		val = 10.D^s1
		WIDGET_CONTROL,user_scale_ids.xmax, $
			SET_VALUE=strtrim(string(val,format='(g20.10)'),2)
		w_plotspec_limits(1) = val
		END
	"USER_SCALE_SLDR3" : BEGIN
		WIDGET_CONTROL,user_scale_ids.slider3,GET_VALUE=s1
		val = 10.D^s1
		WIDGET_CONTROL,user_scale_ids.ymin, $
			SET_VALUE=strtrim(string(val,format='(g20.10)'),2)
		w_plotspec_limits(2) = val
		END
	"USER_SCALE_SLDR4" : BEGIN
		WIDGET_CONTROL,user_scale_ids.slider4,GET_VALUE=s1
		val = 10.D^s1
		WIDGET_CONTROL,user_scale_ids.ymax, $
			SET_VALUE=strtrim(string(val,format='(g20.10)'),2)
		w_plotspec_limits(3) = val
		END
        "USER_SCALE_XMIN" : BEGIN
		WIDGET_CONTROL,user_scale_ids.xmin,GET_VALUE=s1
		val = float(s1)
		w_plotspec_limits(0) = val 
;       	 	UPDATE_PLOT,scanData.lastPlot
		END
        "USER_SCALE_XMAX" : BEGIN
		WIDGET_CONTROL,user_scale_ids.xmax,GET_VALUE=s1
		val = float(s1)
		w_plotspec_limits(1) = val 
;       	 	UPDATE_PLOT,scanData.lastPlot
		END
        "USER_SCALE_YMIN" : BEGIN
		WIDGET_CONTROL,user_scale_ids.ymin,GET_VALUE=s1
		val = float(s1)
		w_plotspec_limits(2) = val 
;       	 	UPDATE_PLOT,scanData.lastPlot
		END
        "USER_SCALE_YMAX" : BEGIN
		WIDGET_CONTROL,user_scale_ids.ymax,GET_VALUE=s1
		val = float(s1)
		w_plotspec_limits(3) = val 
;       	 	UPDATE_PLOT,scanData.lastPlot
		END
        "USER_SCALE_REFRESH" : BEGIN
		scanData.lastPlot = 1
		if realtime_id.ind eq 1 then begin
			realtime_id.ymin =0.
			realtime_id.ymax =0.
			realtime_id.axis = 1 
		endif else begin
       		 	UPDATE_PLOT,1
		end
		END
        "USER_SCALE_OK" : BEGIN
        	WIDGET_CONTROL,user_scale_ids.xmin,GET_VALUE=temp
	        w_plotspec_limits(0) = float(strcompress(temp(0),/remove_all))
       		WIDGET_CONTROL,user_scale_ids.xmax,GET_VALUE=temp
       		w_plotspec_limits(1) = float(strcompress(temp(0),/remove_all))
       	 	WIDGET_CONTROL,user_scale_ids.ymin,GET_VALUE=temp
       	 	w_plotspec_limits(2) = float(strcompress(temp(0),/remove_all))
       	 	WIDGET_CONTROL,user_scale_ids.ymax,GET_VALUE=temp
       	 	w_plotspec_limits(3) = float(strcompress(temp(0),/remove_all))
		scanData.lastPlot = 0
		if realtime_id.ind eq 1 then begin
			realtime_id.axis = 1
		endif else begin
       		 	UPDATE_PLOT,0
;			scanData.lastPlot = 1
		end
		END
        "USER_SCALE_CLOSE" : BEGIN
                WIDGET_CONTROL,event.top,/DESTROY
                END
ENDCASE
END


PRO user_scale, GROUP = GROUP
COMMON user_scale_block,user_scale_ids
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved

if XRegistered('user_scale') ne 0 then begin
	WIDGET_CONTROL,user_scale_ids.base,/DESTROY
	end

user_scale_base=WIDGET_BASE(TITLE = 'Plot Ranges ... ', /COLUMN)
label0 = WIDGET_LABEL(user_scale_base,value='User Scale Plot Ranges')

row1 = WIDGET_BASE(user_scale_base, /ROW)
label1 = WIDGET_LABEL(row1,value='XMIN')
user_scale_xmin = WIDGET_TEXT(row1,VALUE=strtrim(w_plotspec_limits(0),2), $
	EDITABLE=1, UVALUE='USER_SCALE_XMIN', XSIZE=20)
slider1 = WIDGET_SLIDER(row1,MIN=-17, MAX=17, SUPPRESS_VALUE=0, $
	UVALUE='USER_SCALE_SLDR1', VALUE=0)

row2 = WIDGET_BASE(user_scale_base, /ROW)
label2 = WIDGET_LABEL(row2,value='XMAX')
user_scale_xmax = WIDGET_TEXT(row2,VALUE=strtrim(w_plotspec_limits(1),2), $
	EDITABLE=1, UVALUE='USER_SCALE_XMAX', XSIZE=20)
slider2 = WIDGET_SLIDER(row2,MIN=-17, MAX=17, SUPPRESS_VALUE=0, $
	UVALUE='USER_SCALE_SLDR2', VALUE=0)

row3 = WIDGET_BASE(user_scale_base, /ROW)
label3 = WIDGET_LABEL(row3,value='YMIN')
user_scale_ymin = WIDGET_TEXT(row3,VALUE=strtrim(w_plotspec_limits(2),2), $
	EDITABLE=1, UVALUE='USER_SCALE_YMIN', XSIZE=20)
slider3 = WIDGET_SLIDER(row3,MIN=-17, MAX=17, SUPPRESS_VALUE=0, $
	UVALUE='USER_SCALE_SLDR3', VALUE=0)

row4 = WIDGET_BASE(user_scale_base, /ROW)
label4 = WIDGET_LABEL(row4,value='YMAX')
user_scale_ymax = WIDGET_TEXT(row4,VALUE=strtrim(w_plotspec_limits(3),2), $
	EDITABLE=1, UVALUE='USER_SCALE_YMAX', XSIZE=20)
slider4 = WIDGET_SLIDER(row4,MIN=-17, MAX=17, SUPPRESS_VALUE=0, $
	UVALUE='USER_SCALE_SLDR4', VALUE=0)

row5 = WIDGET_BASE(user_scale_base, /ROW)
ok = WIDGET_BUTTON(row5, $
                        VALUE = ' User Scale ', $
                        UVALUE = 'USER_SCALE_OK')

refresh = WIDGET_BUTTON(row5, $
                        VALUE = ' Auto Scale ', $
                        UVALUE = 'USER_SCALE_REFRESH')

close = WIDGET_BUTTON(row5, $
                        VALUE = ' Done ', $
                        UVALUE = 'USER_SCALE_CLOSE')


user_scale_ids = { $
	base : user_scale_base, $
	xmin : user_scale_xmin, $
	xmax : user_scale_xmax, $
	ymin : user_scale_ymin, $
	ymax : user_scale_ymax, $
	slider1 : slider1, $
	slider2 : slider2, $
	slider3 : slider3, $
	slider4 : slider4 $
	}
	

WIDGET_CONTROL, user_scale_base,/REALIZE

XMANAGER, 'user_scale',user_scale_base, GROUP_LEADER = GROUP


END

PRO scanReadCheckFileType,File,suffix,ok
; wrong type ok=-1
; right type ok=1

        ok = -1
        if strpos(File,suffix) ne -1 then begin
                t = strmid(File,strlen(File)-strlen(suffix))
                if t eq suffix then ok = 1
        end
END

;
; figure out the ~ file name
; only work for unix system and HOME is defined
;
PRO filename_expand,F
if !d.name eq 'X' then begin
        h = getenv('HOME')
        u = strupcase(getenv('USER'))
        p0 = strpos(h,u,0)
        s0 = strmid(h,0,p0)
        sp = strpos(F,OS_SYSTEM.file_sep)
        len = strlen(F)-sp
        if STRMID(F,1,1) ne OS_SYSTEM.file_sep then begin
                s1 = strupcase(strmid(F,1,sp-1))
                F=s0+s1+strmid(F,sp,len)
        endif else F=h+strmid(F,sp,len)
end
END


PRO filenamepath,filename,F,P
COMMON CATCH1D_COM, widget_ids, scanData
if n_elements(filename) eq 0 then return
        len = strlen(filename)
        F=filename
        P=scanData.home
        if strpos(filename,!os.file_sep) eq -1 then return
 
        x=byte(filename)
        P=''
        for i=0,len-1 do begin
        is = len-1 -i
        if string(x(is)) eq !os.file_sep  then begin
                P = strmid(filename,0,is+1)
                F = strmid(filename,is+1,len-is)
                return
                end
        end
END

PRO write_config
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids

        CATCH,error_status

; demo mode error -128

        if error_status lt 0 then begin
	res = dialog_message([!error_state.name +  string(error_status), $
		!error_state.msg,'Failed to update the configuration file'],/Error)
	return    ;  exit
        end

openw,unit,scanData.config,/get_lun

printf,unit,"; Generated by ",+scanData.version+scanData.release
printf,unit,"scanData.pv='",scanData.pv,"'"
if strlen(scanData.y_pv) gt 1 then $
	 printf,unit,"scanData.y_pv='",scanData.y_pv,"'"
if strlen(scanData.filemax) gt 1 then $
	printf,unit,"scanData.filemax='",strtrim(scanData.filemax,2),"'"

; add  path 
;	st = "scanData.home='"+scanData.home+"'"
;        printf,unit,st

	x = scanData.path
	first = strpos(x,'/home')
	if first gt 0 then begin
		y = strmid(x,first,strlen(x))
		scanData.path = y
	end

	scanData.trashcan = scanData.path+w_plotspec_array(3)

	st = "scanData.path='"+scanData.path+"'"
        printf,unit,st
	st = "scanData.trashcan='"+w_plotspec_array(3)+"'"
        printf,unit,st
	st = "scanData.tblfile='"+ scanData.tblfile+"'"
        printf,unit,st
	st = "scanData.config='"+ scanData.config+"'"
        printf,unit,st
;	st = "scanData.fastscan='"+ strtrim(scanData.fastscan,2)+"'"
;        printf,unit,st
	st = "scanData.svers='"+ strtrim(scanData.svers,2)+"'"
        printf,unit,st
;	st = "w_plotspec_id.dtime='"+ strtrim(w_plotspec_id.dtime,2)+"'"
;        printf,unit,st
printf,unit,''
free_lun,unit

if scanData.debug eq 1 then $
print,'***File ',scanData.config,' saved.'

END

PRO read_config,filename
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved

file = scanData.config
if n_elements(filename) gt 0 then file = string(filename)

found = findfile(file)
if found(0) eq '' then return
openr,unit,file,/get_lun 

st=''
while(not eof(unit)) do begin
readf,unit,st
;print,st
	key_st=''
	new_st=''
	; get key variable name defined

	ln1 = strpos(st,"=")
	if ln1 gt -1 then begin
		key_st = strmid(st,0,ln1)

	; get string specification

	ln1 = strpos(st,"'")
	if ln1 gt -1 then begin
		new_st = strmid(st,ln1,strlen(st) - ln1)
		ln2 = strpos(new_st,"'",1)
		if ln2 eq -1 then $ 
		new_st = strmid(new_st,1,strlen(new_st)-1) $
		else if ln2 eq 1 then new_st = '' else $
		new_st = strmid(new_st,1,ln2-1)
	end

	CASE key_st OF 
	'scanData.pv' : scanData.pv = new_st
	'scanData.y_pv' : scanData.y_pv = new_st
	'scanData.filemax' : scanData.filemax = fix(new_st)
	'scanData.path' : scanData.path = new_st
	'scanData.trashcan' : w_plotspec_array(3) = new_st
	'scanData.tblfile' : scanData.tblfile = new_st
	'scanData.config' : scanData.config = new_st
	'scanData.option' : scanData.option = new_st
	'scanData.nosave' : scanData.nosave = new_st
	'scanData.debug' : scanData.debug = new_st
	'scanData.fastscan' : scanData.fastscan = fix(new_st)
	'scanData.svers' : scanData.svers = fix(new_st)
	'scanData.wtime' : scanData.wtime = float(new_st)
	'w_plotspec_id.dtime' : w_plotspec_id.dtime = float(new_st)
	ELSE :
	ENDCASE
	end

end
free_lun,unit
END



;
;
;  Sept 30, 2002  bkc fix scanData.path
;



PRO calc_newfilepath,pd,no,err=err
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
 
	prefix = str_sep(scanData.pv,':')
	scanData.prefix = prefix(0)

	nm = prefix[0]+ [':saveData_scanNumber', ':saveData_message', $
		':saveData_fileSystem',':saveData_subDir'] 
	callno = scanData.fileno 
	ln = cagetArray(nm,pd,/string)
	err = ln
	if ln ne 0 then return
	str = strsplit(pd(1),' ',/extract)
	pd(1) = str(n_elements(str)-1)

	no = fix(pd(0))
	scanData.fileno = no 
	if scanData.filemax lt no then scanData.filemax = no
	st = strtrim(no,2)

	; check for path change
	file_sep = !os.file_sep
	IF !d.name eq 'WIN' THEN BEGIN
		p1 = str_sep(pd(3),'/')
		if n_elements(p1) eq 1 then p1 = str_sep(pd(3),'\')
		sd = ''
		for i=0,n_elements(p1)-1 do begin
		sd = sd + p1(i)+file_sep
		end
		ps = strpos(scanData.path,sd)
		if ps lt 0 then begin
		tn = str_sep(scanData.path,file_sep)
		scanData.path = tn(0)+file_sep+sd
		end
	END
	IF !d.name eq 'X' THEN BEGIN
   		ps=0
		l2 = strlen(pd(2))
		if strmid(pd(2),l2-1,1) eq file_sep then pd(2)=STrmid(pd(2),0,l2-1)
		l3 = strlen(pd(3))
		if strmid(pd(3),l3-1,1) eq file_sep then pd(3)=STrmid(pd(3),0,l3-1)
		l3 = strlen(pd(3))
		if strmid(pd(3),0,1) eq file_sep then pd(3)=strmid(pd(3),1,l3-1)
		path = pd(2)+file_sep+pd(3)+file_sep
		ps = strpos(path,scanData.path)
		if ps lt 0 or strtrim(scanData.path,2) eq '' then begin 
                fs = strpos(pd(2),file_sep+file_sep,0)
                if fs ge 0 then begin
                tn = str_sep(path,file_sep)
                lp = strlen(tn(2))+3
                if lp gt 0 then begin
                        npath=strmid(path,lp-1,strlen(path)-lp+1)
			fd = findfile(npath,count=ct)
			if ct gt 0 then scanData.path=npath
                        end
                end
		end 
	END
;print,'new path=',scanData.path

END

PRO catcher_setup_Event, Event
COMMON CATCH1D_COM, widget_ids, scanData
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'SCAN1D_PVNAME': BEGIN
      WIDGET_CONTROL,catcher_setup_ids.pv,GET_VALUE=pv
	if strtrim(pv(0),2) eq '' then begin
		scanData.pv = ''
		scanData.pvconfig(0) = ''
		return
	end
	len = strpos(pv(0),'.')
	if len eq -1 then newpv = pv(0) else newpv = strmid(pv(0),0,len)
	if caSearch(newpv+'.EXSC') eq 0 then begin
      	WIDGET_CONTROL,catcher_setup_ids.pv,SET_VALUE=newpv
	scanData.pv = newpv
	scanData.pvconfig(0) = newpv
	pventry_event
	endif else begin
		w_warningtext,'Error: invalid SCAN 1D Pvname',40,2
	end
      END
  'SCAN2D_PVNAME': BEGIN
      WIDGET_CONTROL,catcher_setup_ids.y_pv,GET_VALUE=pv
	if strtrim(pv(0),2) eq '' then begin
		scanData.y_pv = ''
		scanData.pvconfig(1) = ''
		return
	end
	len = strpos(pv(0),'.')
	if len eq -1 then newpv = pv(0) else newpv = strmid(pv(0),0,len)
	if caSearch(newpv+'.EXSC') eq 0 then begin
        WIDGET_CONTROL,catcher_setup_ids.y_pv,SET_VALUE=newpv
	scanData.y_pv = newpv
	scanData.pvconfig(1) = newpv
	pventry2_event
	endif else begin
		w_warningtext,'Error: invalid SCAN 2D Pvname',40,2
	end
      END
  'CATCHER_SETUP_CANCEL': BEGIN
      WIDGET_CONTROL,catcher_setup_ids.base,/DESTROY,BAD=bad
      END
  'CATCHER_SETUP_DONE': BEGIN
      WIDGET_CONTROL,catcher_setup_ids.y_pv,GET_VALUE=pv1
	if strtrim(pv1(0),2) ne '' then begin
	len = strpos(pv1(0),'.')
	if len eq -1 then newpv = pv1(0) else newpv = strmid(pv1(0),0,len)
	if caSearch(newpv+'.EXSC') eq 0 then begin
        WIDGET_CONTROL,catcher_setup_ids.y_pv,SET_VALUE=newpv
	scanData.y_pv = newpv
	scanData.pvconfig(1) = newpv
	pventry2_event
	endif else begin
		w_warningtext,'Error: invalid SCAN 2D Pvname',40,2
	end
	endif else begin
		scanData.y_pv = ''
		scanData.pvconfig(1) = ''
	end

      WIDGET_CONTROL,catcher_setup_ids.pv,GET_VALUE=pv
	if strtrim(pv(0),2) ne '' then begin
	len = strpos(pv(0),'.')
	if len eq -1 then newpv = pv(0) else newpv = strmid(pv(0),0,len)
	if caSearch(newpv+'.EXSC') eq 0 then begin
      	WIDGET_CONTROL,catcher_setup_ids.pv,SET_VALUE=newpv
	scanData.pv = newpv
	scanData.pvconfig(0) = newpv
	pventry_event
	endif else begin
		w_warningtext,'Error: invalid SCAN 1D Pvname',40,2
	end
	endif else begin
		scanData.pv = ''
		scanData.pvconfig(0) = ''
	end
      WIDGET_CONTROL,catcher_setup_ids.base,/DESTROY,BAD=bad

	calc_newfilepath,err=err
	write_config
	add_caPendEvent,timer=0.1
	print,'add_caPendEvent,timer=0.1'
      END
  ENDCASE
END



; DO NOT REMOVE THIS COMMENT: END catcher_setup
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.

PRO catcher_setup_init
COMMON CATCH1D_COM, widget_ids, scanData
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

if n_elements(catcher_setup_scan) eq 0 then begin
	catcher_setup_scan = { $
		pv : '', $
		y_pv : '' $
		}
	end

	catcher_setup_scan.pv = scanData.pvconfig(0) ; scanData.pv
	catcher_setup_scan.y_pv = scanData.pvconfig(1) ; scanData.y_pv
END


PRO catcher_setup, GROUP=Group
COMMON CATCH1D_COM, widget_ids, scanData
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

IF XRegistered('catcher_setup') ne 0 then $
WIDGET_CONTROL,catcher_setup_ids.base,/DESTROY

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  catcher_setup_init

  catcher_setup_base = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, $
      TITLE='Scan ... ( PV SETUP )', $
      UVALUE='CATCHER_SETUP')

  BASE2 = WIDGET_BASE(catcher_setup_base, $
      ROW=1, $
      MAP=1, $
      TITLE='scan_1d', $
      UVALUE='BASE2')

  SCAN1D_PVNAME = CW_FIELD( BASE2,VALUE=catcher_setup_scan.pv, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='SCAN 1D Pvname:', $
      UVALUE='SCAN1D_PVNAME', $
      XSIZE=30)

  BASE3 = WIDGET_BASE(catcher_setup_base, $
      ROW=1, $
      MAP=1, $
      TITLE='scan_2d', $
      UVALUE='BASE3')

  SCAN2D_PVNAME = CW_FIELD( BASE3,VALUE=catcher_setup_scan.y_pv, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='SCAN 2D Pvname:', $
      UVALUE='SCAN2D_PVNAME', $
      XSIZE=30)

  BASE5 = WIDGET_BASE(catcher_setup_base, $
      ROW=1, $
      MAP=1, $
      TITLE='row4', $
      UVALUE='BASE5')

  CATCHER_SETUP_DONE = WIDGET_BUTTON( BASE5, $
      UVALUE='CATCHER_SETUP_DONE', $
      VALUE='Accept')

  CATCHER_SETUP_CANCEL = WIDGET_BUTTON( BASE5, $
      UVALUE='CATCHER_SETUP_CANCEL', $
      VALUE='Cancel')

catcher_setup_ids = { base : catcher_setup_base, $
	pv : SCAN1D_PVNAME, $
	y_pv : SCAN2D_PVNAME $
	}

  WIDGET_CONTROL, catcher_setup_base, /REALIZE

if XRegistered('w_viewscan') ne 0 then $ 
	WIDGET_CONTROL,catcher_setup_ids.base,SENSITIVE=0

  XMANAGER, 'catcher_setup', catcher_setup_base
;  XMANAGER, 'catcher_setup', catcher_setup_base,NO_BLOCK=0
END
;
; catch1d_realtime.pro
;

PRO  setScanPvnames,file=file,help=help
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

if keyword_set(help) then begin
	st = ["Usage: setScanPvnames,/file", $
	'       This function defines the pvnames monitored by the scan record',$
	'       If file keyword not used, then 4 positioners and 85 detectors assumed', $
	'       If file keyword used, the pvnames are read from the scan_pvnames file']
	w_warningtext,st
	return
	end

filename = scanData.pvfile
if keyword_set(file) then begin
	f = findfile(filename)
	if strlen(f(0)) gt 0 then begin
		readLabelsPvnames,filename,labels,pvnames
		scanData.nonames = n_elements(pvnames)
	       	 realtime_pvnames = make_array(scanData.nonames,/string,value=string(replicate(32b,30)))
		for i=0,scanData.nonames-1 do begin 
			n = strpos(pvnames(i),'.')
			len = strlen(pvnames(i)) - n
			if n ne -1 then realtime_pvnames(i)= scanData.pv+strmid(pvnames(i),n,len) $
				else realtime_pvnames(i)=pvnames(i)
			end
	endif else begin
	w_warningtext,'File '+filename+' not found!'
	return
	end

endif else begin

	scanData.nonames = scanData.num_det+4 ;89 
;	realtime_pvnames = make_array(scanData.nonames,/string,value=string(replicate(32b,5)))
	ncurve = n_elements(realtime_id.def)  ;89
	realtime_pvnames = make_array(ncurve,/string,value=string(replicate(32b,5)))

catch, error_status
if error_status ne 0 then return

	pvs = ['R1','R2','R3','R4',scanData.DI]
	realtime_pvnames = scanData.pv +'.' + pvs +'CV'

	if scanData.new then begin
	realtime_pvnames = scanData.pv +'.'+ [pvs(0:3),pvs(19:88)] + 'CV'
;	DC_pickDetVersion,/vers
	end
end

	scanData.npd = n_elements(realtime_pvnames)
	scanData.nd = scanData.npd-4
;print,'pvnames',realtime_pvnames
;print,"npd=",scanData.npd, "  nd=",scanData.nd
END


PRO terminal_dump_header
COMMON CATCH1D_COM, widget_ids, scanData
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
 
; showlist then dump to terminal window

if scanData.showlist eq 1 then begin
	WIDGET_CONTROL,widget_ids.terminal,BAD_ID=bad
	if bad ne 0 then $
		widget_ids.terminal = CW_TERM(widget_ids.base, $
                                        TITLE=scanData.pv, $
;                                        BGROUP_NAMES=names, $
;                                        BGEVENT_FUNCT='CWTERM_event', $
                                        /FRAME, $
                                        XSIZE=100, YSIZE=20, /SCROLL)
	st = "; VERSION: "+scanData.version+' '+scanData.release
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id
;	st = "; SCAN #: "+ string(w_plotspec_id.seqno+1)
	st = "; SCAN #: "+ string(scanData.scanno)
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id
	st = "; SCAN Record Name: "+scanData.pv
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id


	st =';'
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id

; find name, desc, engu for defined PiPV & DiPV 

	find_desc_engu,x_dn,descs,engus

	no = n_elements(x_dn)
	if scanData.new then no = scanData.npd

        st = ';    I   '
        for i=0,no-1 do begin
        if realtime_id.def(i) ne 0 then begin
                st = st+ ' '+x_dn(i)
                end
        end

;	print,st

	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id

;	s0 = string(replicate(32b,340))
twd = strlen(st) > (scanData.nd-1)*total(realtime_id.def) + 10
s0 = string(replicate(32b,twd))
	st = s0
	strput,st,';  (Desc)',0  &  ij = 17
	for i=0,no-1 do begin 
	if realtime_id.def(i) ne 0 then begin
		strput,st,descs(i),ij
		ij = ij + 18
		end
	end
;	print,st
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id

	st = s0
	strput,st,';  (Units)',0  &  ij = 17
	for i=0,no-1 do begin 
	if realtime_id.def(i) ne 0 then begin
		strput,st,engus(i),ij
		ij = ij + 18
		end
	end
;	print,st
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id

end
END

; calling procedure
;	realtime_init
;	realtime_read, npts
;
;
PRO realtime_init
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON field_block, field_name, field_name_array, field_value, w_scanfield_ids

	realtime_id.ind = 0
	realtime_id.no = 0
	realtime_id.axis = 0
	realtime_id.xmin = 0.
	realtime_id.xmax = 0.
	realtime_id.ymin = 0.
	realtime_id.ymax = 0.

if scanData.readpv then setScanPvnames,/file else setScanPvnames

if caSearch(scanData.pv) ne 0 then begin
	w_warningtext,['Error: scan record  '+ scanData.pv + '  not found']
	return
	end

	ln = cagetArray([scanData.pv+'.NPTS', scanData.pv+'.MPTS'],pd,/short)
	mpts = pd(0)
	scanData.req_npts = pd(0)
	nonames = scanData.npd
	realtime_retval = make_array(scanData.req_npts,nonames,/double)
	realtime_id.mpts = mpts

	list_pvnames = realtime_pvnames(0:nonames-1)
if scanData.realtime eq 0 then begin
	ln = caScan(scanData.pv+'.CPT',list_pvnames,/clear)
	ln = caScan(scanData.pv+'.CPT',list_pvnames,/add,max=mpts)
	ln = caScan(scanData.pv+'.CPT',list_pvnames,nonames,npts,pd,/get,max=mpts)
	realtime_retval = pd
	scanData.realtime = 1

if scanData.debug eq 1 then $
print,'REALTIME_INIT: add caScan at # ',w_plotspec_id.seqno

scanData.p_def = realtime_id.def(0:3)
*scanData.px = make_array(scanData.req_npts,/float)
*scanData.pa = make_array(scanData.req_npts,4,/double)
*scanData.da = make_array(scanData.req_npts,scanData.num_det,/float)
end

	ln = caScan(scanData.pv+'.CPT',list_pvnames,/zero,max=mpts)
	scanData.act_npts = 0

;  check for terminal dump

	terminal_dump_header


WSET, widget_ids.plot_area

;    ind = 0 plot the x axis and get monitor queue

if realtime_id.ind eq 0 then begin 
	if scanData.y_scan then tempTitle=strtrim(w_plotspec_array(0),2) else $
	tempTitle=strtrim(w_plotspec_array(0),2)+' (1D SCAN # '+strtrim(scanData.scanno,2) +')'

	xrange = [0,100]
	realtime_xrange,1,xmin,xmax
	xrange = [xmin,xmax]

	y_range=[w_plotspec_limits(2),w_plotspec_limits(3)]

; Y>0
	if w_plotspec_id.log eq 2 then y_range=[0.,w_plotspec_limits(3)] 
	if w_plotspec_id.log eq 1 then y_range=[0.1,w_plotspec_limits(3)] 

	plot,xrange=xrange, $
		yrange=y_range, $
		title=tempTitle, $
		xtitle= 'P1', $
		xticklen = w_plotspec_id.xticklen, $
		yticklen = w_plotspec_id.yticklen, $
		xgridstyle = w_plotspec_id.gridstyle, $
		ygridstyle = w_plotspec_id.gridstyle, $
		xminor= 10, $
		yminor= 10, $
		ytype = w_plotspec_id.log, $
		/nodata, /xstyle, /ystyle, $
		max_value=0,['1','1']

	realtime_id.ind = 1
	end

if scanData.y_pv ne '' then begin
pd=0
        pvs = scanData.y_pv + ['.NPTS','.CPT','.EXSC']
        pvs = [pvs , scanData.prefix+':ScanDim']
        ln = cagetArray(pvs,pd,/short)
        if ln eq 0 then scanData.y_req_npts = pd(0)
        if pd(2) eq 1 then begin
        scanData.y_seqno = pd(1)
        scanData.scanno = pd(1) + 1
        scanData.y_scan = 1
        endif else begin
        scanData.y_scan = 0
        end
        scanData.dim = pd(3)
end

x_dv = 0
x_dn = 0
realtime_retval = 0
;	print,caclock()
END

PRO realtime_close
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

    nonames = scanData.npd
    list_pvnames = realtime_pvnames(0:nonames-1)
    ln = caScan(scanData.pv+'.CPT',list_pvnames,/clear)

    w_plotspec_id.realtime = 0
    scanData.realtime = 0
    realtime_id.ind = 0

END

;
;
PRO realtime_read,npts
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

if n_params() eq 0 then begin
	st = ['usage:           realtime_init',$
	'                 realtime_read,  npts',$
	'        where',$
	'	                npts > 1']
	w_warningtext,st
	return
	end

;   ind = 2 scan finished already
if realtime_id.ind eq 2 then return 

symbol = w_plotspec_id.type 
if w_plotspec_id.type eq 2 then symbol = -1
ncurve = scanData.npd   ;n_elements(realtime_id.def)  ;89
ln_style = intarr(ncurve)
for i=0,ncurve-1 do begin
	ln_style(i) = i mod 6
	if w_plotspec_id.solid eq 1 then ln_style(i) = 0
end

retval = make_array(ncurve,scanData.req_npts+1);
nonames= scanData.npd
pts = scanData.req_npts+1

	list_pvnames = realtime_pvnames(0:nonames-1)

ln = caScan(scanData.pv+'.CPT',list_pvnames,nonames,pts,pd,/get,max=realtime_id.mpts)

cpts = pts
if cpts le 1 then return
retval = pd

if cpts le scanData.act_npts  then return

if !d.n_colors le !d.table_size then begin
TVLCT,o_red,o_green,o_blue,/get
restore,file=scanData.tblfile
;restore,file='/usr/local/epics/extensions/idllib/catch1d.tbl'
TVLCT,red,green,blue
endif else device,decomposed=1

n1 = realtime_id.no
n2 = cpts - 1  
scanData.act_npts = cpts 
if scanData.act_npts eq 0 then return

realtime_retval = transpose(retval)

	for i=0,scanData.num_pos-1 do begin
	if realtime_id.def(i) then begin
	(*scanData.pa)[n1:n2,i]	= realtime_retval(n1:n2,i)
	end
	if w_plotspec_id.xcord eq i then  $
		(*scanData.px)[n1:n2] = realtime_retval(n1:n2,i)
	end

	for i=0,scanData.nd-1 do begin
	if realtime_id.def(i+scanData.num_pos) then begin
	(*scanData.da)[n1:n2,i] = realtime_retval(n1:n2,i+scanData.num_pos)
	end
	end

;realtime_retval = 0 

; showlist then dump to terminal window

if scanData.showlist eq 1 then begin
	s0=string(replicate(32b,260))
	for i=n1+1,n2 do begin
	st = s0
	strput,st,i,0  &  ij = 10
	for j=0,scanData.num_pos - 1 do begin
	if realtime_id.def(j) ne 0 then begin 
		strput,st,(*scanData.pa)[i,j],ij  & ij = ij + 13 &end

		end
	for j=0,scanData.nd - 1 do begin
	if realtime_id.def(4+j) ne 0 then begin 
		strput,st,(*scanData.da)[i,j],ij  & ij = ij + 13 &end
		end
	;print,st
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id
	end
end

; detect any change of end point during the middle of scanning

xmin = realtime_id.xmin
xmax = realtime_id.xmax
x_dn = [scanData.pv+'.P1WD',scanData.pv+'.P2WD',scanData.pv+'.P3WD', $
	scanData.pv+'.P4WD', $
	scanData.pv+'.P1PP',scanData.pv+'.P2PP',scanData.pv+'.P3PP', $
	scanData.pv+'.P4PP']
ln = caMonitor(x_dn,ret,/check)
if total(ret) gt 0 then begin
	realtime_xrange,1,xmin,xmax
	realtime_id.axis = 1
	end

; if time axis plot is requested

if realtime_id.def(w_plotspec_id.xcord) gt 1 then begin
	if (*scanData.px)[n2] gt realtime_id.xmax then begin
	dxx = 0.1*((*scanData.px)[n2]-(*scanData.px)[0])
	xmin = (*scanData.px)[0] - dxx
	xmax = (*scanData.px)[n2] + dxx
	if xmax gt realtime_id.xmax then realtime_id.xmax = xmax
	if xmin lt realtime_id.xmin then realtime_id.xmin = xmin
	realtime_id.axis = 1
	end
end

; if data point as x axis is requested

xtitle = strtrim(w_plotspec_array(1),2)
if w_plotspec_id.x_axis_u eq 1 then begin
	xa = findgen(n2+1) 
	(*scanData.px)[0:n2] = xa
	xmin = - 0.05 * scanData.req_npts 
	xmax = scanData.req_npts *1.05 
	xtitle = 'Step #'
end

realtime_yrange,scanData.lastPlot,ymin,ymax,plotXTitle,pos_ymin
;print,'axis',realtime_id.axis,xmin,xmax,ymin,ymax,plotXTitle

WSET, widget_ids.plot_area

; reset the realtime plot coordinates

;if ymin eq ymax then return
if ymin eq ymax then begin
	ymin = ymin - 5
	ymax = ymax + 5
end 

if realtime_id.axis eq 1 then begin 

;	tempTitle=strtrim(w_plotspec_array(0))+' (1D SCAN # '+strtrim(w_plotspec_id.seqno+1,2) +')'
	tempTitle=strtrim(w_plotspec_array(0))+' (1D SCAN # '+strtrim(scanData.scanno,2) +')'

	y_range = [ymin,ymax]
; Y > 0
	if w_plotspec_id.log eq 2 and ymin lt 0. then  y_range=[0.,ymax]
	if w_plotspec_id.log eq 1 then begin
		if ymin le 0. then ymin = pos_ymin
		y_range=[ymin,ymax*10]
		end

	if w_plotspec_id.log eq 0 or w_plotspec_id.log eq 2 then $
	plot,xrange=[xmin,xmax], $
		yrange=y_range, $
		title=tempTitle, $
		xtitle=xtitle, $
		xticklen = w_plotspec_id.xticklen, $
		yticklen = w_plotspec_id.yticklen, $
		xgridstyle = w_plotspec_id.gridstyle, $
		ygridstyle = w_plotspec_id.gridstyle, $
		xminor= 10, $
		yminor= 10, $
		/nodata, /xstyle, /ystyle, $
		max_value=0,['1','1'] $

	else $
	plot,xrange=[xmin,xmax], $
		yrange=y_range, $
		title=tempTitle, $
		xtitle=xtitle, $
		xticklen = w_plotspec_id.xticklen, $
		yticklen = w_plotspec_id.yticklen, $
		xgridstyle = w_plotspec_id.gridstyle, $
		ygridstyle = w_plotspec_id.gridstyle, $
		xminor= 10, $
	;	ytype = w_plotspec_id.log, $
		/ylog, /nodata, /xstyle, /ystyle, $
		max_value=0,['1','1']


	n0=1
;	if scanData.y_scan then n0=0
	if n1 gt 1  then begin
	 x = (*scanData.px)[n0:n1]
	
	; plot Detector vs positioner 
	for i=0,scanData.nd - 1 do begin
	if realtime_id.def(4+i) ne 0 and scanData.wf_sel(i) eq 1 then begin
	color = w_plotspec_id.colorI(i)
	if !d.n_colors gt !d.table_size then  color = w_plotspec_id.colorV(i)

	y = (*scanData.da)[n0:n1,i]

		oplot,x,y,LINE=ln_style(i), $
			PSYM = symbol * (i+1) mod 8, $
			COLOR=color
		end
	end
	; plot positoner vs positioner (encode cases)
	for i=0,scanData.num_pos - 1 do begin
	if realtime_id.def(i) ne 0 and scanData.wf_sel(scanData.nd+i) eq 1 then begin
	color = w_plotspec_id.colorI(scanData.nd+i)
	if !d.n_colors gt !d.table_size then  color = w_plotspec_id.colorV(scanData.nd+i)

	y = (*scanData.pa)[n0:n1,i]
	
	oplot,x,y,LINE=ln_style(i+scanData.nd), $
			PSYM = symbol * (i+1) mod 8, $
			COLOR=color
		end
	end
	realtime_id.axis = 0
	end
end

if n2 ge n1 then begin
for i=0,scanData.nd-1 do begin
	if realtime_id.def(4+i) ne 0 then begin
		if n2 eq 0 then begin
		xtemp = [(*scanData.px)[0],(*scanData.px)[0]]
		ytemp = [(*scanData.da)[0,i],(*scanData.da)[0,i]]
		endif else begin
		xtemp = [(*scanData.px)[n1:n2]]
		ytemp = [(*scanData.da)[n1:n2,i]]
		end
		if scanData.wf_sel(i) eq 1 then begin
	color = w_plotspec_id.colorI(i)
	if !d.n_colors gt !d.table_size then  color = w_plotspec_id.colorV(i)

		 oplot,xtemp, ytemp,LINE=ln_style(i), $
			 PSYM = symbol * (i+1) mod 8, $
			 COLOR=color
		end
	end
end
for i=0,scanData.num_pos-1 do begin
	if realtime_id.def(i) ne 0 then begin
		if n2 eq 0 then begin
		xtemp = [(*scanData.px)[0],(*scanData.px)[0]]
		ytemp = [(*scanData.pa)[0,i],(*scanData.pa)[0,i]]
		endif else begin
		xtemp = [(*scanData.px)[n1:n2]]
		ytemp = [(*scanData.pa)[n1:n2,i]]
		end
		if scanData.wf_sel(i+scanData.nd) eq 1 then begin
	color = w_plotspec_id.colorI(i+scanData.nd)
	if !d.n_colors gt !d.table_size then  color = w_plotspec_id.colorV(i+scanData.nd)

		oplot,xtemp, ytemp,LINE=ln_style(i+scanData.nd), $
			PSYM = symbol * (i+1) mod 8, $
			COLOR=color
		end
	end
end
end

xtemp=0
ytemp=0

realtime_id.no = n2 
if (n2+1) ge npts then begin
	realtime_id.ind = 2
;	print,'caclock',caclock()
	end
if !d.n_colors le !d.table_size then TVLCT,o_red,o_green,o_blue else $
device,decomposed=0

END


PRO realtime_yrange,auto,ymin,ymax,plotXTitle,pos_ymin

   COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

   x_axis = w_plotspec_id.x_axis_u

   num_pts = 1 > (scanData.act_npts-1)

   ;extract valid data from global arrays

   ;remember the state of "auto" for next time
   scanData.lastPlot = auto

!y.style = 1
!x.style = 1

   IF (auto EQ 1) THEN BEGIN
;!y.style = 2
;!x.style = 2
     ;if autoscale, determine appropriate Y values
     ;depending on which waveforms will be plotted
     ymin = realtime_id.ymin
     ymax = realtime_id.ymax
     if total(scanData.wf_sel) eq 0 then plotXTitle = 'Nothing Selected'

pos_ymin = 1.e20
for i=0,scanData.nd-1 do begin
     IF (scanData.wf_sel(i) EQ 1 and realtime_id.def(scanData.num_pos+i) NE 0) THEN  BEGIN
     d4 = (*scanData.da)[0:num_pts,i]
         IF (MIN(d4) LT ymin) THEN begin 
		ymin = MIN(d4)
                realtime_id.axis = 1
		end
         IF (MAX(d4) GT ymax) THEN begin
		ymax = MAX(d4)
                realtime_id.axis = 1
		end
	if w_plotspec_id.log eq 1 and ymin le 0. then begin
	for j=0,num_pts-1 do begin
		if d4(j) gt 0. and d4(j) lt pos_ymin then pos_ymin = d4(j)
	end
	end
     END
end

; if Pi to be plotted as Y

for i=0,scanData.num_pos -1 do begin
	IF(scanData.wf_sel(scanData.nd+i) eq 1 and realtime_id.def(i) NE 0) THEN BEGIN
	d4 = (*scanData.pa)[0:num_pts,i]
	if min(d4) lt ymin then begin
		ymin = min(d4)
		realtime_id.axis = 1
		end
	if max(d4) gt ymax then begin
		ymax = max(d4)
		realtime_id.axis = 1
		end
	if w_plotspec_id.log eq 1 and ymin le 0. then begin
	for j=0,num_pts-1 do begin
		if d4(j) gt 0. and d4(j) lt pos_ymin then pos_ymin = d4(j)
	end
	end
	END
end

;
;  increase the ymin,ymax by +5%
;
if realtime_id.axis eq 1 then begin
	if ymax gt realtime_id.ymax or ymin lt realtime_id.ymin then begin
	dy = 0.1 *(ymax-ymin)
	ymax = ymax + dy
	ymin = ymin - dy
	end
	end

   ; if not autoscale, get limits from entry widgets.
   ENDIF ELSE BEGIN
ymin = w_plotspec_limits(2)
ymax = w_plotspec_limits(3)
   ENDELSE
     
realtime_id.ymin = ymin
realtime_id.ymax = ymax

     ;now determine xaxis label 
     IF (x_axis EQ 0) THEN BEGIN
       plotXTitle = 'P' + strtrim(w_plotspec_id.xcord+1)
     ENDIF ELSE BEGIN
       plotXTitle = 'Step #'
     ENDELSE


END

PRO realtime_xrange,auto,xmin,xmax

   COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

   x_axis = w_plotspec_id.x_axis_u

   ;remember the state of "auto" for next time
   scanData.lastPlot = auto

; if the time axis is selected for plot for real time

if realtime_id.def(w_plotspec_id.xcord) eq 2 then begin

        x_rn = realtime_pvnames

	xmin=0
        xmax = realtime_id.xmax
;        ln = caget(x_rn(w_plotspec_id.xcord),pd)
        ln = cagetArray(x_rn(w_plotspec_id.xcord),pd)
        if ln eq 0 and pd(0) gt xmax then begin
                xmax=pd
                dx = 0.1 * pd(0)
                realtime_id.xmax = xmax + dx
                realtime_id.xmin = xmin - dx
                realtime_id.axis = 1
        end
;ln = cagetTypeCount(x_rn(w_plotspec_id.xcord),ty,ct,wty)
;print,pd,ty(0),ct(0),wty(0)
        return

endif else begin

; not time axis case

CASE  w_plotspec_id.xcord OF
0: begin
	x_dn = [scanData.pv+'.P1SP', scanData.pv+'.P1EP', scanData.pv+'.P1CV', $
		scanData.pv+'.P1SM', scanData.pv+'.P1AR', scanData.pv+'.P1PP']
   end
1: begin
	x_dn = [scanData.pv+'.P2SP', scanData.pv+'.P2EP', scanData.pv+'.P2CV', $
		scanData.pv+'.P2SM', scanData.pv+'.P2AR', scanData.pv+'.P2PP']
   end
2: begin
	x_dn = [scanData.pv+'.P3SP', scanData.pv+'.P3EP', scanData.pv+'.P3CV', $
		scanData.pv+'.P3SM', scanData.pv+'.P3AR', scanData.pv+'.P3PP']
   end
3: begin
	x_dn = [scanData.pv+'.P4SP', scanData.pv+'.P4EP', scanData.pv+'.P4CV', $
		scanData.pv+'.P4SM', scanData.pv+'.P4AR', scanData.pv+'.P4PP']
   end
ELSE: w_warningtext,'w_plotspec_id.xcord is an illegal value.'
ENDCASE
	ln = cagetArray(x_dn,pd)
	x_dv = pd

; On the fly mode
	if x_dv(3) eq 2. then begin
	realtime_idlfly = 1
	if x_dv(4) gt 0. then begin
	xmin = x_dv(0) + x_dv(5)
	xmax = x_dv(1) + x_dv(5)
	endif else begin
	xmin = x_dv(0)
	xmax = x_dv(1)
	end
	end


; linear mode
	if x_dv(3) eq 0. then begin
	if x_dv(4) gt 0. then begin
    ;   relative mode
;	xmin = x_dv(2) 
;	if x_dv(2) gt 0 and x_dv(5)  gt x_dv(2) then xmin = x_dv(5)
;	if x_dv(2) lt 0 and x_dv(5) lt x_dv(2) then xmin = x_dv(5) 
;	xmax = xmin + x_dv(1)-x_dv(0)

	xmin = x_dv(0)+x_dv(5)
	xmax = x_dv(1)+x_dv(5)
	if x_dv(0) gt x_dv(1) then begin
		xtemp = xmax
		xmax = xmin
		xmin = xtemp
		end
	endif else begin
    ;   absolute mode
	xmin = min([x_dv(0),x_dv(1)])
	xmax = max([x_dv(0),x_dv(1)])
	end
	end

; table mode
	if x_dv(3) eq 1. then begin
	CASE w_plotspec_id.xcord OF
	0: x_dn = scanData.pv+'.P1PA'
	1: x_dn = scanData.pv+'.P2PA'
	2: x_dn = scanData.pv+'.P3PA'
	3: x_dn = scanData.pv+'.P4PA'
	ENDCASE
;	ln = caget(x_dn, pd, max=scanData.req_npts)
	ln = cagetArray(x_dn, pd, max=scanData.req_npts)
	x = pd
	if x_dv(4) gt 0. then begin
	dx = MAX(x) - MIN(x)
	xmax = MAX(x) + x_dv(2)  
	xmin = xmax - dx
	endif else begin
	xmin = MIN(x)
	xmax = MAX(x)
	end
	end

end
;
;  increase the xmin,xmax by +5%
;
	dx = 0.1 *(xmax-xmin)
	if dx lt 0. then dx = 2.*dx
	xmax = xmax + dx
	xmin = xmin - dx

     
realtime_id.xmin = xmin
realtime_id.xmax = xmax

END


;
;  readLabelsPvnames,'1.out',labels,pvnames
;
PRO readLabelsPvnames,filename,labels,pvnames,help=help
if keyword_set(help) then begin
	st = [$
	"Usage: readLabelsPvnames,filename,labels,pvnames",$
	'',$
	'This function read the pvnames and labels from a text file',$
	'',$
	'   Input:   filename         specifies input filename ',$
	'   Output: ',$
	'             pvnames         first string of input line returned as pvname',$
	'             labels          second string of input line returned as label']
	w_warningtext,st
	return
	end

spawn,['wc','-l',filename],y, /noshell
if y(0) eq '' then begin
	print,'Error: bad filename for readLabelsPvnames' 
	return 
	end
no = y(0) 
pvnames = make_array(no,/string,value=string(replicate(32b,30)))
labels = make_array(no,/string,value=string(replicate(32b,40)))
openr,1,filename
x=''
pv=''
lb=''
i=0
while (not eof(1) and i lt no) do begin
readf,1,x
x = strtrim(x,2)
len = strlen(x) 
pos = strpos(x,' ')
if pos gt -1 then begin
	pv = strmid(x,0,pos)
	lb = strtrim(strmid(x,pos,len-pos),1)
endif else begin
	pv = x
	lb=''
	end
pvnames(i) = pv
labels(i) = lb
i=i+1
end
close,1
pvnames = pvnames(0:i-1)
labels = labels(0:i-1)
END

PRO close_plotspec
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
if XRegistered('w_plotspec') ne 0 then $
	WIDGET_CONTROL,w_plotspec_ids.base,/DESTROY
END

PRO w_plotspec_saveTitle
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved 
		w_plotspec_saved(0) = w_plotspec_array(0)
		w_plotspec_saved(1) = w_plotspec_array(1)
		w_plotspec_saved(2) = w_plotspec_array(2)
END

PRO w_plotspec_restoreTitle
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved 
		w_plotspec_array(0) = w_plotspec_saved(0)
		w_plotspec_array(1) = w_plotspec_saved(1)
		w_plotspec_array(2) = w_plotspec_saved(2)
END


PRO setDefaultLabels
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

npd = scanData.num_det;

if n_elements(x_names) eq 0 then begin
        x_names=make_array(4,/string,value=string(replicate(32b,30)))
        y_names=make_array(npd,/string,value=string(replicate(32b,30)))
        x_descs=make_array(4,/string,value=string(replicate(32b,30)))
        y_descs=make_array(npd,/string,value=string(replicate(32b,30)))
        x_engus=make_array(4,/string,value=string(replicate(32b,30)))
        y_engus=make_array(npd,/string,value=string(replicate(32b,30)))
	end

if w_plotspec_id.mode eq 0 then begin
if casearch(scanData.pv) eq 0 then begin 
        find_desc_engu,names,descs,engus

        x_names = names(0:3)
        y_names(0:scanData.nd-1) = names(4:4+scanData.nd-1)
        x_descs = descs(0:3)
        y_descs(0:scanData.nd-1) = descs(4:4+scanData.nd-1)
        x_engus = engus(0:3)
        y_engus(0:scanData.nd-1) = engus(4:4+scanData.nd-1)

end
end

END

PRO setPlotLabels
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

; if w_plotspec is open

; if w_plotspec_id.mode eq 1 then return
if XRegistered('w_plotspec') ne 0 then return
	

        title = w_plotspec_array(0)
        ix = w_plotspec_id.xcord
	t_xlabel = 'P'+strtrim(ix+1,2)

; get xlabel
     if ix lt 4 then begin
	xdescs = scanData.xdescs
	xlabel = xdescs(ix)
;	if x_descs(ix) ne '' then xlabel = x_descs(ix) else $
;	xlabel = x_names(ix)
;	if x_engus(ix) ne '' then xlabel = xlabel +' ('+x_engus(ix) +')'
     end
     if ix ge 4 then begin   ; if detector for x axis
	ixx = ix - 4
	len = strlen(y_descs(ixx))
        if len gt 1 then strput,xlabel,y_descs(ixx) else $
        	strput,xlabel,y_names(ixx+1,2)
	if len lt 1 then len = 2
	l2 = strlen(y_engus(ixx))
        if l2 gt 1 then begin
		len = len + 2
		strput,xlabel,'(',len
		len = len + 2
		strput,xlabel,strtrim(y_engus(ixx)),len
		len = len + l2 + 1
		strput,xlabel,')',len
		end
     end

	if strtrim(title,2) ne '' then w_plotspec_array(0) = title
	if strtrim(xlabel,2) ne '' then w_plotspec_array(1) = xlabel else $
		w_plotspec_array(1) = t_xlabel
;print,'TITLE:',title
;print,'XLABEL:',xlabel

END




PRO find_desc_engu,names,descs,engus
; RETURN:
;       names - scan record Pi,Di PV names (4 positioners and 70 detectors)
;       descs - corresponding descs from the  database
;       engus - corresponding Pi, Di engu from the database
;
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON field_block, field_name, field_name_array, field_value, w_scanfield_ids
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

; 4 positioner + 85 detectors
TOT = scanData.num_det+4

names= make_array(TOT,/string,value=string(replicate(32b,30)))
descs = make_array(TOT,/string,value=string(replicate(32b,30)))
engus = make_array(TOT,/string,value=string(replicate(32b,30)))

if casearch(scanData.pv) eq 0 then begin 
scan_field_get,scanData.pv

; check for defined PiPV & DiPV
realtime_id.def = make_array(TOT,/int)

x_dn = [ scanData.pv+'.R1PV', $
	scanData.pv+'.R2PV', $
	scanData.pv+'.R3PV', $
	scanData.pv+'.R4PV' ]
ln = cagetArray(x_dn,p1,/string)

x_dn = [ scanData.pv+'.P1PV', $
	scanData.pv+'.P2PV', $
	scanData.pv+'.P3PV', $
	scanData.pv+'.P4PV' ]
ln = cagetArray(x_dn,p2,/string)

p1 = strtrim(p1,2)
p2 = strtrim(p2,2)
for i=0,3 do begin
	if strlen(p1(i)) eq 0 then p1(i) = p2(i)
end

	DI = scanData.DI
	if scanData.new and n_elements(DI) gt 70 then DI = DI(15:84) 
	x_dn = scanData.pv + '.'+ DI + 'PV'

ln = cagetArray(x_dn,pd,/string)
x_dv = strtrim(pd,2)
names(0:3) = strtrim(p1,2)
names(4:4+n_elements(DI)-1) = x_dv

; get desc & eng units
 
s0=string(replicate(32b,30))

for i=0,4+n_elements(DI)-1 do begin
if strlen(names(i)) gt 1 then begin
        realtime_id.def(i) = 1
;	if i lt 4 then begin
        id = strpos(names(i),'.',0)
	v=s0
        if id ne -1 then strput,v,strmid(names(i),0,id),0 else $
		strput,v,names(i),0
	vd = strcompress(v + '.DESC',/remove_all)
	pd=''
	ln = cagetArray(vd,pd)
        if ln eq '-1' then descs(i)='' else descs(i) = pd
;        end
end
end

; read .tmpName
found = findfile('.tmpName',count=ct)
if ct then begin
xdr_open,unit,'.tmpName'
xdr_read,unit,dnames
xdr_close,unit
descs(4:88)=dnames
end


	x_dn = scanData.pv + '.'+ ['P1','P2','P3','P4',DI] +'EU'
	ln = cagetArray(x_dn,pd,/string)
	x_dv = pd
	for i=0,4+n_elements(DI)-1 do begin
        engus(i) = strtrim(x_dv(i),2)
        if strtrim(engus(i),2) eq '-1' then engus(i)=''
	end
 

; check whether time array to be used for one of the positioner

dd = [scanData.pv+'.R1PV',scanData.pv+'.R2PV',scanData.pv+'.R3PV', $
	scanData.pv+'.R4PV']
ln = cagetArray(dd,ptime,/string)
if ln eq 0 then begin
for i=0,3 do begin
	if strlen(ptime(i)) gt 1 and strpos("TIMEtimeTime",ptime(i)) ne -1 then begin	
		 names(i) = ptime(i)
		 descs(i) = 'Time'
		 engus(i) = 'sec'
		 realtime_id.def(i) = 2		; time second used 
		end
end
end

; need redefine the realtime_id.def for the case when MCA array is 
;      entered in the DiPV for scan record
;
end
; need check the case when readback PV name is non zero case======

END

PRO w_plotspec_event,event
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON PLOTMENU_OPTION_BLOCK,ids,r_names        ; update plotoption menu
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvname


WIDGET_CONTROL, event.id, GET_UVALUE = eventval

if w_plotspec_id.scan eq 0 and realtime_id.ind eq -1 then $
  scanData.act_npts = scanData.readin_npts 

CASE eventval OF
	"PLOT_TITLE" : BEGIN
                WIDGET_CONTROL,w_plotspec_ids.title,GET_VALUE=temp
                w_plotspec_array(0) = strcompress(temp(0))
		END
	"PLOT_XTITLE" : BEGIN
                WIDGET_CONTROL,w_plotspec_ids.xtitle,GET_VALUE=temp
                w_plotspec_array(1) = strcompress(temp(0))
		END
	"PLOT_YTITLE" : BEGIN
                WIDGET_CONTROL,w_plotspec_ids.ytitle,GET_VALUE=temp
                w_plotspec_array(2) = strcompress(temp(0))
		END
	"PLOT_SAVENAME" : BEGIN
                WIDGET_CONTROL,w_plotspec_ids.savename,GET_VALUE=temp
                w_plotspec_array(3) = strcompress(temp(0),/remove_all)
		END
	"PLOT_FOOTER" : BEGIN
                WIDGET_CONTROL,w_plotspec_ids.footer,GET_VALUE=temp
                w_plotspec_array(5) = strcompress(temp(0))
		END
;	"PLOT_RANGES" : BEGIN
;		user_scale,GROUP=event.top
;		END
        "PLOTSPEC_OK" : BEGIN
                WIDGET_CONTROL,w_plotspec_ids.title,GET_VALUE=temp
                w_plotspec_array(0) = strcompress(temp(0))
                WIDGET_CONTROL,w_plotspec_ids.xtitle,GET_VALUE=temp
                w_plotspec_array(1) = strcompress(temp(0))
                WIDGET_CONTROL,w_plotspec_ids.ytitle,GET_VALUE=temp
                w_plotspec_array(2) = strcompress(temp(0))
                WIDGET_CONTROL,w_plotspec_ids.savename,GET_VALUE=temp
                w_plotspec_array(3) = strcompress(temp(0),/remove_all)
                WIDGET_CONTROL,w_plotspec_ids.footer,GET_VALUE=temp
                w_plotspec_array(5) = strcompress(temp(0))
                END
	"PLOTSPEC_DONE" : BEGIN
		WIDGET_CONTROL,w_plotspec_ids.title,GET_VALUE=temp
		w_plotspec_array(0) = strcompress(temp(0))
		WIDGET_CONTROL,w_plotspec_ids.xtitle,GET_VALUE=temp
		w_plotspec_array(1) = strcompress(temp(0))
		WIDGET_CONTROL,w_plotspec_ids.ytitle,GET_VALUE=temp
		w_plotspec_array(2) = strcompress(temp(0))
		WIDGET_CONTROL,w_plotspec_ids.savename,GET_VALUE=temp
		w_plotspec_array(3) = strcompress(temp(0),/remove_all)
                WIDGET_CONTROL,w_plotspec_ids.footer,GET_VALUE=temp
                w_plotspec_array(5) = strcompress(temp(0))
		WIDGET_CONTROL, event.top, /DESTROY
		return
                END
	"PLOTSPEC_CANCEL" : BEGIN
		WIDGET_CONTROL, event.top, /DESTROY
		return
		END
ENDCASE

if realtime_id.ind eq 1 then begin
	realtime_id.axis = 1
endif else $
	UPDATE_PLOT,scanData.lastPlot

END

PRO w_plotspec, GROUP = GROUP, help=help
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved 
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

if XRegistered('w_plotspec') ne 0 then return

if n_elements(w_plotspec_array) eq 0 then $
w_plotspec_array = make_array(6,/string,value=string(replicate(32b,60)))
	
if n_elements(w_plotspec_limits) eq 0 then begin
	w_plotspec_limits = make_array(4,/float)
	w_plotspec_limits = [0., 100., 0., 100.]
	end


if strlen(strcompress(w_plotspec_array(3),/remove_all)) lt 1 then $
	w_plotspec_array(3) = 'catch1d.trashcan'

w_plotspec_base=WIDGET_BASE(TITLE = 'Plot Labels ... ', /COLUMN)     

row0 = WIDGET_BASE(w_plotspec_base, /ROW)

seqno_lb = WIDGET_LABEL(row0, VALUE='Scan #: ' + $
	 strcompress(w_plotspec_id.seqno))

;limits_lb = WIDGET_BUTTON(row0, VALUE='Plot Ranges ...', $
;		UVALUE= 'PLOT_RANGES')


row1 = WIDGET_BASE(w_plotspec_base, /ROW)
title_lb = WIDGET_LABEL(row1, VALUE='Title  :')
w_plotspec_title = WIDGET_TEXT(row1, /EDITABLE,  /NO_NEWLINE, $
	SCR_XSIZE = 300, $
        XSIZE=60, YSIZE=1, VALUE=strtrim(w_plotspec_array(0)), UVALUE='PLOT_TITLE')

row2 = WIDGET_BASE(w_plotspec_base, /ROW)
xtitle_lb = WIDGET_LABEL(row2, VALUE='X Label:')
w_plotspec_xtitle = WIDGET_TEXT(row2, /EDITABLE,  /NO_NEWLINE, $
	SCR_XSIZE = 300, $
        XSIZE=60, YSIZE=1, VALUE=strtrim(w_plotspec_array(1)), UVALUE='PLOT_XTITLE')

row3 = WIDGET_BASE(w_plotspec_base, /ROW)
ytitle_lb = WIDGET_LABEL(row3, VALUE='Y Label:')
w_plotspec_ytitle = WIDGET_TEXT(row3, /EDITABLE,  /NO_NEWLINE, $
	SCR_XSIZE = 300, $
        XSIZE=60, YSIZE=1, VALUE=strtrim(w_plotspec_array(2)), UVALUE='PLOT_YTITLE')

row4 = WIDGET_BASE(w_plotspec_base, /ROW)
savefile_lb = WIDGET_LABEL(row4, VALUE='Scan Data Saved in:   ')
w_plotspec_savename = WIDGET_LABEL(row4, VALUE=strtrim(w_plotspec_array(3)) )

row4_1 = WIDGET_BASE(w_plotspec_base, /ROW)
savefile_lb = WIDGET_LABEL(row4_1, VALUE='Comment:')
w_plotspec_footer = WIDGET_TEXT(row4_1, /EDITABLE,  /NO_NEWLINE, $
	SCR_XSIZE = 300, $
        XSIZE=60, YSIZE=1, VALUE=strtrim(w_plotspec_array(5)), UVALUE='PLOT_FOOTER')


lastrow = WIDGET_BASE(w_plotspec_base, /ROW)

w_plotspec_ok = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Apply ', $
                        UVALUE = 'PLOTSPEC_OK')
w_plotspec_cancel = WIDGET_BUTTON(lastrow, $
                        VALUE = 'Cancel', $
                        UVALUE = 'PLOTSPEC_CANCEL')
w_plotspec_done = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Done ', $
                        UVALUE = 'PLOTSPEC_DONE')

; set widget ids :
w_plotspec_ids = { $
	base:	w_plotspec_base, $
	title:  w_plotspec_title, $
	xtitle:  w_plotspec_xtitle, $
	ytitle:  w_plotspec_ytitle, $
	savename:  w_plotspec_savename, $
	footer:  w_plotspec_footer $
	 }

; Realize the widgets:
WIDGET_CONTROL, w_plotspec_base, /REALIZE
if w_plotspec_id.realtime eq 0 then $
WIDGET_CONTROL, w_plotspec_dtime,SENSITIVE=0 

; Hand off to the XMANAGER:
XMANAGER, 'w_plotspec', w_plotspec_base, GROUP_LEADER = GROUP

END


PRO w_viewscan_calcFilename,no
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
	
	; current scan # at most 4 digit 

	if strlen(w_plotspec_array(3)) eq 0 then return
	st = str_sep(w_plotspec_array(3),'.')
	st0 = str_sep(st(0),'_')
	prefix = strmid(w_plotspec_array(3),0,strlen(st0(0))+1)
	; get current file no
	if n_elements(no) eq 0 then begin
	no = fix(st0(1))
	scanData.fileno = no
	if no gt scanData.filemax then scanData.filemax=no
	return
	end

	if no lt 10000 then begin
        str = '0000'
        len0 = strlen(str)
        sss = str
	st = strtrim(no,2)
        len = strlen(st)
        strput,sss,st,len0-len
	endif else sss = strtrim(no,2)

	filename = prefix+sss+ scanData.suffix  ; '.mda'   ;'.scan'
	found = findfile(scanData.path+filename)

	if found(0) ne '' then begin
		w_plotspec_array(3) = filename
		scanData.trashcan = scanData.path+filename
;		catch1d_viewdataSetup
;		WIDGET_CONTROL,widget_ids.trashcan,SET_VALUE=scanData.trashcan
		if  scanData.scanno ge 0 then begin
;		WIDGET_CONTROL,w_viewscan_ids.base,/DESTROY
		w_viewscan,1,/init
		end
	endif else begin
		WIDGET_CONTROL,w_viewscan_ids.fileno,SET_VALUE=scanData.fileno
		res = dialog_message(['File: ', scanData.path+filename, ' not found'],/INFO)
	end
END 


PRO read_desc_engu,labels
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON LABEL_BLOCK,x_names,y_names,x_descs,y_descs,x_engus,y_engus

num = scanData.num_det+4   ;4+85

if n_elements(x_names) lt 4 then begin
	x_names = make_array(4,/string,value=string(replicate(32b,30)))
	x_descs = x_names
	x_engus = x_names
	y_names = make_array(scanData.num_det,/string,value=string(replicate(32b,30)))
	y_descs = y_names
	y_engus = y_names
	end
labels = string(labels)

for i=0,3 do begin 
	x_names(i) = labels(i)
	x_descs(i) = labels(i+num)
	x_engus(i) = labels(i+num*2)
end
for i=0,scanData.nd-1 do begin 
	y_names(i) = labels(i+4)
	y_descs(i) = labels(i+4+num)
	y_engus(i) = labels(i+4+num*2)
end

END


; 
; save ascii file of curr scan record 
; 
PRO save_scan_dump_curr,filename 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved

filename = strcompress(w_plotspec_array(3),/remove_all)+'.tmp'
openw,unit,filename,/get_lun

shortreport_data_dump,unit
free_lun, unit

END

; 
; save ascii file of a scan record 
; 
PRO save_scan_dump,filename 
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

filename = strcompress(w_plotspec_array(3),/remove_all)+'.tmp'

CATCH,error_status
if error_status lt 0 then begin
	w_warningtext,'Error: '+ !err_string
	return
	end

openw,unit,filename,/get_lun
printf,unit,"; VERSION: ",scanData.version,' ',scanData.release

str = "; 2D SCAN #: "
if scanData.dim eq 3 then str="; 3D SCAN #: "
if scanData.y_scan gt 0 then begin
printf,unit,str,scanData.scanno_2d,",      Y INDEX #:",scanData.y_seqno + 1
	end
if scanData.dim eq 2 and scanData.scanH eq 0 then begin
	printf,unit,str,scanData.scanno_2d
end
printf,unit,"; 1D SCAN #: ",w_plotspec_id.seqno 
printf,unit,"; SCAN Record Name: ",scanData.pv

printf,unit,'; '
printf,unit,"; PLOT SPECIFICATIONS"
printf,unit,'; '
printf,unit,"; Title:      ",w_plotspec_array(0)
printf,unit,"; X Label:    ",w_plotspec_array(1)
printf,unit,"; Y Label:    ",w_plotspec_array(2)
printf,unit,"; Saved in:   ",scanData.path+w_plotspec_array(3)
printf,unit,"; Time Stamp: ",w_plotspec_array(4)
printf,unit,"; Comment:    ",w_plotspec_array(5)

if w_plotspec_id.type eq 0 then printf,unit,"; Type:      Line"
if w_plotspec_id.type eq 1 then printf,unit,"; Type:      Point"
if w_plotspec_id.type eq 2 then printf,unit,"; Type:      Line/Point"
if w_plotspec_id.log  eq 0 then printf,unit,"; Y Scale:   Linear"
if w_plotspec_id.log  eq 1 then printf,unit,"; Y Scale:   Log"
if w_plotspec_id.errbars eq 0 then printf,unit,"; Errbars:   Off"
if w_plotspec_id.errbars eq 1 then printf,unit,"; Errbars:   On"
printf,unit,'; Realtime: itime=',w_plotspec_id.itime, ',  dtime=',w_plotspec_id.dtime
printf,unit,'; Plot Vs Position Array # ',w_plotspec_id.xcord + 1

printf,unit,'; '
printf,unit,'; SCAN Data:'
printf,unit,'; '

 save_data_subset_dump, unit
free_lun,unit
return
END


PRO save_data_subset_dump,unit 
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON LABEL_BLOCK,x_names,y_names,x_descs,y_descs,x_engus,y_engus

; get desc & eng units

if w_plotspec_id.mode eq 0 and  casearch(scanData.pv) eq 0 then begin
	find_desc_engu,names,descs,engus
endif else begin    ; viewing mode
	names = [x_names,y_names]
	descs = [x_descs,y_descs]
	engus = [x_engus,y_engus]
end

temp_format = '('+scanData.code+scanData.format+')'
temp_digit = fix(scanData.format)

twd = temp_digit*(total(scanData.id_def) + 1)
s0 = string(replicate(32b,twd))

	no = n_elements(names)

st=s0
strput,st,';  (PI/DI)',0  &  ij = 17 
        for i=0,no-1 do begin
        if scanData.id_def(i) ne 0 then begin
		if i lt 4 then strput,st,scanData.PI(i),ij else begin
		   if scanData.svers then strput,st,scanData.DI[15+i-4],ij $
		   else strput,st,scanData.DI[i-4],ij
		end
                ij = ij + temp_digit
                end
        end
	printf,unit,st

	st = s0
	strput,st,';   PV   ',0
	ij=17
        for i=0,no-1 do begin
        if scanData.id_def(i) ne 0 then begin
		strput,st,names(i),ij
		ij=ij+temp_digit
                end
        end
	printf,unit,st

st=s0
strput,st,';  (Desc)',0  &  ij = 17 
        for i=0,no-1 do begin
        if scanData.id_def(i) ne 0 then begin
                strput,st,descs(i),ij
                ij = ij + temp_digit
                end
        end
	printf,unit,st


st = s0
strput,st,'; (Units)',0  &  ij = 17 
        for i=0,no-1 do begin
        if scanData.id_def(i) ne 0 then begin
                strput,st,engus(i),ij
                ij = ij + temp_digit
                end
        end
printf,unit,st

num_npts = scanData.readin_npts

for i=0,num_npts-1 do begin
st = s0
strput,st,i,0  &  ij = 10
	for j = 0,3 do begin
		if scanData.id_def(j) ne 0 then begin
		strput,st,string((*scanData.pa)[i,j],format=temp_format),ij  
		ij = ij + temp_digit
		end
	end
	for j = 0,84 do begin
		if scanData.id_def(4+j) ne 0 then begin
		strput,st,string((*scanData.da)[i,j],format=temp_format),ij  
		ij = ij + temp_digit
		end
	end
printf,unit,st
end

END


PRO w_viewscan_event,event
COMMON SYSTEM_BLOCK,OS_SYSTEM
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

WIDGET_CONTROL, event.id, GET_UVALUE = eventval

CASE eventval OF
	"VIEWSPEC_FILENO" : BEGIN
		WIDGET_CONTROL,w_viewscan_ids.fileno,GET_VALUE=no
		scanData.fileno = no
;		if no ge 0 and no le scanData.filemax then $
		w_viewscan_calcFilename,no
		END
	"VIEWSPEC_SEQNO" : BEGIN
		close_plotspec
		w_warningtext_close
		WIDGET_CONTROL,w_viewscan_ids.seqno,GET_VALUE=seqno
		i1 = fix(seqno(0))
	if i1 lt 1  or i1 gt w_viewscan_id.maxno then begin
	w_warningtext,'Input out of range !!'
	return
	end
		if i1 ge 1 then begin 
		i2 = i1 + 1
	if scanData.bypass3d then scan_read,1,i1,i2,pickDet=-1 else $
		scan_read,1, i1, i2
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
		WIDGET_CONTROL,w_viewscan_ids.slider,SET_VALUE=i1
		end
                END
	"VIEWSPEC_FORMAT" : BEGIN
	        WIDGET_CONTROL,w_viewscan_ids.format, GET_VALUE=format
	       	format = strcompress(format(0),/remove_all)
	       	scanData.code = strmid(format,0,1)
	        scanData.format = strmid(format,1,10)
		ret = strpos('defgDEFG',scanData.code)
		if ret eq -1 then w_warningtext,'Error:   illegal format entered !!!'
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
		END
	"VIEWSPEC_PSPRINT" : BEGIN
        	if scanData.lastPlot lt 0 then return
		scanData.act_npts = scanData.readin_npts
        	PS_open,'catch1d.ps'
        	UPDATE_PLOT, scanData.lastPlot
        	PS_close
        	PS_print,'catch1d.ps'
       		END
	"VIEWSPEC_NEW" : BEGIN
		w_plotspec, GROUP=w_viewscan_ids.base
		END
	"VIEWSPEC_FIELD" : BEGIN
		save_scan_dump,filename
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=1
	;	xdisplayfile,filename,width=110,GROUP= event.top
		str='0000'
		no = strtrim(w_plotspec_id.seqno,2)
		len = strlen(no)
		if len lt 5 then begin
			strput,str,no,4-len
		endif else str=no
		rename=scanData.trashcan+'.'+str
		rename=scanData.outpath+w_plotspec_array(3)+'.'+str
		res=cw_term(w_viewscan_ids.base,filename=filename,/SCROLL, $
			rename=rename,bg_names='Save As...')
		END
	"VIEWSPEC_PRINT" : BEGIN
		filename = strcompress(w_plotspec_array(3),/remove_all)+'.tmp'
		PS_enscript,filename
		END
	"VIEWSPEC_ASCII" : BEGIN
		save_scan_dump,file1
filename = w_plotspec_array(3)+'.'+ string(w_plotspec_id.seqno + 1)
filename = strcompress(filename,/remove_all)

if OS_SYSTEM.os_family eq 'unix' then spawn,[OS_SYSTEM.mv, file1, filename],/noshell $
	else spawn,[OS_SYSTEM.mv, file1, filename]

		print,'ASCII SCAN data saved in : ',filename
		END
	"VIEWSPEC_PREV" : BEGIN
		close_plotspec
		w_warningtext_close
		WIDGET_CONTROL,w_viewscan_ids.seqno,GET_VALUE=seqno
		i1 = fix(seqno(0)) - 1
	if i1 lt 1 then i1 = w_viewscan_id.maxno 

		i2 = i1 + 1
	if scanData.bypass3d then scan_read,1,i1,i2,pickDet=-1 else $
		scan_read,w_viewscan_id.unit, i1, i2
		seqno = strtrim(string(i1),2)
		WIDGET_CONTROL,w_viewscan_ids.seqno,SET_VALUE=seqno
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
		WIDGET_CONTROL,w_viewscan_ids.slider,SET_VALUE=i1
                END
	"VIEWSPEC_NEXT" : BEGIN
		close_plotspec
		w_warningtext_close
		WIDGET_CONTROL,w_viewscan_ids.seqno,GET_VALUE=seqno
		i1 = fix(seqno(0)) + 1
		if i1 ge (w_viewscan_id.maxno+1) then i1 = 1 
		i2 = i1 + 1

	if scanData.bypass3d then scan_read,1,i1,i2,pickDet=-1 else $
		scan_read,w_viewscan_id.unit, i1, i2 

		seqno = strtrim(string(i1),2)
		WIDGET_CONTROL,w_viewscan_ids.seqno,SET_VALUE=seqno
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
		WIDGET_CONTROL,w_viewscan_ids.slider,SET_VALUE=i1
                END
	"VIEWSPEC_LAST" : BEGIN
		close_plotspec
		w_warningtext_close
		if w_viewscan_id.maxno eq 0 then begin
			w_warningtext,'Error: no data available!'
			return
			end
		i1 = w_viewscan_id.maxno
		i2 = i1+1
	if scanData.bypass3d then scan_read,1,i1,i2,pickDet=-1 else $
		scan_read,w_viewscan_id.unit, i1, i2
		WIDGET_CONTROL,w_viewscan_ids.seqno,SET_VALUE=strtrim(i1,2)
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
		WIDGET_CONTROL,w_viewscan_ids.slider,SET_VALUE=i1
                END
	"VIEWSPEC_FIRST" : BEGIN
		close_plotspec
		w_warningtext_close
		if w_viewscan_id.maxno eq 0 then begin
			w_warningtext,'Error: no data available!'
			return
			end
	if scanData.bypass3d then scan_read,1,1,2,pickDet=-1 else $
		scan_read,w_viewscan_id.unit, 1, 2
		WIDGET_CONTROL,w_viewscan_ids.seqno,SET_VALUE='1'
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
		WIDGET_CONTROL,w_viewscan_ids.slider,SET_VALUE=1
                END
	"VIEWSPEC_SLIDER" : BEGIN
		close_plotspec
		w_warningtext_close
		WIDGET_CONTROL,w_viewscan_ids.slider,GET_VALUE=seqno
	if scanData.bypass3d then scan_read,1,seqno,seqno+1,pickDet=-1 else $
		scan_read,w_viewscan_id.unit,seqno,seqno+1 
		s1 = strtrim(string(seqno),2)
		WIDGET_CONTROL,w_viewscan_ids.seqno,SET_VALUE=s1
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
                END
;	"VIEWSPEC_2DIMAGE" : BEGIN
;		vw2d,/CA, GROUP=event.top, file=scanData.trashcan   ;, $
;			lastDet=scanData.lastDet
;		END
	"VIEWSPEC_FIRST_FILE" : BEGIN
		scanData.fileno = 1
		w_viewscan_calcFilename,scanData.fileno
		END
	"VIEWSPEC_NEXT_FILE" : BEGIN
		scanData.fileno = scanData.fileno+1
		w_viewscan_calcFilename,scanData.fileno
		END
	"VIEWSPEC_PREV_FILE" : BEGIN
		if scanData.fileno eq 0 then begin
			res=dialog_message('First file reached!',/INFO)
			return
		end
		scanData.fileno = scanData.fileno-1
		w_viewscan_calcFilename, scanData.fileno
		END
	"VIEWSPEC_LAST_FILE" : BEGIN
		scanData.fileno = scanData.filemax
		w_viewscan_calcFilename,scanData.fileno
		END
	"VIEWSPEC_CANCEL" : BEGIN
		WIDGET_CONTROL, event.top, /DESTROY
		END
ENDCASE

END

PRO w_viewscan_close, wid
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

WIDGET_CONTROL,/HOURGLASS

; get the last scan first


; close the view mode 
		w_viewscan_id.unit = 0

; reset the w_plotspec variable
		w_viewscan_id.maxno = scanData.scanno
		w_plotspec_id = scanData.plotspec 

		w_warningtext_close

		if XRegistered('catcher_setup') ne 0 then $
		WIDGET_CONTROL,catcher_setup_ids.base,SENSITIVE=1

; reset to config pv names
 
		scanData.pv = scanData.pvconfig(0)
		scanData.y_pv = scanData.pvconfig(1)

 		pventry_event ;======
;		before_sys_scan

		w_plotspec_restoreTitle

	catch1d_check_seqno

scanData.option = 1  ; acquisition on

END

PRO w_viewscan, unit, INIT=INIT, GROUP = GROUP, help=help

COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON view1d_summary_block, view1d_summary_ids, view1d_summary_id

if XRegistered('w_viewscan') ne 0 then $ ; return
		WIDGET_CONTROL,w_viewscan_ids.base,/DESTROY,bad=bad

scanData.option = 0  ; acquisition off
if XRegistered('view1d_summary_setup') ne 0 then $
	WIDGET_CONTROL,view1d_summary_ids.base,/DESTROY

if scanData.lastPlot lt 0 then scanData.lastPlot = 1  ; autoscale  

if n_params() lt 1 then begin
	w_warningtext,'usage:  w_viewscan, unit, GROUP=event.top'
	end

; reset the pv name in viewing mode and save the config pv for later on
;      scan mode
scanData.pvconfig(0) = scanData.pv  
scanData.pvconfig(1) = scanData.y_pv  

WIDGET_CONTROL, /HOURGLASS

w_viewscan_id.unit = unit

if keyword_set(init) then begin
	if scanData.bypass3d then scan_read,1,-1,-1,maxno,dim,scanH ,pickDet=-1 else $
	scan_read,unit,-1,-1,maxno,dim,scanH  
endif else begin
	if scanData.bypass3d then scan_read,1,-1,0,maxno,dim,scanH ,pickDet=-1  else $
	scan_read,unit,-1,0,maxno,dim,scanH  
end

next_step:
if n_elements(maxno) eq 0 then begin
	r = dialog_message(!err_string,/error)
	close,unit
	return
;	retall
end

if dim eq 3 then w_viewscan_id.maxno = maxno

w_viewscan_topbase=WIDGET_BASE(GROUP_LEADER=Group, $
	TLB_FRAME_ATTR = 8, $
	TITLE = 'VIEW 1D ... ', /COLUMN)     


lastrow_0 = WIDGET_BASE(w_viewscan_topbase, /ROW)
lastrow = WIDGET_BASE(lastrow_0, /ROW, /FRAME)

w_viewscan_fileno = CW_FIELD( lastrow,VALUE=scanData.fileno, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS= 1, $
      TITLE='File Seq #: ', $
      XSIZE=5, $
      UVALUE='VIEWSPEC_FILENO')

w_viewscan_first = WIDGET_BUTTON(lastrow, $
                        VALUE = ' First ', $
                        UVALUE = 'VIEWSPEC_FIRST_FILE')
w_viewscan_next = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Next ', $
                        UVALUE = 'VIEWSPEC_NEXT_FILE')
w_viewscan_next = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Prev ', $
                        UVALUE = 'VIEWSPEC_PREV_FILE')
w_viewscan_last = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Last ', $
                        UVALUE = 'VIEWSPEC_LAST_FILE')
w_viewscan_cancel = WIDGET_BUTTON(lastrow_0, $
                        VALUE = ' Done ', $
                        UVALUE = 'VIEWSPEC_CANCEL')

w_viewscan_base = WIDGET_BASE(w_viewscan_topbase, /COLUMN, /FRAME)

w_viewscan_label = WIDGET_LABEL(w_viewscan_base,VALUE='Scan Data from : ' + $
		strcompress(w_plotspec_array(3)),UVALUE='w_viewscan_label')

row0 = WIDGET_BASE(w_viewscan_base, /ROW,/FRAME)

w_viewscan_printplot = WIDGET_BUTTON(row0, $
                        VALUE = 'Print Plot', $
                        UVALUE = 'VIEWSPEC_PSPRINT')
;WIDGET_CONTROL,w_viewscan_printplot,SENSITIVE=0

w_viewscan_plotspec = WIDGET_BUTTON(row0, $
                        VALUE = 'Modify Plot', $
                        UVALUE = 'VIEWSPEC_NEW')
;WIDGET_CONTROL,w_viewscan_plotspec,SENSITIVE=0

w_viewscan_field = WIDGET_BUTTON(row0, $
                        VALUE = 'ASCII View', $
                        UVALUE = 'VIEWSPEC_FIELD')
;WIDGET_CONTROL,w_viewscan_field,SENSITIVE=0
w_viewscan_print = WIDGET_BUTTON(row0, $
                        VALUE = 'ASCII Print', $
                        UVALUE = 'VIEWSPEC_PRINT')
WIDGET_CONTROL,w_viewscan_print,SENSITIVE=0

row1 = WIDGET_BASE(w_viewscan_base, /ROW)

w_viewscan_format = CW_FIELD( row1,VALUE=scanData.code+scanData.format, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS= 1, $
      TITLE='1D Data Column Format: ', $
      XSIZE=8, $
      UVALUE='VIEWSPEC_FORMAT')

@vw2d.bm

  BASE2 = WIDGET_BASE(w_viewscan_base, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

if w_viewscan_id.maxno gt 1 then begin

str = '1D Scan # [ 1 -'+ strcompress(string(maxno)) +' ]:'
w_viewscan_label = WIDGET_LABEL(BASE2,VALUE=str)
w_viewscan_seqno = WIDGET_TEXT(BASE2,VALUE=strtrim(maxno,2), $
		EDITABLE=1, $
		UVALUE='VIEWSPEC_SEQNO', XSIZE = 8)

  BMPBTN10_first = WIDGET_BUTTON( BASE2,VALUE=BMP767, $
      UVALUE='VIEWSPEC_FIRST')

  BMPBTN7_next = WIDGET_BUTTON( BASE2,VALUE=BMP688, $
      UVALUE='VIEWSPEC_NEXT')

  BMPBTN9_prev = WIDGET_BUTTON( BASE2,VALUE=BMP686, $
      UVALUE='VIEWSPEC_PREV')

  BMPBTN11_last = WIDGET_BUTTON( BASE2,VALUE=BMP809, $
      UVALUE='VIEWSPEC_LAST')

; add seqno slider here
	w_viewscan_slider = WIDGET_SLIDER(BASE2, $
		MAX=w_viewscan_id.maxno, $
		MIN=1,UVALUE='VIEWSPEC_SLIDER')

end

;if dim eq 3 then begin
;	WIDGET_CONTROL,ROW0,SENSITIVE=0
;	WIDGET_CONTROL,ROW1,SENSITIVE=0
;	WIDGET_CONTROL,BASE2,SENSITIVE=0
;	r = dialog_message("Please use IMAGE2D / SB / PICK3D to view this file",/info)
;end


; set widget ids :
w_viewscan_ids = { $
	base: w_viewscan_topbase, $
	fileno: w_viewscan_fileno, $
	base1:	w_viewscan_base, $
	file: w_viewscan_label, $
	printplot: w_viewscan_printplot, $
	plotspec: w_viewscan_plotspec, $
	field: w_viewscan_field, $
;	ascii:	w_viewscan_ascii, $
	print:	w_viewscan_print, $
	slider:  0L, $
	seqno:  0L, $
	format:  w_viewscan_format $
	 }

if n_elements(w_viewscan_seqno) gt 0 then w_viewscan_ids.seqno = w_viewscan_seqno
if n_elements(w_viewscan_slider) gt 0 then w_viewscan_ids.slider = w_viewscan_slider

; Realize the widgets:
WIDGET_CONTROL, w_viewscan_topbase, /REALIZE, $ 
	TLB_SET_XOFFSET= 0, TLB_SET_YOFFSET= 0

; Hand off to the XMANAGER:
XMANAGER, 'w_viewscan', w_viewscan_topbase, GROUP_LEADER = GROUP, CLEANUP = 'w_viewscan_close'

END


; FORWARD_FUNCTION READ_SCAN,READ_SCAN_FIRST,READ_SCAN_REST


PRO scanimage_print,gD,test=test
	gData = *gD
	print,'scanno  : ',*gData.scanno
	print,'dim     : ',*gData.dim
	print,'num_pts : ',*gData.num_pts
	print,'cpt     : ',*gData.cpt
	print,'id_def  : ',*gData.id_def
	print,'pvname  : ',*gData.pv
	print,'labels  : ',*gData.labels
	print,'ts      : ',*gData.ts
	if *gData.dim eq 3 then begin
	help,*gData.pa3D
	help,*gData.da3D
	end
	if *gData.dim eq 2 then begin
	help,*gData.pa2D
	help,*gData.da2D
	end
	help,*gData.pa1D
	help,*gData.da1D
	
	if keyword_set(test) then begin
	num_pts = *gData.num_pts
	width = num_pts(0)
	height = num_pts(1)
	help,width,height
	da2D = *gData.da2D
	im = da2d(*,*,1)
	help,im
	tvscl, congrid(im,400,400),/NAN  ; IDL 5.1
	end

END

PRO scanimage_free,gD
	gData = *gD
	if ptr_valid(gData.scanno) then	ptr_free,gData.scanno
	if ptr_valid(gData.dim) then	ptr_free,gData.dim
	if ptr_valid(gData.num_pts) then	ptr_free,gData.num_pts
	if ptr_valid(gData.cpt) then	ptr_free,gData.cpt
	if ptr_valid(gData.id_def) then	ptr_free,gData.id_def
	if ptr_valid(gData.pv) then	ptr_free,gData.pv
	if ptr_valid(gData.labels) then	ptr_free,gData.labels
	if ptr_valid(gData.ts) then	ptr_free,gData.ts
	if ptr_valid(gData.pa1D) then	ptr_free,gData.pa1D
	if ptr_valid(gData.da1D) then	ptr_free,gData.da1D
	if ptr_valid(gData.pa2D) then	ptr_free,gData.pa2D
	if ptr_valid(gData.da2D) then	ptr_free,gData.da2D
	if ptr_valid(gData.pa3D) then	ptr_free,gData.pa3D
	if ptr_valid(gData.da3D) then	ptr_free,gData.da3D
	if ptr_valid(gD) then ptr_free,gD
END

PRO scanimage_cleanup
	help,/heap_variables
	heap_gc
END

PRO scanimage_alloc,filename,gD,scanno,pickDet=pickDet,header=header,lastDet=lastDet ,xdescs=xdescs,ydescs=ydescs,zdescs=zdescs

gData = { $
	scanno	: ptr_new(/allocate_heap), $  ;0L, $
	dim	: ptr_new(/allocate_heap), $  ;0, $
	num_pts	: ptr_new(/allocate_heap), $  ;[0,0,0,0,0], $
	cpt	: ptr_new(/allocate_heap), $  ;[0,0,0,0,0], $
	id_def	: ptr_new(/allocate_heap), $  ;intarr(89,5), $
	pv	: ptr_new(/allocate_heap), $  ;['','','','',''], $
	labels	: ptr_new(/allocate_heap), $  ;strarr(267,5), $
	ts	: ptr_new(/allocate_heap), $  ; time stamp array
	pa1D	: ptr_new(/allocate_heap), $
	da1D	: ptr_new(/allocate_heap), $
	pa2D	: ptr_new(/allocate_heap), $
	da2D	: ptr_new(/allocate_heap), $
	pa3D	: ptr_new(/allocate_heap), $
	da3D	: ptr_new(/allocate_heap), $
	pa4D	: ptr_new(/allocate_heap), $
	da4D	: ptr_new(/allocate_heap) $
	}
	gD = ptr_new(/allocate_heap)
	*gD = gData

	scanno = read_scan(filename,Scan,pickDet=pickDet,header=header,lastDet=lastDet)
	if scanno lt 0 then return

; get y_descs
	sscan_getDescs,Scan,xdescs,ydescs,zdescs

	*gData.scanno = scanno

	rix2DC, Scan, gData
	scanSee_free,Scan

;	scanimage_print,gD

END

;    DC_read.pro
;
;    if seq_no < =0 then the last scan is plotted
;


PRO scan_read,unit,seq_no,id,maxno,dim,scanH,pickDet=pickDet
; keyword pickDet is used for 3D scan
;     pickDet > 0 only the selected 3D detector array is returned
;     pickDet < 0 no 3D detector array is returned
; 		  i.e. da3D will be undefined
; add special treatment for scanH 2D and 3D  cases
;	seqno <= 0  populate initial data structure is desired
;	      > 0   bypass initial data structure population
;	id = -1     implies read from input file is desired
;          >= 0     implies no read is desired only 1D plot is desired
;	maxno       returns maxno of 1D scan
;	dim	    returns dim
;	scanH	    returns  indicator of presence of scanH
;
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus
COMMON CATCH1D_2D_COM,data_2d, gD

 new_pv = [scanData.pv, scanData.y_pv]
 old_yscan = scanData.y_scan
 old_id_def = realtime_id.def
 xdescs = ['','','',''] 

next_seq_no = seq_no + 1
 npd = scanData.num_det + 4

	scanno = -1
	DetMax = [npd-4, 1, 1]  ;scanData.lastDet

; alloc gD only if not exist yet
if id lt 0 then begin

	if n_elements(gD) then begin


  scanno = read_scan(scanData.trashcan,Scan,pickDet=pickDet) ;, lastDet=DetMax)
	
	if scanno lt 0 then return

; get y_descs
sscan_getDescs,Scan,xdescs,ydescs,zdescs

; loose the error check
; if read error with partially data try to plot it anyway
	if n_elements(Scan.scanno) lt 1 then begin   ; a new file is picked
	scanData.y_scan = 0
	scanData.y_seqno = 0
	scanData.scanno = 1 ;scanno
	catch1d_check_seqno
	st = ['Error: failed in reading '+scanData.trashcan, $
		'Use the ','', $
		'File->Open... to pick a new file']
	r = dialog_message(/error,st)
print,scanData.path
print,scanData.trashcan
	return
	end

	WIDGET_CONTROL,widget_ids.trashcan,SET_VALUE=scanData.trashcan
	rix2DC, Scan, *gD

	scanno = *(*gD).scanno
	dim = *(*gD).dim
	num_pts = *(*gD).num_pts
	cpt = *(*gD).cpt
	pv = *(*gD).pv
	labels = *(*gD).labels
	id_def = *(*gD).id_def
	pa1D = *(*gD).pa1D
	da1D = *(*gD).da1D
	pa2D = *(*gD).pa2D
	da2D = *(*gD).da2D
	pa3D = *(*gD).pa3D
	if n_elements(*(*gD).da3D) gt 0 then $
	da3D = *(*gD).da3D

	goto, populate
;print,'READ AGAIN'
	endif else begin

	scanimage_alloc,scanData.trashcan, gD, scanno,pickDet=pickDet,xdescs=xdescs,ydescs=ydescs,zdescs=zdescs

setDefaultLabels

;print,'ALLOC gD'
	if scanno lt 0 then return
	end
end  ; id >= 0

	scanno = *(*gD).scanno
	dim = *(*gD).dim

	num_pts = *(*gD).num_pts
	cpt = *(*gD).cpt
	pv = *(*gD).pv
	labels = *(*gD).labels
	id_def = *(*gD).id_def

	if dim gt 2 then begin
	if n_elements(*(*gD).pa3D) gt 0 then $
	pa3D = *(*gD).pa3D
	if n_elements(*(*gD).da3D) gt 0 then $
	da3D = *(*gD).da3D
	end
	if dim gt 1 then begin
	if n_elements(*(*gD).pa2D) gt 0 then $
	pa2D = *(*gD).pa2D
	if n_elements(*(*gD).da2D) gt 0 then $
	da2D = *(*gD).da2D
	end
	if n_elements(*(*gD).pa1D) gt 0 then $
	pa1D = *(*gD).pa1D
	if n_elements(*(*gD).da1D) gt 0 then $
	da1D = *(*gD).da1D

populate:

	ts = *(*gD).ts
	if id ge 2 then begin
	tp = ts(id-2)
	endif else tp = ts(n_elements(ts)-1)
	w_plotspec_array(4)=tp

	if dim eq 4 then begin
	pa4d = *(*gd).pa4d
	da4d = *(*gd).da4d
	end

	; 1D plot X can only display up to 4000 
	scanH = 0
	if strpos(pv(0),'scanH') ge 0 then  begin
		scanH = 1
		DetMax = [1,npd-4,1,1]
	end
	if scanH eq 0 then begin
	scanData.scanH = 0
	widget_control,widget_ids.scanH,set_droplist_select=0
	end

; make sure 3D scan have correct image_array returned  before continuation
if dim gt 1 and n_elements(da2D) gt 1 then begin
  sz = size(da2D)
  if sz(3) gt 70 then begin
	scanData.svers = 0
	scanData.new = 0
	widget_control,widget_ids.versID,set_droplist_select=0
	 DC_pickDetVersion,vers=0
  end
end

; check the header for the file if the the seq_no is set to le 0
IF seq_no LE 0 THEN BEGIN
	scanData.dim = dim
	realtime_id.def = id_def[0:scanData.npd-1,0]

	label = labels[*,0]
        scanData.req_npts = num_pts[0]
        scanData.act_npts = cpt[0]
	scanData.pv = pv[0]

	maxno = 1
	scanData.y_req_npts = 1
	if dim ge 2 then begin
	scanData.y_pv = pv[1]
	maxno = cpt[1]
	scanData.y_req_npts = num_pts[1]

	if dim eq 3 then begin
	   realtime_id.def = id_def[0:scanData.npd-1,1]
	   label = labels[*,1]
		maxno = cpt[2]
		scanData.req_npts = num_pts[1]
		scanData.y_req_npts = num_pts[2]
	        scanData.y_pv = pv[2]
	        scanData.pv = pv[1]
	end

	; check for existence of scanH

	if scanH then begin
	if dim eq 2 then begin
	   if scanData.scanH eq 0 then begin
	   realtime_id.def = id_def[0:scanData.npd-1,1]
	   label = labels[*,1]
           scanData.req_npts = num_pts[1]
           scanData.act_npts = cpt[1]
	   scanData.pv = pv[1]
		maxno=1
	   endif else begin
	   realtime_id.def = id_def[0:scanData.npd-1,0]
	   label = labels[*,0]
           scanData.req_npts = num_pts[0]
           scanData.act_npts = cpt[0]
	   scanData.pv = pv[0]
	   end
	end
	end

	end
	read_desc_engu,label 

	scanData.refno = 0 ;        scanData.refno = start_seqno
	scanData.p_def = realtime_id.def(0:3)

	w_viewscan_id.maxno = maxno
	w_viewscan_id.file = scanData.trashcan 
	w_viewscan_id.seqno = maxno
	w_plotspec_id.seqno = maxno
	unit = 1
	w_viewscan_id.unit = unit
END  ; end seqno le 0

;  bring up the panimage window for 2D

	; panimage only when read data is true
	IF id lt 0 THEN BEGIN
	if dim ge 2 and n_elements(da2D) gt 3 then begin
; reset to whole panimage 
 widget_control,widget_ids.pickimin,set_slider_max=scanData.req_npts-1
  if scanData.imin gt scanData.req_npts-1 then begin
	scanData.imin = scanData.req_npts-2
 	widget_control,widget_ids.pickimin,set_value=scanData.imin
  end

	scanData.scanno_2d = scanno

	if dim eq 4 then begin
	 realtime_id.def = id_def[0:scanData.npd-1,2]
	 if !d.name ne 'PS' then begin
	   WSET,widget_ids.plot_area
	   erase
		ydis=200+1.2*(!d.y_ch_size * 2)
	   xyouts,100,ydis,'4D Scan Detected !!',/device, $
		charsize=3,charthick=2
	   xyouts,100,150,'Use ViewData->SSCAN to view post scan data array',/device, $
		charsize=2,charthick=2
	 end
	 return  
	end

	update_2d_data,da2D,scanData.req_npts,scanData.y_req_npts
	end
	END

        if dim ge 2 then begin
	t_cpt = cpt[dim-1]
		maxno = cpt[dim-1]
		if seq_no gt 0 then t_cpt = seq_no  ; view old data
		maxno = t_cpt
                seqno = t_cpt
		scanData.y_seqno = t_cpt - 1
		scanData.scanno_2d = scanno 
		scanData.scanno = seqno + 1
;		scanData.y_scan = 1
	end
        if dim eq 1 then begin
                seqno = 1   
		maxno = 1
		scanData.pv = pv(0)
		scanData.scanno = seqno
		scanData.y_scan = 0
		scanData.y_seqno = 0
        end

; get Y positional vector 

if dim ge 2 then begin
	if seq_no le 0 then seq_no = maxno ; cpt[1]
	scanData.y_seqno = seq_no - 1
	if scanData.y_seqno lt 0 then  scanData.y_seqno=0
	yvalue=0.
if n_elements(pa1D) gt 0 then $
yvalue = pa1D(scanData.y_seqno,0)
end

if dim eq 1 and seq_no lt 0 then seq_no = 1
next_seq_no = seq_no + 1

; get last detector defined for each scan record

ndet = scanData.nd 	; ndet = ceil( total(id_def(4:npd-1,0)) )

	for j=0,dim-1 do begin
	det_id = id_def(4:scanData.npd-1,j)
	for i=0,n_elements(det_id)-1 do begin
	if det_id(i) gt 0 then scanData.lastDet(j) = i+1
	end
	end

ndet=scanData.lastDet(0)
if scanH and scanDATA.scanH eq 0 then ndet = scanData.lastDet(1)
if dim eq 3 then ndet = scanData.lastDet(1)

	*scanData.pa = make_array(scanData.req_npts,4,/double)
	*scanData.da = make_array(scanData.req_npts,ndet) 
; populate X positional vectors

act_npts = cpt[0]
if dim eq 3 then act_npts = num_pts[1]
if scanH and scanData.scanH eq 0 then act_npts = num_pts[1]
scanData.act_npts = act_npts 

if act_npts gt 0 then begin
for i=0,3 do begin
        if realtime_id.def[i] gt 0 then begin
	if dim gt 2 then begin 
	if n_elements(pa2D) gt 1 then begin
	  tsz = size(pa2D)
	  act_npts = tsz(1)
	  (*scanData.pa)[0:act_npts-1,i] = pa2D[0:act_npts-1,i] 
	end
	end
	if dim eq 2 then begin 
	   if scanH eq 0 then begin
	     tsz=size(pa2D)
	     act_npts = tsz(1)
	     (*scanData.pa)[0:act_npts-1,i] = pa2D[0:act_npts-1,i] 
	   endif else begin
	     if scanData.scanH eq 0 then begin
	     if n_elements(pa1d) gt 1 then begin
	     tsz=size(pa1D)
	     act_npts = tsz(1)
	     (*scanData.pa)[0:act_npts-1,i] = pa1D[0:act_npts-1,i] 
	     end
	     endif else begin
	     tsz=size(pa2D)
	     act_npts = tsz(1)
	     (*scanData.pa)[0:act_npts-1,i] = pa2D[0:act_npts-1,i] 
	     end
	   end
	end
	if dim eq 1 then $
	(*scanData.pa)[0:act_npts-1,i] = pa1D[0:act_npts-1,i] 
        end
end

; populate detector Y vectors

for i=0,ndet-1 do begin
        if realtime_id.def[4+i] gt 0 then begin
	if dim eq 2 then begin
		if scanData.scanH eq 0 and scanH then $
		(*scanData.da)[0:sz(2)-1,i] = da1D[0:sz(2)-1,i] else $
		(*scanData.da)[0:sz(1)-1,i] = da2D[0:sz(1)-1,scanData.y_seqno,i] 
	end
	if dim eq 3  then begin   ; one-level down from outter loop
		(*scanData.da)[0:num_pts(1)-1,i] = da2D[0:num_pts(1)-1,scanData.y_seqno,i]
	end
	if dim eq 1 then $
		(*scanData.da)[0:act_npts-1,i] = da1D[0:act_npts-1,i]
	end
end
end

; reset for 1D plot  
if scanH then scanData.lastDet(0) = ndet

if dim eq 2 and scanH and scanData.scanH eq 0 then seq_no=1

w_plotspec_id.seqno = seq_no
w_viewscan_id.seqno = seq_no

; populate read data into global arrays

	; get positioner goto_pv name

        for i=0,3 do begin
        if strtrim(x_names(i),2) ne '' then $
        w_plotspec_id.goto_pv(i) = strmid(x_names(i),0,strpos(x_names(i),'.'))
        end

	; get plot labels

	if scanData.debug eq 1 then $
	print,'Scan # ',seq_no, ' accessed.'
	scanData.scanno = seq_no 

	w_plotspec_array(0) = scanData.pv
	if dim eq 3 then w_plotspec_array(0) = pv(1)

	if dim ge 2 then w_plotspec_array(0) = w_plotspec_array(0) +' @ y('+strtrim(scanData.y_seqno,2)+')' +'='+strtrim(yvalue,2)
	if dim eq 2 and scanH and scanData.scanH eq 0 then $
		w_plotspec_array(0) = scanData.pv

	; x positional axis picked
	ix = w_plotspec_id.xcord
	w_plotspec_array(1) = x_descs(ix)

        if dim eq 3 then begin
        xdescs(ix) = labels(ix,dim-2)
        if labels(ix+npd,dim-2) ne '' then xdescs(ix) = xdescs(ix) +' ('+labels(ix+npd,dim-2)+')'
	w_plotspec_array(1) = xdescs(ix)
	end

 if id ge 0 then begin
       xdescs = scanData.xdescs
       ydescs = scanData.ydescs
       if dim gt 1 then zdescs = scanData.zdescs
 endif else begin

;	if dim eq 1 and total(da1D) eq 0. then return

	if scanH eq 0 then begin
		if dim lt 3 then resetDescs,0,labels,id_def,xdescs,ydescs $
		else resetDescs,1,labels,id_def,xdescs,ydescs
	endif else begin
	    if dim eq 2 and scanData.scanH then resetDescs,0,labels,id_def,xdescs,ydescs $
	    else  resetDescs,1,labels,id_def,xdescs,ydescs
	end

	scanData.xdescs = xdescs
	scanData.ydescs = ydescs
	scanData.zdescs = ydescs
end
	scanData.id_def = realtime_id.def
	setPlotLabels

	UPDATE_PLOT,scanData.lastPlot
	id = next_seq_no
	if dim eq 2 and scanH and scanData.scanH eq 0 then id = -1

; reset pvnames and scan record realtime parameters for scan mode (future scan) 

if id lt 0 or dim eq 1 then begin
	if strlen(new_pv[0]) gt 2 then scanData.pv = new_pv[0]
	if strlen(new_pv[1]) gt 2 then scanData.y_pv = new_pv[1]

	if scanData.pv ne '' then realtime_id.def = old_id_def
	scanData.y_scan = old_yscan
end

	scanData.readin_npts=scanData.act_npts
 
	if dim eq 3 and scanData.debug gt 0 then begin
help,pa1D,pa2D,pa3D
help,da1D,da2D,da3D
print,scanData.lastDet
	end

END


PRO catch1d_check_seqno,filename
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
;

if scanData.y_scan then begin
	w_plotspec_id.seqno = scanData.scanno ; scanno
	w_viewscan_id.maxno = scanData.scanno ; scanno
end
END


PRO resetDescs,id,labels,id_def,xdescs,ydescs
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus
; set descs for the picked id axis
; id = 0 for normal 1D/2D
; id = 1 for 2D/3D with scanH
;
	for i=0,3 do begin
	    if id_def[i,id] eq 1 then begin
		if labels[i,id] ne '' then begin
			x_names[i] = labels[i,id]
			x_descs[i] = labels[89+i,id]
			x_engus[i] = labels[89+89+i,id]
		end
;	    print,i,x_names[i],',', x_descs[i],',', x_engus[i]
	    end
	end	
	for i=4,88 do begin
	    if id_def[i,id] eq 1 then begin
		if labels[i,id] ne '' then begin
			y_names[i-4] = labels[i,id]
			y_descs[i-4] = labels[89+i,id]
			y_engus[i-4] = labels[89+89+i,id]
		end
;	    print,i,y_names[i-4],',', y_descs[i-4],',', y_engus[i-4]
	    end
	end	

	for i=0,3 do begin
	    if id_def[i,id] eq 1 then begin
		if labels[i+89,id] ne '' then xdescs[i] = labels[89+i,id] $
		else xdescs[i] = labels[i,id]
		if labels[89+89+i,id] ne '' then $
			xdescs[i] = xdescs[i]+' ('+labels[89+89+i,id] +')'
	    end
;	    print,i,xdescs[i]
	end	
	ydescs = y_descs
	for i=4,88 do begin
	    if id_def[i,id] eq 1 then begin
		if labels[i+89,id] ne '' then ydescs[i-4] = labels[89+i,id] $
		else ydescs[i-4] = labels[i,id]
;	    print,i,ydescs[i-4]
	    end
	end
END

; 
; save data only for curr scan record 
; 
PRO mere_data_dump, unit
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids

str = '; 2D SCAN #: '
if scanData.dim eq 3 then str = '; 3D SCAN #: '
if scanData.y_seqno gt 0 or scanData.y_scan gt 0 then begin
printf,unit,str,scanData.scanno_2d,",      Y INDEX #:",scanData.y_seqno+1
        end

printf,unit,"; 1D SCAN #: ",scanData.scanno
save_data_subset_dump, unit 
printf,unit,' '

END
; 
; save ascii file of curr scan record 
; 
PRO shortreport_data_dump, unit
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids

str = '; 2D SCAN #: '
if scanData.dim eq 3 then str = '; 3D SCAN #: '
printf,unit,"; VERSION: ",scanData.version,' ',scanData.release
if scanData.y_seqno gt 0 or scanData.y_scan gt 0 then begin
printf,unit,str,scanData.scanno_2d,",      Y INDEX #:",scanData.y_seqno+1
        end

printf,unit,"; 1D SCAN #: ",scanData.scanno
printf,unit,"; SCAN Record Name: ",scanData.pv



printf,unit,'; '
printf,unit,"; Saved in:   ",scanData.path+w_plotspec_array(3)
printf,unit,"; Time Stamp: ",w_plotspec_array(4)
printf,unit,"; Comment:    ",w_plotspec_array(5)
printf,unit,'; '
printf,unit,'; SCAN Data:'
printf,unit,'; '

save_data_subset_dump, unit 

printf,unit,' '

END

; 
; save ascii file of a scan record 
; 
PRO summary_report_dump,filename,outfile,i_start,i_stop,header 
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

;tempname = strcompress(w_plotspec_array(3),/remove_all)+'.rep'
tempname=outfile

CATCH,error_status
if error_status ne 0 then begin
;help,!error_state,/st
	scanData.outpath = scanData.home+!os.file_sep
	print,'Try New Outpath:',scanData.outpath
	return
end
openw,unit2,tempname,/get_lun 
unit1 = 1
for i=i_start, i_stop do begin
	ip = i
	scan_read,unit1,ip,ip+1	
	if header eq 0 then report_data_dump,unit2
	if header eq 1 then shortreport_data_dump,unit2
	if header eq 2 then mere_data_dump,unit2
end

end_loop:
	free_lun,unit2

END


PRO report_data_dump,unit
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids


str = '; 2D SCAN #: '
if scanData.dim eq 3 then str = '; 3D SCAN #: '
printf,unit,"; VERSION: ",scanData.version,' ',scanData.release
if scanData.y_seqno gt 0 or scanData.y_scan gt 0 then begin
printf,unit,str,scanData.scanno_2d,",      Y INDEX #:",scanData.y_seqno+1
        end

printf,unit,"; 1D SCAN #: ",w_plotspec_id.seqno 
printf,unit,"; SCAN Record Name: ",scanData.pv


printf,unit,'; '
printf,unit,"; PLOT SPECIFICATIONS"
printf,unit,'; '
printf,unit,"; Title:      ",w_plotspec_array(0)
printf,unit,"; X Label:    ",w_plotspec_array(1)
printf,unit,"; Y Label:    ",w_plotspec_array(2)
printf,unit,"; Saved in:   ",scanData.path+w_plotspec_array(3)
printf,unit,"; Time Stamp: ",w_plotspec_array(4)
printf,unit,"; Comment:    ",w_plotspec_array(5)

if w_plotspec_id.type eq 0 then printf,unit,"; Type:      Line"
if w_plotspec_id.type eq 1 then printf,unit,"; Type:      Point"
if w_plotspec_id.type eq 2 then printf,unit,"; Type:      Line/Point"
if w_plotspec_id.log  eq 0 then printf,unit,"; Y Scale:   Linear"
if w_plotspec_id.log  eq 1 then printf,unit,"; Y Scale:   Log"
if w_plotspec_id.errbars eq 0 then printf,unit,"; Errbars:   Off"
if w_plotspec_id.errbars eq 1 then printf,unit,"; Errbars:   On"
printf,unit,'; Realtime: itime=',w_plotspec_id.itime, ',  dtime=',w_plotspec_id.dtime
printf,unit,'; Plot Vs Position Array # ',w_plotspec_id.xcord + 1

printf,unit,'; '
printf,unit,'; SCAN Data:'
printf,unit,'; '

 save_data_subset_dump, unit

printf,unit,' '

END


PRO view1d_summary_generate_DCreport
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON view1d_summary_block, view1d_summary_ids, view1d_summary_id

	WIDGET_CONTROL, view1d_summary_ids.format, GET_VALUE=format
	format = strcompress(format(0),/remove_all)
	scanData.code = strmid(format,0,1)
	scanData.format = strmid(format,1,10)

	WIDGET_CONTROL, view1d_summary_ids.view, SENSITIVE = 0 
	WIDGET_CONTROL, view1d_summary_ids.print, SENSITIVE = 0 
	WIDGET_CONTROL,view1d_summary_ids.start, GET_VALUE=i_start
	WIDGET_CONTROL,view1d_summary_ids.stop, GET_VALUE=i_stop
	if i_stop lt i_start then i_stop=i_start
	view1d_summary_id.start = i_start
	view1d_summary_id.stop = i_stop

	WIDGET_CONTROL,view1d_summary_ids.file, GET_VALUE=file
	filename=strcompress(file(0),/remove_all)
	found = findfile(filename)
if found(0) ne '' then  begin	
	view1d_summary_id.file = filename
	WIDGET_CONTROL,view1d_summary_ids.outfile, GET_VALUE=file
	view1d_summary_id.outfile=strcompress(file(0),/remove_all)

	save_outfile = scanData.outpath+view1d_summary_id.outfile

; save as one big file
    if view1d_summary_id.separate eq 0 then begin
		w_plotspec_id.mode = 1
		summary_report_dump,filename,save_outfile,i_start,i_stop,view1d_summary_id.header
		w_plotspec_id.mode = 0

	if scanData.debug eq 1 then $
		print, 'Report file: ', save_outfile,' created!'
; save as separate files
     endif else begin
	str = '0000'
	len0 = 4 
	w_plotspec_id.mode = 1
	for i=i_start,i_stop do begin
	sss = str
	st = strtrim(i,2)
	len = strlen(st)
	strput,sss,st,len0-len
		save_outfile=scanData.outpath+w_plotspec_array(3)+'.'+sss
		summary_report_dump,filename,save_outfile,i,i,view1d_summary_id.header
	if scanData.debug eq 1 then print,save_outfile
	end
	w_plotspec_id.mode = 0
     end

endif else w_warningtext,'Error:  Data file " '+filename+' " not found!'
;	WIDGET_CONTROL, view1d_summary_ids.base , /DESTROY
view_print:
	WIDGET_CONTROL, view1d_summary_ids.view, SENSITIVE = 1 
	WIDGET_CONTROL, view1d_summary_ids.print, SENSITIVE = 1 

END

PRO view1d_summary_setup_Event, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON view1d_summary_block, view1d_summary_ids, view1d_summary_id

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'summary_header': BEGIN
      CASE Event.Value OF
      0: view1d_summary_id.header = 0
      1: view1d_summary_id.header = 1
      2: view1d_summary_id.header = 2
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  'summary_format': BEGIN
      Print, 'Event for output format'
	WIDGET_CONTROL, view1d_summary_ids.format, GET_VALUE=format
	format = strcompress(format(0),/remove_all)
	scanData.code = strmid(format,0,1)
	scanData.format = strmid(format,1,10)
        ret = strpos('defgDEFG',scanData.code)
       if ret eq -1 then w_warningtext,'Error:   illegal format entered !!!'
	END
  'summary_file': BEGIN
      Print, 'Event for Filename'
	WIDGET_CONTROL, view1d_summary_ids.file, GET_VALUE=file 
	filename=strcompress(file(0),/remove_all)
	found = findfile(filename)
	if found(0) ne '' then begin	
	view1d_summary_id.file = filename

scan_read,unit,-1,-1,maxno ; scan_read_all,unit,maxno
view1d_summary_id.start = maxno
view1d_summary_id.stop = maxno
if maxno eq 1 then begin
	scanData.y_scan=0
	scanData.y_seqno=0
end

	WIDGET_CONTROL,view1d_summary_ids.start, $ 
		SET_VALUE=strtrim(view1d_summary_id.start,2)
	WIDGET_CONTROL,view1d_summary_ids.stop, $
		SET_VALUE=strtrim(view1d_summary_id.stop,2)
	report_setup
	outfile = view1d_summary_id.outfile
	WIDGET_CONTROL, view1d_summary_ids.outfile, SET_VALUE= outfile
	end
      END
  'summary_start': BEGIN
	WIDGET_CONTROL,view1d_summary_ids.start, GET_VALUE=i_start
	if i_start gt 0 and i_start le w_viewscan_id.maxno then begin
	view1d_summary_id.start = i_start
	report_setup
	WIDGET_CONTROL, view1d_summary_ids.outfile, SET_VALUE= view1d_summary_id.outfile
	endif else w_warningtext,['Error: can not exceed '+ string(w_viewscan_id.maxno) ]
      END
  'summary_end': BEGIN
	WIDGET_CONTROL,view1d_summary_ids.stop, GET_VALUE=i_stop
	if stop gt 0 and stop le w_viewscan_id.maxno then begin
	view1d_summary_id.stop = i_stop
	report_setup
	WIDGET_CONTROL, view1d_summary_ids.outfile, SET_VALUE= view1d_summary_id.outfile
	endif else begin
	  w_warningtext,['Error: can not exceed '+ string(w_viewscan_id.maxno),$
		'       Reset to '+string(w_viewscan_id.maxno) ]
          WIDGET_CONTROL,view1d_summary_ids.stop, SET_VALUE=w_viewscan_id.maxno
	end
      END
  'summary_separate': BEGIN
	view1d_summary_id.separate = Event.Index
	report_setup
	WIDGET_CONTROL, view1d_summary_ids.outfile, SET_VALUE= view1d_summary_id.outfile
	END
  'summary_ok': BEGIN
	view1d_summary_generate_DCreport
      END
  'summary_view': BEGIN
	WIDGET_CONTROL,view1d_summary_ids.outfile, GET_VALUE=file
	filename=strcompress(file(0),/remove_all)

	outfile = scanData.outpath + filename
	found = findfile(outfile,count=ct)
	if ct eq 0 then view1d_summary_generate_DCreport
	  xdisplayfile,outfile,width=110,GROUP=event.top 
	END

  'summary_print': BEGIN

	WIDGET_CONTROL,view1d_summary_ids.outfile, GET_VALUE=file
	filename=strcompress(file(0),/remove_all)

	outfile = scanData.outpath + filename
	found = findfile(outfile,count=ct)
	if ct gt 0 then begin
		if OS_SYSTEM.os_family eq 'unix' then $
		spawn,[OS_SYSTEM.prt, OS_SYSTEM.printer, '-r',outfile], /noshell else $
		spawn,[OS_SYSTEM.prt, OS_SYSTEM.printer,outfile]
		return
	endif else $
		w_warningtext,['Error:','    '+outfile+ '  not found!','      Generate it first.']
	END
  'summary_cancel': BEGIN
	WIDGET_CONTROL, view1d_summary_ids.base , /DESTROY
      END
  ENDCASE
END

PRO report_setup
COMMON view1d_summary_block, view1d_summary_ids, view1d_summary_id

str = '0000'
len0 = strlen(str)
sss = str
st = strtrim(view1d_summary_id.start,2)
len = strlen(st)
strput,sss,st,len0-len

filenamepath, view1d_summary_id.file, file, path
view1d_summary_id.outfile = file+'.'+sss

if view1d_summary_id.separate then return
if view1d_summary_id.stop gt view1d_summary_id.start then begin
eee = str
st = strtrim(view1d_summary_id.stop,2)
len = strlen(st)
strput,eee,st,len0-len
view1d_summary_id.outfile = view1d_summary_id.outfile+ '_'+eee
end

END

PRO view1d_summary_setup, GROUP=Group
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON view1d_summary_block, view1d_summary_ids, view1d_summary_id
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id


if XRegistered('view1d_summary_setup') ne 0 then begin 
	WIDGET_CONTROL,view1d_summary_ids.base,/DESTROY
	end

if XRegistered('w_viewscan') ne 0 then begin
	WIDGET_CONTROL,w_viewscan_ids.base,/DESTROY
	end

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

view1d_summary_id = { $
	start: 1, $
	stop: 1, $
	header: 0, $
	separate: 0, $
	outfile: w_plotspec_array(3)+'.rep',  $
	file: scanData.trashcan  $
	}

scan_read,unit,-1,-1,maxno ; scan_read_all,unit,maxno
view1d_summary_id.start = maxno
view1d_summary_id.stop = maxno
if maxno eq 1 then begin
	scanData.y_scan=0
	scanData.y_seqno=0
end

  report_setup

  view1d_summary_base = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
 TITLE = 'Report ...', $
      UVALUE='view1d_summary_base')

  BASE2 = WIDGET_BASE(view1d_summary_base, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL3', $
      VALUE='SUMMARY ASCII REPORT')

Btns220 = [ 'Full', 'Abbreviated', 'None' ]
  summary_header = CW_BGROUP( BASE2, Btns220, $
      ROW=1, $
      EXCLUSIVE=1, $
      LABEL_LEFT='Header options:', $
      UVALUE='summary_header')
WIDGET_CONTROL,summary_header,SET_VALUE=0

  summary_format = CW_FIELD( BASE2,VALUE=scanData.code+scanData.format, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS= 1, $
      TITLE='Ouput Data Format: ', $
      XSIZE=8, $
      UVALUE='summary_format')

  summary_file = CW_FIELD( BASE2,VALUE=view1d_summary_id.file, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS= 1, $
      TITLE='Data file name: ', $
      XSIZE=60, $
      UVALUE='summary_file')

  FieldVal388 = strtrim(view1d_summary_id.start,2)
  summary_start = CW_FIELD( BASE2,VALUE=FieldVal388, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS= 1, $
      TITLE='Start scan #    ', $
      UVALUE='summary_start')

  FieldVal465 = strtrim(view1d_summary_id.stop,2)
  summary_end = CW_FIELD( BASE2,VALUE=FieldVal465, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS= 1, $
      TITLE='End scan #      ', $
      UVALUE='summary_end')

  summary_separate = WIDGET_DROPLIST(BASE2, VALUE=['No', 'Yes'], $
        UVALUE='summary_separate',TITLE='Save Selected Scans as Separate ASCII Files')
  WIDGET_CONTROL,summary_separate,set_droplist_select = 0

  BASE112 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE112')

;  label = WIDGET_LABEL(BASE112,VALUE='Out Report File: ')
;  summary_outfile = WIDGET_LABEL(BASE112,VALUE=view1d_summary_id.outfile)

  summary_outfile = CW_FIELD( BASE2,VALUE=view1d_summary_id.outfile, $
      ROW=1, XSIZE=60, $
      TITLE='Output file name: ', $
      UVALUE='summary_outfile')

  BASE12 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE12')

  summary_ok = WIDGET_BUTTON( BASE12, $
      UVALUE='summary_ok', $
      VALUE='Generate Report')

  summary_view = WIDGET_BUTTON( BASE12, $
      UVALUE='summary_view', $
      VALUE='View Report')

  summary_print = WIDGET_BUTTON( BASE12, $
      UVALUE='summary_print', $
      VALUE='Print Report')

  summary_cancel = WIDGET_BUTTON( BASE12, $
      UVALUE='summary_cancel', $
      VALUE='Done')


; set widget ids:

	widget_ids.summary_base = view1d_summary_base

view1d_summary_ids = { $
	base: view1d_summary_base, $
	format: summary_format, $
	file: summary_file, $
	outfile: summary_outfile, $
	view: summary_view, $
	print: summary_print, $
	start: summary_start, $
	stop: summary_end $
	}

  WIDGET_CONTROL, view1d_summary_base, /REALIZE

  XMANAGER, 'view1d_summary_setup', view1d_summary_base, GROUP_LEADER = GROUP 
END
PRO getPiDiArray
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
  COMMON CATCH1D_2D_COM, data_2d, gD

	PI = scanData.pv + '.'+ scanData.PI+'RA'
	ln = cagetArray(PI,pa,max=scanData.req_npts)	
	
	*scanData.pa = pa

	t_DI = scanData.DI
	if scanData.new then t_DI = t_DI(15:84)
	DI = scanData.pv + '.'+ t_DI+'DA'
	npd = max(where(realtime_id.def gt 0)) + 1
	nd = npd-4

	DI = DI(0:nd-1)
	ln = cagetArray(DI,da,max=scanData.req_npts,/float)	

	*scanData.da = da
	
	if scanData.y_scan eq 0 then begin
	  w_plotspec_array(0)=scanData.pv
	  scanData.scanno=1
	endif else begin
	  catch1d_process2Ddata
	  w_plotspec_array(0)=scanData.pv+' @ y('+strtrim(scanData.scanno-1,2)+')'
	end
	w_plotspec_id.seqno = scanData.scanno
	UPDATE_PLOT,scanData.lastPlot

	if scanData.dim gt 1 then begin

;print,scanData.y_seqno,w_viewscan_id.maxno,scanData.scanno
;help,da,data_2d.image

if scanData.y_seqno lt scanData.y_req_npts and scanData.scanno gt 0 then begin 
	if n_elements(data_2d) eq 0 then begin
	scan_read,1,-1,-1,maxno,pickDet=-1
	make_2d_data,data_2d,scanData.req_npts,scanData.y_req_npts,*(*gD).da2D
	end
catch,error_status
if error_status ne 0 then begin
	scan_read,1,-1,-1,maxno,pickDet=-1
	make_2d_data,data_2d,scanData.req_npts,scanData.y_req_npts,*(*gD).da2D
end
	for i=0,nd-1 do begin
	if realtime_id.def(i+4) gt 0 then $
	data_2d.image(*,scanData.scanno-1,i) = da(*,i)
	end
end

	if scanData.scanno gt 1 then $
	update_2d_data,data_2d.image,scanData.req_npts,scanData.y_req_npts
	end

END


PRO make_2d_data,data_2d,xdim,ydim,da2D
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

	nd = max(where(realtime_id.def gt 0)) + 1

	if scanData.scanno gt 1 and scanData.y_seqno lt scanData.y_req_npts then begin
	  data_2d = { $
		nd : nd-4, $
		image : da2D $
	  }
	endif else begin

	  data_2d = { $
		nd : nd-4, $
		image : make_array(xdim,ydim,nd-4) $
	  }
	end
END

PRO update_2d_data,da2D,xdim,ydim
COMMON CATCH1D_COM, widget_ids, scanData

	if scanData.sel_id eq 9  then  begin
	if widget_ids.panwin ne -1 then wdelete,widget_ids.panwin
	widget_ids.panwin = -1
		return
	end

	sz = size(da2D)
	y_last = sz(2) - 1

	xdim = sz(1)
	ydim = sz(2)

	scanData.sel_changed = 1

	if !d.n_colors gt 256 then device,decomposed=0
	if scanData.image gt 2 then catch1d_win_2D_update2,y_last else $
	catch1d_win_2D_update   
	if !d.n_colors gt 256 then device,decomposed=1
END



PRO catch1d_process2Ddata
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

        if scanData.y_scan eq 1 then begin


; process the record at the end of scan

if scanData.reverse then begin
        w_plotspec_id.seqno = scanData.y_seqno
end 

str = '2D SCAN #'
if scanData.dim eq 3 then str='3D SCAN #'
if scanData.dim eq 4 then str='4D SCAN #'
if scanData.debug eq 1 then $
print,str,scanData.scanno_2d, '  Y SCAN #',scanData.y_seqno
        end

	if scanData.debug eq 1 then $
	print,'1D SCAN #',scanData.scanno

END

PRO catch1d_win_2D_sel_data,id_def,sel_list,detname,image_subarray
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
  COMMON CATCH1D_2D_COM, data_2d, gD
; return id_def, detname, image_subarray

	npd = scanData.npd

	sz = size(image_subarray)
	last = sz(3)  ;scanData.lastDet[0]
	DI = scanData.DI
	if scanData.svers then DI = DI(15:scanData.num_det-1)

	if scanData.new and scanData.y_scan then $
		DI = scanData.DI(15:scanData.num_det-1)

	; check for read or scan case
	id_def = realtime_id.def(4:scanData.npd-1)
	if scanData.y_scan eq 0 then begin
		dim =*(*gD).dim
                id_def = (*(*gD).id_def)(4:scanData.npd-1,dim-2)
	end

	; all selected
	if scanData.sel_id eq 8 then begin
		nd = scanData.nd  ;lastDet(0)
		sel_list = indgen(nd)
		detname = DI(0:sz(3)-1)
		NUM_SEL = sz(3)   ;scanData.lastDet[2] ;  85
		if scanData.imin eq 0 then return
	endif else begin
	; sublist
		picklist = where(scanData.sel_list gt 0)
		if scanData.sel_id eq 0 then picklist = [0,picklist]
		if scanData.new then begin
			 if scanData.sel_id eq 1 or scanData.sel_id eq 10 then $
			 picklist = [0,picklist]
		end
		sel_list = scanData.sel_list(picklist)
		id_def = id_def(sel_list)
		detname = DI(sel_list)
		NUM_SEL = n_elements(sel_list)
		if NUM_SEL gt sz(3) then NUM_SEL= sz(3)
	end

	new_subarray = make_array(sz(1)-scanData.imin,sz(2),NUM_SEL)
	wd = sz(1)- 1
	ht = sz(2) - 1
	if sz(0) eq 3 then $
	for i=0,NUM_SEL-1 do begin
		idd = sel_list(i)
		if idd lt sz(3) then $
		new_subarray(*,*,i) = image_subarray(scanData.imin:wd,*,idd) 
	end
	image_subarray = new_subarray

END

PRO catch1d_win_2D_update
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
  COMMON CATCH1D_2D_COM, data_2d, gD

; this function call panimage will create new window

str = '2D SCAN #'
if scanData.dim eq 3 then str='3D SCAN #'
if scanData.dim eq 4 then str='4D SCAN #'

	title=str+strtrim(scanData.scanno_2d,2) + ' : '+ $
		scanData.sublist(scanData.sel_id,scanData.new)

if scanData.fastscan then image_subarray = *(*gD).da2D else begin
if realtime_id.ind gt 0 and size(data_2d,/type) eq 8 then image_subarray = data_2d.image else $ 
	image_subarray = *(*gD).da2D
end

	if scanData.sel_id eq 9 then begin
		catch,status_error
		if status_error ne 0 then begin
		wset,widget_ids.plot_area
		return	
		end
		wdelete,widget_ids.panwin
		wset,widget_ids.plot_area
		return
	end 
	catch1d_win_2D_sel_data,id_def,sel_list,detname,image_subarray

	old_win = widget_ids.plot_area
	new_win = widget_ids.panwin
	factor=2
	if scanData.image eq 1 then factor=1

if !d.name ne 'WIN' then begin
	catch,status_error
	if status_error ne 0 then begin
		p=[0,0]
		goto, drawnewwin
	end
	if new_win gt 0 then begin
	wset,new_win
	device,get_window_position=p
	p(0) = p(0)-5
	p(1) = p(1)+30
	end
drawnewwin:
	panimage,image_subarray,id_def,factor,detnm=detname,isel=sel_list, $
		new_win=new_win,title=title,numd=10,xpos=p(0),ypos=p(1)
endif else $
	panimage,image_subarray,id_def,factor,detnm=detname,isel=sel_list, $
		new_win=new_win,title=title,numd=10
	widget_ids.panwin = new_win
	wset,old_win

END



;
; update only selected 2D detector
;
PRO catch1d_win_2D_update2,y_seqno
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
  COMMON CATCH1D_2D_COM, data_2d, gD

; update the image plot

;loadct, 39

npts = scanData.act_npts-1
if n_params() eq 0 then y_seqno = scanData.y_seqno

if y_seqno lt 0 then return

	sel = scanData.image - 3
	DI = scanData.DI
	if scanData.new * scanData.svers then begin
		DI= DI(15:scanData.num_det-1) 
		if sel ge scanData.nd then begin
;			r= dialog_message('Allow 70 detectors only')
			return
		end
	end
	title=DI(sel)
	
if scanData.fastscan then da2D = *(*gD).da2D else begin
if realtime_id.ind gt 0 and size(data_2d,/type) eq 8 then da2D = data_2d.image else $ 
	da2D = *(*gD).da2D
end

	sz = size(da2D)
	if sel ge sz(3) then return  ; case of detector not defined
;	im = da2D(*,0:y_seqno,sel)
	im = da2D(scanData.imin:sz(1)-1,0:y_seqno,sel)

	old_win = widget_ids.plot_area

p=[0,0]
CATCH,error_status
if error_status lt 0 then begin
	if !error_state.name eq 'IDL_M_WINDOW_CLOSED' then begin
;	help,!error_state,/st
	widget_ids.panwin = -1
	end
end
if widget_ids.panwin ne -1 then begin
	wset,widget_ids.panwin
	if !d.name ne 'WIN' then begin
	device,get_window_position=p
	p(0) = p(0)-5
	p(1) = p(1)+30
	end
	wdelete,widget_ids.panwin
end
	window,/free, xsize = 200, ysize=200, title=title ,xpos=p(0),ypos=p(1)
	widget_ids.panwin = !d.window

        if strpos(!version.release,'5.0') eq 0 then $
	TVSCL,congrid(im, 200, 200) else $
	TVSCL,congrid(im, 200, 200) ,/NAN   ;  IDL 5.1

	wset,old_win
;	scanData.last_line = npts
END


PRO show_cross,x,y,d_id,s_id
if n_params() lt 4 then begin
	print,'Usage: show_cross,x,y,d_wid,s_wid
	print,'       x, y - specify cross hair coordinate
	print,'       d_win  - specify tv image window
	print,'       s_win  - saved virtual image window        
	return
	end
CATCH,error_status
if error_status eq -324 then begin
	print,!err_string
	print,'Invalid window id : ', s_id
	return
	end
	WSET,s_id
	width = !d.x_size
	height = !d.y_size
	WSET,d_id
	xa = [0,width-1]
	ya = [y,y]
	plots,xa,ya,/device
	xa = [x,x]
	ya = [0,height-1]
	plots,xa,ya,/device
END

PRO hide_cross,x,y,d_id,s_id
if n_params() lt 4 then begin
	print,'Usage: hide_cros,x,y,d_wid,s_wid
	print,'       x, y - specify cross hair coordinate
	print,'       d_win  - specify tv image window
	print,'       s_win  - saved virtual image window        
	return
	end
CATCH,error_status
if error_status eq -324 then begin
	print,!err_string
	print,'Invalid window id : ', s_id
	return
	end
	WSET,s_id
	width = !d.x_size
	height = !d.y_size
	WSET,d_id
if x ge 0 and x lt width then $
 	device,copy=[x,0,1,height,x,0,s_id]
if y ge 0 and y lt height then $
 	device,copy=[0,y,width,1,0,y,s_id]
END

PRO update_pixmap,wid
	o_wid = !d.window
	if !d.n_colors eq 16777216 then	channel=1 else channel=0
	data = TVRD(TRUE=channel)
	WSET,wid
	TV,data,TRUE=channel
	WSET,o_wid
END

PRO create_pixmap,wid,data=data,xp=xp,yp=yp,width=width,height=height
if n_params() lt 1 then begin
	print,'Usage: create_pixmap,wid 
	print,'       output - wid , saved virtual image window id
	print,'       keyword - xp,yp, width,height
	print,'Save the whole TV window to a new virtual window
	print,'         if keyword is used all four of them must be specified  
	return
	end


	if !d.n_colors eq 16777216 then	data = TVRD(TRUE=1) else $
	data = TVRD(TRUE=0)

	if keyword_set(xp) and keyword_set(yp) and keyword_set(width) $
		 and keyword_set(height) then begin
		if !d.n_colors eq 16777216 then $ 
		newdata = data(0:2, xp:xp+width-1, yp:yp+height-1) else $
		newdata = data(xp:xp+width-1, yp:yp+height-1)
		data = newdata
		end

	ss = size(data)
	if ss(0) eq 2 then begin
		xs = ss(1)
		ys = ss(2)
		channel = 0
		end
	if ss(0) eq 3 and ss(1) eq 3 then begin
		xs = ss(2)
		ys = ss(3)
		channel = 1
		end
	if !d.n_colors eq 16777216 then	$
	print,'CREATE PIXMAP: Array(3,',strtrim(xs,2),',',strtrim(ys,2),')'  else $
	print,'CREATE PIXMAP: Array(',strtrim(xs,2),',',strtrim(ys,2),')'

	window,/free,/pixmap, xsize=xs, ysize=ys
	wid= !d.window
	TV,data,TRUE=channel

END

;
; plot y distributions vs values
;
;    xin: the input index number associated with the TV area
;
PRO catch2d_ydist,xin, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

if catch2d_file.x_act_npts lt 1 or catch2d_file.y_act_npts lt 1 then return
x_size = 3 > catch2d_file.x_act_npts
y_size = 3 > catch2d_file.y_act_npts

	x = xin

if x lt 0 or x ge x_size then begin
	st = ['Error:  X index out of range for image data.', $
		'        Valid X index range : [0 , '+strtrim(x_size-1,2)+']' $
		]
	w_warningtext,st,60,5,'VW2D Messages' 
	return
end

	WIDGET_CONTROL,widget_ids.y_min,GET_VALUE=y_min
	WIDGET_CONTROL,widget_ids.y_max,GET_VALUE=y_max
	WIDGET_CONTROL,widget_ids.x_min,GET_VALUE=x_min
	WIDGET_CONTROL,widget_ids.x_max,GET_VALUE=x_max

	if x_max gt x_size then x_max = x_size - 1

	if x lt x_min or x gt x_max then begin
		st = ['Error:  x index out of range for TV ydist.', $
			'        Valid x index range : ['+ strtrim(x_min,1) $
			+' , '+strtrim(x_max,2)+']' $
			]
		w_warningtext,st,60,5,'VW2D Messages' 
		return
	end

	y_min = fix(y_min)
	y_max = fix(y_max) - 1

	if y_min lt 0 then y_min = 0
	if y_max ge y_size then y_max = y_size - 1

	y_vec = image[x, y_min:y_max]
	xv = catch2d_file.xarr(x)
	title = 'At X(' + strtrim(x,2) + ') = ' + strtrim(xv,2)

	ay = catch2d_file.yarr(y_min:y_max)

; call plot1d resizable window 

	WIDGET_CONTROL,catch2d_file.yprof,BAD=bad,/DESTROY
	no = n_elements(y_vec)
	plot1d,ay,transpose(y_vec),id_tlb,windraw,GROUP=Event.top, $
		/cleanup, $
		wtitle='YZ Profile',xtitle='Y (Values)', ytitle='Z - VAL', $
		title=title
	catch2d_file.yprof =id_tlb 
	catch2d_file.yzdraw = windraw
	WIDGET_CONTROL,catch2d_file.yprof, $
		TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 450

widget_ids.x2 = !x
widget_ids.y2 = !y

if !d.name eq OS_SYSTEM.device then WSET,widget_ids.plot2d_area

END

;
; plot x distributions vs values
;
;    yin: the input y index number associated with the TV area
;
PRO catch2d_xdist,yin, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

if catch2d_file.x_act_npts lt 1 or catch2d_file.y_act_npts lt 1 then return
x_size = 3 > catch2d_file.x_act_npts
y_size = 3 > catch2d_file.y_act_npts

	y = yin

if y lt 0 or y ge y_size then begin
	st = ['Error:  Y index out of range for image data.', $
		'        Valid Y index range : [0 , '+strtrim(y_size-1,2)+']' $
		]
	w_warningtext,st,60,5,'VW2D Messages' 
	return
end

	WIDGET_CONTROL,widget_ids.y_min,GET_VALUE=y_min
	WIDGET_CONTROL,widget_ids.y_max,GET_VALUE=y_max
	WIDGET_CONTROL,widget_ids.x_min,GET_VALUE=x_min
	WIDGET_CONTROL,widget_ids.x_max,GET_VALUE=x_max

	if y_max gt y_size then y_max = y_size - 1

	if y lt y_min or y gt y_max then begin
		st = ['Error:  y index out of range for TV ydist.', $
			'        Valid y index range : ['+ strtrim(y_min,1) $
			+','+strtrim(y_max,2)+']' $
			]
		w_warningtext,st,60,5,'VW2D Messages' 
		return
	end

	x_min = fix(x_min)
	x_max = fix(x_max) - 1

	if x_min lt 0 then y_min = 0
	if x_max ge x_size then x_max = x_size - 1

	x_vec = image[ x_min:x_max,y]
	yv = catch2d_file.yarr(y)
	title = 'At Y(' + strtrim(y,2) + ') = ' + strtrim(yv,2)

	ax = catch2d_file.xarr(x_min:x_max)

; call plot1d resizable window 

	WIDGET_CONTROL,catch2d_file.xprof,BAD=bad,/DESTROY
	no = n_elements(x_vec)
	plot1d,ax,x_vec,id_tlb,windraw, GROUP=Event.top, $
		/cleanup, $
		wtitle='XZ Profile', xtitle='X (Values)', ytitle='Z - VAL', $
		title=title
	catch2d_file.xprof = id_tlb 
	catch2d_file.xzdraw = windraw
	WIDGET_CONTROL,catch2d_file.xprof, $
		TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 50
	
widget_ids.x1 = !x
widget_ids.y1 = !y

if !d.name eq OS_SYSTEM.device then WSET,widget_ids.plot2d_area
END

;
; plot x,y distributions
;
PRO catch2d_xydist, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

;wdelete,catch2d_file.xprof,catch2d_file.yprof

x_size = 3 > catch2d_file.x_act_npts
y_size = 3 > catch2d_file.y_act_npts
;print,'x_size, y size ',x_size,y_size
;print,'x_mag, y_mag ',catch2d_file.x_mag, catch2d_file.y_mag

WSET,widget_ids.plot2d_area
hide_cross,view_option.x,view_option.y,view_option.d_wid,view_option.s_wid
;cursor,x,y,0,/device
x = Event.x
y = Event.y
if x lt 0 or y lt 0 then return

; save cursor location
view_option.x = x
view_option.y = y
show_cross,x,y,view_option.d_wid,view_option.s_wid

;TVCRS,x,y


	; get x plot range

	WIDGET_CONTROL,widget_ids.x_min,GET_VALUE=x_min
	WIDGET_CONTROL,widget_ids.x_max,GET_VALUE=x_max
	x_min = fix(x_min) 
	x_max = fix(x_max) - 1
	if x_min lt 0 then x_min = 0
	if x_max ge x_size then x_max = x_size - 1

	; get y plot range

	WIDGET_CONTROL,widget_ids.y_min,GET_VALUE=y_min
	WIDGET_CONTROL,widget_ids.y_max,GET_VALUE=y_max
	y_min = fix(y_min)  
	y_max = fix(y_max) - 1 
	if y_min lt 0 then y_min = 0
	if y_max ge y_size then y_max = y_size - 1

	; real mag factor

	rx_mag = float(!d.x_size) / (x_max-x_min+1)
	ry_mag = float(!d.y_size) / (y_max-y_min+1)

	x = round( float(x) / rx_mag)
	y = round( float(y) / ry_mag)

if x ge catch2d_file.width or y ge catch2d_file.height then begin
	w_warningtext,'Cursor outside the image range',60,5,'VW2D Messages'
	return
	end

;  find vectior values

zv = image[x+x_min,y+y_min]
if view_option.versus then begin
	xv = catch2d_file.xarr(x+x_min)
	yv = catch2d_file.yarr(y+y_min)
	ax = catch2d_file.xarr(x_min:x_max) 
	ay = catch2d_file.yarr(y_min:y_max)
	xtitle = ' (Values)'
endif else begin
	xv = x+x_min
	yv = y+y_min
	ax = indgen(x_max-x_min+1) + x_min
	ay = indgen(y_max-y_min+1) + y_min
	xtitle = ' (Step #)'
end

if y ge 0 and y lt y_size then begin

	x_vec = image[x_min:x_max,y + y_min]
	y_vec = image[x + x_min, y_min:y_max]

; call plot1d resizaable window

	WIDGET_CONTROL,catch2d_file.xprof,BAD=bad,/DESTROY
	plot1d,ax,x_vec,id_tlb,windraw, GROUP=Event.top, $
		/cleanup, $
		wtitle='XZ Profile', xtitle='X '+xtitle, ytitle='Z - VAL', $
		title='At Y = '+ strtrim(yv,2)  + xtitle
	catch2d_file.xprof = id_tlb 
	catch2d_file.xzdraw = windraw
	WIDGET_CONTROL,catch2d_file.xprof, $
		TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 50
	end
widget_ids.x1 = !x
widget_ids.y1 = !y

if x ge 0 and (x+x_min) lt x_size then begin


; call plot1d resizable window

	WIDGET_CONTROL,catch2d_file.yprof,BAD=bad,/DESTROY
	plot1d,ay,transpose(y_vec),id_tlb,windraw,GROUP=Event.top, $
		/cleanup, $
		wtitle='YZ Profile', xtitle='Y '+xtitle, ytitle='Z - VAL', $
		title='At X = '+ strtrim(xv,2) + xtitle
	catch2d_file.yprof =id_tlb 
	catch2d_file.yzdraw = windraw
	WIDGET_CONTROL,catch2d_file.yprof, $
		TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 450

widget_ids.x2 = !x
widget_ids.y2 = !y

WIDGET_CONTROL,widget_ids.x_cursor,SET_VALUE=strtrim(xv,2)
WIDGET_CONTROL,widget_ids.y_cursor,SET_VALUE=strtrim(yv,2)
WIDGET_CONTROL,widget_ids.z_cursor,SET_VALUE=strtrim(zv)

	end

if !d.name eq OS_SYSTEM.device then WSET,widget_ids.plot2d_area
END

; 
; get cursor coordinates
;
PRO catch2d_xycoord, x, y, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

hide_cross,view_option.x,view_option.y,view_option.d_wid,view_option.s_wid
;cursor,x,y,0,/device
x = Event.x
y = Event.y
view_option.x = x
view_option.y = y
show_cross,x,y,view_option.d_wid,view_option.s_wid

	x = x / catch2d_file.x_mag
	y = y / catch2d_file.y_mag


; if user coordinate mode is set

if view_option.user eq 1 then begin
	WIDGET_CONTROL,widget_ids.y_min,GET_VALUE=y_min
	if y_min gt 0 then y = fix( y + y_min)
	WIDGET_CONTROL,widget_ids.x_min,GET_VALUE=x_min
	if x_min gt 0 then x = fix( x + x_min)
	end

if x lt catch2d_file.width and y lt catch2d_file.height then begin
;print,'x,y,zval',x,y, image[x,y]

	zv = image[x,y]
	if view_option.versus then begin
		xv = catch2d_file.xarr(x)
		yv = catch2d_file.yarr(y)
	endif else begin
		xv = x
		yv = y
	end
WIDGET_CONTROL,widget_ids.x_cursor,SET_VALUE=strtrim(xv,2) + '(*)'
WIDGET_CONTROL,widget_ids.y_cursor,SET_VALUE=strtrim(yv,2) + '(*)'
WIDGET_CONTROL,widget_ids.z_cursor,SET_VALUE=strtrim(zv,2) + '(*)'
endif else begin
	w_warningtext,'Cursor outside the image range',60,5,'VW2D Messages'
	end

END

;
; xdistribution cursor
;
PRO catch2d_xycoord1,st
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON w_warningtext_block,w_warningtext_ids

!x = widget_ids.x1
!y = widget_ids.y1
CATCH,error_status
if error_status ne 0 then begin
	st = ['Click MMB in the 2D image area first!','Before press the XZ button.']
	w_warningtext,st,60,5,'VW2D Messages',xloc=500
	return
	end

;WSET,catch2d_file.xprof
WSET,catch2d_file.xzdraw
wshow,catch2d_file.xzdraw

!ERR = 1
dline = (!y.crange(1)-!y.crange(0)) *.2
hline = (!x.crange(1)-!x.crange(0)) *.1
clr1 = 0
clr2 = !d.table_size - 1

while !ERR eq 1 do begin
cursor,x,y,1,/normal

x = (x - !x.window(0)) / (!x.window(1)-!x.window(0)) * $
        (!x.crange(1)-!x.crange(0)) + !x.crange(0)
 
y = (y - !y.window(0)) / (!y.window(1)-!y.window(0)) * $
        (!y.crange(1)-!y.crange(0)) + !y.crange(0)

oplot,catch2d_file.xzline_x, catch2d_file.xzline_z, color=clr1
oplot,catch2d_file.xzline_xo, catch2d_file.xzline_zo, color=clr1

catch2d_file.xzline_x = [x,x]
catch2d_file.xzline_z = [y-dline,y+dline]
catch2d_file.xzline_xo = [x-hline,x+hline]
catch2d_file.xzline_zo = [y,y]
oplot,catch2d_file.xzline_x, catch2d_file.xzline_z, color=clr2
oplot,catch2d_file.xzline_xo, catch2d_file.xzline_zo, color=clr2

st = 'X='+strtrim(x,2)+', Z='+strtrim(y,2)
WIDGET_CONTROL,widget_ids.xzl,SET_VALUE=st
endwhile
oplot,catch2d_file.xzline_x, catch2d_file.xzline_z, color=clr1
oplot,catch2d_file.xzline_xo, catch2d_file.xzline_zo, color=clr1
WIDGET_CONTROL,w_warningtext_ids.base,/DESTROY

if !d.name eq OS_SYSTEM.device then WSET,widget_ids.plot2d_area
END

;
; ydistribution cursor
;
PRO catch2d_xycoord2,st
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON w_warningtext_block,w_warningtext_ids

!x = widget_ids.x2
!y = widget_ids.y2
CATCH,error_status
if error_status ne 0 then begin
	st = ['Click MMB in the 2D image area first!','Before press the YZ button.']
	w_warningtext,st,60,5,'VW2D Messages',xloc=500
	return
	end
;WSET,catch2d_file.yprof
WSET,catch2d_file.yzdraw
wshow,catch2d_file.yzdraw

!ERR = 1
dline = (!y.crange(1)-!y.crange(0)) *.2
hline = (!x.crange(1)-!x.crange(0)) *.1
clr1 = 0
clr2 = !d.table_size - 1
while !ERR eq 1 do begin
cursor,x,y,1,/normal

x = (x - !x.window(0)) / (!x.window(1)-!x.window(0)) * $
        (!x.crange(1)-!x.crange(0)) + !x.crange(0)
 
y = (y - !y.window(0)) / (!y.window(1)-!y.window(0)) * $
        (!y.crange(1)-!y.crange(0)) + !y.crange(0)

oplot,catch2d_file.yzline_y, catch2d_file.yzline_z, color=clr1
oplot,catch2d_file.yzline_yo, catch2d_file.yzline_zo, color=clr1

catch2d_file.yzline_y = [x,x]
catch2d_file.yzline_z = [y-dline,y+dline]
catch2d_file.yzline_yo = [x-hline,x+hline]
catch2d_file.yzline_zo = [y,y]
oplot,catch2d_file.yzline_y, catch2d_file.yzline_z, color=clr2
oplot,catch2d_file.yzline_yo, catch2d_file.yzline_zo, color=clr2

st = 'Y='+strtrim(x,2)+', Z='+strtrim(y,2)
WIDGET_CONTROL,widget_ids.yzl,SET_VALUE=st
endwhile
oplot,catch2d_file.yzline_y, catch2d_file.yzline_z, color=clr1
oplot,catch2d_file.yzline_yo, catch2d_file.yzline_zo, color=clr1
WIDGET_CONTROL,w_warningtext_ids.base,/DESTROY

if !d.name eq OS_SYSTEM.device then WSET,widget_ids.plot2d_area
END


;
; plot x,y distributions
;
PRO catch2d_xydist2, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

;wdelete,catch2d_file.xprof,catch2d_file.yprof

x_size = 3 > catch2d_file.x_act_npts
y_size = 3 > catch2d_file.y_act_npts

WSET,widget_ids.plot2d_area
hide_cross,view_option.x,view_option.y,view_option.d_wid,view_option.s_wid
;cursor,x,y,0,/device
x = Event.x
y = Event.y
if x lt 0 or y lt 0 then return

; save cursor location
view_option.x = x
view_option.y = y
show_cross,x,y,view_option.d_wid,view_option.s_wid

;TVCRS,x,y


	; get x plot range

	WIDGET_CONTROL,widget_ids.x_min,GET_VALUE=x_min
	WIDGET_CONTROL,widget_ids.x_max,GET_VALUE=x_max
	x_min = fix(x_min) 
	x_max = fix(x_max) - 1
	if x_min lt 0 then x_min = 0
	if x_max ge x_size then x_max = x_size - 1

	; get y plot range

	WIDGET_CONTROL,widget_ids.y_min,GET_VALUE=y_min
	WIDGET_CONTROL,widget_ids.y_max,GET_VALUE=y_max
	y_min = fix(y_min)  
	y_max = fix(y_max) - 1 
	if y_min lt 0 then y_min = 0
	if y_max ge y_size then y_max = y_size - 1

	; real mag factor

	rx_mag = catch2d_file.x_mag
	ry_mag = catch2d_file.y_mag

	x = fix( float(x-view_option.margin_l) / rx_mag)
	y = fix( float(y-view_option.margin_b) / ry_mag)

if x ge catch2d_file.width or y ge catch2d_file.height then begin
	w_warningtext,'Cursor outside the image range',60,5,'VW2D Messages'
	return
	end

;  find vectior values

zv = image[x+x_min,y+y_min]
xv = catch2d_file.xarr(x+x_min)
yv = catch2d_file.yarr(y+y_min)
if view_option.versus then begin
	ax = catch2d_file.xarr(x_min:x_max) 
	ay = catch2d_file.yarr(y_min:y_max)
	xtitle = ' (Values)'
endif else begin
	ax = indgen(x_max-x_min+1) + x_min
	ay = indgen(y_max-y_min+1) + y_min
	xtitle = ' (Step #)'
end

if y ge 0 and y lt y_size then begin

	x_vec = image[x_min:x_max,y + y_min]
	y_vec = image[x + x_min, y_min:y_max]

; call plot1d resizaable window

	WIDGET_CONTROL,catch2d_file.xprof,BAD=bad,/DESTROY
	plot1d,ax,x_vec,id_tlb,windraw, GROUP=Event.top, $
		/cleanup, $
		wtitle='XZ Profile', xtitle='X '+xtitle, ytitle='Z - VAL', $
		title='At Y('+strtrim(y+y_min,2)+') = '+ strtrim(yv,2)  + xtitle
	catch2d_file.xprof = id_tlb 
	catch2d_file.xzdraw = windraw
	WIDGET_CONTROL,catch2d_file.xprof, $
		TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 50
	end
widget_ids.x1 = !x
widget_ids.y1 = !y

if x ge 0 and (x+x_min) lt x_size then begin


; call plot1d resizable window

	WIDGET_CONTROL,catch2d_file.yprof,BAD=bad,/DESTROY
	plot1d,ay,transpose(y_vec),id_tlb,windraw,GROUP=Event.top, $
		/cleanup, $
		wtitle='YZ Profile', xtitle='Y '+xtitle, ytitle='Z - VAL', $
		title='At X('+strtrim(x+x_min,2)+') = '+ strtrim(xv,2) + xtitle
	catch2d_file.yprof =id_tlb 
	catch2d_file.yzdraw = windraw
	WIDGET_CONTROL,catch2d_file.yprof, $
		TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 450

widget_ids.x2 = !x
widget_ids.y2 = !y

WIDGET_CONTROL,widget_ids.x_cursor,SET_VALUE=strtrim(xv,2)
WIDGET_CONTROL,widget_ids.y_cursor,SET_VALUE=strtrim(yv,2)
WIDGET_CONTROL,widget_ids.z_cursor,SET_VALUE=strtrim(zv)

	end

if !d.name eq OS_SYSTEM.device then WSET,widget_ids.plot2d_area
END

; 
; get cursor coordinates
;
PRO catch2d_xycoord_TV, x, y, Event
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

hide_cross,view_option.x,view_option.y,view_option.d_wid,view_option.s_wid
;cursor,x,y,0,/device
x = Event.x
y = Event.y
view_option.x = x
view_option.y = y
show_cross,x,y,view_option.d_wid,view_option.s_wid

        ; real mag factor

        rx_mag = catch2d_file.x_mag
        ry_mag = catch2d_file.y_mag

        x = fix( float(x-view_option.margin_l) / rx_mag)
        y = fix( float(y-view_option.margin_b) / ry_mag)



; if user coordinate mode is set

if view_option.user eq 1 then begin
	WIDGET_CONTROL,widget_ids.y_min,GET_VALUE=y_min
	if y_min gt 0 then y = fix( y + y_min)
	WIDGET_CONTROL,widget_ids.x_min,GET_VALUE=x_min
	if x_min gt 0 then x = fix( x + x_min)
	end

if x lt catch2d_file.width and x ge 0 and y ge 0 and  y lt catch2d_file.height then begin
;print,'x,y,zval',x,y, image[x,y]

	zv = image[x,y]
	if view_option.versus then begin
		xv = catch2d_file.xarr(x)
		yv = catch2d_file.yarr(y)
	endif else begin
		xv = x
		yv = y
	end
WIDGET_CONTROL,widget_ids.x_cursor,SET_VALUE=strtrim(xv,2) + '(*)'
WIDGET_CONTROL,widget_ids.y_cursor,SET_VALUE=strtrim(yv,2) + '(*)'
WIDGET_CONTROL,widget_ids.z_cursor,SET_VALUE=strtrim(zv,2) + '(*)'
endif else begin
	w_warningtext,'Cursor outside the image range',60,5,'VW2D Messages'
	end

END
;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************



PRO ASSIGNDNAME_Event, Event

  WIDGET_CONTROL, Event.Top, GET_UVALUE=assignName_state
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'ASSIGN_HELP': BEGIN
	str =[ $
	'This program allows the user to assign the preferred name to ', $
	'the detectors.', $
	'Accept without any change the default names are assumed.', $
	'No carriage return is allowed. Use backspace to wipe', $
	'out any type error.', '', $
	'Help... - Pops up this help message window', $
	'Reset  -  Reset to default Di names', $
	'Close  -  Abort all changes with save', $
	'Filename - Assigned file name used for this scan', $
	'Accept -  Press Accept button to accept all changes.', $
	'          save the filename and updates .tmpName', $
	'Dname fields  -  Modifiable detector name','' $
	]
	r = dialog_message(str,/Info,title='Help on Assign Names')
	return
      END
  'ASSIGN_FILE': BEGIN
  	WIDGET_CONTROL, Event.Id, GET_VALUE=file
	assignName_state.file = file(0)
	found = findfile(file,count=ct)
  	if ct then begin
	xdr_open,unit,file
	xdr_read,unit,dnames
	xdr_close,unit
	assignName_state.dnames = dnames
	d1 = dnames(0:14)
	d2 = dnames(15:34)
	d3 = dnames(35:54)
	d4 = dnames(55:74)
	d5 = dnames(75:84)
	WIDGET_CONTROL,assignName_state.textD1Wid,SET_VALUE=d1
	WIDGET_CONTROL,assignName_state.textD01Wid,SET_VALUE=d2
	WIDGET_CONTROL,assignName_state.textD21Wid,SET_VALUE=d3 
	WIDGET_CONTROL,assignName_state.textD41Wid,SET_VALUE=d4 
	WIDGET_CONTROL,assignName_state.textD61Wid,SET_VALUE=d5
	end
  	WIDGET_CONTROL, Event.Top, SET_UVALUE=assignName_state
      END
  'ASSIGN_CLOSE': BEGIN
  	WIDGET_CONTROL, Event.Top, SET_UVALUE=assignName_state
	WIDGET_CONTROL,Event.top,/DESTROY
      END
  'ASSIGN_RESET': BEGIN
	list = assignName_state.DI
	d1 = list(0:14)
	d2 = list(15:34)
	d3 = list(35:54)
	d4 = list(55:74)
	d5 = list(75:84)
	WIDGET_CONTROL,assignName_state.textD1Wid,SET_VALUE=d1
	WIDGET_CONTROL,assignName_state.textD01Wid,SET_VALUE=d2
	WIDGET_CONTROL,assignName_state.textD21Wid,SET_VALUE=d3 
	WIDGET_CONTROL,assignName_state.textD41Wid,SET_VALUE=d4 
	WIDGET_CONTROL,assignName_state.textD61Wid,SET_VALUE=d5
	assignName_state.dnames = list
  	WIDGET_CONTROL, Event.Top, SET_UVALUE=assignName_state
	return
      END
  'ASSIGN_D1TEXT': BEGIN
	res = dialog_message('Error found in D1:DF, no <CR> allowed',/error) 
	return
      END
  'ASSIGN_D01TEXT': BEGIN
	res = dialog_message('Error found in D01:D20, no <CR> allowed',/error) 
	return
      END
  'ASSIGN_D21TEXT': BEGIN
	res = dialog_message('Error found in D21:D40, no <CR> allowed',/error) 
	return
      END
  'ASSIGN_D41TEXT': BEGIN
	res = dialog_message('Error found in D41:D60, no <CR> allowed',/error) 
	return
      END
  'ASSIGN_D61TEXT': BEGIN
	res = dialog_message('Error found in D61:D70, no <CR> allowed',/error) 
	return
      END
  'ASSIGN_ACCEPT': BEGIN
	WIDGET_CONTROL,assignName_state.textD1Wid,GET_VALUE=d1
	WIDGET_CONTROL,assignName_state.textD01Wid,GET_VALUE=d2
	WIDGET_CONTROL,assignName_state.textD21Wid,GET_VALUE=d3
	WIDGET_CONTROL,assignName_state.textD41Wid,GET_VALUE=d4
	WIDGET_CONTROL,assignName_state.textD61Wid,GET_VALUE=d5
	list = [d1,d2,d3,d4,d5]
	r = dialog_message('Are you sure?',/question,/default_no,dialog_parent=Event.top)
	if r eq 'Yes' then begin
	assignName_state.dnames = list
	assignName_write,list,file=assignName_state.file
	end
  	WIDGET_CONTROL, Event.Top, SET_UVALUE=assignName_state
	WIDGET_CONTROL,Event.top,/DESTROY
      END
  ENDCASE
END


PRO assignName_init,assignName_state,file=file
  d1 = [ 'D1','D2','D3','D4','D5','D6','D7','D8','D9','DA','DB', $
	'DC','DD','DE','DF']
  d2 = [ 'D01','D02','D03','D04','D05','D06','D07','D08','D09', $
	'D'+strtrim(indgen(11)+10,2)]
  d3 = [ 'D'+strtrim(indgen(20)+21,2)]
  d4 = [ 'D'+strtrim(indgen(20)+41,2)]
  d5 = [ 'D'+strtrim(indgen(10)+61,2)]
  dnames =[d1,d2,d3,d4,d5]

  found = findfile(file,count=ct)
  if ct then begin
	xdr_open,unit,'.tmpName'
	xdr_read,unit,dnames
	xdr_close,unit
  end

  assignName_state = { $
	textD1Wid : 0L, $
	textD01Wid : 0L, $
	textD21Wid : 0L, $
	textD41Wid : 0L, $
	textD61Wid : 0L, $
	fileWid : 0L, $
	file : file, $
	ndet : n_elements(dnames), $
	DI : [d1,d2,d3,d4,d5], $
	dnames : dnames $
	}

END


PRO assign_detName,new=new,part=part,file=file, GROUP=Group
; default is 85 detectors
; if part is specified 15 detectors assumed
; if new is specified, the last 70 detectors assumed

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

;  junk   = { CW_PDMENU_S, flags:0, name:'' }

  assignName_init,assignName_state,file=file

  ASSIGNDNAME = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, title='RE-ASSIGN DETECTOR NAME', $
      MAP=1, $
      UVALUE='ASSIGNDNAME')

  BASE2 = WIDGET_BASE(ASSIGNDNAME, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  BTN3 = WIDGET_BUTTON( BASE3, $
      UVALUE='ASSIGN_HELP', $
      VALUE='Help...')

  BTN4 = WIDGET_BUTTON( BASE3, $
      UVALUE='ASSIGN_RESET', $
      VALUE='Reset')

  BTN6 = WIDGET_BUTTON( BASE3, $
      UVALUE='ASSIGN_CLOSE', $
      VALUE='Close')

  file_name = cw_field(BASE2,/RETURN_EVENTS,/string,value=file, $
	title='Filename:',xsize=40,uvalue='ASSIGN_FILE')

  BUTTON15 = WIDGET_BUTTON( BASE2, $
      UVALUE='ASSIGN_ACCEPT', $
      VALUE='Accept')

  BASE4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  BASE5 = WIDGET_BASE(BASE4, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE5')

  LABEL5 = WIDGET_LABEL( BASE5, $
      UVALUE='LABEL5', $
      VALUE='D1:DF')

  d1 = assignName_state.dnames[0:14]
  TEXT5 = WIDGET_TEXT( BASE5,VALUE=d1, $
      EDITABLE=1, $
      UVALUE='ASSIGN_D1TEXT', $
      XSIZE=10,YSIZE=15)

  BASE6 = WIDGET_BASE(BASE4, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE6')
  LABEL6 = WIDGET_LABEL( BASE6, $
      UVALUE='LABEL6', $
      VALUE='D01:D20')
  d2 = assignName_state.dnames[15:34]
  TEXT6 = WIDGET_TEXT( BASE6,VALUE=d2, $
      EDITABLE=1, $
      UVALUE='ASSIGN_D01TEXT', $
      XSIZE=10,YSIZE=20)

  BASE7 = WIDGET_BASE(BASE4, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE7')
  LABEL7 = WIDGET_LABEL( BASE7, $
      UVALUE='LABEL7', $
      VALUE='D21:D40')
  d3 = assignName_state.dnames[35:54]
  TEXT7 = WIDGET_TEXT( BASE7,VALUE=d3, $
      EDITABLE=1, $
      UVALUE='ASSIGN_D21TEXT', $
      XSIZE=10,YSIZE=20)

  BASE8 = WIDGET_BASE(BASE4, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE8')
  LABEL8 = WIDGET_LABEL( BASE8, $
      UVALUE='LABEL8', $
      VALUE='D41:D60')
  d4 = assignName_state.dnames[55:74]
  TEXT8 = WIDGET_TEXT( BASE8,VALUE=d4, $
      EDITABLE=1, $
      UVALUE='ASSIGN_D41TEXT', $
      XSIZE=10,YSIZE=20)

  BASE9 = WIDGET_BASE(BASE4, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE9')
  LABEL9 = WIDGET_LABEL( BASE9, $
      UVALUE='LABEL9', $
      VALUE='D61:D70')
  d5 = assignName_state.dnames[75:84]
  TEXT9 = WIDGET_TEXT( BASE9,VALUE=d5, $
      EDITABLE=1, $
      UVALUE='ASSIGN_D61TEXT', $
      XSIZE=10,YSIZE=10)

  	assignName_state.textD1Wid = TEXT5
	assignName_state.textD01Wid = TEXT6
	assignName_state.textD21Wid = TEXT7
	assignName_state.textD41Wid = TEXT8
	assignName_state.textD61Wid = TEXT9
	assignName_state.fileWid = file_name

if keyword_set(new)  then begin
	WIDGET_CONTROL,TEXT5,SENSITIVE=0
end
if keyword_set(part)  then begin
	WIDGET_CONTROL,TEXT6,SENSITIVE=0
	WIDGET_CONTROL,TEXT7,SENSITIVE=0
	WIDGET_CONTROL,TEXT8,SENSITIVE=0
	WIDGET_CONTROL,TEXT9,SENSITIVE=0
end

  WIDGET_CONTROL, ASSIGNDNAME, SET_UVALUE=assignName_state
  WIDGET_CONTROL, ASSIGNDNAME, /REALIZE

  XMANAGER, 'ASSIGNDNAME', ASSIGNDNAME
END


PRO assignName_write,dnames,file=file
	
	xdr_open,unit,/write,file
	xdr_write,unit,dnames
	xdr_close,unit
	if file ne '.tmpName' then begin
	xdr_open,unit,/write,'.tmpName'
	xdr_write,unit,dnames
	xdr_close,unit
	end
END

PRO assignName_read,dnames,new=new,file=file,GROUP=group
; use intermediate filename '.tmpName' for saving dnames

	if keyword_set(file) eq 0 then file='.tmpName'
	part = 0
	if n_elements(dnames) eq 15 then part=15
	assign_detName,GROUP=group,part=part,new=new,file=file
	found = findfile(file,count=ct)
	if ct then begin
	xdr_open,unit,'.tmpName'
	xdr_read,unit,dnames
	xdr_close,unit
	end
END



PRO user_scan_getData,st
COMMON USERSCAN,user_scan_state

	sz = size(user_scan_state) 
	if sz(0) eq 0 then return

	widget_control,st.pvWID(0),get_value=pv1
	widget_control,st.pvWID(1),get_value=pv2
	st.pv = [pv1,pv2]
	data = make_array(4,st.nscan)
	for i=0,st.nscan-1 do begin
	widget_control,st.fields(0,i),get_value=xl
	widget_control,st.fields(1,i),get_value=xr
	widget_control,st.fields(2,i),get_value=yl
	widget_control,st.fields(3,i),get_value=yr
	data(0,i) = xl
	data(1,i) = xr
	data(2,i) = yl
	data(3,i) = yr
	end

	user_scan_state.pv = st.pv
	user_scan_state.num = st.nscan
	*user_scan_state.data = data

print,*user_scan_state.data
print,user_scan_state
END

PRO after_sys_scan,file=file,onoff=onoff
COMMON USERSCAN,user_scan_state

	sz = size(user_scan_state)
	if sz(0) eq 0 then return

	; get autoscan # and xl,xr,yl,yr scan ranges

	if keyword_set(onoff) then user_scan_state.onoff=onoff
	if user_scan_state.onoff eq 1 then begin
	user_scan_data = *user_scan_state.data
	line = user_scan_state.num
	user_scan_no = user_scan_state.id

	if user_scan_state.id ge user_scan_state.num then begin
		print,'No more after_sys_scan'
		user_scan_state.onoff = 0
		return
	end
	   	x = user_scan_data(*,user_scan_state.id)
	   	print,user_scan_state.id,x
		pv = user_scan_state.pv

		; check whether 2D scan is going on 

		r = cagetarray(pv(1)+'.EXSC',pd)
		if r ne 0 then begin
		 res = dialog_message(['Invalid SCAN PV names:' ,'',pv],/error)
		 return
		end
		if pd(0) eq 1 then return

		; 2D scan is not going on 

		user_scan_autoscan,pv=pv,x
	   	user_scan_state.id = user_scan_state.id + 1

	end

	
END

PRO user_scan_stopscan
COMMON USERSCAN,user_scan_state
	sz = size(user_scan_state)
	if sz(0) eq 0 then return

	user_scan_state.onoff = 0

	pv = user_scan_state.pv
	st = 'caput ' + pv(1)+'.EXSC  0'
	spawn,st,result
	print,result

END

PRO user_scan_autoscan,x,pv=pv
	if (x(0) eq x(1)) and (x(2) eq x(3)) then begin
		print,'xranges: ',x(0),x(1)
		print,'yranges: ',x(2),x(3)
		return
	end
	if n_elements(pv) eq 2 then begin

	; set scan ranges  x=[xl,xr,yl,yr]
	v = double(transpose(x))
	names = [pv(0)+'.P1SP', pv(0)+'.P1EP', pv(1)+'.P1SP', pv(1)+'.P1EP']
	r = caputarray(names,v)
	r = cagetarray(names,v1)


;	names = pv(0)+'.P1SP' + ',' + pv(0)+'.P1EP' + ',' $
;		+ pv(1)+'.P1SP' + ',' + pv(1)+'.P1EP' 
;	v = strtrim(x,2)
;	values = v(0) + ',' + v(1) + ',' +v(2) + ',' +  v(3)
;	st = 'caput '+ names +' ' + values
;	print,st
;	spawn,st,result
;	print,'result=',result

;	wait,10

	; invoke 2D scans
;	r = caputarray(pv(1)+'.EXSC', double(1))
;	r = cagetarray(pv(1)+'.EXSC',pd)
;	print,'pd=',pd

	st = 'caput ' + pv(1)+'.EXSC  1'
	spawn,st,result
	print,result

	end
END

PRO user_scan_readData,file=file,error=error
COMMON USERSCAN,user_scan_state

filename = 'user_scan.dat'
if keyword_set(file) then filename=file

	sz = size(user_scan_state)
if sz(0) eq 0 then begin
user_scan_state = { $
	num:	 0, $
	pv: ['',''], $
	data:	 ptr_new(/allocate_heap), $
	id:	 0, $
	onoff :	 0, $
	file: 	 filename }
endif else begin
	user_scan_state.num = 0 
	user_scan_state.id = 0
	user_scan_state.onoff = 0
	*user_scan_state.data = 0
	user_scan_state.pv = ['',''] 
	user_scan_state.file = filename
end


	found = findfile(filename,count=ct)

	if ct eq 0 then begin
		error = -1	
		return
	endif else  begin

	line=0
	openr,1,filename
	s = ''
	while not eof(1) do begin
	readf,1,s
	str = strsplit(s,' ',/extract)
	if n_elements(str) eq 4 then begin
	x = float(str)
	if line eq 0 then user_scan_data = x else $
	user_scan_data = [user_scan_data,x]
	line=line+1
	endif else pv = str 
	end
	close,1
	user_scan_no = 0
	user_scan_data = reform(user_scan_data,4,line)
	end

	user_scan_state.pv = pv 
	user_scan_state.num = line
	*user_scan_state.data = user_scan_data
END

FUNCTION user_scan_fields,parent,nscan,file,fileWID,pv=pv
  nm = ['uid:scan1','uid:scan2']
  if n_elements(pv) eq 2 then nm = pv

  if n_elements(nscan) eq 0 then nscan = 1

  fieldWID = make_array(4,nscan,value=0L)

  st = {file: file, $
	pv: nm, $
	nscan:nscan, $
	parent:parent, $
	pvWID: [0L,0L], $
	fileWID : fileWID, $
	base: 0L, $
	fields:fieldWID}

  BASE3 = WIDGET_BASE(parent, $
      COLUMN=1, y_scroll_size=500, /scroll, $
      MAP=1, x_scroll_size=500, $
      UVALUE='BASE3')
  BASE3_1 = WIDGET_BASE(BASE3, $
      ROW=1, /frame, $
      MAP=1, $
      UVALUE='BASE3_1')
  pv1 = CW_FIELD( BASE3_1,VALUE=st.pv(0), $
      TITLE='Scan1 PV:', /return_events, $
      UVALUE='USER_SCAN_PV1')
  pv2 = CW_FIELD( BASE3_1,VALUE=st.pv(1), $
      TITLE='Scan2 PV:', /return_events, $
      UVALUE='USER_SCAN_PV2')

  for i=0,nscan-1 do begin

  BASE4 = WIDGET_BASE(BASE3, $
      COLUMN=1, /frame, $
      MAP=1, $
      UVALUE='BASE4')
  label_i = widget_label(BASE4,value='Auto Scan #' +string(i+1),/align_left)

  BASE4_1 = WIDGET_BASE(BASE4, $
      /ROW, MAP=1, UVALUE='BASE4_1')
  BASE4_2 = WIDGET_BASE(BASE4, $
      /ROW, MAP=1, UVALUE='BASE4_2')
  fieldWID(0,i) = CW_FIELD( BASE4_1,VALUE='', $
      ROW=1, $
      FLOAT=1, $
      TITLE='Xmin', $
      UVALUE='USER_SCAN_XL')

  fieldWID(1,i) = CW_FIELD( BASE4_1,VALUE='', $
      ROW=1, $
      FLOAT=1, $
      TITLE='Xmax', $
      UVALUE='USER_SCAN_XR')

  fieldWID(2,i) = CW_FIELD( BASE4_2,VALUE='', $
      ROW=1, $
      FLOAT=1, $
      TITLE='Ymin', $
      UVALUE='USER_SCAN_YL')

  fieldWID(3,i) = CW_FIELD( BASE4_2,VALUE='', $
      ROW=1, $
      FLOAT=1, $
      TITLE='Ymax', $
      UVALUE='USER_SCAN_YR')

;  FieldVal1117 = [ $
;    '' ]
;  fieldWID(4,i) = CW_FIELD( BASE4,VALUE=FieldVal1117, $
;      ROW=1, $
;      FLOAT=1, $
;      TITLE='ZL', $
;      UVALUE='USER_SCAN_ZL')

;  FieldVal1182 = [ $
;    '' ]
;  fieldWID(5,i) = CW_FIELD( BASE4,VALUE=FieldVal1182, $
;      ROW=1, $
;      FLOAT=1, $
;      TITLE='ZR', $
;      UVALUE='USER_SCAN_YR')
  end

  st.pvWID = [pv1,pv2]
  st.base = base3
  st.nscan = nscan
  st.fields = fieldWID
  return, st
END

PRO user_scan_populate,st

	found = findfile(st.file,count=ct)
	if ct eq 0 then begin
	r = dialog_message(['File is not found:', st.file, $
		'You need first enter new values and then ', $
		'press the "SaveFile" button'],/error)
	return
	end

	line=0
	openr,1,st.file  ;'user_scan.dat'
	s = ''
	readf,1,s
	str = strsplit(s,' ',/extract)
	if n_elements(str) eq 2 then begin
	widget_control,st.pvWID(0),set_value=str(0)
	widget_control,st.pvWID(1),set_value=str(1)
	end

	while not eof(1) do begin
	readf,1,s
	str = strsplit(s,' ',/extract)
	x = float(str)
	if line lt st.nscan then begin
	widget_control,st.fields(0,line),set_value=x(0)
	widget_control,st.fields(1,line),set_value=x(1)
	widget_control,st.fields(2,line),set_value=x(2)
	widget_control,st.fields(3,line),set_value=x(3)
	line=line+1
	end
	end

	close,1
END

PRO user_scan_clearfld,st
	widget_control,st.pvWID(0),set_value=''
	widget_control,st.pvWID(1),set_value=''
	for i=0,st.nscan-1 do begin
	widget_control,st.fields(0,i),set_value=0
	widget_control,st.fields(1,i),set_value=0
	widget_control,st.fields(2,i),set_value=0
	widget_control,st.fields(3,i),set_value=0
	end
END

PRO user_scan_writeData,st
	openw,1,st.file  ;'user_scan.dat'
	widget_control,st.pvWID(0),get_value=pv1
	widget_control,st.pvWID(1),get_value=pv2
	st.pv = [pv1,pv2]
;	print,st.pv
	printf,1,st.pv
	for i=0,st.nscan-1 do begin
	widget_control,st.fields(0,i),get_value=xl
	widget_control,st.fields(1,i),get_value=xr
	widget_control,st.fields(2,i),get_value=yl
	widget_control,st.fields(3,i),get_value=yr
;	print,i,xl,xr,yl,yr
	x=[xl,xr,yl,yr]
	printf,1,x
	end
	close,1
END

PRO user_scan_help
	str = ['This dialog let user define a set of automatic 2D scan ranges.', $
	'', $
	'Usage: USER_SCAN [,NSCAN=#] [,FILE=file] ', $
	'   where', $
	'       NSCAN - specifies the total number of subsequent 2D scans', $
	'               default to 5', $
	'       FILE  - specifies the filename to be used for storing the', $
	'               2D scan ranges, defualt is "user_scan.dat"', $
	'', $
	'If the autoScan option is set in the scanSee it allows the user to ', $
	'access the automatic 2D scan dialog to define the scan # and ranges', $
	'to start or stop the automatic 2D scans', $
	'','User Interface:', $

	' File...   - pick 2D scan ranges file through file selection dialog ', $
	' filename  - display field for current file picked', $
	' Help...   - shows this help page', $
	' Clear     - clears the current Xmin,Xmax,Ymin,Ymax values', $
	' LoadFile  - this button load 2D scan ranges from file', $
	' SaveFile  - save new 2D ranges the file', $
	' Start AutoScan - start/continue the AutoScan loop', $	
	'                  SaveFile must pressed before Start AutoScan', $
	' Stop AutoScan - stop the AutoScan loop', $	
	' Done      - closes the dialog', $
	'']
	r = dialog_message(str,/info)
END

PRO USER_SCAN_MAIN13_Event, Event
COMMON USERSCAN,user_scan_data

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev
  WIDGET_CONTROL,Event.Top,GET_UVALUE=st,/no_copy
	fieldsWID = st.fields
	nscan = st.nscan
  sz = size(user_scan_data)

  CASE Ev OF 

  'USER_SCAN_PV1': BEGIN
	widget_control,Event.Id,get_value=pv1
	st.pv(0) = pv1
	END
  'USER_SCAN_PV2': BEGIN
	widget_control,Event.Id,get_value=pv1
	st.pv(1) = pv1
	END
  'USER_SCAN_FILEPICK': BEGIN
	f = dialog_pickfile(title='AutoScan Pick File', $
		file='user_scan.dat',filter='*.dat*',get_path=p)
	st.file = f
	widget_control,st.fileWID,set_value=f
	user_scan_populate,st
	END
  'USER_SCAN_FILENAME': BEGIN
	widget_control,st.fileWID,get_value=f
	st.file = f
	user_scan_populate,st
	END
  'USER_SCAN_CLEAR': BEGIN
	user_scan_clearfld,st
 	END
  'USER_SCAN_LOADFILE': BEGIN
	user_scan_populate,st
  	END

  'USER_SCAN_SAVEFILE': BEGIN
	r = dialog_message('Are you sure?',/question)
	if r eq 'Yes' then begin
	user_scan_writeData,st
;	user_scan_readData
	end
      END
  'USER_SCAN_SIMULATE': BEGIN
	user_scan_getData,st
	if sz(0) gt 0 then begin
	r = dialog_message(['It is about to START the automatic 2D scan.', $
	'','Are you sure?'],/question)
	if r eq 'Yes' then begin
	after_sys_scan,onoff=1
	end
	endif else user_scan_populate,st
      END
  'USER_SCAN_STOPSCAN': BEGIN
	if sz(0) gt 0 then begin
	r = dialog_message(['It is about to STOP the automatic 2D scan.', $
	'','Are you sure?'],/question)
	if r eq 'Yes' then begin
	user_scan_stopscan
	end
	end
      END
  'USER_SCAN_HELP': BEGIN
	user_scan_help
      END
  'USER_SCAN_CANCEL': BEGIN
	widget_control,Event.Top,/destroy
	return
      END
  ENDCASE

  WIDGET_CONTROL,Event.Top,SET_UVALUE=st,/no_copy

END



PRO user_scan, pv=pv, nscan=nscan, GROUP=Group,file=file


if n_elements(nscan) eq 0 then nscan = 5
if n_elements(file) eq 0 then file='user_scan.dat'
user_scan_readData,file=file,error=error

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  USER_SCAN_MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      TITLE='AUTO_USER_SCAN_SETUP', $
      UVALUE='USER_SCAN_MAIN13')

  BASE2 = WIDGET_BASE(USER_SCAN_MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE1 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE1')
  BUTTON1 = WIDGET_BUTTON( BASE1, $
      UVALUE='USER_SCAN_FILEPICK', $
      VALUE='File...')
  FIELD_file = CW_FIELD( BASE1,VALUE=file, $
      ROW=1, TITLE=' ', /return_events, xsize=50, $
      UVALUE='USER_SCAN_FILENAME')

  BASE44 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE44')

  BUTTON43 = WIDGET_BUTTON( BASE44, $
      UVALUE='USER_SCAN_HELP', $
      VALUE='Help...')

  BUTTON45 = WIDGET_BUTTON( BASE44, $
      UVALUE='USER_SCAN_CLEAR', $
      VALUE='Clear')

  BUTTON44 = WIDGET_BUTTON( BASE44, $
      UVALUE='USER_SCAN_LOADFILE', $
      VALUE='LoadFile')

  BUTTON41 = WIDGET_BUTTON( BASE44, $
      UVALUE='USER_SCAN_SAVEFILE', $
      VALUE='SaveFile')

  BUTTON45 = WIDGET_BUTTON( BASE44, $
      UVALUE='USER_SCAN_SIMULATE', $
      VALUE='Start AutoScan')

  BUTTON46 = WIDGET_BUTTON( BASE44, $
      UVALUE='USER_SCAN_STOPSCAN', $
      VALUE='Stop AutoScan')

  BUTTON42 = WIDGET_BUTTON( BASE44, $
      UVALUE='USER_SCAN_CANCEL', $
      VALUE='Done')

	label = widget_label(BASE2,value="TOTAL # OF AUTOMATIC SCANs"+string(nscan))

     fields = user_scan_fields(BASE2,nscan,file,FIELD_file,pv=pv)

  WIDGET_CONTROL, USER_SCAN_MAIN13, /REALIZE,set_uvalue=fields

  XMANAGER, 'USER_SCAN_MAIN13', USER_SCAN_MAIN13
END



PRO user_scan_init_Event, Event

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev
  WIDGET_CONTROL,Event.top,GET_UVALUE=state

  CASE Ev OF 

  'USER_SCAN_NSCAN': BEGIN
      END
  'USER_SCAN_ACCEPT': BEGIN
	widget_control,state.fldWID,get_value=n
	user_scan,nscan=n,group=state.parent
	widget_control,state.base,/destroy,bad=bad
      END
  ENDCASE
END



PRO user_scan_init, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  user_scan_init = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, $
      TITLE='USER_SCAN', $
      UVALUE='user_scan_init')

  BASE2 = WIDGET_BASE(user_scan_init, $
      COLUMN=1, /frame, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL6 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL6', $
      VALUE='ENTER TOTAL # OF AUTOMATIC 2D SCANS')

  FieldVal167 = [ $
    '5' ]
  FIELD3 = CW_FIELD( BASE2,VALUE=FieldVal167, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='NSCAN: ', $
      UVALUE='USER_SCAN_NSCAN')

  BUTTON5 = WIDGET_BUTTON( user_scan_init, $
      UVALUE='USER_SCAN_ACCEPT', $
      VALUE='Accept...')

  state = { base:user_scan_init, $
	parent:group, $
	fldWID: FIELD3 $
	}
  WIDGET_CONTROL, user_scan_init, /REALIZE, set_uvalue=state

  XMANAGER, 'user_scan_init', user_scan_init
END
;
;  DC.pro
;

@scan_colorbar.pro
@sscan.pro

PRO DC_3DscanMessage,filename
	str = [filename,'','It is a 3D scan file, you have to use', $
		'pick3d   [ViewData->PICK3D...]  or ','SB      [ViewData->1D/2D/3D Browser...', $
		'to view it']
	r = dialog_message(str,/info)
END

PRO catch1d_append
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

;
; destroy the old w_plotspec window
;
	close_plotspec

	WIDGET_CONTROL,widget_ids.summary_base,/DESTROY,BAD_ID=bad
	if n_elements(w_viewscan_ids) then $
	WIDGET_CONTROL,w_viewscan_ids.base,/DESTROY,BAD_ID=bad

	filenamepath,scanData.trashcan,old_file,old_path

        FNAME = '*'+strmid(scanData.suffix,0,4)  ;+ '*'   '*mda*' or '*scan*'


; check for bad directory -296

        CATCH, error_status

        if error_status ne 0 then begin   
        w_warningtext,[!err_string + string(!err) ,'       Bad data directory !!!', $
		'       Please try to fix the problem first.']
        retall
        end


        F = PICKFILE(TITLE='Open ...',/READ,FILE=filename,PATH=old_path,GET_PATH=P,FILTER=FNAME)

        IF F eq '' THEN return 

	if STRMID(F,0,1) eq '~' then filename_expand,F 
        found=findfile(F)

        IF STRPOS(F,!os.file_sep) ge 0 THEN $
                FNAME = F $
        ELSE $
                FNAME = P+F

	filenamepath,FNAME,F,P
	scanData.path = P 

if strlen(P) gt 1 then begin

	CATCH,error_status
	if error_status lt 0 then begin
	w_warningtext,!err_string + string(!err)
	return
	end

	if scanData.debug eq 1 then $
	print,'CURRENT_DIR: ',P
	cl = strlen(P)
	if strmid(P,cl-1,1) eq !os.file_sep  then D = strmid(P,0,cl-1)
	end

; check for bad D

	if n_elements(D) eq 0 then begin
	w_warningtext,'Error:  bad directory path for the data file '
	return
	end

; check file version

        w_plotspec_array(3) = F
	scanData.trashcan = FNAME
	po = strpos(FNAME,'.',/reverse_search)
	scanData.suffix = strmid(FNAME,po,strlen(FNAME)-po)

	catch1d_viewdataSetup

	if string(D) ne string(old_path) then  pventry_event
	w_viewscan_calcFilename     ; reset current fileno
;	WIDGET_CONTROL,widget_ids.trashcan,SET_VALUE=scanData.trashcan
	catch1d_findLast

; set new out path
	re = findfile(P+'ASCII',count=ct)
	if ct eq 0 then begin
	tpath1 = scanData.home+!os.file_sep
	scanData.outpath = tpath1
	end

END

PRO catch1d_findLast
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

	suffix = scanData.suffix   ;'.scan'

	r = rstrpos(scanData.trashcan,'_')
	user = strmid(scanData.trashcan,0,r+1)

        found = findfile(scanData.path+'*'+scanData.suffix+'*',count=ct)
        len = strlen(user)
        sp = rstrpos(user,!os.file_sep)
        if sp gt -1 then sp=sp+1
        prefix = strmid(user,sp,len-sp)
        num = 0
        len1 = strlen(prefix)
        for i=0,n_elements(found)-1 do begin
        rp = strpos(found(i),prefix)
        rp1 = rstrpos(found(i),suffix)
        if rp ge 0 and rp1 gt len1 then begin
                ar = strmid(found(i),rp+len1,rp1-rp-len1)
                if fix(ar) gt num then begin
                        num= fix(ar)
                        ip = i
                        end
                end
        end

	scanData.filemax = num
END

PRO catch1d_viewdataSetup
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

; set plot menu options
;	w_plotspec_id.x_axis_u = 0
;	w_plotspec_id.type = 0
;	w_plotspec_id.log = 0
;	w_plotspec_id.grid = 0
;	w_plotspec_id.errbars = 0
;	w_plotspec_id.xcord = 0

WIDGET_CONTROL,/HOURGLASS

	; reset panimage to all 
;	scanData.sel_id = 8
;	scanData.sel_list = indgen(scanData.npd)	
	if scanData.bypass3d then scan_read,1,-1,-1,maxno,pickDet=-1 else $
	scan_read,1,-1,-1,maxno    ; plot the last scan

	if n_elements(maxno) eq 0 then begin
	r = dialog_message(['Failed to read the file:','',scanData.trashcan, $
		'','Please use File->Open... to pick a new file'],/Error)
	return
	end

	WIDGET_CONTROL,widget_ids.trashcan,SET_VALUE=scanData.trashcan
	scanData.y_scan = 0
END


PRO catch1d_viewmode, Event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

;	cadebug,-1

	w_plotspec_saveTitle
	scanData.plotspec = w_plotspec_id

	FNAME = strcompress(scanData.trashcan,/remove_all) 
	w_plotspec_id.mode = 1
	w_plotspec_id.seqno = 0
	if scanData.debug eq 1 then $
	print,'Read Scan Data from: '+ FNAME

u = findfile(FNAME) 
if u(0) eq '' then begin
w_warningtext,'Error file not found: '+FNAME
w_plotspec_id.mode = 0
return
end

;
; destroy the old w_plotspec window
;
	close_plotspec
	unit = 1
	w_viewscan, unit, GROUP=Event.top

END

PRO PDMENU_VDATA_Event, Event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus
COMMON CATCH1D_2D_COM, data_2d, gD

  CASE Event.Value OF
  'ViewData.1D/2D...': BEGIN
	if scanData.realtime eq 0 then catch1d_viewmode, Event else begin
	if !d.name eq 'WIN' then begin
		r = dialog_message('Please use the "@go_SB" to view the scan data.',/info)
	endif else begin
	if w_plotspec_id.scan eq 0 then sscan,file=scanData.trashcan 
	end
	end
	END
  'ViewData.3D_ARRAY...': BEGIN
	if scanData.bypass3d eq 0 then begin
	da3D = *(*gD).da3D
	sscan_read_pick3d,da3D	
	end
	END
  'ViewData.SSCAN(1D/2D/3D/4D)...': BEGIN
	sscan,file=scanData.trashcan
	END
  'ViewData.ASSIGN_DetName...': BEGIN
	assignname_read,new= scanData.SVERS
	END
  'ViewData.1D_OVERLAY...': BEGIN
	overlay_1d,path=scanData.path,Group=Event.top
	END
  'ViewData.1D_CALIBRA...': BEGIN
	dim = *(*gD).dim
	if dim eq 1 then begin
	scanno = *(*gD).scanno
	id_def = *(*gD).id_def
	pa1d = *(*gD).pa1D
	da1d = *(*gD).da1D
	sz = size(da1d)
	nd = sz(2)
	def = id_def(4:4+nd-1,dim-1)
	if sz(0) eq 2 then begin
	calibration_factor,da1d,def,xv=pa1d,classname=w_plotspec_array(3), $
		dnames=scanData.DI(0:nd-1), $
		title='1D OVERLAY',inpath=scanData.path, $
		format=scanData.format,GROUP=group
	end
	endif else begin
	r = dialog_message('File is not 1D scan !!',/info)
	end
	END
  'ViewData.IMAGE2D...': BEGIN
	dim = *(*gD).dim
	if dim gt 1 then begin
	scanno = *(*gD).scanno
	cpt = *(*gD).cpt
	npts = *(*gD).num_pts
	pv = *(*gD).pv
	id_def = *(*gD).id_def
	pa1d = *(*gD).pa1D
	pa2d = *(*gD).pa2D
	da1d = *(*gD).da1D
	da2d = *(*gD).da2D
	labels = *(*gD).labels
	sz = size(da2d)
	if sz(0) eq 3 then begin
	det_def = id_def(4:4+sz(3)-1,dim-2)

	; if first 70 detectors option set by user
	if scanData.svers then begin
		if sz(3) gt scanData.nd then $
		det_def = det_def(0:scanData.nd-1)
		nd = max(where(det_def > 0))
		if nd(0) eq -1 then return
		da2d = da2d(*,*,0:nd)
		det_def = det_def(0:nd)
	end
	yarr = pa1d(*,0)
	xarr = pa2d(*,w_plotspec_id.xcord)
	yr = [min(yarr),max(yarr)]
	if yr(0) eq yr(1) then yarr = indgen(sz(2))
	xr = [min(xarr),max(xarr)]
	if xr(0) eq xr(1) then xarr = indgen(sz(1))
	if dim eq 3 then pv = pv(1:2) 
	iy = 89
	if scanData.svers eq 1 then iy = 74
	ix = iy+w_plotspec_id.xcord
	xdescs = labels(ix,dim-2)
	if labels(ix+iy,dim-2) ne '' then xdescs = xdescs +' ('+labels(ix+iy,dim-2)+')'
	zdescs = labels(4:4+n_elements(det_def)-1,dim-2)
	for i=0,n_elements(det_def) - 1 do begin
	if strlen(labels(4+iy+i,dim-2)) gt 1 then $ 
	zdescs(i) = labels(4+iy+i,dim-2)
	end
	ydescs = labels(iy,dim-1)
	if labels(iy+iy,dim-1) ne '' then ydescs = ydescs +' ('+labels(iy+iy,dim-1)+')'

	xarr = xarr(scanData.imin:sz(1)-1)
	IMAGE2D,da2d(scanData.imin:sz(1)-1,*,*),xarr,yarr,id_def=det_def, $
		xdescs=xdescs,ydescs=ydescs, $
		title=scanData.trashcan, $
		zdescs=zdescs, scanno=scanno,pv=pv,group=Event.top, $
		VERS=scanData.svers
	end
	end
	END
  ENDCASE

END

PRO HELPMENU_Event, Event
  COMMON CATCH1D_COM, widget_ids, scanData

  CASE Event.Value OF 
  'Help.Version ...': BEGIN
	st = caVersion()
	st = [st,'','scanSee Version : '+scanData.release]
	w_warningtext,st
 	END
  'Help.Release Note ...': BEGIN
        private = getenv('EPICS_EXTENSIONS_PVT') + !os.file_sep +'doc' + !os.file_sep + 'scanSee.README'
        found = findfile(private)
        if found(0) ne '' then xdisplayfile,found(0),GROUP=Event.top else begin
        str = getenv('EPICS_EXTENSIONS')+!os.file_sep + 'doc'+ !os.file_sep + 'scanSee.README'
        xdisplayfile,str, GROUP=Event.top
        end
        END
  'Help.Help ...': BEGIN
        private = getenv('EPICS_EXTENSIONS_PVT') + !os.file_sep +'doc' + !os.file_sep + 'DC_help.txt'
        found = findfile(private)
        if found(0) ne '' then xdisplayfile,found(0),GROUP=Event.top else begin
        str = getenv('EPICS_EXTENSIONS')+!os.file_sep + 'doc'+ !os.file_sep + 'DC_help.txt'
        xdisplayfile,str, GROUP=Event.top
        end
 	END

  ENDCASE
END



PRO PRINTMENU_Event, Event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
  COMMON CATCH1D_2D_COM, data_2d, gD

  CASE Event.Value OF 

  'Print.Plot': BEGIN
	if scanData.lastPlot lt 0 then return
	scanData.act_npts = scanData.readin_npts
    	PS_open,'1d.ps'
    	UPDATE_PLOT, scanData.lastPlot
    	PS_close
    	PS_print,'1d.ps'
 	END
  'Print.Report ...': BEGIN
	view1d_summary_setup,GROUP=Event.top  		; pick the range

 	END
  ENDCASE

if realtime_id.ind eq 1 then realtime_id.axis = 1

END


PRO ZOOMMENU_Event, Event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON user_scale_block,user_scale_ids
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
  COMMON CATCH1D_2D_COM, data_2d, gD

if scanData.lastPlot ge 0 then begin 

if XRegistered('user_scale') ne 0 then begin
        WIDGET_CONTROL,user_scale_ids.base,/DESTROY
        end

scanData.zoom = 1
scanData.act_npts = scanData.readin_npts

  CASE Event.Value OF 

  'Zoom.Zoom To Box': BEGIN
	zoom_to_box
 	END
  'Zoom.Zoom Pan': BEGIN
	zoom_in_out1,group=Event.top
 	END
  'Zoom.Zoom In/Out': BEGIN
	zoom_in_out
	zoom_in_out1,group=Event.top
 	END
  'Zoom.Calc Slopes': BEGIN
	draw_dragLine
 	END
  'Zoom.Zoom Off (AutoScale)': BEGIN
	scanData.lastPlot = 1
	scanData.zoom = 0
	UPDATE_PLOT, 1
 	END
  'Zoom.User Scale ...': BEGIN
	scanData.lastPlot = 1
	scanData.zoom = 0
	user_scale,GROUP=event.top
	UPDATE_PLOT, 1
 	END
  ENDCASE
end

	if realtime_id.ind eq 1 then begin
                        realtime_id.ymin =0.
                        realtime_id.ymax =0.
                        realtime_id.axis = 1
		end

END

PRO STATISTICMENU_Event, Event

  COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_statistic_block,w_statistic_ids
  COMMON CATCH1D_2D_COM, data_2d, gD

  nd = scanData.npd - 4

  CASE Event.Value OF

  'Statistic.None': BEGIN
        w_plotspec_id.statistic = 0
        UPDATE_PLOT,1
	if XRegistered('w_statistic') ne 0 then begin
	WIDGET_CONTROL,w_statistic_ids.base,BAD=bad,/DESTROY
	widget_ids.statistic = 0
	end
    END

  'Statistic.Peak/Centroid/FWHM on plot': BEGIN
        w_plotspec_id.statistic = 1
        UPDATE_PLOT,1,st
	if XRegistered('w_statistic') ne 0 then begin
	WIDGET_CONTROL,w_statistic_ids.base,BAD=bad,/DESTROY
	widget_ids.statistic = 0
	end
    END

  'Statistic.Peak/Centroid/FWHM ...': BEGIN
        w_plotspec_id.statistic = 2
        UPDATE_PLOT,1,st
        if n_elements(st) gt 0 then begin
        if widget_ids.statistic eq 0 then $
                w_statistic,st,34,25,'Statistic',GROUP=Event.top $
          else WIDGET_CONTROL,widget_ids.statistic,SET_VALUE=st
        end
    END

  'Statistic.FWHM on Y.All...': BEGIN
        w_plotspec_id.statistic = 5
        num_pts = scanData.act_npts-1
        VX= (*scanData.pa)[0:num_pts,0]
        for i=0,nd-1 do begin
        IF (realtime_id.def(4+i) gt 0) THEN begin
        VY= (*scanData.da)[0:num_pts,i]
        title='FWHM of Detector '+strtrim(i+1,2) +'    SCAN # '+ strtrim(scanData.scanno,2)
        statistic_1d,VX,VY,c_mass,x_peak,y_peak,y_hpeak,fwhm,xl,xr,/plot, $
                title=title,group=Event.top
        end
        end
    END
  'Statistic.FWHM on Y.One...': BEGIN
        w_plotspec_id.statistic = 5
        num_pts = scanData.act_npts-1
        VX= (*scanData.pa)[0:num_pts,0]
        for i=0,nd-1 do begin
        IF (scanData.wf_sel(i) EQ 1 and realtime_id.def(4+i) gt 0) THEN begin
        VY= (*scanData.da)[0:num_pts,i]
        title='FWHM of Detector '+strtrim(i+1,2) +'    SCAN # '+strtrim(scanData.scanno,2)
        statistic_1d,VX,VY,c_mass,x_peak,y_peak,y_hpeak,fwhm,xl,xr,/plot, $
                report='fwhm.rpt',title=title,group=Event.top
        return
        end
        end
    END

  'Statistic.FWHM on DY/DX.All...': BEGIN
        w_plotspec_id.statistic = 6
        num_pts = scanData.act_npts-1
        VX= (*scanData.pa)[0:num_pts,0]
        for i=0,nd-1 do begin
        IF (realtime_id.def(4+i) gt 0) THEN begin
        VY= (*scanData.da)[0:num_pts,i]
        VY = slope(VX,VY)
        title='FWHM of DY/DX of Detector '+strtrim(i+1,2) + '    SCAN # '+strtrim(scanData.scanno,2)
        statistic_1d,VX,VY,c_mass,x_peak,y_peak,y_hpeak,fwhm,xl,xr,/plot, $
                title=title,group=Event.top
        end
        end
    END
  'Statistic.FWHM on DY/DX.One...': BEGIN
        w_plotspec_id.statistic = 6
        num_pts = scanData.act_npts-1
        VX= (*scanData.pa)[0:num_pts,0]
        for i=0,nd-1 do begin
        IF (scanData.wf_sel(i) EQ 1 and realtime_id.def(4+i) gt 0) THEN begin
        VY= (*scanData.da)[0:num_pts,i]
        VY = slope(VX,VY)
        title='FWHM of DY/DX of Detector '+strtrim(i+1,2) + '    SCAN # '+strtrim(scanData.scanno,2)
        statistic_1d,VX,VY,c_mass,x_peak,y_peak,y_hpeak,fwhm,xl,xr,/plot, $
                report='fwhm.rpt',title=title,group=Event.top
        return
        end
        end
    END

  'Statistic.Average/Deviation ...': BEGIN
        w_plotspec_id.statistic = 3
        UPDATE_PLOT,1,st
        if n_elements(st) gt 0 then begin
        if widget_ids.statistic eq 0 then $
        	w_statistic,st,34,25,'Statistic',GROUP=Event.top $ 
	else WIDGET_CONTROL,widget_ids.statistic,SET_VALUE=st 
	end
    END

  ENDCASE

if realtime_id.ind eq 1 then realtime_id.axis = 1

END

PRO FITTINGMENU_Event, Event
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
  COMMON CATCH1D_2D_COM, data_2d, gD

if scanData.act_npts lt 2 then begin
        w_warningtext,['No data available yet','', $
		 'You have to load 1D data in first.']
	return
end

x = (*scanData.pa)[0:scanData.act_npts-1,w_plotspec_id.xcord]
;nd = scanData.npd-4
sz = size(*scanData.da)
nd = sz(2)
y = make_array(scanData.act_npts,nd)
y(*,*) = (*scanData.da)[0:scanData.act_npts-1,0:nd-1]
WIDGET_CONTROL, widget_ids.wf_select, GET_VALUE = wf_sel
for i=0,nd-1 do begin
	if wf_sel(i) then begin
	if n_elements(jpick) eq 0 then jpick=i else jpick=[jpick,i]
	end
end

  CASE Event.Value OF

  'Fitting.Ez_Fit ...': BEGIN
        ez_fit,x=x,y=y,GROUP=Event.Top,jpick=jpick
        END
  'Fitting.1D Binary': BEGIN
	u_openw,unit,'fitting.bin1d',/XDR
	u_write,unit,x
	u_write,unit,y
	u_close,unit
        st = '1D binary data save in fitting.bin1d'
        w_warningtext,st
	END
  ENDCASE
END


PRO PDMENU127_Event, Event

  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON w_caset_block, w_caset_base, w_caset_ids, w_caset_narray, w_caset_varray

  CASE Event.Value OF 

  'File.Open ...': BEGIN
        if scanData.realtime eq 0 and scanData.scanno_2d gt 0 then begin

	catch,status_error
	if status_error ne 0 then begin
		w_warningtext,!error_state.msg+string(!error_state.code)
		widget_ids.panwin = -1
		return
	end
        if widget_ids.panwin ne -1 then $
        wdelete,widget_ids.panwin        ; delete realtime panimage window
        nowindow:
	widget_ids.panwin = -1
        end
	catch1d_append
    END

  'File.Printer ...': BEGIN
	PS_printer,GROUP=Event.Top
    END

  'File.Quit': BEGIN
	catcher_close,Event.Top
	EXIT
    END
  ENDCASE

;if realtime_id.ind eq 1 then realtime_id.axis = 1

END

PRO catcher_close,wid
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON w_caset_block, w_caset_base, w_caset_ids, w_caset_narray, w_caset_varray

    if XRegistered('w_caset') ne 0 then $
	WIDGET_CONTROL,w_caset_base,/DESTROY
	w_warningtext_close

;    IF (STRLEN(scanData.pv) NE 0) THEN begin 

	; change director error -296

	CATCH,error_status
        if error_status ne 0 then begin     ; eq -296 then begin
	w_warningtext,!err_string + string(!err)
	cd,current=p
        scanData.home = p
	print,p
        end
	write_config

;   end

;	EXIT
END




PRO w_warningtext_close
COMMON w_warningtext_block,w_warningtext_ids

	if XRegistered('w_warningtext') ne 0 then $
	WIDGET_CONTROL, w_warningtext_ids.base, /DESTROY
END



PRO DC_pickDetVersion,vers=vers,updateI=updateI
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
  COMMON CATCH1D_2D_COM, data_2d, gD
;  vers set implies 70 detectors otherwise 85 detectors assumed

  widget_control,widget_ids.wf_select,get_value=wf_sel
  if scanData.new  then begin
	scanData.nd = 70 
	scanData.svers = 1
	scanData.dstart = 0
	scanData.wf_sel = wf_sel(15:88)
  endif else begin
	scanData.nd = 85
	scanData.dstart = 15 
	scanData.wf_sel = wf_sel
  end
  scanData.npd = scanData.nd + 4

  if keyword_set(updateI) and scanData.dim ge 2 then begin
	if scanData.y_scan eq 0 then begin
	catch1d_win_2D_update
	end
  end
END



PRO catch1d_Start_yScan
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

        ; if view window is opened close it while scan is going on
        if XRegistered('w_viewscan') ne 0 then begin
                WIDGET_CONTROL,w_viewscan_ids.base,/DESTROY
                end


       	pventry2_event

;	catch1d_Start_xScan
	pventry_event
	if w_plotspec_id.realtime eq 1 then begin
		realtime_init
		end

        if strlen(scanData.y_pv) lt 1 then begin
                w_warningtext,'Enter Y SCAN Record Name first !!'
                return
                end

; check for proper setup first

if scanData.pvfound eq -1 then return
	
;	scanData.y_scan = 1
	scanData.scanno_2d = scanData.scanno_2d + 1
	if scanData.dim ge 2 then scanData.scanno_2d = scanData.filemax - 1

       scanData.y_value=0.
       ln = cagetArray(scanData.y_pv+'.P1DV',pd,/float)
       if ln eq 0 then scanData.y_value = pd(0)

	setPlotLabels

;  find new filename based on prefix and scan #

	calc_newfilename,/get,err=ferr

; update panimage required for new scan

;	  pvs = scanData.y_pv + '.CPT'
;	  id = cagetArray(pvs,pd,/short) 
;	  scanData.scanno = pd(0) + 1
	scanData.sel_changed = 1
END


PRO catch1dReadScanRecordAppendFile
COMMON CATCH1D_COM,widget_ids,scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotpec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON CATCH1D_2D_COM,data_2d, gD

if scanData.pvfound eq -1 then return
	  ln = cagetArray(scanData.pv+'.CPT',pd)
	   if ln eq 0 then act_npts = pd(0)
          WIDGET_CONTROL, widget_ids.pv_stat, $
            SET_VALUE = 'IDLE : Acquired' +STRING(scanData.act_npts)+' Pts'


	if scanData.act_npts le 1 then begin 
;	w_warningtext,'No data detected in scan record: ' + scanData.pv
	return
	end

	scanData.readin_npts = act_npts

;
; automatic save scan data
;
	F = scanData.trashcan

; spawn,'date', x, /noshell
x = catimestamp(scanData.pv+'.EXSC')
y = strupcase(getenv('USER'))
w_plotspec_array(4) = x(0) + '. User Name: ' + y
	if scanData.debug eq 1 then begin
	print,''
	print,w_plotspec_array(4)
	end

; get position and data array from the scan record

 	w_plotspec_id.scan = 0

	if scanData.y_scan eq 0 then begin
	  wait,scanData.wtime  ; allow time for mda save finished
	  if scanData.bypass3d then scan_read,1,-1,-1,maxno,pickDet=-1 else $
		scan_read,1,-1,-1,maxno
	  scanData.realtime=0
	endif else begin
	 ; fly scan
	  if scanData.fastscan  then begin
	    if scanData.bypass3d then scan_read,1,-1,-1,maxno,pickDet=-1 else $
		scan_read,1,-1,-1,maxno
	  endif else begin
	    wait,scanData.wtime  ; allow time for mda save finished
	    getPiDiArray
	    if scanData.y_seqno lt 1 then begin
	    if scanData.bypass3d then scan_read,1,-1,-1,maxno,pickDet=-1 else $
		scan_read,1,-1,-1,maxno
	    end
          end
	end

;
; update the cw_term with the final scan result
;
	if scanData.showlist eq 1 then begin
	save_scan_dump_curr,filename
	id = cw_term(widget_ids.terminal,filename=filename,/reset)
	end

;	after_sys_scan

	if scanData.y_scan then begin
		setPlotLabels 
	end
	realtime_id.ind = -1

;#####  may need to read the scan # from the data base


END



; CODE MODIFICATIONS MADE ABOVE THIS COMMENT WILL BE LOST.
; DO NOT REMOVE THIS COMMENT: BEGIN MAIN13_1

PRO MAIN13_1_Event, Event

  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
  COMMON CATCH1D_2D_COM, data_2d, gD

if scanData.option eq 1 then begin        ; if Acquisition.On is set by user

  ; this is and added CASE statement to see if the event.id
  ; is on that I assigned to a caWidgetAddmonitor ...

scanData.pv = scanData.pvconfig(0)
scanData.y_pv = scanData.pvconfig(1)

  CASE event.id OF

  widget_ids.base: BEGIN
      IF (STRLEN(scanData.pv) EQ 0) THEN  RETURN
	chkid = caSearch(scanData.pv+'.EXSC')
IF chkid eq 0 then BEGIN

;
; check whether 2D scan started by outside CA clients
;

	if strlen(scanData.y_pv) gt 0 and scanData.y_scan eq 0 then begin
	 if caSearch(scanData.y_pv+'.EXSC') eq 0 then begin 
	  pvs = scanData.y_pv + ['.EXSC','.CPT','.DATA']
	  id = cagetArray(pvs,pd,/short) 
 	    if pd(0) eq 1 and pd(1) lt scanData.y_req_npts then begin
		scanData.y_seqno = pd(1)
		catch1d_Start_yScan
	    endif else begin
		scanData.scanno = 1
	    end
	 end
	end

;   ret = caCheckMonitor(scanData.pv+'.EXSC')
    ret = caMonitor(scanData.pv+'.EXSC',valchange,/check) 
if ret eq -1 then begin                 ; ****may be error in caCheckMonitor	
	pventry_event
	end

; scan mode check the following

if w_plotspec_id.mode eq 0 then begin
if w_plotspec_id.realtime eq 1 then $
setPiDiMonitor,ret,/check 
if total(ret) gt 0 then begin
	if scanData.debug eq 1 then $
	print,ret,'Warning: Reset PV name in scan record!!!'
	if scanData.y_scan eq 0 then $	
	pventry_event
	end
end

scanFlag=0
scanDataReady = 0
if caSearch(scanData.pv+'.EXSC') eq 0 then begin
	ln = cagetArray(scanData.pv+['.EXSC','.DATA'],pd,/short)
	if ln eq 0 then scanFlag = pd(0) else scanFlag=0 ;======= 8/15/96
	scanDataReady = pd[1]
end
      IF (scanFlag EQ 1) THEN BEGIN
	;  find new filename based on prefix and scan #
	if scanData.y_scan eq 0 and valchange(0) then begin

	calc_newfilename,/get,err=ferr
	end

	ln = caMonitor(scanData.pv+'.NPTS',ret,/check)
	if ret(0) gt 0 then begin
	ln = cagetArray(scanData.pv+'.NPTS',pd)
	scanData.req_npts = pd(0) 

; if view window is opened close it while scan is going on

        if XRegistered('w_viewscan') ne 0 then begin
                WIDGET_CONTROL,w_viewscan_ids.base,/DESTROY
                end

;
; if realtime_init has not been called, must call it here
;
	if scanDataReady eq 0 and valchange(0) then begin
	if w_plotspec_id.realtime eq 1 and realtime_id.ind eq -1 $
		 then realtime_init
	realtime_xrange,1
	realtime_id.axis = 1
	end

	end

stt = 'SCANNING: ' +strtrim(scanData.act_npts,2)+ ' of '+  strtrim(scanData.req_npts,2)+ ' Pts' 
if scanData.y_scan gt 0 then stt = stt+' @ y('+strtrim(scanData.y_seqno+1,2)+')'
WIDGET_CONTROL, widget_ids.pv_stat, SET_VALUE = stt

;
;  set scan for outside CA events, e.g. medm set EXSC
;
	w_plotspec_id.scan = 1

if w_plotspec_id.realtime eq 1 then begin
	if realtime_id.ind eq -1 then begin

; if view window is opened close it while scan is going on

        if XRegistered('w_viewscan') ne 0 then begin
                WIDGET_CONTROL,w_viewscan_ids.base,/DESTROY
                end

	if scanData.y_scan eq 0 then begin
	calc_newfilename,/get,err=ferr
	end
	if scanDataReady eq 0 then realtime_init
	end

	if realtime_id.ind ne 2 then begin
		realtime_read,scanData.req_npts
;		WIDGET_CONTROL,widget_ids.base, /clear_events
		WIDGET_CONTROL,widget_ids.base, timer=w_plotspec_id.dtime
	endif else begin
;		WIDGET_CONTROL,widget_ids.base, /clear_events
		empty
	end

end
;	if scanDataReady or scanData.act_npts ge scanData.req_npts then begin
	if scanDataReady  then begin
	if  scanData.act_npts gt 1 then begin
	catch1dReadScanRecordAppendFile
if scanData.debug then $
print,'A:',scanData.act_npts,scanData.req_npts,scanData.y_seqno,valchange,scanDataReady
	goto, check2Dend
	end
	end


      ENDIF ELSE BEGIN
;
; scanFlag eq 0 case (1D scan is done)
;
if scanDataReady then begin
	if w_plotspec_id.scan eq 1 then begin
	catch1dReadScanRecordAppendFile
if scanData.debug then $
print,'B:',scanData.act_npts,scanData.req_npts,scanData.y_seqno,valchange,scanDataReady
	end

check2Dend:
;
; check whether 2D scan stopped by outside CA clients
;
	if scanData.y_scan eq 1 then begin
		id = cagetArray(scanData.y_pv+'.EXSC',pd) 
		if pd(0) eq 0 then begin
	if scanData.bypass3d then scan_read,1,-1,-1,maxno,pickDet=-1 else $
		scan_read,1,-1,-1,maxno
		scanData.y_scan = 0
;print,'stop by CA client',scanData.y_seqno,scanData.scanno,pd(0)
		end
	  end

;
; check whether to terminate the  Y-scan 
;
	if scanData.y_scan eq 1 and $
		scanData.y_seqno ge scanData.y_req_npts then begin
	if scanData.bypass3d then scan_read,1,-1,-1,maxno,pickDet=-1 else $
		scan_read,1,-1,-1,maxno
		scanData.y_scan = 0
;print,'terminate by y_req_npts'
		end

	if scanData.y_scan eq 0 and w_plotspec_id.scan eq 0  then begin
	if valchange(0) then $
		after_sys_scan
		end
	scanData.realtime=0
end
      ENDELSE
ENDIF else begin
	WIDGET_CONTROL, widget_ids.pv_stat,SET_VALUE = '>> PV NOT VALID <<'
	return
	end

  END
  ELSE:
  ENDCASE
end ;     end of if scanData.option = 1

  ; The next CASE statement is from the Widget Builder.
  ; It uses the User_value of a widget to identify itself.

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  ; Event for FILE_MENU
  'PDMENU127': PDMENU127_Event, Event

  'PDMENU_VDATA': PDMENU_VDATA_Event, Event

 ; Event for SETUP_MENU
  'SETUPOPTIONSMENU': SETUPOPTIONSMENU_Event, Event
  'PLOTOPTIONSMENU': PLOTOPTIONSMENU_Event, Event
  'HELPMENU': HELPMENU_Event, Event
;  'IMAGEMENU': IMAGEMENU_Event, Event
  'PRINTMENU': PRINTMENU_Event, Event
  'ZOOMMENU': ZOOMMENU_Event, Event
  'STATISTICMENU': STATISTICMENU_Event, Event
  'FITTINGMENU': FITTINGMENU_Event, Event
  'EZFIT_FITTING': BEGIN
	x = (*scanData.pa)[0:scanData.act_npts-1,w_plotspec_id.xcord]
	nd = scanData.npd - 4
	y = make_array(scanData.act_npts,nd)
	y(*,*) = (*scanData.da)[0:scanData.act_npts-1,0:nd-1]
	ez_fit,x=x,y=y,GROUP=Event.Top
	END
  'PICK_PANIMAGE': BEGIN
	if scanData.dim lt 2 then return
	scanData.sel_list = 0
        sublist = WIDGET_INFO(Event.ID,/DROPLIST_SELECT)
        CASE sublist OF
        0: ret = indgen(15) 
        1: ret = indgen(10) +scanData.dstart 
        2: ret = indgen(10) +scanData.dstart + 10 
        3: ret = indgen(10) +scanData.dstart + 20
        4: ret = indgen(10) +scanData.dstart + 30 
        5: ret = indgen(10) +scanData.dstart + 40 
        6: ret = indgen(10) +scanData.dstart + 50 
        7: ret = indgen(10) +scanData.dstart + 60 
        8: ret = indgen(scanData.npd-4) 
	9: ret = -1
	10: ret = indgen(scanData.npd-4) + scanData.dstart ; 15 
        ENDCASE
	if ret(0) ge scanData.nd then return
        scanData.sel_list = ret 
        scanData.sel_id = sublist 
	catch1d_win_2D_update
  	if scanData.y_scan then begin
		realtime_id.ymin=0.
		realtime_id.ymax=0.
		realtime_id.axis=1
	end
	END
  'PICK_IMAGE': BEGIN
	if scanData.scanno le 1 then return
	scanData.image = Event.Index + 1
	da2D = *(*gD).da2D
	update_2d_data,da2D
  	if scanData.y_scan then begin
		realtime_id.ymin=0.
		realtime_id.ymax=0.
		realtime_id.axis=1
	end
	END
  'PICK_IMIN': BEGIN
	widget_control,Event.ID,get_value=imin
	if imin ge scanData.req_npts-1 then begin
		imin = imin-1
		widget_control,Event.ID,set_value=imin
	end
	scanData.imin = imin
	if scanData.y_seqno gt 0 then begin
	da2D = *(*gD).da2D
	scanData.sel_changed = 1
	update_2d_data,da2D,scanData.req_npts,scanData.y_req_npts
	scanData.sel_changed = 0
  	if scanData.y_scan then begin
		realtime_id.ymin=0.
		realtime_id.ymax=0.
		realtime_id.axis=1
	end
	end
	END
  'PICK_IMINL': BEGIN
	scanData.imin = scanData.imin - 1
	if scanData.imin lt 0 then scanData.imin = 0
	widget_control,widget_ids.pickimin,set_value=scanData.imin
	if scanData.y_seqno gt 0 then begin
	da2D = *(*gD).da2D
	scanData.sel_changed = 1
	update_2d_data,da2D,scanData.req_npts,scanData.y_req_npts
	scanData.sel_changed = 0
	end
	END
  'PICK_IMINR': BEGIN
	scanData.imin = scanData.imin + 1
	if scanData.imin ge scanData.req_npts then scanData.imin = scanData.req_npts - 2 
	widget_control,widget_ids.pickimin,set_value=scanData.imin
	if scanData.y_seqno gt 0 then begin
	da2D = *(*gD).da2D
	scanData.sel_changed = 1
	update_2d_data,da2D,scanData.req_npts,scanData.y_req_npts
	scanData.sel_changed = 0
	end
	END

  'DC_AUTOSCAN': BEGIN
	user_scan_init,group=Event.top
	END
  'DC_REFRESH': BEGIN
	scanData.zoom = 0
	if w_plotspec_id.scan eq 0 then UPDATE_PLOT,1
	END
  'DC_BYPASS3D': BEGIN
	scanData.bypass3d = Event.Index
	END
  'DC_FASTSCAN': BEGIN
	scanData.fastscan = Event.Index
	END
  'DC_PICKVERS': BEGIN
	if Event.Index ne scanData.svers then begin
	scanData.svers = Event.Index
;	scanData.new = Event.Index
	DC_pickDetVersion,vers=Event.Index,/updateI
        UPDATE_PLOT, scanData.lastPlot
	end
	END
  'PICK_IPLOT1D': BEGIN
	pa = (*scanData.pa)[0:scanData.act_npts-1,w_plotspec_id.xcord]	
	da = (*scanData.da)[0:scanData.act_npts-1,0:scanData.num_det-1]
	WIDGET_CONTROL, widget_ids.wf_select, GET_VALUE = wf_sel
	iplot1d_drv,pa,da,detname=scanData.DI, $
		sel=wf_sel(0:scanData.num_det-1), $
		title=scanData.trashcan,group=Event.top
	END
  'PICK_SCANH': BEGIN 
	if scanData.dim eq 2 then begin
	scanData.scanH = Event.Index
	scan_read,1,-1,0,maxno    ; no read desired
	end
	END
  'PICK_PS': BEGIN
        ratio = .5 ^ Event.Index
        PS_open,'1d.ps',scale_factor=ratio
        UPDATE_PLOT, scanData.lastPlot
        PS_close
        PS_print,'1d.ps'
	END
  'PICK_XAXIS': BEGIN
	if scanData.zoom eq 0 then  begin    ; no zoom
	  w_plotspec_id.x_axis_u = 0
	  w_plotspec_id.xcord = 0
	  if Event.Index eq 0 then w_plotspec_id.x_axis_u = 1 else $
	  w_plotspec_id.xcord = Event.Index - 1
	  sz = size(*scanData.pa)
	  p1 = (*scanData.pa)[0:sz[1]-1,w_plotspec_id.xcord] 
	  w_plotspec_limits(0) = p1(0)
	  w_plotspec_limits(1) = p1(sz[1]-1)
	endif else  zoom_checkAxis,Event
	setPlotLabels
	if realtime_id.ind eq 1 then begin
		realtime_id.no = 0
		realtime_xrange,1,xmin,xmax
		realtime_id.axis = 1
 	endif else $	
	UPDATE_PLOT, scanData.lastPlot
	END

  'MOREORLESS': BEGIN
	WIDGET_CONTROL,Event.Id,GET_VALUE=oldvalue
	if oldvalue eq 'Less' then begin
		WIDGET_CONTROL,widget_ids.menubar_base,/DESTROY
		WIDGET_CONTROL,widget_ids.axis_base,/DESTROY
		WIDGET_CONTROL,Event.Id,SET_VALUE='More'
	endif else begin
		wid = widget_ids.base
		WIDGET_CONTROL,wid,/DESTROY
		DC,config=scanData.config
	end
	END

  'DRAW61': BEGIN
	WSET, widget_ids.plot_area
   if w_plotspec_id.scan eq 0 then begin
	if (!x.window(1) - !x.window(0)) eq 0 then begin
		w_warningtext,'Error: Plot data not established yet.'
		return
		end
; cross-hairs
      IF (Event.PRESS EQ 1) THEN BEGIN
	if XRegistered('main13_2') ne 0 then UPDATE_PLOT, scanData.lastPlot
	xy_coord, GROUP=Event.top
	xycoord
	END
   end
      END

  'BUTTON165': BEGIN
      UPDATE_PLOT, 1
      END
  'USER_SCALE': BEGIN
      UPDATE_PLOT, 0
      END

; Take care case of w_viewscan been closed by WM
  'USER_SANE': BEGIN
	if Xregistered('w_viewscan') then  $
		WIDGET_CONTROL,w_viewscan_ids.base,/DESTROY
	END

  'BGROUP145': BEGIN
	WIDGET_CONTROL, widget_ids.wf_select, GET_VALUE = wf_sel
	if scanData.svers then scanData.wf_sel = wf_sel(15:88) else $
	scanData.wf_sel = wf_sel
	if scanData.y_scan eq 0 and w_plotspec_id.scan eq 0 then begin
;		scan_read,1,-1,0,maxno    ; no read desired
;	endif else	
		UPDATE_PLOT, scanData.lastPlot 
	end

  	if scanData.y_scan or w_plotspec_id.scan then begin
		realtime_id.ymin=0.
		realtime_id.ymax=0.
		realtime_id.ind = 1
		realtime_id.axis=1
	end
      END

  ELSE:     ;don't stop of no matches
  ENDCASE

  if !d.name ne 'WIN' then $
	widget_control,Event.Id,/clear_events
END


PRO pventry2_event 
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
  COMMON CATCH1D_2D_COM, data_2d, gD
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

if XRegistered('catcher_setup') ne 0 then begin

        WIDGET_CONTROL,catcher_setup_ids.y_pv,GET_VALUE=input_string
        new_pv_name = input_string(0)

	; clear monitors on existing pv.EXSC

      IF new_pv_name ne scanData.y_pv THEN begin 
	if caSearch(scanData.y_pv+'.EXSC') eq 0 then begin
           u=caWidgetClearmonitor(scanData.y_pv+'.EXSC',widget_ids.base)
	   u = caMonitor(scanData.y_pv+'.EXSC',/clear)
	   u = caMonitor(scanData.y_pv+'.CPT',/clear)
	   u = caMonitor(scanData.y_pv+'.NPTS',/clear)
		end
	  end

scanData.y_pv = new_pv_name
end

; get the new PV

	new_pv_name = scanData.y_pv

      IF (STRLEN(new_pv_name) EQ 0) THEN res = -1  $
      ELSE res = caSearch(new_pv_name+'.EXSC')

      IF res EQ 0 THEN BEGIN
        WIDGET_CONTROL, widget_ids.pv_stat, SET_VALUE = '>> PV2 Valid <<'
	res=caWidgetSetMonitor(new_pv_name+'.EXSC',widget_ids.base,time=1.)
	u = caMonitor(new_pv_name+'.EXSC',/add)
	u = caMonitor(scanData.y_pv+'.CPT',/add)
	u = caMonitor(new_pv_name+'.NPTS',/add)

	pd=0
	pvs = scanData.y_pv + ['.NPTS','.CPT','.EXSC']
	pvs = [pvs,scanData.pv+'.NPTS']
	ln = cagetArray(pvs,pd,/short)
	if ln eq 0 then scanData.y_req_npts = pd(0)

	if pd(2) eq 1 then begin
	  scanData.y_seqno = pd(1)
	  scanData.scanno = pd(1)+1
	  scanData.y_scan = 1
	  calc_newfilename,/get,err=ferr

; create 2D data arrays

catch,error_status
if error_status ne 0 then begin
	print,!error_state
	st = ['You have to load the file in first during the middle',$
		'of multi-dimensional scan at the startup time', $
		'or you have to', $
		'Accept the scan setup dialog and then followed with', $
		'File->Open to load the current MDA file in']
	r = dialog_message(st,/info)
;	spawn,'scanSee &'
; 	exit	
	scanData.y_seqno = pd(1) 
	scanData.scanno = pd(1) + 1
	return
end

if scanData.fastscan eq 0 then begin
    if pd(1) gt 0 and pd(2) gt 0  then begin
	if n_elements(*(*gD).da2D) eq 0 then $
	scan_read,1,-1,-1,maxno,dim,pickDet=-1
        da2D = *(*gD).da2D
	scanData.scanno = scanData.y_seqno
	make_2d_data,data_2d,pd(3),pd(0),da2D
    endif else begin
	realtime_init
	make_2d_data,data_2d,pd(3),pd(0),0
    end
end

	if caSearch(scanData.pv+'.EXSC') eq 0 then begin
	if caMonitor(scanData.pv+'.EXSC',/check) ne 0 then begin
	scan_field_get,scanData.pv
	setDefaultLabels
	if w_plotspec_id.realtime eq 1 then $
	setPiDiMonitor,/add
	end
	end

;	scanData.y_seqno = 0
	if scanData.debug eq 1 then $
        print,'scanData.y_seqno=',scanData.y_seqno

	endif else begin
		scanData.y_scan = 0
		realtime_id.ind = -1
	end

      ENDIF ELSE BEGIN
        WIDGET_CONTROL, widget_ids.pv_stat,SET_VALUE = '>> PV2 NOT VALID <<'
        scanData.y_pv = ''
      ENDELSE

END


PRO pventry_event 
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

      cadebug,0

if XRegistered('catcher_setup') ne 0 then begin
; get the new PV

	WIDGET_CONTROL,catcher_setup_ids.pv,GET_VALUE=input_string
	new_pv_name = input_string(0)

; clear monitors on existing pv.EXSC

      IF (STRLEN(scanData.pv) NE 0 and new_pv_name ne scanData.pv) THEN begin 
	if caSearch(scanData.pv+'.EXSC') eq 0 then begin
           u=caWidgetClearmonitor(scanData.pv+'.EXSC',widget_ids.base)
	   u = caMonitor(scanData.pv+'.EXSC',/clear)
	   u = caScan(scanData.pv+'.CPT','',/clear)
	   u = caMonitor(scanData.pv+'.NPTS',/clear)
	if w_plotspec_id.realtime eq 1 then $
	   setPiDiMonitor,/clear
		end
	  scanData.pv = new_pv_name
	  end

end

	if scanData.pvconfig(0) ne '' then scanData.pv = scanData.pvconfig(0)
	new_pv_name = scanData.pv

w_plotspec_array(0) = new_pv_name
w_plotspec_id.scan = 0

      IF (STRLEN(new_pv_name) EQ 0) THEN res = -1  $
      ELSE res = caSearch(new_pv_name+'.EXSC')

scanData.pvfound = res

      IF res EQ 0 THEN BEGIN
        WIDGET_CONTROL, widget_ids.pv_stat, SET_VALUE = '>> PV Valid <<',BAD=bad
	res=caWidgetSetMonitor(new_pv_name+'.EXSC',widget_ids.base,time=1.)
	u = caMonitor(new_pv_name+'.EXSC',/add)
	u = caMonitor(new_pv_name+'.NPTS',/add)
	pd=0

	ln = cagetArray(scanData.pv+'.NPTS',pd)
	if ln eq 0 then scanData.req_npts = pd(0)
	scanData.realtime = 0
	realtime_id.ind = -1

        ; get type and count for  positioner & detector

	r = cagetArray(scanData.pv+'.VERS',pd)
	if pd(0) gt 5.18 then scanData.new = 1 else scanData.new = 0
	DI = scanData.DI
	if scanData.new and n_elements(DI) gt 70 then DI = DI(15:84)
	pvnames = scanData.pv +'.' + DI + 'CV'

        ln = caGetTypeCount(pvnames,types,counts,wave_types)
        scanData.x_dpt = counts
        scanData.x_dtype = wave_types

	scan_field_get,scanData.pv ;,/print
	setDefaultLabels
	setPlotLabels
	scanData.lastDet(0) = MAX(where(y_names ne '')) + 1

	if w_plotspec_id.realtime eq 1 then begin
	if n_elements(realtime_pvnames) eq 0 then setScanPvnames
	setPiDiMonitor,/add
	end

;	before_sys_scan

      ENDIF ELSE BEGIN
;	w_warningtext,'Invalid SCAN Record Name !!'
        WIDGET_CONTROL, widget_ids.pv_stat,SET_VALUE = '>> PV NOT VALID <<',BAD=bad
        scanData.pv = ''
      ENDELSE

;	WIDGET_CONTROL,widget_ids.trashcan, SET_VALUE = scanData.trashcan,BAD=bad
	
; update imax for panimage window
	widget_control,widget_ids.pickimin,set_slider_max=scanData.req_npts-1

END



PRO calc_newfilename,st,get=get,err=err
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
 
count=1
while (count lt 5)  do begin
	calc_newfilepath,pd,no,err=err
	if err ne 0 then begin
	ld = cagetArray(scanData.prefix+':saveData_scanNumber',nd)
	no = nd(0)-1
	end
	filename = pd(1) 
	if filename ne '' then begin

	  	; calculate new suffix

		l = strpos(filename,'.',/reverse_search)
		suffix = strmid(filename,l,strlen(filename)-l)
		if suffix ne scanData.suffix then scanData.suffix = suffix

	  	; check file number 
		l1 = strpos(filename,'_')
		l2 = strpos(filename,'.')
		seq = fix(strmid(filename,l1+1,l2-l1-1))
		scanData.filemax = seq

if scanData.debug then begin
print,pd,tn,' filename=',filename,', w_plotspec_array(3)=',w_plotspec_array(3)
print,callno,seq,no,ps
end

	scanData.trashcan = scanData.path + filename
	w_plotspec_array(3) = filename
	WIDGET_CONTROL,widget_ids.trashcan, SET_VALUE = scanData.trashcan
	wait,0.005 ; fly scan
;	if scanData.y_scan then $
;	if scanData.bypass3d then scan_read,1,-1,-1,maxno,dim,pickDet=-1 else $
;	scan_read,unit,-1,-1,maxno,dim
	return

	endif else begin
	print,"***saveData_message busy*** scan #",no
	wait,0.005 ; time delay appropriate for getting the new filename 
	err=-1
	if count eq 5 then return
	end
count=count+1
end

END



PRO setPiDiMonitor,ret,add=add,clear=clear,check=check
COMMON CATCH1D_COM, widget_ids, scanData

x_wd = [scanData.pv+ '.P1WD', $
	scanData.pv+'.P2WD', $
	scanData.pv+'.P3WD', $
	scanData.pv+'.P4WD', $
	scanData.pv+'.P1PP', $
	scanData.pv+'.P2PP', $
	scanData.pv+'.P3PP', $
	scanData.pv+'.P4PP' $
	]

x_dn = [ scanData.pv+'.R1PV', $
        scanData.pv+'.R2PV', $
        scanData.pv+'.R3PV', $
        scanData.pv+'.R4PV', $
        scanData.pv+'.P1PV', $
        scanData.pv+'.P2PV', $
        scanData.pv+'.P3PV', $
        scanData.pv+'.P4PV' $
	] 

; monitor first 25 detectors
DI = scanData.DI
if scanData.new and n_elements(DI) gt 70 then DI = DI(15:84)
x_dn = [x_dn, scanData.pv +'.'+DI(0:24) +'PV']

if keyword_set(check) eq 1 then begin
	ln = caMonitor(x_dn,ret,/check)
	return
	end
if keyword_set(add) eq 1 then begin
        ln = caMonitor(x_wd,/add)
	ret = caMonitor(x_dn,/add)
	ln = caMonitor(x_dn,ret,/check)
	return
	end
if keyword_set(clear) eq 1 then begin
        ln = caMonitor(x_wd,/clear)
	ret = caMonitor(x_dn,/clear)
	return
	end
END

PRO catch1d_scanInitSetup
 COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON field_block, field_max, field_name, field_name_array, field_value,  field_label_array,w_scanfield_ids
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

; check for demo mode

	if demo_mode() eq 1 then begin
	str = ['Sorry: Not able to obtain the license (Demo mode)', ''$
	]
	w_warningtext,str,75,6
	exit
	end

; initalize env arrays

; read in configuration file  

  	read_config
	if scanData.pv ne '' then begin
	if cagetArray(scanData.pv+'.VERS',pd) eq 0 then begin 
	  if pd(0) gt 5.18 then scanData.new = 1 else scanData.new = 0
 	end	
	end

  	widget_control,widget_ids.scantype,set_droplist_select=scanData.fastscan 
	if scanData.svers or scanData.new then begin
	widget_control,widget_ids.versID,set_droplist_select=1
	DC_pickDetVersion,/vers
	end

        po = strpos(w_plotspec_array(3),'.',/reverse_search)
	if po gt 0 then $
        scanData.suffix = strmid(w_plotspec_array(3),po, $
		strlen(w_plotspec_array(3))-po)

	F = scanData.path + w_plotspec_array(3)
	scanReadCheckFileType,F,scanData.suffix,ok
	if ok ne 1 then begin
	res = dialog_message(['No or wrong DC.config file found.', $
			'',scanData.path,w_plotspec_array(3), $
			'','You have to open a new scan file first!'],/Info)
	catch1d_append
	return
	end

; set init fileno
	w_viewscan_calcFilename

scanData.pvconfig(0) = scanData.pv
scanData.pvconfig(1) = scanData.y_pv
if scanData.debug eq 1 then begin
print,scanData.home
print,scanData.path
end

; set output ASCII path
outpath = scanData.path
outpath = outpath+'ASCII'+!os.file_sep
scanData.outpath = outpath
re = findfile(outpath,count=ct)

if ct eq 0 then begin
tpath1 = scanData.home+!os.file_sep
scanData.outpath = tpath1
end
print,'Outpath: ',scanData.outpath

; override the data file on command line by the setting in 
; the configuration file

	scanData.trashcan = scanData.path + w_plotspec_array(3)
	w_viewscan_id.file = scanData.trashcan

	r = dialog_message(['Is it OK to start scanSee with filename:','',$
			scanData.trashcan,''],/question)
	if r eq 'No' then return

	if scanData.option ne 0 and  strlen(scanData.pv) lt 2 then begin
	st = [ $
	'Note:  ', $
	'',$
	'      You first have to set up the scan PV name by using the', $
	'           Setup->Scan ...    menu ' $
	]
	w_warningtext,st
;	if scanData.nosave eq 0 then return
	end


	found = findfile(scanData.trashcan)
	if found(0) eq '' then begin
		st = ['Filename not found :','',scanData.trashcan, '']
		oldfile = scanData.trashcan
		calc_newfilename,/get,err=ferr
		if ferr eq 0 and oldfile ne scanData.trashcan then $
		st = [st,'been reset to :',scanData.trashcan, '', $
		'Otherwise use the "File" menu to set up the read file.'] else $
		st = [st,'Use the "File" menu to set up the read file.']
		st = [st,'Then use the "Setup" menu to set up the scan PV names.']
		mes = widget_message(st,/Error)
		if ferr ne 0 then return
		if oldfile eq scanData.trashcan then return
		end
	
WIDGET_CONTROL,/HOURGLASS

	if scanData.bypass3d then scan_read,1,-1,-1,maxno,dim,pickDet=-1 else $
	scan_read,unit,-1,-1,maxno,dim

	if n_elements(maxno) eq 0 or n_elements(dim) eq 0 then begin
	r = dialog_message(['Failed to read the file:','',scanData.trashcan, $
		'','Please use File->Open... to pick a new file'],/Error)
	return
	end

	WIDGET_CONTROL,widget_ids.trashcan, SET_VALUE = scanData.trashcan

	if scanData.y_scan  then scanData.y_seqno = maxno

	widget_control,widget_ids.pickimin,set_slider_max=scanData.req_npts-1

if scanData.option gt 0 and scanData.pv eq scanData.pvconfig(0) then begin

if strlen(scanData.y_pv) gt 1  and caSearch(scanData.y_pv+'.EXSC') eq 0 then begin
        pventry2_event
        end
if strlen(scanData.pv) gt 1  and caSearch(scanData.pv+'.EXSC') eq 0 then begin
        pventry_event
        end
end

; set plot menu options

;	w_plotspec_id.x_axis_u = 0
;	w_plotspec_id.type = 0
;	w_plotspec_id.log = 0
;	w_plotspec_id.grid = 0
;	w_plotspec_id.errbars = 0
;	w_plotspec_id.xcord = 0

;	w_plotspec_id.scan = 1

; sensitive off on  acquisition and autosave menu 
;	setupoptionsmenu_sensitive,1,0
	setupoptionsmenu_sensitive,4,0

END




PRO DC, config=config, data=data, nosave=nosave, viewonly=viewonly, GROUP=Group,Autoscan=autoscan,VERS=VERS
;+
; NAME:
;       DC
;
; PURPOSE:
;	This procedure runs the scanSee 1D/2D data displayer
;
; CALLING SEQUENCE:
;       DC [,Group=wid] [,Config=config] [,Data=data]  
;
; INPUTS:
;       None.
; KEYWORD PARAMETERS:
;       Group:       Parent group id 
;       Config:      Specifies the configuration file, default to DC.config
;       Data:        Specifies the input file name for scan data
;	AutoScan:    Start with autoscan option
;	VERS:	     Sscan 70 detectors version used if set
;
; EXAMPLE:
;
;       .run DC 
;       DC
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Oct 30, 2003.
;       10-31-2003 bkc  R3.1
; 			Support Database 5.19 drop the first 15 detectors
;       03-17-2004 bkc  R3.2
;                       fix 1D line plot color use from the 'catch1d.tbl'
;			fix image2d normalization color scheme
;       06-17-2004 bkc  R3.3
;			1D PS plot now support colored PostScript Printer
; 			Add support of 1D_OVERLAY... to overlay 1D plot of 
;			selected multiple 1D arrays from different file
;			Add support of 1D_CALIBRA... plot to manupulate 
;			1D data vectors from a selected 1D scan file 
;       10-17-2004 bkc  R3.4
;			Support 6.1 IDL
;			Add zoom->pan option
;			Fix realtime line color scheme for 24 bit devices
;			Plot1d remember previous user setting
;       02-05-2005 bkc  R3.4.1
;			Add assignname_read support in image2d
;			Add support for 70 detects in configuration file
;       03-11-2005 bkc  R3.4.3
;			Add add_capendevent,time for 3.14.7
;			Increase the time event interval in realtime
;			Separate id_def for readin/ realtime case 
;			Add save as IGOR data in image2d
;			Add spectrum energy level to view3d_2dsum
;       04-25-2005 bkc  R3.4.4 Remove restriction of 4000 dim
;       08-12-2005 bkc  R3.4.4.1 
;			Increase font size in graphic area
;                       Fix log image in image2d.pro
;			Add From 2D /1D Array report in sscan.pro
;       02-03-2006 bkc  R3.4.4.2 
;			Improve various 1D report generateion features
;			Fixed X label and discriminate the inconsistant case 
;			in realtime and MDA file
;       06-04-2013 jph  R3.4.4.3 
;                       Changes made to make this work with IDL 8.2.2
;                       These changes were made by Mark Rivers
;-
;
COMMON SYSTEM_BLOCK,OS_SYSTEM
 COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON field_block, field_max, field_name, field_name_array, field_value,  field_label_array,w_scanfield_ids
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

  if XRegistered('MAIN13_1') NE 0 then return

;Widget_Control, Default_Font= text_font

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  MAIN13_1 = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, /TLB_SIZE_EVENTS, /tracking_events, $
	/KBRD_FOCUS_EVENTS, $
;      TLB_FRAME_ATTR = 8, $
      TITLE='scanSee ( R3.4.4.3)', $
      UVALUE='MAIN13_1')

  BASE68 = WIDGET_BASE(MAIN13_1, $
	/ROW, $
      MAP=1, $
;      FRAME=2, $
;      TITLE='Top Menu Line', $
      UVALUE='BASE68')

  MenuDesc1981 = [ $
      { CW_PDMENU_S,       3, 'File' }, $ ;        0
;        { CW_PDMENU_S,       0, 'New ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Open ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Printer ...' }, $ ;        2
        { CW_PDMENU_S,       2, 'Quit' } $  ;      4
  ]

  PDMENU127 = CW_PDMENU( BASE68, MenuDesc1981, /RETURN_FULL_NAME, $
      UVALUE='PDMENU127')


  SETUPOPTIONSMENU = setupOptions( BASE68, UVALUE='SETUPOPTIONSMENU')

  PLOTOPTIONSMENU = plotOptions( BASE68, UVALUE='PLOTOPTIONSMENU')

; add the 1D/2D view memu


  MenuVData = [ $
      { CW_PDMENU_S,       3, 'ViewData' }, $ ;        0
        { CW_PDMENU_S,       0, '1D/2D...' }, $ ;        1
        { CW_PDMENU_S,       0, 'IMAGE2D...' },  $ ;        3
        { CW_PDMENU_S,       0, '1D_OVERLAY...' },  $ ;        3
        { CW_PDMENU_S,       0, '1D_CALIBRA...' },  $ ;        3
        { CW_PDMENU_S,       0, 'ASSIGN_DetName...' },  $ ;        3
        { CW_PDMENU_S,       2, 'SSCAN(1D/2D/3D/4D)...' }  $ ;        3
	]

  PDMENU_VDATA = CW_PDMENU( BASE68, MenuVData, /RETURN_FULL_NAME, $
      UVALUE='PDMENU_VDATA')

  MenuPrint = [ $
      { CW_PDMENU_S,       3, 'Print' }, $ ;        0
        { CW_PDMENU_S,       0, 'Plot' }, $ ;        1
        { CW_PDMENU_S,       2, 'Report ...' } $ ;        4
  ]
  PDMENU_print = CW_PDMENU( BASE68, MenuPrint, /RETURN_FULL_NAME, $
      UVALUE='PRINTMENU')

  MenuZoom = [ $
      { CW_PDMENU_S,       3, 'Zoom' }, $ ;        0
        { CW_PDMENU_S,       0, 'Zoom To Box' }, $ ;    1   
        { CW_PDMENU_S,       0, 'Zoom Pan' }, $ ;        2
        { CW_PDMENU_S,       0, 'Zoom In/Out' }, $ ;        2
        { CW_PDMENU_S,       0, 'Calc Slopes' }, $ ;       3
        { CW_PDMENU_S,       0, 'Zoom Off (AutoScale)' }, $ ;       4
        { CW_PDMENU_S,       2, 'User Scale ...' } $ ;       5
  ]
  PDMENU_zoom = CW_PDMENU( BASE68, MenuZoom, /RETURN_FULL_NAME, $
      UVALUE='ZOOMMENU')

; statistic menu

  MenuStatistic = [ $
      { CW_PDMENU_S,       3, 'Statistic' }, $ ;        0
        { CW_PDMENU_S,       0, 'None' }, $ ;        1
        { CW_PDMENU_S,       0, 'Peak/Centroid/FWHM on plot' }, $ ;        1
        { CW_PDMENU_S,       0, 'Peak/Centroid/FWHM ...' }, $ ;        1
        { CW_PDMENU_S,       1, 'FWHM on Y' }, $ ;        1
        { CW_PDMENU_S,       0, 'One...' }, $ ;        1
        { CW_PDMENU_S,       2, 'All...' }, $ ;        1
        { CW_PDMENU_S,       1, 'FWHM on DY/DX' }, $ ;        1
        { CW_PDMENU_S,       0, 'One...' }, $ ;        1
        { CW_PDMENU_S,       2, 'All...' }, $ ;        1
        { CW_PDMENU_S,       2, 'Average/Deviation ...' } $ ;        1
  ]

  PDMENU_statistic = CW_PDMENU( BASE68, MenuStatistic, /RETURN_FULL_NAME, $
      UVALUE='STATISTICMENU')

; fitting menu

  MenuFitting = [ $
      { CW_PDMENU_S,       3, 'Fitting' }, $ ;        0
        { CW_PDMENU_S,       0, 'Ez_Fit ...' }, $ ;        1
        { CW_PDMENU_S,       2, '1D Binary'} $ ;        1
  ]

  PDMENU_fitting = CW_PDMENU( BASE68, MenuFitting, /RETURN_FULL_NAME, $
      UVALUE='FITTINGMENU')

  MenuHelp = [ $
      { CW_PDMENU_S,       3, 'Help' }, $ ;        0
        { CW_PDMENU_S,       0, 'Version ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Release Note ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Help ...' } $ ;        1
	]

  PDMENU_help = CW_PDMENU( BASE68, MenuHelp, /RETURN_FULL_NAME, $
      UVALUE='HELPMENU')

;  bypass3d = WIDGET_DROPLIST(BASE68, VALUE=['Read3D','Bypass3D'], $
;        TITLE='', UVALUE='DC_BYPASS3D')

  scantype = WIDGET_DROPLIST(BASE68, VALUE=['CA','MDA'], $
        TITLE='', UVALUE='DC_FASTSCAN')

  detvers = WIDGET_DROPLIST(BASE68, VALUE=['85','70'], $
        TITLE='DIs:', UVALUE='DC_PICKVERS')

  BASE69 = WIDGET_BASE(MAIN13_1, $
      ROW=1, $
      MAP=1, $
      TITLE='FILE', $
      UVALUE='BASE69')

  BASE140 = WIDGET_BASE(BASE69, $
      ROW=1, $
;      FRAME=2, $
      MAP=1, $
      TITLE='STATUS', $
      UVALUE='BASE140')

  FieldVal1988 = [ $
    '>> PV NOT VALID <<' ]
  FIELD141 = CW_FIELD( BASE140,VALUE=FieldVal1988, $
      ROW=1,/NOEDIT, $
      STRING=1, $
      TITLE='Status', $
      UVALUE='FIELD141', $
      XSIZE=30)

  CATCHER_FILE = WIDGET_LABEL(BASE69, $
	/ALIGN_LEFT,/DYNAMIC_RESIZE, VALUE='')

  more_less = WIDGET_BUTTON(BASE69, VALUE='Less', $
        UVALUE='MOREORLESS')

  BASE61 = WIDGET_BASE(MAIN13_1, $
      COLUMN=1, $
      MAP=1, $
      TITLE='Plot Area', $
      UVALUE='BASE61')


  DRAW61 = WIDGET_DRAW( BASE61, $
      BUTTON_EVENTS=1, $
      RETAIN=2,  $
      UVALUE='DRAW61', $
      XSIZE=400, $
      YSIZE=300)


  BASE144 = WIDGET_BASE(MAIN13_1, $
      ROW=1, $
      MAP=1, $
      TITLE='WF Selector', $
      UVALUE='BASE144')

  BASE144_0 = WIDGET_BASE(BASE144, $
      ROW=1, X_SCROLL_SIZE=520, $
      MAP=1, $
      TITLE='DET Selector', $
      UVALUE='BASE144_0')

	detname = 'D'+ [strtrim(indgen(9)+1,2),'A','B','C','D','E','F', $
		'01','02','03','04','05','06','07','08','09', $
		strtrim(indgen(61)+10,2)]

  Btns1994 = [ detname, 'P1','P2','P3','P4']
  BGROUP145 = CW_BGROUP( BASE144_0, Btns1994, $
      ROW=9, $
      NONEXCLUSIVE=1, $
;      LABEL_LEFT='Y', $
      UVALUE='BGROUP145')

;  if keyword_set(VERS) then begin
;      BGROUP145_1 = CW_BGROUP( BASE144_0, Btns1994(15:88), $
;      ROW=9, $
;      NONEXCLUSIVE=1, $
;      LABEL_LEFT='Y', $
;      UVALUE='BGROUP145')
;      detname = detname(15:84)
;  end

  BASE144_1 = WIDGET_BASE(BASE144, $
      ROW=1, $
      FRAME=2, $
      MAP=1, $
      TITLE='Image', $
      UVALUE='BASE144_1')
  BASE144_2 = WIDGET_BASE(BASE144_1,COLUMN=1, MAP=1) 
  BASE144_3 = WIDGET_BASE(BASE144_1,COLUMN=1, MAP=1) 

  Btns913 = ['#','P1','P2','P3','P4']
  pick_xaxis = WIDGET_DROPLIST(BASE144_2, VALUE=BTNS913, $
        UVALUE='PICK_XAXIS',title='Xaxis')
  WIDGET_CONTROL,pick_xaxis,set_droplist_select = 1
  
  refresh = WIDGET_BUTTON(BASE144_2,value='Refresh',UVALUE='DC_REFRESH')
if keyword_set(autoscan) then $
  autoscan = WIDGET_BUTTON(BASE144_2,value='AutoScan...',UVALUE='DC_AUTOSCAN')

  ; add sublist panimage

l123_1 = ['D01-D15','D01-D10','D11-D20','D21-D30','D31-D40','D41-D50','D51-D60','D61-D70','ALL','NONE','D01-D70']
l123 = ['D1-DF','D01-D10','D11-D20','D21-D30','D31-D40','D41-D50','D51-D60','D61-D70','ALL','NONE','D01-D70']

  pick_panimage = WIDGET_DROPLIST(BASE144_3, VALUE=l123, $
	UVALUE='PICK_PANIMAGE',title='Images')
  WIDGET_CONTROL,pick_panimage,set_droplist_select = 8

   Btns912 = ['Small','Large','Det: '+strtrim(indgen(85)+1,2)]
   pick_image = WIDGET_LIST(BASE144_3, VALUE=BTNS912, $
        UVALUE='PICK_IMAGE',YSIZE=3)

  BASE144_4 = WIDGET_BASE(BASE144_3,/ROW, MAP=1) 
   pick_imin_l = WIDGET_BUTTON(BASE144_4,value='<',UVALUE='PICK_IMINL')
   pick_imin = WIDGET_SLIDER(BASE144_4,VALUE=0,MAXIMUM=10,TITLE='Imin', $
	UVALUE='PICK_IMIN',xsize=80)
   pick_imin_r = WIDGET_BUTTON(BASE144_4,value='>',UVALUE='PICK_IMINR')

  pick_iplot1d = WIDGET_BUTTON(BASE144_2, VALUE='IPLOT_1D',UVALUE='PICK_IPLOT1D')
  pick_PS = WIDGET_DROPLIST(BASE144_2, VALUE=['1','1/2','1/4'], $
        UVALUE='PICK_PS',TITLE='PS ratio')
  WIDGET_CONTROL,pick_PS,set_droplist_select = 1

  pick_scanH = WIDGET_DROPLIST(BASE144_2, VALUE=['scan1','scanH'], $
        UVALUE='PICK_SCANH',TITLE='1D:')

; set drawing area as wide as window width
win_state = WIDGET_INFO(MAIN13_1, /GEOMETRY)
WIDGET_CONTROL, DRAW61, DRAW_XSIZE=win_state.scr_xsize 

  WIDGET_CONTROL, MAIN13_1, /REALIZE


  ; Get drawable window index

  COMMON DRAW61_Comm, DRAW61_Id
  WIDGET_CONTROL, DRAW61, GET_VALUE=DRAW61_Id

  ; Need to create !os system variables
  @os.init

  ; Need to initialize ezcaIDL
  caInit

@DC.init

; check for vers
  if keyword_set(VERS) then begin
	widget_control,detvers,set_droplist_select = 1
	DC_pickDetVersion,/vers
  	scanData.svers = 1
  end

; get start home work directory
;  WIDGET_CONTROL,bypass3d,set_droplist_select = scanData.bypass3d

  CD,CURRENT=old_path
  scanData.home=old_path

;  default is acquisition mode now

	scanData.option = 1
	w_plotspec_id.mode = 0

if keyword_set(viewonly) then  scanData.option = 0
 
; check for input file names

if keyword_set(config) then scanData.config = config

if keyword_set(data) then w_plotspec_array(3) = data 

  catch1d_scanInitSetup

  XMANAGER, 'MAIN13_1', MAIN13_1, CLEANUP='catcher_close'
;  XMANAGER, 'MAIN13_1', MAIN13_1, CLEANUP='catcher_close',NO_BLOCK=0


END

