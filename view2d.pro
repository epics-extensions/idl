; $Id: view2d.pro,v 1.40 2002/03/21 16:51:35 cha Exp $

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
wtitle = 'Catcher Messages'
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
; view2d_datatotext.pro
;

;
;  convert byte array to strings
;
PRO BytesToStrings,inbyte,outstring,lrecl=lrecl,print=print
if n_elements(inbyte) eq 0 then begin
        print,''
        print,"BytesToStrings  routine converts a byte array to a string array"
        print,"               with the user specifyable lrecl."
        print,''
        print,"USAGE: BytesToStrings, inbyte, outstring [,lrecl=#,/print]
        print,"INPUT:"
        print,'        inbyte   - input byte array, required'
        print,'OUTPUT:'
        print,'       outstring - output string array'
        print,'KEYWORD:
        print,'       LRECL=#   - specifies the output string length,'
        print,'                   # default to 80 if not specified.'
        print,'       /PRINT    - print the string array generated'
        print,''
        return
        end
len = 80
if n_elements(lrecl) gt 0 then len = lrecl
s = size(inbyte)
no = s(1)/len
if s(1) gt (no*len) then no = no +1
outstring = make_array(no,/string,value=string(replicate(32b,len)))
for i=0,no-1 do begin
        i1 = i*len & i2 = i1 + len - 1
        if i2 gt (s(1)-1) then i2 = s(1)-1
        outstring(i) = string(inbyte(i1:i2))
        if keyword_set(print) then print,outstring(i)
        end
END

PRO subarray,data,y1,y2,x1,x2,newdata
if n_elements(data) eq 0 then begin
	print,''
	print,'SUBARRAY   extracts a subarray from a given array'
	print,''
	print,'USAGE: subarray, data, y1, y2, x1, x2, newdata'
	print,''
	print,'INPUT: 
	print,'    data     -  Input array
	print,'    y1       -  Dimension 1 start index
	print,'    y2       -  Dimension 1 end index
	print,'    x1       -  Dimension 2 start index
	print,'    x2       -  Dimension 2 end index
	print,'OUTPUT:'
	print,'    newdata  -  Extracted sub-array
	print,''
	return
	end
dx = x2 - x1 + 1
dy = y2 - y1 + 1
if dx lt 1 or dy lt 1 then begin
	print,'Error: Subarray - invalid index range!'
	return
	end
temp = make_array(dy)
newdata = make_array(dy,dx)
for j=0,dx-1 do begin
        temp = data(y1:y1+dy-1,x1+j)
        newdata(0,j)=temp
        end
END

PRO dataToText,data,px,py,title=title,unit=unit,file=file,help=help
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

if n_elements(data) eq 0 then begin
	w_warningtext,'Error: no SDS data available!',60,3
	return
	end

if keyword_set(help) then begin
	print,''
	print,'DataToText  displays the 1D or 2D data in a scrolled window.'
	print,''
	print,'USAGE: DataToText, data, title, unit, file'
	print,''
	print,'INPUT:'
	print,'     data     - Input data array to be displayed'
	print,'     px       - Input px vector to be displayed'
	print,'     py       - Input py vector to be displayed'
	print,'KEYWORD:'
	print,'     title    - Descriptive title string, defaults to NULL'
	print,'     unit     - Descriptive unit string, defaults to NULL'
	print,'     file     - Text displaying file, defaults to "view2d_data.txt"
	return
	end

filename = 'view2d_data.txt'
if n_elements(file) ne 0 then filename = file

dir = ''
if catch2d_file.path ne '' then dir = catch2d_file.path
no_dir = 1
CATCH,error_status
if error_status ne 0 then begin  ; eq -206 then begin
	if no_dir eq 1 then dir = catch2d_file.home+OS_SYSTEM.file_sep ; '/'
	if no_dir eq 2 then dir = getenv('HOME')+OS_SYSTEM.file_sep ; '/'
	no_dir = no_dir + 1 
	if no_dir gt 3 then begin
	 res = widget_message([!err_string,dir+filename],/INFO)
	 return
	end
end

openw,fw,dir+filename,/get_lun
printf,fw,';'
close,fw
;
; rename filename
;
       ino = catch2d_file.image_no(catch2d_file.scanno_current-1) $
                + catch2d_file.detector
        suf0 = '0000'
        suf = strtrim(ino,2)
        ln = strlen(suf)
        strput,suf0,suf,4-ln
        rename = catch2d_file.name+'.'+suf0

        WIDGET_CONTROL,widget_ids.textdata,BAD_ID=bad ,/DESTROY
;        if widget_ids.textdata eq 0 or bad ne 0 then $
        widget_ids.textdata = CW_TERM(widget_ids.base, $
		TITLE='VIEW2D SDS Text Window', BG_NAMES='Save As...', $
		FILENAME=dir+filename, RENAME=dir+'ASCII'+!os.file_sep+rename, $
                 XSIZE=80, YSIZE=20, /SCROLL)

s = size(data)
no = s(0)
dim = make_array(no)
dim = s(1:no)
type = s(n_elements(s)-2)

T1='' & T2=''
if n_elements(title) ne 0 then T1 = title
if n_elements(unit) ne 0 then T2 = unit 
s1 = '  data('+strtrim(dim(0),2)
for i=1,no-1 do begin
	s1 = s1 + ',' + strtrim(dim(i),2)
end
s1 = s1 + ')'

st = ['; ' + T1 + T2 + s1 ]

se = catch2d_file.image_no(catch2d_file.scanno_current-1) $
	+ catch2d_file.detector
 
f2 = str_sep(strmid(view_option.format,1,strlen(view_option.format)-1),'.')
f2 = 'I'+f2(0)

openw,fw,dir+filename,/get_lun

printf,fw,'; 2D SCAN #  ',strtrim(catch2d_file.scanno_current,2) + $
                ',    Image seqno = ' + strtrim(se,2) + ',    Detector = '+ $
                strtrim(catch2d_file.detector,2)
printf,fw,st
printf,fw, '; ------------------------------'

;
; BYTE type data
;
	if type eq 1 then begin
	if no eq 1 then begin 
		BytesToStrings,data,outdata,lrecl=80 
		printf,fw,outdata
	endif else begin 
		newdata = string(data)
		for i=0,dim(1)-1 do printf,fw,newdata(i)
	end
	free_lun,fw
;	xdisplayfile,filename
	id = CW_TERM(widget_ids.textdata,filename=dir+filename,rename=rename,/reset)
	return
	end
;
;  other type 
;
if no eq 1 then begin
	f1 = '(I,'+view_option.format+')'
	for j=0,dim(0)-1 do begin
	printf,fw,format=f1,j,data(j)
	end
	free_lun,fw
;	xdisplayfile,filename
	id = CW_TERM(widget_ids.textdata,filename=dir+filename,rename=rename,/reset)
	return
end


if no eq 2 then begin
	f0 = '(";              (yvalues)",'+ '5000('+view_option.format+',:))'
	if n_elements(py) gt 0 then printf,fw,format=f0,py
	if n_elements(py) gt 0 then begin
		f1 = '('+view_option.format+',I,'+strtrim(dim(1),2)+'('+view_option.format+'))' 
		f0 = '(";                   \ Y",'+strtrim(dim(1),2)+f2+',/,";                  X \",/,";      (xvalues)")'
		endif else begin
		f0 = '(";    \ Y",'+strtrim(dim(1),2)+f2+',/,";   X \",/)'
		f1 = '(I,'+strtrim(dim(1),2)+'('+view_option.format+'))' 
		end
	printf,fw,format=f0,indgen(dim(1))
	newdata = transpose(data)
	d1 = dim(1)
	d2 = dim(0)
	temp = make_array(dim(1))
	for j=0,d2-1 do begin
	temp = newdata(0:d1-1,j)
	if n_elements(px) gt 0 then printf,fw,format=f1,px(j),j,temp else $
		printf,fw,format=f1,j,temp
	end
	free_lun,fw
	
	id = CW_TERM(widget_ids.textdata,filename=dir+filename,rename=rename,/reset)
	return
end


if no eq 3 then begin
	f0 = '("J =    ",'+strtrim(dim(1),2)+'I10,/)'
	f1 = '(I,'+strtrim(dim(1),2)+''+view_option.format+')'
	ij=dim(0)*dim(1)
	newdata = make_array(dim(0),dim(1))
	for k=0,dim(2)-1 do begin
	printf,fw,''
	printf,fw,'K = ',strtrim(k+1,2)
	printf,fw,format=f0,indgen(dim(1))
		k1 = ij * k
		k2 = ij - 1 + k1 
	d1=dim(0)-1
	d2=dim(1)-1	
	newdata(0:d1,0:d2)=data(k1:k2)
	new = transpose(newdata)
	d1 = dim(1)
	d2 = dim(0)
	for j=0,d2-1 do begin
	j1=j*d1
	j2 = j1+d1-1	
	x1 = new(j1:j2)
	printf,fw,format=f1,j,x1
	end
	end
	free_lun,fw

	id = CW_TERM(widget_ids.textdata,filename=dir+filename,rename=rename,/reset)
	return
end

END


PRO view2d_datatotext,filename=filename
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref

; check for user range
                xdim = catch2d_file.x_act_npts
                ydim = catch2d_file.y_act_npts
                xdim = catch2d_file.width
                ydim = catch2d_file.height

                x_min=0
                x_max=xdim-1
		y_min=0
                y_max=ydim-1

        if view_option.user eq 1 then begin
                if view_option.x_min gt x_min and view_option.x_min lt x_max then x_min = view_option.x_min
                if view_option.x_max lt x_max and view_option.x_max gt x_min then x_max = view_option.x_max
                if view_option.y_min gt y_min and view_option.y_min lt y_max then y_min = view_option.y_min
                if view_option.y_max lt y_max and view_option.y_max gt y_min then y_max = view_option.y_max

                newimage = image(x_min:x_max,y_min:y_max)
        endif else begin
                newimage = image
                end

                x = catch2d_file.xarr(0:catch2d_file.width-1)
                y = catch2d_file.yarr(0:catch2d_file.height-1)
                ix = n_elements(x)
                iy = n_elements(y)

                        if x_max lt ix then ix=x_max
                        if y_max lt iy then iy=y_max
                        newim = image(x_min:ix,y_min:iy)
                        nx=x(x_min:ix)
                        ny=y(y_min:iy)
		if n_elements(filename) eq 0 then $
			dataToText,newim,nx,ny else $
			dataToText,newim,nx,ny,file=filename

	
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
	w_warningtext,st,60,5,'VIEW2D Messages' 
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
		w_warningtext,st,60,5,'VIEW2D Messages' 
		return
	end

	y_min = fix(y_min)
	y_max = fix(y_max) - 1

	if y_min lt 0 then y_min = 0
	if y_max ge y_size then y_max = y_size - 1

	y_vec = image(x, y_min:y_max)
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
	w_warningtext,st,60,5,'VIEW2D Messages' 
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
		w_warningtext,st,60,5,'VIEW2D Messages' 
		return
	end

	x_min = fix(x_min)
	x_max = fix(x_max) - 1

	if x_min lt 0 then y_min = 0
	if x_max ge x_size then x_max = x_size - 1

	x_vec = image( x_min:x_max,y)
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
	w_warningtext,'Cursor outside the image range',60,5,'VIEW2D Messages'
	return
	end

;  find vectior values

zv = image(x+x_min,y+y_min)
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

	x_vec = image(x_min:x_max,y + y_min) 
	y_vec = image(x + x_min, y_min:y_max)

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
	w_warningtext,st,60,5,'VIEW2D Messages',xloc=500
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
;if error_status eq -324 then begin
	st = ['Click MMB in the 2D image area first!','Before press the YZ button.']
	w_warningtext,st,60,5,'VIEW2D Messages',xloc=500
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
	x_max = fix(x_max); - 1
	if x_min lt 0 then x_min = 0
	if x_max ge x_size then x_max = x_size - 1

	; get y plot range

	WIDGET_CONTROL,widget_ids.y_min,GET_VALUE=y_min
	WIDGET_CONTROL,widget_ids.y_max,GET_VALUE=y_max
	y_min = fix(y_min)  
	y_max = fix(y_max); - 1 
	if y_min lt 0 then y_min = 0
	if y_max ge y_size then y_max = y_size - 1

	; real mag factor

	rx_mag = catch2d_file.x_mag
	ry_mag = catch2d_file.y_mag

	x = fix( float(x-view_option.margin_l) / rx_mag) 
	y = fix( float(y-view_option.margin_b) / ry_mag)

	catch,error_status
	if error_status ne 0 then begin
	w_warningtext,'Cursor outside the image range',60,5,'VIEW2D Messages'
	return
	end

;  find vectior values

zv = image(x+x_min,y+y_min)
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

	x_vec = image(x_min:x_max,y + y_min) 
	y_vec = image(x + x_min, y_min:y_max)

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
;print,'x,y,zval',x,y, image(x,y)

	zv = image(x,y)
	if view_option.versus then begin
		xv = catch2d_file.xarr(x)
		yv = catch2d_file.yarr(y)
	endif else begin
		xv = x
		yv = y
	end
WIDGET_CONTROL,widget_ids.x_cursor,SET_VALUE=strtrim(xv,2) ;+ '(*)'
WIDGET_CONTROL,widget_ids.y_cursor,SET_VALUE=strtrim(yv,2) ;+ '(*)'
WIDGET_CONTROL,widget_ids.z_cursor,SET_VALUE=strtrim(zv,2) ;+ '(*)'
endif else begin
	w_warningtext,'Cursor outside the image range',60,5,'VIEW2D Messages'
	end

END
PRO view2d_normalize_accept,pick_i
COMMON CATCH2D_FILE_BLOCK, catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON w_warningtext_block,w_warningtext_ids

	view_option.pick_ref = pick_i
	view_option.fullcolor = 2

	save_seqno = catch2d_file.seqno
        scanno = catch2d_file.scanno_current
        if scanno le 0 then begin
                st = 'You have to load the scan # in first'
                w_warningtext,st, 60,3,title='VIEW2D Messages'
                return
                end
        begin_seqno = catch2d_file.image_no(scanno-1)
        end_seqno = catch2d_file.image_no(scanno)
        seqno = begin_seqno + view_option.pick_ref - 1
        if seqno lt end_seqno and seqno ge begin_seqno then begin
                catch2d_file.seqno = seqno
                if XRegistered('w_warningtext') then $
                WIDGET_CONTROL,w_warningtext_ids.base,BAD=bad,/DESTROY

		unit = catch2d_file.opened
		seqno = catch2d_file.seqno
		point_lun,unit,catch2d_file.fptr(seqno)
		scanimage_read_record,catch2d_file.opened

		view_option.r_k_max = MAX(image)
		view_option.r_k_min = MIN(image)

		image_ref = image

        endif else begin
                st = [ $
                'No more image for SCAN #' + string(scanno), $
                'Total number of images for this scan is ' + $
                string(end_seqno - begin_seqno) ]
                w_warningtext,st, 60,5,title='VIEW2D Messages'
		return
        end
	REPLOT
	catch2d_file.seqno = save_seqno
	point_lun,unit,catch2d_file.fptr(catch2d_file.seqno)
	scanimage_read_record,catch2d_file.opened,/view
END

PRO view2d_normalize_Event, Event
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON VIEW2D_NORM_BLOCK, norm_ids

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'NORM_PICKED': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=i
	WIDGET_CONTROL,norm_ids.norm,SET_VALUE=2
	view2d_normalize_accept, i
      END
  'NORM_COLOR_SCHEME': BEGIN
	view_option.fullcolor = Event.value
	if view_option.fullcolor lt 2 then begin
	   zmin = view_option.z_min
   	   zmax = view_option.z_max
	   if view_option.fullcolor eq 1 then begin
		zmin = view_option.u_k_min
		zmax = view_option.u_k_max
	   end
           WIDGET_CONTROL,widget_ids.z_min,SET_VALUE=zmin
           WIDGET_CONTROL,widget_ids.z_max,SET_VALUE=zmax
	   REPLOT
	endif else begin
   	   WIDGET_CONTROL,norm_ids.pick,GET_VALUE=i
	   view2d_normalize_accept, i
	end
      END
  'NORM_HELP': BEGIN
	st=['Currently, there are three modes of image color scheme available:', $
	'', '    Auto_Scaled - Automatically scaled by the z-max, z-min of the image data.', $
	'', '    User_Scaled - User settable interest range of z-min, z-max.', $
	'', '    Normalized - Value normalized against the value of the selected detector.', $
	'', $
	'It defaults to the Auto Scaled scheme. The title label reflects current scheme', $
	'is used for the TV image.', '', $
	'If the User Scaled color scheme is selected, then a user can modify the Zmin and Zmax', $
	'fields of the main window.', '', $ 
	'If the detector number field is entered with a <CR>, it automatically', $
	'turns on the normalized color scheme mode. The entered # will be the reference detector.', $
	'The normalized value of the reference detector itself will be a uniform 1.', $
	'', $
	 'The z-min and z-max corresponding to different schemes are all shown in this dialog window.', $
	'' $
	 ] 
	res=WIDGET_MESSAGE(st)
	END
  'NORM_CANCEL': BEGIN
	WIDGET_CONTROL,widget_ids.norm_base,/DESTROY
	widget_ids.norm_base = 0L
      END
  ENDCASE
END

PRO view2d_normalize_setvalue
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON VIEW2D_NORM_BLOCK, norm_ids

      mode = 'Normalized Against ...'
   if view_option.fullcolor eq 0 then mode = 'Auto Scaled'
   if view_option.fullcolor eq 1 then mode = 'User Scaled'

      st1='Auto Scaled (Max  Value) :' + string(view_option.z_max)
      st2='Auto Scaled (Min  Value) :' + string(view_option.z_min)
      st3='User Scaled (Upper Bound) :' + string(view_option.u_k_max)
      st4='User Scaled (Lower Bound) :' + string(view_option.u_k_min)
      st5='Ref Detector '+strtrim(view_option.pick_ref,2) + $
		' (Max Value) :' + string(view_option.r_k_max)
      st6='Ref Detector '+strtrim(view_option.pick_ref,2) + $
		' (Min Value) :' + string(view_option.r_k_min)

	WIDGET_CONTROL,norm_ids.mode,SET_VALUE=mode
	WIDGET_CONTROL,norm_ids.lb1,SET_VALUE=st1
	WIDGET_CONTROL,norm_ids.lb2,SET_VALUE=st2
	WIDGET_CONTROL,norm_ids.lb3,SET_VALUE=st3
	WIDGET_CONTROL,norm_ids.lb4,SET_VALUE=st4
	WIDGET_CONTROL,norm_ids.lb5,SET_VALUE=st5
	WIDGET_CONTROL,norm_ids.lb6,SET_VALUE=st6
END

PRO view2d_normalize, GROUP=Group
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON VIEW2D_NORM_BLOCK, norm_ids

if XRegistered('view2d_normalize') then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  view2d_normalize = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      TITLE='VIEW2D (Image Color Scheme)', $
      UVALUE='view2d_normalize')

  BASE2 = WIDGET_BASE(view2d_normalize, $
      COL=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
      FONT='-dt-application-bold-r-normal-sans-20-140-100-*', $
      UVALUE='LABEL3', /DYNAMIC_RESIZE, $
      VALUE='Auto Scaled')

  color_schemes = [ $
	'AutoScaled', $
	'UserScaled', $
	'Normalized' $
	]
  NORM_COLOR_SCHEME = CW_BGROUP(BASE2, color_schemes, $
	ROW=1, /EXCLUSIVE, /NO_RELEASE, $
;	LABEL_LEFT='Color Scheme', $
	UVALUE='NORM_COLOR_SCHEME')

  norm_min = WIDGET_LABEL( BASE2, $
      UVALUE='NORM_MIN', /ALIGN_LEFT,$
      VALUE='Auto Scaled (Min Value) :' + string(view_option.z_min))

  norm_max = WIDGET_LABEL( BASE2, $
      UVALUE='NORM_MAX', /ALIGN_LEFT,$
      VALUE='Auto Scaled (Max  value) :' + string(view_option.z_max))

  BASE5 = WIDGET_BASE( BASE2, COL=1)   ;/FRAME)
  norm_lower = WIDGET_LABEL( BASE5, $
      UVALUE='NORM_LOWER', /ALIGN_LEFT,$
      VALUE='User Scaled (Lower Bound) :' + string(view_option.u_k_min))

  norm_upper = WIDGET_LABEL( BASE5, $
      UVALUE='NORM_UPPER', /ALIGN_LEFT,$
      VALUE='User Scaled (Upper Bound) :' + string(view_option.u_k_max))

  BASE3 = WIDGET_BASE( BASE2, COLUMN=1)  ;/FRAME)
  FieldVal1198 = [ $
    '1' ]
  norm_picked = CW_FIELD( BASE3,VALUE=FieldVal1198, $
      ROW=1, $
      INTEGER=1, /return_events, $
      TITLE='Normalize Against Detector:', XSIZE=2, $
      UVALUE='NORM_PICKED')

  norm_ref_lower = WIDGET_LABEL( BASE3, $
      UVALUE='NORM_LOWER', /ALIGN_LEFT,$
      VALUE='Reference (Min Value) :' + string(view_option.r_k_min))

  norm_ref_upper = WIDGET_LABEL( BASE3, $
      UVALUE='NORM_UPPER', /ALIGN_LEFT,$
      VALUE='Reference (Max Value)) :' + string(view_option.r_k_max))

  BASE4 = WIDGET_BASE( BASE2, ROW=1)

  NORM_HELP = WIDGET_BUTTON( BASE4, $
      UVALUE='NORM_HELP', $
      VALUE='Help')

  NORM_CANCEL = WIDGET_BUTTON( BASE4, $
      UVALUE='NORM_CANCEL', $
      VALUE='Done')

  norm_ids = { $
	mode : LABEL3, $
	lb1 : norm_max, $
	lb2 : norm_min, $
	lb3 : norm_upper, $
	lb4 : norm_lower, $
	lb5 : norm_ref_upper, $
	lb6 : norm_ref_lower, $
	norm : norm_color_scheme, $
	pick : norm_picked $
	}

  WIDGET_CONTROL, norm_ids.norm, SET_VALUE=0
  widget_ids.norm_base = view2d_normalize
  view2d_normalize_setvalue

  WIDGET_CONTROL, view2d_normalize, /REALIZE

  XMANAGER, 'view2d_normalize', view2d_normalize,/NO_BLOCK
END
PRO scan2dROIRpt,scanno,Ref=Ref,roifile=roifile,rptfile=rptfile,header=header,comment=comment,append=append
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2d_IMAGE, widget_ids, view_option, image, image_ref
COMMON STATISTIC_2DBLOCK, statistic_2dids

unit = catch2d_file.opened
seq = catch2d_file.scanno_current
if n_elements(scanno) then seq = scanno 

if unit le 0 then begin
	res=WIDGET_MESSAGE('Error: no image file loaded in')
	return
end
if seq lt 1 or seq gt catch2d_file.scanno_2d_last then begin
	res=WIDGET_MESSAGE('Error: outside range 2D scanno entered')
	return
end

	seqno = catch2d_file.image_no(seq-1)
;	scanimage_current
xdim = catch2d_file.width 
ydim = catch2d_file.y_req_npts

	point_lun, unit, catch2d_file.fptr(seqno)
	image_array  = make_array(xdim,ydim,15,/float)
	def = make_array(15,value=0)

	scanno_2d = seq
	for i=0,14 do begin
		if EOF(unit) eq 1 then goto,update
		u_read, unit, pvs
		u_read, unit, nos
		if nos(4) ne seq then goto,update
		u_read, unit, x
		u_read, unit, y
		u_read, unit, t_image
		def(nos(3)) = 1
		image_array(*,*,nos(3)) = t_image
	end

; pops up roi images

update:
	header_ass = ''
	comment_ass = ''
	if keyword_set(header) then header_ass=header
	if keyword_set(comment) then comment_ass=comment

	nodet = 15
	pick = 1

	if keyword_set(ref) then pick=ref	
	if pick gt 0 and pick le nodet then im_ref = image_array(*,*,pick-1) else begin
		res = dialog_message('Invalid reference detector # picked',/error)
		return
		end

	xarr = catch2d_file.xarr(0:xdim-1)
	yarr = catch2d_file.yarr(0:ydim-1)

	reportname =view_option.rptfile;   'tmproi.rpt'
	if keyword_set(rptfile) then reportname=rptfile

	if view_option.roifile eq '' then begin
	f = dialog_pickfile(path=statistic_2dids.roipath,filter='*roi.xdr*',title='Pick ROI definition File',/READ)
	if f eq '' then return
	found = findfile(f)
	if found(0) eq '' then begin
		res = dialog_message(['Filename:',f, 'not found!'],/info)
		return
	end
	view_option.roifile = f
	end
	; read roi

	xrange=[0,xdim-1]
	yrange=[0,ydim-1]
	filename=view_option.roifile ; 'tmproi.xdr'
	if keyword_set(roifile) then filename=roifile
	if statistic_2dids.back eq 2 then filename=filename+'.poly'
	found = findfile(filename)
	if found(0) eq '' then begin
		res = dialog_message(['ROI filename',filename, 'not found.', $
			'','The whole range is assumed'],/info)
	endif else begin

	if statistic_2dids.back eq 0 then begin
	xdr_open,unit,filename
	xdr_read,unit,x
	xdr_close,unit
	xrange=fix([x(0)/x(4),x(1)/x(4)])
	yrange=fix([x(2)/x(5),x(3)/x(5)])
	end
	if statistic_2dids.back eq 1 then begin
		lower_b = statistic_2dids.backave
		upper_b = statistic_2dids.backave2
	end
	if statistic_2dids.back eq 2 then begin
		 statistic_2dReadPolyROI,statistic_2dids,xverts,yverts,xv,yv,arr
	end

	if xrange(1) ge xdim then xrange(1) = xdim -1
	if yrange(1) ge ydim then yrange(1) = ydim -1

	if statistic_2dids.refresh eq 1 then begin
		xrange= statistic_2dids.xrange
		yrange= statistic_2dids.yrange
	end
	end

	if keyword_set(append) then $
        openw,1,reportname,ERROR=err,/append else $
        openw,1,reportname,ERROR=err
        IF (err NE 0) then begin
		PRINTF, -2, !ERR_STRING
		close,1
		return
	end
	printf,1,'===================================================='
	printf,1,'Generated at: ',systime(0)
        printf,1,'Header: ',header_ass
        printf,1,'Comment: ',comment_ass
	if statistic_2dids.refresh eq 0 then $
	printf,1,'ROIfile: ',filename else $
	printf,1,'ROIfile: [Temporary ROI specified]'
;	printf,1,'Rptfile: ',view_option.rptfile
	printf,1,''

	for i=0,15-1 do begin
	if def(i) then begin
	;
        printf,1,'Detector #: ', i+1
		im = image_array(*,*,i)
		if keyword_set(ref) then im = image_array(*,*,i)/im_ref	
		if statistic_2dids.back eq 2 then begin
			nelem = n_elements(arr)
	if nelem eq 0 then begin
		r = dialog_message('You have to define polygon ROI first',/info)
		close,1
		return
	end
			temp = make_array(nelem)
                	for ij=0,nelem-1 do begin
			j = arr(ij) / xdim
			k = arr(ij) MOD xdim
			temp(ij) = im(k,j)
			end
		endif else begin
		nelem = (xrange(1)-xrange(0)+1)*(yrange(1)-yrange(0)+1)
		temp = im[xrange(0):xrange(1),yrange(0):yrange(1)]
		end
		result = moment(temp,mdev=mdev,sdev=sdev)
		temp_max = max(temp)
		temp_min = min(temp)
		total = total(temp)
		ave = result[0]

	; write  report
	if statistic_2dids.back eq 2 then begin
		printf,1,'ROI defined by polygon'
		printf,1,'Xverts index:',xv
		printf,1,'Yverts index:',yv
	endif else begin
        printf,1,'ROI in index: [',strtrim(xrange(0),2),':', $
                strtrim(xrange(1),2),', ', $
                strtrim(yrange(0),2),':', $
                strtrim(yrange(1),2),'] '
        printf,1,'ROI in values: [',strtrim(catch2d_file.xarr(xrange(0)),2),':', $
                strtrim(catch2d_file.xarr(xrange(1)),2),', ', $
                strtrim(catch2d_file.yarr(yrange(0)),2),':', $
                strtrim(catch2d_file.yarr(yrange(1)),2),'] '
	end

        printf,1,'ave = ',ave
        printf,1,'dev = ',sdev
        printf,1,'min = ',temp_min
        printf,1,'max = ',temp_max
        printf,1,'total = ',total
        printf,1,'nelem = ',nelem
        printf,1,''
	end
	end
        close,1

	xdisplayfile,reportname
END


PRO PDMENU2D_ROI_event, Event
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON STATISTIC_2DBLOCK, statistic_2dids

if n_elements(image) eq 0  then begin
	w_warningtext,['No image data found','','You have to load the 2D data first']
	return
end

	header=[ catch2d_file.path+catch2d_file.name, $ 
	   'Image Seq # '+strtrim(catch2d_file.seqno+1,2)+ ',  2D Scan # '+ $
		strtrim(catch2d_file.scanno_current,2) + ',  Detector # '+ $
		strtrim(catch2d_file.detector,2) ]
	comment=''
	if view_option.fullcolor eq 2 then $
	comment='Normalized against detecor '+strtrim(view_option.pick_ref,2)

	if XRegistered('scan2d_ROI')  then  $
		WIDGET_CONTROL,statistic_2dids.base,/DESTROY,BAD=bad
	mode=0
	if n_elements(statistic_2dids) then begin
	  mode = statistic_2dids.back
	  if view_option.roifile ne statistic_2dids.file then $
	  view_option.roifile = statistic_2dids.file
	  if view_option.rptfile ne statistic_2dids.rpt then $
	  view_option.rptfile = statistic_2dids.rpt
	end

	x = catch2d_file.xarr(0:catch2d_file.width-1)
	y = catch2d_file.yarr(0:catch2d_file.height-1)
	im=image(0:catch2d_file.width-1, 0:catch2d_file.height-1)	

	if view_option.rptfile eq '' then $
		view_option.rptfile = catch2d_file.home+!os.file_sep+'ROI'+!os.file_sep+$
			catch2d_file.name+'_roi.rpt'
	if view_option.roifile eq '' then $
		view_option.roifile = catch2d_file.home+!os.file_sep+'ROI'+!os.file_sep+$
			catch2d_file.name+'_roi.xdr'

	if n_elements(statistic_2dids) eq 0 then $
	scan2d_roi,im,x,y,GROUP=Event.Top,header=header,comment=comment, $
		mode=mode,rptfile=view_option.rptfile, $
		roifile=view_option.roifile,roi_data=statistic_2dids

;	if statistic_2dids.back eq 1 then begin
;		st=[ $
;		'2D-ROI report for all detectors is not available yet for this mode.', $
;		'You have to use the AppendRpt...  button in', $
;		'the "2D Statistic ROI" window to add current report', $
;		'for each detector for          ROI Mode: FilterROI ' $
;		]
;		res=dialog_message(st,/info)
;		return
;	end 

  CASE Event.Value OF
  '2D-ROI.Type.RectROI': BEGIN
	statistic_2dids.back = 0
	return
        END
  '2D-ROI.Type.FilterROI': BEGIN
	statistic_2dids.back = 1
	return
        END
  '2D-ROI.Type.PolyROI': BEGIN
	statistic_2dids.back = 2
	return
        END
  '2D-ROI.AppendRpt...': BEGIN
	F = view_option.rptfile
;	f = dialog_pickfile(path=statistic_2dids.rptpath,filter='*rpt*',title='Append ROI Rpt File',/READ)
	if f eq '' then return
	found = findfile(f)
	if found(0) eq '' then begin
		res = dialog_message(['Filename:',f, 'not found will be created!'],/info)
	end

	st = ['New 2D-ROI statistic report will be calculated.', $
		'If you enter Yes, then the new results will be appended.', $
		'','Appending ',F , ' ???']
	res = dialog_message(st,/question)
	if res eq 'No' then return

;	view_option.rptfile = f
	if view_option.fullcolor eq 2 then $
	scan2dROIRpt,catch2d_file.scanno_current, $
		header=header, comment=statistic_2dids.comment, $
		Ref=view_option.pick_ref,/append else $
	scan2dROIRpt,catch2d_file.scanno_current,/append , $
		header=header, comment=statistic_2dids.comment

        END
  '2D-ROI.ReplaceRpt...': BEGIN
        F = view_option.rptfile
;       f = dialog_pickfile(path=statistic_2dids.rptpath,filter='*rpt*',title='Replace ROI Rpt File',/READ)
        if f eq '' then return
        found = findfile(f)
        if found(0) eq '' then begin
                res = dialog_message(['Filename:',f, 'not found will be created!'],/info)
        endif else begin
        st = ['Are you sure you want to overwrite this file ?', $
                'If you enter Yes, then all the old text contents', $
                'in this file will be lost.', $
                '','Replacing ',F , ' ???']
        res = dialog_message(st,/question)
        if res eq 'No' then return
        end

;       view_option.rptfile = f
        if statistic_2dids.comment ne '' then comment=statistic_2dids.comment
        if view_option.fullcolor eq 2 then $
        scan2dROIRpt,catch2d_file.scanno_current, $
                header=header, comment=statistic_2dids.comment, $
                Ref=view_option.pick_ref else $
        scan2dROIRpt,catch2d_file.scanno_current, $
                header=header, comment=statistic_2dids.comment
        END
  '2D-ROI.ViewRpt...': BEGIN
	f = dialog_pickfile(path=statistic_2dids.rptpath,filter='*rpt*',title='View ROI Rpt File',/READ)
	if f eq '' then return
	found = findfile(f)
	if found(0) eq '' then begin
		res = dialog_message(['Filename:',f, 'not found!'],/info)
		return
	end
;	view_option.rptfile = f
	xdisplayfile,f
        END
  '2D-ROI.RenameRpt...': BEGIN
	old = view_option.rptfile
	rename_dialog,catch2d_file.home+!os.file_sep+'ROI',old,'',GROUP=Event.top
        END
  '2D-ROI.ROI...': BEGIN
        scan2d_roi,im,x,y,GROUP=Event.Top,header=header,comment=comment, $
                mode=mode,rptfile=view_option.rptfile, $
                roifile=view_option.roifile,roi_data=statistic_2dids
        END
  '2D-ROI.Help...': BEGIN
        st = [ $
                'In general the 2D ROI reports generated by 2D-ROI menu',$
                'in vw2d consist of all detectors defined in a given 2D scan.',$
                '',$
                '           Options of 2D-ROI Menu', '', $
                'Help...           - Show this help info ', $
                'ROI...            - Pops up 2D Statistic ROI program', $
                'Type->RectROI     - Set the type of ROI used in the summary report', $
                '       FilterROI', $
                '        PolyROI', $
                'AppendRpt...      - Append 2D statistic summary report of all',$
                '                       detectors to the report file', $
                '                       for a given 2D scan ROI',$
                'ReplaceRpt...     - Overwrite 2D statistic report file', $
                '                       with the summary of all the detectors',$
                '                       with ROI as show in 2D Statistic ROI window',$
                'ViewRpt...        - Select and view any 2D statistic report', $
                'RenameRpt...      - Rename the rpt file to a new name', '',$
                '',$
                'If the detailed 2D ROI reports for a specified detector, or', $
                'refining of the ROI are desired, a user should run the ',$
                '"2D Statistic ROI" program first which can be brought up by ',$
                '','          2D-ROI->ROI...   ', '', $
                'The AppendRep... button in "2D Statistic ROI" window generates', $
                'detail report for the displayed image.', '', $
                'For file management simplicity the 2D ROI statistic report ', $
                'should end with roi.rpt and the region of interest file ',$
                'for rectangle or polygon ROI should end with roi.xdr ' $
                ]
        xdisplayfile,text=st,title='Vw2d Help on 2D-ROI'

	END
  ENDCASE

END
;
; catch2d.pro
;

@u_read.pro
@PS_open.pro
@cw_term.pro
@plot1d.pro
@plot2d.pro
@scan2d_roi.pro

PRO view2d_pan_images_on,Event,tiff=tiff,xdr=xdr,rtiff=rtiff,def=def,image_array=image_array
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref

if !d.name eq 'WIN' then device,decomposed=0

unit = catch2d_file.opened
seq = catch2d_file.scanno_current
last = catch2d_file.scanno_2d_last

if unit le 0 then begin
	res=WIDGET_MESSAGE('Error: no image file loaded in')
	return
end
if seq lt 1 or seq gt catch2d_file.scanno_2d_last then begin
	res=WIDGET_MESSAGE('Error: outside range seq entered')
	return
end

	seqno = catch2d_file.image_no(seq-1)

	point_lun, unit, catch2d_file.fptr(seqno)

	scanno_2d = seq
	for i=0,14 do begin
		if EOF(unit) eq 1 then goto,update
		u_read, unit, pvs
		u_read, unit, nos
		if i eq 0 then begin
			image_array  = make_array(nos(1),nos(5),15,/float)
			def = make_array(15,value=0)
		end
		if nos(4) gt seq then goto,update
		if nos(3) lt i then goto,update
		u_read, unit, x
		u_read, unit, y
		u_read, unit, t_image
		def(nos(3)) = 1
		image_array(*,*,nos(3)) = t_image
	end


; update the image plot

update:

	width = 60
	height = 60
	old_win = !D.window
		window, xsize = 8*width, ysize=2*height, $
			title='PanImages 2D SCAN # '+strtrim(catch2d_file.scanno_current,2)
		for i=0,14 do begin
		xi=(i mod 8)*width+width/2 - 5 
		yi=height/2+(15-i)/8*height
		xyouts, xi,yi,'D'+strtrim(i+1,2),/device
		end

	catch2d_file.win = !D.window
	wset,catch2d_file.win
	for sel=0,14 do begin
	if def(sel) gt 0 then begin
	v_max = max(image_array(*,*,sel),min=v_min)
	if v_max eq v_min then begin
		temp = view_option.ncolors * image_array(*,*,sel) 
		TV,congrid(temp,width,height),sel
	endif else begin
		temp = congrid(image_array(*,*,sel), width, height)
		TVSCL, temp, sel
	end
	end
	end


	plots,[0,8*width],[height,height],/device
	for i=1,7 do plots,[i*width,i*width],[0,2*height],/device

	if keyword_set(TIFF) then begin
	tvlct,R,G,B,/get
	WRITE_TIFF,'view2d.tiff',TVRD(),red=R,green=G,blue=B
	fileSeqString,catch2d_file.scanno_current,suf0
	outname=catch2d_file.name+'.pan'+suf0+'.tiff'
	dir = catch2d_file.outpath+'TIFF'+!os.file_sep
	rename_dialog,dir,'view2d.tiff',outname,GROUP=Event.Top
	end

	if keyword_set(RTIFF) then begin
	tvlct,R,G,B,/get
	WRITE_TIFF,'view2d.tiff',reverse(TVRD(),2),1,red=R,green=G,blue=B
	fileSeqString,catch2d_file.scanno_current,suf0
	outname=catch2d_file.name+'.pan'+suf0+'.rtiff'
	dir = catch2d_file.outpath+'TIFF'+!os.file_sep
	rename_dialog,dir,'view2d.tiff',outname,GROUP=Event.Top
	end

	if keyword_set(XDR) then begin
	xdr_open,unit,'view2d.xdr',/write,error=error
	xdr_write,unit,image_array
	xdr_close,unit
	fileSeqString,catch2d_file.scanno_current,suf0
	outname=catch2d_file.name+'.pan'+suf0+'.xdr'
	dir = catch2d_file.outpath+'XDR'+!os.file_sep
	rename_dialog,dir,'view2d.xdr',outname,GROUP=Event.Top
	end

	wset,old_win
	viewscanimage_current

END




PRO viewscanimage_init,file
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref

        if catch2d_file.opened ne 0 then free_lun,catch2d_file.opened
        catch2d_file.opened = 0

	open_binary_type_image,unit,file,type,widget_ids.bin_type
	catch2d_file.XDR = type
	scanimage_read_all,unit,maxno
	catch2d_file.opened = unit

print,'Selected Image File        : ', file 
print,'Total Number of 2D Scans   : ', catch2d_file.scanno_2d_last
print,'Total Number of Images     : ', catch2d_file.maxno

view_option.fullcolor = 0 ; initialize to auto scaled image

	if catch2d_file.scanno_2d_last gt 0 then viewscanimage_first

; update the slider widget

 if catch2d_file.maxno gt 0 then begin
	if widget_ids.slider gt 0 then $
		WIDGET_CONTROL,widget_ids.slider,BAD=bad,/DESTROY
		
	if catch2d_file.scanno_2d_last gt 1 then $
 	widget_ids.slider = WIDGET_SLIDER(widget_ids.sel_base, $
		MAX=catch2d_file.scanno_2d_last, $
		MIN=1,UVALUE='VIEW2D_SLIDER')
end

END

PRO open_binary_type_image,unit,filename,type,wid
; check for XDR format 

	type = 1
	u_openr,unit,filename,/XDR
	u_read,unit,pvs,errcode
	u_rewind,unit

	if errcode lt 0 then begin
	u_close,unit	
	type = 0
	u_openr,unit,filename
	end

	if n_params() eq 4 then $
	WIDGET_CONTROL,wid,set_droplist_select=type

END

PRO scanimage_read_all,unit,maxno
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref

	seqno = 0
	id = 0
	catch2d_file.fptr = make_array(10000,/long)

	point_lun, unit, 0 

	catch2d_file.seqno = 0
        catch2d_file.scanno_2d = 0
        catch2d_file.scanno_2d_last = 0
	WHILE NOT  EOF(unit) DO BEGIN
;		scanimage_read_record,unit
		u_read,unit,pvs
		u_read,unit,nos
;  the EOF(unit) will not work on XDR data, following is add for terminate
;  the read
	if catch2d_file.scanno_2d_last gt nos(4) then goto,readfail   
	id = id + 1
		u_read,unit,x
		u_read,unit,y
		u_read,unit,image
		point_lun,-unit,pos
		catch2d_file.fptr(id) = pos

	if n_elements(nos) lt 5 then begin
	 str = ['Error: bad data detected in this image file', '',$
	'       Only '+ strtrim(catch2d_file.scanno_2d_last) + '    images successfully read in.', '',$
	'       You may use the toImage program manually to ',$
	'       reconstruct the remaining images']
	 res = dialog_message( str,/ERROR,title='View2d Read Error')
	 goto,readfail
	end
		scanno_2d = nos(4)
		if scanno_2d eq 0 then scanno_2d = 1   ; fix 0 2D scan #
		detector = nos(3) + 1

	;  check for scan # increment

		if catch2d_file.scanno_2d_last lt scanno_2d then begin
			catch2d_file.scanno_2d_last = scanno_2d
			catch2d_file.image_no(scanno_2d-1) = catch2d_file.seqno 
			end

	; check for scan # out of sync case ???

		if scanno_2d lt catch2d_file.scanno_2d_last and detector eq 1 then begin 
			catch2d_file.scanno_2d_last = catch2d_file.scanno_2d_last + 1 
			catch2d_file.image_no(catch2d_file.scanno_2d_last-1) = catch2d_file.seqno 
			end

		catch2d_file.seqno = catch2d_file.seqno + 1 

	if detector gt 1 then catch2d_file.image_no(catch2d_file.scanno_2d_last) = catch2d_file.seqno


	END

readfail:
	maxno = id
	catch2d_file.maxno = maxno
	catch2d_file.seqno = maxno-1
	catch2d_file.image_no(catch2d_file.scanno_2d_last) = maxno 

;	point_lun, unit, 0 

END

PRO REPLOT
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON COLORBAR, colorbar_data
COMMON PRINTER_BLOCK,printer_info
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

if catch2d_file.scanno_current lt 0 then return

if !d.name eq 'WIN' then device,decomposed=0
; update the info block

xtitle='2D SCAN # '+strtrim(catch2d_file.scanno_current,2)+ $
;	',  D'+strtrim(catch2d_file.detector,2)+ $
	', '+catch2d_file.x_desc +  $
	', FILE:'+catch2d_file.name + $ 
	',  USER:'+strupcase(getenv('USER')) 

	ytitle = catch2d_file.y_desc

        header_note1='MAX: ' + strtrim(view_option.z_max,2) + ' @ ('+ $
		strtrim(catch2d_file.xarr(view_option.i_max),2) + ', ' + $
		strtrim(catch2d_file.yarr(view_option.j_max),2) + ')' 

        header_note='MIN: ' + strtrim(view_option.z_min,2) + ' @ ('+ $
		strtrim(catch2d_file.xarr(view_option.i_min),2) + ', ' + $
		strtrim(catch2d_file.yarr(view_option.j_min),2) + ')' 

str = ['Selected Image File        : '+ catch2d_file.name] 
str = [str,'Total Number of 2D Scans   : '+ string(catch2d_file.scanno_2d_last)]
str = [str,'Total Number of Images     : '+ string(catch2d_file.maxno)]
	str = [ str,'2D SCAN # ='+strtrim(catch2d_file.scanno_current)+ $
		',   DETECTOR='+strtrim(catch2d_file.detector,2) + $
		',   IMAGE # ='+strtrim(catch2d_file.seqno+1,2) + $
	', ('+catch2d_file.x_desc +', '+ catch2d_file.y_desc+', '+ $
		catch2d_file.z_desc + ')']
	str = [str, '1D scan #=('+strtrim(catch2d_file.scanno-catch2d_file.height+1,2)+'-'$
		+ strtrim(catch2d_file.scanno,2)+ ')'+$
		',   width='+strtrim(catch2d_file.width,2)+ $
		',   height='+strtrim(catch2d_file.height,2) ] 
	str = [str, 'x_pv = '+catch2d_file.x_pv+',   y_pv = '+catch2d_file.y_pv]
	str = [str, 'catch1d filename = '+ catch2d_file.file_1d]
	WIDGET_CONTROL, widget_ids.info, SET_VALUE= str
	str = strtrim(view_option.z_min,2) + ' @ (' + $
		strtrim(view_option.i_min,2) + ',' + $
		strtrim(view_option.j_min,2) + ')'
	WIDGET_CONTROL, widget_ids.zmin, SET_VALUE= str
	str = strtrim(view_option.z_max,2) + ' @ (' + $
		strtrim(view_option.i_max,2) + ',' + $
		strtrim(view_option.j_max,2) + ')'
	WIDGET_CONTROL, widget_ids.zmax, SET_VALUE= str

if view_option.fullcolor eq 0 then begin
	WIDGET_CONTROL,widget_ids.z_min,SET_VALUE=strtrim(view_option.z_min,2)
	WIDGET_CONTROL,widget_ids.z_max,SET_VALUE=strtrim(view_option.z_max,2)
end
if view_option.fullcolor eq 1 then begin
	WIDGET_CONTROL,widget_ids.z_min,SET_VALUE=strtrim(view_option.u_k_min,2)
	WIDGET_CONTROL,widget_ids.z_max,SET_VALUE=strtrim(view_option.u_k_max,2)
end

; check for user range
		xdim = catch2d_file.width
		ydim = catch2d_file.height
		catch2d_file.x_act_npts = xdim
		catch2d_file.y_act_npts = ydim
		x_min=0
		x_max=xdim-1
		y_min=0
		y_max=ydim-1

	if view_option.fullcolor eq 2 then begin
	 	image = image/ image_ref	
	end
		

	if view_option.user eq 1 then begin
	if view_option.x_min gt x_min and view_option.x_min lt x_max then $
	x_min = view_option.x_min
	if view_option.x_max lt x_max and view_option.x_max gt x_min then  $
	x_max = view_option.x_max
	if view_option.y_min gt y_min and view_option.y_min le y_max then  $
	y_min = view_option.y_min
	if view_option.y_max lt y_max and view_option.y_max gt y_min then  $
	y_max = view_option.y_max
	if y_max gt 0 and y_max eq y_min then y_min = y_min - 1

		newimage = image(x_min:x_max,y_min:y_max)
	endif else begin
		newimage = image
		end
;      
; set plot area for 2D view
;


if !d.name eq OS_SYSTEM.device then WSET,widget_ids.plot2d_area

		x = catch2d_file.xarr(0:catch2d_file.width - 1)
		y = catch2d_file.yarr(0:catch2d_file.height - 1)
		ix = n_elements(x)
		iy = n_elements(y)

;  draw 2D data as data image
   if view_option.user eq 0 then begin
	erase,view_option.bg
; expand data to drawing area
		catch2d_file.x_mag = 1
		catch2d_file.y_mag = 1
		newimage2 = newimage

	v_max = max(newimage2)
	v_min = min(newimage2)
	ncolors = view_option.ncolors

	
	if v_max eq v_min then begin       ;(all same value)
		dv = v_max - v_min
		if dv eq 0 then fact = ncolors  else fact = ncolors / dv
		TV,newimage2*fact
	endif else begin
		newimage2 = bytscl(newimage2,top=ncolors,min=v_min,max=v_max)
		TV,newimage2
	end
	return
   end

; set plot axis color

	  t_color = view_option.fg
	  if !d.name eq 'PS' then t_color = 0


	CASE view_option.surface OF
	2: begin
		if view_option.versus then begin
			if x_max lt ix then ix=x_max
			if y_max lt iy then iy=y_max 
			newim = image(x_min:ix,y_min:iy)
			nx=x(x_min:ix)
			ny=y(y_min:iy)
			SHADE_SURF, newim,nx,ny 
		endif else SHADE_SURF, newimage
	   end
	3: begin
			if x_max lt ix then ix=x_max
			if y_max lt iy then iy=y_max 
			newim = image(x_min:ix-1,y_min:iy-1)
			nc = view_option.ncolors
			labels=[1,1,1,1,1,1,1,1,1,1,1]
;			colors = [31,28,25,22,19,16,13,10]
			zmax = max(newim)
			zmin = min(newim)
			dz= (zmax-zmin)/ 9.
			dc = nc / 10 
			colors = nc 
			levels = zmin
			for i=1,9 do begin
			levels = [levels, zmin + dz*i]
			colors = [colors, nc - dc*i ]
			end
			if !d.n_colors eq 16777216 then begin
				catch1d_get_pvtcolor,colors(0),lcolor
				for i=1,9 do begin
				catch1d_get_pvtcolor,colors(i),tcolor
				lcolor =[lcolor,tcolor]
				end
			colors = lcolor
			end

		if view_option.versus then begin       ; versus values
			nx=x(x_min:ix-1)
			ny=y(y_min:iy-1)
		endif else begin       			; versus step # 
			temp = indgen(ix)
			nx=temp(x_min:ix-1)
			temp = indgen(iy)
			ny=temp(y_min:iy-1)
		end
		CONTOUR, newim,nx,ny, $
			background=view_option.bg, color=t_color, $
			levels = levels, $
			c_colors=reverse(colors), c_labels=labels, $
			 c_charsize=1.5,/follow
	   end
	4: begin
		if view_option.versus then begin
			if x_max lt ix then ix=x_max
			if y_max lt iy then iy=y_max 
			newim = image(x_min:ix-1,y_min:iy-1)
			nx=x(x_min:ix-1)
			ny=y(y_min:iy-1)
			SHOW3, newim 
		endif else SHOW3, newimage
;		SHOW3, newimage, sscale=2
	   end
	5: begin
		PLOT2D, newimage,id, $
			xarr=catch2d_file.xarr(0:catch2d_file.width-1), $
			yarr=catch2d_file.yarr(0:catch2d_file.height-1), $
	                comment=[header_note1,header_note], $
			wtitle='View2d(Plot2d)',$
       	         	xtitle=xtitle, ytitle=ytitle, $
       	         	title='D'+strtrim(catch2d_file.detector,2)+' - '+catch2d_file.z_desc
         
;		XSURFACE, newimage, id
		if n_elements(id) eq 0 then begin
		w_warningtext,'Error: close the old XSurface window first!', $
			60,5,title='VIEW2D Messages'
		return
		endif else $
		widget_ids.xsurface = id
	   end
	0: begin
	   erase,view_option.bg
		; expand data to drawing area
		catch2d_file.x_mag = 1
		catch2d_file.y_mag = 1

	 	    xratio = 1.
		    yratio = float(y_max-y_min+1)/(x_max-x_min+1) 
		    if yratio gt 1. then begin
			xratio = 1. / yratio
			yratio = 1.
		    end 
		width = (x_max - x_min + 1) / xratio
		height = (y_max - y_min + 1)/ yratio

		if view_option.user eq 1 and !d.name eq OS_SYSTEM.device then begin
		width = !d.x_size - view_option.margin_l - view_option.margin_r
		height = !d.y_size - view_option.margin_t - view_option.margin_b
		catch2d_file.x_mag = float(width)/(x_max-x_min +1)
		catch2d_file.y_mag = float(height)/(y_max-y_min +1)
		end

		newimage2 = CONGRID(newimage,width,height)
		ncolors = view_option.ncolors

	if view_option.fullcolor eq 0 then begin
		v_max = max(newimage2)
		v_min = min(newimage2)
	end
	if view_option.fullcolor eq 1 then begin
		v_max = view_option.u_k_max
		v_min = view_option.u_k_min
	end

	if view_option.fullcolor eq 2 then begin

		v_max = max(newimage2)
		v_min = min(newimage2)
	WIDGET_CONTROL,widget_ids.z_min,SET_VALUE=strtrim(v_min,2)
	WIDGET_CONTROL,widget_ids.z_max,SET_VALUE=strtrim(v_max,2)
	end

;		if !d.name ne OS_SYSTEM.device then ncolors = ncolors - 1

		if v_max eq v_min then begin       ;(all same value)
			dv = v_max - v_min
			if dv eq 0 then fact = ncolors  else fact = ncolors / dv
			newimage2 = newimage2*fact
		endif else begin
		if view_option.fullcolor lt 2 then $
		newimage2 = bytscl(newimage2,top=ncolors,min=v_min,max=v_max) 
		if view_option.fullcolor eq 2 then $
		newimage2 = bytscl(newimage2,top=ncolors,min=v_min,max=v_max)
		end

		xrange = [x_min,x_max]
		yrange = [y_min,y_max]
		title = 'vs X,Y Step #'
		if view_option.versus then begin
		xrange = [ catch2d_file.xarr(x_min), catch2d_file.xarr(x_max)]
		yrange = [ catch2d_file.yarr(y_min), catch2d_file.yarr(y_max)]
		title = 'vs X,Y Values'
		end 

		    xstyle = 1
		    ystyle = 1
		    if xrange(0) eq xrange(1) then xstyle = 4
		    if yrange(0) eq yrange(1) then ystyle = 4

		; for PS  get aspect ratio, outward tick marks 

	; draw headers

	  xdis = 0.01 * !d.x_size
	  if !d.name eq 'PS' then xdis = 0.001 * !d.x_size
          ydis = !d.y_size - 1.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note1,/device, color=t_color
          ydis = !d.y_size - 2.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note,/device, color=t_color

	if strtrim(catch2d_file.z_desc,2) ne '' then $
	title = catch2d_file.z_desc + ' - ' + title else $
	title = 'D'+strtrim(catch2d_file.detector,2) + ' - '+title

	colorbar_data.min = v_min
	colorbar_data.max = v_max
	colorbar_data.wid = !d.window 
	posy = colorbar_data.y

		if !d.name eq 'PS' then begin
		    t_color = 0
		    xo = !d.x_size * view_option.ps_l
		    yo = !d.y_size * view_option.ps_b
		    xw = !d.x_size * (view_option.ps_r - view_option.ps_l)
		    yw = !d.y_size * (view_option.ps_t - view_option.ps_b)

		    pos = [view_option.ps_l, view_option.ps_b, $
			view_option.ps_r, view_option.ps_t]

		    TV,newimage2,xo,yo,xsize=xw,ysize=yw

		    plot,/noerase,/nodata,pos=pos, [-1,-1], $
			xrange=xrange, yrange=yrange, $
			xticklen= -!p.ticklen, yticklen=-!p.ticklen, $
			title=title, xtitle=xtitle, $
			ytitle=ytitle, $
			xstyle = xstyle, ystyle=ystyle ,color=t_color

x = !d.x_size * (.02+view_option.ps_r)
y = !d.y_size * view_option.ps_b
h = colorbar_data.height*30 
w = colorbar_data.width*10
		colorbar,[v_min,v_max], $
                        w,h,x=x,y=y, $
                        horizontal=colorbar_data.horiz, $
                        reverse=printer_info.reverse, $
                        format=colorbar_data.format, $
                        ncap=colorbar_data.nlabel

		endif else begin

		    TV,newimage2, view_option.margin_l, view_option.margin_b

		    p1 = [float(view_option.margin_l)/ !d.x_size, $
			float(view_option.margin_b)/!d.y_size, $
			float(!d.x_size - view_option.margin_r) / !d.x_size, $
			float(!d.y_size - view_option.margin_t) / !d.y_size $
			]

		    plot,/noerase,/nodata,pos=p1 ,[-1,-1], $
			xrange=xrange, yrange=yrange, $
			xtitle= catch2d_file.x_desc, $
			ytitle=ytitle, $
			title=title, xstyle = xstyle, ystyle=ystyle,color=t_color

		colorbar,[v_min,v_max],colorbar_data.width, $
			colorbar_data.height, $
			horizontal=colorbar_data.horiz, $
			x=colorbar_data.x, y=posy, $
			reverse=printer_info.reverse, $
			ncap=colorbar_data.nlabel, format=colorbar_data.format
		end
			

                ; save pixmap
                if !d.name ne OS_SYSTEM.device then return
                view_option.d_wid = !d.window
                if view_option.s_wid ge 0 then begin
                        wid = view_option.s_wid
                        update_pixmap,wid
                endif else begin
                        create_pixmap,wid
                        view_option.s_wid = wid
                end

	if widget_ids.norm_base ne 0 then view2d_normalize_setvalue

	   end
	1: begin
	   erase,view_option.bg
		; equal aspect ratio  
		catch2d_file.x_mag = 1
		catch2d_file.y_mag = 1
		width = x_max - x_min + 1
		height = y_max - y_min + 1

	 	    xratio = 1.
		    yratio = float(y_max-y_min+1)/(x_max-x_min+1) 
		    if yratio gt 1. then begin
			xratio = 1. / yratio
			yratio = 1.
		    end 

		if view_option.user eq 1 and !d.name eq OS_SYSTEM.device then begin
		width = !d.x_size - view_option.margin_l - view_option.margin_r
		height = !d.y_size - view_option.margin_t - view_option.margin_b
		catch2d_file.x_mag = floor(width/(x_max-x_min + 1))
		catch2d_file.y_mag = floor(height/(y_max-y_min + 1))
		width = ceil(width * xratio)
		height = ceil(height * yratio)
		end

		newimage2 = CONGRID(newimage,width,height)

		v_max = max(newimage2)
		v_min = min(newimage2)
		ncolors = view_option.ncolors

		if v_max eq v_min then begin       ;(all same value)
			dv = v_max - v_min
			if dv eq 0 then fact = ncolors  else fact = ncolors / dv
			newimage2 = newimage2*fact
		endif else begin
		newimage2 = bytscl(newimage2,top=ncolors,min=v_min,max=v_max)
		end

		xrange = [x_min,x_max]
		yrange = [y_min,y_max]
		title = 'vs X,Y Step #'
		if view_option.versus then begin
		xrange = [ catch2d_file.xarr(x_min), catch2d_file.xarr(x_max)]
		yrange = [ catch2d_file.yarr(y_min), catch2d_file.yarr(y_max)]
		title = 'vs X,Y Values'
		end

		    xstyle = 1
		    ystyle = 1
		    if xrange(0) eq xrange(1) then xstyle = 4
		    if yrange(0) eq yrange(1) then ystyle = 4

		; for PS  get aspect ratio, outward tick marks 

	; draw headers

        header_note1='MAX: ' + strtrim(view_option.z_max,2) + ' @ ('+ $
		strtrim(catch2d_file.xarr(view_option.i_max),2) + ', ' + $
		strtrim(catch2d_file.yarr(view_option.j_max),2) + ')' 

        header_note='MIN: ' + strtrim(view_option.z_min,2) + ' @ ('+ $
		strtrim(catch2d_file.xarr(view_option.i_min),2) + ', ' + $
		strtrim(catch2d_file.yarr(view_option.j_min),2) + ')' 

          xdis = 0.01 * !d.x_size
 	  if !d.name eq 'PS' then xdis = 0.001 * !d.x_size
          ydis = !d.y_size - 1.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note1,/device,color=t_color
          ydis = !d.y_size - 2.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note,/device,color=t_color

	ytitle = catch2d_file.y_desc
	if strtrim(catch2d_file.z_desc,2) ne '' then $
	title = catch2d_file.z_desc + ' - ' + title else $
	title = 'D'+strtrim(catch2d_file.detector,2) + ' - '+title

		if !d.name eq 'PS' then begin
		    xo = !d.x_size * view_option.ps_l
		    yo = !d.y_size * view_option.ps_b
		    xw = !d.x_size * xratio *(view_option.ps_r - view_option.ps_l)
		    yw = !d.y_size * yratio *(view_option.ps_t - view_option.ps_b)

		    TV,newimage2,xo,yo,xsize=xw,ysize=yw

		    pos = [view_option.ps_l, view_option.ps_b, $
			xw / !d.x_size + view_option.ps_l,  $
			yw / !d.y_size + view_option.ps_b ]

		    plot,/noerase,/nodata,pos=pos, [-1,-1], $
			xrange=xrange, yrange=yrange, $
			xticklen= -!p.ticklen, yticklen=-!p.ticklen, $
			title=title, xtitle=xtitle, ytitle=ytitle, $
			xstyle = xstyle, ystyle=ystyle, color=t_color

		endif else begin
		    TV,newimage2, view_option.margin_l, view_option.margin_b

		    p1 = [float(view_option.margin_l)/ !d.x_size, $
			float(view_option.margin_b)/!d.y_size, $
			float(!d.x_size - view_option.margin_r) / !d.x_size, $
			(float(view_option.margin_b) + height)/!d.y_size $
			]

		    plot,/noerase,/nodata,pos=p1 ,[-1,-1], $
			xrange=xrange, yrange=yrange,title=title, $
			xtitle=xtitle, ytitle=ytitle, $
			xstyle = xstyle, ystyle=ystyle,color=t_color
		end
		
	colorbar_data.min = v_min
	colorbar_data.max = v_max
	colorbar_data.wid = !d.window
	posy = colorbar_data.y

		colorbar,[v_min,v_max],colorbar_data.width, $
			colorbar_data.height, $
			horizontal=colorbar_data.horiz, $
			x=colorbar_data.x, y=posy, $
			reverse=printer_info.reverse, $
			ncap=colorbar_data.nlabel, format=colorbar_data.format

	   end
	ELSE: print,'Unknow case entered'
	ENDCASE

END

PRO scanimage_read_record,unit, view=view 
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

	IF EOF(unit) THEN RETURN

	if (widget_ids.xsurface) then begin
		WIDGET_CONTROL,widget_ids.xsurface,/DESTROY,BAD=bad
		widget_ids.xsurface = 0L
		end

	; read pv names
	u_read,unit,pvs
	pvs = string(pvs)
	catch2d_file.x_pv = pvs(0)
	catch2d_file.y_pv = pvs(1) 
	catch2d_file.file_1d = pvs(2) 
	if n_elements(pvs) gt 3 then begin
	catch2d_file.x_desc = pvs(3)
	catch2d_file.y_desc = pvs(4) 
	catch2d_file.z_desc = pvs(5) 
	end
;	print,string(pvs)
	

	; read seqno, dims

	u_read,unit,x
;	print,x
	scanno_2d = x(4)

	catch2d_file.scanno = x(0)
	catch2d_file.width = x(1)
	catch2d_file.height = x(2)
	catch2d_file.detector = x(3) + 1
	catch2d_file.scanno_current = x(4) 
	if x(4) eq 0 then catch2d_file.scanno_current = 1    ; fix 0 2d #
	catch2d_file.y_req_npts = x(5)

	; read x and y position array

	u_read,unit,xa
	u_read,unit,ya

	; read image
	u_read,unit,image

	view_option.x_min = 0
	view_option.y_min = 0
	view_option.x_max = catch2d_file.width
	view_option.y_max = catch2d_file.height
		WIDGET_CONTROL,widget_ids.x_min,SET_VALUE=0
		WIDGET_CONTROL,widget_ids.x_max,SET_VALUE=view_option.x_max
		WIDGET_CONTROL,widget_ids.y_min,SET_VALUE=0
		WIDGET_CONTROL,widget_ids.y_max,SET_VALUE=view_option.y_max

	catch2d_file.xarr = xa(0:catch2d_file.width-1)
	catch2d_file.yarr = ya(0:catch2d_file.height-1)
;	Image = image(0:catch2d_file.width-1, 0:catch2d_file.height-1)
;	catch2d_file.image = image

	newImage = image
	s = size(newImage)

	; find the max only for 2D or 1D

	if s(0) eq 2 then totalno = s(4) - 1
	if s(0) eq 1 then totalno = s(3) - 1
        view_option.z_max = newImage(0)
	view_option.i_max = 0
	view_option.j_max = 0
	view_option.z_max = MAX(newImage,imax)
	view_option.j_max = imax / s(1)
	view_option.i_max = imax mod s(1) 
	view_option.z_min = MIN(newImage,imax)
	view_option.j_min = imax / s(1)
	view_option.i_min = imax mod s(1) 
	if view_option.fullcolor eq 0 then begin
		view_option.k_max = view_option.z_max 
		view_option.k_min = view_option.z_min 
	end

	if s(0) ne 2 then begin
		w_warningtext,'Warning: data is not 2D image ',60,5,'VIEW2D Messaes'
		end 


	if view_option.pickx gt 1 then view2d_changeXaxis
	if keyword_set(view) then REPLOT

        WIDGET_CONTROL, widget_ids.x1WID, SET_VALUE= catch2d_file.xarr(0)
        WIDGET_CONTROL, widget_ids.x2WID, SET_VALUE= catch2d_file.xarr(catch2d_file.width-1)
        WIDGET_CONTROL, widget_ids.y1WID, SET_VALUE= catch2d_file.yarr(0)
        WIDGET_CONTROL, widget_ids.y2WID, SET_VALUE= catch2d_file.yarr(catch2d_file.height-1)
END

PRO viewscanimage_current
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref

	unit = catch2d_file.opened
	seqno = catch2d_file.seqno
	point_lun, unit, catch2d_file.fptr(seqno)
	scanimage_read_record,unit,/view

END

PRO viewscanimage_first
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref

	current = 1
	catch2d_file.scanno_current = current
        WIDGET_CONTROL,widget_ids.seqno, SET_VALUE=strtrim(string(current),2)
	unit = catch2d_file.opened
	
	seqno = 0
	if catch2d_file.detector lt catch2d_file.image_no(1) and catch2d_file.detector gt 0 then $
		seqno =seqno+catch2d_file.detector - 1
	point_lun, unit, catch2d_file.fptr(seqno)
	catch2d_file.seqno = seqno
	scanimage_read_record,unit,/view

	WIDGET_CONTROL, widget_ids.sel_image, SET_VALUE=catch2d_file.detector-1
END

PRO viewscanimage_next
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref

	current = catch2d_file.scanno_current + 1
	if current gt catch2d_file.scanno_2d_last then begin
		w_warningtext,'End of file', 60,5,title='VIEW2D Messages'
		return
		end
	catch2d_file.scanno_current = current
	unit = catch2d_file.opened
	seqno = catch2d_file.image_no(current-1) + catch2d_file.detector - 1
	if seqno ge catch2d_file.image_no(current) then $
		seqno = catch2d_file.image_no(current-1)

	point_lun, unit, catch2d_file.fptr(seqno) 
	catch2d_file.seqno = seqno 
	scanimage_read_record,catch2d_file.opened, /view

	WIDGET_CONTROL, widget_ids.sel_image, SET_VALUE=catch2d_file.detector-1 
        WIDGET_CONTROL,widget_ids.seqno, SET_VALUE=strtrim(catch2d_file.scanno_current,2)

END

PRO viewscanimage_prev
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref

	current = catch2d_file.scanno_current - 1
	catch2d_file.scanno_current = current
	if current lt 1 then begin
		w_warningtext,'Beginning of file',60,5,title='VIEW2D Messages'
		catch2d_file.scanno_current = 1
		return
		end
	unit = catch2d_file.opened
	endno = catch2d_file.image_no(current) 
	begno = catch2d_file.image_no(current-1) 
	if begno eq 0 and current gt 2 then current = current -1 
	if begno ge endno then current = current - 1
	begno = catch2d_file.image_no(current-1) 
	while begno ge endno and current gt 1 do begin
		begno = catch2d_file.image_no(current-1) 
		current = current-1
		end
	seqno = catch2d_file.image_no(current-1) + catch2d_file.detector - 1
	point_lun, unit, catch2d_file.fptr(seqno) 
	catch2d_file.seqno = seqno 
	scanimage_read_record,catch2d_file.opened, /view

	WIDGET_CONTROL, widget_ids.sel_image, SET_VALUE=catch2d_file.detector - 1
        WIDGET_CONTROL,widget_ids.seqno, SET_VALUE=strtrim(catch2d_file.scanno_current,2)
;strtrim(string(current),2)
END

PRO viewscanimage_last
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref

	current = catch2d_file.scanno_2d_last
	catch2d_file.scanno_current = current
        WIDGET_CONTROL,widget_ids.seqno, SET_VALUE=strtrim(string(current),2)
	unit = catch2d_file.opened
	seqno = catch2d_file.image_no(current-1)
	if catch2d_file.detector le (catch2d_file.maxno-seqno) then seqno =seqno+catch2d_file.detector - 1
	point_lun, unit, catch2d_file.fptr(seqno) 
	catch2d_file.seqno = seqno 
	scanimage_read_record,unit, /view

	WIDGET_CONTROL, widget_ids.sel_image, SET_VALUE=catch2d_file.detector-1
END

PRO dc2aim,infile
common com_file,jfile
savefile = 'dc2aim.sav'
if n_elements(infile) then savefile=infile
found = findfile(savefile) 
if found(0) eq '' then begin
	res = widget_message(savefile + ' not found!',/Error)
	return
end
restore,file=savefile
;help,ncol,nrow,imarr,output=st
;res = widget_message(st,/Info)
jfile.nxin=ncol
jfile.nyin=nrow
retall
END

PRO fileSeqString,no,suf0
        suf0 = '0000'
        suf = strtrim(no,2)
        ln = strlen(suf)
        strput,suf0,suf,4-ln
	if no gt 9999 then suf0=suf
END


PRO PDMENU189_Event, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

  CASE Event.Value OF 

  'File.Open ...': BEGIN

        FNAME = '*image*'
;	if catch2d_file.XDR eq 1 then FNAME = '*image.xdr'
        F = DIALOG_PICKFILE(/READ,FILE='catch1d.trashcan.image',GET_PATH=P,PATH=catch2d_file.path,FILTER=FNAME)
        IF F eq '' THEN begin
                F = ''
		return
                end
        found=findfile(F)

        catch2d_file.name = F

        if found(0) ne '' then viewscanimage_init, F else begin
		w_warningtext,'Error:  file not found - '+ F, 60,5, $
			title='VIEW2D Messages'
		return
		end
                
	pos = rstrpos(F,OS_SYSTEM.file_sep) ;'/'
	if pos gt 0 then catch2d_file.name = strmid(F,pos+1,strlen(F))

	catch2d_file.path = P
	catch,error_status
	if error_status ne 0 then begin
		cd,current=p
		catch2d_file.outpath = p+!os.file_sep
		return
	end
	openw,1,P+'.tmp'
	close,1
;	spawn,!os.rm + ' '+P+'.tmp'
	catch2d_file.outpath = P

    END


  'File.Save Image for AIM': BEGIN
	ncol = catch2d_file.width
	nrow = catch2d_file.height
   	xarr = catch2d_file.xarr(0:ncol-1)
   	yarr = catch2d_file.yarr(0:nrow-1)
   	imarr = image
	save,filename='dc2aim.sav',/XDR,ncol,nrow,xarr,yarr,imarr
    END

  'File.Save as XDR': BEGIN
	if catch2d_file.scanno_current le 0 then return
	xdr_open,unit,'view2d.xdr',/write,error=eror
	xdr_write,unit,image
	xdr_close,unit
	fileSeqString,catch2d_file.seqno,suf0
	outname=catch2d_file.name+'.'+suf0+'.xdr'
	dir = catch2d_file.outpath+'XDR'+!os.file_sep
	rename_dialog,dir,'view2d.xdr',outname,GROUP=Event.Top
    END

  'File.Save as R-TIFF': BEGIN
	tvlct,R,G,B,/get
	WRITE_TIFF,'view2d.tiff',reverse(TVRD(),2),1,red=R,green=G,blue=B
	fileSeqString,catch2d_file.seqno,suf0
	outname=catch2d_file.name+'.'+suf0+'.rtiff'
	dir = catch2d_file.outpath+'TIFF'+!os.file_sep
	rename_dialog,dir,'view2d.tiff',outname,GROUP=Event.Top
    END
  'File.Save as TIFF': BEGIN
	tvlct,R,G,B,/get
	WRITE_TIFF,'view2d.tiff',TVRD(),red=R,green=G,blue=B
	fileSeqString,catch2d_file.seqno,suf0
	outname=catch2d_file.name+'.'+suf0+'.tiff'
	dir = catch2d_file.outpath+'TIFF'+!os.file_sep
	rename_dialog,dir,'view2d.tiff',outname,GROUP=Event.Top
    END

  'File.Printer ...': BEGIN
    PS_printer,GROUP=Event.Top
    END

  'File.Print': BEGIN
    PS_open,'view2d.ps',/TV
    REPLOT
    PS_close
    PS_print,'view2d.ps'
    END

  'File.PS_close': BEGIN
    PS_close
    END

  'File.Quit': BEGIN
    WIDGET_CONTROL, event.top, /DESTROY
    if catch2d_file.win ne -1 then WDELETE,catch2d_file.win 
    if widget_ids.xsurface then begin 
	WIDGET_CONTROL, widget_ids.xsurface,BAD=bad, /DESTROY
	widget_ids.xsurface = 0L
	end
;    WDELETE,catch2d_file.xprof,catch2d_file.yprof
    if catch2d_file.opened ne 0 then free_lun,catch2d_file.opened
    catch2d_file.opened=0
    END
  ENDCASE
END



PRO PDMENU188_Event, Event
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON PRINTER_BLOCK,printer_info
COMMON COLORBAR, colorbar_data

  CASE Event.Value OF 

  'Color.Save Private Color Table': BEGIN
	TVLCT,red,green,blue,/Get
	Save,red,green,blue,file='pvtcolors.dat'
	return
    END
  'Color.Load Private Color Table': BEGIN
	found = findfile('pvtcolors.dat')
	if found(0) eq ''  then begin
		st = ['Error: Private color table never been saved before. ', $
	      '       You have to save the private color first, before', $
	      '       you can load it into the view2d program.'] 
		w_warningtext,st,60,5,title='VIEW2D Messages'
	endif else begin
		restore,'pvtcolors.dat'
		TVLCT,red,green,blue
	end
	return
    END

  'Color.Change Color Table ...': BEGIN
    XLOADCT
	return
    END
  'Color.Image Color Scheme ...': BEGIN
	view2d_normalize,GROUP=Event.top
	return
	END
  'Color.Reverse Background': BEGIN
	if printer_info.reverse then begin
		 printer_info.reverse = 0
		 view_option.bg = 0
		 view_option.fg = !d.table_size-1 
	endif else begin
		 printer_info.reverse = 1
		 view_option.bg = !d.table_size-1 
		 view_option.fg = 0
	end
	REPLOT
	return
	END
  'Color.ColorBar Config ...': BEGIN
	WSET,widget_ids.plot2d_area
	colorbar_config,caller='REPLOT',GROUP=Event.top
	return
    END
  'Color.Font.Name.Default': BEGIN
	view_option.fontname = 'Default'
    END
  'Color.Font.Name.Courier': BEGIN
	view_option.fontname = 'Courier'
    END
  'Color.Font.Name.Helvetica': BEGIN
	view_option.fontname = 'Helvetica'
    END
  'Color.Font.Name.Times': BEGIN
	view_option.fontname = 'Times'
    END
  'Color.Font.Name.Symbol': BEGIN
	view_option.fontname = 'Symbol'
    END
  ENDCASE

	setfont,view_option.fontname 
	REPLOT
END



PRO PDMENU189_help_Event, Event

  if getenv('EPICS_EXTENSIONS') eq '' then begin
	res=WIDGET_MESSAGE('EPICS_EXTTENSIONS not defined.')
	return
	end
  CASE Event.Value OF 

  'Help.Help ...': BEGIN
    str = getenv('EPICS_EXTENSIONS')
    new = getenv('EPICS_EXTENSIONS_PVT')
    if strlen(new) gt 3 then str = new 
	str = str+'/doc/view2d_help.txt'
    xdisplayfile, str, GROUP=Event.top 
    END
  ENDCASE
END

PRO PDMENU2D_PanImage_Event, Event
COMMON CATCH2D_FILE_BLOCK,catch2d_file

  CASE Event.Value OF
  'PanImage.PanImages.PanImages...': begin
	view2d_pan_images_on,Event
	end
  'PanImage.PanImages.PanImages+TIFF': begin
	view2d_pan_images_on,Event,/tiff
	end
  'PanImage.PanImages.PanImages+RTIFF': begin
	view2d_pan_images_on,Event,/rtiff
	end
  'PanImage.PanImages.PanImages+XDR': begin
	view2d_pan_images_on,Event,/xdr
	end
  'PanImage.Overlay...': begin
	title=':  SCAN # '+ strtrim(catch2d_file.scanno_current,2)
	view2d_pan_images_on,Event,image_array=image_array,def=def
	xv = catch2d_file.xarr(0:catch2d_file.width-1)
	yv = catch2d_file.yarr(0:catch2d_file.height-1)
	im_array=image_array(0:catch2d_file.width-1,0:catch2d_file.height-1,*)
	overlay2DImages,im_array,def,selects=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
	end
  'PanImage.Calibration...': begin
	title=':  SCAN # '+ strtrim(catch2d_file.scanno_current,2)
	view2d_pan_images_on,Event,image_array=image_array,def=def
	xv = catch2d_file.xarr(0:catch2d_file.width-1)
	yv = catch2d_file.yarr(0:catch2d_file.height-1)
	im_array=image_array(0:catch2d_file.width-1,0:catch2d_file.height-1,*)
	calibration_factor,im_array,def,title=title, $
		inpath=catch2d_file.path,classname=catch2d_file.name, $
		xv=xv,yv=yv,GROUP=Event.top
	end
  ENDCASE
END

PRO PDMENU2D_FITTING_Event, Event
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON w_viewscanimage_block, w_viewscanimage_ids
COMMON w_warningtext_block,w_warningtext_ids

if n_elements(image) eq 0  then begin
	w_warningtext,['No image data found','','You have to load the 2D data first']
	return
end
x = catch2d_file.xarr(0:catch2d_file.width-1)
y = catch2d_file.yarr(0:catch2d_file.height-1)
im = image(0:catch2d_file.width-1, 0:catch2d_file.height-1)

  CASE Event.Value OF

  'Fitting.Ez_Fit ...': BEGIN
        ez_fit,xarray=x,yarray=y,im=im,GROUP=Event.Top
        END
  'Fitting.2D Binary': BEGIN
        u_openw,unit,'fitting.bin',/XDR
        u_write,unit,x
        u_write,unit,y
	u_write,unit,image
        u_close,unit
        st = '2D binary data save in "fitting.bin"'
        w_warningtext,st
        END
  ENDCASE
END

PRO view2d_changeXaxis
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

	WIDGET_CONTROL,/HOURGLASS
	file1 = catch2d_file.path + catch2d_file.file_1d
	found = findfile(file1,count=ct)
	if ct lt 1 then return

	scanno = catch2d_file.scanno-catch2d_file.height+1
	x_names=''
	x_descs=''
	x_engus=''
	v1 = obj_new('scan1d',file=file1)
	if obj_valid(v1) then begin
	v1->point_lun,scanno
	v1->read,scanno,pa=pa,da=da,nd=nd,np=np,x_names=x_names,x_descs=x_descs,x_engus=x_engus
	sz = size(pa)
	if view_option.pickx gt np then begin
		x = indgen(sz(1))
		xlabel = 'P'+strtrim(view_option.pickx,2)
	endif else begin
		x = pa(*,view_option.pickx-1)
		xlabel = x_descs(view_option.pickx-1) + ' ('+ x_engus(view_option.pickx-1) + ')'
	end
	catch2d_file.x_desc = xlabel
	catch2d_file.xarr = x
	end
	obj_destroy,v1
END
	
PRO main13_2_Event, Event

COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON w_viewscanimage_block, w_viewscanimage_ids
COMMON w_warningtext_block,w_warningtext_ids


  ; The next CASE statement is from the Widget Builder.
  ; It uses the User_value of a widget to identify itself.



  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  ; Event for FILE_MENU
  'PDMENU189': PDMENU189_Event, Event

  ; Event for Color_MENU
  'PDMENU188': PDMENU188_Event, Event

  'PDMENU189_help': PDMENU189_help_Event, Event
  'PDMENU2D_PANIMAGE': PDMENU2D_PanImage_Event, Event
  'PDMENU2D_ROI': PDMENU2D_ROI_Event, Event
  'PDMENU2D_FITTING': PDMENU2D_FITTING_Event, Event

  'SURFACE_PLOT': BEGIN
	view_option.surface = Event.Index
	REPLOT
	END
  'PLOTVERSUS': BEGIN
	view_option.versus = Event.Index
	REPLOT
	END
  'BGROUP184': BEGIN
	view_option.user = Event.Index
	CASE view_option.user OF
	0: begin
		WIDGET_CONTROL,widget_ids.x_min,SET_VALUE=0
		WIDGET_CONTROL,widget_ids.x_max,SET_VALUE=view_option.width
		WIDGET_CONTROL,widget_ids.y_min,SET_VALUE=0
		WIDGET_CONTROL,widget_ids.y_max,SET_VALUE=view_option.height
	end
	1: begin
		WIDGET_CONTROL,widget_ids.x_min,SET_VALUE=view_option.x_min
		WIDGET_CONTROL,widget_ids.x_max,SET_VALUE=view_option.x_max
		WIDGET_CONTROL,widget_ids.y_min,SET_VALUE=view_option.y_min
		WIDGET_CONTROL,widget_ids.y_max,SET_VALUE=view_option.y_max
	end
	ELSE:
	ENDCASE
	REPLOT
      END
; Event for PDMENU226
;  'PDMENU226': PDMENU226_Event, Event

  'DRAW62': BEGIN
	if catch2d_file.maxno le 0 then return

	WSET,view_option.d_wid

	view = view_option.surface
	if (view eq 0 ) then begin


      IF ((Event.PRESS EQ 1)) THEN BEGIN
	catch2d_xycoord_TV, x, y, Event
	END

      IF ((Event.PRESS EQ 2)) THEN BEGIN
	catch2d_xydist2, Event
	END

      IF (Event.PRESS EQ 4) THEN BEGIN
	hide_cross,view_option.x,view_option.y,view_option.d_wid,view_option.s_wid
	nx = 50 
	ny = 50 
	x0 = Event.x - 25
	y0 = Event.y - 25
	
	my_box_cursor, x0, y0, nx,ny, /INIT

	if view_option.user eq 1 then begin
		x_min = float(x0-view_option.margin_l) / $
			catch2d_file.x_mag + view_option.x_min
	x_min = fix(x_min)
		x_max = float(x0+nx-view_option.margin_l) / $
			catch2d_file.x_mag + view_option.x_min
	x_max = ceil(x_max)
		y_min = float(y0-view_option.margin_b) / $
			catch2d_file.y_mag + view_option.y_min
	y_min = fix(y_min)
		y_max = float(y0+ny-view_option.margin_b) / $
			catch2d_file.y_mag + view_option.y_min
	y_max = ceil(y_max)

	if x_min lt 0 then x_min = 0
	if y_min lt 0 then y_min = 0
	if x_max ge catch2d_file.width then x_max = catch2d_file.width - 1
	if y_max ge catch2d_file.height then y_max = catch2d_file.height - 1
	if x_max gt 0 and x_min ge x_max then x_min = x_max - 1
	if y_max gt 0 and y_min ge y_max then y_min = y_max - 1
	if x_max le 0 then x_max = 1
	if y_max le 0 then y_max = 1

close,1
openw,1,'box.txt'
printf,1,'lower_left',catch2d_file.xarr(x_min),catch2d_file.yarr(y_min)
printf,1,'upper_right',catch2d_file.xarr(x_max),catch2d_file.yarr(y_max)
close,1

; update start/end range
        WIDGET_CONTROL, widget_ids.x1WID, SET_VALUE= catch2d_file.xarr(x_min)
        WIDGET_CONTROL, widget_ids.x2WID, SET_VALUE= catch2d_file.xarr(x_max)
        WIDGET_CONTROL, widget_ids.y1WID, SET_VALUE= catch2d_file.yarr(y_min)
        WIDGET_CONTROL, widget_ids.y2WID, SET_VALUE= catch2d_file.yarr(y_max)

	WIDGET_CONTROL,widget_ids.plot_wid,/CLEAR_EVENTS
		WIDGET_CONTROL,widget_ids.x_min, $
			SET_VALUE = strtrim( fix(x_min),2)
		WIDGET_CONTROL,widget_ids.x_max, $
			SET_VALUE = strtrim( ceil(x_max),2)
		WIDGET_CONTROL,widget_ids.y_min, $
			SET_VALUE = strtrim( fix(y_min),2)
		WIDGET_CONTROL,widget_ids.y_max, $
			SET_VALUE = strtrim( ceil(y_max),2)

		view_option.x_min = fix(x_min)
		view_option.y_min = fix(y_min)
		view_option.x_max = ceil(x_max)
		view_option.y_max = ceil(y_max)
		REPLOT
	end
	return
	END

      end
      END

  'REFRESH_DATA': BEGIN
        WIDGET_CONTROL, widget_ids.x_min, SET_VALUE = 0 
        WIDGET_CONTROL, widget_ids.x_max, SET_VALUE = view_option.width
        WIDGET_CONTROL, widget_ids.y_min, SET_VALUE = 0
        WIDGET_CONTROL, widget_ids.y_max, SET_VALUE = view_option.height
		view_option.x_min = 0
		view_option.y_min = 0
		view_option.x_max = view_option.width
		view_option.y_max = view_option.height
	REPLOT
	END
  'FIELD246': BEGIN
         WIDGET_CONTROL, widget_ids.y_max, GET_VALUE = y_max  
	if fix(y_max) gt view_option.y_min then begin
	view_option.y_max = fix(y_max)
        if view_option.user eq 1 then REPLOT
	end
      END
  'FIELD157': BEGIN
         WIDGET_CONTROL, widget_ids.y_min, GET_VALUE = y_min
	if fix(y_min) lt view_option.y_max then begin
	view_option.y_min = fix(y_min)
        if view_option.user eq 1 then REPLOT
	end
      END
  'FIELD159': BEGIN
         WIDGET_CONTROL, widget_ids.x_max, GET_VALUE = x_max
	if fix(x_max) gt view_option.x_min then begin
	view_option.x_max = fix(x_max)
        if view_option.user eq 1 then REPLOT
	end
      END
  'FIELD161': BEGIN
         WIDGET_CONTROL, widget_ids.x_min, GET_VALUE = x_min
	if fix(x_min) lt view_option.x_max then begin
	view_option.x_min = fix(x_min)
        if view_option.user eq 1 then REPLOT
	end
      END
  'VIEW2D_ZMAX': BEGIN
         WIDGET_CONTROL, widget_ids.z_max, GET_VALUE = z_max
	view_option.u_k_max = z_max
	view_option.k_max = z_max
        if view_option.user eq 1 then REPLOT
      END
  'VIEW2D_ZMIN': BEGIN
         WIDGET_CONTROL, widget_ids.z_min, GET_VALUE = z_min
	view_option.u_k_min = z_min
	view_option.k_min = z_min
        if view_option.user eq 1 then REPLOT
      END
  'VIEW2D_SEQNO': BEGIN
	WIDGET_CONTROL,widget_ids.seqno,GET_VALUE=x
	if x le 0 or x gt catch2d_file.scanno_2d_last then begin
		res = WIDGET_MESSAGE('Input out of range!')
		return
		end
	catch2d_file.scanno_current = x
	catch2d_file.seqno = catch2d_file.image_no(x-1)+catch2d_file.detector-1
	if catch2d_file.seqno ge catch2d_file.maxno then $
	catch2d_file.seqno = catch2d_file.image_no(x-1)
	if catch2d_file.seqno ge 0 and catch2d_file.seqno lt catch2d_file.maxno then begin
		viewscanimage_current  
		WIDGET_CONTROL, widget_ids.sel_image, SET_VALUE=catch2d_file.detector -1
	endif else  begin
		w_warningtext,'End of file',60,5,title='VIEW2D Messages'
	end
      END
  'VIEW2D_SLIDER': BEGIN
      WIDGET_CONTROL,Event.id, GET_VALUE=seqno
      WIDGET_CONTROL,widget_ids.seqno, SET_VALUE=strtrim(string(seqno),2)
      WIDGET_CONTROL,widget_ids.slider, SET_VALUE=seqno
      catch2d_file.scanno_current = seqno
      catch2d_file.seqno = catch2d_file.image_no(seqno-1) + catch2d_file.detector-1
	if catch2d_file.seqno gt catch2d_file.image_no(seqno) then $ 
		catch2d_file.seqno = catch2d_file.image_no(seqno-1)
	viewscanimage_current
	WIDGET_CONTROL, widget_ids.sel_image, SET_VALUE=catch2d_file.detector-1
      END
  'PICK_XDR': BEGIN
	catch2d_file.XDR = Event.Index
	END
  'PICK_2DXAXIS': BEGIN
	if catch2d_file.maxno eq 0 then return
	view_option.pickx = Event.Index + 1
	view2d_changeXaxis
	REPLOT
	END
  'IMAGE_PAN': BEGIN
	view2d_pan_images_on
      END
  'BASEVIEW_FIRST': BEGIN
	viewscanimage_first
	END
  'BASEVIEW_NEXT': BEGIN
	viewscanimage_next
	END
  'BASEVIEW_PREV': BEGIN
	viewscanimage_prev
	END
  'BASEVIEW_LAST': BEGIN
	viewscanimage_last
	END
  'IMAGE186': BEGIN
	scanno = catch2d_file.scanno_current
	if scanno le 0 then begin
		st = 'You have to load the scan # in first'
		w_warningtext,st, 60,3,title='VIEW2D Messages'
		return
		end
	begin_seqno = catch2d_file.image_no(scanno-1)	
	end_seqno = catch2d_file.image_no(scanno)	
	seqno = begin_seqno + event.value
	if seqno lt end_seqno then begin
		catch2d_file.seqno = seqno
		if XRegistered('w_warningtext') then $
		WIDGET_CONTROL,w_warningtext_ids.base,BAD=bad,/DESTROY
		viewscanimage_current
	endif else begin
		st = [ $
		'No more image for SCAN #' + string(scanno), $
		'Total number of images for this scan is ' + $
		string(end_seqno - begin_seqno) ]
		w_warningtext,st, 60,5,title='VIEW2D Messages'
	end
	END
  'CURSOR62_X': BEGIN
;	if view_option.versus eq 1 then begin
;		r=dialog_message(["Not available for 'Plot vs Values'","Must change to 'Plot vs Step#' first"],/error)
;		return
;	end
;	WIDGET_CONTROL,Event.id,GET_VALUE=x
;	WIDGET_CONTROL,widget_ids.y_cursor,GET_VALUE=y
;	catch2d_zcursor,fix(x(0)),fix(y(0)),z
;	if z ne '' then  catch2d_ydist,fix(x(0)), Event
	END
  'CURSOR62_Y': BEGIN
;	if view_option.versus eq 1 then begin
;		r=dialog_message(["Not available for 'Plot vs Values'","Must change to 'Plot vs Step#' first"],/error)
;		return
;	end
;	WIDGET_CONTROL,Event.id,GET_VALUE=y
;	WIDGET_CONTROL,widget_ids.x_cursor,GET_VALUE=x
;	catch2d_zcursor,fix(x(0)),fix(y(0)),z
;;	if z ne '' then  catch2d_xdist,fix(y(0)), Event
	END

  'CURSOR62_X1': BEGIN
        END
  'CURSOR62_X2': BEGIN
        END
  'CURSOR62_Y1': BEGIN
        END
  'CURSOR62_Y2': BEGIN
        END
  'CURSOR62_XYRANGE': BEGIN
	if view_option.CA then cursor62_caput,3
        END

  'CURSOR62_XZ': BEGIN
;	if catch2d_file.xzdraw eq 0 then begin
;        st = ['Click MMB in the 2D image area first!','Before press the XZ button.']
;	w_warningtext, st,60,5,title='VIEW2D Messages',xloc=500
;        return
;        end
;
;	w_warningtext,['Query XZ Profile Cursor: Left Mouse Button', $
;                       '     Stop Query : Other Buttons'],60,5, $
;			title='VIEW2D Messages',xloc=500
;	catch2d_xycoord1,st
	END
  'CURSOR62_YZ': BEGIN
;	if catch2d_file.yzdraw eq 0 then begin
;        st = ['Click MMB in the 2D image area first!','Before press the YZ button.']
;	w_warningtext, st,60,5,title='VIEW2D Messages',xloc=500
;        return
;        end
;
;	w_warningtext,['Query YZ Profile Cursor: Left Mouse Button', $
;                       '     Stop Query : Other Buttons'],60,5, $
;			title='VIEW2D Messages',xloc=500
;	catch2d_xycoord2,st
	END
  'ASCII_FORMAT': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=f
	view_option.format = f(0)
	view2d_datatotext
	END
  'ASCII_DATA': BEGIN
	view2d_datatotext
	END
  'TEXT133': BEGIN
      Print, 'Event for Information Block'
      END
  'CURSOR62_CAPUT0': BEGIN
	if view_option.CA then cursor62_caput,1
	END
  'CURSOR62_CAPUT': BEGIN
	if view_option.CA then cursor62_caput,2
	END
  ENDCASE
END

PRO catch2d_zcursor,x,y,z
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file
; x y is index number 

	z=''
	if x lt 0 or x ge catch2d_file.x_act_npts then begin 
	 r = dialog_message('X Index out of range',/error)
	 return
	end
	if  y lt 0 or y ge catch2d_file.y_act_npts then begin
	 r = dialog_message('Y Index out of range',/error)
	 return
	end

	z = string(image(x,y))
	WIDGET_CONTROL,widget_ids.z_cursor,SET_VALUE=strtrim(z,2)

	hide_cross,view_option.x,view_option.y,view_option.d_wid,view_option.s_wid
	view_option.x = view_option.margin_l + x*catch2d_file.x_mag
	view_option.y = view_option.margin_b + y*catch2d_file.y_mag
	show_cross,view_option.x,view_option.y,view_option.d_wid,view_option.s_wid

END

; DO NOT REMOVE THIS COMMENT: END main13_2
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO view2d, GROUP=Group, file=file, XDR=XDR,CA=CA
;
;+
; NAME:
;       VIEW2D	
;
; PURPOSE:
;       This program provides the EPICS user a convenient IDL 2D scan data 
;       display tool.  Its input image file is automatically saved by the  
;       data catcher program CATCHER_V1. 
;
;       Currently, this program provides TV, SURFACE, CONTOUR, SHOW3, and 
;       PLOT2D plot. It also provides simple xz, yz line plot and data
;       value query information.
;
; CATEGORY:
;	Widgets. 
;
; CALLING SEQUENCE:
;	VIEW2D
;
; INPUTS:
;       None.	
;
; KEYWORD PARAMETERS:
;     GROUP:    The widget ID of the group leader of the widget.  If this 
;               keyword is specified, the death of the group leader results in
;               the death of VIEW2D.
;
;     FILE:    The input image file name.  If this keyword is specified, the
;              file should contain the image data must be in the data catcher
;              created format. 
;          
;     XDR:     If specified, indicates that the input is in XDR format.
;
; OUTPUTS:
;       It provides option of postscript plot of drawing area.
;
; COMMON BLOCKS:
;       None.
;
; RESTRICTIONS:
;	Drawing area is 460 x 400 pixels.
;
; PROCEDURE:
;       This program is available as an epics/extensions tool. It can be
;       directly accessed from the view data menu of the CATCHER_V1.  
; 
; EXAMPLE:
;       VIEW2D
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 02-27-96.
;       04-29-96 bkc  Add image button, 2d scan #, on-line help      	
;       09-30-96 bkc  Add cross hair to reflect the cursor position      	
;       11-21-96 bkc  Text window image out include x,y array vales     	
;		      Option of save and load private color map
;                     Take care out of sync 2D scan #
;                     Modify XZ, YZ profile distributions
;       12-13-96 bkc  Add zoom-in, zoom-off, printer support,
;                     color PS plot, rewrite TV, equal aspect TV
;                     ratio, reverse PS video, cursor features, etc
;       04-11-97 bkc  Call the MAX function to give the max location
;       08-08-97 bkc  Release R2.1
;                     Cursor position readout fixed by catcher R2.1
;                     Fix the Y title cut off problem for PostScript 
;                     More useful plots and printouts info for TV screen
;                     and PostScript output
;                        min, max values and locations
;                        detector database  description
;                        positioner database description
;       12-15-97 bkc  Release R2.2
;                     Add the curve fitting support
;                     Rename Refresh button to ReNew
;                     Use scroll window for base widget
;                     Reverse PS color becomes an option in PS_printer setup 
;                     Contour plot support 24 bit visual device
;                     ASCII ... allows the user override default filename
;				try data directory first, then start directory,
;				then home directory
;       01-12-98 bkc  Modify the view2d.init file to increase the allowable 
;                     image size to 1000x1000
;       04-09-98 bkc  Release R2.3
;                     If bad image data detected, at least let the user view
;                     all the good image so far read into view2d
;       05-14-98 bkc  TV legend uses the first color entry from the color table
;                     If reverse legend color is set, use the last color entry
;                     from the color table
;       05-26-98 bkc  Remove scroll bar problem on the main window
;       08-31-98 bkc  Release R2.3a
;                     Fix the problem of only 1 set of 2D scan in data, 
;                     Add nodata keyword for axis plot for TV image
;       09-31-98 bkc  Release R2.3b
;                     Add save image option for AIM program
;       01-09-99 bkc  Release R2.3c,  replace the XSURFACE by PLOT2D 
;                     Dynamic read in u_read PS_open cw_term plot1d plot2d source 
;                     Fix the TV plot problem of Step # option
;       05-14-99 bkc  release R2.3d
;                     Add colorbar, scan2d_roi support
;                     Add the option of saving as TIFF or GIF useing rename dialog
;       06-02-99 bkc  release R2.3e
;                     Tiff, gif data will be saved under TIFF directory
;                     Add 2D-ROI menu, ROIs..., UpdtRpt..., AppendRpt...
;                     Add scan2dROIRpt for all detectors for a given 2D scanno
;       09-14-99 bkc  release R2.3f
;                     open_binary_type default to XDR  (support alpha too)
;                     Add 2D image ASCII report format option
;       11-02-99 bkc  release R2.3g
;                     Add support of saving TIFF/GIF pan images
;                     Upgrade the readascii user interface with table support
;                     Add support of image calibration
;                     New files saved will be in corresponding sub-directory
;       01-20-00 bkc  Increase the X,Y array to 2001
;                     Suppress the axis plot of TV if constant value is found
;       01-25-00 bkc  Automatically set the Xmin,Xmax,Ymin,Ymax widget
;                     release R2.3g+
;                     Add the set new postions button if caInit found
;       06-01-00 bkc  Release R2.3h
;                     Add the Color->ColorBar Config ... dialog for configuring
;                     the colorbar on 2D image
;       12-05-00 bkc  Release R2.3h+
;                     Delete GIF, add XDR option save image as XDR data
;                     Modify plot2d save in XDR too
;       06-13-01 bkc  Release R2.4
;                     Add the option of plot against to P2,P3,P4 X axis
;                     Add re-assign dnames to dcviewer driver
;                     Read user preferred detector names from '.tmpName'
;                     Scan2d_roi support save filter ROI to pass into multiroi
;       03-20-02 bkc  Release R2.5
;                     Add XL,XR,YL,YR fields reflecting ROI for scan
;                     Add set new 2D scan start and end position
;-
;
@os.init
PS_init

if XRegistered('main13_2') ne 0  then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  version = 'VIEW2D (R2.5)'

  main13_2 = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $; SCR_XSIZE=750, SCR_YSIZE=820, /SCROLL, $
      MAP=1, $
      TITLE= version, $
      UVALUE='main13_2')

  BASE68 = WIDGET_BASE(main13_2, $
      COLUMN=2, $
      MAP=1, $
      TITLE='Top Menu Line', $
      UVALUE='BASE68')

  BASE190 = WIDGET_BASE(BASE68, $
      COLUMN=3, $
;      FRAME=2, $
      MAP=1, $
      TITLE='menu base', $
      UVALUE='BASE190')


  MenuDesc907 = [ $
      { CW_PDMENU_S,       3, 'File' }, $ ;        0
        { CW_PDMENU_S,       0, 'Open ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Save Image for AIM' }, $ ;        2
        { CW_PDMENU_S,       0, 'Save as TIFF' }, $ ;        2
        { CW_PDMENU_S,       0, 'Save as R-TIFF' }, $ ;        2
        { CW_PDMENU_S,       0, 'Save as XDR' }, $ ;        2
        { CW_PDMENU_S,       0, 'Printer ...' }, $ ;        2
        { CW_PDMENU_S,       0, 'Print' }, $ ;        2
        { CW_PDMENU_S,       0, 'PS_close' }, $ ;        2
        { CW_PDMENU_S,       2, 'Quit' } $  ;      6
  ]


  PDMENU189 = CW_PDMENU( BASE190, MenuDesc907, /RETURN_FULL_NAME, $
      UVALUE='PDMENU189')

  MenuDesc909 = [ $
      { CW_PDMENU_S,       3, 'Color' }, $ ;        0
        { CW_PDMENU_S,       0, 'Reverse Background' },  $  ;      1
        { CW_PDMENU_S,       0, 'Save Private Color Table' }, $  ;      1
        { CW_PDMENU_S,       0, 'Load Private Color Table' }, $  ;      1
        { CW_PDMENU_S,       0, 'Change Color Table ...' }, $  ;      1
        { CW_PDMENU_S,       0, 'Image Color Scheme ...' }, $  ;      1
        { CW_PDMENU_S,       0, 'ColorBar Config ...' },  $  ;      1
      { CW_PDMENU_S,       3, 'Font' }, $ ;        0
        { CW_PDMENU_S,       1, 'Name' }, $ ;        1
          { CW_PDMENU_S,       0, 'Default' }, $ ;        2
          { CW_PDMENU_S,       0, 'Courier' }, $ ;        3
          { CW_PDMENU_S,       0, 'Helvetica' }, $ ;        4
          { CW_PDMENU_S,       0, 'Times' }, $ ;        5
          { CW_PDMENU_S,       2, 'Symbol' } $ ;        6

  ]

  PDMENU188 = CW_PDMENU( BASE190, MenuDesc909, /RETURN_FULL_NAME, $
      UVALUE='PDMENU188')



  MenuDesc911 = [ $
      { CW_PDMENU_S,       3, 'Help' }, $ ;        0
        { CW_PDMENU_S,       2, 'Help ...' } $ ;        1
	]
  PDMENU189_help = CW_PDMENU( BASE190, MenuDesc911, /RETURN_FULL_NAME, $
      UVALUE='PDMENU189_help')


  BASE177 = WIDGET_BASE(BASE68, $
      ROW=1, $
;      FRAME=1, $
      MAP=1, $
      TITLE='image/surf select base', $
      UVALUE='BASE177')


  Btns912 = ['TV','Eq.TV.AspRt','SURFACE','CONTOUR','SHOW3','PLOT2D...']
  surface_plot = WIDGET_DROPLIST(BASE177, VALUE=BTNS912, $
	UVALUE='SURFACE_PLOT',TITLE='View as')

  Btns915 = ['By Image', 'By User']
  BGROUP184 = WIDGET_DROPLIST(BASE177, VALUE=Btns915, $
	UVALUE='BGROUP184', TITLE='Pixel')

  Btns918 = ['Step #', 'Values']
  plot_versus = WIDGET_DROPLIST(BASE177, VALUE=Btns918, $
	UVALUE='PLOTVERSUS', TITLE='Plot vs')

  Btns915 = ['BIN','XDR']
  pick_xdr = WIDGET_DROPLIST(BASE177, VALUE=BTNS915, $
        UVALUE='PICK_XDR',TITLE='')


; add the view mode widgets

  BASE185 = WIDGET_BASE(main13_2, $
      ROW=1, MAP=1, $
;	FRAME=1, $
      TITLE='View btns', $
      UVALUE='BASE185')

  pick_xaxis = WIDGET_DROPLIST(BASE185, VALUE=['P1','P2','P3','P4'], $
        UVALUE='PICK_2DXAXIS',TITLE='Xaxis')

  base185_1 = widget_base(BASE185,/row,/frame)
  ascii_data = WIDGET_BUTTON( BASE185_1, VALUE='ASCII ...', $
      UVALUE='ASCII_DATA')

  ascii_fmt = widget_text( BASE185_1,VALUE='G17.7', $
      ysize=1, /editable, $
      UVALUE='ASCII_FORMAT', XSIZE=5)

  refresh_data = WIDGET_BUTTON( BASE185, VALUE='ReNew', $
      UVALUE='REFRESH_DATA')

  baseview_first = WIDGET_BUTTON( BASE185, VALUE='First', $
      UVALUE='BASEVIEW_FIRST')
  baseview_next = WIDGET_BUTTON( BASE185, VALUE='Next', $
      UVALUE='BASEVIEW_NEXT')
  baseview_prev = WIDGET_BUTTON( BASE185, VALUE='Prev', $
      UVALUE='BASEVIEW_PREV')
  baseview_last = WIDGET_BUTTON( BASE185, VALUE='Last', $
      UVALUE='BASEVIEW_LAST')

  FIELD100 = CW_FIELD( BASE185,VALUE='0', $
      ROW=1, INTEGER=1, RETURN_EVENTS=1, $
      TITLE='Scan #', UVALUE='VIEW2D_SEQNO', XSIZE=5)

; add detectors

  BASE186 = WIDGET_BASE(main13_2, $
      ROW=1, $
      MAP=1, $ ; /scroll, $
      TITLE='Detector btns', $
      UVALUE='BASE186')
  Btns_detector = [ $
    '1', $
    '2', $
    '3', $
    '4', $
    '5', $
    '6', $
    '7', $
    '8', $
    '9', $
    '10', $
    '11', $
    '12', $
    '13', $
    '14', $
    '15' $
         ]

  found = findfile('.tmpName',count=ct)
  if ct gt 0 then begin
	xdr_open,unit,'.tmpName'
	xdr_read,unit,dnames
	xdr_close,unit
  end
  if n_elements(dnames) then Btns_detector = dnames[0:14]
  IMAGE186 = CW_BGROUP( BASE186, Btns_detector, $
      ROW=2, EXCLUSIVE=1, LABEL_LEFT='Images', /NO_RELEASE, $
      UVALUE='IMAGE186')

  BASE62 = WIDGET_BASE(main13_2, $
      COLUMN=2, $
      MAP=1, $
      TITLE='Plot Area', $
      UVALUE='BASE62')

  BASE62 = WIDGET_BASE(main13_2, $
      COLUMN=2, $
      MAP=1, $
      TITLE='Plot Area', $
      UVALUE='BASE62')
 
  PLOT62 = WIDGET_BASE( BASE62, /COLUMN)
  LABEL60 = WIDGET_LABEL( PLOT62, $
      UVALUE='LABEL60', $
      VALUE='3 Buttons: LMB -Values, MMB - Line plots, RMB - Zoom_In')
 
  DRAW62 = WIDGET_DRAW( PLOT62, $
      BUTTON_EVENTS=1, $
      RETAIN=2, $
      UVALUE='DRAW62', $
      XSIZE=460, $
      YSIZE=400)


  IMAGE62 = WIDGET_BASE( BASE62, /COLUMN )
  IMAGE62_L0 = WIDGET_LABEL( IMAGE62, $
      VALUE='--IMAGE--')
  MINBASE = WIDGET_BASE( IMAGE62, /ROW)
  IMAGE62_L1 = WIDGET_LABEL( MINBASE, $
      VALUE='MIN Z:')

  str='                        '
  CURSOR62_ZMIN = WIDGET_LABEL( MINBASE, /DYNAMIC_RESIZE, $
      VALUE=str, UVALUE='CURSOR62_ZMIN')

  CURSOR62 = WIDGET_BASE( IMAGE62, /ROW )
  CURSOR62_L1 = WIDGET_LABEL( CURSOR62, $
      VALUE='MAX Z:')
  CURSOR62_ZMAX = WIDGET_LABEL( CURSOR62, /DYNAMIC_RESIZE, $
      VALUE=str, UVALUE='CURSOR62_ZMAX')

  CURSOR62_B1 = WIDGET_BASE( IMAGE62, /COLUMN,/FRAME)

if keyword_set(CA) then begin
CURSOR62_B2 = WIDGET_BASE( CURSOR62_B1, /ROW)
putp1pvbutton = WIDGET_BUTTON(CURSOR62_B2,VALUE='Set New P1PV...', $
		UVALUE='CURSOR62_CAPUT0')
putp1cpbutton = WIDGET_BUTTON(CURSOR62_B2,VALUE='Set New P1CP...', $
		UVALUE='CURSOR62_CAPUT')
end

  CURSOR62_XL = WIDGET_LABEL( CURSOR62_B1, $
      VALUE='Cursor @ X')
  CURSOR62_X = WIDGET_TEXT( CURSOR62_B1, VALUE='', $
	/EDITABLE, /NO_NEWLINE, $
      UVALUE='CURSOR62_X', $
	XSIZE=20, YSIZE=1)
	
  CURSOR62_YL = WIDGET_LABEL( CURSOR62_B1, $
      VALUE='Cursor @ Y')
  CURSOR62_Y = WIDGET_TEXT( CURSOR62_B1, VALUE='', $
	/EDITABLE, /NO_NEWLINE, $
      UVALUE='CURSOR62_Y', $
	XSIZE=20, YSIZE=1)
	
  CURSOR62_B3 = WIDGET_BASE( IMAGE62, /ROW)
  CURSOR62_ZL = WIDGET_LABEL( CURSOR62_B3, $
      VALUE='Z:')
  CURSOR62_Z = WIDGET_LABEL( CURSOR62_B3, VALUE=' ', XSIZE=150, $
      UVALUE='CURSOR62_Z')

  CURSOR62_B4 = WIDGET_BASE( IMAGE62, /COLUMN,frame=2)
  CURSOR62_B41 = WIDGET_BASE( CURSOR62_B4, /ROW,/frame)
  CURSOR62_X1 = CW_FIELD( CURSOR62_B41, VALUE='', title='XL',/float, $
        /RETURN_EVENTS, UVALUE='CURSOR62_X1', XSIZE=10, YSIZE=1)
  CURSOR62_X2 = CW_FIELD( CURSOR62_B41, VALUE='', title='XR',/float, $
        /RETURN_EVENTS, UVALUE='CURSOR62_X2', XSIZE=10, YSIZE=1)

  CURSOR62_B42 = WIDGET_BASE( CURSOR62_B4, /ROW,/frame)
  CURSOR62_Y1 = CW_FIELD( CURSOR62_B42, VALUE='',title='YL', /float, $
        /RETURN_EVENTS, UVALUE='CURSOR62_Y1', XSIZE=10, YSIZE=1)
  CURSOR62_Y2 = CW_FIELD( CURSOR62_B42, VALUE='', title='YR',/float, $
        /RETURN_EVENTS, UVALUE='CURSOR62_Y2', XSIZE=10, YSIZE=1)

if keyword_set(CA) then $
  CURSOR62_B5 = WIDGET_BUTTON( CURSOR62_B4, $
      VALUE='Set New 2D Scan Ranges...',UVALUE='CURSOR62_XYRANGE')


;  CURSOR62_PL = WIDGET_LABEL( CURSOR62_B4, $
;      VALUE='PROBE:')
;  CURSOR62_XZ = WIDGET_BUTTON( CURSOR62_B4, $
;      UVALUE='CURSOR62_XZ', VALUE='XZ')
;  CURSOR62_XZL = WIDGET_LABEL( CURSOR62_B4, VALUE=' ', XSIZE=250)
;  CURSOR62_YZ = WIDGET_BUTTON( CURSOR62_B4, $
;      UVALUE='CURSOR62_YZ', VALUE='YZ')
;  CURSOR62_YZL = WIDGET_LABEL( CURSOR62_B4, VALUE=' ', XSIZE=250)


  BASE151 = WIDGET_BASE(main13_2, $
      COLUMN=1, $
;      FRAME=2, $
      MAP=1, $
      TITLE='Plot Limits', $
      UVALUE='BASE151')

;  LABEL152 = WIDGET_LABEL( BASE151, $
;      UVALUE='LABEL152', $
;      VALUE='User Entered Indices of Plot Range')

  BASE153 = WIDGET_BASE(BASE151, $
      ROW=1, MAP=1, $
      TITLE='user entered fields', $
      UVALUE='BASE153')

  BASE154 = WIDGET_BASE(BASE153, $
      ROW=1, FRAME=1, MAP=1, $
      TITLE='user entered fields', $
      UVALUE='BASE154')

  FieldVal947 = [ $
    '' ]
  FIELD161 = CW_FIELD( BASE154,VALUE=FieldVal947, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Xmin', $
      UVALUE='FIELD161', $
      XSIZE=4)

  FieldVal945 = [ $
    '' ]
  FIELD159 = CW_FIELD( BASE154,VALUE=FieldVal945, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Xmax', $
      UVALUE='FIELD159', $
      XSIZE=4)

  FieldVal943 = [ $
    '' ]
  FIELD157 = CW_FIELD( BASE154,VALUE=FieldVal943, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Ymin', $
      UVALUE='FIELD157', $
      XSIZE=4)

  FieldVal941 = [ $
    '' ]
  FIELD246 = CW_FIELD( BASE154,VALUE=FieldVal941, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Ymax', $
      UVALUE='FIELD246', $
      XSIZE=4)

  VIEW2D_ZMIN = CW_FIELD( BASE154,VALUE=0., $
      ROW=1, $
      FLOATING=1, $
      RETURN_EVENTS=1, $
      TITLE='Zmin', $
      UVALUE='VIEW2D_ZMIN', $
      XSIZE=8)

  VIEW2D_ZMAX = CW_FIELD( BASE154,VALUE=0., $
      ROW=1, $
      FLOATING=1, $
      RETURN_EVENTS=1, $
      TITLE='Zmax', $
      UVALUE='VIEW2D_ZMAX', $
      XSIZE=8)

  BASE129 = WIDGET_BASE(main13_2, $
      ROW=1, FRAME=2, MAP=1, TITLE='Info Block', $
      UVALUE='BASE129')

  BASE129_1 = WIDGET_BASE(BASE129, $
      COL=1, MAP=1)

  MenuPANImage = [ $
      { CW_PDMENU_S,       3, 'PanImage' }, $ ;        0
      { CW_PDMENU_S,       1, 'PanImages' }, $ ;        0
        { CW_PDMENU_S,       0, 'PanImages...' }, $ ;        1
        { CW_PDMENU_S,       0, 'PanImages+TIFF' }, $ ;        1
        { CW_PDMENU_S,       0, 'PanImages+RTIFF' }, $ ;        1
        { CW_PDMENU_S,       2, 'PanImages+XDR' }, $ ;        1
      { CW_PDMENU_S,       0, 'Overlay...' } , $ ;        0
      { CW_PDMENU_S,       2, 'Calibration...' } $ ;        0
	]
  PDMENU2D_panimage = CW_PDMENU( BASE129_1, MenuPANImage, /RETURN_FULL_NAME, $
      UVALUE='PDMENU2D_PANIMAGE')
  MenuROI = [ $
      { CW_PDMENU_S,       3, '2D-ROI' }, $ ;        0
        { CW_PDMENU_S,       0, 'Help...' }, $ ;        1
        { CW_PDMENU_S,       0, 'ROI...' }, $ ;        1
        { CW_PDMENU_S,       1, 'Type' }, $ ;        1
        { CW_PDMENU_S,        0, 'RectROI' }, $ ;        1
        { CW_PDMENU_S,        0, 'FilterROI' }, $ ;        1
        { CW_PDMENU_S,        2, 'PolyROI' }, $ ;        1
        { CW_PDMENU_S,       0, 'AppendRpt...' }, $ ;        1
        { CW_PDMENU_S,       0, 'ReplaceRpt...' }, $ ;        1
        { CW_PDMENU_S,       0, 'ViewRpt...' }, $ ;        1
        { CW_PDMENU_S,       2, 'RenameRpt...' } $ ;        1
        ]
  PDMENU2D_fitting = CW_PDMENU( BASE129_1, MenuROI, /RETURN_FULL_NAME, $
      UVALUE='PDMENU2D_ROI')

  MenuFitting = [ $
      { CW_PDMENU_S,       3, 'Fitting' }, $ ;        0
        { CW_PDMENU_S,       0, 'Ez_Fit ...' }, $ ;        1
        { CW_PDMENU_S,       2, '2D Binary' } $ ;        1
        ]
  PDMENU2D_fitting = CW_PDMENU( BASE129_1, MenuFitting, /RETURN_FULL_NAME, $
      UVALUE='PDMENU2D_FITTING')

  TextVal952 = [ $
    '' ]
  TEXT133 = WIDGET_TEXT( BASE129,VALUE=TextVal952, $
;      EDITABLE=1, $
      UVALUE='TEXT133', /SCROLL, $
      XSIZE=70, $
      YSIZE=5)

  WIDGET_CONTROL, main13_2, /REALIZE

  ; Get drawable window index

  COMMON DRAW62_Comm, DRAW62_Id
  WIDGET_CONTROL, DRAW62, GET_VALUE=DRAW62_Id

@view2d.init
if keyword_set(CA) then view_option.CA = 1
  WIDGET_CONTROL, surface_plot, SET_DROPLIST_SELECT=view_option.surface
  WIDGET_CONTROL, BGROUP184, SET_DROPLIST_SELECT=view_option.user
  WIDGET_CONTROL, plot_versus, SET_DROPLIST_SELECT=view_option.versus

catch2d_file.version = version

; get path if file defined
  if keyword_set(file) then begin
	catch2d_file.name = file

    if catch2d_file.name ne '' then begin
	 found=findfile(catch2d_file.name)
	 if found(0) ne '' then begin
 		if keyword_set(XDR) then catch2d_file.XDR = 1 
		viewscanimage_init,catch2d_file.name
	 endif else begin
		w_warningtext,'Error: file not found - '+catch2d_file.name, $
			60,5,title='VIEW2D Messages'
	end
    end
	pos = rstrpos(file,OS_SYSTEM.file_sep)   ;'/'
	if pos gt 0 then begin
		catch2d_file.path = strmid(file,0,pos+1)
		catch2d_file.name = strmid(file,pos+1,strlen(file))
	endif else begin
		catch2d_file.path = catch2d_file.home
	end
  end

  if catch2d_file.XDR eq 1 then $
  WIDGET_CONTROL,pick_xdr,set_droplist_select = 1

  XMANAGER, 'main13_2', MAIN13_2 ; ,/NO_BLOCK

END


