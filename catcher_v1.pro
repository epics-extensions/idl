;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
; $Id: catcher_v1.pro,v 1.54 2003/09/15 21:31:24 cha Exp $

; Copyright (c) 1991-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
PRO XDispFile_evt, event


WIDGET_CONTROL, event.top, GET_UVALUE = state
WIDGET_CONTROL, event.id, GET_UVALUE=Ev

CASE Ev OF 
'EXIT': WIDGET_CONTROL, event.top, /DESTROY
'FILE_PRINT': begin
	r = findfile(state.filename,count=ct)
	if r(0) ne '' then begin
		PS_enscript,state.filename
	endif else begin
	WIDGET_CONTROL,state.filetext,GET_VALUE=str
	openw,unit,'tmp',/GET_LUN
	for i=0,n_elements(str)-1 do printf,unit,str(i)
	FREE_LUN,unit
	PS_enscript,'tmp'
	end
   end
ENDCASE
END


PRO XDisplayFile, FILENAME, TITLE = TITLE, GROUP = GROUP, WIDTH = WIDTH, $
	HEIGHT = HEIGHT, TEXT = TEXT, FONT = font, BLOCK=block,MODAL=MODAL
;+
; NAME: 
;	XDISPLAYFILE
;
; PURPOSE:
;	Display an ASCII text file using widgets and the widget manager.
;
; CATEGORY:
;	Widgets.
;
; CALLING SEQUENCE:
;	XDISPLAYFILE, Filename
;
; INPUTS:
;     Filename:	A scalar string that contains the filename of the file
;		to display.  The filename can include a path to that file.
;
; KEYWORD PARAMETERS:
;	FONT:   The name of the font to use.  If omitted use the default
;		font.
;	GROUP:	The widget ID of the group leader of the widget.  If this 
;		keyword is specified, the death of the group leader results in
;		the death of XDISPLAYFILE.
;
;	HEIGHT:	The number of text lines that the widget should display at one
;		time.  If this keyword is not specified, 24 lines is the 
;		default.
;
;	TEXT:	A string or string array to be displayed in the widget
;		instead of the contents of a file.  This keyword supercedes
;		the FILENAME input parameter.
;
;	TITLE:	A string to use as the widget title rather than the file name 
;		or "XDisplayFile".
;
;	WIDTH:	The number of characters wide the widget should be.  If this
;		keyword is not specified, 80 characters is the default.
;
;	BLOCK:  Set this keyword to have XMANAGER block when this application 
;	        is registered. By default the Xmanager keyword NO_BLOCK 
;	        is set to 1
;	MODAL: 
; OUTPUTS:
;	No explicit outputs.  A file viewing widget is created.
;
; SIDE EFFECTS:
;	Triggers the XMANAGER if it is not already in use.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Open a file and create a widget to display its contents.
;
; MODIFICATION HISTORY:
;	Written By Steve Richards, December 1990
;	Graceful error recovery, DMS, Feb, 1992.
;       12 Jan. 1994  - KDB
;               If file was empty, program would crash. Fixed.
;       4 Oct. 1994     MLR Fixed bug if /TEXT was present and /TITLE was not.
;      14 Jul. 1995     BKC Increased the max line to variable size.
;      16 Jun. 1997     BKC Max dispalyable line is 10000 for non-unix OS system.
;      28 Aug. 1997     BKC Add the printer button, file name label, it uses the
;                       PS_print,file to print.
;      30 Jul. 2002     BKC Add the block, modal keywords take care the 
;                       animation help problem
;      09 Aug  2002     BKC fix the problem with no input filename case
;-
COMMON SYSTEM_BLOCK,OS_SYSTEM
                                                        ;use the defaults if
IF(NOT(KEYWORD_SET(HEIGHT))) THEN HEIGHT = 24		;the keywords were not
IF(NOT(KEYWORD_SET(WIDTH))) THEN WIDTH = 80		;passed in

if n_elements(block) eq 0 then block=0
noTitle = n_elements(title) eq 0

IF(NOT(KEYWORD_SET(TEXT))) THEN BEGIN
  IF noTitle THEN TITLE = FILENAME     
  OPENR, unit, FILENAME, /GET_LUN, ERROR=i		;open the file and then
  if i lt 0 then begin		;OK?
	a = [ !err_string, ' Can not display ' + filename]  ;No
  endif else begin

    y=10000
    if OS_SYSTEM.os_family eq 'unix' then begin
	spawn,[OS_SYSTEM.wc,'-l',FILENAME],y,/noshell

	lines=long(y(0))
	if lines eq 0 then begin
	res=WIDGET_MESSAGE('Unable to display '+FILENAME)
	return
	end
    end

	  a = strarr(y(0))				;Maximum # of lines
	  i = 0L
	  c = ''
	  while not eof(unit) do begin
		readf,unit,c
		a(i) = c
		i = i + 1
		if i ge y(0) then goto,stopread
		endwhile
	  stopread:
	  a = a(0:(i-1)>0)  ;Added empty file check -KDB
	  FREE_LUN, unit				;free the file unit.
  endelse
ENDIF ELSE BEGIN
    IF(NOT(KEYWORD_SET(TITLE))) THEN TITLE = 'XDisplayFile'
    a = TEXT
ENDELSE

ourGroup = 0L
if n_elements(group) eq 0 then ourGroup = widget_base() else ourGroup=group

if keyword_set(MODAL) then $
filebase = WIDGET_BASE(TITLE = TITLE, /MODAL, $		;create the base
		GROUP=ourGROUP,/COLUMN ) else $
filebase = WIDGET_BASE(TITLE = TITLE, $			;create the base
		GROUP=ourGROUP,/COLUMN ) 

label=WIDGET_LABEL(filebase,value=TITLE)
rowbtn = WIDGET_BASE(filebase,/ROW,TITLE='ROWBTN')
fileprint = WIDGET_BUTTON(rowbtn, $			;create a Print Button
		VALUE = "Print", $
		UVALUE = "FILE_PRINT")

filequit = WIDGET_BUTTON(rowbtn, $			;create a Done Button
		VALUE = "Done", $
		UVALUE = "EXIT")

IF n_elements(font) gt 0 then $
 filetext = WIDGET_TEXT(filebase, $			;create a text widget
		XSIZE = WIDTH, $			;to display the file's
		YSIZE = HEIGHT, $			;contents
		/SCROLL, FONT = font, $
		VALUE = a) $
ELSE filetext = WIDGET_TEXT(filebase, $			;create a text widget
		XSIZE = WIDTH, $			;to display the file's
		YSIZE = HEIGHT, $			;contents
		/SCROLL, $
		VALUE = a)

;state = { $
;	 base: filebase, $
;	 filetext: filetext, $
;	 file: '' $
;	 }
 
 state = { ourGroup: ourGroup, $
	filename: title, $
	filetext: filetext, $
	notitle: noTitle}

WIDGET_CONTROL,filebase,SET_UVALUE=state

WIDGET_CONTROL, filebase, /REALIZE			;instantiate the widget

Xmanager, "XDisplayFile", $				;register it with the
		filebase, $				;widget manager
		GROUP_LEADER = GROUP, $
		EVENT_HANDLER = "XDispFile_evt",NO_BLOCK=(NOT(FLOAT(block))) 

END  ;--------------------- procedure XDisplayFile ----------------------------

;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************


PRO RENAME_DIALOG_Event, Event
COMMON RENAME_BLOCK,rename_ids


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 
  'RENAME_PATH': BEGIN
	WIDGET_CONTROL,rename_ids.path_id,GET_VALUE=pathdir
	len = strlen(pathdir(0))
	found = findfile(pathdir(0),count=ct)
	if ct eq 0 then spawn, !os.mkdir +' '+ pathdir(0)
	END
  'RENAME_DIALOGACCEPT': BEGIN
	WIDGET_CONTROL,rename_ids.path_id,GET_VALUE=pathdir
	WIDGET_CONTROL,rename_ids.old_id,GET_VALUE=file1
	WIDGET_CONTROL,rename_ids.new_id,GET_VALUE=file2
	if strtrim(file2(0),2) eq '' then return
	len = strlen(pathdir(0))
	if strmid(pathdir(0),len-1,1) ne !os.file_sep then $
		pathdir = pathdir(0)+!os.file_sep
	found = findfile(pathdir(0),count=ct)
	if ct eq 0 then spawn,!os.mkdir+ ' '+pathdir(0)
	oldname = strtrim(file1(0),2)
	found = findfile(oldname)
	if found(0) eq '' then begin
		st =[ 'Filename: ','',oldname,'', 'not found!']
		res = dialog_message(st,/info)
		return
	end
	newname = pathdir(0)+strtrim(file2(0),2)

found = findfile(pathdir(0)+'*',count=ct)
if ct eq 0 then begin
	r = strpos(pathdir(0),!os.file_sep,2,/reverse_search,/reverse_offset)
        sdir = strmid(pathdir(0),r+1,strlen(pathdir(0))-r-2)
	r = dialog_message(['Directory: '+pathdir(0),' does not exist yet!!!', $
		'You have to first create the sub-directory :','',sdir], $
		/Info,title='Error in Rename')
	newname = dialog_pickfile(path=pathdir(0),get_path=p,file=file2(0),$
		title='Please create the '+sdir+' sub-directory')
end
	found = findfile(newname)
	if found(0) ne '' then begin
		st =[ 'Filename: ','', newname,'','already exists!', $
		    '','Do you want to over-write the old content?']
		res = dialog_message(st,/question)
		if res eq 'No' then return	
	end
	spawn,[!os.mv, oldname, newname],/noshell

	WIDGET_CONTROL,Event.Top,/DESTROY
      END
  'RENAME_DIALOGCANCEL': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END RENAME_DIALOG
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO rename_dialog, pathdir,oldname,newname, GROUP=Group
COMMON RENAME_BLOCK,rename_ids

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  RENAME_DIALOG = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
	TITLE='Rename File ...', $
      UVALUE='RENAME_DIALOG')

  BASE2 = WIDGET_BASE(RENAME_DIALOG, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  RENAME_OLD = CW_FIELD( BASE2,VALUE=oldname, $
      ROW=1, $
      STRING=1, $
  ;    RETURN_EVENTS=1, $
      TITLE='Old Filename:', $
      UVALUE='RENAME_OLD', $
      XSIZE=60)

  RENAME_PATH = CW_FIELD( BASE2,VALUE=pathdir, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Dest Path:', $
      UVALUE='RENAME_PATH', $
      XSIZE=60)

  new=''
  if n_elements(newname) then new=newname
  RENAME_NEW = CW_FIELD( BASE2,VALUE=new, $
      ROW=1, $
      STRING=1, $
  ;    RETURN_EVENTS=1, $
      TITLE='New Filename:', $
      UVALUE='RENAME_NEW', $
      XSIZE=60)

  BASE5 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE5')

  BUTTON6 = WIDGET_BUTTON( BASE5, $
      UVALUE='RENAME_DIALOGACCEPT', $
      VALUE='Accept')

  BUTTON7 = WIDGET_BUTTON( BASE5, $
      UVALUE='RENAME_DIALOGCANCEL', $
      VALUE='Cancel')

  rename_ids = { $
	path: pathdir, $
	oldname: oldname, $
	newname: oldname, $
	path_id: RENAME_PATH, $
	old_id: RENAME_OLD, $
	new_id: RENAME_NEW $
	}

  WIDGET_CONTROL, RENAME_DIALOG, /REALIZE

  XMANAGER, 'RENAME_DIALOG', RENAME_DIALOG
END
;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************

@u_read.pro

PRO catch1d_newIndexFile,file,array,XDR=XDR,print=print,TV=TV,header=header,nowrite=nowrite
;
; this will fix the index file if there is problem with index data 
;
; Get 1D data index
;
;      catch1d_newindexfile,file1,array,/xdr,/print
;
; Get 2D image file data index, the keyword TV must be set
;
; x1 = '2idd:scan1'
; y = make_array(60,/byte)
; y(0,0) = byte(x1)
;
;   catch1d_newindexfile,file,array,/xdr,/print,/tv,header=y
;

	filename = '/home/sricat/CHA/user/s2bmb/align2.xdr.bk'
	if n_elements(file) eq 0 then begin
		print,'Usage: catch1d_newIndexFile,File [,/XDR] [,/PRINT] [,HEADER=header]'
		return
	end
	filename = file

	buff1 = byte( 'CATCHER_V1')
	if keyword_set(header) then buff1 = byte(header)

	openr,unit,filename,/get_lun
	st = fstat(unit)
	buff = make_array(st.size,/byte)
	readu,unit,buff
	close,unit
	no=0
	array=0L
	xdr_offset = 28L
	sz = size(buff1)
	shift = sz(n_elements(sz)-1)  
	if keyword_set(TV) then begin
		xdr_offset = 24L
		end
	if keyword_set(print) then begin
		print,buff(0:50)
		print,st.size,shift,xdr_offset
	end

	for i=xdr_offset,st.size-shift do begin
		buff2 = buff(i:i+shift-1)
		diff = total(buff1-buff2) 
		if diff eq 0. then begin
		no = no + 1
		if keyword_set(XDR) then begin
			if keyword_set(print) then print,no,i-xdr_offset,buff(i:i+shift)
			if (i-xdr_offset) gt 0 then array = [array,i-xdr_offset]
			i = i + xdr_offset + shift+1
			endif else begin
			if keyword_set(print) then print,no,i-20,buff(i:i+shift)
			if (i-20) gt 0 then array = [array,i-20]
			i = i + 20 + shift+1
			end
		end
	end
	array = [array,st.size]

	if keyword_set(print) then begin
		print,st.name
		print,st.size
		print,no
		print,array
	end
	if keyword_set(nowrite) or keyword_set(TV) then return

	if !d.name eq 'WIN' then openw,unit,filename+'.index',/get_lun,/XDR else $
	openw,unit,filename+'.index',/get_lun
	u_write,unit,st.name
	u_write,unit,st.size
	u_write,unit,no
	u_write,unit,array
	close,unit
	u_close,unit
	print,'***New Index File generated***'
        print,filename+'.index'

END


PRO readfixindex,indexfile,fsize,maxno,array
; The fixed index file on WIN system will be save in XDR format
; this routine especially written for readin the fixed index file for WIN system
;
	if !d.name ne 'WIN' then return

	found = findfile(indexfile,count=ct)
	if ct eq 0 then return

	t = lonarr(5)
	openr,unit1,indexfile,/get_lun,/XDR 
	point_lun,unit1,0
	readu,unit1,t
	if t(0) eq 0 and t(1) eq 7 then fname=''
	readu,unit1,fname
	readu,unit1,t
	if t(0) eq 0 and t(1) eq 3 then fsize=0L 
	readu,unit1,fsize
	readu,unit1,t
	if t(0) eq 0 and t(1) eq 2 then maxno=0 
	readu,unit1,maxno
	readu,unit1,t
	if t(2) eq 3 then array = make_array(t(1),/long)
	readu,unit1,array
	free_lun,unit1
	close,unit1

END
; $Id: catcher_v1.pro,v 1.54 2003/09/15 21:31:24 cha Exp $

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
;  w_caset,labels,pvnames,title
;
PRO w_caset_get_array,pvnames

COMMON w_caset_block, w_caset_base, w_caset_ids, w_caset_narray, w_caset_varray

no = n_elements(pvnames)
if no eq 0 then return 

	w_caset_narray = pvnames 
	ln = cagetArray(pvnames,pd,/string)
	w_caset_varray = pd
END

PRO w_caset_put_array,pvnames,values

no = n_elements(pvnames)
if no eq 0 then return 
n2 = n_elements(values)
if  no ne n2 then begin
	w_warningtext,'Error: w_caset_put_array -- pvnames, values size not same'
endif else begin
	for i=0,no-1 do begin
	if strlen(values(i)) lt 1 then values(i) = ' '
	end
	i = caputArray(pvnames,values)
	if i eq -1 then w_warningtext,'Error: w_caset_put_array -- caput failed!'
	end
END

PRO w_caset_event,event

COMMON w_caset_block, w_caset_base, w_caset_ids, w_caset_narray, w_caset_varray

WIDGET_CONTROL, event.id, GET_UVALUE = eventval

no = n_elements(w_caset_narray)

CASE eventval OF
	
	"PVVALUE" : BEGIN
		WIDGET_CONTROL, event.id, GET_VALUE = newvalue 
                END
	"SET_PV_OK" : BEGIN
		for i=0,no-1 do begin
		WIDGET_CONTROL, w_caset_ids(i), GET_VALUE = newvalue 
		w_caset_varray(i) = newvalue(0)
		end
;
; null string will cause problem in CaWave/CaIDL so set to ' '
;
		for i=0,no-1 do begin
			if strlen(w_caset_varray(i)) eq 0L then $
				w_caset_varray(i) = ' '
			end
;
; put the new value to IOC
;
		if caputArray(w_caset_narray,w_caset_varray) ne 0 then begin
			print,'SET_PV_OK' 
			for i=0,no-1 do print,w_caset_varray(i),'$'
			w_warningtext,' failed on array put. '
			end
                END
	"SET_PV_REFRESH" : BEGIN
		ln = cagetArray(w_caset_narray,pd,/string)
		w_caset_varray = pd
		for i=0,no-1 do begin
		WIDGET_CONTROL, w_caset_ids(i), SET_VALUE = w_caset_varray(i)
			end
		END
	"SET_PV_CANCEL" : BEGIN
		WIDGET_CONTROL, event.top, /DESTROY
		END
ENDCASE

END

PRO w_caset,labels,pvnames,title, GROUP = GROUP, help=help

COMMON w_caset_block, w_caset_base, w_caset_ids, w_caset_narray, w_caset_varray

if XRegistered('w_caset') ne 0 then return

if keyword_set(help) then begin
	st = [$
	"Usage: w_caset,label_array,pvname_array,header_title",$
	'',$
	'This function display and reset the  channel access values',$
	'',$
	'   where    label_array      gives a string description for pvname ',$
	'            pvname_array     specifies pvnames ',$
	'            header_title     specifies header for dialog']
	w_warningtext,st
	return
	end

no = n_elements(pvnames)
w_caset_narray = make_array(no,/string,value=string(replicate(32b,30)))
w_caset_varray = make_array(no,/string,value=string(replicate(32b,40)))

if n_elements(labels) ne no then begin
	w_warningtext,'Error: w_caset --> labels, pvnames size not same'
	return
	end 
if n_elements(pvnames) gt 0 then begin
	w_caset_get_array,pvnames
	end

if strlen(title) eq 0 then title = 'SET PV ARRAY VALUES'

w_caset_base=WIDGET_BASE(TITLE = 'w_caset Widget', /COLUMN)     

row0 = WIDGET_BASE(w_caset_base, /COLUMN, /FRAME)
scanpvlabel = WIDGET_LABEL(row0, VALUE=title)

;row1 = make_array(no,/long,value=0L)
w_caset_ids = make_array(no,/long,value=0L)

for i = 0,no-1 do begin
row1 = WIDGET_BASE(w_caset_base, /ROW)
temp = string(replicate(32b,50))
strput,temp,labels(i),0
strput,temp,'[',30
strput,temp,w_caset_narray(i),31
l1 = strlen(w_caset_narray(i))
strput,temp,']',31+l1
caset_label = WIDGET_LABEL(row1, VALUE=temp )
w_caset_ids(i) = WIDGET_TEXT(row1, /EDITABLE, /NO_NEWLINE, $
		 XSIZE=30, YSIZE=1, VALUE=w_caset_varray(i), UVALUE='PVVALUE')
end


lastrow = WIDGET_BASE(w_caset_base, /ROW)
w_caset_ok = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Apply ', $
                        UVALUE = 'SET_PV_OK')
w_caset_refresh = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Refresh ', $
                        UVALUE = 'SET_PV_REFRESH')
w_caset_cancel = WIDGET_BUTTON(lastrow, $
                        VALUE = 'Cancel', $
                        UVALUE = 'SET_PV_CANCEL')

; set widget ids :

; Realize the widgets:
WIDGET_CONTROL, w_caset_base, /REALIZE

; Hand off to the XMANAGER:
XMANAGER, 'w_caset', w_caset_base, GROUP_LEADER = GROUP

END


;
;  w_casetMonitor,labels,pvnames,title
;
PRO w_casetMonitor_get_array,pvnames,no

COMMON w_casetMonitor_block, w_casetMonitor_ids, w_casetMonitor_narray, w_casetMonitor_varray,w_casetMonitor_id

if no eq 0 then return 

	w_casetMonitor_narray = pvnames 
 	w_casetMonitor_id.add = caMonitor(pvnames,/add)
	ln = cagetArray(pvnames,pd,/string)
	w_casetMonitor_varray = pd
END

PRO w_casetMonitor_put,ind,value
COMMON w_casetMonitor_block, w_casetMonitor_ids, w_casetMonitor_narray, w_casetMonitor_varray,w_casetMonitor_id
if ind lt n_elements(w_casetMonitor_narray) and ind ge 0 then begin
	if strlen(value) lt 1 then value = ' '
	w_casetMonitor_varray(ind) = value 
	i=caput(w_casetMonitor_narray(ind),value)
	end
END

PRO w_casetMonitor_event,event

COMMON w_casetMonitor_block, w_casetMonitor_ids, w_casetMonitor_narray, w_casetMonitor_varray,w_casetMonitor_id

no = n_elements(w_casetMonitor_narray)

WIDGET_CONTROL, event.id, GET_UVALUE = eventval
if (event.id eq event.handler) then begin
; update screen
ind = caMonitor(w_casetMonitor_narray,/check)
if total(ind) gt 0 then begin
;	w_casetMonitor_varray = caget(w_casetMonitor_narray,/string,/monitor)
	ln = cagetArray(w_casetMonitor_narray,pd,/string)
	w_casetMonitor_varray = pd
	for i=0,no-1 do begin
	if ind(i) eq 1 then $
	WIDGET_CONTROL,w_casetMonitor_ids(i), SET_VALUE = w_casetMonitor_varray(i)
	end
end
WIDGET_CONTROL, w_casetMonitor_ids(no), TIMER=w_casetMonitor_id.timer

endif else begin

CASE eventval OF
	
	"MONITOR_PV_CANCEL" : BEGIN
		WIDGET_CONTROL, event.top, /DESTROY
		i = caMonitor(w_casetMonitor_narray,/clear)
		END
	"PVVALUE_0" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,0,newvalue(0)
                END
	"PVVALUE_1" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,1,newvalue(0)
                END
	"PVVALUE_2" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,2,newvalue(0)
                END
	"PVVALUE_3" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,3,newvalue(0)
                END
	"PVVALUE_4" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,4,newvalue(0)
                END
	"PVVALUE_5" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,5,newvalue(0)
                END
	"PVVALUE_6" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,6,newvalue(0)
                END
	"PVVALUE_7" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,7,newvalue(0)
                END
	"PVVALUE_8" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,8,newvalue(0)
                END
	"PVVALUE_9" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,9,newvalue(0)
                END
	"PVVALUE_10" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,10,newvalue(0)
                END
	"PVVALUE_11" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,11,newvalue(0)
                END
	"PVVALUE_12" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,12,newvalue(0)
                END
	"PVVALUE_13" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,13,newvalue(0)
                END
	"PVVALUE_14" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,14,newvalue(0)
                END
	"PVVALUE_15" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,15,newvalue(0)
                END
	"PVVALUE_16" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,16,newvalue(0)
                END
	"PVVALUE_17" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,17,newvalue(0)
                END
	"PVVALUE_18" : BEGIN
                WIDGET_CONTROL, event.id, GET_VALUE = newvalue
		w_casetMonitor_put,18,newvalue(0)
                END

ENDCASE
end
END

PRO w_casetMonitor,labels,pvnames,title, GROUP = GROUP, help=help, time=time

COMMON w_casetMonitor_block, w_casetMonitor_ids, w_casetMonitor_narray, w_casetMonitor_varray,w_casetMonitor_id

if XRegistered('w_casetMonitor') ne 0 then return

if keyword_set(help) then begin
	st = [$
	"Usage: w_casetMonitor,label_array,pvname_array,header_title",$
	'',$
	'This function display and reset the  channel access values',$
	'',$
	'   where    label_array      gives a string description for pvname ',$
	'            pvname_array     specifies pvnames ',$
	'            header_title     specifies header for dialog']
	w_warningtext,st
	return
	end

w_casetMonitor_id = { $
	add : -1, $
	timer : 1. $
	}

if keyword_set(time) then w_casetMonitor_id.timer=time
no = n_elements(pvnames)
if no gt 19 then begin
	w_warningtext,'Error: array can not exceed 19!!'
	return
	end
w_casetMonitor_narray = make_array(no,/string,value=string(replicate(32b,30)))
w_casetMonitor_varray = make_array(no,/string,value=string(replicate(32b,40)))

if n_elements(labels) ne no then begin
	w_warningtext,'Error: w_casetMonitor --> labels, pvnames size not same'
	return
	end 
if n_elements(pvnames) gt 0 then begin
	w_casetMonitor_get_array,pvnames,no
	end

if strlen(title) eq 0 then title = 'MONITOR PV ARRAY VALUES'

w_casetMonitor_base=WIDGET_BASE(TITLE = 'w_casetMonitor Widget', /COLUMN)     

row0 = WIDGET_BASE(w_casetMonitor_base, /COLUMN, /FRAME)
scanpvlabel = WIDGET_LABEL(row0, VALUE=title, XSIZE=50)

;row1 = make_array(no,/long,value=0L)
w_casetMonitor_ids = make_array(no+1,/long,value=0L)
w_casetMonitor_ids(no) = w_casetMonitor_base
MON_PVVALUE = make_array(no,/string,value=string(replicate(32b,10)))

for i = 0,no-1 do begin
row1 = WIDGET_BASE(w_casetMonitor_base, /ROW)
temp = labels(i) + ' ( '+ w_casetMonitor_narray(i) + ' )'
MON_PVVALUE(i)='PVVALUE_'+strcompress(string(i),/remove_all)
caset_label = WIDGET_LABEL(row1, VALUE=temp)
w_casetMonitor_ids(i) = WIDGET_TEXT(row1, /EDITABLE, /NO_NEWLINE, $
		 XSIZE=30, YSIZE=1, VALUE=w_casetMonitor_varray(i), $
		 UVALUE=MON_PVVALUE(i))
end


lastrow = WIDGET_BASE(w_casetMonitor_base, /COLUMN)
w_casetMonitor_cancel = WIDGET_BUTTON(lastrow, $
                        VALUE = 'Cancel', $
                        UVALUE = 'MONITOR_PV_CANCEL')

; set widget ids :

; Realize the widgets:
WIDGET_CONTROL, w_casetMonitor_base, /REALIZE
WIDGET_CONTROL, w_casetMonitor_ids(no), TIMER=w_casetMonitor_id.timer

; Hand off to the XMANAGER:
XMANAGER, 'w_casetMonitor', w_casetMonitor_base, GROUP_LEADER = GROUP

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
	
		x1 = MAX(scanData.pa(0:num_pts,w_plotspec_id.xcord))
		x0 = MIN(scanData.pa(0:num_pts,w_plotspec_id.xcord))
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
		xmax = MAX(scanData.pa(0:num_pts,i))
		xmin = MIN(scanData.pa(0:num_pts,i))
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

PRO catch1d_get_pvtcolor,i,color
COMMON COLORS, R_ORIG, G_ORIG, B_ORIG, R_CURR, G_CURR, B_CURR
; 24 bits
	if n_elements(R_ORIG) eq 0 then $
	catch1d_get_pvtct
	color = R_ORIG(i) + G_ORIG(i)*256L + B_ORIG(i)*256L ^2
;	plot,indgen(10),color=color
END

PRO catch1d_load_pvtct,ctfile
	if n_params() eq 0 then restore,'catch1d.tbl' else $
	restore,ctfile
	tvlct,red,green,blue
	xpalette
END

PRO catch1d_save_pvtct
	tvlct,red,green,blue,/get
	save,red,green,blue,file='catch1d.tbl'
END

PRO catch1d_get_pvtct
COMMON COLORS, R_ORIG, G_ORIG, B_ORIG, R_CURR, G_CURR, B_CURR

; 8 bit visual

	if  !d.n_colors lt 16777216 then begin
		tvlct,red,green,blue,/get
	endif else begin

; 24 bit visual
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
	end

; set ORIG color 

	R_ORIG = red
	G_ORIG = green
	B_ORIG = blue

	LOADCT,39
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
;        WIDGET_CONTROL,widget_ids.plot_area , SENSITIVE = 1
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
;	UPDATE_PLOT,1
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


;
; Auto Save File For catch1d_setup.pro
;
;  Wed Apr  3 11:04:10 CST 1996
;


PRO catcher_setup_Event, Event
COMMON CATCH1D_COM, widget_ids, scanData
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'SCAN1D_PVNAME': BEGIN
      WIDGET_CONTROL,catcher_setup_ids.pv,GET_VALUE=pv
	if strtrim(pv(0),2) eq '' then begin
		scanData.pv = ''
		scanData.pvconfig = ''
		return
	end
	len = strpos(pv(0),'.')
	if len eq -1 then newpv = pv(0) else newpv = strmid(pv(0),0,len)
	if caSearch(newpv+'.EXSC') eq 0 then begin
      	WIDGET_CONTROL,catcher_setup_ids.pv,SET_VALUE=newpv
	scanData.pv = newpv
	scanData.pvconfig = newpv
	pventry_event
	endif else begin
		w_warningtext,'Error: invalid SCAN 1D Pvname',40,2
	end
      END
  'SCAN2D_PVNAME': BEGIN
      WIDGET_CONTROL,catcher_setup_ids.y_pv,GET_VALUE=pv
	if strtrim(pv(0),2) eq '' then begin
		scanData.y_pv = ''
		return
	end
	len = strpos(pv(0),'.')
	if len eq -1 then newpv = pv(0) else newpv = strmid(pv(0),0,len)
	if caSearch(newpv+'.EXSC') eq 0 then begin
        WIDGET_CONTROL,catcher_setup_ids.y_pv,SET_VALUE=newpv
	scanData.y_pv = newpv
	pventry2_event
	endif else begin
		w_warningtext,'Error: invalid SCAN 2D Pvname',40,2
	end
      END
  'SCAN1D_START': BEGIN
	catch1d_Start_xScan
	WIDGET_CONTROL,catcher_setup_ids.stop,SENSITIVE=1
      END
  'SCAN1D_STOP': BEGIN
	catch1d_Stop_xScan
      END
  'SCAN2D_START': BEGIN
	catch1d_Start_yScan
;  	WIDGET_CONTROL,catcher_setup_ids.y_handshake_proc,SENSITIVE=1
      END
  'SCAN2D_STOP': BEGIN
	catch1d_Stop_yScan
;  	WIDGET_CONTROL,catcher_setup_ids.y_handshake_proc,SENSITIVE=0
      END
  'SCAN2D_HANDSHAKE': BEGIN
      WIDGET_CONTROL,catcher_setup_ids.y_handshake,GET_VALUE=pv
	if caSearch(pv(0)) eq 0 or pv(0) eq '' then scanData.y_handshake = pv(0)
      END
  'SCAN2D_HANDSHAKE_V': BEGIN
      WIDGET_CONTROL,catcher_setup_ids.y_handshake_v,GET_VALUE=v
	if strlen(v(0)) gt 0 then catcher_setup_scan.y_handshake_v = v(0)
      END
  'SCAN2D_HANDSHAKE_PROC': BEGIN
	ln = caputArray(scanData.y_handshake, catcher_setup_scan.y_handshake_v) 
      END
  'CATCHER_SETUP_CANCEL': BEGIN
      WIDGET_CONTROL,catcher_setup_ids.base,/DESTROY,BAD=bad
      END
  'CATCHER_SETUP_DONE': BEGIN
      WIDGET_CONTROL,catcher_setup_ids.pv,GET_VALUE=pv
	if strtrim(pv(0),2) ne '' then begin
	len = strpos(pv(0),'.')
	if len eq -1 then newpv = pv(0) else newpv = strmid(pv(0),0,len)
	if caSearch(newpv+'.EXSC') eq 0 then begin
	WIDGET_CONTROL,catcher_setup_ids.pv,SET_VALUE=newpv
	scanData.pv = newpv
	scanData.pvconfig = newpv
	pventry_event
	endif else begin
		w_warningtext,'Error: invalid SCAN 1D Pvname',40,2
	end
	endif else begin
		scanData.pv = ''
		scanData.pvconfig = ''
	end

      WIDGET_CONTROL,catcher_setup_ids.y_pv,GET_VALUE=pv1
	if strtrim(pv1(0),2) ne '' then begin
	len = strpos(pv1(0),'.')
	if len eq -1 then newpv = pv1(0) else newpv = strmid(pv1(0),0,len)
	if caSearch(newpv+'.EXSC') eq 0 then begin
	WIDGET_CONTROL,catcher_setup_ids.y_pv,SET_VALUE=newpv
	scanData.y_pv = newpv
	pventry2_event
	endif else begin
		w_warningtext,'Error: invalid SCAN 2D Pvname',40,2
	end
	endif else scanData.y_pv = ''

      WIDGET_CONTROL,catcher_setup_ids.y_handshake,GET_VALUE=pv2
	  if caSearch(pv2(0)) eq 0 then begin
	  scanData.y_handshake = pv2(0)
          WIDGET_CONTROL,catcher_setup_ids.y_handshake_v,GET_VALUE=v
	  if strlen(v(0)) gt 0 then catcher_setup_scan.y_handshake_v = v(0)
	  ln = caputArray(pv2(0),v(0))
	  end

	WIDGET_CONTROL,catcher_setup_ids.base,/DESTROY,BAD=bad
	write_config
      END
  ENDCASE
END



; DO NOT REMOVE THIS COMMENT: END catcher_setup
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.

PRO catcher_setup_init
COMMON CATCH1D_COM, widget_ids, scanData
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

;if n_elements(catcher_setup_scan) eq 0 then begin
;	scanData = {$
;		pv : 'cha:scanRec3SC', $
;		y_pv : 'cha:scanRec4SC', $
;		y_handshake : 'cha:scanRec4SC.PROC' $
;		} 
;	end

if n_elements(catcher_setup_scan) eq 0 then begin
	catcher_setup_scan = { $
		pv : '', $
		y_pv : '', $
		y_handshake : '', $
		y_handshake_v : '' $
		}
	end

;if strlen(scanData.pv) gt 0   then $
	catcher_setup_scan.pv = scanData.pv
;if strlen(scanData.y_pv) gt 0 then $
	catcher_setup_scan.y_pv = scanData.y_pv
;if strlen(scanData.y_handshake) gt 0  then  $
	catcher_setup_scan.y_handshake = scanData.y_handshake
END


PRO catcher_setup, GROUP=Group
COMMON CATCH1D_COM, widget_ids, scanData
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

IF XRegistered('catcher_setup') ne 0 then return

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

  SCAN1D_START = WIDGET_BUTTON( BASE2, $
      UVALUE='SCAN1D_START', $
      VALUE='Start')

  SCAN1D_STOP = WIDGET_BUTTON( BASE2, $
      UVALUE='SCAN1D_STOP', $
      VALUE='Stop')


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

  SCAN2D_START = WIDGET_BUTTON( BASE3, $
      UVALUE='SCAN2D_START', $
      VALUE='Start')

  SCAN2D_STOP = WIDGET_BUTTON( BASE3, $
      UVALUE='SCAN2D_STOP', $
      VALUE='Stop')


  BASE4 = WIDGET_BASE(catcher_setup_base, $
      ROW=1, $
      MAP=1, $
      TITLE='2d_handshake', $
      UVALUE='BASE4')

  SCAN2D_HANDSHAKE = CW_FIELD( BASE4,VALUE=catcher_setup_scan.y_handshake, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='2D Handshake PV:', $
      UVALUE='SCAN2D_HANDSHAKE', $
      XSIZE=30)

  fld='1'
  if strlen(catcher_setup_scan.y_handshake) gt 1 then begin
  ln = cagetArray(catcher_setup_scan.y_handshake,pd,/string)
  if ln eq 0 then fld = pd(0)
  end
  SCAN2D_HANDSHAKE_V = CW_FIELD( BASE4,VALUE=fld, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Value:', $
      UVALUE='SCAN2D_HANDSHAKE_V', $
      XSIZE=10)

;  SCAN2D_HANDSHAKE_PROC = WIDGET_BUTTON( BASE4, $
;      UVALUE='SCAN2D_HANDSHAKE_PROC', $
;      VALUE='Proc')
;  WIDGET_CONTROL,SCAN2D_HANDSHAKE_PROC,SENSITIVE=0

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
	y_pv : SCAN2D_PVNAME, $
	y_handshake: SCAN2D_HANDSHAKE, $
	y_handshake_v: SCAN2D_HANDSHAKE_V, $
;	y_handshake_proc: SCAN2D_HANDSHAKE_PROC, $
	start : SCAN1D_START, $
	stop : SCAN1D_STOP, $
	start2 : SCAN2D_START, $
	stop2 : SCAN2D_STOP $
	}

  WIDGET_CONTROL, catcher_setup_base, /REALIZE

if XRegistered('w_viewscan') ne 0 then $ 
	WIDGET_CONTROL,catcher_setup_ids.base,SENSITIVE=0
if scanData.nosave and scanData.y_scan and strtrim(scanData.y_handshake,2) eq ''then WIDGET_CONTROL,catcher_setup_ids.y_handshake,SENSITIVE=0

  XMANAGER, 'catcher_setup', catcher_setup_base
;  XMANAGER, 'catcher_setup', catcher_setup_base,NO_BLOCK=0
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



PRO readDBDescs,pvnames,descs,print=print,help=help

if keyword_set(help) then begin
	print,'
	print,'USAGE: getDBDescs, pvnames, descs
	print,' INPUT:'
	print,'   pvnames    -  array of PV names'
	print,'   descs      -  input / output description string array ( each'
	print,'                 string is limited to 40 characters'
	print,' OUTPUT:'
	print,'   descs      -  null descs string will be filled with the '
	print,'                 DESC field found from the database'
	print,''
	return
	end

no = n_elements(pvnames)
names = make_array(no,/string,value=string(replicate(32b,30)))

; desc is partially defined such as in catch1d.env
if n_elements(descs) eq 0 then $
	descs=make_array(no,/string)

if n_elements(descs) lt no then begin
	 w_warningtext,['readDBDescs','Error: pvnames and descs array size not same!']
	return
end

for i=0,no-1 do begin
        if descs(i) eq '' then begin
                id = strpos(pvnames(i),'.',0)
                if id ne -1 then begin
                        names(i) = strmid(pvnames(i),0,id) + '.DESC'
                endif else names(i) = pvnames(i) + '.DESC'
;		ln = caget(names(i),pd)
;                descs(i) = pd
                end
;	if keyword_set(print) then print,names(i), i,' ',descs(i)
        end
	ln = cagetArray(names,pd)
	descs = pd
print,descs
END



;
;  readEnvPvnames,'1.out',labels,pvnames
;
PRO readEnvPvnames,filename,labels,pvnames,keys,help=help,no=no
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON env_field_block,env_field

if keyword_set(help) then begin
	st = [$
	"Usage: readEnvPvnames,filename,labels,pvnames,keys",$
	'',$
	'This function read the pvnames and labels from a text file',$
	'',$
	'   Input:   filename         specifies input filename ',$
	'   Output: ',$
	'             pvnames         first string of input line returned as pvname',$
	'             labels          second string of input line returned as label',$
	'             keys            PV envs to be included in short report']
	w_warningtext,st
	return
	end

spawn,[OS_SYSTEM.wc,'-l',filename],y, /noshell
if y(0) eq '' then begin
	w_warningtext,['readEnvPvnames','Error: bad filename for readEnvPvnames' ]
	return 
	end
no = fix(y(0))
if no le 0 then return
pvnames = make_array(no,/string,value=string(replicate(32b,30)))
labels = make_array(no,/string,value=string(replicate(32b,40)))
keys = make_array(no,/int)

; read pvnames from the env file

openr,1,filename
x=''
pv=''
lb=''
i=0
j=0
while (not eof(1) and i lt no) do begin
readf,1,x
x = strtrim(x,2)
len = strlen(x) 
if len gt 1 then begin 

	; check for comment line 
	key_comment = strpos(x,'#',0)
	if key_comment ne 0  then begin 

		pos = strpos(x,' ')
		if pos gt -1 then begin
			pv = strmid(x,0,pos)
			lb = strtrim(strmid(x,pos,len-pos),1)
		endif else begin
			pv = x
			lb=''
			end

		; check for key env

		key = strpos(pv,'*',0)
		if key ne -1 then begin 
			pv = strmid(pv,key+1,strlen(pv)-1)
			keys(j) = i
			j=j+1
			end
		pvnames(i) = pv
		labels(i) = lb
		i=i+1
		end
	end
end
close,1
no = i
if no le 0 then return
pvnames = pvnames(0:i-1)
labels = labels(0:i-1)
if j gt 0 then keys = keys(0:j-1) else keys=0
env_field.numkey = j

; search for pvnames

id = caSearch(pvnames)
if id ne 0 then begin
	ln = caGetError(pvnames,p1)
	st =  'Error: Following PV names from the enviroment file not found !!' 
	for i=0,no-1 do begin
	if p1(i) ne 0 then begin
	st = [st, '       PV name :  "'+pvnames(i)+'"  not found!']
	end
	end
	w_warningtext,st
end
END

;
;  find the key env variable and changed env variable for display
;
PRO find_envs_changed,new_diff
COMMON CATCH1D_COM, widget_ids, scanData
COMMON env_field_block,env_field

n = env_field.noenv

if n gt 0 then begin ;======

diff = make_array(n,/int)
;help,n,diff

; get the key env variables

;	k = n_elements(env_field.keys)
	k = env_field.numkey
	if k gt 0 then begin
	diff(0:k-1) = env_field.keys(0:k-1)
	end
	ij = k

; get the changed values

	for l=0,n-1 do begin
	if strtrim(env_field.values(l)) ne strtrim(env_field.oldvalues(l)) then begin

	if k gt 0 then begin
		find = 0 
		if l le diff(k-1) then begin
		for i=0,k-1 do if l eq diff(i) then find = 1
		end
		if find eq 0 then begin
		diff(ij) = l 
		ij = ij+1
		end
	endif else begin
		diff(ij) = l 
		ij = ij+1
	end

	end ; end compare
	end



if ij gt 0 then begin
	new_diff = make_array(ij,/int)
	new_diff(0:ij-1) = diff(0:ij-1)
endif else new_diff=0

;	print,'ij, NEW_DIFF:',ij, new_diff
end ;====== 
END

;
; ask y/n input with user specified q_text
;
FUNCTION yorn,q_text
print,q_text+' (y/n)? '
st=''
read,st
return,strupcase(st)
END


PRO check_file_seqno,F
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved

maxno=0
	found = findfile(F)
	if found(0) ne '' then begin

	if scanData.XDR eq 1 then openr,unit,F,/GET_LUN,/XDR else $
	openr,unit,F,/GET_LUN
        scan_read_all,unit,maxno
        w_plotspec_id.seqno = maxno
        free_lun,unit
        endif else begin
        w_plotspec_id.seqno = 0
        end
;filenamepath,F,filename,P
if scanData.option gt 0  and scanData.debug eq 1 then begin
	print,'Current Seqno:',maxno
	print,'Append scan data to ', F
end

END

PRO write_config
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids

        CATCH,error_status

; demo mode error -128

        if error_status eq -128 then begin
	w_warningtext,['Error: Demo mode no write on config file allowed !', $
		'       You can exit the IDL now.']
	stop
	exit
	end

; write permission  error -171  

        if error_status eq -171 then begin
	w_warningtext,[!err_string ,'       Permission denied']
        return
        end

openw,unit,scanData.config,/get_lun

printf,unit,"; Generated by ",+scanData.version+scanData.release
printf,unit,"scanData.pv='",scanData.pv,"'"
if strlen(scanData.y_pv) gt 1 then $
	 printf,unit,"scanData.y_pv='",scanData.y_pv,"'"
if strlen(scanData.pvwait) gt 1 then $
	printf,unit,"scanData.pvwait='",scanData.pvwait,"'"
if strlen(scanData.pvbusy) gt 1 then $
	printf,unit,"scanData.pvbusy='",scanData.pvbusy,"'"
if strlen(scanData.y_handshake) gt 1 then $
	printf,unit,"scanData.y_handshake='",scanData.y_handshake,"'"

; add  path 

	st = "scanData.path='"+scanData.path+"'"
        printf,unit,st
	st = "scanData.trashcan='"+w_plotspec_array(3)+"'"
        printf,unit,st
	st = "scanData.envfile='"+ scanData.envfile +"'"
        printf,unit,st
	st = "scanData.config='"+ scanData.config+"'"
        printf,unit,st

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
	'scanData.pvwait' : scanData.pvwait = new_st
	'scanData.pvbusy' : scanData.pvbusy = new_st
	'scanData.y_handshake' : scanData.y_handshake = new_st
	'scanData.path' : scanData.path = new_st
	'scanData.trashcan' : w_plotspec_array(3) = new_st
	'scanData.envfile' : scanData.envfile = new_st
	'scanData.config' : scanData.config = new_st
	'scanData.option' : scanData.option = new_st
	'scanData.nosave' : scanData.nosave = new_st
	'scanData.debug' : scanData.debug = new_st
	ELSE :
	ENDCASE
	end

end
free_lun,unit

END

;
; check the env values 
;
PRO env_after_scan,print=print
COMMON CATCH1D_COM, widget_ids, scanData
COMMON env_field_block,env_field

filename = scanData.envfile 			; 'catch1d.env'
found = findfile(filename)
if found(0) eq '' then return
if scanData.debug eq 1 then print,'***Get values for ',filename

no = env_field.no
if no gt 0 then begin
	env_field.oldvalues = env_field.values 
	pvnames=env_field.pvnames(0:no-1)
	ln = cagetArray(pvnames,pd,/string)
	y = pd
	env_field.values = y
	y = 0
end

END

;
; env_before_scan get the IOC desc based on catch1d.env
;
PRO env_before_scan,file=file
COMMON CATCH1D_COM, widget_ids, scanData
COMMON env_field_block,env_field

; clear old env_field array

	if (env_field.no) gt 0 then begin
	for i=0,env_field.no-1 do begin
		env_field.pvnames(i) = ''
		env_field.descs(i) = ''
		env_field.values(i) = ''
		env_field.oldvalues(i) = ''
		env_field.keys(i) = 0 
		end
	env_field.noenv = 0
	env_field.no = 0
	end

;
; check whether to use the catch1d.env file from the local direcotry or frome
; the start up directory 
;

filename = scanData.envfile 			; 'catch1d.env'

	if n_elements(file) ne 0 then begin
	filename = string(file)
	end

found = findfile(filename)
if found(0) eq '' then begin
;	print, 'Status: file " '+filename+' " not found.' 

endif else begin

if scanData.debug eq 1 then $
print,'***Read PV names from ',filename
readEnvPvnames,filename,no=no,values,pvnames,keys

env_field.no = no 
if no gt 0 then begin
env_field.pvnames = pvnames
env_field.descs = values
env_field.keys = keys

; get the description array if not defined

if scanData.option eq 0 then return
	temp = make_array(no,/string,value=string(replicate(32b,30)))
        for i=0,no-1 do begin
	res = strpos(pvnames(i),'.')
	if res eq -1 then temp(i) = pvnames(i) + '.DESC' else $
		temp(i) = strmid(pvnames(i),0,res)+'.DESC'
	end

	ln = caSearch(temp)
	ln = cagetArray(temp,pd,/string)	
	for i=0,no-1 do begin
	env_field.descs(i) = values(i)
	if values(i) eq '' then env_field.descs(i) = pd(0,i)
	end

; get the value array

	ln = cagetArray(pvnames,pd,/string)
	if ln eq 0 then begin
	env_field.oldvalues = pd
	env_field.values = pd 
	endif else begin
;	w_warningtext,'Error: env_before_scan failed on cagetArray'
	end
end
end
	addScanFieldToEnvList

END

PRO addScanFieldToEnvList
COMMON CATCH1D_COM, widget_ids, scanData
COMMON env_field_block,env_field
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids

; add the scan record field to the env list

	scan_field_get,scanData.pv
	no = field_max
	n1 = env_field.no
	env_field.noenv = n1
	env_field.pvnames(n1:n1+no-1) = field_name_array
	env_field.descs(n1:n1+no-1) = field_label_array
	env_field.values(n1:n1+no-1) = field_value
	env_field.oldvalues(n1:n1+no-1) = field_value
	env_field.exist = 1
	env_field.no = env_field.no + no

;print,'field_max',field_max
;print,'env_field.noenv',env_field.noenv
;print,'env_field.no',env_field.no
;print,'env_field.numkey',env_field.numkey

END


FUNCTION check_data_version,filename,nowid=nowid
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH1D_COM, widget_ids, scanData
;
; check for save data version consistancy for XDR or native binary
;  -99 read err
;   -1 wrong type data
;    1 new file

        F = strcompress(filename,/remove_all)
        found=findfile(F)

if found(0) ne '' then begin

	; check for old data type 

    if !d.name eq 'X' then begin

	CATCH,error_status
	if error_status ne 0 then begin
	return,-1
	end

	scanData.XDR  = 0
	U_OPENR,unit,F
	s = FSTAT(unit)

	IF s.size EQ 0 THEN BEGIN
		scanData.XDR = 1 
		errcode = 1
		u_close,unit
	ENDIF ELSE BEGIN ;  size > 0
	 u_read,unit,version,errcode
	 if errcode ne 0 then begin  
		; wrong type 
	 endif else begin
	  if n_elements(version) eq 1 then begin
		if strpos(version(0),scanData.version) eq -1 then begin
			U_OPENR,unit1,F,/XDR
			u_read,unit1,vers,errc
			if errc eq 0 and n_elements(vers) eq 1 then begin
			  if strpos(vers(0),scanData.version) ge 0 then begin
				scanData.XDR = 1
				errcode = 0
			  endif else errcode = -1
			end 
			u_close,unit1
	 	end
	  endif else errcode = -1
	 end
	u_close,unit
	END
    endif else begin
	scanData.XDR = 1
		U_OPENR,unit,F,/XDR
		s = fstat(unit)
		if s.size eq 0 then errcode = 1 else $
		u_read,unit,vers,errcode
		u_close,unit
    end

endif else begin ; file not found
	errcode = 1   
	scanData.XDR = 1
	scanData.scanno = 1
end

;print,errcode,filename,scanData.XDR, scanData.scanno
	if keyword_set(nowid) then return,errcode
	WIDGET_CONTROL,widget_ids.binary_type,set_droplist_select=scanData.XDR
	return,errcode


END

;
; update the internal file name to newname  
;
PRO update_internal_filename,file1,file2
COMMON SYSTEM_BLOCK,OS_SYSTEM

; check for xdr type
        pos = strpos(strupcase(file1),'.XDR')
        pos2 = strpos(strupcase(file2),'.XDR')
	if scanData.XDR then begin
	if (strlen(file1)-pos) ne 4 or (strlen(file2)-pos2) ne 4 then begin
                w_warningtext,['Error: Wrong type of file entered!!']
                return
                end
        endif else begin
		if (strlen(file1)-pos) eq 4 or (strlen(file2)-pos2) eq 4 then begin
                w_warningtext,['Error: Wrong type of file entered!!']
                return
		end
        end

	if scanData.XDR then begin
	openw,unit2,file2,/GET_LUN,/XDR
	openr,unit1,file1,/GET_LUN,/XDR
	endif else begin
	openw,unit2,file2,/GET_LUN
	openr,unit1,file1,/GET_LUN
	end
	while not EOF(unit1) do begin
        u_read,unit1,version
        u_read,unit1,pv
        u_read,unit1,num_pts
        u_read,unit1,FA
        u_read,unit1,x
        u_read,unit1,y
        u_read,unit1,z
        u_read,unit1,n
	if n(0) gt 0 then u_read,unit1,ze

; update the file name used in w_plotspec_array
	x(3) = file1
	len = strlen(file1)
	if len lt 60 then x(3)=  x(3) + string(replicate(32b,60-len))

        u_write,unit2,version(0)
	u_write,unit2,pv(0)
	u_write,unit2,num_pts(0)
	u_write,unit2,FA
	u_write,unit2,x
	u_write,unit2,y
	u_write,unit2,z
        u_write,unit2,n
	if n(0) gt 0 then u_write,unit2,ze
	end
	free_lun,unit1
	free_lun,unit2

;
;  move file1 to file1.old,   file2 to file1
;
	if OS_SYSTEM.os_family eq 'unix' then begin 
	spawn,[OS_SYSTEM.mv, file1, file1+'.bk'],/noshell
	spawn,[OS_SYSTEM.chmod,'444',file1+'.bk'],/noshell
	spawn,[OS_SYSTEM.mv, file2, file1],/noshell
	endif else begin
	spawn,[OS_SYSTEM.mv, file1, file1+'.bk']
	spawn,[OS_SYSTEM.chmod,'444',file1+'.bk']
	spawn,[OS_SYSTEM.mv, file2, file1]
	end
END




;
; copy the internal file name to newname  
; automatic resequence the no
;
PRO copy_internal_filename,file1,file2
COMMON CATCH1D_COM, widget_ids, scanData

re_seqno = 0
	if scanData.XDR then begin
	u_openw,unit2,file2,/XDR
	u_openr,unit1,file1,/XDR
	endif else begin
	u_openw,unit2,file2
	u_openr,unit1,file1
	end
	while not EOF(unit1) do begin
        u_read,unit1,version
        u_read,unit1,pv
        u_read,unit1,num_pts
        u_read,unit1, realtime_id_def
	u_read,unit1, x_dpt
	for i=0,3 do begin
		if realtime_id_def(i) gt 0 then begin
		u_read,unit1,px
		scanData.pa(0:num_pts(0),i) = px
		end
	end
	for i=4,18 do begin
		if realtime_id_def(i) gt 0 then begin
		u_read,unit1,px
		scanData.da(0:num_pts(0),i-4) = px
		end
	end
        u_read,unit1,labels
        u_read,unit1,x
        u_read,unit1,y
        u_read,unit1,n
	if n(0) gt 0 then u_read,unit1,ze

; update the file name used in w_plotspec_array
	x(3) = file2
	len = strlen(file2)
	if len lt 60 then x(3)=  x(3) + string(replicate(32b,60-len))

; update the seqno this will fix internal seqno problem  
	y(0) = re_seqno

        u_write,unit2,version(0)
	u_write,unit2,pv(0)
	u_write,unit2,num_pts(0)
        u_write,unit2, realtime_id_def
	u_write,unit2, x_dpt
	for i=0,3 do begin
		if realtime_id_def(i) gt 0 then begin
		px = scanData.pa(0:num_pts(0),i)
		u_write,unit2,px
		end
	end
	for i=4,18 do begin
		if realtime_id_def(i) gt 0 then begin
		px = scanData.da(0:num_pts(0),i-4)
		u_write,unit2,px
		end
	end
        u_write,unit2,labels
	u_write,unit2,x
	u_write,unit2,y
        u_write,unit2,n
	if n(0) gt 0 then u_write,unit2,ze
re_seqno = re_seqno + 1
	end
	close,unit2
	free_lun,unit1
	free_lun,unit2

END

PRO w_catch1dCopy_event,event
COMMON w_catch1dCopy,w_catch1dCopy_ids

WIDGET_CONTROL, event.id, GET_UVALUE = eventval

CASE eventval OF
	"RENAME_OLDFILE" : BEGIN
		END
	"RENAME_NEWFILE" : BEGIN
		END
	"RENAME_OK" : BEGIN
		WIDGET_CONTROL,w_catch1dCopy_ids.oldfile,GET_VALUE=oldfile
		WIDGET_CONTROL,w_catch1dCopy_ids.newfile,GET_VALUE=newfile
		if strtrim(newfile(0),2) eq '' then return
		if oldfile(0) eq newfile(0) then return
		copy_internal_filename,strtrim(oldfile(0),2),strtrim(newfile(0),2)
		WIDGET_CONTROL,w_catch1dCopy_ids.base,/DESTROY
		END
	"RENAME_CANCEL" : BEGIN
		WIDGET_CONTROL,w_catch1dCopy_ids.base,/DESTROY
		END
ENDCASE

END


PRO w_catch1dCopy, GROUP=GROUP
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_catch1dCopy,w_catch1dCopy_ids

if XRegistered('w_catch1dCopy') ne 0 then return

;
; destroy the old w_plotspec window
;
	close_plotspec

base = WIDGET_BASE(GROUP_LEADER=Group, $
;base = WIDGET_BASE( $
        TITLE = 'CATCH1D File Copy ', /COLUMN)

row1 = WIDGET_BASE(base, /ROW,/FRAME)
lab1 = WIDGET_LABEL(row1,VALUE='Source File:      ')
text1 = WIDGET_TEXT(row1,VALUE=strtrim(w_plotspec_array(3),2), $
                EDITABLE=1, $
                UVALUE='RENAME_OLDFILE', XSIZE = 60)

row2 = WIDGET_BASE(base, /ROW,/FRAME)
lab2 = WIDGET_LABEL(row2,VALUE='Destination File: ')
text2 = WIDGET_TEXT(row2,VALUE='', $
                EDITABLE=1, $
                UVALUE='RENAME_NEWFILE', XSIZE = 60)

row3 = WIDGET_BASE(base, /ROW,/FRAME)
cancel = WIDGET_BUTTON(row3, VALUE = ' Cancel ', UVALUE = 'RENAME_CANCEL')
ok = WIDGET_BUTTON(row3, VALUE = ' OK ', UVALUE = 'RENAME_OK')

w_catch1dCopy_ids = { $
	base	: base, $
	oldfile : text1, $
	newfile : text2 $
	}

WIDGET_CONTROL, base, /REALIZE

XMANAGER, 'w_catch1dCopy', base, GROUP_LEADER = GROUP

END


PRO w_catch1dNewfile
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_catch1dCreate,w_catch1dCreate_ids

		WIDGET_CONTROL,w_catch1dCreate_ids.newfile,GET_VALUE=newfile
		if strtrim(newfile(0),2) eq '' then return
		F =  strcompress(newfile(0),/REMOVE_ALL)

        	if STRMID(F,0,1) eq '~' then filename_expand,F 

		found = findfile(F)
		if found(0) ne '' then begin
			st=['Error: file already exist!','       Reenter please.']
			w_warningtext,st
			return
			end
		w_plotspec_array(3) = F
		w_plotspec_id.seqno = 0
		WIDGET_CONTROL,w_catch1dCreate_ids.base,/DESTROY
END


PRO w_catch1dCreate_event,event
COMMON w_catch1dCreate,w_catch1dCreate_ids

WIDGET_CONTROL, event.id, GET_UVALUE = eventval

CASE eventval OF
        "CREATE_NEWFILE" : BEGIN
		w_catch1dNewfile
                END

	"CREATE_OK" : BEGIN
		w_catch1dNewfile
		END

	"CREATE_CANCEL" : BEGIN
		WIDGET_CONTROL,w_catch1dCreate_ids.base,/DESTROY
		END
ENDCASE

END


PRO w_catch1dCreate, GROUP=GROUP
COMMON w_catch1dCreate,w_catch1dCreate_ids

if XRegistered('w_catch1dCreate') ne 0 then return
;
; destroy the old w_plotspec window
;
	close_plotspec

base = WIDGET_BASE(GROUP_LEADER=Group, $
;base = WIDGET_BASE( $
        TITLE = 'CATCH1D Create New File ', /COLUMN)

row2 = WIDGET_BASE(base, /ROW,/FRAME)
lab2 = WIDGET_LABEL(row2,VALUE='Destination File: ')
text2 = WIDGET_TEXT(row2,VALUE='', $
                EDITABLE=1, $
                UVALUE='CREATE_NEWFILE', XSIZE = 60)

row3 = WIDGET_BASE(base, /ROW, /FRAME)
cancel = WIDGET_BUTTON(row3, VALUE = '    Cancel    ', UVALUE = 'CREATE_CANCEL')
    ok = WIDGET_BUTTON(row3, VALUE = '      OK      ', UVALUE = 'CREATE_OK')

w_catch1dCreate_ids = { $
	base	: base, $
	newfile : text2 $
	}

WIDGET_CONTROL, base, /REALIZE

XMANAGER, 'w_catch1dCreate', base, GROUP_LEADER = GROUP

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


;
; extract the filename & filepath from the input filename,
;     return P as file path
;
PRO filenamepath,filename,F,P
COMMON CATCH1D_COM, widget_ids, scanData
COMMON SYSTEM_BLOCK,OS_SYSTEM

if n_elements(filename) eq 0 then return
	len = strlen(filename)
	F=filename
	P=scanData.home
	if strpos(filename,OS_SYSTEM.file_sep) eq -1 then return 

	x=byte(filename)
	P=''
	for i=0,len-1 do begin
	is = len-1 -i
	if string(x(is)) eq OS_SYSTEM.file_sep then begin
		P = strmid(filename,0,is+1)
		F = strmid(filename,is+1,len-is)
		return
		end
	end
END

PRO before_sys_scan
COMMON CATCH1D_COM, widget_ids, scanData
	if scanData.option gt 0 then begin
	env_before_scan
	before_scan
	end
END

PRO before_scan
; w_warningtext,'No before_scan routine provided by user.'
END

PRO after_scan
; w_warningtext,'No after_scan routine provided by user.'
END
PRO salvage_read,unit,maxno
        seqno = 0
        id = 0
        WHILE NOT  EOF(unit) DO BEGIN
        id = id + 1
                salvage_read_record,unit
		print,'debug: finished reading seqno=',id
        maxno = id
        END
END

PRO salvage_read_record,unit
        u_read,unit,version
        u_read,unit,pv
        u_read,unit,num_pts
        u_read,unit,realtime_id_def

	u_read,unit,x_dpt   	; detector multi-channel array size
	for i=0,18 do begin
		if realtime_id_def(i) gt 0 then u_read,unit,px
	end

        u_read,unit,labels
        u_read,unit,x
        u_read,unit,y
        u_read,unit,n
        if n(0) gt 0 then begin
        u_read,unit,ze
        end
END


;
; throw away bad data   and salvage the good data up to 'seqno'
;
PRO salvage_trashcan_problem,file1,seqno,XDR=XDR
COMMON SYSTEM_BLOCK,OS_SYSTEM

if n_params() ne 2 then begin
	print,'Usage: salvage_trashcan_problem,filename,seqno,/XDR'
	return
end
file2 = file1+'.tmp'
	if keyword_set(xdr) then begin 
		openr,unit1,file1,/GET_LUN,/XDR
	endif else begin
	openr,unit1,file1,/GET_LUN
	u_read,unit1,version,errcode	
	  if errcode eq 0 then begin
		if version(0) eq '' then begin
		u_close,unit1
		openr,unit1,file1,/GET_LUN,/XDR
		end
	  endif else begin
		print,'Wrong type of data entered: ',file1
		return
	  end
	end
	if keyword_set(XDR) then openw,unit2,file2,/GET_LUN,/XDR else $
	openw,unit2,file2,/GET_LUN
id = 0
	u_rewind,unit1
	while id lt seqno and not EOF(unit1) do begin
        u_read,unit1,version
        u_read,unit1,pv
        u_read,unit1,num_pts
        u_read,unit1,realtime_id_def
        u_read,unit1,x_dpt       ; detector multi-channel array size

	num_po = 0
        for i=0,3 do begin
                if realtime_id_def(i) gt 0 then num_po = num_po+1
        end

; get positioners array
	if num_po gt 0 then begin
	FA = make_array(num_pts(0)+1,num_po,/double)
        for i=0,num_po-1  do begin
                u_read,unit1,px
		FA(*,i)=px
        end
	end

; get detectors array
	num_po = 0
        for i=4,18  do begin
                if realtime_id_def(i) gt 0 then num_po = num_po+1
        end
	FB = make_array(num_pts(0)+1,num_po)
        for i=0,num_po-1  do begin
                u_read,unit1,px
		FB(*,i)=px 
        end

        u_read,unit1,labels
        u_read,unit1,x
        u_read,unit1,y
        u_read,unit1,n
	if n(0) gt 0 then u_read,unit1,ze

; duplicate the data set in the file 

        u_write,unit2,version(0)
	u_write,unit2,pv(0)
	u_write,unit2,num_pts(0)
        u_write,unit2,realtime_id_def
        u_write,unit2,x_dpt       ; detector multi-channel array size

	ip=0
        for i=0,3 do begin
                if realtime_id_def(i) gt 0 then begin
			px=FA(0:num_pts(0),ip)
			u_write,unit2,px
			ip=ip+1
		end	
        end

	ip=0
        for i=4,18 do begin
                if realtime_id_def(i) gt 0 then begin
			px=FB(0:num_pts(0),ip)
			u_write,unit2,px
			ip=ip+1
		end	
        end
        
        u_write,unit2,labels
	u_write,unit2,x
	u_write,unit2,y
	u_write,unit2,z
        u_write,unit2,n
	if n(0) gt 0 then u_write,unit2,ze
id = id+1
	end
	free_lun,unit1
	free_lun,unit2

;
;  move file1 to file1.old,   file2 to file1
;
	if OS_SYSTEM.os_family eq 'unix' then begin
	spawn,[OS_SYSTEM.mv, file1, file1+'.bk'],/noshell
	spawn,[OS_SYSTEM.mv, file2, file1],/noshell
	endif else begin
	spawn,[OS_SYSTEM.mv, file1, file1+'.bk']
	spawn,[OS_SYSTEM.mv, file2, file1]
	end
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
; this routine does an auto-scaled plot of the selected waveforms
;
PRO UPDATE_PLOT, auto, st

   COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON font_block, text_font, graf_font, ps_font
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON w_statistic_block,w_statistic_ids

if !d.name eq 'WIN' then device,decomposed=1

   WIDGET_CONTROL, widget_ids.wf_select, GET_VALUE = wf_sel
   if total(wf_sel) eq 0 then return
   x_axis = w_plotspec_id.x_axis_u

win_state = WIDGET_INFO(widget_ids.plot_wid, /GEOMETRY)

;   plotSubTitle = strtrim(w_plotspec_array(4))
   plotSubTitle = ''
   plotTitle=''
   plotYTitle=''
   plotXTitle =''


   num_pts = 1 > (scanData.act_npts-1)


   ;extract valid data from global arrays

   ; If plotting before p1 was read ...
   IF num_pts lt 1 then begin
	w_warningtext,'Error: You have to open input file first !'
;	return
   END

   catch1d_check_xaxis,num_pts,p1,xmin,xmax

; check any data available 
   if scanData.lastPlot lt 0 then begin 
	if xmax eq xmin then return 
	auto = 1
	end
 
if w_plotspec_id.log eq 0 and auto ne 0 then auto=1   
if w_plotspec_id.log eq 2 and auto ne 0 then auto=2   ;  Y > 0

;   setPlotLabels                

   scanData.lastPlot = auto
   y_zero = 0
   if auto eq 2 then y_zero = 1.e-7    ; exclude zero for auto scale

   IF (auto gt 0) THEN BEGIN     ;  auto scale
!y.style = 1
!x.style = 1

     ;if autoscale, determine appropriate Y values
     ;depending on which waveforms will be plotted
     pos_ymin = 1.e20
     ymin = 1.e20
     ymax = -1.e20
     err_dy = 0


for i=0,14 do begin

     IF (wf_sel(i) EQ 1) THEN  BEGIN

	d1 = scanData.da(0:num_pts,i)

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
for i=15,18 do begin
     IF (wf_sel(i) EQ 1) THEN  BEGIN
	d1 = scanData.pa(0:num_pts,i-15)
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
       xmin = 0
	xmax=num_pts
	if auto eq 1 then view1d_adjust_ranges,xmin,xmax
       plotXTitle = 'Step #'        
     ENDELSE

     if total(wf_sel) eq 0 then  plotXTitle = 'Nothing Selected' 

     if n_elements(w_plotspec_array) ne 0 then begin 
	if strlen(strtrim(w_plotspec_array(0))) gt 1 then $
	plotTitle = strtrim( w_plotspec_array(0),2) 
	if scanData.y_scan gt 0 then $
	plotTitle = plotTitle+ '='+ strtrim(scanData.y_value,2)

	if strlen(w_plotspec_array(2)) gt 1 then $
	plotYTitle = strtrim(w_plotspec_array(2))
	end

   ;Now draw the axis and plot the selected waveforms


if !d.name ne 'PS' then WSET, widget_ids.plot_area
;   ERASE

   ;fake out PLOT to plot an empty axis
   junk = ['5','6']

   ;Plot the axis w/o any waveforms

	POS=[0.15,0.2,0.78,0.85] 
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
;	FONT = graf_font, $
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
;	FONT = graf_font, $
         MAX_VALUE = 0, junk
end

y_descs = strtrim(y_descs,2)

st='Scan #: ' + strtrim(scanData.scanno)

is = 0
for i=0,14 do begin
   IF (wf_sel(i) EQ 1 and realtime_id.def(4+i) gt 0) THEN begin
	d1 = scanData.da(0:num_pts,i)
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
        end
end

for i=15,18 do begin
   IF (wf_sel(i) EQ 1 and realtime_id.def(i-15) gt 0) THEN begin
        d1 = scanData.pa(0:num_pts,i-15)
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

	if scanData.y_scan gt 0 then begin
	xdis = 0.45 * !d.x_size
	ydis = !d.y_size - 1.2 * !d.y_ch_size

	header_note = '2D SCAN # : '+string(scanData.scanno_2d)
	xyouts,xdis,ydis,header_note,/device
	end

	header_note =  '1D SCAN # : ' + string(scanData.scanno) 
	xdis = 0.70 * !d.x_size
	ydis = !d.y_size - 1.2 * !d.y_ch_size
	xyouts,xdis,ydis,header_note,/device


	footer_note = strmid(strtrim(w_plotspec_array(4)),0,29)
	xdis = 0.01 * !d.x_size
	ydis = 1.2*!d.y_ch_size
	xyouts,xdis,ydis,footer_note,/device


	len = strlen( strtrim(w_plotspec_array(4)))
	footer_note = strmid(strtrim(w_plotspec_array(4)),30,len-30)
	xdis = 0.7 * !d.x_size
	ydis = 1.2*!d.y_ch_size
	xyouts,xdis,ydis,footer_note,/device


footer_note= 'comment: ' + strtrim(w_plotspec_array(5))
	view1d_ydist,(.01-pos(1)),ydis	
	xdis = 0.01 * !d.x_size
	ydis = 0.1*!d.y_ch_size
	xyouts,xdis,ydis,footer_note,/device

END


PRO catch1d_check_xaxis,num_pts,p1,xmin,xmax
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

; select the x-axis for plot

	i = w_plotspec_id.xcord
	if w_plotspec_id.xcord lt 4 then $
	   p1 = scanData.pa(0:num_pts,i) else $
	   p1 = scanData.da(0:num_pts,i-4)
	   xmin = MIN(p1)
	   xmax = MAX(p1)
	   if xmin eq xmax and w_plotspec_id.x_axis_u eq 0 then begin
		str=['Warning: Constant value found for x-axis P'+ $
			strtrim(i+1,2)+ ' vector', $
		     '         If you desired, you may try Step # for Xaxis.']
		w_warningtext,str
	   end
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
        if dx le 1.e-15 then dx = 1.
        xmax = xmax + dx
        xmin = xmin - dx

	view1d_round_xrange,xmin,xmax

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

	color = w_plotspec_id.colorI(id)
	; 24 bit visual case
	if !d.n_colors eq 16777216 then begin
		catch1d_get_pvtcolor,color,t_color
		color = t_color
		end
	if !d.name eq 'PS' then color = 0

	line = id1
	if w_plotspec_id.solid eq 1 and !d.name ne 'PS' then line = 0

if w_plotspec_id.type eq 0 then begin
      IF (x_axis EQ 0) THEN OPLOT, p1, d1, color=color, LINE = line , THICK=2 $
      ELSE OPLOT, d1, color=color, LINE = line, THICK=2
	; write legend 
	if w_plotspec_id.log ne 1 then $ 
	oplot,[xdis,xdis2],[lydis,lydis],color=color,LINE=line,/noclip else $
	oplot,[xdis,xdis2],[10^lydis,10^lydis],color=color,LINE=line,/noclip
	xdis = 0.8*!d.x_size
	xdis2 = 0.85*!d.x_size
	ydis = pos(3) * !d.y_size - 5 *id1*!d.y_ch_size
	if id lt 15 then begin
	   if strlen(y_descs(id)) gt 1 then $
		xyouts,xdis2,ydis,'  '+y_descs(id), /device else $
		xyouts,xdis2,ydis,'  Detector '+strtrim(id+1,1), /device
	endif else begin
	   idd = id-15
	   if strlen(x_descs(idd)) gt 1 then $
		xyouts,xdis2,ydis,'  '+x_descs(idd), /device else $
		xyouts,xdis2,ydis,'  Encoder P'+strtrim(idd+1,1), /device
	end
endif else begin
	sym = id1+1
	if w_plotspec_id.type eq 2 then sym = -(id1+1)
		IF (x_axis EQ 0) THEN OPLOT, p1, d1,color=color, PSYM = sym else $
		OPLOT, d1,color=color, PSYM = sym

	if w_plotspec_id.log ne 1 then $ 
		oplot,[xdis,xdis],[lydis,lydis],color=color,PSYM=sym,/noclip else $
		oplot,[xdis,xdis],[10^lydis,10^lydis],color=color,PSYM=sym,/noclip
	; write legend
	xdis = 0.8*!d.x_size
	xdis2 = 0.85*!d.x_size
	ydis = pos(3) * !d.y_size - 5 *id1*!d.y_ch_size
	if id lt 15 then begin
	   if strlen(y_descs(id)) gt 1 then  $
		xyouts,xdis2,ydis,'  '+y_descs(id),/device  else $
		xyouts,xdis2,ydis,'  Detector '+strtrim(id+1,1),/device
	endif else begin
	   idd = id-15
	   if strlen(x_descs(idd)) gt 1 then $
		xyouts,xdis2,ydis,'  '+x_descs(idd), /device else $
		xyouts,xdis2,ydis,'  Encoder P'+strtrim(idd+1,1), /device
	end
end

if w_plotspec_id.errbars eq 1 then begin 
	d_err = sqrt(abs(d1))
	for i=0, num_pts do begin
	x2 = p1(i)
	ny1 = d1(i) - d_err(i)
	ny2 = d1(i) + d_err(i)
      IF (x_axis EQ 0) THEN $
	OPLOT,color=color, [x2,x2],[ny1,ny2] else OPLOT,color=color,[i,i], [ny1,ny2] 
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

w_statistic_base=WIDGET_BASE(TITLE = 'Catch1d '+ title, $
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



;
; catch1d_realtime.pro
;
PRO  setScanPvnames,file=file,help=help
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

if keyword_set(help) then begin
	st = ["Usage: setScanPvnames,/file", $
	'       This function defines the pvnames monitored by the scan record',$
	'       If file keyword not used, then 4 positioners and 15 detectors assumed', $
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
	scanData.nonames = 19 
	realtime_pvnames = make_array(19,/string,value=string(replicate(32b,5)))
	realtime_pvnames(0)=scanData.pv+'.R1CV'
	realtime_pvnames(1)=scanData.pv+'.R2CV'
	realtime_pvnames(2)=scanData.pv+'.R3CV'
	realtime_pvnames(3)=scanData.pv+'.R4CV'
	realtime_pvnames(4)=scanData.pv+'.D1CV'
	realtime_pvnames(5)=scanData.pv+'.D2CV'
	realtime_pvnames(6)=scanData.pv+'.D3CV'
	realtime_pvnames(7)=scanData.pv+'.D4CV'
	realtime_pvnames(8)=scanData.pv+'.D5CV'
	realtime_pvnames(9)=scanData.pv+'.D6CV'
	realtime_pvnames(10)=scanData.pv+'.D7CV'
	realtime_pvnames(11)=scanData.pv+'.D8CV'
	realtime_pvnames(12)=scanData.pv+'.D9CV'
	realtime_pvnames(13)=scanData.pv+'.DACV'
	realtime_pvnames(14)=scanData.pv+'.DBCV'
	realtime_pvnames(15)=scanData.pv+'.DCCV'
	realtime_pvnames(16)=scanData.pv+'.DDCV'
	realtime_pvnames(17)=scanData.pv+'.DECV'
	realtime_pvnames(18)=scanData.pv+'.DFCV'
	end

;print,'pvnames',realtime_pvnames
END


PRO terminal_dump_header
COMMON CATCH1D_COM, widget_ids, scanData
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON env_field_block,env_field
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
	st = "; SCAN #: "+ string(w_plotspec_id.seqno+1)
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id
	st = "; SCAN Record Name: "+scanData.pv
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id

no = env_field.numkey
if no gt 0 then begin
	st =';'
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id
	st = '; KEY PV names got from the catch1d.env'
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id
	st =';'
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id
twd = 18*total(realtime_id.def) + 10
s0 = string(replicate(32b,twd))
st = s0
strput,st,'; ',0
strput,st,'PVNAME',2
strput,st,'VALUE',30
strput,st,'DESCRIPTION',60
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id

noenv = env_field.noenv
if noenv ge no then begin

	for i=0,no-1 do begin
	st = s0
	strput,st,'; ',0
	strput,st,env_field.pvnames(i),2
	strput,st,env_field.values(i),30
	strput,st,env_field.descs(i),60
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id
	end

endif else begin

	for i=0,no-1 do begin
	st = s0
	strput,st,'; ',0
	strput,st,env_field.pvnames(env_field.keys(i)),2
	strput,st,env_field.values(env_field.keys(i)),30
	strput,st,env_field.descs(env_field.keys(i)),60
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id
	end
end
end

	st =';'
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id

; find name, desc, engu for defined PiPV & DiPV 

	find_desc_engu,x_dn,descs,engus
	no = n_elements(x_dn)
        st = ';    I   '
        for i=0,no-1 do begin
        if realtime_id.def(i) ne 0 then begin
                st = st+ ' '+x_dn(i)
                end
        end

;	print,st

	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id

;	s0 = string(replicate(32b,340))
twd = strlen(st) > 18*total(realtime_id.def) + 10
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
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus


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

;	ln = caget(scanData.pv+'.NPTS',pd)
;	ln = caget(scanData.pv+'.MPTS',mpts)
	ln = cagetArray([scanData.pv+'.NPTS', scanData.pv+'.MPTS'],pd,/short)
	mpts = pd(1) 
	scanData.req_npts = pd(0)
	realtime_retval = make_array(scanData.req_npts,scanData.nonames,/double)
	realtime_id.mpts = mpts

;if scanData.y_seqno eq 0 then begin
if scanData.realtime eq 0 then begin
	ln = caScan(scanData.pv+'.CPT',realtime_pvnames,/clear)
	ln = caScan(scanData.pv+'.CPT',realtime_pvnames,/add,max=mpts)
	ln = caScan(scanData.pv+'.CPT',realtime_pvnames,scanData.nonames,npts,pd,/get,max=mpts)
	realtime_retval = pd
	scanData.realtime = 1

if scanData.debug eq 1 then $
print,'REALTIME_INIT: add caScan at # ',w_plotspec_id.seqno

scanData.p_def = realtime_id.def(0:3)
scanData.px = make_array(4000,/float)
scanData.pa = make_array(4000,4,/float)
scanData.da = make_array(4000,15,/float)
end
	ln = caScan(scanData.pv+'.CPT',realtime_pvnames,/zero,max=mpts)
	scanData.act_npts = 0
 
;  check for terminal dump

	terminal_dump_header


WSET, widget_ids.plot_area

;    ind = 0 plot the x axis and get monitor queue

if scanData.y_scan then begin
	in = cagetArray(scanData.y_pv+'.P1DV',pd)
	scanData.y_value=pd(0)
end

if realtime_id.ind eq 0 then begin 
	tempTitle=strtrim(w_plotspec_array(0))+' (1D SCAN # '+strtrim(w_plotspec_id.seqno+1,2) +')'

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

; set the new goto_pv for new realtime

        for i=0,3 do begin
        if strtrim(x_names(i),2) ne '' then $
        w_plotspec_id.goto_pv(i) = strmid(x_names(i),0,strpos(x_names(i),'.'))
        end

x_dv = 0
x_dn = 0
realtime_retval = 0
;	print,caclock()
END

;
;
PRO realtime_read,npts
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

   WIDGET_CONTROL, widget_ids.wf_select, GET_VALUE = wf_sel

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
ln_style = intarr(19)
for i=0,18 do begin
	ln_style(i) = i mod 6
	if w_plotspec_id.solid eq 1 then ln_style(i) = 0
end

retval = make_array(scanData.nonames,scanData.req_npts+1);
nonames= scanData.nonames
pts = scanData.req_npts+1
ln = caScan(scanData.pv+'.CPT',realtime_pvnames,nonames,pts,pd,/get,max=realtime_id.mpts)
cpts = pts
if cpts le 1 then return
retval = pd

if cpts le scanData.act_npts  then return

n1 = realtime_id.no
n2 = cpts - 1  
scanData.act_npts = cpts 
if scanData.act_npts eq 0 then return

realtime_retval = transpose(retval)
	for i=0,scanData.num_pos-1 do begin
	is = i*cpts
	scanData.pa(n1:n2,i)	= realtime_retval(n1:n2,i)
	if w_plotspec_id.xcord eq i then  $
		scanData.px(n1:n2) = realtime_retval(n1:n2,i)
	end

	for i=0,scanData.num_det-1 do begin
	is = i*cpts
	scanData.da(n1:n2,i) = realtime_retval(n1:n2,i+scanData.num_pos)
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
		strput,st,scanData.pa(i,j),ij  & ij = ij + 13 &end

		end
	for j=0,scanData.num_det - 1 do begin
	if realtime_id.def(4+j) ne 0 then begin 
		strput,st,scanData.da(i,j),ij  & ij = ij + 13 &end
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
	if scanData.px(n2) gt realtime_id.xmax then begin
	dxx = 0.1*(scanData.px(n2)-scanData.px(0))
	xmin = scanData.px(0) - dxx
	xmax = scanData.px(n2) + dxx
	if xmax gt realtime_id.xmax then realtime_id.xmax = xmax
	if xmin lt realtime_id.xmin then realtime_id.xmin = xmin
	realtime_id.axis = 1
	end
end

; if data point as x axis is requested

xtitle = strtrim(w_plotspec_array(1),2)
if w_plotspec_id.x_axis_u eq 1 then begin
	xa = findgen(n2+1) 
	scanData.px(0:n2) = xa
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

	tempTitle=strtrim(w_plotspec_array(0))+' (1D SCAN # '+strtrim(w_plotspec_id.seqno+1,2) +')'

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


	if n1 gt 0  then begin
	; plot Detector vs positioner 
	for i=0,scanData.num_det - 1 do begin
	if realtime_id.def(4+i) ne 0 and wf_sel(i) eq 1 then begin
	color = w_plotspec_id.colorI(i)
	; 24 bit visual case
	if !d.n_colors eq 16777216 then begin
		catch1d_get_pvtcolor,color,t_color
		color = t_color
		end
		oplot,scanData.px(0:n1), scanData.da(0:n1,i),LINE=ln_style(i), $
			PSYM = symbol * (i+1) mod 8, $
			COLOR=color
		end
	end
	; plot positoner vs positioner (encode cases)
	for i=0,scanData.num_pos - 1 do begin
	if realtime_id.def(i) ne 0 and wf_sel(15+i) eq 1 then begin
	color = w_plotspec_id.colorI(15+i)
	; 24 bit visual case
	if !d.n_colors eq 16777216 then begin
		catch1d_get_pvtcolor,color,t_color
		color = t_color
		end
	oplot,scanData.px(0:n1), scanData.pa(0:n1,i),LINE=ln_style(i+15), $
			PSYM = symbol * (i+1) mod 8, $
			COLOR=color
		end
	end
	realtime_id.axis = 0
	end
end

if n2 ge n1 then begin
for i=0,scanData.num_det-1 do begin
	if realtime_id.def(4+i) ne 0 then begin
		if n2 eq 0 then begin
		xtemp = [scanData.px(0),scanData.px(0)]
		ytemp = [scanData.da(0,i),scanData.da(0,i)]
		endif else begin
		xtemp = [scanData.px(n1:n2)]
		ytemp = [scanData.da(n1:n2,i)]
		end
		if wf_sel(i) eq 1 then begin
	color = w_plotspec_id.colorI(i)
	; 24 bit visual case
	if !d.n_colors eq 16777216 then begin
		catch1d_get_pvtcolor,color,t_color
		color = t_color
		end
			 oplot,xtemp, ytemp,LINE=ln_style(i), $
			PSYM = symbol * (i+1) mod 8, $
			 COLOR=color
		end
	end
end
for i=0,scanData.num_pos-1 do begin
	if realtime_id.def(i) ne 0 then begin
		if n2 eq 0 then begin
		xtemp = [scanData.px(0),scanData.px(0)]
		ytemp = [scanData.pa(0,i),scanData.pa(0,i)]
		endif else begin
		xtemp = [scanData.px(n1:n2)]
		ytemp = [scanData.pa(n1:n2,i)]
		end
		if wf_sel(i+15) eq 1 then begin
	color = w_plotspec_id.colorI(i+15)
	; 24 bit visual case
	if !d.n_colors eq 16777216 then begin
		catch1d_get_pvtcolor,color,t_color
		color = t_color
		end
			oplot,xtemp, ytemp,LINE=ln_style(i+15), $
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

END


PRO realtime_yrange,auto,ymin,ymax,plotXTitle,pos_ymin

   COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

   WIDGET_CONTROL, widget_ids.wf_select, GET_VALUE = wf_sel
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
     if total(wf_sel) eq 0 then plotXTitle = 'Nothing Selected'

pos_ymin = 1.e20
for i=0,scanData.num_det-1 do begin
     IF (wf_sel(i) EQ 1 and realtime_id.def(scanData.num_pos+i) NE 0) THEN  BEGIN
     d4 = scanData.da(0:num_pts,i)
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
	IF(wf_sel(scanData.num_det+i) eq 1 and realtime_id.def(i) NE 0) THEN BEGIN
	d4 = scanData.pa(0:num_pts,i)
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

   WIDGET_CONTROL, widget_ids.wf_select, GET_VALUE = wf_sel
   x_axis = w_plotspec_id.x_axis_u

   ;remember the state of "auto" for next time
   scanData.lastPlot = auto

; if the time axis is selected for plot for real time

if realtime_id.def(w_plotspec_id.xcord) eq 2 then begin

        x_rn = realtime_pvnames

        xmax = realtime_id.xmax
        ln = cagetArray(x_rn(w_plotspec_id.xcord),pd)
        if ln eq 0 and pd gt xmax then begin
                xmin=0
                xmax=pd(0)
                dx = 0.1 * pd
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
	realtime_id.xsetup = pd

; On the fly mode
	if x_dv(3) eq 2. then begin
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
	xmin = x_dv(0)
	xmax = x_dv(1)
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

if n_elements(x_names) eq 0 then begin
        x_names=make_array(4,/string,value=string(replicate(32b,30)))
        y_names=make_array(15,/string,value=string(replicate(32b,30)))
        x_descs=make_array(4,/string,value=string(replicate(32b,30)))
        y_descs=make_array(15,/string,value=string(replicate(32b,30)))
        x_engus=make_array(4,/string,value=string(replicate(32b,30)))
        y_engus=make_array(15,/string,value=string(replicate(32b,30)))
	end

if w_plotspec_id.mode eq 0 then begin
if casearch(scanData.pv) eq 0 then begin 
        find_desc_engu,names,descs,engus

        x_names = names(0:3)
        y_names = names(4:18)
        x_descs = descs(0:3)
        y_descs = descs(4:18)
        x_engus = engus(0:3)
        y_engus = engus(4:18)

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

; get title
	st = ''
	ln = cagetArray(scanData.pv+'.NAME',pd)
if ln eq 0 then begin
	st = pd(0)
	title = string(replicate(32b,60))
        title = st +' ('+ scanData.pv +')'
        if scanData.y_scan then begin
		y_seqno = scanData.y_seqno

		if y_seqno gt 0 and y_seqno eq scanData.y_req_npts then $
			 y_seqno = y_seqno-1
;		if scanData.dataversion ne '' then $
;		title = st + ' @ y('+strtrim(y_seqno,2) + ')=' + $
;			strtrim(scanData.y_value,2) $
;		else $
		title = st + ' @ y('+strtrim(y_seqno,2) + ')'
	end
end
; get xlabel
	xlabel = string(replicate(32b,60))
     if ix lt 4 then begin
	len = strlen(x_descs(ix))
        if len gt 1 then strput,xlabel,x_descs(ix) else $
        	strput,xlabel,x_names(ix)
;        	strput,xlabel,'P'+strtrim(ix+1,2)
	if len lt 1 then len = 2
	l2 = strlen(x_engus(ix))
        if l2 gt 1 then begin
		len = len + 2
		strput,xlabel,'(',len
		len = len + 2
		strput,xlabel,strtrim(x_engus(ix)),len
		len = len + l2 + 1
		strput,xlabel,')',len
		end
     end
     if ix ge 4 then begin   ; if detector for x axis
	ixx = ix - 4
	len = strlen(y_descs(ixx))
        if len gt 1 then strput,xlabel,y_descs(ixx) else $
        	strput,xlabel,'D'+strtrim(ixx+1,2)
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
;       names - scan record Pi,Di PV names (4 positioners and 15 detectors)
;       descs - corresponding descs from the  database
;       engus - corresponding Pi, Di engu from the database
;
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON field_block, field_name, field_name_array, field_value, w_scanfield_ids
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

names= make_array(19,/string,value=string(replicate(32b,30)))
descs = make_array(19,/string,value=string(replicate(32b,30)))
engus = make_array(19,/string,value=string(replicate(32b,30)))

if casearch(scanData.pv) eq 0 then begin 
scan_field_get,scanData.pv

; check for defined PiPV & DiPV
realtime_id.def = make_array(19,/int)
; 4 positioner + 15 detectors

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
 
x_dn = [ scanData.pv+'.D1PV', $
	scanData.pv+'.D2PV', $
	scanData.pv+'.D3PV', $
	scanData.pv+'.D4PV', $
	scanData.pv+'.D5PV', $
	scanData.pv+'.D6PV', $
	scanData.pv+'.D7PV', $
	scanData.pv+'.D8PV', $
	scanData.pv+'.D9PV', $
	scanData.pv+'.DAPV', $
	scanData.pv+'.DBPV', $
	scanData.pv+'.DCPV', $
	scanData.pv+'.DDPV', $
	scanData.pv+'.DEPV', $
	scanData.pv+'.DFPV' $
	]
ln = cagetArray(x_dn,pd,/string)
x_dv = strtrim(pd,2)
names(0:3) = p1
names(4:18) = x_dv

; get desc & eng units
 
s0=string(replicate(32b,30))

for i=0,18 do begin
if strlen(names(i)) gt 1 then begin
 
        realtime_id.def(i) = 1
        id = strpos(names(i),'.',0)
 
	v=s0
        if id ne -1 then strput,v,strmid(names(i),0,id),0 else $
		strput,v,names(i),0
	vd = strcompress(v + '.DESC',/remove_all)
	pd=''
	ln = cagetArray(vd,pd)
	descs(i) = pd
        if strtrim(descs(i),2) eq '-1' then descs(i)=''
        end
end

x_dn = [ scanData.pv+'.P1EU', $
	scanData.pv+'.P2EU', $
	scanData.pv+'.P3EU', $
	scanData.pv+'.P4EU', $
	scanData.pv+'.D1EU', $
	scanData.pv+'.D2EU', $
	scanData.pv+'.D3EU', $
	scanData.pv+'.D4EU', $
	scanData.pv+'.D5EU', $
	scanData.pv+'.D6EU', $
	scanData.pv+'.D7EU', $
	scanData.pv+'.D8EU', $
	scanData.pv+'.D9EU', $
	scanData.pv+'.DAEU', $
	scanData.pv+'.DBEU', $
	scanData.pv+'.DCEU', $
	scanData.pv+'.DDEU', $
	scanData.pv+'.DEEU', $
	scanData.pv+'.DFEU' $
	]
	ln = cagetArray(x_dn,pd,/string)
	x_dv = pd
	for i=0,18 do begin
        engus(i) = strtrim(x_dv(i),2)
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
	 strcompress(w_plotspec_id.seqno + 1))

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


PRO plotoption_setcolor
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

  if w_plotspec_id.color eq 1 then begin
;        LOADCT, 39
	dcl = !d.table_size - 2
	ncv = 4 
        colorlevel = dcl / ncv
        for i=0,18 do begin
        ii = i / ncv
        im = i mod ncv
        w_plotspec_id.colorI(i) = dcl - ii - im * colorlevel
        end
  end

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
	plotoption_setcolor
	end
    3: begin
	plotoptionsmenu_set_string,3,2
	w_plotspec_id.color = 0
	for i=0,18 do begin
       	w_plotspec_id.colorI(i) = !d.table_size - 1 
	end
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
        WIDGET_CONTROL,widget_ids.savelabel,SET_VALUE='  No Save'
        return
	end
      6: begin
	WIDGET_CONTROL,ids(6),SET_VALUE=r_names(6)
	len = strlen(r_names(5))-1
	WIDGET_CONTROL,ids(5),SET_VALUE=' '+strmid(r_names(5),1,len)
        w_plotspec_id.autosave = 0
        WIDGET_CONTROL,widget_ids.savelabel,SET_VALUE='          '
	catch1d_check_seqno
        return
	end
      8: begin
	WIDGET_CONTROL,ids(8),SET_VALUE=r_names(8)
	len = strlen(r_names(9))-1
	WIDGET_CONTROL,ids(9),SET_VALUE=' '+strmid(r_names(9),1,len)
        w_plotspec_id.realtime = 0
        return
	end
      9: begin
	WIDGET_CONTROL,ids(9),SET_VALUE=r_names(9)
	len = strlen(r_names(8))-1
	WIDGET_CONTROL,ids(8),SET_VALUE=' '+strmid(r_names(8),1,len)
        w_plotspec_id.realtime = 1
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
	xloadct, GROUP= event.top
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
        { CW_PDMENU_S,       0, 'Color ...' } $ ;        2
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


PRO scanReadAll,unit,maxno,pos,errcode,print=print
	u_rewind,unit
	pos=0L
	errcode = 0
	while NOT EOF(unit) and errcode eq 0 do begin
	u_read,unit,version,errcode
	u_read,unit,pv,errcode
	u_read,unit,num_pts,errcode
	u_read,unit,id_def,errcode
	u_read,unit,x_dpt,errcode
	for i=0,3 do begin
		if id_def(i) eq 1 then begin
		u_read,unit,pa,errcode
		if keyword_set(print) then print,i,id_def(i),pa
		end
	end
	for i=4,18 do begin
		if id_def(i) eq 1 then begin
		u_read,unit,da,errcode
		if keyword_set(print) then print,i,id_def(i),da
		end
	end
	u_read,unit,lebels,errcode
	u_read,unit,x,errcode
	u_read,unit,y,errcode
	u_read,unit,n,errcode
	if keyword_set(print) then begin
		print,version
		print,'pv=',pv
		print,'num_pts=',num_pts
		print,'id_def=',id_def
		print,'x_dpt=',x_dpt
		print,'labels=',lebels
		print,'x=',x
		print,'y=',y
		print,'n=',n
	end
	if n(0) gt 0 then begin
		u_read,unit,ze,errcode
	end
	point_lun,-unit,loc
	pos = [pos,loc]
	end
	maxno = n_elements(pos)-1
END
;
; position to the head of seqno'th record
;
PRO scan_read_position,unit,seqno
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
	w_viewscan_id.seqno = seqno 
	id = seqno
	i = 1
	POINT_LUN,w_viewscan_id.unit,0
	WHILE i lt seqno do begin 
		scan_read_record,unit
		i = i + 1
	END 
END

PRO scan_read_all,unit,maxno
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

	status = FSTAT(unit)

	indexFile = status.name + '.index'
	size = status.size

	w_viewscan_id.file = status.name

; check whether indexFile exist
found = findfile(indexFile)
if found(0) eq '' then begin
	id = 0
	w_viewscan_id.fptr = make_array(10000,/long)

	scanReadAll,unit,maxno,pos,errcode
	w_viewscan_id.fptr = pos
	w_viewscan_id.maxno = maxno
endif else begin

; check file size change

if w_viewscan_id.file ne status.name then begin

	id = 0
	w_viewscan_id.fptr = make_array(10000,/long)

	scanReadAll,unit,maxno,pos,errcode
	w_viewscan_id.fptr = pos
	w_viewscan_id.maxno = maxno

endif else begin

	if size gt w_viewscan_id.size then begin

	id = w_viewscan_id.maxno
	point_lun,unit,w_viewscan_id.size
        w_viewscan_id.fptr(id) = w_viewscan_id.size 

	WHILE NOT  EOF(unit) DO BEGIN
	id = id + 1
		scan_read_record,unit
		point_lun,-unit,pos
		w_viewscan_id.fptr(id) = pos
	END
	maxno = id	
	w_viewscan_id.maxno = maxno
	endif else maxno = w_viewscan_id.maxno 
end
end
	w_viewscan_id.seqno = 0
	w_viewscan_id.size = status.size
	w_viewscan_id.file = status.name

END

PRO read_desc_engu,labels
COMMON LABEL_BLOCK,x_names,y_names,x_descs,y_descs,x_engus,y_engus

if n_elements(x_names) lt 4 then begin
	x_names = make_array(4,/string,value=string(replicate(32b,30)))
	x_descs = x_names
	x_engus = x_names
	y_names = make_array(15,/string,value=string(replicate(32b,30)))
	y_descs = y_names
	y_engus = y_names
	end
labels = string(labels)
for i=0,3 do begin 
	x_names(i) = labels(i)
	x_descs(i) = labels(i+19)
	x_engus(i) = labels(i+38)
end
for i=0,14 do begin 
	y_names(i) = labels(i+4)
	y_descs(i) = labels(i+4+19)
	y_engus(i) = labels(i+4+38)
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
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON env_field_block,env_field

filename = strcompress(w_plotspec_array(3),/remove_all)+'.tmp'

CATCH,error_status
if error_status lt 0 then begin
	w_warningtext,'Error: '+ !err_string
	return
	end

openw,unit,filename,/get_lun
printf,unit,"; VERSION: ",scanData.version,' ',scanData.release
if scanData.y_seqno gt 0 or scanData.y_scan gt 0 then begin
printf,unit,"; 2D SCAN #: ",scanData.scanno_2d,",      Y INDEX #:",scanData.y_seqno
	end
printf,unit,"; 1D SCAN #: ",w_plotspec_id.seqno + 1 
printf,unit,"; SCAN Record Name: ",scanData.pv

if env_field.exist gt 0 then begin
no = env_field.no 

if no gt 0 then begin
printf,unit,'; '
if scanData.scanno gt scanData.refno then $
printf,unit,';  REFERENCE ENVIRONMENT VARIABLES SAVED IN SCAN #',scanData.refno
printf,unit,';  ENVIRONMENT VARIABLES SAVED FOR SCAN #:', scanData.scanno
printf,unit,'; '
twd = 18*total(realtime_id.def) + 10
s0 = string(replicate(32b,twd))
st = s0
strput,st,'; ',0
strput,st,'PVNAME',2
strput,st,'VALUE',30
strput,st,'DESCRIPTION',60
printf,unit,st
printf,unit,';'
	for i=0,no-1 do begin
	if i eq env_field.noenv then printf,unit,';'
	st = s0
	strput,st,'; ',0
	strput,st,env_field.pvnames(i),2
	strput,st,env_field.values(i),30
	strput,st,env_field.descs(i),60
	printf,unit,st
	end
end
endif else begin
if scanData.scanno gt scanData.refno then begin
printf,unit,'; '
printf,unit,';  ENVIRONMENT VARIABLES ARE SAME AS THE PREVIOUS SCAN, OR '
printf,unit,';  THE ENVIRONMENT FILE  catch1d.env DOES NOT EXIST.'
printf,unit,'; '
	end
end

printf,unit,'; '
printf,unit,"; PLOT SPECIFICATIONS"
printf,unit,'; '
if scanData.y_scan gt 0 then $
printf,unit,"; Title:      ",strtrim(w_plotspec_array(0),2) + '='+strtrim(scanData.y_value,2) else $
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
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON LABEL_BLOCK,x_names,y_names,x_descs,y_descs,x_engus,y_engus

; get desc & eng units

if w_plotspec_id.mode eq 0 and  casearch(scanData.pv) eq 0 then begin
	find_desc_engu,names,descs,engus
endif else begin    ; viewing mode
	names = [x_names,y_names]
	descs = [x_descs,y_descs]
	engus = [x_engus,y_engus]
end

	no = n_elements(names)
	st = ';    I   '
        for i=0,no-1 do begin
        if realtime_id.def(i) ne 0 then begin
                st = st+ ' '+names(i)
                end
        end
	printf,unit,st

;s0 = string(replicate(32b,340))
twd = strlen(st) > 18*total(realtime_id.def) + 10
s0 = string(replicate(32b,twd))
st = s0
strput,st,';  (Desc)',0  &  ij = 17 
        for i=0,no-1 do begin
        if realtime_id.def(i) ne 0 then begin
                strput,st,descs(i),ij
                ij = ij + 18
                end
        end
printf,unit,st

st = s0
strput,st,'; (Units)',0  &  ij = 17 
        for i=0,no-1 do begin
        if realtime_id.def(i) ne 0 then begin
                strput,st,engus(i),ij
                ij = ij + 18
                end
        end
printf,unit,st

num_npts = scanData.readin_npts

temp_format = '('+scanData.code+scanData.format+')'
temp_digit = fix(scanData.format)

for i=0,num_npts-1 do begin
st = s0
strput,st,i,0  &  ij = 10
	for j = 0,3 do begin
		if realtime_id.def(j) ne 0 then begin
		strput,st,string(scanData.pa(i,j),format=temp_format),ij  
		ij = ij + temp_digit
		end
	end
	for j = 0,14 do begin
		if realtime_id.def(4+j) ne 0 then begin
		strput,st,string(scanData.da(i,j),format=temp_format),ij  
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

	"VIEWSPEC_SEQNO" : BEGIN
		WIDGET_CONTROL,w_viewscan_ids.seqno,GET_VALUE=seqno
		i1 = fix(seqno(0))
	if i1 lt 1  or i1 gt w_viewscan_id.maxno then begin
	w_warningtext,'Input out of range !!'
	return
	end
		if i1 ge 1 then begin 
		point_lun, w_viewscan_id.unit, w_viewscan_id.fptr(i1-1)
		i2 = i1 + 1
		scan_read,w_viewscan_id.unit, i1, i2
		WIDGET_CONTROL,w_viewscan_ids.plotspec,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.field,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
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
		xdisplayfile,filename,width=110,GROUP= event.top
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
		point_lun,w_viewscan_id.unit,w_viewscan_id.fptr(i1-1)
		scan_read,w_viewscan_id.unit, i1, i2
		seqno = strtrim(string(i1),2)
		WIDGET_CONTROL,w_viewscan_ids.seqno,SET_VALUE=seqno
		WIDGET_CONTROL,w_viewscan_ids.printplot,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.plotspec,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.field,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
                END
	"VIEWSPEC_OK" : BEGIN
		close_plotspec
		w_warningtext_close
		WIDGET_CONTROL,w_viewscan_ids.seqno,GET_VALUE=seqno
		i1 = fix(seqno(0))
	if i1 lt 1  or i1 gt w_viewscan_id.maxno then begin
	w_warningtext,'Input out of range !!'
	return
	end

		point_lun,w_viewscan_id.unit,w_viewscan_id.fptr(i1-1)
		i2 = i1 + 1
		scan_read,w_viewscan_id.unit, i1, i2
		WIDGET_CONTROL,w_viewscan_ids.printplot,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.plotspec,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.field,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
                END
	"VIEWSPEC_NEXT" : BEGIN
		close_plotspec
		w_warningtext_close
		WIDGET_CONTROL,w_viewscan_ids.seqno,GET_VALUE=seqno
		i1 = fix(seqno(0)) + 1
		if i1 ge (w_viewscan_id.maxno+1) then i1 = 1 
		i2 = i1 + 1
		point_lun,w_viewscan_id.unit,w_viewscan_id.fptr(i1-1)

		scan_read,w_viewscan_id.unit, i1, i2 

		seqno = strtrim(string(i1),2)
		WIDGET_CONTROL,w_viewscan_ids.seqno,SET_VALUE=seqno
		WIDGET_CONTROL,w_viewscan_ids.printplot,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.plotspec,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.field,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0

                END
	"VIEWSPEC_LAST" : BEGIN
		close_plotspec
		w_warningtext_close
		if w_viewscan_id.maxno eq 0 then begin
			w_warningtext,'Error: no data available!'
			return
			end
		i1 = w_viewscan_id.maxno-1
		i2 = i1+1
		point_lun,w_viewscan_id.unit,w_viewscan_id.fptr(i1)
		scan_read,w_viewscan_id.unit, i2, i2+1
		WIDGET_CONTROL,w_viewscan_ids.seqno,SET_VALUE=strtrim(i2,2)
		WIDGET_CONTROL,w_viewscan_ids.printplot,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.plotspec,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.field,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
                END
	"VIEWSPEC_FIRST" : BEGIN
		close_plotspec
		w_warningtext_close
		if w_viewscan_id.maxno eq 0 then begin
			w_warningtext,'Error: no data available!'
			return
			end
		point_lun,w_viewscan_id.unit,w_viewscan_id.fptr(0)
		scan_read,w_viewscan_id.unit, 1, 2
		WIDGET_CONTROL,w_viewscan_ids.seqno,SET_VALUE='1'
		WIDGET_CONTROL,w_viewscan_ids.printplot,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.plotspec,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.field,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
                END
	"VIEWSPEC_SLIDER" : BEGIN
		close_plotspec
		w_warningtext_close
		WIDGET_CONTROL,w_viewscan_ids.slider,GET_VALUE=seqno
		point_lun,w_viewscan_id.unit,w_viewscan_id.fptr(seqno-1)
		scan_read,w_viewscan_id.unit,seqno,seqno+1 
		s1 = strtrim(string(seqno),2)
		WIDGET_CONTROL,w_viewscan_ids.seqno,SET_VALUE=s1
		WIDGET_CONTROL,w_viewscan_ids.printplot,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.plotspec,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.field,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
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

if w_viewscan_id.unit gt 0 then begin
	if (w_viewscan_id.seqno+1) lt w_viewscan_id.maxno then begin
		i1 = w_viewscan_id.maxno
		point_lun, w_viewscan_id.unit, w_viewscan_id.fptr(i1-1)
		i2 = i1 + 1
		scan_read,w_viewscan_id.unit, i1, i2
	end
end

; close the view mode 
		if w_viewscan_id.unit gt 0 then free_lun,w_viewscan_id.unit
		w_viewscan_id.unit = 0
		if w_plotspec_id.opened gt 0 then free_lun,w_plotspec_id.opened
		w_plotspec_id.opened = 0
		set_sensitive_on

; reset the w_plotspec variable
		w_plotspec_id.seqno = w_viewscan_id.maxno
		w_plotspec_id = scanData.plotspec 

		w_warningtext_close

		if XRegistered('catcher_setup') ne 0 then $
		WIDGET_CONTROL,catcher_setup_ids.base,SENSITIVE=1

; reset to config pv names
 
		scanData.pv = scanData.pvconfig

 		pventry_event ;======
;		before_sys_scan

		w_plotspec_restoreTitle


END

PRO w_viewscan, unit, GROUP = GROUP, help=help

COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON view1d_summary_block, view1d_summary_ids, view1d_summary_id


if XRegistered('w_viewscan') ne 0 then return

if XRegistered('view1d_summary_setup') ne 0 then $
	WIDGET_CONTROL,view1d_summary_ids.base,/DESTROY

if scanData.lastPlot lt 0 then scanData.lastPlot = 1  ; autoscale  

if n_params() lt 1 then begin
	w_warningtext,'usage:  w_viewscan, unit, GROUP=event.top'
	end

; reset the pv name in viewing mode and save the config pv for later on
;      scan mode
scanData.pvconfig = scanData.pv  

WIDGET_CONTROL, /HOURGLASS

w_viewscan_id.unit = unit
scan_read_all,w_viewscan_id.unit,maxno

w_viewscan_base=WIDGET_BASE(GROUP_LEADER=Group, $
	TLB_FRAME_ATTR = 8, $
	TITLE = 'VIEW 1D ... ', /COLUMN)     

w_viewscan_label = WIDGET_LABEL(w_viewscan_base,VALUE='Scan Data from : ' + $
		strcompress(w_plotspec_array(3)))
row0 = WIDGET_BASE(w_viewscan_base, /ROW,/FRAME)

w_viewscan_printplot = WIDGET_BUTTON(row0, $
                        VALUE = 'Print Plot', $
                        UVALUE = 'VIEWSPEC_PSPRINT')
WIDGET_CONTROL,w_viewscan_printplot,SENSITIVE=0

w_viewscan_plotspec = WIDGET_BUTTON(row0, $
                        VALUE = 'Modify Plot', $
                        UVALUE = 'VIEWSPEC_NEW')
WIDGET_CONTROL,w_viewscan_plotspec,SENSITIVE=0
w_viewscan_field = WIDGET_BUTTON(row0, $
                        VALUE = 'ASCII View', $
                        UVALUE = 'VIEWSPEC_FIELD')
WIDGET_CONTROL,w_viewscan_field,SENSITIVE=0
w_viewscan_print = WIDGET_BUTTON(row0, $
                        VALUE = 'ASCII Print', $
                        UVALUE = 'VIEWSPEC_PRINT')
WIDGET_CONTROL,w_viewscan_print,SENSITIVE=0

	str = 'Enter Scan # [ 1 -'+ strcompress(string(maxno)) +' ] '
row1 = WIDGET_BASE(w_viewscan_base, /ROW)
w_viewscan_label = WIDGET_LABEL(row1,VALUE=str)
w_viewscan_seqno = WIDGET_TEXT(row1,VALUE='0', $
		EDITABLE=1, $
		UVALUE='VIEWSPEC_SEQNO', XSIZE = 8)

w_viewscan_format = CW_FIELD( row1,VALUE=scanData.code+scanData.format, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS= 1, $
      TITLE='Column Format: ', $
      XSIZE=8, $
      UVALUE='VIEWSPEC_FORMAT')

; add seqno slider here

if w_viewscan_id.maxno gt 1 then begin
	w_viewscan_slider = WIDGET_SLIDER(w_viewscan_base, $
		MAX=w_viewscan_id.maxno, $
		MIN=1,UVALUE='VIEWSPEC_SLIDER')
	end


lastrow = WIDGET_BASE(w_viewscan_base, /ROW)

w_viewscan_first = WIDGET_BUTTON(lastrow, $
                        VALUE = ' First ', $
                        UVALUE = 'VIEWSPEC_FIRST')
w_viewscan_next = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Next ', $
                        UVALUE = 'VIEWSPEC_NEXT')
w_viewscan_next = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Prev ', $
                        UVALUE = 'VIEWSPEC_PREV')
w_viewscan_last = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Last ', $
                        UVALUE = 'VIEWSPEC_LAST')
w_viewscan_cancel = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Done ', $
                        UVALUE = 'VIEWSPEC_CANCEL')

; set widget ids :
w_viewscan_ids = { $
	base:	w_viewscan_base, $
	printplot: w_viewscan_printplot, $
	plotspec: w_viewscan_plotspec, $
	field: w_viewscan_field, $
	print:	w_viewscan_print, $
	slider:  0L, $
	seqno:  w_viewscan_seqno, $
	format:  w_viewscan_format $
	 }

if n_elements(w_viewscan_slider) gt 0 then w_viewscan_ids.slider = w_viewscan_slider

WIDGET_CONTROL, widget_ids.wf_select,SENSITIVE=1

; Realize the widgets:
WIDGET_CONTROL, w_viewscan_base, /REALIZE, $
	TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 100

; Hand off to the XMANAGER:
XMANAGER, 'w_viewscan', w_viewscan_base, GROUP_LEADER = GROUP, CLEANUP = 'w_viewscan_close'

END



; 
; save data only for curr scan record 
; 
PRO mere_data_dump, unit
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON env_field_block,env_field

if scanData.y_seqno gt 0 or scanData.y_scan gt 0 then begin
printf,unit,"; 2D SCAN #: ",scanData.scanno_2d,",      Y INDEX #:",scanData.y_seqno
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
COMMON env_field_block,env_field

printf,unit,"; VERSION: ",scanData.version,' ',scanData.release
if scanData.y_seqno gt 0 or scanData.y_scan gt 0 then begin
printf,unit,"; 2D SCAN #: ",scanData.scanno_2d,",      Y INDEX #:",scanData.y_seqno
        end

printf,unit,"; 1D SCAN #: ",scanData.scanno
printf,unit,"; SCAN Record Name: ",scanData.pv

no = env_field.numkey 
if no gt 0 then begin
printf,unit,'; '
printf,unit,';  KEY PV names got from the catch1d.env'
printf,unit,'; '
;s0 = string(replicate(32b,340))
twd = 18*total(realtime_id.def) + 10
s0 = string(replicate(32b,twd))
st = s0
strput,st,'; ',0
strput,st,'PVNAME',2
strput,st,'VALUE',30
strput,st,'DESCRIPTION',60
printf,unit,st

; handle the read in env same as the key env for summary report
noenv = env_field.noenv
if noenv ge no then begin

	for i=0,no-1 do begin
	st = s0
	strput,st,'; ',0
	strput,st,env_field.pvnames(i),2
	strput,st,env_field.values(i),30
	strput,st,env_field.descs(i),60
	printf,unit,st
	end
	
endif else begin

	for i=0,no-1 do begin
	st = s0
	strput,st,'; ',0
	strput,st,env_field.pvnames(env_field.keys(i)),2
	strput,st,env_field.values(env_field.keys(i)),30
	strput,st,env_field.descs(env_field.keys(i)),60
	printf,unit,st
	end
end
end


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
PRO summary_report_dump,filename,outfile,start,stop,header 
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

;tempname = strcompress(w_plotspec_array(3),/remove_all)+'.rep'
tempname=outfile

; position record to the startno
	if scanData.XDR then U_OPENR,unit1,filename,/XDR else $
	u_openr,unit1,filename

	if start gt 0 then point_lun,unit1,w_viewscan_id.fptr(start-1)

end_unit1:
if EOF(unit1) then begin
                print,'EOF! Last record is',i-1
                u_close, unit1
                return
	end

openw,unit2,tempname,/get_lun 

for i=start, stop do begin
	if not EOF(unit1) then begin
	scan_read,unit1,i,i	
	scanData.scanno = i
	if header eq 0 then report_data_dump,unit2
	if header eq 1 then shortreport_data_dump,unit2
	if header eq 2 then mere_data_dump,unit2
	endif else begin
		print,'EOF!  Last scan # is',i-1
		goto, end_loop 
	end
end

end_loop:
	u_close,unit2
	u_close,unit1

END


PRO report_data_dump,unit
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON env_field_block,env_field


printf,unit,"; VERSION: ",scanData.version,' ',scanData.release
if scanData.y_seqno gt 0 or scanData.y_scan gt 0 then begin
printf,unit,"; 2D SCAN #: ",scanData.scanno_2d,",      Y INDEX #:",scanData.y_seqno
        end

printf,unit,"; 1D SCAN #: ",w_plotspec_id.seqno + 1 
printf,unit,"; SCAN Record Name: ",scanData.pv


no = env_field.no             
if env_field.exist gt 0 then begin
; in summary report generation the env_field.no is the actual 
; environment values written for the case

if no gt 0 then begin
printf,unit,'; '
if scanData.scanno gt scanData.refno then $
printf,unit,';  REFERENCE ENVIRONMENT VARIABLES SAVED IN SCAN #',scanData.refno
printf,unit,';  ENVIRONMENT VARIABLES SAVED FOR SCAN #:', scanData.scanno
printf,unit,'; '
;s0 = string(replicate(32b,340))
twd = 18*total(realtime_id.def) + 10
s0 = string(replicate(32b,twd))
st = s0
strput,st,'; ',0
strput,st,'PVNAME',2
strput,st,'VALUE',30
strput,st,'DESCRIPTION',60
printf,unit,st
printf,unit,';'
	for i=0,no-1 do begin
	if i eq env_field.noenv then printf,unit,';'
	st = s0
	strput,st,'; ',0
	strput,st,env_field.pvnames(i),2
	strput,st,env_field.values(i),30
	strput,st,env_field.descs(i),60
	printf,unit,st
	end
end
endif else begin
if scanData.scanno gt scanData.refno then begin
printf,unit,'; '
printf,unit,';  ENVIRONMENT VARIABLES ARE SAME AS THE PREVIOUS SCAN, OR '
printf,unit,';  THE ENVIRONMENT FILE  catch1d.env DOES NOT EXIST.'
printf,unit,'; '
	end
end

if scanData.scanno eq scanData.refno then begin
printf,unit,'; '
	for i=no,env_field.no-1 do begin
	st = s0
	strput,st,'; ',0
	strput,st,env_field.pvnames(i),2
	strput,st,env_field.values(i),30
	strput,st,env_field.descs(i),60
	printf,unit,st
	end
end

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

PRO view1d_summary_generate
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
	WIDGET_CONTROL,view1d_summary_ids.start, GET_VALUE=start
	WIDGET_CONTROL,view1d_summary_ids.stop, GET_VALUE=stop
	if stop lt start then stop=start
	view1d_summary_id.start = start
	view1d_summary_id.stop = stop

	filename = scanData.trashcan 
	found = findfile(filename)
if found(0) ne '' then  begin	
	WIDGET_CONTROL,view1d_summary_ids.outfile, GET_VALUE=file
	view1d_summary_id.outfile=strcompress(file(0),/remove_all)

	;  use the data directory first if failed then home directory

	report_path = view1d_summary_id.outpath
	save_outfile = report_path+view1d_summary_id.outfile

; quard trashcan
	if save_outfile eq scandata.trashcan then begin
		res = widget_message('Error: illigal file name entered!!',/error)
		return
	end

; quard existing file 
	found = findfile(save_outfile)
	if found(0) ne '' then begin

		st = ['Warning!  Warning!  Warning!  ' , save_outfile, '     already existed.', $
		     ' Is it ok to rename as ', $
		     save_outfile+ '.bk','???']
		res = widget_message(st,/Question)
		if res eq 'No' then goto,view_print
		move_file = save_outfile + '.bk'
deepmove:
		found1 = findfile(move_file)
		if found1(0) ne '' then begin
			st = [' Warning!  Warning!', $
				move_file, $
				'also already existed !!']
			res = widget_message(st,/Question)
			if res eq 'No' then goto,view_print 
			move_file = move_file + '.bk'
			goto,deepmove
		end
		str = OS_SYSTEM.mv+' '+save_outfile+' '+move_file+' &' 
		spawn,str
	end

RESETSENSE:

; save as one big file
    if view1d_summary_id.separate eq 0 then begin
		w_plotspec_id.mode = 1
		summary_report_dump,filename,save_outfile,start,stop,view1d_summary_id.header
		w_plotspec_id.mode = 0

	if scanData.debug eq 1 then $
		print, 'Report file: ', save_outfile,' created!'
; save as separate files
     endif else begin
	str = '0000'
	len0 = 4 
	w_plotspec_id.mode = 1
	for i=start,stop do begin
	sss = str
	st = strtrim(i,2)
	len = strlen(st)
	strput,sss,st,len0-len
		save_outfile=report_path+w_plotspec_array(3)+'.'+sss
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
  'summary_start': BEGIN
	WIDGET_CONTROL,view1d_summary_ids.start, GET_VALUE=start
	if start gt 0 and start le w_viewscan_id.maxno then begin
	view1d_summary_id.start = start
	report_setup
	WIDGET_CONTROL, view1d_summary_ids.outfile, SET_VALUE= view1d_summary_id.outfile
	endif else begin
		w_warningtext,['Error: can not exceed '+ string(w_viewscan_id.maxno) ]
          WIDGET_CONTROL,view1d_summary_ids.start, SET_VALUE=w_viewscan_id.maxno
	end
      END
  'summary_end': BEGIN
	WIDGET_CONTROL,view1d_summary_ids.stop, GET_VALUE=stop
	if stop gt 0 and stop le w_viewscan_id.maxno then begin
	view1d_summary_id.stop = stop
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
  'summary_outpath': BEGIN
        WIDGET_CONTROL,view1d_summary_ids.outpath,GET_VALUE=path
        outpath = strcompress(path(0),/remove_all)
        len = strlen(outpath)
        if strmid(outpath,len-1,1) ne !os.file_sep then  $
		outpath=outpath+!os.file_sep
        found = findfile(outpath,count=ct)
        if ct eq 0 then spawn,!os.mkdir + ' '+ outpath
        view1d_summary_id.outpath = outpath
        WIDGET_CONTROL,view1d_summary_ids.outpath,SET_VALUE=outpath
      END
  'summary_outfile': BEGIN
        WIDGET_CONTROL,view1d_summary_ids.outfile,GET_VALUE=file
        view1d_summary_id.outfile = file(0)
      END
  'summary_ok': BEGIN
	  view1d_summary_generate
      END
  'summary_view': BEGIN
        WIDGET_CONTROL,view1d_summary_ids.outfile,GET_VALUE=file
	view1d_summary_id.outfile = file(0)
	filename = view1d_summary_id.outfile

	outfile = view1d_summary_id.outpath + filename
	found = findfile(outfile)
	if found(0) ne '' then begin
	  xdisplayfile,outfile,width=110,GROUP=event.top 
  		return
 	end

	  view1d_summary_generate
	  xdisplayfile,outfile,width=110,GROUP=event.top 
	END

  'summary_print': BEGIN
        WIDGET_CONTROL,view1d_summary_ids.outfile,GET_VALUE=file
	view1d_summary_id.outfile = file(0)
	filename = view1d_summary_id.outfile

	outfile = view1d_summary_id.outpath + filename
	found = findfile(outfile)
	if found(0) ne '' then begin
		if OS_SYSTEM.os_family eq 'unix' then $
		str = !os.prt+' '+!os.printer+' -r '+outfile else $
		str = !os.prt+' '+outfie
		spawn,str+' &'
		return
	endif else $
		w_warningtext,['Error:','    '+outfile+ '  not found!']
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

  outpath = scanData.path +'ASCII'+!os.file_sep
  catch,error_status
  if error_status ne 0 then begin
	cd, current=p
	outpath = p +!os.file_sep+'ASCII'+!os.file_sep
	found = findfile(outpath,count=ct)
	if ct eq 0 then spawn,!os.mkdir + ' '+outpath
  end
  openw,1,outpath+'.tmp'
  close,1
  ;spawn,!os.rm + ' '+outpath+'1.tmp'
  
view1d_summary_id = { $
	start: 1, $
	stop: 1, $
	header: 0, $
	separate: 0, $
	outpath: outpath,  $
	outfile: w_plotspec_array(3)+'.rep',  $
	file: scanData.trashcan  $
	}

if scanData.XDR then U_OPENR,unit,view1d_summary_id.file,/XDR else $
u_openr,unit,view1d_summary_id.file
scan_read_all,unit,maxno
free_lun,unit
view1d_summary_id.start = maxno
view1d_summary_id.stop = maxno

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

  summary_file = WIDGET_LABEL( BASE2,/ALIGN_LEFT, $
		VALUE='Source: '+view1d_summary_id.file)


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

  summary_outpath = CW_FIELD( BASE2,VALUE=view1d_summary_id.outpath, $
      ROW=1, XSIZE=60, /return_events, $
      TITLE='Output file path: ', $
      UVALUE='summary_outpath')

  summary_outfile = CW_FIELD( BASE2,VALUE=view1d_summary_id.outfile, $
      ROW=1, XSIZE=60, /return_events, $
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
	outpath: summary_outpath, $
	outfile: summary_outfile, $
	view: summary_view, $
	print: summary_print, $
	start: summary_start, $
	stop: summary_end $
	}

  WIDGET_CONTROL, view1d_summary_base, /REALIZE

  XMANAGER, 'view1d_summary_setup', view1d_summary_base, GROUP_LEADER = GROUP 
END
PRO scan_read_record,unit,version,pv,num_pts,FA,x,y,n,ze

	u_read,unit,version
	u_read,unit,pv
	u_read,unit,num_pts
	u_read,unit,id_def
	u_read,unit,x_dpt

num_po = 0
for i=0,18 do begin
       if id_def(i) gt 0 then num_po = num_po + 1
end
FA = make_array(num_pts(0)+1,num_po)
for i=0,num_po-1 do begin
        u_read,unit,px
	FA(*,i) = px
end

	u_read,unit,labels
	u_read,unit,x
	u_read,unit,y
	u_read,unit,n
	if n(0) gt 0 then begin
	u_read,unit,ze
	end
END


;
; if seq_no + 1 = id will plot
;
PRO scan_read,unit,seq_no,id,pv

COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON env_field_block,env_field
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

if n_params() lt 2 then begin
        w_warningtext,'usage:   scan_read, unit, seq_no, id'
        return
        end

if EOF(unit) eq 1 then begin
	w_warningtext,'end of file reached ! '
	POINT_LUN,w_viewscan_id.unit,0
	id = 0
	return
	end

; 
next_seq_no = seq_no + 1
	u_read,unit,version,ERRCODE
	if ERRCODE eq -99 or version(0) ne scanData.version then begin
		if scanData.XDR then $
                s0 = 'Error: data version is in native binary form.' else $
                s0 = 'Error: data version is in XDR binary form.'
	 
		if scanData.XDR then $
                s1 = '       Current setting is in XDR binary form' else $
                s1 = '       Current setting is in native binary form' 
                st = ['Error: wrong binary form selected.',s0,s1]
                w_warningtext,st
                return
                end

	u_read,unit,pv
	u_read,unit,num_pts
	u_read,unit,id_def
	u_read,unit,x_dpt

	realtime_id.def = id_def
	scanData.p_def = id_def(0:3)
	scanData.pa = make_array(4000,4,/double)
	scanData.da = FLTARR(4000,15)

	num_pts = fix(num_pts(0))

for i=0,3 do begin
        if realtime_id.def(i) gt 0 then begin
	u_read,unit,px
	scanData.pa(0:num_pts,i) = px
        end
end

for i=0,14 do begin
        if realtime_id.def(4+i) gt 0 then begin
	u_read,unit,px
	scanData.da(0:num_pts,i) = px
        end
end


; read 1D-plot labels

	labels = make_array(30,57,/byte)
	u_read,unit,labels
	read_desc_engu,labels 
	
x = make_array(6,/string,value=string(replicate(32b,60)))

y = make_array(8,/float)

	u_read,unit,x,errcode
	u_read,unit,oy,errcode
if errcode lt 0 then goto,TRY_PLOT
y=oy
no = n_elements(oy)
if n_elements(y) gt 1 then begin
        scanData.refno = fix(y(1))
        scanData.y_seqno = fix(y(2))
        scanData.scanno_2d = fix(y(3))
	if no ge 5 then scanData.y_scan = fix(y(4))
	if no eq 8 then begin
		scanData.y_value = y(7)
		scanData.dataversion = scanData.release
		endif else scanData.dataversion = ''
        endif else scanData.refno = fix(y(0)) + 1

w_plotspec_id.seqno = fix(y(0))
w_viewscan_id.seqno = fix(y(0))

        for i=0,3 do begin
        if strtrim(x_names(i),2) ne '' then $
        w_plotspec_id.goto_pv(i) = strmid(x_names(i),0,strpos(x_names(i),'.'))
        end

	u_read,unit,n,errcode
if errcode lt 0 then goto,TRY_PLOT

env_field.exist = 0 
if n(0) gt 0 then begin 
no=n(0)
env_field.exist = no
env_field.no = no        ; actual no of env written on data file
ze = make_array(no,/string,value=string(replicate(32b,110)))

	u_read,unit,ze

	for i=0,no-1 do begin
	env_field.pvnames(i)=strmid(ze(i),0,30)
	env_field.values(i)=strmid(ze(i),30,40)
	env_field.descs(i)=strmid(ze(i),70,40)
	end

	env_field.pvnames = strtrim(env_field.pvnames,2)

end

;
; external file name is used for display the data
; the internal file name may be not consistant with the external file name
; due to the user copy the trashcan to a new file without use file->copy
;
TRY_PLOT:

temp_name = w_plotspec_array(3)
s1 = n_elements(x)
for i=0,s1-1  do w_plotspec_array(i)=x(i) 
w_plotspec_array(3) = temp_name

;field_value = make_array(field_max,/string,value=string(replicate(32b,40)))
;for i=0,field_max-1 do field_value(i)=strtrim(ze(i+env_field.noenv))

   ;populate read data into global arrays

scanData.pv = string(pv(0))
scanData.act_npts = num_pts + 1

if id eq next_seq_no or next_seq_no le 0 then begin
	if scanData.debug eq 1 then $
	print,'Scan # ',seq_no, ' accessed.'
	scanData.scanno = seq_no
	setPlotLabels
	UPDATE_PLOT, scanData.lastPlot
	id = next_seq_no
	end

scanData.readin_npts=scanData.act_npts

END

PRO catch1d_releaseLockTurnonAutosave
COMMON CATCH1D_COM, widget_ids, scanData
; forcely remove the filelock

    if scanData.pid eq 1 then begin
	st = ['Are you ABSOLUTELY sure you want to override the lockfile?', $
		"Unless you are 100% sure don't select Yes"]

	res = dialog_message(st,/default_no,/question)
	if res eq 'No' then return
	
       spawn,[!os.rm,'-f',scanData.filelock],/noshell
       found = findfile(scanData.filelock)
	if found(0) eq '' then begin
	catch1d_grabLock,scanData.trashcan
	catch1d_turnonAutosave
	scanData.nosave = 0
	endif else begin
		scanData.pid = 2
		res = dialog_message('You have no write permission !',/info)
	end
    end
END

PRO catch1d_grabLock,filename
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved

	scanData.filelock = filename + '.lock'

	flock = findfile(scanData.filelock)
	if flock(0) eq '' then begin
		scanData.pid = 0
	CATCH, error_status
	if error_status ne 0 then begin
;		print,!err_string,!err
		scanData.pid = 2       ; fail to create
		scanData.nosave = 1
		close,1
		return
	end
		openw,1,scanData.filelock
		printf,1,systime(0)
		printf,1,'USER='+getenv('USER')+',PID='+ $
			strtrim(scanData.pid,2)+',FILE='+filename
		close,1
		spawn,!os.chmod + ' 777 '+scanData.filelock

		if scanData.nosave eq 1 then catch1d_turnonAutosave
		scanData.nosave = 0

	endif else begin

		scanData.pid = 1     ; lock file exists

	end

END

; add the support of index file this should improve the
; perfomance of data catcher
;

PRO catch1d_readFileIndex,filename,errcode
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

; check whether filename exists

errcode=-99
fd = findfile(filename)
IF fd(0) NE '' THEN BEGIN
	
	catch1d_grabLock,filename

	indexfile = filename+'.index'

found = findfile(indexfile)
if found(0) ne '' then begin
errcode = 0
	u_openr,unit,indexfile
	u_read,unit,name
	u_read,unit,fsize
	u_read,unit,maxno
	u_read,unit,array
	u_close,unit

	openr,1,filename
	status = FSTAT(1)
	close,1
	
	if status.size eq fsize(0) then begin
	w_viewscan_id.size = fsize(0)
	w_viewscan_id.maxno = maxno(0)
;	w_viewscan_id.fptr(0:w_viewscan_id.maxno) = array
	w_viewscan_id.fptr = array
	end
;	print,'***Read Index File: ',indexfile
end
ENDIF ELSE BEGIN
	w_warningtext,'Warning: file "' + filename + '" not found, it will be created.'
END 
END


PRO catch1d_writeFileIndex,filename
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

; check file existence

found = findfile(filename)
if found(0) eq '' then return
	openr,1,filename
	status = FSTAT(1)
	close,1
	
	if filename eq w_viewscan_id.file then begin

	if w_viewscan_id.maxno gt 0 then begin

	indexfile = w_viewscan_id.file + '.index'
	CATCH,error_status
	if error_status lt 0 then return
	openw,unit,indexfile,/GET_LUN

	array = w_viewscan_id.fptr(0:w_viewscan_id.maxno)
	array(w_viewscan_id.maxno) = status.size

	u_write,unit,status.name
	u_write,unit,status.size
	u_write,unit,w_viewscan_id.maxno
	u_write,unit,array
	u_close,unit

;print,'***File ',indexfile,' updated.'
	end
	end
END


PRO catch1d_process2Ddata
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

; update the seq no

        w_plotspec_id.seqno = w_plotspec_id.seqno + 1
if  w_plotspec_id.autosave eq 0  then $
        w_viewscan_id.maxno = w_plotspec_id.seqno
        scanData.scanno = w_plotspec_id.seqno

	if scanData.debug eq 1 then $
	print,'1D SCAN #',scanData.scanno

; update the 2D-scan data

        if scanData.y_scan eq 1 then begin

        ; assign 2D data array

        catch1d_fill_2D_data

	empty

; process the record at the end of scan

;       id = caPutArray(scanData.y_pv+'.PROC',1)
	if strlen(strtrim(scanData.y_handshake,2)) gt 0 then $
        id = caPutArray(scanData.y_handshake,1)

        scanData.y_seqno = scanData.y_seqno + 1

if scanData.debug eq 1 then $
print,'2D SCAN #',scanData.scanno_2d, '  Y SCAN #',scanData.y_seqno,scanData.y_value
        end

END


PRO scan_mode_write, unit
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON env_field_block,env_field
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON LABEL_BLOCK,x_names,y_names,x_descs,y_descs,x_engus,y_engus
  COMMON CATCH1D_2D_COM, data_2d


if n_params() ne 1 then begin
	print,'usage:   scan_mode_write, unit'
 	return
	end

        env_after_scan

; check for any other tools wrote to the same file


   scanData.readin_npts = scanData.act_npts

   num_pts = 1 > (scanData.act_npts-1)
   ;extract valid data from global arrays

	u_write,unit,scanData.version
	u_write,unit,scanData.pv
	u_write,unit,num_pts
	u_write,unit,realtime_id.def
	u_write,unit,scanData.x_dpt

; determine the position array to be saved in FA

for i=0,3 do begin
	if realtime_id.def(i) gt 0 then begin
	px = scanData.pa(0:num_pts,i)
	u_write,unit,px
	end
end

; determine the detector array to be saved in FB

for i=0,14 do begin
	if realtime_id.def(4+i) gt 0 then begin
	px = scanData.da(0:num_pts,i)
	u_write,unit,px
	end
end

; save the names, descs, engus in label  array
; byte array for names,descs,engus

labels = make_array(30,57,/byte)
for i=0,3 do begin
	len = strlen(x_names(i))
	if len gt 0 then labels(0:len-1,i) = byte(x_names(i))
	len = strlen(x_descs(i))
	if len gt 0 then labels(0:len-1,i+19) = byte(x_descs(i))
	len = strlen(x_engus(i))
	if len gt 0 then labels(0:len-1,i+38) = byte(x_engus(i))
end
for i=0,14 do begin
	len = strlen(y_names(i))
	if len gt 0 then labels(0:len-1,i+4) = byte(y_names(i))
	len = strlen(y_descs(i))
	if len gt 0 then labels(0:len-1,i+4+19) = byte(y_descs(i))
	len = strlen(y_engus(i))
	if len gt 0 then labels(0:len-1,i+4+38) = byte(y_engus(i))
end

	u_write,unit,labels

; save the plot description in x array

x = make_array(6,/string,value=string(replicate(32b,60)))
for i=0,5 do begin
len = strlen(w_plotspec_array(i))
x(i) = w_plotspec_array(i)
if len lt 60 then x(i)=  x(i) + string(replicate(32b,60-len))
end
	u_write,unit,x

; save the seqno  in y array

y = make_array(8,/float)
y(0) = w_plotspec_id.seqno
y(1) = scanData.startno
y(2) = scanData.y_seqno
y(3) = scanData.scanno_2d
y(4) = scanData.y_scan
y(5) = scanData.y_req_npts
y(6) = scanData.y_act_npts
;if scanData.y_scan eq 1 then begin
;	scanData.y_value=0.
;	ln = cagetArray(scanData.y_pv+'.P1CV',pd,/float)
;	if ln eq 0 then scanData.y_value = pd(0)
;end
y(7) = scanData.y_value
scanData.y_array(scanData.y_seqno) = scanData.y_value

	u_write,unit,y


; save the scan record field description  now is saved in the 
; ze  array


; save the environment values or changed values in the ze array
; get env fields

n = env_field.no
; check for the case only if scanno >= startno for the value changed 

if scanData.scanno ge scanData.startno then begin

if n gt 0 then begin ;======

	find_envs_changed,diff
	ij  = n_elements(diff)

	u_write,unit,ij


	if ij gt 0 then begin
	s0 = string(replicate(32b,110))
	ze = make_array(ij,/string,value=string(replicate(32b,110)))

	for i=0,ij-1  do begin
	st = s0
	strput,st,env_field.pvnames(diff(i)),0
	strput,st,env_field.values(diff(i)),30
	strput,st,env_field.descs(diff(i)),70
	len = strlen(st)
	ze(i) = st
	if len lt 110 then ze(i)=  ze(i) + string(replicate(32b,110-len))
	end
	u_write,unit,ze
	end
	
endif else begin 
	u_write,unit,n  
end ;======
end

if scanData.scanno lt scanData.startno then begin 

	u_write,unit,n

	if n gt 0 then begin
	ze = make_array(n,/string,value=string(replicate(32b,110)))
	for i=0,n-1  do begin
	st = string(replicate(32b,110))
	strput,st,env_field.pvnames(i),0
	strput,st,env_field.values(i),30
	strput,st,env_field.descs(i),70
	len = strlen(st)
	ze(i) = st
	if len lt 110 then ze(i)=  ze(i) + string(replicate(32b,110-len))
	end
	u_write,unit,ze
	end
end

; update the seq no
; update the 2D-scan data
; process the record at the end of scan

	catch1d_process2Ddata

END

PRO catch1d_fill_2D_data
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
  COMMON CATCH1D_2D_COM, data_2d

; assign 2D data array

ip = 0
npts = scanData.act_npts-1
y_seqno = scanData.y_seqno
px = make_array(npts,/float)
for i=0,14 do begin
	if realtime_id.def(4+i) gt 0 then begin
	px = scanData.da(0:npts,i)
	CASE i OF
	0: data_2d.d1(0,0:npts,y_seqno) = px
	1: data_2d.d2(0,0:npts,y_seqno) = px
	2: data_2d.d3(0,0:npts,y_seqno) = px
	3: data_2d.d4(0,0:npts,y_seqno) = px
	4: data_2d.d5(0,0:npts,y_seqno) = px
	5: data_2d.d6(0,0:npts,y_seqno) = px
	6: data_2d.d7(0,0:npts,y_seqno) = px
	7: data_2d.d8(0,0:npts,y_seqno) = px
		8: data_2d.d9(0,0:npts,y_seqno) = px
	9: data_2d.d10(0,0:npts,y_seqno) = px
	10: data_2d.d11(0,0:npts,y_seqno) = px
	11: data_2d.d12(0,0:npts,y_seqno) = px
	12: data_2d.d13(0,0:npts,y_seqno) = px
	13: data_2d.d14(0,0:npts,y_seqno) = px
	14: data_2d.d15(0,0:npts,y_seqno) = px
	ENDCASE
	ip = ip + 1
	end
end

	if scanData.eof2d eq 0 then scan_mode_write_image

	if scanData.image gt 2 then catch1d_win_2D_update2 else $
		catch1d_win_2D_update1

END

PRO catch1d_win_2D_update1,y_seqno
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
  COMMON CATCH1D_2D_COM, data_2d

; update the image plot

;loadct, 39

npts = scanData.act_npts-1
if n_params() eq 0 then y_seqno = scanData.y_seqno
if y_seqno lt 0 then return
	width = scanData.image_width * scanData.image
	height = scanData.image_height * scanData.image

	old_win = !D.window
	new_win = old_win - 1 
	if y_seqno eq 0 then begin
		window,new_win, xsize = 8*width, ysize=2*height, title='2D_Images'
		for i=0,14 do begin
		xi=(i mod 8)*width+width/2 - 5
		yi=height/2+(15-i)/8*height
		xyouts, xi,yi,'D'+strtrim(i+1,2),/device
		end
        plots,[0,8*width],[height,height],/device
        for i=1,7 do plots,[i*width,i*width],[0,2*height],/device

	end

CATCH,error_status
if error_status lt 0 then begin
	print,!err_string,!err
	if error_status ne 0 then begin

	window,new_win, xsize = 8*width, ysize=2*height, title='2D_Images'
		for i=0,14 do begin
		xi=(i mod 8)*width+width/2 - 5
		yi=height/2+(15-i)/8*height
		xyouts, xi,yi,'D'+strtrim(i+1,2),/device
		end
        plots,[0,8*width],[height,height],/device
        for i=1,7 do plots,[i*width,i*width],[0,2*height],/device

        end
end

	wset,new_win

;	erase
	for sel=0,14 do begin
	if realtime_id.def(4+sel) gt 0 then begin
	CASE sel OF
	0: data_2d.image = data_2d.d1(0,0:npts,0:y_seqno)
	1: data_2d.image = data_2d.d2(0,0:npts,0:y_seqno)
	2: data_2d.image = data_2d.d3(0,0:npts,0:y_seqno)
	3: data_2d.image = data_2d.d4(0,0:npts,0:y_seqno)
	4: data_2d.image = data_2d.d5(0,0:npts,0:y_seqno)
	5: data_2d.image = data_2d.d6(0,0:npts,0:y_seqno)
	6: data_2d.image = data_2d.d7(0,0:npts,0:y_seqno)
	7: data_2d.image = data_2d.d8(0,0:npts,0:y_seqno)
	8: data_2d.image = data_2d.d9(0,0:npts,0:y_seqno)
	9: data_2d.image = data_2d.d10(0,0:npts,0:y_seqno)
	10: data_2d.image = data_2d.d11(0,0:npts,0:y_seqno)
	11: data_2d.image = data_2d.d12(0,0:npts,0:y_seqno)
	12: data_2d.image = data_2d.d13(0,0:npts,0:y_seqno)
	13: data_2d.image = data_2d.d14(0,0:npts,0:y_seqno)
	14: data_2d.image = data_2d.d15(0,0:npts,0:y_seqno)
	ENDCASE
	temp = congrid(data_2d.image, width, height)
	TVSCL, temp, sel
	end
	end

	wset,old_win
END

PRO catch1d_checkImageFile,filename,errcode
;  return -99 error in filename
;  return -1 native binary format 
;  return 0 XDR format
;  return 1 not exist yet

        found = findfile(filename)
        if found(0) ne '' then begin
 
	u_openr,unit,filename,/XDR
	u_read,unit,pvs,errcode
	u_close,unit	
	if errcode eq -99 then begin
		u_openr,unit,filename
		u_read,unit,pvs,err
		u_close,unit
		if err eq 0 then errcode = -1
	endif else errcode=0
        endif else errcode=1

	return
END

PRO scan_mode_write_image
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON LABEL_BLOCK,x_names,y_names,x_descs,y_descs,x_engus,y_engus
  COMMON CATCH1D_2D_COM, data_2d

	npts = scanData.req_npts-1
	y_seqno = scanData.y_seqno - 1
	if y_seqno lt 0 then return

filename = scanData.path + strtrim(w_plotspec_array(3),2)+'.image'


CATCH,error_status
if error_status eq -171 then begin
	w_warningtext,['scan_mode_write_image',!err_string]
	return
        end

	u_openw,unit,filename,/XDR,/APPEND
	u_rewind,unit
	point_lun,unit,scanData.im_eof
	if scanData.debug then $
	print,'unit=',unit,scanData.y_seqno,scanData.im_eof

;	u_openw,unit,filename,/APPEND,/XDR 
;status = FSTAT(unit)
;point_lun,unit,status.size

for i=0,14 do begin
	if  realtime_id.def(4+i) gt 0 then begin
	CASE i OF
	0: data_2d.image = data_2d.d1(0,0:npts,0:y_seqno)
	1: data_2d.image = data_2d.d2(0,0:npts,0:y_seqno)
	2: data_2d.image = data_2d.d3(0,0:npts,0:y_seqno)
	3: data_2d.image = data_2d.d4(0,0:npts,0:y_seqno)
	4: data_2d.image = data_2d.d5(0,0:npts,0:y_seqno)
	5: data_2d.image = data_2d.d6(0,0:npts,0:y_seqno)
	6: data_2d.image = data_2d.d7(0,0:npts,0:y_seqno)
	7: data_2d.image = data_2d.d8(0,0:npts,0:y_seqno)
	8: data_2d.image = data_2d.d9(0,0:npts,0:y_seqno)
	9: data_2d.image = data_2d.d10(0,0:npts,0:y_seqno)
	10: data_2d.image = data_2d.d11(0,0:npts,0:y_seqno)
	11: data_2d.image = data_2d.d12(0,0:npts,0:y_seqno)
	12: data_2d.image = data_2d.d13(0,0:npts,0:y_seqno)
	13: data_2d.image = data_2d.d14(0,0:npts,0:y_seqno)
	14: data_2d.image = data_2d.d15(0,0:npts,0:y_seqno)
	ENDCASE

	; write the detector  number i, width, height

	pvs = make_array(60,6,/byte)
	pvs(0,0) = byte(scanData.pv)
	pvs(0,1) = byte(scanData.y_pv)
	temp = w_plotspec_array(3)
	if strlen(temp) gt 60 then temp = strmid(temp,0,60)
	pvs(0,2) = byte(temp)

;  3  for scan1  positioner desc
	pv_desc = scanData.x_desc
	len = strlen(pv_desc)
	if len gt 0 then pvs(0:len-1,3) = byte(pv_desc)

;  4  for scan2  positioner desc
	pv2_desc = scanData.y_desc
	len = strlen(pv2_desc)
	if len gt 0 then pvs(0:len-1,4) = byte(pv2_desc)

;  5  for detector desc
        len = strlen(y_descs(i))
        if len gt 0 then pvs(0:len-1,5) = byte(y_descs(i)) else $
	pvs(0,5) = byte(y_names(i))

	u_write,unit,pvs

	nos = [scanData.scanno, scanData.req_npts, scanData.y_seqno, i, $
		scanData.scanno_2d, scanData.y_req_npts]

	u_write,unit,nos

	; write x and y positioner array
	
	x = scanData.pa(0:npts,0)
	if scanData.act_npts gt 0 then begin
;	id = cagetarray(scanData.pv+'.P1RA',x,max=scanData.req_npts) 
	if scanData.act_npts lt scanData.req_npts then begin
	x = indgen(scanData.req_npts)  ; reset if aborted
	po = realtime_id.xsetup
	dx = (po(1)-po(0)) / npts
	if po(4) eq 0 then x = po(0) + dx * x
	if po(4) eq 1 then x = po(5) + po(0) + dx * x
	end
	end

	u_write,unit,x

; new scan 3.13 software this wont work
;	id = cagetarray(scanData.y_pv+'.P1RA',y,max=scanData.y_seqno)
	y = scanData.y_array(0:y_seqno)
	u_write,unit,y

	; write image

	u_write,unit,data_2d.image

if scanData.debug eq 1 then begin
print,'2D Scan # =',scanData.scanno_2d, ' Scan # =',scanData.scanno
print,'    width =',scanData.req_npts, ' height =', $
	scanData.y_seqno, ' Detector # =',i
help,x,y,data_2d.image
end
	end
end
	u_close,unit
	free_lun,unit

END


PRO test_read_image

	if scanData.XDR then u_openr,unit,'catch1d.trashcan.image.xdr',/XDR else $
	u_openr,unit,'catch1d.trashcan.image'

	; read pv names

	u_read,unit,pvs
	print,string(pvs)

	; read seqno, dims

	u_read,unit,x
	print,x
	end_scanno = x(0)
	width = x(1)
	height= x(2)
	detector = x(3)

	; read x, y postion array
	u_read,unit,px
	u_read,unit,py

	; read image

	u_read,unit,image
	print,image(0:width-1,height-1)

	show3,image
END


;
; update only selected 2D detector
;
PRO catch1d_win_2D_update2,y_seqno
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
  COMMON CATCH1D_2D_COM, data_2d

; update the image plot

;loadct, 39
npts = scanData.act_npts-1
if n_params() eq 0 then y_seqno = scanData.y_seqno
if y_seqno lt 0 then return

	old_win = !D.window
	new_win = old_win - 1 
	if y_seqno eq 0 then window,new_win, $
		xsize = 200, ysize=200, $
		title='2D_Image'

; -481 window not round in 5.4
CATCH,error_status
if error_status lt 0 then begin
	if error_status ne 0 then begin
	window,new_win, $
		xsize = 200, ysize=200, $
                title='2D_Image'
        end
end

	wset,new_win

	sel = scanData.image - 3

	CASE sel OF
	0: data_2d.image = data_2d.d1(0,0:npts,0:y_seqno)
	1: data_2d.image = data_2d.d2(0,0:npts,0:y_seqno)
	2: data_2d.image = data_2d.d3(0,0:npts,0:y_seqno)
	3: data_2d.image = data_2d.d4(0,0:npts,0:y_seqno)
	4: data_2d.image = data_2d.d5(0,0:npts,0:y_seqno)
	5: data_2d.image = data_2d.d6(0,0:npts,0:y_seqno)
	6: data_2d.image = data_2d.d7(0,0:npts,0:y_seqno)
	7: data_2d.image = data_2d.d8(0,0:npts,0:y_seqno)
	8: data_2d.image = data_2d.d9(0,0:npts,0:y_seqno)
	9: data_2d.image = data_2d.d10(0,0:npts,0:y_seqno)
	10: data_2d.image = data_2d.d11(0,0:npts,0:y_seqno)
	11: data_2d.image = data_2d.d12(0,0:npts,0:y_seqno)
	12: data_2d.image = data_2d.d13(0,0:npts,0:y_seqno)
	13: data_2d.image = data_2d.d14(0,0:npts,0:y_seqno)
	14: data_2d.image = data_2d.d15(0,0:npts,0:y_seqno)
	ENDCASE
	
	TVSCL,congrid(data_2d.image, 200, 200)

	wset,old_win
END


PRO make_2d_data,data_2d,npts,xdim,ydim
no = n_elements(npts)
if no eq 0 then begin
print,'Usage:   make_2d_data,data,npts,xdim,ydim
print,'
print,'	INPUT:  
print,' 	npts(15)     - 15 detectors data array size
print,'		xdim	     - x direction scan acquired data point 
print,'		ydim	     - y direction scan acquired data point 
print,' OUTPUT:
print,'         data	     - a data structure with following dims 
print,'				if npts(i) eq 0 then an int 0 is returned
print,'                         for      data.di=0
print,'
print,'         data.d1(npts(0),xdim,ydim)
print,'         data.d2(npts(1),xdim,ydim)
print,'         .
print,'         .
print,'         .
print,'         data.d15(npts(14),xdim,ydim)
print,'
end

d1=0
d2=0
d3=0
d4=0
d5=0
d6=0
d7=0
d8=0
d9=0
d10=0
d11=0
d12=0
d13=0
d14=0
d15=0
	for i=0,no-1 do begin
	CASE i OF
	0: d1 = make_array(npts(i),xdim,ydim)
	1: d2 = make_array(npts(i),xdim,ydim)
	2: d3 = make_array(npts(i),xdim,ydim)
	3: d4 = make_array(npts(i),xdim,ydim)
	4: d5 = make_array(npts(i),xdim,ydim)
	5: d6 = make_array(npts(i),xdim,ydim)
	6: d7 = make_array(npts(i),xdim,ydim)
	7: d8 = make_array(npts(i),xdim,ydim)
	8: d9 = make_array(npts(i),xdim,ydim)
	9: d10 = make_array(npts(i),xdim,ydim)
	10: d11 = make_array(npts(i),xdim,ydim)
	11: d12 = make_array(npts(i),xdim,ydim)
	12: d13 = make_array(npts(i),xdim,ydim)
	13: d14 = make_array(npts(i),xdim,ydim)
	14: d15 = make_array(npts(i),xdim,ydim)
	ENDCASE
	end
data_2d = { $
	image : make_array(xdim,ydim), $
	d1 : d1, $ 
	d2 : d2, $ 
	d3 : d3, $ 
	d4 : d4, $ 
	d5 : d5, $ 
	d6 : d6, $ 
	d7 : d7, $ 
	d8 : d8, $ 
	d9 : d9, $ 
	d10 : d10, $ 
	d11 : d11, $ 
	d12 : d12, $ 
	d13 : d13, $ 
	d14 : d14, $ 
	d15 : d15 $ 
	}
END

;
; Auto Save File For ./catch1d.pro
;
;  Tue Oct  4 11:20:04 CDT 1994
;

@u_read.pro
@PS_open.pro
@cw_term.pro

PRO catch1d_refreshScreen,pv
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

; plot the very last scan at the startup time

w_plotspec_id.seqno = w_viewscan_id.maxno

pv=''
if w_plotspec_id.seqno gt 0 then begin
w_plotspec_id.mode = 1
        if scanData.XDR then openr,unit,scanData.trashcan,/GET_LUN,/XDR else $
        openr,unit,scanData.trashcan,/GET_LUN
        i1= w_plotspec_id.seqno
        point_lun, unit, w_viewscan_id.fptr(i1-1)
        i2 = i1+1
        scan_read, unit, i1, i2, pv
        free_lun,unit
        w_plotspec_id.seqno = i1
w_plotspec_id.mode = 0

end

END

PRO catch1d_viewmode, Event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

;	cadebug,-1
	if w_plotspec_id.opened ne 0 then free_lun,w_plotspec_id.opened
	w_plotspec_id.opened = 0

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
	id = check_data_version(FNAME)
	if id ne 0 then begin
		w_plotspec_id.mode = 0
		return
		end

	if scanData.XDR then openr,unit,FNAME,/GET_LUN,/XDR else $
	openr,unit,FNAME,/GET_LUN

	set_sensitive_off
	WIDGET_CONTROL,widget_ids.rept_base,SENSITIVE=1
	WIDGET_CONTROL,widget_ids.viewdata,SENSITIVE=1
;
; destroy the old w_plotspec window
;
	close_plotspec
	w_viewscan, unit, GROUP=Event.top

END

PRO PDMENU_VDATA_Event, Event
  COMMON CATCH1D_COM, widget_ids, scanData

  CASE Event.Value OF 
  'ViewData.1D ...': BEGIN
	catch1d_viewmode, Event
 	END
  'ViewData.2D ...': BEGIN
	if scanData.XDR then $
	view2d,/CA, GROUP= event.top, file=scanData.trashcan+'.image', /XDR else $
	view2d,/CA, GROUP= event.top, file=scanData.trashcan+'.image'
 	END
  'ViewData.1D Overlay ...': BEGIN
	if scanData.XDR then $
        view1d_overlay, scanData.trashcan,/XDR, GROUP=event.top else $
        view1d_overlay, scanData.trashcan, GROUP=event.top 
 	END
  ENDCASE
END

PRO HELPMENU_Event, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
  COMMON CATCH1D_COM, widget_ids, scanData

  docp = OS_SYSTEM.file_sep + 'doc' + OS_SYSTEM.file_sep

  CASE Event.Value OF 
  'Help.Version ...': BEGIN
	st = caVersion()
	st = [st,'','Scan Data Catcher Version :  '+ scanData.release ]
	w_warningtext,st
 	END
  'Help.Release Note ...': BEGIN
	private = getenv('EPICS_EXTENSIONS_PVT') + docp + 'catcher.README'
	found = findfile(private)
	if found(0) ne '' then xdisplayfile,found(0),GROUP=Event.top else begin
	str = getenv('EPICS_EXTENSIONS')+docp + 'catcher.README'
	xdisplayfile,str, GROUP=Event.top
	end
        END
  'Help.Help ...': BEGIN
	private = getenv('EPICS_EXTENSIONS_PVT') + docp + 'catcher_help.txt'
	found = findfile(private)
	if found(0) ne '' then xdisplayfile,found(0),GROUP=Event.top else begin
	str = getenv('EPICS_EXTENSIONS')+docp+'catcher_help.txt'
	xdisplayfile, str, GROUP= event.top
	end
 	END
  'Help.Catcher Html ...': BEGIN
	if scanData.option eq 0 then begin
	str = 'file:'+getenv('EPICS_EXTENSIONS')+docp+'catcher.html'
	spawn,['netscape',str],/noshell
	endif else w_warningtext,'Sorry: This option is only accessible by the View Only mode.'
 	END
  'Help.ezcaIDL Html ...': BEGIN
	if scanData.option eq 0 then begin
	str = 'file:'+getenv('EPICS_EXTENSIONS')+docp+'ezcaIDLRef.html'
	spawn,['netscape',str],/noshell
	endif else w_warningtext,'Sorry: This option is only accessible by the View Only mode.'
 	END

  ENDCASE
END



PRO PRINTMENU_Event, Event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON font_block, text_font, graf_font, ps_font

  CASE Event.Value OF 

  'Print.Plot': BEGIN
	if scanData.lastPlot lt 0 then return
	scanData.act_npts = scanData.readin_npts
    	PS_open,'catch1d.ps'
    	UPDATE_PLOT, scanData.lastPlot
    	PS_close
    	PS_print,'catch1d.ps'
 	END
  'Print.Report ...': BEGIN
	view1d_summary_setup,GROUP=Event.top  		; pick the range

 	END
  ENDCASE
END

PRO ZOOMMENU_Event, Event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON user_scale_block,user_scale_ids

if scanData.lastPlot ge 0 then begin 

if XRegistered('user_scale') ne 0 then begin
        WIDGET_CONTROL,user_scale_ids.base,/DESTROY
        end

scanData.act_npts = scanData.readin_npts

  CASE Event.Value OF 

  'Zoom.Zoom To Box': BEGIN
	zoom_to_box
 	END
  'Zoom.Zoom In/Out': BEGIN
	zoom_in_out
 	END
  'Zoom.Calc Slopes': BEGIN
	draw_dragLine
 	END
  'Zoom.Zoom Off (AutoScale)': BEGIN
	scanData.lastPlot = 1
	UPDATE_PLOT, 1
 	END
  'Zoom.User Scale ...': BEGIN
	scanData.lastPlot = 0
	user_scale,GROUP=event.top
;	UPDATE_PLOT, 0
 	END
  ENDCASE
end
END

PRO STATISTICMENU_Event, Event

  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_statistic_block,w_statistic_ids
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

   WIDGET_CONTROL, widget_ids.wf_select, GET_VALUE = wf_sel

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

  'Statistic.FWHM on Y.All...': BEGIN
        w_plotspec_id.statistic = 5
	num_pts = scanData.act_npts-1
	VX=scanData.pa(0:num_pts,0)
	for i=0,14 do begin
	IF (realtime_id.def(4+i) gt 0) THEN begin
	VY=scanData.da(0:num_pts,i)
	title='FWHM of Detector '+strtrim(i+1,2) +'    SCAN # '+strtrim(scanData.scanno,2)
	statistic_1d,VX,VY,c_mass,x_peak,y_peak,y_hpeak,fwhm,xl,xr,/plot, $
		title=title,group=Event.top
	end
	end
    END
  'Statistic.FWHM on Y.One...': BEGIN
        w_plotspec_id.statistic = 5
	num_pts = scanData.act_npts-1
	VX=scanData.pa(0:num_pts,0)
	for i=0,14 do begin
	IF (wf_sel(i) EQ 1 and realtime_id.def(4+i) gt 0) THEN begin
	VY=scanData.da(0:num_pts,i)
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
	VX=scanData.pa(0:num_pts,0)
	for i=0,14 do begin
	IF (realtime_id.def(4+i) gt 0) THEN begin
	VY=scanData.da(0:num_pts,i)
	VY = slope(VX,VY)
	title='FWHM of DY/DX of Detector '+strtrim(i+1,2) +'    SCAN # '+strtrim(scanData.scanno,2)
	statistic_1d,VX,VY,c_mass,x_peak,y_peak,y_hpeak,fwhm,xl,xr,/plot, $
		title=title,group=Event.top
	end
	end
    END
  'Statistic.FWHM on DY/DX.One...': BEGIN
        w_plotspec_id.statistic = 6
	num_pts = scanData.act_npts-1
	VX=scanData.pa(0:num_pts,0)
	for i=0,14 do begin
	IF (wf_sel(i) EQ 1 and realtime_id.def(4+i) gt 0) THEN begin
	VY=scanData.da(0:num_pts,i)
	VY = slope(VX,VY)
	title='FWHM of DY/DX of Detector '+strtrim(i+1,2) +'    SCAN # '+strtrim(scanData.scanno,2)
	statistic_1d,VX,VY,c_mass,x_peak,y_peak,y_hpeak,fwhm,xl,xr,/plot, $
		report='fwhm.rpt',title=title,group=Event.top
	return
	end
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

END

PRO FITTINGMENU_Event, Event
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

if scanData.act_npts lt 2 then begin
        w_warningtext,['No data available yet','', $
		 'You have to load 1D data in first.']
	return
end

x = scanData.pa(0:scanData.act_npts-1,w_plotspec_id.xcord)
y = make_array(scanData.act_npts,15)
y(*,*) = scanData.da(0:scanData.act_npts-1,0:14)
   WIDGET_CONTROL, widget_ids.wf_select, GET_VALUE = wf_sel
	for i=0,14 do begin
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

PRO catch1d_newfile
COMMON SYSTEM_BLOCK,OS_SYSTEM
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

;
; destroy the old w_plotspec window
;
	close_plotspec

filenamepath,scanData.trashcan,old_file,old_path

if old_file ne '' then begin
	fd = findfile(scanData.trashcan+'.index')
	if fd(0) eq '' then $
	catch1d_writeFileIndex,scanData.trashcan
end

        FNAME = ''
;	if scanData.XDR then FNAME='*.xdr'
	filename=''

; check for bad directory -296

        CATCH, error_status

        if error_status ne 0 then begin ;  eq -296 then begin
        w_warningtext,[!err_string + string(!err), $
		'       Bad data directory !!!', $
		'       Quit data catcher, and then modify data path or delete the', $
                '       configuration file and restart it.']
        return
        end

        F = DIALOG_PICKFILE(TITLE='New ...',/WRITE,FILE=filename,PATH=old_path,GET_PATH=P,FILTER=FNAME)

        IF F eq '' THEN return
 
	if !d.name eq 'X' then begin
        IF (STRMID(F,0,1) EQ OS_SYSTEM.file_sep) THEN $
                FNAME = F $
        ELSE $
                FNAME = P+F
	if STRMID(FNAME,0,1) eq '~' then filename_expand,FNAME 
	endif else FNAME = F

	found=findfile(FNAME)

if found(0) ne '' then begin
	st = ['Error:','     '+FNAME+' - File already exists!']
	w_warningtext,st
	return
	end

	catch1d_newCheck,FNAME,old_path

END

PRO catch1d_append,new=new
COMMON SYSTEM_BLOCK,OS_SYSTEM
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

;
; destroy the old w_plotspec window
;
	close_plotspec

	WIDGET_CONTROL,widget_ids.summary_base,/DESTROY,BAD_ID=bad

filenamepath,scanData.trashcan,old_file,old_path

if old_file ne '' then begin
	fd = findfile(scanData.trashcan+'.index')
	if fd(0) eq '' then $
	catch1d_writeFileIndex,scanData.trashcan
end

        FNAME = ''

filename = 'catch1d.trashcan.xdr'

if keyword_set(new) then begin
	filename=''
	FNAME = ''
	end

        if w_plotspec_id.opened ne 0 then free_lun,w_plotspec_id.opened
        w_plotspec_id.opened = 0

; check for bad directory -296

        CATCH, error_status

        if error_status ne 0 then begin     ; eq -296 then begin
        w_warningtext,[!err_string + string(!err) ,'       Bad data directory !!!', $
		'       Please try to use the  File->New ...  option.']
        return
        end


        F = DIALOG_PICKFILE(TITLE='Open ...',/WRITE,FILE=filename,PATH=old_path,GET_PATH=P,FILTER=FNAME)

        IF F eq '' THEN return 

	if STRMID(F,0,1) eq '~' then filename_expand,F 

        found=findfile(F)

if found(0) eq '' then begin
	WIDGET_CONTROL,widget_ids.binary_type,set_droplist_select=1
	scanData.XDR = 1
	res = dialog_message(['File: ',F,'not found!!'],/info)
	return
end

if keyword_set(new) and found(0) ne '' then begin
	st = ['Error:','     '+F+' - File already exists!']
	w_warningtext,st
	catch1d_append,/new
	return
	end

if !d.name eq 'X' then begin
        IF (STRMID(F,0,1) EQ OS_SYSTEM.file_sep) THEN $
                FNAME = F $
        ELSE $
                FNAME = P+F
endif else FNAME = F
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
	if strmid(P,cl-1,1) eq OS_SYSTEM.file_sep then D = strmid(P,0,cl-1)
	end

; check for bad D

	if n_elements(D) eq 0 then begin
	w_warningtext,'Error:  bad directory path for the data file '
	return
	end

; check file for current path

        w_plotspec_array(3) = F

	ln = check_data_version(FNAME)
	if ln lt 0 then begin
		scanData.trashcan = old_path+old_file
		str = ['Warning!!!  Warning!!!  Warning!!!   ','', $
		'	  Wrong type of file selected']
		res = dialog_message(str,/ERROR)
		print,scanData.trashcan,scanData.scanno
		return
	end

	if ln  eq 0 then begin

	 if scanData.pid eq 0 then spawn,[!os.rm,'-f',scanData.filelock],/noshell
	 scanData.trashcan = FNAME
	 catch1d_appendCheck,FNAME
	 if string(D) ne string(old_path) then  pventry_event
	 
	end
	
	if ln eq 1 then catch1d_newCheck,FNAME,old_path

;
;   check whether catcher was started with the -n option
;
	if scanData.pid gt 0  and scanData.nosave eq 0 then begin
		scanData.nosave = 1
		catch1d_turnoffAutosave
	end 


END

PRO catch1d_newCheck,FNAME,old_path
COMMON SYSTEM_BLOCK,OS_SYSTEM
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id


; check for no write permission -171


        CATCH, error_status

        if error_status ne 0 then begin ; eq -171 then begin
        w_warningtext,["catch1d_newCheck: write permission denied",!err_string + string(!err)]
		scanData.pid = 2
		scanData.nosave = 1
        return
        end

	 if scanData.pid eq 0 then spawn,[!os.rm,'-f',scanData.filelock],/noshell

; use XDR binary as default

	WIDGET_CONTROL,widget_ids.binary_type,set_droplist_select=1
	scanData.XDR = 1
        u_openw,unit,FNAME,/APPEND,/XDR
        free_lun,unit
filenamepath,FNAME,F,P
scanData.path = P 

if strlen(P) gt 1 then begin
;	cd,P,CURRENT=old_path
	if scanData.debug eq 1 then $
	print,'CURRENT_DIR: ',P
	cl = strlen(P)
	if strmid(P,cl-1,1) eq OS_SYSTEM.file_sep then D = strmid(P,0,cl-1)
	scanData.trashcan = FNAME
	if string(D) ne string(old_path) then  pventry_event
	end

; check file for current path

if scanData.debug eq 1 then $
print,'Use New file ', FNAME

        if w_plotspec_id.opened ne 0 then free_lun,w_plotspec_id.opened
        w_plotspec_id.opened = 0
        w_plotspec_array(3) = F
        w_plotspec_id.mode = 0
        w_viewscan_id.seqno = 0
        w_plotspec_id.seqno = 0 
                w_viewscan_id.maxno = 0 
                w_viewscan_id.file = FNAME

	scanData.y_seqno = 0
	scanData.scanno_2d = 0

WIDGET_CONTROL,widget_ids.trashcan,SET_VALUE=FNAME

	scanData.nosave = 0
	scanData.viewonly = 0       ; override startup viewonly option
	catch1d_turnonAcquisition

	catch1d_grabLock,scanData.trashcan
	erase

END

PRO catch1d_appendCheck,FNAME
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

; check file for current path

	scanData.trashcan = FNAME
	catch1d_readFileIndex,FNAME,errcode

;        id = check_data_version(FNAME)
;	if id ne 0 then return 

if scanData.XDR eq 0 then scanData.option = 0

if errcode lt 0 then begin
	str=['Warning: The corresponding index file is not found !!', $
	     '         Use the ViewData->1D ... menu to create it']
	w_warningtext,str
	return
end

	maxno = 0
	found = findfile(FNAME)
        if found(0) ne '' then begin
		if scanData.XDR then u_openr,unit,FNAME,/XDR else $
                u_openr,unit,FNAME
		maxno = w_viewscan_id.maxno
		; if maxno gt 0 then update the screen
		if maxno gt 0 then begin
		point_lun,unit,w_viewscan_id.fptr(maxno-1)
		scan_read,unit, maxno, maxno+1
		end

                w_viewscan_id.seqno = maxno
                w_plotspec_id.seqno = maxno
                free_lun,unit
        endif else begin
                w_viewscan_id.seqno = 0
                w_viewscan_id.maxno = 0 
                w_viewscan_id.size = 0 
                w_plotspec_id.seqno = 0 
		scanData.scanno_2d = 0
		end
	
	if w_plotspec_id.opened ne 0 then free_lun,w_plotspec_id.opened
	w_plotspec_id.opened = 0 
	
	if scanData.viewonly eq 0 then begin
	if scanData.XDR eq 1 then begin
		catch1d_checkImageFile,scanData.trashcan+'.image',errcode
		if errcode lt 0 then catch1d_turnoffAcquisition else $
			catch1d_turnonAcquisition
	endif else catch1d_turnoffAcquisition
	if scanData.option eq 1 and scanData.nosave eq 1 then catch1d_turnoffAutosave
	end

if scanData.option eq 0 then begin         ; viewonly mode
w_warningtext,['NOTICE: ', $
	'     File path:  '+ scanData.path, $
	'     File name:  '+ FNAME, $
	'     Last Scan # : ' + string(maxno), $
	'     View Only  - Old data format, no data acquisition is allowed!!!' $
	]
endif else begin

WIDGET_CONTROL,widget_ids.trashcan,SET_VALUE=FNAME
if scanData.debug eq 1 then begin
print,'Current Seqno:',maxno
print,'Append scan data to ', FNAME
end

if maxno gt 0 then begin
if scanData.pid eq 0 then $ 
w_warningtext,quest='APPEND',['NOTICE: ', $
	'     File path:  '+ scanData.path, $
	'     File name:  '+ FNAME, $
	'     Last Scan # : ' + string(maxno), $
	'     New scan data will be appended to this selected file.' $
	]
endif else begin
w_warningtext,['NOTICE: ', $
	'     File path:  '+ scanData.path, $
	'     File name:  '+ FNAME, $
	'     It is a new file, no scan data in this file.', $
	'     New scan data in XDR form will be appended to this selected file.' $
	]
end
end
        w_plotspec_id.mode = 0
;print,' scan: unit=',unit
END 

PRO catch1dReadScanRecordAppendFile
COMMON CATCH1D_COM,widget_ids,scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotpec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

if scanData.pvfound eq -1 then return

	  ln = cagetArray(scanData.pv+'.CPT',pd)
          if ln eq 0 then scanData.act_npts = pd(0)
          WIDGET_CONTROL, widget_ids.pv_stat, $
            SET_VALUE = 'IDLE : Acquired' +STRING(scanData.act_npts)+' Pts'

	 if scanData.y_scan eq 0 then  set_sensitive_on

	if scanData.act_npts lt 1 then begin
;	w_warningtext,'No data detected in scan record: ' + scanData.pv
	return
	end

	scanData.readin_npts = scanData.act_npts

; get position and data array from the scan record

	getPositionDetectorData

	realtime_id.ind = -1

;
; automatic save scan data
;
	F = scanData.trashcan

if w_plotspec_id.autosave eq 0 then begin

; spawn,'date', x, /noshell
x = catimestamp(scanData.pv+'.EXSC')
y = strupcase(getenv('USER'))
w_plotspec_array(4) = x(0) + '. User Name: ' + y
if scanData.debug eq 1 then begin
print,''
print,w_plotspec_array(4)
end


;   append  scan data 

	if w_plotspec_id.opened gt 0 then w_plotspec_id.seqno = w_viewscan_id.maxno

OPEN_RET=0
	safeGuardTrashcan, F, error=OPEN_RET

	IF OPEN_RET GE 0 THEN BEGIN

        status = FSTAT(w_plotspec_id.opened)
        point_lun, w_plotspec_id.opened, status.size


; check wether other tool already wrote to the same file after initialization

	if status.size gt w_viewscan_id.size then begin
	st =['Error: other client wrote to the same file ','       '+ F, $
	     '       The scan data will not saved for this data catcher.']
	w_warningtext,st
	catch1d_turnoffAutosave	
	return
	end

;	spawn,[!os.chmod,'644',scanData.trashcan ],/noshell
	scan_mode_write, w_plotspec_id.opened
	flush, w_plotspec_id.opened
;	spawn,[!os.chmod,'444',scanData.trashcan],/noshell

	status = FSTAT(w_plotspec_id.opened)

;
; only if autosave is on then update the  fptr
;
	if w_plotspec_id.autosave eq 0 then begin
	w_viewscan_id.size = status.size
	w_viewscan_id.fptr(w_plotspec_id.seqno) = status.size
	;  always update the index file
	catch1d_writeFileIndex,scanData.trashcan
	end

	if scanData.debug eq 1 then $
	print,'Appending scan data # '+ strcompress(scanData.scanno)+ ' to "' + w_plotspec_array(3) +'"'
	ENDIF ELSE scanData.scanno = w_plotspec_id.seqno+1 ; OPEN_RET > 0 

endif else catch1d_process2Ddata 

	UPDATE_PLOT, 1
	if scanData.y_scan then setPlotLabels 

END


PRO catch1d_saveas,new=new
COMMON SYSTEM_BLOCK,OS_SYSTEM
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

;
; destroy the old w_plotspec window
;
	close_plotspec

	if scanData.pv eq '' then begin
		w_warningtext,'Enter SCAN Record Name first !!'
		return
		end

filenamepath,scanData.trashcan,old_file,old_path

        FNAME = ''
	filename = ''

;        if w_plotspec_id.opened ne 0 then free_lun,w_plotspec_id.opened
;        w_plotspec_id.opened = 0

        F = DIALOG_PICKFILE(TITLE='Save As ...',/WRITE,FILE=filename,PATH=old_path,GET_PATH=P,FILTER=FNAME)

        IF F eq '' THEN return 

	if STRMID(F,0,1) eq '~' then filename_expand,F 

	if !d.name eq 'X' then begin
        IF (STRMID(F,0,1) EQ OS_SYSTEM.file_sep) THEN $
                FNAME = F $
        ELSE $
                FNAME = P+F
	endif else FNAME = F

        found=findfile(FNAME)

if keyword_set(new) and found(0) ne '' then begin
	st = ['Error:','     '+FNAME+' - File already exists!']
	w_warningtext,st
	return
	end

; check for no write permission -171

	CATCH, error_status

        if error_status ne 0 then begin       ; eq -171 then begin
	str = !err_string + string(!err)
        w_warningtext,str
        return
        end

if scanData.debug eq 1 then $
print, 'Data Saved in new:        ',FNAME
; check file for current path
	filenamepath,FNAME,F,P

	w_plotspec_array(3) = F
	w_plotspec_id.seqno = 0

    	write_1_save, FNAME

; At the end of save as, the new file name and path will be in effect

 	scanData.trashcan = FNAME
	scanData.path = P

END



PRO PDMENU127_Event, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON w_caset_block, w_caset_base, w_caset_ids, w_caset_narray, w_caset_varray

  CASE Event.Value OF 

  'File.New ...': BEGIN
	catch1d_newfile
;	w_catch1dCreate,GROUP=Event.top
	END

  'File.Open ...': BEGIN
	catch1d_append
	catcher_setup,GROUP=Event.top
    END

   'File.FixIndexFile': BEGIN
       WIDGET_CONTROL,/HOURGLASS
       if scanData.XDR then $
       catch1d_newIndexFile,scanData.trashcan,array,/XDR else $
       catch1d_newIndexFile,scanData.trashcan,array
       w_viewscan_id.maxno = n_elements(array) - 1
       w_viewscan_id.fptr = array
       w_viewscan_id.size = array(w_viewscan_id.maxno)
     END

  'File.Close ': BEGIN
;
; destroy the old w_plotspec window
;
	close_plotspec

	if scanData.debug eq 1 then $
	print,w_plotspec_array(3), ' closed !!'
        if w_plotspec_id.opened ne 0 then free_lun,w_plotspec_id.opened
        w_plotspec_id.opened = 0
	if scanData.XDR then w_plotspec_array(3) = 'catch1d.trashcan.xdr' else $
        w_plotspec_array(3) = 'catch1d.trashcan'
	l = strlen(scanData.home)
	if strmid(scanData.home,l-1,1) ne OS_SYSTEM.file_sep then $
		 scanData.trashcan = scanData.home + OS_SYSTEM.file_sep + $
		w_plotspec_array(3) else $
        scanData.trashcan = scanData.home+w_plotspec_array(3)

	catch1d_appendCheck,scanData.trashcan

    END

  'File.Save As ...': BEGIN
	catch1d_saveas,/new

    END

  'File.GetScanData + Save ...': BEGIN
	if w_plotspec_id.autosave eq 0 then begin
	if scanData.debug eq 1 then $
	print,'Read the scan record and append to data file!!'
	st = [ $
	'Enter Y with a carriage return, the scan data from the scan record', $
	'         '+scanData.pv, $
        '    will be obtained and saved at the end of the data file.', $
	'Be aware of that the scan number will be increased by 1 and the plot', $
	'    will be updated accordingly.'] 
	w_warningtext,st,70,5,quest='Get Scan Data and Save'
	return
	end
    END

  'File.Copy ...': BEGIN
	w_catch1dCopy,GROUP=Event.Top
    END

  'File.Printer ...': BEGIN
	if !d.name eq 'X' then PS_printer,GROUP=Event.Top else $
        res = dialog_printersetup()
    END

  'File.Quit': BEGIN
;    catcher_close,event.top
    	WIDGET_CONTROL, event.top, /DESTROY
    END
  ENDCASE
END

PRO catcher_close,wid
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON w_caset_block, w_caset_base, w_caset_ids, w_caset_narray, w_caset_varray
    if w_plotspec_id.opened ne 0 then free_lun,w_plotspec_id.opened
    w_plotspec_id.opened = 0
    if XRegistered('w_caset') ne 0 then $
	WIDGET_CONTROL,w_caset_base,/DESTROY
	w_warningtext_close

	if scanData.pid eq 0 then spawn,[!os.rm,'-f',scanData.filelock],/noshell

    IF (STRLEN(scanData.pv) NE 0) THEN begin 
	if caSearch(scanData.pv) eq 0 then begin
;    	u=caWidgetClearmonitor(scanData.pv+'.EXSC',widget_ids.base)
;	u = caMonitor(scanData.pv+'.EXSC',/clear)
;	u = caMonitor(scanData.pv+'.NPTS',/clear)
;	u = caScan(scanData.pv+'.CPT','',/clear)


	; write index file

	fd = findfile(scanData.trashcan+'.index')
	if fd(0) eq '' then $
	catch1d_writeFileIndex,scanData.trashcan

	; change director error -296

	CATCH,error_status
        if error_status ne 0 then begin     ; eq -296 then begin
	w_warningtext,!err_string + string(!err)
        scanData.home = oldpath
        end
;	CD,scanData.home, CURRENT=oldpath
	spawn,['rm','catch1d.config.tmp'],/noshell
	if scanData.nosave eq 0 then write_config

	end

;	if caSearch(scanData.y_pv) eq 0 then begin
;    	u=caWidgetClearmonitor(scanData.y_pv+'.EXSC',widget_ids.base)
;	u = caMonitor(scanData.y_pv+'.EXSC',/clear)
;	u = caScan(scanData.y_pv+'.CPT','',/clear)
;	u = caMonitor(scanData.y_pv+'.NPTS',/clear)
;	end
	end

	if !d.name eq 'X' then EXIT
END

PRO getPositionDetectorData
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

if n_elements(realtime_pvnames) gt 0 then begin

	p_name = [ scanData.pv+'.P1RA', + scanData.pv+'.P2RA', $
		scanData.pv+'.P3RA', + scanData.pv+'.P4RA']

	d_name = [ scanData.pv+'.D1DA', + scanData.pv+'.D2DA', $
		scanData.pv+'.D3DA', + scanData.pv+'.D4DA', $
		scanData.pv+'.D5DA', + scanData.pv+'.D6DA', $
		scanData.pv+'.D7DA', + scanData.pv+'.D8DA', $
		scanData.pv+'.D9DA', + scanData.pv+'.DADA', $
		scanData.pv+'.DBDA', + scanData.pv+'.DCDA', $
		scanData.pv+'.DDDA', + scanData.pv+'.DEDA', $
		scanData.pv+'.DFDA'   $
		]

	scanData.pa = make_array(1000,4,/double)
	scanData.da = make_array(1000,15,/float)

	; get type and count for  positioner & detector

	ln = caGetTypeCount(realtime_pvnames,types,counts,wave_types)
	scanData.x_dpt(0:14) = counts(4:18)
	scanData.x_dtype(0:14) = wave_types(4:18)

	; fill position array

px_name = [ scanData.pv+'.P1PV', scanData.pv+'.P2PV', $
		scanData.pv+'.P3PV', scanData.pv+'.P4PV' ]
ln = cagetArray(px_name,pname,/string)

px_name = [ scanData.pv+'.R1PV', scanData.pv+'.R2PV', $
		scanData.pv+'.R3PV', scanData.pv+'.R4PV' ]
ln = cagetArray(px_name,tname,/string)

num_pts = 1 > (scanData.act_npts - 1) 
	for i=0,3 do begin
		if strlen(pname(i)) gt 1 or strlen(tname(i)) gt 1 then begin
		npts = scanData.act_npts + 1
		type = wave_types(i)
		ln = cagetArray(p_name(i), pd, max=npts,/double)
		if ln eq 0 then $
		scanData.pa(0:num_pts,i) = pd(0:num_pts)
		end
	end

	; fill detector array

px_name = [ scanData.pv+'.D1PV', scanData.pv+'.D2PV', $
		scanData.pv+'.D3PV', scanData.pv+'.D4PV', $
		scanData.pv+'.D5PV', scanData.pv+'.D6PV', $
		scanData.pv+'.D7PV', scanData.pv+'.D8PV', $
		scanData.pv+'.D9PV', scanData.pv+'.DAPV', $
		scanData.pv+'.DBPV', scanData.pv+'.DCPV', $
		scanData.pv+'.DDPV', scanData.pv+'.DEPV', $
		scanData.pv+'.DFPV' $
	]

ln = cagetArray(px_name,pname,/string)

	for i=0,14 do begin
	if strlen(pname(i)) gt 1 then begin
		npts = counts(i+4) * scanData.act_npts + 1
		type = wave_types(i+4)
		ln = cagetArray(d_name(i), pd, max=npts,type=type)
		if ln eq 0 then $
		scanData.da(0:num_pts,i) = pd(0:num_pts)
	   end
	end

end

END 




PRO write_1_save, filename
COMMON CATCH1D_COM, widget_ids, scanData

if scanData.act_npts gt 0 then begin
	u_openw,unit,filename,/XDR
	scan_mode_write,unit
	free_lun,unit
endif else begin
	w_warningtext,'Error: no scan data in memory yet!'
end
END



PRO w_warningtext_close
COMMON w_warningtext_block,w_warningtext_ids

	if XRegistered('w_warningtext') ne 0 then $
	WIDGET_CONTROL, w_warningtext_ids.base, /DESTROY
END


PRO set_sensitive_off
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

	WIDGET_CONTROL,widget_ids.file,SENSITIVE=0
	WIDGET_CONTROL,widget_ids.viewdata,SENSITIVE=0
	WIDGET_CONTROL,widget_ids.rept_base,SENSITIVE=0

	;  set plot option menu errbar, labels
	plotoptionsmenu_sensitive,14,0
	plotoptionsmenu_sensitive,21,0

	if XRegistered('catcher_setup') ne 0 then begin
	WIDGET_CONTROL,catcher_setup_ids.start,SENSITIVE=0
	WIDGET_CONTROL,catcher_setup_ids.start2,SENSITIVE=0
	WIDGET_CONTROL,catcher_setup_ids.stop,BAD=bad,SENSITIVE=0
	end

     if scanData.nosave and scanData.y_scan and strtrim(scanData.y_handshake,2) eq '' then WIDGET_CONTROL,widget_ids.file,SENSITIVE=1

END

PRO set_sensitive_on
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

;	WIDGET_CONTROL,widget_ids.menubar_base,SENSITIVE=1
	WIDGET_CONTROL,widget_ids.file,SENSITIVE=1
	WIDGET_CONTROL,widget_ids.viewdata,SENSITIVE=1
	WIDGET_CONTROL,widget_ids.rept_base,SENSITIVE=1
	WIDGET_CONTROL,widget_ids.wf_select,SENSITIVE=1

	;  set plot option menu errbar, labels
	plotoptionsmenu_sensitive,14,1
	plotoptionsmenu_sensitive,21,1

	if XRegistered('catcher_setup') ne 0 then begin
	WIDGET_CONTROL,catcher_setup_ids.start,SENSITIVE=1
	WIDGET_CONTROL,catcher_setup_ids.stop,SENSITIVE=1
	WIDGET_CONTROL,catcher_setup_ids.start2,SENSITIVE=1
	end

     WIDGET_CONTROL,widget_ids.axis_base,SENSITIVE=1

END

PRO catch1d_Start_xScan
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

	x = caputArray(scanData.pv+'.EXSC',1)
	if x eq -1 then w_warningtext,'Error encounted in START_1D_SCAN'

        if XRegistered('w_viewscan') ne 0 then begin
                WIDGET_CONTROL,w_viewscan_ids.base,/DESTROY
                end
        if XRegistered('w_plotspec') ne 0 then begin
                WIDGET_CONTROL,w_plotspec_ids.base,/DESTROY
                end
	WIDGET_CONTROL,catcher_setup_ids.pv,GET_VALUE=pv
	pv = strtrim(pv(0),2)
	if scanData.pv ne pv then begin
		scanData.pv = pv 
	pventry_event
		end

	if strlen(scanData.pv) lt 1 then begin
		w_warningtext,'Enter SCAN Record Name first !!'
		return
		end

; check for proper setup first

if scanData.pvfound eq -1 then return

	w_plotspec_id.scan = 1
	set_sensitive_off
	setPlotLabels

	if w_plotspec_id.realtime eq 1 then begin
		realtime_init
		end

END

PRO catch1d_Stop_xScan
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

	if scanData.pvfound eq -1 then return
	if caSearch(scanData.pv) eq 0 then begin
	ln = cagetArray(scanData.pv+'.EXSC',pd) 
	if ln eq 0 and pd(0) gt 0 then begin
	x = caputArray(scanData.pv+'.EXSC',0)
	if x eq -1 then w_warningtext,'Error encounted in STOP_1D_SCAN'
	end
	endif else w_warningtext,['Error: scan record  '+ scanData.pv +'  not found!']
	w_plotspec_id.scan = 1
        if scanData.y_scan eq 0 then set_sensitive_on
END

PRO catch1d_Stop_yScan
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

print,'ENTER catch1d_Stop_yScan'
if scanData.pvfound eq -1 then return

        if strlen(scanData.y_pv) lt 1 then return
        ln = cagetArray(scanData.y_pv+'.EXSC',pd)
        if ln eq 0 and pd(0) gt 0 then begin
        x = caputArray(scanData.y_pv+'.EXSC',0)
        if x eq -1 then w_warningtext,'Error encounted in STOP_2D_SCAN'

	; save the 2D-image file
	if w_plotspec_id.autosave eq 0 then $
	scan_mode_write_image

	; reset y-scan parameters

        w_plotspec_id.scan = 1
        scanData.y_scan = 0
        scanData.y_seqno = 0
        set_sensitive_on
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

;	catch1d_Start_xScan
	pventry_event
	if w_plotspec_id.realtime eq 1 then begin
		realtime_init
		end

       	pventry2_event

        if strlen(scanData.y_pv) lt 1 then begin
                w_warningtext,'Enter Y SCAN Record Name first !!'
                return
                end

; check for proper setup first

if scanData.pvfound eq -1 then return

	ln = cagetArray(scanData.y_pv+'.P1PV',s1)
	if ln eq 0 and s1(0) eq ''  then $ 
	begin
	st = ['Error: Y Direction Scan is not properly set', $
		'       for the scan record -  '+scanData.y_pv]
	w_warningtext,st
	return
	end
	

	scanData.y_scan = 1
	scanData.scanno_2d = scanData.scanno_2d + 1
	set_sensitive_off

	ln = cagetArray(scanData.y_pv+'.EXSC',pd)
	if pd(0) eq 0 then begin 
	x = caputArray(scanData.y_pv+'.EXSC', 1)
	if x eq -1 then w_warningtext,'Error encounted in START_2D_SCAN'
	end

       scanData.y_value=0.
       ln = cagetArray(scanData.y_pv+'.P1DV',pd,/float)
       if ln eq 0 then scanData.y_value = pd(0)

	scanData.x_desc = ''
	scanData.y_desc = ''
	p1 = [scanData.y_pv+'.P1PV',scanData.y_pv+'.DESC', $
		scanData.pv+'.P1PV',scanData.pv+'.DESC']
	id = cagetArray(p1,pd,/string)
	if id eq 0 then begin
	scanData.y_desc = pd(0)
	scanData.x_desc = pd(2)
	if pd(0) ne '' then begin
		res = strpos(pd(2),'.')
		if res gt 0 then px=strmid(pd(2),0,res) else px=pd(2)
		res = strpos(pd(0),'.')
		if res gt 0 then py= strmid(pd(0),0,res) else py=pd(0)
		p2 = [ py+'.DESC',py+'.EGU',px+'.DESC',px+'.EGU']
		nid = cagetArray(p2,pn2,/string)
		if nid eq 0 then begin
		   if pn2(0) ne '' then scanData.y_desc = pn2(0)
		   if strtrim(pn2(1),2) ne '' then scanData.y_desc =scanData.y_desc+ ' ('+strtrim(pn2(1),2)+ ')'
		   if pn2(2) ne '' then scanData.x_desc = pn2(2)
		   if strtrim(pn2(3),2) ne '' then scanData.x_desc =scanData.x_desc+ ' ('+strtrim(pn2(3),2)+ ')'
		   end
		end
	if pn2(0) eq '' and strtrim(pd(1),2) ne '' then scanData.y_desc = pd(1)
	if pn2(2) eq '' and strtrim(pd(3),2) ne '' then scanData.x_desc = pd(3)
	end

	scanData.y_array(0:2000) = 0.

	setPlotLabels

; get the start position

	filename = scanData.path+w_plotspec_array(3)+'.image'
	fn = findfile(filename,count=ct)
	if ct gt 0 then begin
	openr,1,filename
	f = fstat(1)
	close,1
	scanData.im_eof = f.size	
	endif else scanData.im_eof = 0L
	
END

PRO catch1d_turnonAcquisition
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

	scanData.option = 1
;	scanData.nosave = 0
	w_plotspec_id.autosave = 0
	WIDGET_CONTROL,widget_ids.savelabel,SET_VALUE='                '
	setupoptionsmenu_sensitive,1,1
	setupoptionsmenu_sensitive,4,1
	setupoptionsmenu_sensitive,7,1
	setupoptionsmenu_sensitive,10,1
	setupoptionsmenu_sensitive,13,1
	setupoptionsmenu_sensitive,16,1
END 

PRO catch1d_turnoffAcquisition
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

	scanData.option = 0
	w_plotspec_id.mode = 1
;	scanData.nosave = 1
	w_plotspec_id.autosave = 1
	w_plotspec_id.realtime = 1
	WIDGET_CONTROL,widget_ids.savelabel,SET_VALUE='  View Only'
;	setupoptionsmenu_set_string,5,6
	setupoptionsmenu_sensitive,1,0
	setupoptionsmenu_sensitive,4,0
	setupoptionsmenu_sensitive,7,0
	setupoptionsmenu_sensitive,10,0
	setupoptionsmenu_sensitive,13,0
	setupoptionsmenu_sensitive,16,0

END

PRO catch1d_turnonAutosave
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

	w_plotspec_id.autosave = 0
	WIDGET_CONTROL,widget_ids.savelabel,SET_VALUE='             '
	setupoptionsmenu_sensitive,4,1
END

PRO catch1d_turnoffAutosave
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

	w_plotspec_id.autosave = 1
	WIDGET_CONTROL,widget_ids.savelabel,SET_VALUE='  No Save'
;	setupoptionsmenu_set_string,5,6
	setupoptionsmenu_sensitive,4,0
	str= [ " --- For your information ---","", $
	     "Currently your catcher is running with 'No Save' mode.", $
		"Although you can not save data, you still can view the scanning data.", "", $
		"If you open any new or non-locked file, the catcher will automatically",$
		"turn on the save mode again for you.","" $
		]

	if scanData.pid eq 2 then $ 
	str = [str, $
		"It is detected that you have no write permission on file:", $
		scanData.trashcan, "" $
		]
	if scanData.pid eq 1 then $ 
	str = [str, $
		"It is due to the lock file already existed: ",$
		"         "+ scanData.filelock, '', $
		"Two possible cases:", $
		"Case 1 - file already used by another catcher", $
		"         Select 'No' ", "", $
		"Case 2 - dead lock due to PRIOR ABNORMAL termination ", $
		"         Select 'Yes' - if you want to add more scan data to the file", $

		"         Select 'No'  - otherwise ", "" $
		]

	if scanData.pid eq 1 then begin
	str = [str,"If you are in Case 2, do you want to remove the old lock file?"]
	res = dialog_message(str,/question,/default_no, title='Catcher No Save Warning')
	if res eq 'Yes' then catch1d_releaseLockTurnonAutosave
	endif else $

	res = dialog_message(str,/info, title='Catcher No Save Warning')


END

PRO safeGuardTrashcan,F,error=error
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id


if w_plotspec_id.opened eq 0 then begin


        CATCH,error_status

; demo mode error -128
; -215, -206, -171

        if error_status eq -215 then begin   ;  eq -128 then begin
	str = [!err_string + string(!err), '', $
		'    Warning!!!  Warning!!!  Warning!!! ','', $
		'    No scan data will be saved in this file.',$
		'    You need to select a new file.']
;		w_warningtext,str
		res = dialog_message(str,/Error)
;		w_plotspec_id.autosave = 1
		retall
		end

;       openu,unit,F,/GET_LUN,/APPEND
	if scanData.XDR then openw,unit,F,/GET_LUN,/APPEND,/XDR else $
        openw,unit,F,/GET_LUN,/APPEND
        w_plotspec_id.opened = unit
;print,'FILE OPENED for write:',unit
end


END

; CODE MODIFICATIONS MADE ABOVE THIS COMMENT WILL BE LOST.
; DO NOT REMOVE THIS COMMENT: BEGIN MAIN13_1

PRO MAIN13_1_Event, Event

  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

if scanData.option eq 1 then begin        ; if Acquisition.On is set by user

  ; this is and added CASE statement to see if the event.id
  ; is on that I assigned to a caWidgetAddmonitor ...

scanData.pv = scanData.pvconfig

  CASE event.id OF

  widget_ids.base: BEGIN
      IF (STRLEN(scanData.pv) EQ 0) THEN  RETURN
	chkid = caSearch(scanData.pv+'.EXSC')
IF chkid eq 0 then BEGIN

;   ret = caCheckMonitor(scanData.pv+'.EXSC')
    ret = caMonitor(scanData.pv+'.EXSC',/check) 
if ret eq -1 then begin                 ; ****may be error in caCheckMonitor	
	pventry_event
	end

;
; check whether 2D scan started by outside CA clients
;

	if strlen(scanData.y_pv) gt 0 and scanData.y_scan eq 0 then begin
	if caSearch(scanData.y_pv+'.EXSC') eq 0 then begin 
		id = cagetArray(scanData.y_pv+'.EXSC',pd) 
		if pd(0) eq 1 then  catch1d_Start_yScan
	end

	end

; scan mode check the following

if w_plotspec_id.mode eq 0 then begin
setPiDiMonitor,ret,/check 
if total(ret) gt 0 then begin
	if scanData.debug eq 1 then $
	print,ret,'Warning: Reset PV name in scan record!!!'
	if scanData.y_scan eq 0 then $	
	pventry_event
	end
end

scanFlag=0
if caSearch(scanData.pv+'.EXSC') eq 0 then begin
	ln = cagetArray([scanData.pv+'.EXSC',scanData.pv+'.DATA'],pd)
	if ln eq 0 then scanFlag = pd(0) else scanFlag=0 ;======= 8/15/96
	scanDataReady = pd(1)
end
      IF (scanFlag EQ 1) THEN BEGIN
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
if w_plotspec_id.realtime eq 1 then begin
	if  n_elements(realtime_pvnames) lt 1 then realtime_init
	realtime_xrange,1
	realtime_id.axis = 1
	  end
end

stt = 'SCANNING: ' +strtrim(scanData.act_npts,2)+ ' of '+  strtrim(scanData.req_npts,2)+ ' Pts' 
if scanData.y_scan gt 0 then stt = stt+' At '+strtrim(scanData.y_seqno,2)+"'th Scan 2D#"+strtrim(scanData.scanno_2d,2)
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

set_sensitive_off   ; when realtime is going on don't let user change 
                    ; the plot options or detecters 
                    ; this will guard the outside client invoked scan
		    ; only stop can terminate this
		realtime_init
		end
	if realtime_id.ind ne 2 then begin
		realtime_read,scanData.req_npts
		WIDGET_CONTROL,widget_ids.base, timer=w_plotspec_id.dtime
		endif else begin
;		WIDGET_CONTROL,widget_ids.base, /clear_events
		empty

		end
end


      ENDIF ELSE BEGIN
;
; scanFlag eq 0 case
;
	if w_plotspec_id.scan eq 1 then begin
	if scanData.y_scan eq 0 and scanDataReady eq 0 then return

	catch1dReadScanRecordAppendFile	
;
; update the cw_term with the final scan result
;
	if scanData.showlist eq 1 then begin
	save_scan_dump_curr,filename
	id = cw_term(widget_ids.terminal,filename=filename,/reset)
	end

	w_plotspec_id.scan = 0
	after_scan
	end
	
;
; check whether 2D scan stopped by outside CA clients
;
	if w_plotspec_id.mode eq 0 then begin

	  if scanData.y_scan eq 1 then begin
		id = cagetArray([scanData.y_pv+'.EXSC',scanData.y_pv+'.DATA'],pd,/short) 
		if pd(0) eq 0  and pd(1) eq 1 then begin
;		if pd(0) eq 0  and scanData.y_seqno lt scanData.y_req_npts then begin
; print,'Stopped by EXSC',scanData.scanno_2d,realtime_id.ind,scanData.y_seqno,scanData.y_req_npts,pd(1)
		if w_plotspec_id.autosave eq 0 then $
		scan_mode_write_image
		scanData.y_scan = 0
		scanData.y_seqno = 0
		set_sensitive_on
		end
		return
	  end

;
; check whether to terminate the  Y-scan 
;
	if scanData.y_scan eq 1 and $
		scanData.y_seqno ge scanData.y_req_npts then begin
; print,'Stopped by NPTS',scanData.scanno_2d,realtime_id.ind,scanData.y_seqno,scanData.y_req_npts,pd(1)
		if w_plotspec_id.autosave eq 0 then $
		scan_mode_write_image
		scanData.y_scan = 0
		scanData.y_seqno = 0
		set_sensitive_on
		if scanData.pid eq 0 then catch1d_refreshScreen
	        w_plotspec_id.mode = 0
		end
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
	x = scanData.pa(0:scanData.act_npts-1,w_plotspec_id.xcord)
	y = make_array(scanData.act_npts,15)
	y(*,*) = scanData.da(0:scanData.act_npts-1,0:14)
	ez_fit,x=x,y=y,GROUP=Event.Top
	END
  'PICK_IMAGE': BEGIN
	scanData.image = Event.Index + 1
	catch,status_error
	if status_error ne 0 then return
	wdelete,!D.window - 1		; 2D image window
	END
  'IMAGE_UPDATE': BEGIN
	scanData.eof2d = Event.Index
	END
  'BINARY_TYPE': BEGIN
	scanData.XDR = Event.Index
	END
  'PICK_PS': BEGIN
	ratio = .5 ^ Event.Index
    	PS_open,'catch1d.ps',scale_factor=ratio
    	UPDATE_PLOT, scanData.lastPlot
    	PS_close
	PS_print,'catch1d.ps'
	END

  'PICK_XAXIS': BEGIN
	w_plotspec_id.x_axis_u = 0
	w_plotspec_id.xcord = 0
	if Event.Index eq 0 then w_plotspec_id.x_axis_u = 1 else $
	w_plotspec_id.xcord = Event.Index - 1
	setPlotLabels
	if realtime_id.ind eq 1 then begin
		realtime_id.no = 0
		realtime_xrange,1,xmin,xmax
		realtime_id.axis = 1
 	endif else $	
	UPDATE_PLOT, scanData.lastPlot
	END


  'DRAW61': BEGIN
;print,'Event.PRESS',event.press
   if w_plotspec_id.scan eq 0 then begin
	if (!x.window(1) - !x.window(0)) eq 0 then begin
		w_warningtext,'Error: Plot data not established yet.'
		return
		end
; cross-hairs
      IF (Event.PRESS EQ 1) THEN BEGIN
	WSET, widget_ids.plot_area
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

	if w_plotspec_id.scan eq 0 then begin
		UPDATE_PLOT, scanData.lastPlot 
		return
	end
	if realtime_id.ind eq 1 then begin
                        realtime_id.ymin =0.
                        realtime_id.ymax =0.
                        realtime_id.axis = 1
		end
      END

  'USER_BEFORE': BEGIN
      before_scan
      END
  'USER_AFTER': BEGIN
      after_scan
      END
  ELSE:     ;don't stop of no matches
  ENDCASE
END


PRO pventry2_event 
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
  COMMON CATCH1D_2D_COM, data_2d 
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan


if XRegistered('catcher_setup') ne 0 then begin

        WIDGET_CONTROL,catcher_setup_ids.y_pv,GET_VALUE=input_string
        new_pv_name = input_string(0)

	; clear monitors on existing pv.EXSC

      IF new_pv_name ne scanData.y_pv THEN begin 
	if caSearch(scanData.y_pv+'.EXSC') eq 0 then begin
           u=caWidgetClearmonitor(scanData.y_pv+'.EXSC',widget_ids.base)
	   u = caMonitor(scanData.y_pv+'.EXSC',/clear)
	   u = caScan(scanData.y_pv+'.CPT','',/clear)
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
	res=caWidgetSetMonitor(new_pv_name+'.EXSC',widget_ids.base)
	u = caMonitor(new_pv_name+'.EXSC',/add)
	u = caMonitor(new_pv_name+'.NPTS',/add)
	pd=0
	ln = cagetArray(scanData.y_pv+'.NPTS',pd)
	if ln eq 0 then scanData.y_req_npts = pd(0)
	realtime_id.ind = -1


; create 2D data arrays

	make_2d_data,data_2d,scanData.x_dpt,scanData.req_npts,scanData.y_req_npts

	if caSearch(scanData.pv+'.EXSC') eq 0 then begin
	if caMonitor(scanData.pv+'.EXSC',/check) ne 0 then begin
	scan_field_get,scanData.pv
	setDefaultLabels
	setPiDiMonitor,/add
	end
	end

      ENDIF ELSE BEGIN
        WIDGET_CONTROL, widget_ids.pv_stat,SET_VALUE = '>> PV2 NOT VALID <<'
        scanData.y_pv = ''
      ENDELSE

	scanData.y_seqno = 0
	if scanData.debug eq 1 then $
        print,'scanData.y_seqno=',scanData.y_seqno
END


PRO pventry_event 
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

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
	   setPiDiMonitor,/clear
		end
	  scanData.pv = new_pv_name
	  end

end

        new_pv_name = scanData.pv

w_plotspec_array(0) = new_pv_name
w_plotspec_id.scan = 0

      IF (STRLEN(new_pv_name) EQ 0) THEN res = -1  $
      ELSE res = caSearch(new_pv_name+'.EXSC')

scanData.pvfound = res

      IF res EQ 0 THEN BEGIN
        WIDGET_CONTROL, widget_ids.pv_stat, SET_VALUE = '>> PV Valid <<'
	res=caWidgetSetMonitor(new_pv_name+'.EXSC',widget_ids.base)
	u = caMonitor(new_pv_name+'.EXSC',/add)
	u = caMonitor(new_pv_name+'.NPTS',/add)
	pd=0

	ln = cagetArray(scanData.pv+'.NPTS',pd)
	if ln eq 0 then scanData.req_npts = pd(0)
	scanData.realtime = 0
	realtime_id.ind = -1

        ; get type and count for  positioner & detector

        pvnames = [ scanData.pv+'.D1CV', $
        	scanData.pv+'.D2CV', $
	        scanData.pv+'.D3CV', $
       	 	scanData.pv+'.D4CV', $
       	 	scanData.pv+'.D5CV', $
       	 	scanData.pv+'.D6CV', $
       	 	scanData.pv+'.D7CV', $
        	scanData.pv+'.D8CV', $
        	scanData.pv+'.D9CV', $
        	scanData.pv+'.DACV', $
        	scanData.pv+'.DBCV', $
        	scanData.pv+'.DCCV', $
        	scanData.pv+'.DDCV', $
        	scanData.pv+'.DECV', $
        	scanData.pv+'.DFCV' $
		]

        ln = caGetTypeCount(pvnames,types,counts,wave_types)
        scanData.x_dpt = counts
        scanData.x_dtype = wave_types

	scan_field_get,scanData.pv
	setDefaultLabels
	setPlotLabels
	setScanPvnames

	setPiDiMonitor,/add

	before_sys_scan

      ENDIF ELSE BEGIN
;	w_warningtext,'Invalid SCAN Record Name !!'
        WIDGET_CONTROL, widget_ids.pv_stat,SET_VALUE = '>> PV NOT VALID <<'
        scanData.pv = ''
      ENDELSE

if w_plotspec_id.opened eq 0 then begin
	id = check_data_version(scanData.trashcan) 
	if id eq 0 then catch1d_check_seqno 
end

	WIDGET_CONTROL,widget_ids.trashcan, SET_VALUE = scanData.trashcan
	scanData.y_scan = 0

END

PRO catch1d_check_seqno
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
;
;       update seqno based on scanData.trashcan file
;

        check_file_seqno,strcompress(scanData.trashcan,/remove_all)

; set the startno

        scanData.startno = w_plotspec_id.seqno + 1
        scanData.refno = w_plotspec_id.seqno + 1
if scanData.option gt 0 and scanData.debug eq 1 then $
        print,'scanData.startno=',scanData.startno

END

PRO setPiDiMonitor,ret,add=add,clear=clear,check=check
COMMON CATCH1D_COM, widget_ids, scanData

x_wd = [scanData.pv+'.P1WD', $
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
        scanData.pv+'.P4PV', $
        scanData.pv+'.D1PV', $
        scanData.pv+'.D2PV', $
        scanData.pv+'.D3PV', $
        scanData.pv+'.D4PV', $
        scanData.pv+'.D5PV', $
        scanData.pv+'.D6PV', $
        scanData.pv+'.D7PV', $
        scanData.pv+'.D8PV', $
        scanData.pv+'.D9PV', $
        scanData.pv+'.DAPV', $
        scanData.pv+'.DBPV', $
        scanData.pv+'.DCPV', $
        scanData.pv+'.DDPV', $
        scanData.pv+'.DEPV', $
        scanData.pv+'.DFPV' $
        ]

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
COMMON env_field_block,env_field
COMMON field_block, field_max, field_name, field_name_array, field_value,  field_label_array,w_scanfield_ids
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

; check for demo mode

	if demo_mode() eq 1 then begin
	str = ['Sorry: Not able to obtain the license (Demo mode)', $
		'      Try:', $
		'           catcher -D       ( for developer license) ', $
		'', $
		'           catcher          ( for runtime license) ' $
	]
	w_warningtext,str,75,6
	exit
	end

; initalize env arrays

if n_elements(env_field) eq 0 then $
env_field = { $
	exist	: 0, $
	no	: 0, $
	noenv	: 0, $
	numkey	: 0, $
	keys    : make_array(scanData.maxenv,/int), $
	pvnames : make_array(scanData.maxenv,/string,value=string(replicate(32b,30))), $
	descs   : make_array(scanData.maxenv,/string,value=string(replicate(32b,40))), $
	values  : make_array(scanData.maxenv,/string,value=string(replicate(32b,40))), $
	oldvalues : make_array(scanData.maxenv,/string,value=string(replicate(32b,40))) $
	}

; read in configuration file  

found = findfile('catch1d.config.tmp')
if found(0) ne '' then read_config,'catch1d.config.tmp' else read_config

; read in go_catcher2 for runtime version

found = findfile('go_catcher2')
if found(0) ne '' then read_config,'go_catcher2'

scanData.pvconfig = scanData.pv
if scanData.debug eq 1 then begin
print,scanData.home
print,scanData.path
end

	if scanData.option ne 0 and strlen(scanData.pv) lt 2 then begin
	st = [ $
	'Note: There is no configuration file found.  ', $
	'',$
	'      You first have to set up the scan PV name by using the', $
	'           Setup->Scan ...    menu ' $
	]
	w_warningtext,st
	if scanData.nosave eq 0 then return
	end

; override the data file on command line by the setting in 
; the configuration file

	scanData.trashcan = scanData.path + w_plotspec_array(3)
	w_viewscan_id.file = scanData.trashcan
;	if scanData.nosave eq 1 then scanData.fileinuse = scanData.trashcan

		id = check_data_version(scanData.trashcan) 

		if id lt 0 then begin
			st = ['NOTICE: ','', $
				scanData.trashcan,'', $
			'Illegal input file detected.', '', $
			'You have to pick a new input file first.']
			mes = dialog_message(st,/Error,dialog_parent=widget_ids.base)
			return      
		end

	if strlen(scanData.pv) gt 0 then u = caMonitor(scanData.pv+'.EXSC',/add)

	found = findfile(scanData.trashcan)
	if found(0) eq '' then begin
		st = ['Filename will be created','',scanData.trashcan, $
		'','Otherwise use the "File" menu to set up the catcher file.', $
		'Then use the "Setup" menu to set up the scan PV names.']
		mes = widget_message(st,/Error)
if strlen(scanData.y_pv) gt 1  and caSearch(scanData.y_pv+'.EXSC') eq 0 then begin
        pventry2_event
        end
if strlen(scanData.pv) gt 1  and caSearch(scanData.pv+'.EXSC') eq 0 then begin

        pventry_event
        end
		return
		end
	
	catch1d_readFileIndex,scanData.trashcan

WIDGET_CONTROL,/HOURGLASS

if scanData.option eq 0 then begin
;	id = check_data_version(scanData.trashcan) 
	if id eq 0 then catch1d_check_seqno 
endif else if scanData.option gt 0 then begin
if strlen(scanData.y_pv) gt 1  and caSearch(scanData.y_pv+'.EXSC') eq 0 then begin
        pventry2_event
        end
if strlen(scanData.pv) gt 1  and caSearch(scanData.pv+'.EXSC') eq 0 then begin

        pventry_event
        end
end

new_pv = scanData.pv
new_y_pv = scanData.y_pv

; set plot menu options

	w_plotspec_id.x_axis_u = 0
	w_plotspec_id.type = 0
	w_plotspec_id.log = 0
	w_plotspec_id.grid = 0
	w_plotspec_id.errbars = 0
	w_plotspec_id.xcord = 0

; check for autosave

       if scanData.nosave eq 1  or scanData.pid gt 0 then begin
		catch1d_turnoffAutosave 
	endif else begin

	if scanData.XDR eq 1 and scanData.option eq 1 then begin
		catch1d_checkImageFile,scanData.trashcan+'.image',errcode
		if errcode lt 0 then catch1d_turnoffAcquisition else $
			catch1d_turnonAcquisition
	endif else catch1d_turnoffAcquisition

       end


; plot the very last scan at the startup time

	catch1d_refreshScreen,pv

scanData.pv = new_pv
scanData.y_pv = new_y_pv

;	before_sys_scan
	pventry_event

if scanData.pvfound eq -1 then return

	y = [scanData.pv+'.EXSC', scanData.pv+'.CPT']
	ln = cagetArray(y,pd,/short)

	if ln eq 0 and scanData.option gt 0 and scanData.nosave eq 0 then begin
	x = catimestamp(scanData.pv+'.EXSC')
	
	if strpos(w_plotspec_array(4),x,0) eq -1 then begin
		if pd(0) eq 0 and pd(1) gt 1 then begin
		st = ['NOTICE:   At TimeStamp = '+x, $
		'    Scan record  "'+scanData.pv+'"  holds scanned data.', $
		'    If you desired, you may enter Y with carriage return', $
		'    to save this data before you do any new scan.']
		w_warningtext,st,70,5,quest='Get Scan Data and Save'
		endif else begin
;		w_warningtext,'Data catcher initialization completed!!'
		end
	endif else begin
;	w_warningtext,'Data catcher initialization completed!!'
	end
	end

; if pv in data file not consistant with config file
	if (pv(0) ne scanData.pv and pv(0) ne '') then begin
	  str=['Note Scan 1D PVname is different in the config and data files', $
	       'You may use Setup->Scan... Menu reset the scan monitor PVnames']
	  ret = dialog_message(str,/info)
	end

END

; DO NOT REMOVE THIS COMMENT: END MAIN13_1
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO catcher_v1, config=config, envfile=envfile, data=data, nosave=nosave, viewonly=viewonly, GROUP=Group
;
;+
; NAME:
;	CATCHER_V1
;
; PURPOSE:
;       EPICS 3.12.2 Scan record data acquistion tool.
;
;       This program provides the EPICS user a convenient IDL data 
;       acquistion tool.  It calls the IDL external CA functions which 
;       are defined in the ezcaIDL package. 
;
;       Currently, this program can capture both 1D and 2D scan data
;       through setting monitor on the EPICS scan record. During data
;       acquisition it also provides real-time plotting. Post acquisition
;       it also provides primitive 1D and 2D post data plot and analysis. 
;       Each scan record can support up-to four positioners and fifteen
;       detectors."
;
; CATEGORY:
;	Widgets. 
;
; CALLING SEQUENCE:
;	CATCHER_V1 
;
; INPUTS:
;	None
;
; KEYWORD PARAMETERS:
;     CONFIG:   Specifies the configuration file to be saved when data
;               catcher is normally terminated. If not specified, the 
;               default configuration file name 'catch1d.config' is 
;               assumed.
;     DATA:     Specifies the data file name to be used for appending 
;               new captured scan data. If not specified, the default 
;               data file name 'catch1d.trashcan.xdr' is assumed.
;     ENVFILE:  Specifies the environment (process variables) file to
;               be saved with the captured scan data. If not specified,
;               the default environment file name 'catch1d.env' is 
;               assumed.
;     NOSAVE:   Turns off the autosave feature. If it is specified, the
;               data catcher will be able to display the life data but
;               data will not be recorded.
;     VIEWONLY: Runs the data catcher as a scan data browser. If it is 
;               specified, it can only display the recorded scan data 
;               from a saved data file. 
;     GROUP:    The widget ID of the group leader of the widget.  If this 
;               keyword is specified, the death of the group leader results in
;               the death of CATCHER_V1.
;
; OUTPUTS:
;       It provides realtime plot and various post acquistion 1D and
;       2D surface plot. In addition of DATA file is saved, if 2D scan 
;       is detected the DATA.image file will also be saved at the end 
;       of 2D scan. Various levels of report files can also be generated 
;       by this program.  All the file generated will be prefixed by the 
;       DATA file name.
;
; COMMON BLOCKS:
;       CATCH1D_COM
;       W_PLOTSPEC_BLOCK
;       W_VIEWSCAN_BLOCK
;       REALTIME_BLOCK 
;       ENV_FIELD_BLOCK
;       FIELD_BLOCK
;
; SIDE EFFECTS:
;       It is capable to detect scan invoked by outside channel access
;       client.
;
; RESTRICTIONS:
;       Supports EPICS 3.12.2 and later scan record.
;
; PROCEDURE:
;       For live data acquisition: boot the IOC first, setup and initialize
;       the scan records, setup the run ezcaIDL environment, run IDL,
;       then start up the data catcher at the IDL prompt. 
;       
;       A simplified UNIX script file catcher can be used for starting up 
;       this program. For help on catcher enter "catcher -h" at the UNIX
;       prompt.
;
; EXAMPLE:
;       Use default setting for config, envfile, and data file
;
;       CATCHER_V1
;
;       Override the default setting for config, envfile, and data file, 
;       the 'test' is used for this example.
;
;       CATCHER_V1, CONFIG='test.config', ENVFILE='test.env', DATA='test.dat'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 02-27-96.
;
;       04-16-96  bkc  - Reduce the data catcher window size by moving scan 
;                        pvname entries to a pop up setup window 
;                      - Realtime trend plot against the real time axis is 
;                        implemented
;                      - Realtime plot also displays the scan record name
;                      - Fix various miscellaneous replot bugs
;                      - Add option of accessing HTML documents on help menu 
;
;       04-29-96 bkc   - Modified pv status, main window manager, release
;                        note 1.4
;       09-30-96 bkc   - Implemented more new features, dialog windows, 
;                        exceptional catches, check for non saved scan
;                        data, improve the efficiency, release note 1.5.1 
;                        etc...
;       11-07-96 bkc   - Monitor positioner readback name, debug option,
;                        various realtime plot option
;       12-13-96 bkc   - Modified index file updating, aborting 2D scan,
;                        image file location, report location
;       07-23-97 bkc   - Added image strip to show all 15 detectors 2D image 
;                        (two sizes are available); 
;                        Added the support for different operating system, eg
;                        UNIX or W95 (see SYSTEM_BLOCK, os.init);
;                        Modified to run under IDL4.0.1 as well as IDL5.0 
;       08-08-97 bkc   - Release R2.1 changes
;                        Fix the problem of 'No Save', wrong X, Y array due 
;                        to user aborting 2D scans from the MEDM screen
;                        Database desc info for detectors are added to 
;                        the 2D image data
;       09-08-97 bkc   - Release R2.2.X changes
;                        Supports both XDR and native type data format
;                        If no configuration file exists, it defaults to XDR 
;                        format
;                        Add the curve fitting menu support
;			 Add the color support for the 24 bit visual display
;			 Fix the problem with xmax plot range exceeds the 
;                        valid int range 64000
;       01-28-98 bkc   - Fix the trend analysis problem if time exceeds 32000
;                        Fix the error in standard deviation calc if max occurs
;                        at y(0)
;       02-04-98 bkc   - Add realtime 2D image options for any selected detector
;       02-06-98 bkc   - Fix the realtime problem of setting NPTS exceeds 1724
;                        Fix the problem with 2D header on 1D drawing area
;                        Fix automatic ylog plot scale problem 
;       02-11-98 bkc   - Allows the user enter any Y scan PV name 
;			 Automatic figure out bad file selected 
;       02-19-98 bkc   - Load default color table once only, use the common
;                        color table between view1d, view2d, default line 
;			 color defined in catch1d.tbl save by IDL5
;       02-27-98 bkc   - Make the File menu sensitive if 'NoSave' is specified
;                        in configuration file for 2D scan
;       03-02-98 bkc   - Fix Y log plot yrange not defined problem
;       03-13-98 bkc   - Fix the realtime plot problem for loading table 
;                        positioner array 
;       03-24-98 bkc   - Fix the problem of runtime version the user specified
;                        configuration file did not get updated but the 
;                        'catch1d.config' got updated
;       04-14-98 bkc   - Add checking for the wrong type of file picked 
;       04-15-98 bkc   - Fix the print problem in cw_term function 
;       05-15-98 bkc   - Eliminate the repeat of 1st scan in 2D scan
;                        Use P1DV value as y(0) value for 1D plot 
;       09-26-98 bkc   - Add Y-vector to hold the y-value for 2D scan (dim 2001)
;       11-24-98 bkc   - R2.2.2b
;                        New data acquisition will be saved in XDR format.
;                        Data catcher will be in viewing mode only if the old 
;                        data is used. 
;                        Fix the read problem introduced by the user due to 
;                        too many characters entered in the comment field.
;       12-08-98 bkc   - R2.2.2c Port to W95
;       01-12-99 bkc   - Dynamic read in u_read,PS_open,cw_term sources
;       01-20-99 bkc   - Fix nosave option at start up it stays nosave option 
;                        until a new file is entered 
;                      - Fix viewonly option at start up it stays nosave option 
;                        until a new file is entered 
;       02-02-99 bkc   - R2.2.2.c+
;                        Fix the File->Copy option it will not work if detectors;                        number exceeds 11
;                        Add the pop up message window to warn user if the 1D
;                        monitor pvname in config file is not same as in data file
;       03-11-99 bkc   - Update Save As ... in XDR format
;                        Add the fixIndexFile support in File Menu
;                        Fix the problem caused by Start/Prior Pos setting in 2Dscan
;       03-19-99 bkc   - Fix the problem starting catcher with blank new file
;       03-23-99 bkc   - Fix the plot title for 1D scan 
;       04-15-99 bkc   - R2.2.2c1
;                        Fix the problem associated with 1D scan aborted by the 
;                        operator sometimes the scan record is still in busy 
;                        state and the scan record return arrays have not been 
;                        updated yet such that old scan value is saved
;       04-15-99 bkc   - Check for the case empty environment data set is used 
;       05-24-99 bkc   - R2.2.2c2
;                        Add 2D ROI support, TIFF, GIF
;       06-17-99 bkc   - R2.2.2c3
;                        Check for nosave mode if same file been used by other
;                        catcher at startup time
;       07-17-99 bkc   - R2.2.2c4
;                        Use the lock file system to check for nosave mode
;       08-26-99 bkc   - R2.2.2c5
;                        Do not reset plot range after zooming
;                        Eliminate the extra set of 2D images
;       08-26-99 bkc   - R2.2.2c6
;                        Fix 2D image X axis for relative and absolute scan mode
;                        View report button automatically created report if not ;                        existed yet
;                        Add FWHM on Y and DY item in statistic menu
;                        Automatic load the first curve into the ez_fit program
;                        Upgraded Plot1D plot package
;       11-11-99 bkc   - R2.2.2c7
;                        Plugin calibration program to panimage menu
;                        PanImage menu supports saving TIFF/RTIFF/GIF images
;                        Creating ASCII/TIFF/GIF/CALIB/ROI directory for saving
;                        respective type of files
;                        Increase X,Y vector array to 2001 in view2D
;       01-27-00 bkc   - R2.2.2c7+
;                        View2d R2.3g+ with set new 2D positions
;       05-15-00 bkc   - R2.2.2c8
;                        Fix the plot broblem if the request NPTS is set to 2 
;                        by the user. If NPTS is set to 1, data is duplicate 
;                        once to get around the oplot problem.
;                        Change the dialog info if configuration error is
;                        detected, it allows the user re-enter the File menu.
;       08-31-00 bkc   - Add the error handling for null file entered 
;       12-06-00 bkc   - R2.2.2c8+
;                        Fix realtime panimage window problem
;                        Replace GIF by XDR saving options in view2d, plot2d
;       06-30-01 bkc   - R2.2.2c9
;                        Fix xtitle with positioner number
;	08-01-01 bkc     Modified the PV goto dialog according to the readin 
;			 positioner values
;       01-16-02 bkc   - Modified scan setup dialog
;       03-12-02 bkc   - Add 2D scan ranges setup
;                        Modified regessfit, lorentzian, readascii
;                        Add bkg color button to plot1d, ez_fit
;                        Modified 1D Goto based on the input file
;       03-19-02 bkc   - Save 2D image at the end of each 1D scan
;                        Add a variation error function FUNCT_ERF1 to
;                        ez_fit
;       03-17-03 bkc   - Add option of selecting 'PS ratio' droplist
;-
COMMON SYSTEM_BLOCK,OS_SYSTEM
 COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON env_field_block,env_field
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON field_block, field_max, field_name, field_name_array, field_value,  field_label_array,w_scanfield_ids
COMMON font_block, text_font, graf_font, ps_font
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

; check for demo mode
 

  if XRegistered('MAIN13_1') NE 0 then return

; text_font = '6x13'
; graf_font = text_font
; ps_font ='9x15'
;Widget_Control, Default_Font= text_font

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  MAIN13_1 = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, /TLB_SIZE_EVENTS, $
      TLB_FRAME_ATTR = 8, $
      TITLE='Scan Data Catcher (R2.2.2c9+)', $
      UVALUE='MAIN13_1')

  BASE68 = WIDGET_BASE(MAIN13_1, $
	/ROW, $
      MAP=1, $
;      FRAME=2, $
;      TITLE='Top Menu Line', $
      UVALUE='BASE68')

  MenuDesc1981 = [ $
      { CW_PDMENU_S,       3, 'File' }, $ ;        0
        { CW_PDMENU_S,       0, 'New ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Open ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'FixIndexFile'}, $ ;        1
;        { CW_PDMENU_S,       0, 'Close '}, $ ;        1
        { CW_PDMENU_S,       0, 'Save As ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'GetScanData + Save ...'}, $
        { CW_PDMENU_S,       0, 'Copy ...' }, $ ;        2
        { CW_PDMENU_S,       0, 'Printer ...' }, $ ;        2
        { CW_PDMENU_S,       2, 'Quit' } $  ;      4
  ]

  PDMENU127 = CW_PDMENU( BASE68, MenuDesc1981, /RETURN_FULL_NAME, $
      UVALUE='PDMENU127')


  SETUPOPTIONSMENU = setupOptions( BASE68, UVALUE='SETUPOPTIONSMENU')

  PLOTOPTIONSMENU = plotOptions( BASE68, UVALUE='PLOTOPTIONSMENU')


;  Btns_mode = [ $
;    'Scan', $
;    'View' ]
;  BGROUP_mode = CW_BGROUP( BASE68, Btns_mode, $
;      COLUMN=2, $
;      FRAME=1, $
;      LABEL_LEFT='Mode:', $
;      EXCLUSIVE=1, $
;      UVALUE='BGROUP_MODE')
;  WIDGET_CONTROL, BGROUP_mode, SET_VALUE=0

  MenuHelp = [ $
      { CW_PDMENU_S,       3, 'Help' }, $ ;        0
        { CW_PDMENU_S,       0, 'Version ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Release Note ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Help ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Catcher Html ...'}, $ ;        1
        { CW_PDMENU_S,       0, 'ezcaIDL Html ...' } $ ;        1
	]

  PDMENU_help = CW_PDMENU( BASE68, MenuHelp, /RETURN_FULL_NAME, $
      UVALUE='HELPMENU')

; add the 1D/2D view memu

  MenuVData = [ $
      { CW_PDMENU_S,       3, 'ViewData' }, $ ;        0
        { CW_PDMENU_S,       0, '1D ...' }, $ ;        1
        { CW_PDMENU_S,       0, '2D ...' }, $ ;        1
        { CW_PDMENU_S,       0, '1D Overlay ...' } $ ;        1
	]

  PDMENU_VDATA = CW_PDMENU( BASE68, MenuVData, /RETURN_FULL_NAME, $
      UVALUE='PDMENU_VDATA')

  IMAGE_UPDATE = WIDGET_DROPLIST(BASE68, VALUE=['Image@1D','Image@2D'], $
        UVALUE='IMAGE_UPDATE',TITLE='')
  BINARY_TYPE = WIDGET_DROPLIST(BASE68, VALUE=['BIN','XDR'], $
        UVALUE='BINARY_TYPE',TITLE='')
  WIDGET_CONTROL,BINARY_TYPE,set_droplist_select = 1
  WIDGET_CONTROL,BINARY_TYPE,sensitive = 0

  SAVE_LABEL = WIDGET_LABEL( BASE68, $
      FONT=OS_SYSTEM.font, $
	xsize=150, VALUE='          ')

  BASE69 = WIDGET_BASE(MAIN13_1, $
      ROW=1, $
      MAP=1, $
      TITLE='FILE', $
      UVALUE='BASE69')

  CATCHER_FILE = WIDGET_LABEL(BASE69, $
	/ALIGN_LEFT,/DYNAMIC_RESIZE, VALUE='catch1d.trashcan.xdr')

  BASE140 = WIDGET_BASE(MAIN13_1, $
      ROW=1, $
;      FRAME=2, $
      MAP=1, $
      TITLE='STATUS', $
      UVALUE='BASE140')

  FieldVal1988 = [ $
    '>> PV NOT VALID <<' ]
  FIELD141 = CW_FIELD( BASE140,VALUE=FieldVal1988, $
      ROW=1, $
      STRING=1, /NOEDIT, $
      TITLE='Status', $
      UVALUE='FIELD141', $
      XSIZE=40)

  BASE140_1 = WIDGET_BASE(BASE140, $
      ROW=1, $
      MAP=1, YSIZE=30, $
      UVALUE='BASE140_1')
  MenuPrint = [ $
      { CW_PDMENU_S,       3, 'Print' }, $ ;        0
        { CW_PDMENU_S,       0, 'Plot' }, $ ;        1
        { CW_PDMENU_S,       2, 'Report ...' } $ ;        4
  ]
  PDMENU_print = CW_PDMENU( BASE140_1, MenuPrint, /RETURN_FULL_NAME, $
      UVALUE='PRINTMENU')

  MenuZoom = [ $
      { CW_PDMENU_S,       3, 'Zoom' }, $ ;        0
        { CW_PDMENU_S,       0, 'Zoom To Box' }, $ ;    1   
        { CW_PDMENU_S,       0, 'Zoom In/Out' }, $ ;        2
        { CW_PDMENU_S,       0, 'Calc Slopes' }, $ ;       3
        { CW_PDMENU_S,       0, 'Zoom Off (AutoScale)' }, $ ;       4
        { CW_PDMENU_S,       2, 'User Scale ...' } $ ;       5
  ]
  PDMENU_zoom = CW_PDMENU( BASE140_1, MenuZoom, /RETURN_FULL_NAME, $
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

  PDMENU_statistic = CW_PDMENU( BASE140_1, MenuStatistic, /RETURN_FULL_NAME, $
      UVALUE='STATISTICMENU')

;  fitting_1d = WIDGET_BUTTON(BASE140_1,VALUE='Fitting',UVALUE='EZFIT_FITTING')
; fitting menu
  MenuFitting = [ $
      { CW_PDMENU_S,       3, 'Fitting' }, $ ;        0
        { CW_PDMENU_S,       0, 'Ez_Fit ...' }, $ ;        1
        { CW_PDMENU_S,       2, '1D Binary'} $ ;        1
  ]

  PDMENU_fitting = CW_PDMENU( BASE140_1, MenuFitting, /RETURN_FULL_NAME, $
      UVALUE='FITTINGMENU')

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
;      COLUMN=1, $
      ROW=1, $
;      FRAME=2, $
      MAP=1, $
      TITLE='WF Selector', $
      UVALUE='BASE144')

  Btns1994 = [ $
    'D1', $
    'D2', $
    'D3', $
    'D4', $
    'D5', $
    'D6', $
    'D7', $
    'D8', $
    'D9', $
    'D10', $
    'D11', $
    'D12', $
    'D13', $
    'D14', $
    'D15', $
    'P1', $
    'P2', $
    'P3', $
    'P4' $
	 ]
  BGROUP145 = CW_BGROUP( BASE144, Btns1994, $
      ROW=2, $
      NONEXCLUSIVE=1, $
      LABEL_LEFT='Y', $
      UVALUE='BGROUP145')

  BASE144_1 = WIDGET_BASE(BASE144, $
      COLUMN=1, $
      FRAME=2, $
      MAP=1, $
      TITLE='Image', $
      UVALUE='BASE144_1')

  BASE144_2 = WIDGET_BASE(BASE144, $
      COLUMN=1, $
      FRAME=2, $
      MAP=1, $
      TITLE='PS', $
      UVALUE='BASE144_2')
  pick_PS = WIDGET_DROPLIST(BASE144_2, VALUE=['1','1/2','1/4'], $
        UVALUE='PICK_PS',TITLE='PS ratio')
  WIDGET_CONTROL,pick_PS,set_droplist_select = 1

;  Btns913 = ['#','P1','P2','P3','P4', $
;	     'D1','D2','D3','D4','D5','D6','D7','D8', $
;	     'D9','D10','D11','D12','D13','D14','D15']

; if detector for X axis is desired just comment out the following line
  Btns913 = ['#','P1','P2','P3','P4']

  pick_xaxis = WIDGET_DROPLIST(BASE144_1, VALUE=BTNS913, $
        UVALUE='PICK_XAXIS',TITLE='Xaxis')
  WIDGET_CONTROL,pick_xaxis,set_droplist_select = 1

   Btns912 = ['Small','Large', 'D1','D2','D3','D4','D5','D6','D7','D8', $
	     'D9','D10','D11','D12','D13','D14','D15']

  pick_image = WIDGET_DROPLIST(BASE144_1, VALUE=BTNS912, $
        UVALUE='PICK_IMAGE',TITLE='Images')


;  user_before = WIDGET_BUTTON( BASE144, $
;      UVALUE='USER_BEFORE', $
;      VALUE='Before_Scan')

;  user_after = WIDGET_BUTTON( BASE144, $
;      UVALUE='USER_AFTER', $
;      VALUE='After_Scan')

; set drawing area as wide as window width
win_state = WIDGET_INFO(MAIN13_1, /GEOMETRY)
WIDGET_CONTROL, DRAW61, DRAW_XSIZE=win_state.scr_xsize 

  WIDGET_CONTROL, MAIN13_1, /REALIZE


  ; Get drawable window index

  COMMON DRAW61_Comm, DRAW61_Id
  WIDGET_CONTROL, DRAW61, GET_VALUE=DRAW61_Id

@catcher_v1.init
scanData.release = '(R2.2.2c9+)'

; get start home work directory

  CD,'.',CURRENT=old_path
  scanData.home=old_path

;  default is acquisition mode now

	scanData.option = 1
	w_plotspec_id.mode = 0

if keyword_set(viewonly) then  begin
	 scanData.option = 0
	 scanData.viewonly = 1
end
 
; check for input file names
;

if keyword_set(config) then scanData.config = config

if keyword_set(data) then w_plotspec_array(3) = data 

if keyword_set(envfile) then scanData.envfile=envfile

if keyword_set(nosave) then scanData.nosave = 1 

  catch1d_scanInitSetup

WIDGET_CONTROL,widget_ids.trashcan, SET_VALUE = scanData.trashcan

if scanData.option eq 0 then catch1d_turnoffAcquisition

  XMANAGER, 'MAIN13_1', MAIN13_1, CLEANUP='catcher_close'
;  XMANAGER, 'MAIN13_1', MAIN13_1, CLEANUP='catcher_close',NO_BLOCK=0


END
