;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
; $Id: xdisplayfile.pro,v 1.8 2004/04/14 17:51:11 cha Exp $
; Copyright (c) 1991-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.

PRO WC,filename,nline,ncol
; simulate unix command WC to read an ASCII file
; nline - return the total # of lines in file
; ncol  - returns the totol # of data elements in a line
        openr,1,filename
        line=''
        nline=0
        while NOT EOF(1) do begin
        on_ioerror,close1
        readf,1,line
        nline=nline+1
        if nline eq 1 then begin
                y = strsplit(line,' ',/extract)
                ncol = n_elements(y)
        end
        end
close1:
        close,1
END

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
	WC,FILENAME,y

	lines=long(y(0))
	if lines eq 0 then begin
	res=WIDGET_MESSAGE('Unable to display '+FILENAME)
	return
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

label=WIDGET_LABEL(filebase,value=TITLE(0))
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

