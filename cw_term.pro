;+
; NAME:
;	cw_term
;
; PURPOSE:
;      writtable text window widget
;
; CATEGORY:
;	Compound widgets.
;
; CALLING SEQUENCE:
;	widget_id = CW_TERM(parent)
;
; INPUTS:
;       PARENT - The ID of the parent widget.
;
; KEYWORD PARAMETERS:
;	BG_NAMES:	An array of strings to be associated with
;			each button and returned in the event structure as VALUE.
;	BGEVENT_FUNCT:	The name of an user-supplied event function 
;			for the buttons. This function is called with the return
;			value structure whenever a button is pressed, and 
;			follows the conventions for user-written event
;			functions.
;	FONT:		The name of the font to be used for the text output 
;			If this keyword is not specified, the default
;			font is used.
;	FRAME:		Specifies the width of the frame to be drawn around
;			the base.
;       FILENAME:       Copy contents of file into widget
;       RESET:          Clear existing widget contents and write new value/file.
;                       The parent widget is the existing widget id. 
;	MAP:		If set, the base will be mapped when the widget
;			is realized (the default).
;	SCROLL:		If set, the base will include scroll bars to allow
;			viewing a large text area through a smaller viewport.
;	SET_VALUE:	The initial value of the text widget. This is equivalent
;			to the later statement:
;
;			WIDGET_CONTROL, widget, set_value=value
;
;       TITLE:          New Window title
;	UVALUE:         The user value for the compound widget
;
;	XSIZE:		The width of the text widget
;	YSIZE:		The height of the text widget
;
; OUTPUTS:
;       The ID of the created widget is returned.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;
; PROCEDURE:
;	WIDGET_CONTROL, id, SET_VALUE=value can be used to change the
;		current value displayed by the widget.
;
;	WIDGET_CONTROL, id, GET_VALUE=var can be used to obtain the current
;		value displayed by the widget.
;
; MODIFICATION HISTORY:
;  01  8-9-95  jps  	modified from idl's cw_tmpl.pro
;  02  5-13-99  bkc  	modified check for 80 columns for enscript print on unix
;  03 11-23-99  bkc     added findpath function, and file destination directory 
;-


FUNCTION findpath,dir,print=print,create=create
; return  -1 not found, 1 found, 0 created
;
if n_elements(dir) eq 0 then begin
	print,'Usage:   check_dir,dirname'
	return,-1
	end

	catch,err_status
	if err_status ne 0 then begin
	if keyword_set(create) then begin
		spawn,!os.mkdir + ' '+dir
		print,'Tried to create the path directory '+dir+ ' !!!'
		return,0
	endif else begin
		print,'Error: The path directory '+dir+ ' not found!!!'
		return,-1
	end
	end

	pushd,dir
	popd
	if keyword_set(print) then begin
	cd,current=p
	print,'Current path: ',p
	end
	return,1
END

PRO cwterm_Save_Event, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM

  WIDGET_CONTROL,Event.Top,GET_UVALUE=info
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'CWTERM_SAVEFILE': BEGIN
      END
  'CWTERM_DESTDIR': BEGIN
	WIDGET_CONTROL,info.dir, GET_VALUE=dir
	if strtrim(dir(0),2) ne '' then begin
	ret = findpath(dir(0),/create)
	end
      END
  'CWTERM_MKDIRHELP': BEGIN
	str =['If the destination directory path does not exist yet,', $
		'enter the Dest Path field and depress the RETURN key will', $
		'create the destination directory for you.']
	res = dialog_message(str,/Info)
	return
      END
  'CWTERM_SAVEACCEPT': BEGIN
	WIDGET_CONTROL,info.dir, GET_VALUE=dir
	ret = findpath(dir(0),/create)
	WIDGET_CONTROL,info.newname, GET_VALUE=newname
	filename = strtrim(dir,2)+strtrim(newname,2)

	if strtrim(filename(0),2) ne '' then begin
	found = findfile(filename(0))
	if found(0) ne '' then begin
		WIDGET_CONTROL,info.base,/DESTROY
		st = [ 'File: '+filename(0),' already existed!', $
		'',	'ASCII data is saved in ',info.oldname]
		res = widget_message(st,/info)
		return
	end
	spawn,[OS_SYSTEM.mv, info.oldname, filename(0)],/noshell
	WIDGET_CONTROL,info.base,/DESTROY
	end
      END
  'CWTERM_SAVECANCEL': BEGIN
	WIDGET_CONTROL,info.base,/DESTROY
      END
  ENDCASE
END

;
; if filename specifies the default file name used by the cw_term, 
;     it will be override by the textfield entered by the user
;
PRO cwterm_save_dialog, GROUP=Group,oldname=oldname, rename=rename

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  if keyword_set(rename) eq 0 then begin
	res = dialog_message('Error: rename file is required.',/Error)
  	return
  end

  len = rstrpos(rename,!os.file_sep)+1
  dir = strmid(rename,0,len)
  name = strmid(rename,len,strlen(rename)-len)

  cwterm_Save = WIDGET_BASE(GROUP_LEADER=Group, $
	TITLE='CW_TERM Save File', $
      COLUMN=1, $
      MAP=1, $
      UVALUE='cwterm_Save')

  BASE2 = WIDGET_BASE(cwterm_Save, $
      TITLE='CW_TERM SaveFile', $
      column=1, $
      MAP=1, $
      UVALUE='BASE2')

  FIELD2 = CW_FIELD( BASE2,VALUE=dir, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Dest Path:', $
      UVALUE='CWTERM_DESTDIR', $
      XSIZE=60)

  FIELD3 = CW_FIELD( BASE2,VALUE=name, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='File:', $
      UVALUE='CWTERM_SAVEFILE', $
      XSIZE=60)

  BASE4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  CWTERM_SAVE_BUTTON5 = WIDGET_BUTTON( BASE4, $
      UVALUE='CWTERM_SAVEACCEPT', $
      VALUE='Accept')

  CWTERM_SAVE_HELP = WIDGET_BUTTON( BASE4, $
      UVALUE='CWTERM_MKDIRHELP', $
      VALUE='Help...')

  CWTERM_SAVE_BUTTON6 = WIDGET_BUTTON( BASE4, $
      UVALUE='CWTERM_SAVECANCEL', $
      VALUE='Cancel')

  info = {  $
	base : cwterm_Save, $
	dir : FIELD2, $
	oldname: oldname, $
	newname: FIELD3 $
	}

  WIDGET_CONTROL, cwterm_Save, SET_UVALUE=info
  WIDGET_CONTROL, cwterm_Save, /REALIZE

  XMANAGER, 'cwterm_Save', cwterm_Save
END

PRO term_set_value, id, value

	; This routine is used by WIDGET_CONTROL to set the value for
	; your compound widget.  It accepts one variable.  
	; You can organize the variable as you would like.  If you have
	; more than one setting, you may want to use a structure that
	; the user would need to build and then pass in using 
	; WIDGET_CONTROL, compoundid, SET_VALUE = structure.

	; Return to caller.
  ON_ERROR, 2

	; Retrieve the state.
   stash = WIDGET_INFO(id, /CHILD)
   WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY, BAD_ID=bad_id

    IF (N_ELEMENTS(value) NE 0) THEN BEGIN
	   WIDGET_CONTROL, state.text_id, $
				SET_VALUE=value, $
				/APPEND, $
				BAD_ID=bad_id, $
				/NO_COPY
    ENDIF
   
   WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY, BAD_ID=bad_id

END



FUNCTION term_get_value, id, value

	; This routine is by WIDGET_CONTROL to get the value from 
	; your compound widget.  As with the set_value equivalent,
	; you can only pass one value here so you may need to load
	; the value by using a structure or array.

	; Return to caller.
  ON_ERROR, 2

	; Retrieve the structure from the child that contains the sub ids.
  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY, BAD_ID=bad_id

	; Get the value here
  WIDGET_CONTROL, state.text_id, GET_VALUE=ret, BAD_ID=bad_id

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY, BAD_ID=bad_id
  
        ; Return the value here.
  RETURN,ret
END

;-----------------------------------------------------------------------------

FUNCTION term_event, event
COMMON SYSTEM_BLOCK,OS_SYSTEM

  parent=event.handler

  WIDGET_CONTROL, event.id, GET_UVALUE=Ev

		; Retrieve the structure from the child that contains the sub ids.
  stash = WIDGET_INFO(parent, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY,  BAD_ID=bad_id
  fileName = state.win_file

  CASE Ev OF 

  'MAIN': BEGIN
      END
  'BGROUP':BEGIN
	CASE event.value OF
	  'Save As...': BEGIN
		if XRegistered('cwterm_Save') eq 0 then $
;		cwterm_save_dialog,GROUP=Event.id, $
;			rename=state.rename,oldname=fileName
 	p = strpos(state.rename,!os.file_sep,/reverse_search)
	path = strmid(state.rename,0,p)
	new = strmid(state.rename,p+1,strlen(state.rename)-p-1)
 	p = strpos(fileName,!os.file_sep,/reverse_search)
	old = strmid(fileName,p+1,strlen(fileName)-p-1)
	WIDGET_CONTROL,Event.top,/DESTROY,BAD_ID=bad
	rename_dialog,path,old,new
	      END
	  'Close': BEGIN
	      WIDGET_CONTROL, parent, DESTROY=1, BAD_ID=bad_id
	      END
	  'Clear': BEGIN
		  WIDGET_CONTROL, state.text_id, SET_VALUE='', BAD_ID=bad_id
	      END
	  'Print': BEGIN
		  ANS = WIDGET_MESSAGE('Are you sure ?',/QUESTION, $
			/DEFAULT_NO, DIALOG_PARENT=Event.top)
		  IF ANS EQ 'Yes' THEN BEGIN
		  WIDGET_CONTROL, state.text_id, GET_VALUE=value, BAD_ID=bad_id
		width = max(strlen(value))
			; open the scratch file for printing
		  fileName = state.win_file
		  OPENW, unit, fileName, /GET_LUN, ERROR=error	;
	    	  IF error LT 0 THEN BEGIN		;OK?
		     print, [ !err_string, ' Can not display ' + filename]  ;No
		  ENDIF ELSE BEGIN	
		     printf,unit, FORMAT='(A)',value
	     	     FREE_LUN, unit			;free the file unit.
		     if OS_SYSTEM.os_family eq 'unix' then begin
		     if OS_SYSTEM.printer ne '' then begin
			str=OS_SYSTEM.prt+' '+OS_SYSTEM.printer+' '+fileName
		     	if width gt 80 then $
			str=OS_SYSTEM.prt+' '+OS_SYSTEM.printer+' -r '+fileName	
		     endif else begin
			str = OS_SYSTEM.prt + ' ' + fileName
		     	if width gt 80 then $
			str = OS_SYSTEM.prt + ' -r ' + fileName 
		     end
		     	spawn,str
		     	spawn,[OS_SYSTEM.rm, '-f', fileName], /noshell
		     endif else begin
		     	spawn,[OS_SYSTEM.prt, fileName]
		     	spawn,[OS_SYSTEM.rm, fileName]
		     end
		  ENDELSE
		  END
	      END
	   ELSE: 
	ENDCASE
      END
  'TEXT': BEGIN
      End
  ENDCASE

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY,  BAD_ID=bad_id

  RETURN, { ID:parent, TOP:event.top, HANDLER:0L }
END

;-----------------------------------------------------------------------------

FUNCTION cw_term, parent, SET_VALUE=value, $
	COLUMN=column, TITLE=title, $
	FILENAME=filename, $
	RENAME = rename, $
        RESET=reset, $
	BG_NAMES = bg_names, BGEVENT_FUNCT = bg_efun, $
	FONT=font, FRAME=frame, $
	MAP=map, SENSITIVE=sense, $
	ROW=row, SCROLL=scroll, SPACE=space, UVALUE=uvalue, $
	XSIZE=xsize, YSIZE=ysize

COMMON SYSTEM_BLOCK,OS_SYSTEM

  IF (N_PARAMS() LT 1) THEN MESSAGE, 'Must specify a parent for cw_term.'

  ON_ERROR, 2					;return to caller

	; Defaults for keywords
  version = WIDGET_INFO(/version)
  if (version.toolkit eq 'OLIT') then def_space_pad = 4 else def_space_pad = 3
  IF NOT (KEYWORD_SET(append))  THEN append = 0
  IF NOT (KEYWORD_SET(xsize)) THEN xsize = 80
  IF NOT (KEYWORD_SET(ysize)) THEN ysize = 24
  IF NOT (KEYWORD_SET(reset)) THEN reset = 0

;  IF (N_ELEMENTS(value) eq 0) 	then value = ''
  IF (N_ELEMENTS(Title) eq 0) 	 	then Title = ''
  IF (N_ELEMENTS(column) eq 0) 		then column = 0
  IF (N_ELEMENTS(frame) eq 0)		then frame = 0
  IF (N_ELEMENTS(map) eq 0)		then map=1
  IF (N_ELEMENTS(row) eq 0)		then row = 0
  IF (N_ELEMENTS(scroll) eq 0)		then scroll = 0
  IF (N_ELEMENTS(sense) eq 0)		then sense = 1
  IF (N_ELEMENTS(uvalue) eq 0)		then uvalue = 0



; File read section copied from XDISPLAYFILE utility
;	Written By Steve Richards, December 1990
;	Graceful error recovery, DMS, Feb, 1992.
;       12 Jan. 1994  - KDB
;               If file was empty, program would crash. Fixed.
;       4 Oct. 1994     MLR Fixed bug if /TEXT was present and /TITLE was not.
;      14 Jul. 1995     BKC Increased the max line to variable size.
;      16 Jun. 1997     BKC Max line set to 10000, os system check.
;      18 Dec. 1997     BKC add the save file event.

  IF(KEYWORD_SET(filename)) THEN BEGIN

    IF(NOT(KEYWORD_SET(TITLE))) THEN TITLE = filename     
    OPENR, unit, filename, /GET_LUN, ERROR=i		;open the file and then
    IF i LT 0 THEN BEGIN		;OK?
	text = [ !err_string, ' Can not display ' + filename]  ;No
    ENDIF ELSE BEGIN

    y=10000
    if OS_SYSTEM.os_family eq 'unix' then  spawn,[OS_SYSTEM.wc,'-l',FILENAME],y,/noshell

	text = strarr(y(0))				;Maximum # of lines
	i = 0L
	c = ''
	WHILE not eof(unit) do BEGIN
		READF,unit,c
		text(i) = c
		i = i + 1
		if i ge y(0) then goto,stopread
	ENDWHILE
    stopread:
	value = text(0:(i-1)>0)  ;Added empty file check -KDB
	FREE_LUN, unit			;free the file unit.
    ENDELSE
  ENDIF ELSE BEGIN
    IF(NOT(KEYWORD_SET(TITLE))) THEN TITLE = 'Term'
  ENDELSE

  winFile = ''
  if n_elements(filename) then winFile=filename
  winTitle = title
 
 IF reset EQ 0 THEN BEGIN

  if n_elements(rename) then $
	  state = { main_id:0L, group_leader:0L, $
			rename : rename, $
		    bgroup_id:0L, text_id:0L, win_file:winFile } else $
	  state = { main_id:0L, group_leader:0L, $
		    bgroup_id:0L, text_id:0L, win_file:winFile }

	  MAIN = WIDGET_BASE( $
			    GROUP_LEADER=parent, $
			    UVALUE = uvalue, $
			    TITLE=winTitle, $
			    MAP=map, $
			    EVENT_FUNC = "term_event", $
			    FUNC_GET_VALUE = "term_get_value", $
			    PRO_SET_VALUE = "term_set_value", $
			    /COLUMN)
		
	  state.main_id = MAIN
	  state.group_leader = parent

	  ; Create text widget
	  IF (N_ELEMENTS(font) EQ 0) THEN BEGIN
	      state.text_id = WIDGET_TEXT( MAIN, $
	      XSIZE=xsize, $
	      YSIZE=ysize, $
	      /NO_COPY, $
	      SCROLL=scroll)
	  ENDIF ELSE BEGIN
	      state.text_id = WIDGET_TEXT( MAIN, $
	      XSIZE=xsize, $
	      YSIZE=ysize, $
	      /NO_COPY, $
	      SCROLL=scroll, $
	      FONT=font)
	  ENDELSE


	  IF (N_ELEMENTS(value) NE 0) THEN $
		WIDGET_CONTROL, state.text_id, SET_VALUE=value

	  ; Standard control buttons
	  N_BUTTONS = 3
	  buttons = STRARR(N_BUTTONS+N_ELEMENTS(bg_names))
	  buttons(0:N_BUTTONS-1) = ['Print','Clear','Close']

	
	  ; User control buttons
	  IF N_ELEMENTS(bg_names) NE 0 THEN BEGIN
 	   buttons(N_BUTTONS:N_BUTTONS+N_ELEMENTS(bg_names)-1) = bg_names(*)
	  ENDIF

	  ; Create control buttons
	  state.bgroup_id = CW_BGROUP( MAIN, buttons, $
				      /ROW, $
				      /RETURN_NAME, $
				      EVENT_FUNCT=bg_efun, $
				      FRAME=frame, $
				      UVALUE='BGROUP')

	  ; Save out the initial state structure into the first childs UVALUE.
	  WIDGET_CONTROL, WIDGET_INFO(MAIN, /CHILD), SET_UVALUE=state, /NO_COPY

	  WIDGET_CONTROL, MAIN, /REALIZE 

  ENDIF ELSE BEGIN
		; Retrieve the structure from the child that contains the sub ids.
	  IF  WIDGET_INFO(parent, /VALID_ID) THEN BEGIN
	      stash = WIDGET_INFO(parent, /CHILD)
	      WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY, BAD_ID=bad_id
              state.win_file= winFile

	      IF (N_ELEMENTS(value) eq 0) 	then value = ''	  

	      WIDGET_CONTROL, state.text_id, SET_VALUE=value, BAD_ID=bad_id

	      WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY, BAD_ID=bad_id
	 ENDIF

         MAIN = parent
   ENDELSE

	; value is all the user will know about the internal structure
	; of your widget.
  RETURN, MAIN

END



