;
; this routine is requied for generate the runtime executable
;
PRO os_init
;+
; NAME:
;	OS_INIT
;
; PURPOSE:
;       Defines the structure of main operating system variables. All the 
;       operating system dependent varibles used in data catcher are 
;       assembled in this routine. 
;
; CATEGORY:
;       Global System Variables !os.
;
; CALLING SEQUENCE:
;       OS_INIT
;
; PARAMETER FIELDS:
;       ARCH:           IDL detected operating system architecture
;       OS:             IDL detected operating system 
;       OS_FAMILY:      IDL detected operating system family
;       RELEASE:        IDL release number
;       FONT:           Bold font used in highlight label in dialog
;       DEVICE:         Default output device
;       FILE_SEP:       Operating sytem file separator, '/' for unix '\' for W95
;       CHMOD:          Command change file permission mode, 'chmod'
;       MV:             Command rename file, 'mv' for unix, 'rename' for W95
;       CP:             Command copy file, 'cp' for unix, 'copy' for W95
;       RM:             Command remove file, 'rm' for unix, 'del' for W95
;       LPR:            Command print PS file, 'lpr' for unix, 'print' for W95
;       PRT:            Command print text file, 'enscript' for unix, 'print' for W95
;       PRINTER:        Default printer name, '' 
;       WC:             Command return line count, 'wc' for unix
;
; COMMON BLOCKS:
;       COMMON SYSTEM_BLOCK
;
; SIDE EFFECTS:
;       This routine defines the OS_SYSTEM structure and the global 
;       system variable !os. All the system dependent varialbes used in
;       data catcher and data viewer are kept in this routine.
;
; RESTRICTIONS:
;       Current version works for Unix and W95 operating system. 
;
; PROCEDURE:
;       Porting to other operating system, the corresponding
;       fields in 'os.init' may need to be modified accordingly.
;
; EXAMPLE:
;
;               OS_INIT
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 6-01-97.
;       xx-xx-xx iii  - comment
;-
@os.init
END

;
; PS_open,'name.ps'               name defalut to idl
;
PRO PS_init
COMMON PRINTER_BLOCK,printer_info
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

if n_elements (printer_info) eq 0 then $
  printer_info = { $
	name: '', $
	color: 1, $
	reverse: 0, $
	base: 0L, $
	ptr_field:0L }
; inherit from the parent process
;if n_elements(r_curr) eq 0 then begin
;	LOADCT,39  
;	end
END

PRO PS_open,psfile,TV=TV
;+
; NAME:
;       PS_OPEN
;
; PURPOSE:
;       This routine sets the current graphics device to PostScript.
;       and saves plot in a user specified PostScript file.
;
; CALLING SEQUENCE:
;       PS_OPEN, 'myfile.ps' [,/TV]
;
; INPUTS:
;       myfile.ps:  Specifies the PostScript filename to be saved.
;
; OUTPUTS:
;       The PostScript graphic output is saved which can be sent to
;       any PostScript printer or viewer. 
;
; KEYWORD PARAMETERS:
;       TV:       Specifies whether reverse color video to be used in PS. 
;
; COMMON BLOCKS:
;       COMMON PRINTER_BLOCK
;       COMMON COLORS 
;
; RESTRICTIONS:
;       The program 'PS_open.pro' must be loaded into IDL first before 
;       calling this routine.
;
; EXAMPLE:
;
;        PS_OPEN, 'myfile.ps'
;        tvscl,scan
;        PS_CLOSE
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       07-28-97   bkc  Add the support for color PostScript
;                       Add the support for reverse video
;                       Add handling capability for different operating system
;       05-15-98   bkc  Change the reverse video to reverse legend color for
;                       2D TV plot, to get reverse video use the xloadct's
;                       option, reverse feature  
;-

COMMON PRINTER_BLOCK,printer_info
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

	PS_init
	set_plot,'PS'
	!P.FONT=-1
	if (n_elements(psfile) ne 0) then begin

	if keyword_set(TV) then begin 

	; use xloadct reverse video, reverse legend only  


	    if printer_info.color gt 0 then $
		device,filename=psfile,/color,bits=8, $
			/Courier,/Bold, $
			 yoffset=7, xsize=15, ysize=15  else  $
		device,filename=psfile,/Courier,/Bold
	endif else begin
	    if printer_info.color gt 0 then $
		device,filename=psfile,/color,bits=8, $
			/Courier,/Bold, $
			yoffset=7, xsize=17.78, ysize=12.7  else $
		device,filename=psfile,/Courier,/Bold
	end

	end
END

PRO PS_close
;+
; NAME:
;       PS_CLOSE
;
; PURPOSE:
;       This routine closes the PostScript output device and resets the
;       the original system graphic device as the output plot device.
;
; CALLING SEQUENCE:
;       PS_CLOSE
;
; INPUTS:
;       None.
;
; OUTPUTS:
;       None.
;
; KEYWORD PARAMETERS:
;       None.
;
; COMMON BLOCKS:
;       COMMON SYSTEM_BLOCK
;       COMMON COLORS 
;
; RESTRICTIONS:
;       The program 'PS_open.pro' must be loaded into IDL prior calling
;       this routine. 
;
; EXAMPLE:
;
;        PS_OPEN, 'myfile.ps'
;        tvscl,scan
;        PS_CLOSE
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       07-28-97   bkc  Add the support for reverse PostScript color scheme. 
;                       Add handling capability for different operating system
;-

COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

	if !d.name eq 'PS' then begin
	!P.FONT=-1
	device,/close

	r_curr = r_orig
	g_curr = g_orig
	b_curr = b_orig
	TVLCT,r_orig,g_orig,b_orig

	set_plot,OS_SYSTEM.device
	end
END

PRO PS_enscript,fileName
;+
; NAME:
;       PS_ENSCRIPT
;
; PURPOSE:
;       This routine uses the system printing command to print
;       an ASCII text file. On unix operating system the command
;       'enscript -r' is used. 
;
; CALLING SEQUENCE:
;       PS_ENSCRIPT, 'filename'
;
; INPUTS:
;       filename : Specifies the ASCII filename to be printed.
;
; OUTPUTS:
;       A copy of the specified file is sent to the user selected  
;       printer.
;
; KEYWORD PARAMETERS:
;
; COMMON BLOCKS:
;       COMMON SYSTEM_BLOCK
;
; RESTRICTIONS:
;       The program 'PS_open.pro' must be loaded into IDL prior calling
;       this routine. 
;
; EXAMPLE:
;
;        PS_ENSCRIPT, 'myfile'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       07-28-97   bkc  Add handling capability for different operating system
;-
COMMON SYSTEM_BLOCK,OS_SYSTEM

	if n_elements(fileName) eq 0 then begin
		print,'Usage: PS_enscript, <fileName>'
		return
	end
	if strtrim(fileName,2) eq '' then begin
		print,'Usage: PS_enscript, <fileName>'
		return
	end
	if OS_SYSTEM.os_family eq 'unix' then $
	spawn,[OS_SYSTEM.prt, OS_SYSTEM.printer ,  '-r', fileName], /noshell else $
	spawn,[OS_SYSTEM.prt, fileName, OS_SYSTEM.printer]
END

PRO PS_print,psfile
;+
; NAME:
;       PS_PRINT
;
; PURPOSE:
;       This routine uses the system printing command to print
;       a PostScript or ASCII text file. On the unix operating 
;       system the command 'lpr' is used. 
;
; CALLING SEQUENCE:
;       PS_PRINT, 'myfile.ps'
;
; INPUTS:
;       myfile:    Specifies either the PostScript or ASCII text filename 
;                  to be printed.
;
; OUTPUTS:
;       A copy of the specified file is sent to the user selected  
;       printer.
;
; KEYWORD PARAMETERS:
;
; COMMON BLOCKS:
;       COMMON SYSTEM_BLOCK
;
; RESTRICTIONS:
;       The program 'PS_open.pro' must be loaded into IDL prior calling
;       this routine. 
;
; EXAMPLE:
;
;        PS_PRINT, 'myfile.ps'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       07-28-97   bkc  Add handling capability for different operating system
;       05-14-98   bkc  Add the checking for unreadable color on the PS plot
;                       On unix if the color is too light use the gv to preview 
;			pops up setup printer and info dialog
;-
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON PRINTER_BLOCK,printer_info
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

	if (n_elements(psfile) ne 0) then begin 
		if strtrim(psfile,2) eq '' then begin
			print,'Usage: PS_print, <filename>'
			return
		end
	end else psfile = 'idl.ps'
	if OS_SYSTEM.os_family eq 'unix' then begin
        	str =  OS_SYSTEM.lpr + ' ' + OS_SYSTEM.printer +  psfile 
		color = r_curr(0) + g_curr(0)*256L + b_curr(0)*256L ^2
		if color ge 16777200 then begin 
			temp = ['Warning:','',$
			 'There may be problem of unreadable title or legend on PS plot.',$
			'The ghostview is brought up for you to preview the PS plot.', $
			'If the PS plot looks fine you may use the ghostview to send the',$
			'print job and then close the ghostview and Printer Setup Dialog.',$
			'','If you can not see the title and legend, please close', $
			'the ghostview program first, try different color table or set the ',$
			'Reverse_Legend_Color to "Y" in Printer Setup Dialog first then', $
			'try Print again'] 
			res=dialog_message(temp,/info,title='PS legend problem')
			PS_printer
			spawn,'gv '+psfile + ' &'
		endif else spawn,str
	endif else begin
		str = OS_SYSTEM.lpr + ' ' + psfile + ' ' + OS_SYSTEM.printer
	        spawn,str
	end
	print,str
END


PRO PS_printer_Event, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON PRINTER_BLOCK,printer_info

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'PS_REVERSE': BEGIN
	printer_info.reverse = Event.Index
      END

  'BGROUP3': BEGIN
      CASE Event.Value OF
      0: begin
		Print,'Button B/W Pressed'
		printer_info.color = 0
	 end
      1: begin
		Print,'Button Color Pressed'
		printer_info.color = 1
	 end
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END

  'FIELD5': BEGIN
      	WIDGET_CONTROL, printer_info.ptr_field, GET_VALUE=str
	printer_info.name = strtrim(str(0),2)
      END

  'BGROUP7': BEGIN
      CASE Event.Value OF
      0: begin
      		WIDGET_CONTROL, printer_info.ptr_field, GET_VALUE=str
		printer_info.name = strtrim(str(0),2)
      		WIDGET_CONTROL, printer_info.base, /DESTROY, BAD_ID=bad
	 end
      1: begin
      		WIDGET_CONTROL, printer_info.base, /DESTROY, BAD_ID=bad
	 end
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  ENDCASE

if printer_info.name ne '' then begin
	if OS_SYSTEM.os_family eq 'unix' then $
	OS_SYSTEM.printer = '-P'+printer_info.name + ' ' else $
	OS_SYSTEM.printer = printer_info.name 
end

END



PRO PS_printer, GROUP=Group
;+
; NAME:
;	PS_PRINTER
;
; PURPOSE:
;       This widget dialog allows the user to set up PostScript printer
;       and printer name to be used by the IDL session. Default setting
;       is color PS using default printer.
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;       PS_PRINTER [,GROUP=Group]
;
; INPUTS:
;       None.
;	
; KEYWORD PARAMETERS:
;       GROUP:  The widget ID of the group leader of the widget. If this 
;               keyword is specified, the death of the group leader results in
;               the death of PS_PRINTER.
;
; OUTPUTS:
;
; COMMON BLOCKS:
;       COMMON PRINTER_BLOCK
;
; SIDE EFFECTS:
;       Initially the system printer is set to the user's default 
;       printer. If a null printer name is specified, whatever the 
;       system printer was previously set will be used. 
;
; RESTRICTIONS:
;       The program 'PS_open.pro' must be loaded into IDL prior calling
;       this routine. 
;       
; PROCEDURE:
;      
; EXAMPLE:
;
;               PS_PRINTER
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 6-01-97.
;       10-15-97 bkc  - Add droplist Y/N for reverse color option
;                       Now it defaults to non reverse color option.
;       05-14-98 bkc  - Remove the B/W option, use the xloadct to select B/W
;                       Change reverse video to reverse legeng color if legend
;                       is in white color 
;-

COMMON PRINTER_BLOCK,printer_info

if XRegistered('PS_printer') then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  PS_init

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  PS_printer_base = WIDGET_BASE(GROUP_LEADER=Group, $
      TITLE='Setup Printer', $
      ROW=1, $
      MAP=1, $
      UVALUE='PS_PRINTER')
  printer_info.base = PS_printer_base 

  BASE2 = WIDGET_BASE(PS_printer_base, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
;	FONT=!os.font, $
      UVALUE='LABEL3', $
      VALUE='Setup PS Printer')

;  Btns167 = [ $
;    'B/W', $
;    'Color' ]
;  BGROUP3 = CW_BGROUP( BASE2, Btns167, $
;      ROW=1, $
;      EXCLUSIVE=1, $
;      LABEL_LEFT='Output PS', $
;      UVALUE='BGROUP3')
;  WIDGET_CONTROL,BGROUP3,SET_VALUE= printer_info.color

  Btn168 = ['N','Y']
  ps_reverse = WIDGET_DROPLIST(BASE2, VALUE=Btn168, $
        UVALUE='PS_REVERSE',TITLE='Reverse Legend Color')
  WIDGET_CONTROL,ps_reverse,SET_DROPLIST_SELECT=printer_info.reverse

  FieldVal269 = [ $
    '' ]
  FIELD5 = CW_FIELD( BASE2,VALUE=FieldVal269, $
      ROW=1, RETURN_EVENTS=1, $
      STRING=1, $
      TITLE='Printer Name', $
      UVALUE='FIELD5', $
      XSIZE=10)
  printer_info.ptr_field = FIELD5  
  if strtrim(printer_info.name,2) ne '' then $
  WIDGET_CONTROL,FIELD5,SET_VALUE=printer_info.name

  Btns342 = [ 'Accept', 'Cancel' ] 
  BGROUP7 = CW_BGROUP( BASE2, Btns342, $
      ROW=1, $
      UVALUE='BGROUP7')

  WIDGET_CONTROL, PS_printer_base, /REALIZE

  XMANAGER, 'PS_printer', PS_printer_base
END
;
;  To save platform independent data use the /XDR option on file open
;
;  u_openw, unit [,/XDR]  [,'filename']
;  u_openr, unit [,/XDR]  [,'filename']
;  u_write 
;  u_read
;  u_dump
;  u_rewind, unit
;
; Examples: 
;  u_openw, unit, 'filename'
;  u_write, unit, x
;  u_close, unit
;
;
;  u_dump
;
;  u_openr, unit, 'filename'
;  u_read, unit, x  &  print,x
;  u_close, unit
;

;PRO DebugError
;help,/struct,!error_state
;END

FUNCTION u_writePermitted,filename,VT=VT
;
; check for filename write permission
;
; existed
	found = findfile(filename)
	if found(0) ne '' then begin
	ret=''
	if keyword_set(VT) then $ 
	read,ret,prompt='Overwrite the existed file - '+filename+' (Yes/No) ?' else $
	ret= widget_message(['Do you want to overwrite the existed file : ',$
		'','     '+filename], $
			/question)
	if strupcase(ret) eq 'NO' then return,-1
	end
; create new
        CATCH,error_status
;        if !error_state.name eq 'IDL_M_CNTOPNFIL' then begin 
	if error_status eq -215 or error_status eq -206 then begin
		if keyword_set(VT) then $
		read,ret,prompt=!err_string+string(!err) else $
                ret=WIDGET_MESSAGE(!err_string + string(!err))
                if n_elements(unit) then u_close,unit
                return,-1
        end
	openw,1,filename
	close,1
	return,0
END

PRO u_rewind,unit
;+
; NAME:
;       U_REWIND
;
; PURPOSE:
;       This routine locates the LUN file pointer at the beginning of the 
;       file.
;
; CALLING SEQUENCE:
;
;       U_REWIND, Unit
;
; INPUTS:
;       Unit:     The LUN number to be rewind.
;
; OUTPUTS:
;       None.
;
; EXAMPLE:
;
;        U_REWIND, unit
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       07-25-97      bkc  Rename routine rewind to u_rewind     
;-
point_lun,unit,0
END

PRO u_openw,unit,filename,append=append,help=help,XDR=XDR,ERRCODE
;+
; NAME:
;       U_OPENW
;
; PURPOSE:
;       This routine assigns a LUN to the opened file for unformatted
;       write only.
;
; CALLING SEQUENCE:
;
;       U_OPENW, Unit, 'filename' [,/Append] [,/XDR] [,/Help]
;
; INPUTS:
;       filename: Specifies the filename to be created or opened for
;                 data recording through using the U_WRITE command.
;
; OUTPUTS:
;       Unit:     The LUN number to be associated with the opened file.
;
; KEYWORD PARAMETERS:
;       APPEND:   This keyword specifies that the file is opened for 
;                 data appending. If not specified, write on the unit
;                 will replace the old file content by the new data.
;       XDR:      This keyword specifies that the file is opened for 
;                 writing data in platform-independent XDR binary form. 
;       HELP:     If this keyword is set, a simple on line help is given.
;
; RESTRICTIONS:
;       The data file should contain only consistant type of binary 
;       objects: either native binary data or platform-independent 
;       XDR data. No mixture type is allowed.
;
; EXAMPLE:
;
;        U_OPENW, unit, 'catch1d.trashcan'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       xx-xx-xx      iii  comment     
;-
;
if keyword_set(help) then goto, help1
if n_elements(filename) eq 0 then filename='data.dat'
ERRCODE=0
        CATCH,error_status
;        if !error_state.name eq 'IDL_M_CNTOPNFIL' then begin 
	if !err eq -215 or !err eq -206 then begin
                ret=WIDGET_MESSAGE(!err_string + string(!err))
                if n_elements(unit) then u_close,unit
		ERRCODE=-99
;		exit
                return 
        end

if keyword_set(XDR) then begin
	if keyword_set(append) then $
	openw,/XDR,unit,filename,/GET_LUN,/APPEND else $
	openw,/XDR,unit,filename,/GET_LUN  
endif else begin
	if keyword_set(append) then $
	openw,unit,filename,/GET_LUN,/APPEND else $
	openw,unit,filename,/GET_LUN  
end

if n_params() eq 0 then print,'unit=',unit
return

help1:
	print,''
	print,'Usage:  u_openw, unit, filename'
	print,''
	print,'This routine assigns a LUN to the opened file for write only.'
	print,'        unit     - a LUN is returned by unit'
	print,"        filename - optional, default to 'data.dat'" 
	print,'e.g.'
	print,'       u_openw,unit'
	print,"       u_openw,unit,'data1.dat'"
	print,''
END

PRO u_openr,unit,filename,help=help,XDR=XDR
;+
; NAME:
;       U_OPENR
;
; PURPOSE:
;       This routine assigns a LUN to the opened file for unformatted
;       read only.
;
; CALLING SEQUENCE:
;
;       U_OPENR, Unit, 'filename' [,/XDR] [,/Help]
;
; INPUTS:
;       filename: Specifies the filename to be read by the U_READ 
;                 command.
;
; OUTPUTS:
;       Unit:     The LUN number to be associated with the opened file.
;
; KEYWORD PARAMETERS:
;       XDR:      This keyword specifies that the file is opened for 
;                 reading data in platform-independent XDR binary form. 
;       HELP:     If this keyword is set, a simple on line help is given.
;
; RESTRICTIONS:
;       The data file should contain only consistant type of binary 
;       objects: either native binary data or platform-independent 
;       XDR data. No mixture type is allowed.
;
; EXAMPLE:
;
;        U_OPENR, unit, 'catch1d.trashcan'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       xx-xx-xx      iii  comment     
;-
if keyword_set(help) then goto, help1
if n_elements(filename) eq 0 then filename='data.dat'

if keyword_set(XDR) then  $
	openr,/XDR,unit,filename,/GET_LUN $
else $
	openr,unit,filename,/GET_LUN

if n_params() eq 0 then print,'unit=',unit
return

help1:
	print,''
	print,'Usage:  u_openr, unit, filename '
	print,''
	print,'This routine assigns a LUN to the opened file for read only.'
	print,'        unit     - a LUN is returned by unit'
	print,"        filename - optional, default to 'data.dat'" 
	print,'e.g.'
	print,'       u_openr,unit'
	print,"       u_openr,unit,'data1.dat'"
	print,''
END

PRO u_close,unit
;+
; NAME:
;       U_CLOSE
;
; PURPOSE:
;       This routine closes a file LUN opened for unformmated I/O.
;
; CALLING SEQUENCE:
;
;       U_CLOSE, Unit
;
; INPUTS:
;       Unit:     The LUN number to be closed.
;
; OUTPUTS:
;       None.
;
; EXAMPLE:
;
;        U_CLOSE, unit
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       xx-xx-xx      iii  comment     
;-
close,unit
free_lun,unit
END

;
;  u_write, unit, array
;
PRO u_write,unit,array,help=help
;+
; NAME:
;       U_WRITE
;
; PURPOSE:
;       This routine writes an IDL data array to a file unit which is
;       opened for unformatted write. It supports all IDL data type except the
;       complex number and up to 2D array.  
;
; CALLING SEQUENCE:
;
;       U_WRITE, Unit, Var [,/Help]
;
; INPUTS:
;       Unit:   The logic unit number returned by file open for unformatted
;               write.
;
;       Var:    This variable holds the data  array to be written to
;               the opened file, it can be scaler, vector, or 2D array.
;	
; KEYWORD PARAMETERS:
;       HELP:   If this keyword is set, a simple on line help is given.
;
;
; RESTRICTIONS:
;       The data array can not be complex number. In order to make the
;       data to be read by the U_READ routine, all the data saved must 
;       be using this routine. 
;
; EXAMPLE:
;
;       Create the 'test.dat'  and write the variable X to the file
;
;         u_openw,unit,'test.dat'
;         u_write, unit, X
;         u_close,unit
;
;       Create or append X to the 'test.dat'  
;
;         u_openw,unit,'test.dat',/append
;         u_write, unit, X
;         u_close,unit
;
;       Create XDR platform independent data to the 'test.dat'  
;
;         u_openw,unit,/XDR,'test.dat'
;         u_write, unit, X
;         u_close,unit
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       05-30-97	bkc	Support opened file as XDR type data.
;-
;
if keyword_set(help) then goto, help1
if n_params() ne 2 then begin
	print,'Usage: u_write, unit, array'
	return
end
s = size(array)
if (s(0) eq 0) and (s(1) eq 0) then return    	; undefined
no = n_elements(s)
if s(no-2) eq 7 then begin
	len = strlen(array)
	if no eq 4 then s = [s,len(0)] else $		; vector
	if no eq 3 then s = [s,0,len]			; scalor
endif else begin
	if no eq 4 then s = [s,0] else $		; vector
	if no eq 3 then s = [s,0,0]			; scalor
end
writeu,unit,s(0:4),array
return

help1:
	print,''
	print,'Usage:  u_write, unit, array'
	print,''
	print,'This routine writes an array variable to the referenced unit in'
	print,'unformatted form.'
	print,'       unit     - required, a LUN already opened'
	print,"       array    - the array contents to be recorded" 
	print,''
	print,'Note:  For each array, two variables are written:'
	print,'       the long(5) size info array, and the array itself.'
	print,'       For string array each element must be exactly same size'
	print,' e.g.'
	print,'       u_write, unit, findgen(3,5)'
	print,"       u_write, unit, ,['111','222','333']"
	print,''
END


;
; dump unfomatted data ( which was written by u_write)
;
PRO u_dump,norecord,filename,data=data,help=help,XDR=XDR
ON_IOERROR,BAD
if keyword_set(help) then goto, help1
if n_elements(filename) eq 0 then filename='data.dat'

if keyword_set(XDR) then $
	openr,/XDR,unit,filename,/GET_LUN $
else $
	openr,unit,filename,/GET_LUN
	
s = lonarr(5)
norecord = 0
print,''
print,'DUMP FILE : ',filename
WHILE NOT EOF(unit) DO BEGIN
norecord = norecord + 1
u_read_set,unit,s,x
print,''
if keyword_set(data) then print,'<<',norecord,'>>  RECORDED SIZE & DATA:' else print,'<<',norecord,'>>  RECORDED SIZE INFO :'
print,'SIZE=',s
if keyword_set(data) then print,x
ENDWHILE
	goto, done
BAD:
	print,'Error encounted in'
	print,!ERR_STRING
done:
free_lun,unit
return

help1:
print,''
print,'Usage: u_dump, /data, norecord, filename '
print,''
print,'This routine dump the unformatted data from a file which was recored by u_write'
print,"       norecord - optional, if given it returns the number of records to the caller"
print,"       filename - optional, default to 'data.dat'"
print,"       /data    - optional, both size, and data arrays will be printed'"
print,''
print,' e.g.'
print,''
print,'       u_dump'
print,"       u_dump,no,'data1.dat',/data"
print,''
END


PRO u_read_set,unit,s,x,ERRCODE,help=help
if n_params() lt 3 then begin
	print,''
	print,'Usage: u_read_set, unit, s, x'
	print,''
	print,'This routine reads a set of two varaibles: size and array '
	print,'from the LUN unit. Note that the data must be recorded by the'
	print,'u_write routine.'
	print,'     where   S    LONG = Array(5), must be defined before calling this routine'
	print,'             X    returned array'
	print,'       ERRCODE    returned code, 0 for success, -99 for failure'
	return
	end

CATCH,error_status
if error_status  eq -229 or error_status eq -219 or error_status eq -184 then begin 
	str = [ !err_string + string(!err),'', $
		'Error: unable to read data, wrong type of file opened!!' ]
	ret=WIDGET_MESSAGE(str)
	return
	end

IF EOF(unit) THEN begin
	print,'Error! Error! Error!'
	print,'Error: wrong type or bad data encountered'
	return 
END
readu,unit,s

if (s(0) gt 1L) then begin	; two dim
	type = s(3)
	int = s(4)
endif else if (s(0) eq 1) then begin    ; one dim
	type = s(2)
	int = s(3)
endif else if (s(0) eq 0) then begin     ; scalor
	type = s(1)
	int = s(2)
end
;print,s
;print,'type=',type, '  dim=',int
int2 = int/s(1)
case fix(type) of 
	1: if (s(0) eq 2) then begin		; byte
		x = make_array(s(1),s(2),/byte) 
	   endif else begin
		x = bytarr(fix(int))  	
	   end
	2: if (s(0) eq 2) then begin		; int
		x = make_array(s(1),s(2),/int) 
	   endif else begin
		x = intarr(fix(int))  	
	   end
	3: if (s(0) eq 2) then begin		; long 
		x = make_array(s(1),s(2),/long) 
	   endif else begin
		x = lonarr(fix(int))  	
	   end
	4: if (s(0) eq 2) then begin		; float
		x = make_array(s(1),s(2),/float) 
	   endif else begin
		x = fltarr(fix(int))  	
	   end
	5: if (s(0) eq 2) then begin		; double 
		x = make_array(s(1),s(2),/double) 
	   endif else begin
		x = make_array(fix(int),/double)  	
	   end
	6: if (s(0) eq 2) then begin		; complex
		x = make_array(s(1),s(2),/complex) 
	   endif else begin
		x = make_array(fix(int),/complex)  	
	   end
	7: if (s(0) eq 2) then begin		; string
		print,'Error u_write/u_read can only support single string array'
		print,'size=',s
		return
	   endif else begin
		x = make_array(fix(int),/string,value=string(replicate(32b,s(4))))  	
	   end
else: begin
	print,'type=',type
;		ret=WIDGET_MESSAGE('Error: wrong type of data read in!!')
;		retall
		return
	end
endcase

	readu,unit,x
ERRCODE = 0
END


PRO u_read,unit,x,ERRCODE,help=help
;+
; NAME:
;       U_READ
;
; PURPOSE:
;       This routine reads an unformatted data entity from a file unit which is
;       opened for unformatted read. It supports all IDL data type except the
;       complex number and up to 2D array.  
;
; CALLING SEQUENCE:
;
;       U_READ, Unit, Var [,ERRCODE ,/Help]
;
; INPUTS:
;       Unit:   The logic unit number returned by file open for unformatted
;               read.
;	
; KEYWORD PARAMETERS:
;       HELP:   If this keyword is set, a simple on line help is given.
;
; OUTPUTS:
;       Var:    This variable holds the right type of data obtained from 
;               the opened file, it can be either 1D vector, or 2D array.
;   ERRCODE:    This variable holds the error code for the u_read. It
;               returns 0 if succeeded, returns -99 if failed.
;
; RESTRICTIONS:
;       All the data must be created by the U_WRITE routine in order to be 
;       read by this routine.
;
; EXAMPLE:
;
;       Read the first data entity from the 'test.dat' which was previously
;       created by the U_WRITE routine.
;
;         u_openr,unit,'test.dat'
;         u_read, unit, X
;         u_close,unit
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       05-30-97	bkc	Support opened file as XDR type data.
;       10-13-97	bkc	Add the ERRCODE to indicate success or failure.
;-

ERRCODE = -99
if keyword_set(help) then goto, help1
if n_params() lt 2 then begin
	print,'Usage: u_read, unit, array'
	return
	end
s = lonarr(5)
IF NOT EOF(unit) THEN  u_read_set,unit,s,x,ERRCODE ;ELSE print,'EOF on unit ',unit
return

help1:
	print,''
	print,'Usage: u_read, unit, x'
	print,''
	print,'This routine reads an array from the LUN unit.'
	print,'Note that the data must be recorded by the u_write routine.'
	print,'       X    returned array'

END


PRO u_bi2xdr,filename,help=help,VT=VT
;+
; NAME:
;       U_BI2XDR
;
; PURPOSE:
;       This IDL routine converts native binary data into platform-independent
;       XDR binary data. 
;
;       The input file should contain only pure native binary data.
;       The output filename uses the input filename suffixed with '.xdr'.
;
; CALLING SEQUENCE:
;
;       U_BI2XDR, 'filename' [,/VT] [,/Help]
;
; INPUTS:
;       filename:   The data file should contain pure binary data objects.
;
; OUTPUTS:
;       filename.xdr:   Output file. 
;                   It contains the converted XDR binary data objects.
;
; KEYWORD PARAMETERS:
;       VT:     If a dumb terminal without X window server is used, 
;               this option must be set, e.g. a telnet session.
;       HELP:   If this keyword is set, a simple on line help is given.
;
; RESTRICTIONS:
;       The input data file should contain pure binary data objects.
;
; EXAMPLE:
;
;        U_BI2XDR,'catch1d.trashcan'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 05-30-97.
;
;       xx-xx-xx      iii  comment     
;-
;

if n_elements(filename) eq 0 or keyword_set(help) then goto,help1

	found = findfile(filename)
	if found(0) eq '' then begin
		print,'Error: '+filename+' not found!'
		return
		end
	if keyword_set(VT) then $
	OK_WRITE = u_writePermitted(filename+'.xdr',/VT) else $
	OK_WRITE = u_writePermitted(filename+'.xdr')
	if OK_WRITE lt 0 then return

        id=0
        u_openr,unit,filename
;        u_openw,unit2,filename+'.xdr',/XDR
	openw,/XDR,unit2,filename+'.xdr',/GET_LUN  

        WHILE NOT  EOF(unit) DO BEGIN
        id = id + 1
        u_read,unit,x
	u_write,unit2,x
        END
        maxno = id
        u_close,unit
        u_close,unit2
	if keyword_set(VT) then $
	print,string(maxno)+' sets of binary objects saved in "'+ filename+'.xdr"' else $
        ret=WIDGET_MESSAGE(string(maxno)+' sets of binary objects saved in "'+ $
		filename+'.xdr"')

	return

help1:

	print,''
	print,'Usage: U_BI2XDR,"filename"'
	print,''
	print,'This program converts the pure binary data objects into XDR binary format.'
	print,'The file "filename.xdr" created will be IDL platform independent.'
	print,''
END

PRO u_xdr2bi,filename,help=help,VT=VT
;+
; NAME:
;       U_XDR2BI
;
; PURPOSE:
;       This IDL routine converts platform-independent XDR data into
;       native binary data. 
;
;       The output filename uses the input filename suffixed with '.2bi'.
;
; CALLING SEQUENCE:
;
;       U_XDR2BI, 'filename' [,/VT] [,/Help]
;
; INPUTS:
;       filename:   The data file should contain XDR binary data objects.
;
; OUTPUTS:
;       filename.2bi:   Output file. 
;                   It contains the converted native binary data objects.
;
; KEYWORD PARAMETERS:
;       VT:     If a dumb terminal without X window server is used, 
;               this option must be set, e.g. a telnet session.
;       HELP:   If this keyword is set, a simple on line help is given.
;
; RESTRICTIONS:
;       The XDR input file should be created by the u_write command.
;
; EXAMPLE:
;
;        U_XDR2BI,'catch1d.trashcan.xdr'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 08-10-98.
;
;       xx-xx-xx      iii  comment     
;-
;

if n_elements(filename) eq 0 or keyword_set(help) then goto,help1

	found = findfile(filename)
	if found(0) eq '' then begin
		print,'Error: '+filename+' not found!'
		return
		end
	if keyword_set(VT) then $
	OK_WRITE = u_writePermitted(filename+'.2bi',/VT) else $
	OK_WRITE = u_writePermitted(filename+'.2bi')
	if OK_WRITE lt 0 then return

        id=0
        u_openr,unit,filename,/XDR
        u_openw,unit2,filename+'.2bi'

        WHILE NOT  EOF(unit) DO BEGIN
        id = id + 1
        u_read,unit,x
	u_write,unit2,x
        END
        maxno = id
        u_close,unit
        u_close,unit2
	if keyword_set(VT) then $
	print,string(maxno)+' sets of binary objects saved in "'+ filename+'.2bi"' else $
        ret=WIDGET_MESSAGE(string(maxno)+' sets of binary objects saved in "'+ $
		filename+'.2bi"')

	return

help1:

	print,''
	print,'Usage: U_XDR2BI,"filename"'
	print,''
	print,'This program converts the xdr data objects into native binary format.'
	print,'A new file "filename.2bi" will be created.'
	print,''
END

; $Id: ez_fit.pro,v 1.5 1998/12/22 19:30:59 cha Exp $

; Copyright (c) 1991-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
PRO XDispFile_evt, event


WIDGET_CONTROL, event.top, GET_UVALUE = state
WIDGET_CONTROL, event.id, GET_UVALUE=Ev

CASE Ev OF 
'EXIT': WIDGET_CONTROL, event.top, /DESTROY
'FILE_PRINT': begin
	WIDGET_CONTROL,state.text_area,GET_VALUE=str
	openw,unit,'tmp',/GET_LUN
	for i=0,n_elements(str)-1 do printf,unit,str(i)
	FREE_LUN,unit
	PS_print,'tmp'
	end
ENDCASE
END


PRO XDisplayFile, FILENAME, TITLE = TITLE, GROUP = GROUP, WIDTH = WIDTH, $
		HEIGHT = HEIGHT, TEXT = TEXT, FONT = font
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
;-
COMMON SYSTEM_BLOCK,OS_SYSTEM
                                                        ;use the defaults if
IF(NOT(KEYWORD_SET(HEIGHT))) THEN HEIGHT = 24		;the keywords were not
IF(NOT(KEYWORD_SET(WIDTH))) THEN WIDTH = 80		;passed in

IF(NOT(KEYWORD_SET(TEXT))) THEN BEGIN
  IF(NOT(KEYWORD_SET(TITLE))) THEN TITLE = FILENAME     
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

filebase = WIDGET_BASE(TITLE = TITLE, $			;create the base
		/COLUMN ) 

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

state = { $
	 base: filebase, $
	 text_area: filetext, $
	 file: filename $
	 }
WIDGET_CONTROL,filebase,SET_UVALUE=state

WIDGET_CONTROL, filebase, /REALIZE			;instantiate the widget

Xmanager, "XDisplayFile", $				;register it with the
		filebase, $				;widget manager
		GROUP_LEADER = GROUP, $
		EVENT_HANDLER = "XDispFile_evt" 

END  ;--------------------- procedure XDisplayFile ----------------------------

@PS_open.pro


PRO catch1d_get_pvtcolor,i,color
COMMON COLORS, R_ORIG, G_ORIG, B_ORIG, R_CURR, G_CURR, B_CURR
; 24 bits
	if n_elements(R_ORIG) eq 0 then $
	catch1d_get_pvtct
	color = R_ORIG(i) + G_ORIG(i)*256L + B_ORIG(i)*256L ^2
;	plot,indgen(10),color=color
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

if state.yrange(0) eq state.yrange(1) then begin
	dy =  state.yrange(0)/5
	state.yrange(0) =  state.yrange(0) - 2*dy
	state.yrange(1)=  state.yrange(1) + 3*dy
end
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
	ncv = 4  ;7
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
;       09-04-98 bkc   Fix the plot problem due to ymax eq ymin
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

PRO readascii,filename,rarray,x,y,double=double,skip=skip,lines=lines,l_column=l_column,columns=columns ,print=print
;+
; NAME: 
;      READASCII
;
; PURPOSE:
;      Read data from an ASCII file into an array. It is assumed that each line
;      with same number of entries. It can extract a sub-rectangle of region
;      from the ascii file.
;
; CATEGORY:
;      Input/Output
;
; CALLING SEQUENCE:
;      READASCII, Filename, Rarray, X, Y [ /DOUBLE, SKIP=skip, LINES=lines,
;                        L_COLUMN=l_column, COLUMNS=columns ]
;
; INPUT:
;      Filename -  Input file name
;
; KEYWORD PARAMETERS:
;      DOUBLE    -  specifies output variable in double position
;      SKIP      -  skip number of lines at beginning of file
;      LINES     -  total number of lines to be read
;      L_COLUMN  -  skip number of columns from the input line
;      COLUMNS   -  total number of columns to be read from the line
;      
; OUTPUTS:
;      Rarray   -  Return default column matrix
;      X        -  Return the 1st column of Rarray as independent variable
;      Y        -  Return transpose of the remaining columns of Rarray as 
;                  dependent variables
;
; RESTRICTIONS:
;     The input file must be in ASCII form. The header lines must be placed
;     at the beginning of the file. They can be skipped by setting the SKIP 
;     equal to the number of header lines at beginning. Each data line must 
;     contains the exactly same number of columns.
;
; EXAMPLE:
;   
;       READASCII, 'Filename', RARRAY, X, Y 
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha
;
;       09-16-98      bkc  Allows blank lines at end of ascii file, no blank
;                          lines allowed between data
;-

if n_params() eq 0 then begin
	print,'Usage: READASCII, Filename, Rarray, X, Y [ /DOUBLE, SKIP=skip, LINES=lines, $' 
	print,'      L_COLUMN=l_column, COLUMNS=columns ]'
	return
end

spawn,['wc','-l',filename],y, /noshell
if y(0) eq '' then begin
        print,'Error: bad filename for readascii'
        return
        end
no = y(0)

start_line=0
last_line=no
start_col = 0
if keyword_set(skip) then start_line=skip
if keyword_set(lines) then last_line=skip+lines
if last_line gt no then last_line = no

line=''
openr,unit,filename,/get_lun
i=0
nline=0
WHILE NOT eof(unit) and i lt last_line DO begin
	readf,unit,line,prompt=''
	if i ge start_line then begin
	line=strcompress(strtrim(line,2))
	
	res = str_sep(line,' ',/trim)
	sz=size(res)

	if strmid(line,0,1) ne ';' then begin 
	if i eq start_line then begin
	lines = last_line - start_line
	end_col = sz(1)
		if keyword_set(l_column) then begin
		 if l_column lt sz(1) and l_column ge 0 then start_col=l_column
		end
		if keyword_set(columns) then end_col = start_col + columns
		if end_col gt sz(1) then end_col = sz(1)
		if keyword_set(double) then $
		rarray = make_array(end_col-start_col,lines,/double) else $
		rarray = make_array(end_col-start_col,lines,/float)
	end
	if strlen(line) gt 0 then begin
	rarray(*,nline) = float(res(start_col:end_col-1))
	nline = nline+1
	end
	endif else  begin
		rstart_line = i+1
		if rstart_line gt start_line then start_line = rstart_line
		end
	end
	i = i+1
end
free_lun,unit
	if nline lt lines then rarray = rarray(*,0:nline-1) 

	temp = transpose(rarray)
	x = temp(*,0)
	y = temp(*,1: (end_col-start_col-1 > 1))

	if keyword_set(print) then begin
	help,rarray
	print,rarray
	help,x
	print,x
	help,y
	print,transpose(y)
	end
END


PRO w_readascii_Event, Event
COMMON EZ_FIT_BLOCK,ezfitData,image

  WIDGET_CONTROL,Event.top,GET_UVALUE=info
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'READASCII_SELECT': BEGIN
        F=PICKFILE(/READ,FILTER='*.*',GET_PATH=P,PATH=OP)
        if F eq '' then return
        found = findfile(F)
        if found(0) ne '' then begin
		WIDGET_CONTROL,info.file,SET_VALUE=strtrim(F(0),2)
        endif else begin
                res=widget_message(F+ 'not found',/info,title='W_READASCII Info', $
                        DIALOG_PARENT=Event.id)
                return
        end
      END
  'READASCII_FILENAME': BEGIN
 ;     Print, 'Event for filename'
      END
  'FIELD4': BEGIN
      Print, 'Event for Start line'
      END
  'FIELD5': BEGIN
 ;     Print, 'Event for lines'
      END
  'FIELD6': BEGIN
 ;     Print, 'Event for l_column'
      END
  'FIELD7': BEGIN
 ;     Print, 'Event for columns'
      END
  'READASCII_ACCEPT': BEGIN
      Print, 'Event for accept'
	WIDGET_CONTROL,info.skip,GET_VALUE=skip
	WIDGET_CONTROL,info.lines,GET_VALUE=lines
	WIDGET_CONTROL,info.left_col,GET_VALUE=l_col
	WIDGET_CONTROL,info.columns,GET_VALUE=columns
	WIDGET_CONTROL,info.file,GET_VALUE=filename
	readascii,filename(0),rarray,x,y,skip=skip,lines=lines,l_column=l_col,columns=columns

		ezfitData.im = y
                ezfit_init1d,x,y
                image2 = y
                if XRegistered('GETVECTOR_MAIN13') then $
                WIDGET_CONTROL,ezfitData.base_getvector,/DESTROY
                ezfit_getvector,image2

      END
  'READASCII_HELP': BEGIN
	str=['USAGE: ', $
     '      READASCII, Filename, Rarray [ ,X, Y, Skip=skip, Lines=lines, $', $
     '                 L_column=l_column, Columns=columns, print=print]', $
	'',$
     'INPUT: ', $
     '      Filename  - Input file name for ASCII data', $
     'OUTPUT: ', $
     '      Rarray    - Parameter returned for column matrix array ', $
     '      X         - First column from the Rarray ', $
     '      Y         - Transpose of remaining columns from the Rarray ', $
     'KEYWORD:', $
     '      SKIP      - Number of header lines to be skipped, default 0', $
     '      LINES     - Number of lines to be read, default all', $
     '      L_COLUMN  - Number of column to be skipped, default 0', $
     '      COLUMNS   - Number of columns to be read from the line, default all', $
     '      PRINT     - Print return parameters if it is set.' $
	]
   res = widget_message(str,/info,title='W_READASCII Help')
      END
  'READASCII_CANCEL': BEGIN
                WIDGET_CONTROL,Event.top,/DESTROY
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END MAIN13
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO w_readascii, filename, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      TITLE='W_READASCII', $
      MAP=1, $
      UVALUE='MAIN13')

  BASE2 = WIDGET_BASE(MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE2_0 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  file_button = WIDGET_BUTTON(BASE2_0, $
	VALUE='File', $
	UVALUE='READASCII_SELECT')

  READASCII_FILENAME = CW_FIELD( BASE2_0,VALUE=filename, $
      ROW=1, XSIZE=60, $
      RETURN_EVENTS=1, $
	TITLE=' ', $
      UVALUE='READASCII_FILENAME')

  BASE3 = WIDGET_BASE(BASE2, $
      MAP=1, /ROW, $
      UVALUE='BASE3')

  FieldVal429 = [ $
    '0' ]
  FIELD4 = CW_FIELD( BASE3,VALUE=FieldVal429, $
      ROW=1, XSIZE=5, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='       Start Line:', $
      UVALUE='FIELD4')

  FieldVal494 = [ $
    '' ]
  FIELD5 = CW_FIELD( BASE3,VALUE=FieldVal494, $
      ROW=1, XSIZE=5, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='   Number of Lines:', $
      UVALUE='FIELD5')

  BASE4 = WIDGET_BASE(BASE2, $
      MAP=1, /ROW, $
      UVALUE='BASE3')

  FieldVal559 = [ $
    '0' ]
  FIELD6 = CW_FIELD( BASE4,VALUE=FieldVal559, $
      ROW=1, XSIZE=5,$
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='     Start Column:', $
      UVALUE='FIELD6')

  FieldVal624 = [ $
    '' ]
  FIELD7 = CW_FIELD( BASE4,VALUE=FieldVal624, $
      ROW=1, XSIZE=5,$
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Number of Columns:', $
      UVALUE='FIELD7')

  BASE8 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE8')

  accept = WIDGET_BUTTON( BASE8, $
      UVALUE='READASCII_ACCEPT', $
      VALUE='Accept')

  help = WIDGET_BUTTON( BASE8, $
      UVALUE='READASCII_HELP', $
      VALUE='Help')

  cancel = WIDGET_BUTTON( BASE8, $
      UVALUE='READASCII_CANCEL', $
      VALUE='Close')


  info = { skip:FIELD4,  lines:FIELD5, left_col:FIELD6, columns:FIELD7, $
	file:READASCII_FILENAME }
  WIDGET_CONTROL, MAIN13, SET_UVALUE=info 

  WIDGET_CONTROL, MAIN13, /REALIZE
  XMANAGER, 'W_READASCII', MAIN13
END
;@plot1d.pro

FUNCTION goodness_fit,yres,N,Weight=Weight
;  sqrt(( W * YRES) ^2 / (M-N))

        M = n_elements(yres)
        if M gt N then begin
        goodness = 0.
        if keyword_set(Weight) then $
        for i=0,M-1 do goodness = goodness + ( Weight(i)*yres(i))^2 else $
        for i=0,M-1 do goodness = goodness + yres(i) ^ 2
        goodness = sqrt(goodness /( M - N))
        return,goodness
        end
END


PRO newSegment,x,y,begin_no,end_no,newX,newY,newW,gaussian=gaussian,poisson=poisson, whole=whole

	if n_params() lt 5 then begin
	print,'Usage: newSegment,X,Y,begin_no,end_no,newX,newY,newW'
	print,''
	print,'This routine extracts a continuous segment from the data array'
	print,''
	return 
	end

	width = n_elements(x)
	if begin_no ge end_no then begin
		print,'Error: start_no >= end_no'
		return
	end

	if n_elements(y) eq width and width ge end_no then begin
		newX = x(begin_no:end_no)
		newY = y(begin_no:end_no)

	newW = replicate(1., end_no-begin_no+1)
	if keyword_set(gaussian) then begin
		sdev = 0.05*newY 
		newW=1.0 / sdev ; Gaussian 
		end
	if keyword_set(poisson) then newW=1.0 / newY       ; Poisson 
	end

	if keyword_set(whole) then begin 
	tempY=make_array(width, value=MIN(newY))
	tempY(begin_no:end_no) = newY
	newY = tempY
	tempW=make_array(width)
	tempW(begin_no:end_no) = newW
	newW = tempW
	newX = X
	end
END

PRO ComfitGraf,x,y,a,yfit,sigma,print=print,test=test,geometric=geometric, $
	exponential=exponential, gompertz=gompertz, hyperbolic=hyperbolic, $
	logistic=logistic, logsquare=logsquare
;+
; NAME:
; 	COMFITGRAF
;
; PURPOSE:
;       This routine packages the IDL gradient-expansion least
;       square fit COMFIT function to fit the paired data {x(i), y(i)}.
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       COMFITGRAF, X, Y [,A] [,YFIT] [,SIGMA] ,/FIT_TYPE [,/PRINT] [,/TEST]
;         
;
; INPUTS:
;
;       X:        Position X vector of type integer, float or double. 
;
;       Y:        Data value Y vector of type integer, float or double. 
;
;       A:        Optional input, initial estimates of fitting coefficients.
;                 Number of elements in A depends on the FIT_TYPE specified.
;                 If A given, the number of elements in A must be consistant 
;                 with the FIT_TYPE entered.
;	
; KEYWORD PARAMETERS:
;
;   FIT_TYPE:    Six type of COMFIT, it can be any of following 
;
;                EXPONENTIAL Y = a0  * a1^x + a2 
;
;                GEOMETRIC Y = a0 * x^a1 + a2 
;
;                GOMPERTZ Y = a0 * a1^(a2*x) + a3 
;
;                HYPERBOLIC  Y = 1./(a0 + a1*x) 
;
;                LOGISTIC Y = 1./(a0 * a1^x + a2) 
;
;                LOGSQUARE Y = a0 + a1*alog10(x) + a2 * alog10(x)^2 
;
;   PRINT:       Specifies whether the output window will be poped up.
;
;   TEST:        Specifies whether the default test data will be used.
;
; OPTIONAL OUTPUTS:
;
;   YFIT:        Y vector calculated from the fitted equation.
;
;   SIGMA:       Standard deviation for the parameters in A.
;
; SIDE EFFECTS:
;
;      The computed parameters and the convergence may depend on the data and
;      the initial parameters of A vector entered.
;
; RESTRICTIONS:
;      The number of parameters must match exactly to the FIT_TYPE specified.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;      Run the geometric fitting, and pops up the fitting result window
;
;      X = [ ...]
;      Y = [ ...]
;      A = [ 0.5, 0.5, 0.5]
;      COMFITGRAF,X,Y,A,/GEOMETRIC,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 08-13-97.
;      xx-xx-xxbkc  comment
;-

if keyword_set(test) then begin
x = [2.27, 15.01, 34.74, 36.01, 43.65, 50.02, 53.84, 58.30, 62.12, $
 64.66, 71.66, 79.94, 85.67, 114.95]

y = [5.16, 22.63, 34.36, 34.92, 37.98, 40.22, 41.46, 42.81, 43.91, $
 44.62, 46.44, 48.43, 49.70, 55.31]
	 yfit = make_array(n_elements(x)) 
end
;  newA = comfit(x, y, a, sigma = sigma, yfit = yfit, _Extra=extra)

if n_elements(a) eq 0 then begin
 if keyword_set(hyperbolic) then a=[0.5,0.5] else a=[0.5,0.5,0.5]
 if keyword_set(gompertz) then a =[a,0.5]
	N = n_elements(A)
end

title = 'GRADIENT-EXPANSION LEAST-SQUARE FIT'
if keyword_set(exponential) then begin
	title = title + ' ( EXPONENTIAL ) ' 
	comment = 'Y = a0 * a1^x + a2 '
	newA = comfit(x, y, a, sigma = sigma, yfit = yfit, /exponential)
end

if keyword_set(gompertz) then begin
	title = title + ' ( GOMPERTZ ) '
	comment = 'Y = a0 * a1^( a2 * x)  + a3 '
	newA = comfit(x, y, a, sigma = sigma, yfit = yfit, /gompertz)
end

if keyword_set(geometric) then begin
	title = title + ' ( GEOMETRIC )'
	comment = 'Y = a0 * x^a1 + a2 '
	newA = comfit(x, y, a, sigma = sigma, yfit = yfit, /geometric)
end

if keyword_set(hyperbolic) then begin
	title = title + ' ( HYPERBOLIC ) '
	comment = 'Y = 1. / ( a0 + a1 * x )'
	newA = comfit(x, y, a, sigma = sigma, yfit = yfit, /hyperbolic)
end

if keyword_set(logistic) then begin
	title = title + ' ( LOGISTIC ) '
	comment = 'Y = 1. / ( a0 * a1^x + a2 )'
	newA = comfit(x, y, a, sigma = sigma, yfit = yfit, /logistic)
end

if keyword_set(logsquare) then begin
	title = title + ' ( LOGSQUARE ) '
	comment = 'Y =  a0 + a1 * alog10(x) + a2 * alog10(x)^2 '
	newA = comfit(x, y, a, sigma = sigma, yfit = yfit, /logsquare)
end

if n_elements(yfit) eq 0 then begin
	str=['Usage:  comfitGraf, X, Y [,A] [,YFIT] [,SIGMA] ,/FIT_TYPE [,/PRINT] [,/TEST]', $
	'', $
	'   FIT_TYPE  -  EXPONENTIAL       Y = a0  * a1^x + a2',$
	'', $
	'                GEOMETRIC         Y = a0 * x^a1 + a2',$
	'', $
	'                GOMPERTZ          Y = a0 * a1^(a2*x) + a3',$
	'', $
	'                HYPERBOLIC        Y = 1./(a0 + a1*x)',$
	'', $
	'                LOGISTIC          Y = 1./(a0 * a1^x + a2)',$
	'', $
	'                LOGSQUARE         Y = a0 + a1*alog10(x) + a2 * alog10(x)^2',$
	'','If A is entered, then the number of elements in A must be ', $
	'consistant with the FIT_TYPE entered.' $
	]
	res=widget_message(str,/info,title='FITTING Info')
	return
end

N=n_elements(newA)
if n_elements(newA) eq 2 then begin
	comment = [comment, $ 
		'a0 = '+ string(newA(0)) + ',  sigma = '+strtrim(sigma(0),2),$
		'a1 = ' + string(newA(1)) + ',  sigma = '+strtrim(sigma(1),2) ]
end
if n_elements(newA) eq 3 then begin
	comment = [comment, $ 
		'a0 = '+ string(newA(0)) + ',  sigma = '+strtrim(sigma(0),2),$
		'a1 = ' + string(newA(1)) + ',  sigma = '+strtrim(sigma(1),2),$
		'a2 = ' + string(newA(2)) + ',  sigma = '+strtrim(sigma(2),2) ]
end
if n_elements(newA) eq 4 then begin
	comment = [comment, $ 
		'a0 = '+ string(newA(0)) + ',  sigma = '+strtrim(sigma(0),2),$
		'a1 = ' + string(newA(1)) + ',  sigma = '+strtrim(sigma(1),2),$
		'a2 = ' + string(newA(2)) + ',  sigma = '+strtrim(sigma(2),2),$
		'a3 = ' + string(newA(3)) + ',  sigma = '+strtrim(sigma(3),2) ]
end

if n_elements(y) le 1 then begin
	str=['','Usage: comfitGraf,X,Y,A,/geometric','', $
		'e.g. use default test data','', $
		' comfitGraf,/geometric,/test']
	res = widget_message(str,/info,title='FITTING Info')
	return
end
        yres = yfit - y
        goodness = goodness_fit(yres,N)
	comment = [comment,'','GOODNESS OF FIT = '+string(goodness)]

	curv=make_array(n_elements(y),2)
	curv(0,0) = float(yfit)
	curv(0,1) = float(y)

;	plot,x,yfit
;	oplot,x,y,PSYM=7
	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		wtitle='COMFIT'

	if keyword_set(print) then begin

        OPENW,unit,'fitting.tmp',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end

	printf,unit,''
	printf,unit,title
	printf,unit,''
	for i=0,n_elements(comment)-1 do printf,unit,comment(i)
	printf,unit,''

	printf,unit,'       X         Y          YFIT       YFIT-Y '
	for i=0,n_elements(x) - 1 do  printf,unit,x(i),y(i),yfit(i),yres(i)
	FREE_LUN,unit
 	xdisplayfile,'fitting.tmp',title=title
	end

END


PRO ladfitgraf,x,y,yfit,absdev=absdev,test=test,double=double,print=print
;+
; NAME:
; 	LADFITGRAF
;
; PURPOSE:
;       This routine uses the IDL LADFIT function to fit the paired data 
;       {x(i), y(i)} with the linear model Y = A + Bx. 
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       LADFITGRAF, X, Y [,YFIT] [,ABSDEV=absdev] [,/DOUBLE] [,/PRINT] [,/TEST]
;         
;
; INPUTS:
;       X:        Position X vector 
;       Y:        Data value Y vector
;	
; KEYWORD PARAMETERS:
;  ABSDEV:       Specifies whether the mean absolute deviation to be returned.
;   PRINT:       Specifies whether the output window will be poped up.
;   TEST:        Specifies whether the default test data will be used.
;  DOUBLE:       If set to a non-zero value, computations are done in double 
;                precision arithmetic.
;
; OPTIONAL OUTPUTS:
;   YFIT:        Y vector calculated from the fitted equation.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;
;      X = [ ...]
;      Y = [ ...]
;      LADFITGRAF,X,Y,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 08-13-97.
;      xx-xx-xxbkc  comment
;-

if keyword_set(test) then begin

         x = [-3.20, 4.49, -1.66, 0.64, -2.43, -0.89, -0.12, 1.41, $
               2.95, 2.18,  3.72, 5.26]
         y = [-7.14, -1.30, -4.26, -1.90, -6.19, -3.98, -2.87, -1.66, $
              -0.78, -2.61,  0.31,  1.74]
endif else begin
	if n_params() eq 0 then begin
	str='Usage: ladfitGraf,X,Y [,YFIT] [,ABSDEV=absdev] [,/DOUBLE] [,/PRINT] [,/TEST]'
	str=[str,'', '        Y = A0 + A1 * X   ','', $
		'Linear fit - Least Absolute Deviation Method']
	res=widget_message(str,/info,title='FITTING Info')
	return
	end
end
if keyword_set(double) then begin
	x=double(x)
	y=double(y)
end
       A = ladfit(x, y, absdev = absdev)
	yfit = A(0) + A(1) * x

	curv=make_array(n_elements(y),2)
	curv(0,0) = float(yfit)
	curv(0,1) = float(y)

	title = 'Linear Fit -  Least Absolute Deviation Method' 
	comment = 'Y = A0 + A1 * X'
	for i=0,1 do comment=[comment,'A'+strtrim(i,2)+'='+ $
		string(A(i)) ]
	comment=[comment,'','ABS_DEVIATION=' + string(absdev)]

        yres = yfit - y
        goodness = goodness_fit(yres,n_elements(A))
	comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		wtitle='LADFIT'

	if keyword_set(print) then begin
        OPENW,unit,'fitting.tmp',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end

	printf,unit,'LADFIT - ',title
	printf,unit,''
	for i=0,n_elements(comment)-1 do printf,unit,comment(i)
	printf,unit,''

	printf,unit,'       X             Y          YFIT       YFIT-Y'
	for i=0,n_elements(x)-1 do printf,unit,x(i),y(i),yfit(i),yres(i)
	FREE_LUN,unit
	xdisplayfile,'fitting.tmp',title=title
	end
END

PRO linfitgraf,x,y,sdev=sdev,chisq=chisq,prob=prob,sigma=sigma,double=double,test=test,print=print
;+
; NAME:
; 	LINFITGRAF
;
; PURPOSE:
;       This routine uses the IDL LINFIT function to fit the paired data 
;       {x(i), y(i)} with the linear model Y = A + Bx.  It minimize the
;       chi-square error statistic.
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       LINFITGRAF, X, Y [,YFIT] [,CHISQ=chisq] [,PROG=prob] [,SDEV=sdev] 
;                  [,SIGMA=sigma] [,/DOUBLE] [,/PRINT] [,/TEST]
;         
;
; INPUTS:
;       X:        Position X vector 
;       Y:        Data value Y vector
;	
; KEYWORD PARAMETERS:
;   CHISQ:    Use this keyword to specify a named variable which returns the
;             chi-square error statistic as the sum of squared errors between
;             Y(i) and A + BX(i). If individual standard deviations are 
;             supplied, then the chi-square error statistic is computed as
;             the sum of squared errors divided by the standard deviations.
;    PROB:    Use this keyword to specify a named variable which returns the
;             probability that the computed fit would have a value of CHISQR 
;             or greater. If PROB is greater than 0.1, the model parameters 
;             are "believable". If PROB is less than 0.1, the accuracy of the
;             model parameters is questionable.
;    SDEV:    An n-element vector of type integer, float or double that 
;             specifies the individual standard deviations for {X(i), Y(i)}.
;   SIGMA:    Use this keyword to specify a named variable which returns a 
;             two-element vector of probable uncertainties for the model par-
;             ameters, [SIG_A,SIG_B].
;   PRINT:    Specifies whether the output window will be poped up.
;   TEST:     Specifies whether the default test data will be used.
;  DOUBLE:    If set to a non-zero value, computations are done in double 
;             precision arithmetic.
;
; OPTIONAL OUTPUTS:
;   YFIT:        Y vector calculated from the fitted equation.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;
;      X = [ ...]
;      Y = [ ...]
;      LINFITGRAF,X,Y,sigma=sigma,chisq=chisq,prob=prob,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 08-13-97.
;      xx-xx-xxbkc  comment
;-

if keyword_set(test) then begin

         x = [-3.20, 4.49, -1.66, 0.64, -2.43, -0.89, -0.12, 1.41, $
               2.95, 2.18,  3.72, 5.26]
         y = [-7.14, -1.30, -4.26, -1.90, -6.19, -3.98, -2.87, -1.66, $
              -0.78, -2.61,  0.31,  1.74]
	 sdev = replicate(0.85, n_elements(x))
endif else begin
	if n_params() eq 0 then begin
		str='Usage: linfitGraf,X,Y [,YFIT] [,CHISQ=chisq] [,PROB=prob] [,SDEV=sdev] '
		str=[str, '           [,SIGMA=sigma] [,/DOUBLE] [,/PRINT] [,/TEST]']
		str=[str,'','    Y = A0 + A1 * X   ',  '', $
			'Linear fit by Minimize the Chi-Square Error']
		res=widget_message(str,/info,title='FITTING Info')
		return
	end
	if keyword_set(sdev) then begin
		sdev = float(sdev)*y
	endif else begin
 	vec = moment(y, mdev=md, sdev=sd)
	mean = vec(0)
	variance = vec(1)
	skew = vec(2)
	kurtosis = vec(3)
	sdev = replicate(sd, n_elements(x))
	end
end

	if strpos(!version.release,'4.') lt 0 then $
        A = linfit(x, y, sdev=sdev, chisq=chisq, prob=prob, sigma=sigma) else $
        A = linfit(x, y, sdev=sdev, chisq=chisq, prob=prob)
	yfit = A(0) + A(1) * x

	curv=make_array(n_elements(y),2)
	curv(0,0) = float(yfit)
	curv(0,1) = float(y)

	title = 'Linear Fit by Minimize the Chi-Square Error' 
	comment = 'Y = A0 + A1 * X'
	if strpos(!version.release,'4.') lt 0  then begin
	for i=0,1 do comment=[comment,'A'+strtrim(i,2)+'='+ $
		string(A(i)) + '     SIGMA='+string(sigma(i))]
	endif else begin
	for i=0,1 do comment=[comment,'A'+strtrim(i,2)+'='+ $
		string(A(i))]
	end

        yres = yfit - y
        goodness = goodness_fit(yres,n_elements(A))
	comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

;	plot,x,yfit
;	oplot,x,y,PSYM=7
	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		wtitle='LINFIT - minimize chi-square'


	if keyword_set(print) then begin
        OPENW,unit,'fitting.tmp',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end

	printf,unit,'LINFIT - ',title
	printf,unit,''
	printf,unit,'        MEAN=',mean
	printf,unit,'        SDEV=',sqrt(variance)
	printf,unit,'    VARIANCE=',variance
	printf,unit,''
	for i=0,n_elements(comment)-1 do printf,unit,comment(i)
	printf,unit,''
	printf,unit,'       CHISQ=',chisq
	printf,unit,'        PROB=',prob
	printf,unit,''

	printf,unit,'        X            Y            YFIT     YFIT-Y     SDEV'
	for i=0,n_elements(x)-1 do printf,unit,x(i),y(i),yfit(i),yres(i),sdev(i)
	printf,unit,''
	printf,unit,'  PROB =',prob
	printf,unit,' CHISQ =',chisq, '
	FREE_LUN,unit
	xdisplayfile,'fitting.tmp',title=title
	end
END

PRO polyfitwgraf,x,y,w,ndegree,A,yfit,yband,sigma,print=print
;+
; NAME:
; 	POLYFITWGRAF
;
; PURPOSE:
;       This routine uses the IDL least square polynomial fit function
;       PLOYFITW with optional error estimates. 
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       POLYFITWGRAF,X,Y,W,NDEGREE [,A ] [,YFIT] [,YBAND] [,SIGMA] [,/PRINT] 
;         
;
; INPUTS:
;       X:        Position X vector 
;       Y:        Data value Y vector
;       W:        The vector of weights.  This vector should be same length as 
;                 X and Y.
;     NDEGREE:    The degree of polynomial to fit.
;	
; KEYWORD PARAMETERS:
;   PRINT:       Specifies whether the output window will be poped up.
;
; OPTIONAL OUTPUTS:
;            A:  Correlation matrix of the coefficients.
;         YFIT:  The vector of calculated Y's.  Has an error of + or - Yband.
;        YBAND:  Error estimate for each point = 1 sigma.
;        SIGMA:  The standard deviation in Y units.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;
;      X = [ ...]
;      Y = [ ...]
;      POLYFITWGRAF,X,Y,W,4,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 10-03-97.
;      xx-xx-xxbkc  comment
;-

if n_params() lt 4 then begin
	str='Usage: polyfitwGraf,X,Y,W,NDEGREE [,A] [,YFIT] [,YBAND] [,SIGMA] [,/PRINT]'
	str=[str,'',$
	'Y = A0 + A1 * X^1 + A2 * X^2 + A3 * X^3 + A4 * X^4 + ...', $
	'','POLYFITW - Least-Square Polynomial Fit with Weights']
	res=widget_message(str,/info,title='FITTING Info')
		return
	end

	result = polyfitw(x,y,w,ndegree,yfit,yband,sigma,corrm)

	curv=make_array(n_elements(y),2)
	curv(0,0) = float(yfit)
	curv(0,1) = float(y)

	title = 'Least-Square Polynomial Fit with Weights' 
	comment = 'Y = A0'
	for i=1,ndegree do comment=comment+' + A'+strtrim(i,2) +' * X^'+strtrim(i,2)
	for i=0,ndegree do comment=[comment,'A'+strtrim(i,2)+'='+strtrim(result(i),2)]
	comment=[comment,'SIGMA='+strtrim(sigma,2)]

        yres = yfit - y
        goodness = goodness_fit(yres,n_elements(A))
	comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		wtitle='POLYFITW'

	if keyword_set(print) then begin
        OPENW,unit,'fitting.tmp',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end


	printf,unit,'POLYFITW - ',title

 	vec = moment(y, mdev=md, sdev=sd)
	mean = vec(0)
	variance = vec(1)
	skew = vec(2)
	kurtosis = vec(3)
	sdev = replicate(sd, n_elements(x))

	statistic_1d,x,y,c_mass,x_peak,y_peak,y_hpeak,fwhm,fwhm_xl,fwhm_wd

	st = ''
	st = [st, '   Peak Y   = '+strtrim(y_peak,1)]
	st = [st, '   1st Peak @ '+strtrim(x_peak,1)]
;       st = [st, '   H-Peak Y = '+strtrim(y_hpeak)]
        st = [st, '   Centroid @ '+ strtrim(c_mass,1)]
        st = [st, '   FWHM     = '+strtrim(FWHM,1)]
	for i=0,n_elements(st)-1 do printf,unit,st(i)

	printf,unit,''
	printf,unit,'        MEAN=',mean
	printf,unit,'        SDEV=',sqrt(variance)
	printf,unit,'    VARIANCE=',variance
	printf,unit,''

 	for i=0,n_elements(comment) - 1 do printf,unit,comment(i)	
	printf,unit,''

	printf,unit,'            X             Y            YFIT           YFIT-Y        WEIGHT          YBAND '
	for i=0,n_elements(x)-1 do printf,unit,X(i),Y(i),YFIT(i),YRES(i),W(i),yband(i),format='(6G15.8)'
;	print,'A',A
	FREE_LUN,unit
	xdisplayfile,'fitting.tmp',title=title,width=100
	end
END

PRO polyfitgraf,x,y,ndegree,A,YFIT,YBAND,SIGMA,CORRM,print=print
;+
; NAME:
; 	POLYFITGRAF
;
; PURPOSE:
;       This routine uses the IDL least square polynomial fit function
;       PLOYFIT with optional error estimates. Double precision computation
;       is assumed. 
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       POLYFITGRAF,X,Y,NDEGREE [,A ] [,YFIT] [,YBAND] [,SIGMA] [,CORRM] [,/PRINT] 
;
; INPUTS:
;       X:        Position X vector 
;       Y:        Data value Y vector
;     NDEGREE:    The degree of polynomial to fit.
;	
; KEYWORD PARAMETERS:
;   PRINT:       Specifies whether the output window will be poped up.
;
; OPTIONAL OUTPUTS:
;            A:  Correlation matrix of the coefficients.
;         YFIT:  The vector of calculated Y's.  Has an error of + or - Yband.
;        YBAND:  Error estimate for each point = 1 sigma.
;        SIGMA:  The standard deviation in Y units.
;        CORRM:  The correlation matrix of the coefficients.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;
;      X = [ ...]
;      Y = [ ...]
;      POLYFITGRAF,X,Y,4,A,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 10-03-97.
;      xx-xx-xxbkc  comment
;-

; if ndegree >=3 singular matrix detected
; always use double precision in this routine
if n_params() lt 3 then begin
	str='Usage: polyfitGraf,X,Y,NDEGREE [,A] [,YFIT] [,YBAND] [,SIGMA] [,CORRM] [,/PRINT]'
	str=[str,'',$
	'Y = A0 + A1 * X^1 + A2 * X^2 + A3 * X^3 + A4 * X^4 + ...', $
	'','POLY_FIT - Least-Square Polynomial Fit']
	res=widget_message(str,/info,title='FITTING Info')
	return
end
 
;	CATCH,Error_status
;	if Error_status ne 0 then begin
;		res=widget_message([!err_string ,'','NDEGREE='+string(ndegree)])
;		retall
;		return
;	end

dx=double(x)
dy=double(y)
	result = poly_fit(dx,dy,ndegree,yfit,yband,sigma,corrm) 

	curv=make_array(n_elements(y),2)
	curv(0,0) = float(yfit)
	curv(0,1) = float(y)

	title = 'Least-Square POLY_FIT ' 
	comment = 'Y = A0'
	for i=1,ndegree do comment=comment+' + A'+strtrim(i,2) +' * X^'+strtrim(i,2)
	for i=0,ndegree do comment=[comment,'A'+strtrim(i,2)+'='+strtrim(result(i),2)]
	comment=[comment,'SIGMA='+strtrim(sigma,2)]

        yres = yfit - y
        goodness = goodness_fit(yres,n_elements(A))
	comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		wtitle='POLY_FIT'

	if keyword_set(print) then begin
        OPENW,unit,'fitting.tmp',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end

	printf,unit,'POLY_FIT - ',title
	printf,unit,''
 	for i=0,n_elements(comment) - 1 do printf,unit,comment(i)	
	printf,unit,''

	printf,unit,'          X              Y             YFIT         YFIT-Y       YBAND'
	for i=0,n_elements(x)-1 do printf,unit,X(i),Y(i),YFIT(i),yres(i),yband(i)
;	print,'A',A
	FREE_LUN,unit
	xdisplayfile,'fitting.tmp',title=title,width=90
	end

END

PRO gaussfitgraf,x,y,A,estimates=estimages,nterms=nterms,print=print
;+
; NAME:
; 	GAUSSFITGRAF
;
; PURPOSE:
;       This routine uses the IDL GAUSSIAN fit function y=f(x) where:
;               F(x) = A0*EXP(-z^2/2) + A3 + A4*x + A5*x^2
;                        and
;                z=(x-A1)/A2
;
;        A0 = height of exp, A1 = center of exp, A2 = sigma (the width).
;        A3 = constant term, A4 = linear term, A5 = quadratic term.
;
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       GAUSSFITGRAF,X,Y [,A ] [,ESTIMATES=extimates] [,NTERMS=nterms] [,/PRINT] 
;         
;
; INPUTS:
;       X:        Position X vector 
;       Y:        Data value Y vector
;	
; KEYWORD PARAMETERS:
;  ESTIMATES:    Optional starting estimates for the parameters of the 
;                equation.  Should contain NTERMS (6 if NTERMS is not
;                provided) elements.
;     NTERMS:    Set NTERMS of parameters used in Gaussian fit.
;      PRINT:    Specifies whether the output window will be poped up.
;
; OPTIONAL OUTPUTS:
;       A:       The coefficients of the fit.  A is a three to six
;                element vector as described under PURPOSE.
;
; RESTRICTIONS:
;        The peak or minimum of the Gaussian must be the largest
;        or smallest point in the Y vector.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;
;      X = [ ...]
;      Y = [ ...]
;      GAUSSFITGRAF,X,Y,NTERMS=4,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 10-03-97.
;      xx-xx-xxbkc  comment
;-

	if n_params() lt 2 then begin
	str="Usage: gaussfitGraf, X, Y [,A] [,ESTIMATES=estimates] [,NTERMS=nterms] [,/PRINT]"
	str=[str,'', $
	 'F(X) = A0 * exp( -Z^2 / 2 ) [ + A3 + A4 * X + A5 * X^2 ]',$
	 '      where   Z = (X - A1) / A2']
	res=widget_message(str,/info,title='FITTING Info')
	return
	end

	if keyword_set(nterms) then begin
	if nterms lt 3 then nterms = 3
	if nterms gt 6 then nterms = 6
	yfit = gaussfit(x,y,A,nterms=nterms)
	endif else yfit = gaussfit(x,y,A) 

	curv=make_array(n_elements(y),2)
	curv(0,0) = float(yfit)
	curv(0,1) = float(y)

	fname = 'gaussfit'
	title = 'Non-linear Least-square Fit of  Gaussian' 
	get_curvefit_function,fname,comment
	for i=0,n_elements(A)-1 do comment=[comment,'A'+strtrim(i,2)+'='+ $
		string(A(i))]
 
        yres = yfit - y
        goodness = goodness_fit(yres,n_elements(A))
	comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

;	plot,x,yfit
;	oplot,x,y,PSYM=7
	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		wtitle='GAUSSFIT'


	if keyword_set(print) then begin
	OPENW,unit,'fitting.tmp',/GET_LUN,ERROR=err
	if err ne 0 then begin
	res = widget_message(!err_string,/info,title='FITTING Info')
	return
	end


 	vec = moment(y, mdev=md, sdev=sd)
	mean = vec(0)
	variance = vec(1)
	skew = vec(2)
	kurtosis = vec(3)
	sdev = replicate(sd, n_elements(x))

	printf,unit,''
	printf,unit,'GAUSSFIT - ',title
	printf,unit,''
	printf,unit,'        MEAN=',mean
	printf,unit,'        SDEV=',sqrt(variance)
	printf,unit,'    VARIANCE=',variance
	printf,unit,''

	for i=0,n_elements(comment)-1 do printf,unit,comment(i)
	printf,unit,''

	printf,unit,'      X            Y           YFIT      YFIT-Y'
	for i=0,n_elements(x)-1 do printf,unit,x(i),y(i),yfit(i),yres(i)
	FREE_LUN,unit
	xdisplayfile,'fitting.tmp',title='GAUSSFIT'
	end
END

PRO curvefitgraf,x,y,Weights,A,sigma,test=test,print=print,function_name=function_name,noderivative=noderivative,itmax=itmax,tol=tol
;+
; NAME:
; 	CURVEFITGRAF
;
; PURPOSE:
;
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       CURVEFITGRAF,X,Y,Weights [,A ] [,Sigma] [,/PRINT]  $
;            [,FUNCTION_NAME='funct']  $
;            [,/NODERIVATIVE] [,ITMAX=20] [,TOL=1.e-3] 
;         
;
; INPUTS:
;       X:       Position X vector 
;       Y:       Data value Y vector
;  Weights:      A row vector of weights, the same length as Y. Defaults to 1.
;                Instrumental weighting-Gaussian : Weights(i) = 1./sigma(i) ^2
;                Statistical weighting-Poisson :  Weights(i) = 1./y(i)
;       A:       The coefficients of the fit.  The number of elements in A 
;                must be exactly the same as that defined in the function_name.
;                If not specified, the fitting function should provide the
;                initial default.
;	
; KEYWORD PARAMETERS:
;      PRINT:    Specifies whether the output window will be poped up.
; FUNCTION_NAME: The name of the procedure function to fit. If omitted,
; NODERIVATIVE:  If this keyword is set then the partial derivatives will be
;                calculated by CURVEFIT by forward differences. Otherwise
;                the procedure function should provide the partial 
;                derivatives calculation.  Defaults nodevivative is not set.
;                The procedure function must be written as in 'FUNCT' as
;                described in IDL 'CURVEFIT' restrictions.
;      ITMAX:    Maximum number of iterations. Default = 20.
;      TOL:      The convergence tolerance. Default = 1.e-3. The routine 
;                returns when the relative decrease in chi-squared is less
;                than TOL.
;
; OPTIONAL OUTPUTS:
;       A:       Returns the coefficients of the fit.  
;      Sigma:    A vector of standard deviations for the parameters in A.
;
; RESTRICTIONS:
;       The function to be fit must be defined and called FUNCT,
;       unless the FUNCTION_NAME keyword is supplied.  This function,
;       (actually written as a procedure) must accept values of
;       X (the independent variable), and A (the fitted function's
;       parameter values), and return F (the function's value at
;       X), and PDER (a 2D array of partial derivatives).
;       For an example, see FUNCT in the IDL User's Libaray.
;       A call to FUNCT is entered as:
;       FUNCT, X, A, F, PDER
; where:
;       X = Variable passed into CURVEFIT.  It is the job of the user-written
;                function to interpret this variable.
;       A = Vector of NTERMS function parameters, input.
;       F = Vector of NPOINT values of function, y(i) = funct(x), output.
;       PDER = Array, (NPOINT, NTERMS), of partial derivatives of funct.
;               PDER(I,J) = DErivative of function at ith point with
;               respect to jth parameter.  Optional output parameter.
;               PDER should not be calculated if the parameter is not
;               supplied in call. If the /NODERIVATIVE keyword is set in the
;               call to CURVEFIT then the user routine will never need to
;               calculate PDER.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
;      For more information please refer the PROCEDURE section of the
;      CURVEFIT in IDL online help.
;
; EXAMPLE:
;
;      X = [ ...]
;      Y = [ ...]
;      CURVEFITGRAF,X,Y,Weights,A,Sigma,/PRINT
;
;      For more information please refer the EXAMPLE section of the CURVEFIT
;      in IDL online help.
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 10-15-97.
;      xx-xx-xxbkc  comment
;-

fname = 'funct'	
if keyword_set(function_name) then fname=string(function_name)
if keyword_set(test) then begin
	fname = 'gfunct'	
	x=findgen(10)
	y=[12.,11.,10.2,9.4,8.7,8.1,7,5,6.9,6.5,6.1]

	A =[ 10., -0.1, 2.0]
endif else begin
	if n_params() lt 2 then begin
	str = "Usage: curvefitGraf, X, Y, Weights [,A] [,Sigma] [./PRINT] [,FUNCTION_NAME='funct']"
	str = [str,'        [,/NODERIVATIVE] [,ITMAX=20] [,TOL=1.e-3] ']
	str = [str,'', 'with default fit function', $
	'', 'F(X) = A0 * exp( -Z^2 / 2 ) + A3 + A4 * X + A5 * X^2 ', $
	'         where  Z = (X - A1) / A2','', $
	'Non-linear Least Square Fit with Weights' ]
	res = widget_message(str,/info,title='FITTING Info')
	return
	end
end
	
if keyword_set(noderivative) then $
yfit = curvefit(x,y,Weights,A,sigma,iter=iter,function_name=fname,itmax=itmax,tol=tol,/noderivative) else $
yfit = curvefit(x,y,Weights,A,sigma,iter=iter,function_name=fname,itmax=itmax,tol=tol)
help,iter

	curv=make_array(n_elements(y),2)
	curv(0,0) = float(yfit)
	curv(0,1) = float(y)

	title = 'Non-linear Least Square Fit with Weights' 
	get_curvefit_function,fname,comment
	for i=0,n_elements(A)-1 do comment=[comment,'A'+strtrim(i,2)+'='+ $
		string(A(i)) + '     SIGMA='+string(sigma(i))]
 
        yres = yfit - y
        goodness = goodness_fit(yres,n_elements(A))
	comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

;	plot,x,yfit
;	oplot,x,y,PSYM=7
	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		wtitle='CURVEFIT'


	if keyword_set(print) then begin
        OPENW,unit,'fitting.tmp',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end

 	vec = moment(y, mdev=md, sdev=sd)
	mean = vec(0)
	variance = vec(1)
	skew = vec(2)
	kurtosis = vec(3)
	sdev = replicate(sd, n_elements(x))

	printf,unit,'CURVEFIT - ',title
	printf,unit,''
	printf,unit,'        MEAN=',mean
	printf,unit,'        SDEV=',sqrt(variance)
	printf,unit,'    VARIANCE=',variance
	printf,unit,''

	for i=0,n_elements(comment)-1 do printf,unit,comment(i)
	printf,unit,''

	printf,unit,'      X            Y           YFIT       YFIT-Y   WEIGHTS'
	for i=0,n_elements(x)-1 do printf,unit,x(i),y(i),yfit(i),yres(i),Weights(i)

	FREE_LUN,unit
	xdisplayfile,'fitting.tmp',title='CURVEFIT'
	end
END

PRO gfunct,x,a,f,pder
	bx=exp(a(1)*x)
	f = a(0)*bx +a(2)
	if n_params() ge 4 then $
	 pder=[[bx], [a(0)*x*bx], [replicate(1.,n_elements(f))] ]
END

PRO get_curvefit_funct_pvt,fname,expres
	expres = 'User defined private function used in curvefit.'
END

PRO get_curvefit_function,fname,expres
lfname = strlowcase(fname)
if  lfname eq 'gfunct' then begin
	expres='F(X) = A0 * exp( A1 * X ) + A2'
	return
	end
if  lfname eq 'funct' then begin 
	expres='F(X) = A0 * exp( -Z^2 / 2 ) + A3 + A4 * X + A5 * X^2 ,'
	expres=[expres,' Z = (X - A1) / A2' ]
	return
	end
if  lfname eq 'gaussfit' then begin 
	expres='F(X) = A0 * exp( -Z^2 / 2 ) [ + A3 + A4 * X + A5 * X^2 ] ,'
	expres=[expres,' Z = (X - A1) / A2' ]
	return
	end
if  lfname eq 'lorentzian' then begin 
	expres = 'Y = A0 * A2^2 / ( (X-A1)^2 + A2^2 ) '
	return
	end
get_curvefit_funct_pvt,fname,expres
END


PRO regressfitgraf,x,y,weights,yfit,const,sigma,ftest,r,rmul,chisq,status, $
	relative_weight=relative_weight, $
	print=print,test=test

if n_params() eq 0 and keyword_set(test) eq 0 then begin
	str='Usage: regressfitGraf,X,Y,Weights,yfit,const[,sigma,ftest,r,rmul,chisq,status,/RELATIVE_WEIGHT,/PRINT]'
	str=[str,'',$
	'Y = A0 + A1 * X1 + A2 * X2 + A3 * X3 + A4 * X4 + ...', $
	'','MULTIPLE Linear Regression Fit with Weights']
	res=widget_message(str,/info,title='FITTING Info')
		return
end

if keyword_set(test) then begin
;Create a two by six array of independent variable data.

X = [[0.0, 0.0], $     
     [2.0, 1.0], $
     [2.5, 2.0], $
     [1.0, 3.0], $
     [4.0, 6.0], $    
     [7.0, 2.0]]

;Create an Npoints-element vector of dependent variable data.

Y = [5.0, 10.0, 9.0, 0.0, 3.0, 27.0]

end

	sz1 = size(x)
	nterms = sz1(1)
	npoints = sz1(2)
	sz2 = size(y)

	if sz2(0) ne 1 and sz2(1) ne npoints then begin
	str =[ 'Inconsistant dimension in X, Y input','NTERMS=',string(nterms), $
		'NPOINTS=',string(npoints), $
		' Y = A0 + A1*X1 + A2*X2 + ... ']
	res = widget_message(str,/info,title='Fitting - multiple regress')
	return
	end

	weights = replicate(1.0,n_elements(y))
	result = regress(x,y,weights,yfit,const,sigma,ftest,r,rmul,chisq,status,/RELATIVE_WEIGHT)
;if keyword_set(print) then begin
;print,'sigma',sigma
;print,'ftest',ftest
;print,'r',r
;print,'rmul',rmul
;print,'chisq',chisq
;print,'status',status
;end

	curv=make_array(n_elements(y),2)
	curv(0,0) = float(yfit(*))
	curv(0,1) = float(y)

	title = 'Multiple Linear Regression Fit with Weights' 
	comment = 'Y(i) = A0'
	for i=1,nterms do comment=comment+' + A'+strtrim(i,2)+' * X' + $
			strtrim(i,2) + '(i)'

	comment = ['',comment,'A0 = '+strtrim(const,2)]
	for i=0,nterms-1 do begin
		str = 'A'+strtrim(i+1,2)+' = '+strtrim(result(0,i),2)
		comment=[comment,str]
		end
	
        yres = yfit - y
        goodness = goodness_fit(yres,n_elements(A))
	comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

	plot1d,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		wtitle='REGRESS'
;	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
;		wtitle='REGRESS'

	if keyword_set(print) then begin
        OPENW,unit,'fitting.tmp',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end

	printf,unit,'REGRESS - ',title
	printf,unit,''
	printf,unit,' X [',strtrim(sz1(1),2),' x ',strtrim(sz1(2),2),'] :'
	printf,unit,X
	printf,unit,' Y [',strtrim(sz2(1),2),'] :'
	printf,unit,Y
	printf,unit,''

 	for i=0,n_elements(comment) - 1 do printf,unit,comment(i)	
	printf,unit,''

	printf,unit,'          Weights      Y             YFIT       YFIT-Y '
	for i=0,n_elements(y)-1 do printf,unit,weights(i),Y(i),YFIT(i),yres(i)
	FREE_LUN,unit
	xdisplayfile,'fitting.tmp',title='REGRESS'
	end
END



PRO GETVECTOR_MAIN13_Event, Event
COMMON EZ_FIT_BLOCK,ezfitData,image

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'FIELD4': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=i
	if i ge 0 and i lt ezfitData.width then ezfitData.I_index = i else $
		WIDGET_CONTROL,Event.id,SET_VALUE=ezfitData.width-1
      END
  'FIELD5': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=j
	if j ge 0 and j lt ezfitData.height then ezfitData.J_index = j else $
		WIDGET_CONTROL,Event.id,SET_VALUE=ezfitData.height-1
      END
  'HELP_GETVECTOR': BEGIN
	str=["Click the  'Image' button to show the image", $
		"Click the left mouse button in the image area to select the interest point",$
		"Click the 'Z vs X' to plot the ZX curve", $
		"Click the 'Z vs Y' to plot the ZY curve", $
		"Click the 'Help' button to get this info", $
		"Click the 'Close' button to close the dialog" ]
	res=widget_message(str,/info,title='GetVector - Help')	
	END
  'CLOSE_GETVECTOR': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
	END
  'FRESH_GETVECTOR': BEGIN
	TVSCL,congrid(image,ezfitData.TV_width,ezfitData.TV_height)
	END
  'YX_GETVECTOR': BEGIN
	if ezfitData.dim eq 1 then begin
	WIDGET_CONTROL, ezfitData.J_field, GET_VALUE=j
	if j ge 0 and j lt ezfitData.height then ezfitData.J_index = j else begin
	WIDGET_CONTROL,ezfitData.J_field, SET_VALUE=ezfitData.height-1
	ezfitData.J_index = ezfitData.height-1
	end
	if j ge ezfitData.height then ezfitData.J_index = ezfitData.height-1 
        ezfitData.y = image(*,ezfitData.J_index)
        title='Y vs X (1D Data)'

	WSET,ezfitData.image_area
        plot,ezfitData.x(0:ezfitData.width-1), ezfitData.y,PSYM=0,thick=2,$
                xtitle='X', ytitle='Y', title=title
	if XRegistered('POLYFITW_SETUP') then $
	WIDGET_CONTROL,ezfitData.polyfit_label, $
		SET_VALUE='Number of Elements: '+strtrim(ezfitData.width,2)
        ezfitData.pick=0
	str = '       I    X Values         Y Values'
	for i=0,ezfitData.width-1 do $
	str = [str,string(i, ezfitData.x(i), ezfitData.y(i),/print)]
	WIDGET_CONTROL, ezfitData.text_area, SET_VALUE=str	
        end
	END
  'ZX_GETVECTOR': BEGIN
        if ezfitData.dim eq 2 then begin
;               ezfit_get2DData,ezfitData.file,image
                ezfitData.zx = image(*,ezfitData.J_index)
        title='Z vs X @ Y='+strtrim(ezfitData.y(ezfitData.J_index),2)

	WSET,ezfitData.image_area
        plot,ezfitData.x(0:ezfitData.width-1), ezfitData.zx,PSYM=0,thick=2, $
                xtitle='X', ytitle='Z', title=title
	if XRegistered('POLYFITW_SETUP') then $
	WIDGET_CONTROL,ezfitData.polyfit_label, $
		SET_VALUE='Number of Elements: '+strtrim(ezfitData.width,2)
        ezfitData.pick=1
	str = '       I    X Values         Zy Values'
	for i=0,ezfitData.width-1 do $
	str = [str,string(i, ezfitData.x(i), ezfitData.zx(i),/print)]
	WIDGET_CONTROL, ezfitData.text_area, SET_VALUE=str	
        end
	END
  'ZY_GETVECTOR': BEGIN
        if ezfitData.dim eq 2 then begin
;                ezfit_get2DData,ezfitData.file,image
                ezfitData.zy = image(ezfitData.I_index,*)
        title='Z vs Y @ X='+strtrim(ezfitData.x(ezfitData.I_index),2)

	WSET,ezfitData.image_area
        plot,ezfitData.y(0:ezfitData.height-1), ezfitData.zy,PSYM=0,thick=2, $
                xtitle='Y', ytitle='Z', title=title
	if XRegistered('POLYFITW_SETUP') then $
	WIDGET_CONTROL,ezfitData.polyfit_label, $
		SET_VALUE='Number of Elements: '+strtrim(ezfitData.height,2)
        ezfitData.pick=2
	str = '       I    Y Values         Zx Values'
	for i=0,ezfitData.height-1 do $
	str = [str,string(i, ezfitData.y(i), ezfitData.zy(i),/print)]
	WIDGET_CONTROL, ezfitData.text_area, SET_VALUE=str	
        end
	END
  'DRAW6': BEGIN
	;cursor,x,y,0,/device
	x=Event.x
	y=Event.y
	x = fix(x / ezfitData.x_mag)
	y = fix(y / ezfitData.y_mag)

	if x ge 0 and x lt ezfitData.width then begin
		ezfitData.I_index = x
		WIDGET_CONTROL,ezfitData.I_field,SET_VALUE=x
	end
	if y ge 0 and y lt ezfitData.height then begin
		ezfitData.J_index = y
		WIDGET_CONTROL,ezfitData.J_field,SET_VALUE=y
	end
      END
  ENDCASE
END



PRO ezfit_getvector,image, GROUP=Group
COMMON EZ_FIT_BLOCK,ezfitData

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

device,retain=2

  GETVECTOR_MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, TLB_FRAME_ATTR=8,$
      TITLE='EZ_FIT-2D Image', $
      UVALUE='GETVECTOR_MAIN13')

  BASE2 = WIDGET_BASE(GETVECTOR_MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  sz = size(image)
  dim='('+strtrim(sz(1),2)+'x'+strtrim(sz(2),2)+')'
  LABEL3 = WIDGET_LABEL( BASE2, $
;     FONT='-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-1', $
      UVALUE='LABEL3', $
      VALUE='GetVector '+dim)

  BASE3 = WIDGET_BASE(GETVECTOR_MAIN13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  FieldVal1722 = [ $
    strtrim(ezfitData.I_index,2) ]
  FIELD4 = CW_FIELD( BASE3,VALUE=FieldVal1722, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='I Index', $
      UVALUE='FIELD4', $
      XSIZE=4)

  FieldVal1787 = [ $
    strtrim(ezfitData.J_index,2) ]
  FIELD5 = CW_FIELD( BASE3,VALUE=FieldVal1787, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='J Index', $
      UVALUE='FIELD5', $
      XSIZE=4)

  DRAW6 = WIDGET_DRAW( BASE2, $
      BUTTON_EVENTS=1, $
      RETAIN=2, $
      UVALUE='DRAW6', $
      XSIZE=ezfitData.TV_width, $
      YSIZE=ezfitData.TV_height, $
      X_SCROLL_SIZE=300, $
      Y_SCROLL_SIZE=300)

  TEXT_AREA = WIDGET_TEXT(BASE2,YSIZE=10,VALUE='',/SCROLL)
  ezfitData.text_area = TEXT_AREA

  BASE4 = WIDGET_BASE(GETVECTOR_MAIN13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  fresh_getvector=WIDGET_BUTTON(BASE4,value='Image',UVALUE='FRESH_GETVECTOR')
if ezfitData.dim eq 1 then $
  YX_getvector=WIDGET_BUTTON(BASE4,value='Y vs X',UVALUE='YX_GETVECTOR') $
else begin
  ZX_getvector=WIDGET_BUTTON(BASE4,value='Zy vs X',UVALUE='ZX_GETVECTOR')
  ZY_getvector=WIDGET_BUTTON(BASE4,value='Zx vs Y',UVALUE='ZY_GETVECTOR')
end
  help_getvector=WIDGET_BUTTON(BASE4,value='Help',UVALUE='HELP_GETVECTOR')
  close_getvector=WIDGET_BUTTON(BASE4,value='Close',UVALUE='CLOSE_GETVECTOR')


  WIDGET_CONTROL, GETVECTOR_MAIN13, /REALIZE

  ; Get drawable window index

  COMMON DRAW6_Comm, DRAW6_Id
  WIDGET_CONTROL, DRAW6, GET_VALUE=DRAW6_Id

  ezfitData.base_getvector = GETVECTOR_MAIN13
  ezfitData.I_field = FIELD4
  ezfitData.J_field = FIELD5
  ezfitData.image_area = DRAW6_Id
  WSET,ezfitData.image_area

;  TVSCL,image
   TVSCL,congrid(image,ezfitData.TV_width,ezfitData.TV_height)
   ezfitData.x_mag= float(ezfitData.TV_width)/ezfitData.width
   ezfitData.y_mag= float(ezfitData.TV_height)/ezfitData.height

  XMANAGER, 'GETVECTOR_MAIN13', GETVECTOR_MAIN13
END




PRO POLYFITW_SETUP_Event, Event
COMMON EZ_FIT_BLOCK,ezfitData,image

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'FIELD4': BEGIN
      Print, 'Event for Ndegree'
	WIDGET_CONTROL,ezfitData.polyfit_ndfield, $
		GET_VALUE=i
	ezfitData.polyfit_ndegree=i
      END
  'FIELD5': BEGIN
      Print, 'Event for Weight Factor'
	WIDGET_CONTROL,ezfitData.polyfit_wffield, $
		GET_VALUE=f
	ezfitData.polyfit_factor=f
      END
  'OK_POLYFITW': BEGIN
	WIDGET_CONTROL,ezfitData.polyfit_ndfield, $
		GET_VALUE=i
	ezfitData.polyfit_ndegree=i
	WIDGET_CONTROL,ezfitData.polyfit_wffield, $
		GET_VALUE=f
	ezfitData.polyfit_factor=f
        ezfit_picktype,x,y
        w=replicate(ezfitData.polyfit_factor,n_elements(x))
	ndegree = ezfitData.polyfit_ndegree
	if ezfitData.polyfit eq 1 then polyfitwGraf,x,y,w,ndegree,/print $
	else polyfitGraf,x,y,ndegree,/print
	END
  'CANCEL_POLYFITW': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
	END
  ENDCASE
END



PRO polyfitwSetup, GROUP=Group
COMMON EZ_FIT_BLOCK,ezfitData,image

  IF XRegistered('POLYFITW_SETUP') then return
  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0


  junk   = { CW_PDMENU_S, flags:0, name:'' }


  POLYFITW_SETUP = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, TITLE='POLYFIT Setup', $
      UVALUE='POLYFITW_SETUP')

  BASE2 = WIDGET_BASE(POLYFITW_SETUP, $
      COLUMN=1, $
      MAP=1, $
      TITLE='POLYFITW - Setup', $
      UVALUE='BASE2')

nelem=strtrim(ezfitData.width,2)
if ezfitData.pick eq 2 then nelem=strtrim(ezfitData.height,2)
  LABEL3 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL3', /ALIGN_LEFT, $
      VALUE='Number of Elements: '+nelem)
  ezfitData.polyfit_label = LABEL3 

  FieldVal395 = [ $
    '4' ]
  FIELD4 = CW_FIELD( BASE2,VALUE=FieldVal395, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Ndegree', $
      UVALUE='FIELD4')
  ezfitData.polyfit_ndfield = FIELD4

  FieldVal460 = [ $
    '1.' ]
  FIELD5 = CW_FIELD( BASE2,VALUE=FieldVal460, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='Weight Factor', $
      UVALUE='FIELD5')
  ezfitData.polyfit_wffield = FIELD5

  BASE3 = WIDGET_BASE(POLYFITW_SETUP, $
      ROW=1, $
      MAP=1 )
  OK_polyfitw=WIDGET_BUTTON(BASE3,VALUE='Accept',UVALUE='OK_POLYFITW')
  Cancel_polyfitw=WIDGET_BUTTON(BASE3,VALUE='Cancel',UVALUE='CANCEL_POLYFITW')

  WIDGET_CONTROL, POLYFITW_SETUP, /REALIZE

  XMANAGER, 'POLYFITW_SETUP', POLYFITW_SETUP

END





PRO SVDFIT_MAIN13_Event, Event
COMMON EZ_FIT_BLOCK,ezfitData,image

  WIDGET_CONTROL,Event.top,GET_UVALUE=info
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'SVDFIT_FNAME': BEGIN
	WIDGET_CONTROL,info.fname_fld,GET_VALUE=fname
	info.fname = fname(0)
	ezfitData.svdfit_fname = info.fname
      END
  'SVDFIT_NTERM': BEGIN
	WIDGET_CONTROL,info.nterm_fld,GET_VALUE=n
	ezfitData.svdfit_nterm = info.nterm
	info.nterm = n
      END
  'SVDFIT_WEIGHT': BEGIN
	WIDGET_CONTROL,info.weight_fld,GET_VALUE=n
	info.weight = n
	ezfitData.svdfit_factor = info.weight
      END
  'LEGENDRE_YES': BEGIN
	info.legendre = Event.Index
	ezfitData.svdfit_legendre = info.legendre
      END
  'SVDFIT_ACCEPT': BEGIN
	WIDGET_CONTROL,info.fname_fld,GET_VALUE=fname
	info.fname = fname(0)
	WIDGET_CONTROL,info.nterm_fld,GET_VALUE=n
	info.nterm = n
	WIDGET_CONTROL,info.weight_fld,GET_VALUE=n
	info.weight = n

	ezfitData.svdfit_fname = info.fname
	ezfitData.svdfit_nterm = info.nterm
	ezfitData.svdfit_factor = info.weight
	ezfitData.svdfit_legendre = info.legendre
       ezfit_picktype,x,y
       ws = replicate(info.weight,n_elements(y))
	if info.legendre eq 0 then begin 
      	 svdfitGraf,x,y,info.nterm, $
		function_name=info.fname, $
		weights=ws, $
		/print
	endif else begin
      	 svdfitGraf,x,y,info.nterm, $
		weights=ws, $
		/legendre, $
		/print
	end
      END
  'BUTTON7': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
      END
  ENDCASE

 WIDGET_CONTROL,Event.top,SET_UVALUE=info,bad_id=bad
;if bad eq 0 then begin
;ezfitData.svdfit_base = info.base
;ezfitData.svdfit_fname = info.fname
;ezfitData.svdfit_nterm = info.nterm
;ezfitData.svdfit_factor= info.weight
;ezfitData.svdfit_legendre = info.legendre
;end
END




PRO svdfitSetup, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  SVDFIT_MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, title='SVDFIT Setup', $
      UVALUE='SVDFIT_MAIN13')

  BASE2 = WIDGET_BASE(SVDFIT_MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  FieldVal2739 = [ $
    'svdfunct' ]
  SVDFIT_FNAME = CW_FIELD( BASE2,VALUE=FieldVal2739, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Function_Name:', $
      UVALUE='SVDFIT_FNAME', $
      XSIZE=20)

  FieldVal2804 = [ $
    '4' ]
  SVDFIT_NTERM = CW_FIELD( BASE2,VALUE=FieldVal2804, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='NTERMS:', $
      UVALUE='SVDFIT_NTERM')

  FieldVal2804 = [ $
    '1.' ]
 SVDFIT_WEIGHT = CW_FIELD( BASE2,VALUE=FieldVal2804, $
      ROW=1, $
      Float=1, $
      RETURN_EVENTS=1, $
      TITLE='WEIGHTS:', $
      UVALUE='SVDFIT_WEIGHT')
  
  legendre_yes = WIDGET_DROPLIST(BASE2,VALUE=['No','Yes'], $
	UVALUE='LEGENDRE_YES', TITLE='LEGENDRE')
  WIDGET_CONTROL,legendre_yes,SET_DROPLIST_SELECT=0

  BASE5 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE5')

  svdfit_accept = WIDGET_BUTTON( BASE5, $
      UVALUE='SVDFIT_ACCEPT', $
      VALUE='Accept')

  BUTTON7 = WIDGET_BUTTON( BASE5, $
      UVALUE='BUTTON7', $
      VALUE='Cancel')

info = { base: SVDFIT_MAIN13, $
	fname_fld: SVDFIT_FNAME, $
	weight_fld: SVDFIT_WEIGHT, $
	nterm_fld: SVDFIT_NTERM, $
	fname: 'svdfunct', $
	nterm: 4, $
	weight :1., $
	legendre : 0 $
	}

WIDGET_CONTROL,SVDFIT_MAIN13,SET_UVALUE=info

  WIDGET_CONTROL, SVDFIT_MAIN13, /REALIZE

  XMANAGER, 'SVDFIT_MAIN13', SVDFIT_MAIN13
END


;
; find  fwh_max, c_mass, peak for a given x,y array
;   fwhm -  max of fwhm_wd(i)
;   fwhm_xl(i)  - start position of fwhm
;   fwhm_wd(i)  - width of fwhm
;
PRO statistic_1d,x,y,c_mass,x_peak,y_peak,y_hpeak,fwhm, fwhm_xl,fwhm_wd, $
	FIT=FIT,XINDEX=XINDEX,LIST=LIST

xindex = keyword_set(XINDEX)
list = keyword_set(LIST)

nx = n_elements(x)
a=make_array(nx,/float)
da=make_array(nx,/float)
ny=make_array(nx,/float)
slopey=make_array(nx,/float)

ymin = min(y)
ymax = max(y)
ny = y - ymin

peak = ymax
hpeak = 0.5 * max(ny)
y_hpeak= hpeak + ymin

; area = int_tabulated(x,ny)
; harea = 0.5 * area

if list then print,'    I      X       Y      delta_A     A      Slope    Y-Ymin
d0=0
for i=1,nx-1 do begin
	dx = x(i) - x(i-1)
	if dx ne 0. then begin
	da(i) = 0.5 *(ny(i)+ny(i-1)) * dx
	d0 = d0 + da(i)
	a(i) = d0
	slopey(i)= (ny(i)-ny(i-1))/dx
	if list then print,strtrim(i,1),x(i),y(i),da(i),a(i),slopey(i),ny(i)
	end
end

area = d0
harea = 0.5 * area

; Find c_mass

newtons_method,x,a,harea,c_mass
if list then print,'===='
if list then print,'C_mass',harea,c_mass


; Find half peaks

if list then print,'===='

;if abs(ny(0)-hpeak) lt 1.e-5 then begin
; 	x_hwdl=x(0)
;	nohwdl=0
;end
for i=1,nx-1 do begin
	yl = ny(i-1) - hpeak
	yr = ny(i) - hpeak
        if yl*yr lt 0. then begin
		if n_elements(nohwdl) eq 0 then nohwdl = i-1 else $
		nohwdl = [nohwdl, i-1]
		newtons_method,[x(i-1),x(i)],[yl,yr],0.,x_sol,notfound
		if n_elements(x_hwdl) eq 0 then x_hwdl = x_sol else $
		x_hwdl= [x_hwdl,x_sol]
		i=i+1
	endif else begin
		if abs(yl) lt 1.e-5 then begin
			x_hwdl=[x_hwdl,x(i-1)]
			nohwdl = [nohwdl, i-1]
		end
	end	
end

	lo=0
	fwhm = 0.
if n_elements(nohwdl) gt 1 then begin 
	x_hwd = x_hwdl(0:n_elements(nohwdl)-1)
	nohw = n_elements(nohwdl) / 2
	if slopey(nohwdl(0)) lt 0. then nohw = (n_elements(nohwdl)- 1) / 2
	fwhm_wd = make_array(nohw,/float)		; fwhm width
	fwhm_xl = make_array(nohw,/float)		; fwhm start point 

	is = 0
	if slopey(nohwdl(0)) lt 0. then is=1	
	for i=0,nohw-1 do begin
		x1 = x_hwdl(is)
		fwhm_xl(i) = x1
		fwhm_wd(i) = abs(x_hwdl(is+1) - x1)
		lo=lo+1
;		print,'FWHM',lo, fwhm_xl(i), fwhm_wd(i)
		is=is+2
	end
	FWHM = max(fwhm_wd,imax)
	if keyword_set(list) then begin
		print,'Y_MIN,Y_HPeak,Y_PEAK',ymin,y_hpeak,peak
		print,'HPeak pts:',x_hwdl
		print,'FWHM start point:',fwhm_xl
		print,'FWHM width      :',fwhm_wd
	end
end


; Find peaks

if keyword_set(FIT) then begin
nopeaks=0
if list then print,'===='
for i=1,nx-1 do begin
       if slopey(i-1) gt 0 and slopey(i-1)*slopey(i) lt 0. then begin
;		print,i,slopey(i-1),slopey(i)
		nopeaks = [nopeaks, i]
		end
end
print,'nopeaks',nopeaks
no = n_elements(nopeaks)-1
if no gt 0 then begin
x_peak = make_array(no,/float)
y_peak = make_array(no,/float)
for i=1,no do begin
	i2= nopeaks(i)
	i1= i2-1
	newtons_method,[x(i1),x(i2)],[slopey(i1),slopey(i2)],0.,x_sol,notfound
	if notfound eq 0 then begin
if list then 	print,'Peak #',i,x_sol,y(i1)
		x_peak(i-1)= x_sol
		y_peak(i-1) = y(i1)
		end
end
endif else begin
	y_peak = ymax
	if y(0) gt y(nx-1) then x_peak = x(0) else x_peak = x(nx-1)
if list then 	print,'Ymax at pt ',y_peak,x_peak
end
endif else begin

	for i=0,nx -1 do begin
		if y(i) eq peak then begin
		x_peak = x(i)
		y_peak = peak
		return
		end
	end
end

END



PRO newtons_method,x,y,y_sol,x_sol,notfound
notfound = 0
nx = n_elements(y)
n1 = 0 
n2 = nx-1 
RETEST:
;print,'N1,N2',n1,n2,y(n1),y(n2)
if (n2-n1) le 1 then begin
	if (y_sol - y(n2)) * (y_sol - y(n1)) gt 0 then begin
		x_sol= x(n1)
		notfound = 1
		return
		end
	if (x(n2)-x(n1)) eq 0. then begin
		x_sol = x(n1)
		return
	end
	x_sol = x(n1)+ (y_sol - y(n1)) /(y(n2)-y(n1)) *(x(n2)-x(n1))
;print,y_sol,y(n1),y(n2),x(n1),x(n2)
	 return
	end
 
nm = (n2-n1)/ 2 + n1
fm = y (nm)
;print,nm,fm,y_sol
if abs(fm-y_sol) le 1.e-5 then begin
	x_sol = x(nm)
;	print,'Stop at NM,x_sol',nm,x_sol
	return
endif else begin
	if (fm-y_sol) *(y(n2) - y_sol) gt 0 then begin
		n2 = nm
	endif else  begin
		n1 = nm
	end
	goto,RETEST
	end
END

;
; using index and factor instead of real value for x array
;
PRO newtons_method_norm,x,y,y_sol,n1,x_sol,notfound
	rx = float(x)
	newtons_method,rx,y,y_sol,x_sol,notfound
	n1 = fix(x_sol)
	x_sol = x_sol-float(n1)
END


PRO lorentzian_curve,a,x,y,plot=plot
;+
; NAME:
; 	LORENTZIAN_CURVE
;
; PURPOSE:
;       For a given set of lorentzian parameters [a0,a1,a2], this routine
;       calculate the corresponding {y(i)} for a given set of {x(i)}.
;
; CATEGORY:
; 	Fitted data with plot.
;
; CALLING SEQUENCE:
;       LORENTZIAN_CURVE, A, X, Y [,/PLOT]
;
; INPUTS:
;       A:        Fitted lorentzian coefficients, [A0,A1,A2].
;       X:        Position X vector of type float or double. 
;       Y:        Data value Y vector of type float or double. 
;	
; KEYWORD PARAMETERS:
;   PLOT:       Specifies whether to plot the Y vector.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;      Run the geometric fitting, and pops up the fitting result window
;
;      X = [ ...]
;      A = [Peak, Mean, FWHM/2] 
;      LORENTZIAN_CURVE,A,X,Y,/PLOT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 10-15-97.
;      xx-xx-xxbkc  comment
;-
N = n_elements(x)
y=make_array(N)
for i=0,N-1 do begin
        lorentzian,x(i),a,f
        y(i)=f
end
if keyword_set(plot) then plot,x,y
END

PRO lorentzfitgraf,x,y,a,print=print,yfit=yfit
;+
; NAME:
; 	LORENTZFITGRAF
;
; PURPOSE:
;       This routine uses the CURVEFIT function with the non-linear fitting
;       function_name='lorentzian' specified. y=f(x) where:
;         F(X) = A0 * A2^2 / ((X-A1)^2 + A2^2) and
;          A0 = height of exp, A1 = center of exp, A2 = FWHM/2
;
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       LORENTZFITGRAF, X, Y [,A] [,/PRINT] [,YFIT=yfit]
;         
;
; INPUTS:
;
;       X:        Position X vector of type float or double. 
;       Y:        Data value Y vector of type float or double. 
;       A:        Optional input [A0,A1,A2], initial estimates of 
;                 fitting coefficients.
;	
; KEYWORD PARAMETERS:
;   PRINT:       Specifies whether the output window will be poped up.
;   YFIT:        Y vector calculated from the fitted equation.
;
; SIDE EFFECTS:
;      The computed parameters and the convergence may depend on the data and
;      the initial parameters of A vector entered.
;
; RESTRICTIONS:
;      The number of parameters must be three. The initial value should be
;      close to the real data value with:
;          A0 = height of exp, A1 = center of exp, A2 = FWHM/2
;      Especially the center of expectation must corresponds to the peak 
;      of the lorentzian.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;      Run the geometric fitting, and pops up the fitting result window
;
;      X = [ ...]
;      Y = [ ...]
;      A = [ Peak, Mean, FWHM/2 ]
;      LORENTZFITGRAF,X,Y,A,/GEOMETRIC,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 10-15-97.
;      xx-xx-xxbkc  comment
;-
if n_params() lt 2 then begin
	str = ['Usage: lorentzfitGraf, X, Y [,A] [,/PRINT] [,YFIT=yfit]', '',$
	  	'      Y = A0 * A2^2 / ( (X-A1)^2 + A2^2 )']
	res=widget_message(str,/info,title='FITTING Info')
	return
end

statistic_1d,x,y,c_mass,x_peak,y_peak,y_hpeak,fwhm,fwhm_xl,fwhm_wd
	if n_elements(fwhm_wd) eq 0 then begin
	res = WIDGET_MESSAGE('Data not suitable for lorentzian fit !',/info)
	return
	end
a=[y_peak, x_peak, 0.5*fwhm_wd]

Weights= replicate(1.0,n_elements(x))

if n_elements(a) lt 3 then a=[1.,10.,20.] 

yfit = curvefit(x,y,Weights,a,sigma,function_name='lorentzian',/noderiv)
;yfit = curvefit(x,y,Weights,a,sigma,function_name='lorentzian')

        yres = yfit - y
        goodness = goodness_fit(yres,1)

        curv=make_array(n_elements(y),2)
        curv(0,0) = float(yfit)
        curv(0,1) = float(y)

	title = 'Lorentzian fit'
	comment = 'Y = A0 * A2^2 / ( (X-A1)^2 + A2^2 ) '
	for i=0,n_elements(A)-1 do comment= [comment,'A'+strtrim(i,2)+'='+ $
		strtrim(A(i),2) + '     SIGMA='+strtrim(sigma(i),2)]

        comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

	plot1d, x, curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		wtitle='Lorentzian Fit'


	if keyword_set(print) then begin
        OPENW,unit,'fitting.tmp',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end

	printf,unit,'LORENTZIAN FIT - ',title
	printf,unit,''

        vec = moment(y, mdev=md, sdev=sd)
        mean = vec(0)
        variance = vec(1)
        printf,unit,'        MEAN=',mean
        printf,unit,'        SDEV=',sqrt(variance)
        printf,unit,'    VARIANCE=',variance

        printf,unit,''
        printf,unit,'    Centroid @ X=',c_mass
        printf,unit,'        Y_Peak  =',y_peak
        printf,unit,'          @  X  =',x_peak
        printf,unit,'        Y_Hpeak =',y_hpeak
        printf,unit,'        FWHM    =',fwhm
        printf,unit,'        FWHM_xl =',fwhm_xl
        printf,unit,'        FWHM_wd =',fwhm_wd
        printf,unit,''
	for i=0,n_elements(comment)-1 do printf,unit,comment(i)
        printf,unit,''


	printf,unit,'         X           Y           YFIT        YFIT - Y'
	for i=0,n_elements(x)-1 do printf,unit,x(i),y(i),yfit(i),yres(i)
	FREE_LUN,unit
	xdisplayfile,'fitting.tmp',title=title
	end

END


PRO lorentzian,x,a,f,pder
; function + partials

if n_elements(a) eq 3 then begin
a1 = a(0)
a2= a(1)
a3= a(2)
deno =  (x-a2)^2 + a3^2 
f = a1 * a3^2 /deno
f1 = a3^2 / deno
f2 = 2*a1*a3^2*(x-a2) / deno^2
f3 = 2*a1*a3*(x-a2)^2 / deno^2
if n_params() ge 4 then $
	pdef = [ [f1],[f2],[f3] ]
end

END


PRO multi_lorentzian,x,a,f,pder
; function + partials

N = n_elements(a)/3
if n_elements(a) eq 3*N then begin
	f=0. 
	f1=0.
	f2=0.
	f3=0.
	For i=0,N-1 do begin
	a1 = a(i*3 + 0)
	a2= a(i*3 + 1)
	a3= a(i*3 + 2)
	deno =  (x-a2)^2 + a3^2 
	f = f + a1 * a3^2 /deno
	f1 = f1 + a3^2 / deno
	f2 = f2 + 2*a1*a3^2*(x-a2) / deno^2
	f3 = f3 + 2*a1*a3*(x-a2)^2 / deno^2
	end
if n_params() ge 5 then $
	pdef = [ [f1],[f2],[f3] ]
endif else begin
	res=widget_message('Error: inconsistant parameter numbers',/error)
end

END


PRO multi_lorentzfitgraf,x,y,a,print=print,yfit=yfit,subplot=subplot
; A0:   Ymax at x=mu
; A1:   MU 
; A2:   FWHM / 2.
; A=[[A0,A1,A2],[A0(1),A1(1),A2(1)], ...]
;+
; NAME:
; 	MULTI_LORENTZFITGRAF
;
; PURPOSE:
;       This routine uses the CURVEFIT function to fit a set of multiple 
;       LORENTZIAN functions y=f(x) where:
;         F(X) = A0 * A2^2 / ((X-A1)^2 + A2^2) + ... and
;          A0 = height of exp, A1 = center of exp, A2 = FWHM/2
;
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       MULTI_LORENTZFITGRAF, X, Y, A [,YFIT=yfit] [,/PRINT] [,/SUBPLOT] 
;         
;
; INPUTS:
;
;       X:        Position X vector of type float or double. 
;       Y:        Data value Y vector of type float or double. 
;       A:        Lorentzian coefficients vector. It consists of a set of 
;                 multiple of 3 parameters for each lorentzian.  
;                 [ [A0,A1,A2], [A0,A1,A2], ... ] 
;	
; KEYWORD PARAMETERS:
;   YFIT:        Y vector calculated from the fitted lorentzian equations.
;   PRINT:       Specifies whether the output window will be poped up.
;   SUBPLOT:     If specified, a plot window shows the composition of all
;                the multiple lorentzian curves.
;
; SIDE EFFECTS:
;      The computed parameters and the convergence may depend on the data and
;      the initial parameters of A vector entered.
;
; RESTRICTIONS:
;      The number of parameters must be multiple of 3. The initial value 
;      should be close to the real data value with:
;          A0 = height of exp, A1 = center of exp, A2 = FWHM/2
;      Especially the center of expectation must corresponds to each local
;      peak of the lorentzian.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;      Run the geometric fitting, and pops up the fitting result window
;
;      X = [ ...]
;      Y = [ ...]
;      A = [ Peak1, Mean1, FWHM1/2, Peak2, Mean2, FWHM2/2, ... ]
;      MULTI_LORENTZFITGRAF,X,Y,A,/GEOMETRIC,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 10-15-97.
;      xx-xx-xxbkc  comment
;-
if n_params() lt 3 then begin
	str = ['Usage: multi_lorentzfitGraf, X, Y ,A, /PRINT', '',$
	  	'      Y = A0 * A2^2 / ( (X-A1)^2 + A2^2 ) + ...']
	res=widget_message(str,/info,title='FITTING Info')
	return
end

N = n_elements(A) / 3
NEWA = make_array(3,N)
if n_elements(A) ne 3*N then begin
	res=widget_message('Error: number of coefficients in A is not right.',/error)
	return
end

Weights= replicate(1.0,n_elements(x))

yfit = curvefit(x,y,Weights,a,sigma,function_name='multi_lorentzian',/noderiv)

	yres = yfit - y
	goodness = goodness_fit(yres,N)

for i=0,N-1 do NEWA(*,i) = a(i*3:i*3+2)

        curv=make_array(n_elements(y),2)
        curv(0,0) = float(yfit)
        curv(0,1) = float(y)

	title = 'Multiple Lorentzian fit'
	comment = 'Y = A0 * A2^2 / ( (X-A1)^2 + A2^2 ) + ... '
	for i=0,N-1 do begin
	comment=[comment,string(i)+string(NEWA(0,i)) $
		+ string(NEWA(1,i)) + string(NEWA(2,i))]
	end

	comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

; plot seperate lines
if keyword_set(subplot) then begin
ytemp = make_array(n_elements(x),N+2)
ytemp(*,0) = yfit
ytemp(*,1) = y
for i=0,N-1 do begin
	a = NEWA(*,i)	
	lorentzian_curve,a,x,temp
	ytemp(*,i+2) = temp
end
	plot1d,x,ytemp,/symbol,title=title
end


	plot1d, x, curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		wtitle='Multiple Lorentzian Fit'


	if keyword_set(print) then begin
        OPENW,unit,'fitting.tmp',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end

	printf,unit,'MULTIPLE LORENTZIAN FIT - ',title
	printf,unit,''

        vec = moment(y, mdev=md, sdev=sd)
        mean = vec(0)
        variance = vec(1)
        printf,unit,'        MEAN=',mean
        printf,unit,'        SDEV=',sqrt(variance)
        printf,unit,'    VARIANCE=',variance
        printf,unit,''

	for i=0,n_elements(comment)-1 do printf,unit,comment(i)
        printf,unit,''

	for i=0,N-1 do begin
        printf,unit,'    	i    =',i 
        printf,unit,'        Ymax(i) =',newa(0,i)
        printf,unit,'         MU(i)  =',newa(1,i)
        printf,unit,'        FWHM(i) =',newa(2,i)*2.
        printf,unit,''
	end

	printf,unit,'         X           Y           YFIT      YFIT - Y '
	for i=0,n_elements(x)-1 do printf,unit,x(i),y(i),yfit(i),yres(i)
	FREE_LUN,unit
	xdisplayfile,'fitting.tmp',title=title
	end

END

FUNCTION goodness_fit,yres,N,Weight=Weight
;  sqrt(( W * YRES) ^2 / (M-N))

	M = n_elements(yres)
	if M gt N then begin
	goodness = 0.
	if keyword_set(Weight) then $
	for i=0,M-1 do goodness = goodness + ( Weight(i)*yres(i))^2 else $
	for i=0,M-1 do goodness = goodness + yres(i) ^ 2 
	goodness = sqrt(goodness /( M - N))
	return,goodness
	end
END
PRO regressfitgraf,x,y,weights,yfit,const,sigma,ftest,r,rmul,chisq,status, $
	relative_weight=relative_weight, $
	print=print,test=test

if n_params() eq 0 and keyword_set(test) eq 0 then begin
	str='Usage: regressfitGraf,X,Y,Weights,yfit,const[,sigma,ftest,r,rmul,chisq,status,/RELATIVE_WEIGHT,/PRINT]'
	str=[str,'',$
	'Y = A0 + A1 * X1 + A2 * X2 + A3 * X3 + A4 * X4 + ...', $
	'','MULTIPLE Linear Regression Fit with Weights']
	res=widget_message(str,/info,title='FITTING Info')
		return
end

if keyword_set(test) then begin
;Create a two by six array of independent variable data.

X = [[0.0, 0.0], $     
     [2.0, 1.0], $
     [2.5, 2.0], $
     [1.0, 3.0], $
     [4.0, 6.0], $    
     [7.0, 2.0]]

;Create an Npoints-element vector of dependent variable data.

Y = [5.0, 10.0, 9.0, 0.0, 3.0, 27.0]

end

	sz1 = size(x)
	nterms = sz1(1)
	npoints = sz1(2)
	sz2 = size(y)

	if sz2(0) ne 1 and sz2(1) ne npoints then begin
	str =[ 'Inconsistant dimension in X, Y input','NTERMS=',string(nterms), $
		'NPOINTS=',string(npoints), $
		' Y = A0 + A1*X1 + A2*X2 + ... ']
	res = widget_message(str,/info,title='Fitting - multiple regress')
	return
	end

	weights = replicate(1.0,n_elements(y))
	result = regress(x,y,weights,yfit,const,sigma,ftest,r,rmul,chisq,status,/RELATIVE_WEIGHT)
;if keyword_set(print) then begin
;print,'sigma',sigma
;print,'ftest',ftest
;print,'r',r
;print,'rmul',rmul
;print,'chisq',chisq
;print,'status',status
;end

	curv=make_array(n_elements(y),2)
	curv(0,0) = float(yfit(*))
	curv(0,1) = float(y)

	title = 'Multiple Linear Regression Fit with Weights' 
	comment = 'Y(i) = A0'
	for i=1,nterms do comment=comment+' + A'+strtrim(i,2)+' * X' + $
			strtrim(i,2) + '(i)'

	comment = ['',comment,'A0 = '+strtrim(const,2)]
	for i=0,nterms-1 do begin
		str = 'A'+strtrim(i+1,2)+' = '+strtrim(result(0,i),2)
		comment=[comment,str]
		end
	
	plot1d,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		wtitle='REGRESS'
;	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
;		wtitle='REGRESS'

	if keyword_set(print) then begin
        OPENW,unit,'fitting.tmp',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end


	printf,unit,'REGRESS - ',title
	printf,unit,''
	printf,unit,' X [',strtrim(sz1(1),2),' x ',strtrim(sz1(2),2),'] :'
	printf,unit,X
	printf,unit,' Y [',strtrim(sz2(1),2),'] :'
	printf,unit,Y
	printf,unit,''

 	for i=0,n_elements(comment) - 1 do printf,unit,comment(i)	
	printf,unit,''
	printf,unit,'          Weights      Y             YFIT'
	for i=0,n_elements(y)-1 do printf,unit,weights(i),Y(i),YFIT(i)
	FREE_LUN,unit
	xdisplayfile,'fitting.tmp'
	end
END
PRO get_svdfit_function,fname,expres
ffname= strlowcase(fname)
if ffname eq 'svdfunct' then begin
	express='Function Name Used:  SVDFUNCT'
        expres=[ express,'F = A0 + A1*X + A2*X^2 + A3*X^3 + ...']
	return
	end
if ffname eq 'legendre' then begin
	express='Function Name Used:  SVDLEG'
        expres=[ express,'P0(X) = 1']
        expres=[expres,'P1(X) = X ']
        expres= [expres,'P2(X) = (3 * X^2 - 1)/2']
        expres=[expres,'P3(X) = (5 * X^3 - 3 * X) / 2 ', ' ...']
        expres=[expres,'Pn(X) = ((2n - 1)* X * Pn-1(X) - (n-1) * Pn-2(X)) / n ']
	return
        end
get_svdfit_myfunct,fname,expres  ; private myfunct
if n_elements(expres) eq 0 then expres='Function Name Used in SVDFIT:'+ffname
END

PRO get_svdfit_myfunct,fname,expres
if fname eq 'myfunct' then begin
	express='Function Name Used: MYFUNCT'
        expres=[express,'Y = A0 + A1 * sin(2*X)/X + A2 * cos(4*X)^2 ']
        end
if n_elements(expres) eq 0 then expres='Function Name Used: SVDFUNCT '
END

FUNCTION myfunct, X ,M
if M ne 3 then $ 
    res=widget_message('For myfunct the NTERMS must be 3', $
		/info,title='FITTING Info')
    return,[ [1.0], [SIN(2*X)/X], [COS(4.*X)^2.] ]
END

PRO svdfitgraf,x,y,NTERMS,weights=weights,print=print,test=test, $
	function_name=function_name,legendre=legendre,_Extra=extra

if n_params() eq 0 and keyword_set(test) eq 0 then begin
	str='Usage: svdfitGraf,X,Y,NTERMS [,/PRINT]'
	str=[str,'',$
	'','General Least Squares Fit with optional error estimates', $
	'', '         User-supplied function', $
	'  or       Legendre polynomial']
	res=widget_message(str,/info,title='FITTING Info')
		return
end

if keyword_set(test) then begin

fname = 'myfunct'
C = [7.77, 8.88, -9.99]          ;Provide an array of coefficients.
X = FINDGEN(100)/15.0 + 0.1
Y = C(0) + C(1) * SIN(2*X)/X + C(2) * COS(4.*X)^2.

sig = 0.05 * Y                        ; Set uncertainties to 5%
A=[1,1,1]                               ;Provide an initial guess
ws = 1/sig^2

	result = SVDFIT(X, Y, A=A, WEIGHTS=ws, $
	FUNCTION_NAME=fname, SIGMA=SIGMA, YFIT=YFIT)
	FOR I = 0, N_ELEMENTS(A)-1 DO $
    	PRINT, I, result(I), SIGMA(I), C(I),$
    	FORMAT = '(" A( ",I1," ) = ",F7.4," +- ",F7.4," VS. ",F7.4)'

endif else begin
	if n_elements(NTERMS) eq 0 then NTERMS=3
	A = replicate(1.,NTERMS)
	ws = replicate(1.,n_elements(y))
	if keyword_set(weights) then ws=weights
	if keyword_set(function_name) then fname=function_name else $
	fname = 'svdfunct'
	if keyword_set(legendre) then begin
	 fname='legendre'
	result = svdfit(x,y,A=A,yfit=yfit,weights=ws, $
		legendre=legendre,sigma=sigma, _Extra=extra)
	endif else begin
	result = svdfit(x,y,A=A,yfit=yfit,weights=ws, $
		function_name=fname,sigma=sigma, _Extra=extra)
	end
end


	curv=make_array(n_elements(y),2)
	curv(0,0) = float(yfit(*))
	curv(0,1) = float(y)

	title = 'SVDFIT - General Least Squares Fit with Weights' 
	get_svdfit_function,fname,comment
	
	for i=0,n_elements(A)-1 do begin
		str = 'A'+strtrim(i,2)+' = '+strtrim(result(i),2)
		comment=[comment,str]
		end
	
	yres = yfit - y
	goodness = goodness_fit(yres,n_elements(A))
        comment = [comment,'','GOODNESS OF FIT = '+string(goodness)]

	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		wtitle='SVDFIT'

	if keyword_set(print) then begin
        OPENW,unit,'fitting.tmp',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end


	printf,unit,title
	printf,unit,''

 	for i=0,n_elements(comment) - 1 do printf,unit,comment(i)	
	printf,unit,''
	printf,unit,'      X             Y             YFIT        YFIT-Y      WEIGHT'
	for i=0,n_elements(y)-1 do printf,unit,x(i),Y(i),YFIT(i),yres(i),ws(i)
	FREE_LUN,unit
	xdisplayfile,'fitting.tmp',title=title
	end
END
PRO curvefit_setup_help
str = [ 'Itmax    - Maximun number of iterations for fitting.', $
	'TOL      - The convergence tolerance. The routine returns when the',$
	'           relative decrease in chi-squared is less than TOL in an iteration.', $
	'NoDerivative - Yes/No. If analytical derivatives are available then',$
	'               they should be defined in the function defenition.', $
	'               If No is set the analytical derivatives will be used.', $
	'               If Yes is set the forward differences will be used in',$
	'               estimate of the partial derivatives.', $ 
	'Function_Name   - Specifies the fit function to be used by the CURVEFIT. ',$
	'                  It defaults to FUNCT.', $
	'Parameters in Fit Function - Specifies the starting fit coefficients.', $
	'                  Values entered must be separated by the comma.',$
	'                  The number of values entered must consist with the',$
	'                  fit function used. If not given the default values ',$
	'                  will be used.',$
	'                  The fit function should provide the default values for', $
	'                  the fit coefficients.',$
	'Accept     - Accepts the setting and excutes the CURVEFIT',$
	'Help       - Provides this help info.',$
	'Close      - Closes the curvefit_setup dialog.' $
	]
	res=widget_message(str,title='CURVEFIT_SETUP_HELP',/info)
END

PRO CURVEFIT_SETUP_Event, Event
COMMON EZ_FIT_BLOCK,ezfitData,image

  WIDGET_CONTROL,Event.Top,GET_UVALUE=info
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'CURVEFIT_ITMAX': BEGIN
      Print, 'Event for Itmax'
      END
  'CURVEFIT_TOL': BEGIN
      Print, 'Event for TOL'
      END
  'CURVEFIT_NODERIV': BEGIN
	ezfitData.curvefit_noderiv = Event.index
      END
  'CURVEFIT_FNAME': BEGIN
      Print, 'Event for Function_Name'
      END
  'CURVEFIT_PARAMS': BEGIN
      Print, 'Event for parameters in Vector A'
      END
  'BUTTON8': BEGIN
	curvefit_setup_help
      END
  'BUTTON9': BEGIN
      WIDGET_CONTROL,info.tol_fld,GET_VALUE=tol 
      WIDGET_CONTROL,info.itmax_fld,GET_VALUE=itmax
      WIDGET_CONTROL,info.fname_fld,GET_VALUE=fname
      WIDGET_CONTROL,info.A_fld,GET_VALUE=A
	newa = strcompress(strtrim(a(0),2))
	params=str_sep(newa,',')

     ezfit_picktype,x,y
	a = float(params)
	name = strlowcase(fname(0))
        statistic_1d,x,y,c_mass,x_peak,y_peak,y_hpeak,fwhm,fwhm_xl,fwhm_wd

; if lorentzian type
	if name eq 'lorentzian' then $ 
	a=[y_peak, x_peak, 0.5*fwhm_wd]

; six parameters required in funct
	if name eq 'funct' then begin
        sd= 0.05 * y
        a0 = y_peak
        a1 = x_peak
        a2 = 1.
        a3 = -1.
        a4 = 1.
        a5 = -1.
        A = [a0,a1,a2,a3,a4,a5]
        end
Weights=replicate(1.0,n_elements(x))
if keyword_set(gaussian) and n_elements(sd) then Weights=1.0 / sd ; Gaussian
if keyword_set(poisson) and min(y) gt 0. then Weights=1.0 / y       ; Poisson

sigma = replicate(1.,n_elements(A))

	if ezfitData.curvefit_noderiv then $
     	curvefitGraf,x,y, Weights, A, sigma, itmax=itmax, tol=tol, $
		function_name=name,/print,/noderivative else $ 
     	curvefitGraf,x,y, Weights, A, sigma, itmax=itmax, tol=tol, function_name=name,/print 
	
      END
  'BUTTON10': BEGIN
      WIDGET_CONTROL,Event.Top,/DESTROY
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END CURVEFIT_SETUP
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO curvefit_setup, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  CURVEFIT_SETUP = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, $
      TITLE='CURVEFIT_SETUP', $
      UVALUE='CURVEFIT_SETUP')

  BASE2 = WIDGET_BASE(CURVEFIT_SETUP, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  FieldVal234 = [ $
    '20' ]
  CURVEFIT_ITMAX = CW_FIELD( BASE3,VALUE=FieldVal234, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Itmax', $
      UVALUE='CURVEFIT_ITMAX', $
      XSIZE=4)

  FieldVal299 = [ $
    '1.e-3' ]
  CURVEFIT_TOL = CW_FIELD( BASE3,VALUE=FieldVal299, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='TOL', $
      UVALUE='CURVEFIT_TOL', $
      XSIZE=8)

  CURVEFIT_NODERIV = WIDGET_DROPLIST(BASE3,title='NoDerivative', $
	value=['No','Yes'], $
	UVALUE='CURVEFIT_NODERIV')
  WIDGET_CONTROL,CURVEFIT_NODERIV,SET_DROPLIST_SELECT=0


  FieldVal429 = [ $
    'FUNCT' ]
  CURVEFIT_FNAME = CW_FIELD( BASE2,VALUE=FieldVal429, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Procedure_Name', $
      UVALUE='CURVEFIT_FNAME', $
      XSIZE=20)


  FieldVal429 = [ $
    '' ]
  CURVEFIT_PARAMS = CW_FIELD( BASE2,VALUE=FieldVal429, $
      COLUMN=1, FRAME=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Initial Fitting Coefficients (comma separated)', $
      UVALUE='CURVEFIT_PARAMS', $
      XSIZE=50)

  BASE8 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE8')

  BUTTON9 = WIDGET_BUTTON( BASE8, $
      UVALUE='BUTTON9', $
      VALUE='Accept')

  BUTTON8 = WIDGET_BUTTON( BASE8, $
      UVALUE='BUTTON8', $
      VALUE='Help')

  BUTTON10 = WIDGET_BUTTON( BASE8, $
      UVALUE='BUTTON10', $
      VALUE='Close')


  info = { itmax_fld: CURVEFIT_ITMAX, $
	tol_fld: CURVEFIT_TOL, $
	fname_fld: CURVEFIT_FNAME, $
	A_fld: CURVEFIT_PARAMS, $
	itmax: 20, $
	tol: 1.e-3, $
	noderiv:0, $
	fname: 'funct', $
	A: make_array(50) $
	}

  WIDGET_CONTROL, CURVEFIT_SETUP, SET_UVALUE=info
  WIDGET_CONTROL, CURVEFIT_SETUP, /REALIZE

  XMANAGER, 'CURVEFIT_SETUP', CURVEFIT_SETUP
END

PRO ROIFIT2_Event, Event

COMMON ROIFIT2_BLOCK,widget_ids

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev


IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN begin
	print,'kill the application ?'
        end

  CASE Ev OF 
  'ROI_SELECT': BEGIN
	widget_ids.roi = Event.index
	xl = strtrim(widget_ids.xl_val(Event.index),2)
	WIDGET_CONTROL,widget_ids.left_fld,SET_VALUE=xl
	xl = strtrim(widget_ids.xr_val(Event.index),2)
	WIDGET_CONTROL,widget_ids.right_fld,SET_VALUE=xl
	xl = strtrim(widget_ids.A0(Event.index),2)
	WIDGET_CONTROL,widget_ids.intensity,SET_VALUE=xl
	xl = strtrim(widget_ids.A1(Event.index),2)
	WIDGET_CONTROL,widget_ids.centroid,SET_VALUE=xl
	xl = strtrim(2.*widget_ids.A2(Event.index),2)
	WIDGET_CONTROL,widget_ids.fwhm,SET_VALUE=xl
	END
  'ROI_LEFT': BEGIN
	END
  'ROI_RIGHT': BEGIN
	END
  'ROI_CENTROID': BEGIN
	WIDGET_CONTROL, widget_ids.centroid, GET_VALUE= x_peak
        widget_ids.A1(widget_ids.roi) = float(x_peak)
	END
  'ROI_FWHM': BEGIN
	WIDGET_CONTROL, widget_ids.fwhm, GET_VALUE= fwhm
        widget_ids.A2(widget_ids.roi) = float(fwhm) * .5
	END
  'ROI_INTENSITY': BEGIN
	WIDGET_CONTROL, widget_ids.intensity, GET_VALUE= y_peak
        widget_ids.A0(widget_ids.roi) = float(y_peak)
	END
  'ROI_CENTROID_Y': BEGIN
	print,'ROI_CENTROID_Y',Event.Value
	END
  'ROI_FWHM_Y': BEGIN
	print,'ROI_FWHM_Y',Event.Value
	END
  'ROI_INTENSITY_Y': BEGIN
	print,'ROI_INTENSITY_Y',Event.Value
	END
  'SLIDER_CENTROID': BEGIN
	xl = widget_ids.x(Event.Value)
	WIDGET_CONTROL,widget_ids.centroid,SET_VALUE=strtrim(xl,2)
	widget_ids.A1(widget_ids.roi) = xl
      END
  'SLIDER2': BEGIN
	widget_ids.xl= Event.Value
	xl = widget_ids.x(Event.Value)
	WIDGET_CONTROL,widget_ids.left_fld,SET_VALUE=strtrim(xl,2)
      END
  'SLIDER3': BEGIN
	widget_ids.xr= Event.Value
	xr = widget_ids.x(Event.Value)
	WIDGET_CONTROL,widget_ids.right_fld,SET_VALUE=strtrim(xr,2)
      END
  'ROI_CLEAR': BEGIN
	widget_ids.code(widget_ids.roi) = 0
	widget_ids.A0(widget_ids.roi) = 0. 
	widget_ids.A1(widget_ids.roi) = 0. 
	widget_ids.A2(widget_ids.roi) = 0.
	WIDGET_CONTROL, widget_ids.centroid, SET_VALUE='0.'
	WIDGET_CONTROL, widget_ids.fwhm, SET_VALUE= '0.'
	WIDGET_CONTROL, widget_ids.intensity, SET_VALUE= '0.'
      END
  'ROI_ADD': BEGIN
if widget_ids.code(widget_ids.roi) eq 1 and widget_ids.roi lt 19 then begin
	widget_ids.roi = widget_ids.roi + 1
	WIDGET_CONTROL,widget_ids.roi_select,SET_DROPLIST_SELECT=widget_ids.roi
end
	widget_ids.code(widget_ids.roi) = 1
	newSegment,widget_ids.x,widget_ids.y,widget_ids.xl, widget_ids.xr,$
		newX,newY,newW 
	statistic_1d,newX,newY,c_mass,x_peak,y_peak,y_hpeak,fwhm,fwhm_xl,fwhm_wd
	widget_ids.A0(widget_ids.roi) = y_peak
	widget_ids.A1(widget_ids.roi) = x_peak
	widget_ids.A2(widget_ids.roi) = fwhm * .5
	WIDGET_CONTROL, widget_ids.centroid, SET_VALUE= strtrim(x_peak,2)
	WIDGET_CONTROL, widget_ids.fwhm, SET_VALUE= strtrim(fwhm,2)
	WIDGET_CONTROL, widget_ids.intensity, SET_VALUE= strtrim(y_peak,2)
	widget_ids.xl_val(widget_ids.roi) = newX(0)
	widget_ids.xr_val(widget_ids.roi) = max(newX)
        END
  'ROI_PANEL': BEGIN
	widget_ids.code(widget_ids.roi) = 1
	WIDGET_CONTROL, widget_ids.left_fld, GET_VALUE= xl 
	WIDGET_CONTROL, widget_ids.right_fld, GET_VALUE= xr 
	WIDGET_CONTROL, widget_ids.intensity, GET_VALUE= y_peak
	WIDGET_CONTROL, widget_ids.centroid, GET_VALUE= x_peak
	WIDGET_CONTROL, widget_ids.fwhm, GET_VALUE= fwhm
	widget_ids.xl_val(widget_ids.roi) = float(xl)
	widget_ids.xr_val(widget_ids.roi) = float(xr)
	widget_ids.A0(widget_ids.roi) = float(y_peak)
	widget_ids.A1(widget_ids.roi) = float(x_peak)
	widget_ids.A2(widget_ids.roi) = float(fwhm) * .5
        END
  'ROI_LIST': BEGIN
	for i=0,widget_ids.n_roi -1 do begin 
	if widget_ids.code(i) then begin
	str = string(i)+string(widget_ids.xl_val(i))+ $
		string(widget_ids.xr_val(i)) + $
		string(widget_ids.A0(i)) + $
		string(widget_ids.A1(i)) + string(widget_ids.A2(i))
	if n_elements(A) eq 0 then $
	A = ['      ROI         XL          XR         Intensity   Centroid       FWHM','-------------------------------------------------------------------------',str] else $
	A =[A,str]
	end
	end
	WIDGET_CONTROL,widget_ids.text,SET_VALUE=A
	END
  'ROI_CALC': BEGIN
	for i=0,widget_ids.n_roi -1 do begin 
	if widget_ids.code(i) then begin
	if n_elements(A) eq 0 then A = [widget_ids.A0(i),widget_ids.A1(i), widget_ids.A2(i)] else $
	A =[A,widget_ids.A0(i),widget_ids.A1(i), widget_ids.A2(i)]
	end
	end
	x = widget_ids.x
	y = widget_ids.y
	if widget_ids.subplot then $
	multi_lorentzfitgraf,x, y, A, /print, /subplot else $
	multi_lorentzfitgraf,x, y, A, /print
	END
  'ROI_SUBPLOT': BEGIN
	widget_ids.subplot = Event.index
	END
  'ROI_HELP': BEGIN
	str = ['Y = A0 * A2^2 / ( (X-A1)^2 + A2^2 ) + ... ', $
	'',$
	'Total # of ROIs can be set for Lorentzian Fit defaults to 20', $
	'','ROI #     - Indicator for current ROI addressed', $
	'Left      -  Starting X value', $
	'Right     - Ending X value', $
	'Intensity - Peak intensity factor, A0', $
	'Centroid  -  X value where peak intensity located, A1', $
	'FWHM      -  Full width half maximum, (=2*A2)', $
	'left      -  Slider setting the starting X value', $
	'right     -  Slider setting the ending X value', $
	'Subplot   -  Option of plotting each Lorentzian sub-curve', $
	'ROIs      -  Display ROIs info in the text scroll window', $
	'Add       -  Add ROI with default intensity, centroid, FWHM values', $
	'Mod       -  Modifiy selected ROI intensity, centroid, and FWHM values', $
	'Del       -  Delete selected ROI # from the fit calc', $
	'Calc Fits -  Accept the ROIs and do curve fitting', $
	'Help      -  Pops up this help info window', $
	'Close     -  Close the multi Lorentzian fit dialog' $
	]

 	res = widget_message(str,/info,title='ROI Help Info')	
        END
  'ROI_EXIT': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
        END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END ROIFIT2
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO ez_fit2, N_ROI,X,Y, GROUP=Group

COMMON ROIFIT2_BLOCK,widget_ids

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  if n_elements(x) eq n_elements(y) then begin
	dim = n_elements(x)
	statistic_1d,X,Y,c_mass,x_peak,y_peak,y_hpeak,fwhm,fwhm_xl,fwhm_wd
	end

  ROIFIT2 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, TITLE='LORENTZIAN ROI Fitting', $
      UVALUE='ROIFIT2')

  BASE2 = WIDGET_BASE(ROIFIT2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL4 = WIDGET_LABEL( BASE2, $
      FONT='-misc-fixed-bold-r-normal--14-130-75-75-c-70-iso8859-1', $
      UVALUE='LABEL4', $
      VALUE='ROI')

  LABEL5 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL5', /ALIGN_LEFT, $
      VALUE='  ROI #    Left        Right         Intensity  Fix/Var   Centroid  Fix/Var     FWHM       Fix/Var')

  BASE7 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE7')

value='1'
if n_elements(N_ROI) then value=strtrim(indgen(N_ROI),2)
  roi_select = WIDGET_DROPLIST( BASE7, $
      UVALUE='ROI_SELECT', $
      VALUE=value)

  BASE7_2 = WIDGET_BASE(BASE7, $
      COLUMN=1, $
      MAP=1, FRAME=1, $
      UVALUE='BASE7_2')
  TextVal541 = [ $
    strtrim(x(0)) ]
  TEXT10 = WIDGET_TEXT( BASE7_2,VALUE=TextVal541, $
      XSIZE=10, $
      EDITABLE=1, $
      UVALUE='ROI_LEFT', $
      YSIZE=1)
  SLIDER2 = WIDGET_SLIDER( BASE7_2, $
      MAXIMUM=dim-1, $
      MINIMUM=0, $
      UVALUE='SLIDER2', $
      VALUE=0, $
      XSIZE=80)


  BASE7_3 = WIDGET_BASE(BASE7, $
      COLUMN=1, $
      MAP=1, FRAME=1, $
      UVALUE='BASE7_3')
  TextVal595 = [ $
    strtrim(x(dim-1)) ]
  TEXT11 = WIDGET_TEXT( BASE7_3,VALUE=TextVal595, $
      EDITABLE=1, $
      UVALUE='ROI_RIGHT', $
      XSIZE=10, $
      YSIZE=1)
  SLIDER3 = WIDGET_SLIDER( BASE7_3, $
      MAXIMUM=dim-1, $
      MINIMUM=0, $
      UVALUE='SLIDER3', $
      VALUE=dim-1, $
      XSIZE=80)


  BASE7_4 = WIDGET_BASE(BASE7, $
      ROW=1, $
      MAP=1, FRAME=1, $
      UVALUE='BASE7_4')
  TextVal757 = [ $
    strtrim(y_peak,2) ]
  TEXT14 = WIDGET_TEXT( BASE7_4,VALUE=TextVal757, $
      EDITABLE=1, $
      UVALUE='ROI_INTENSITY', $
      XSIZE=15, $
;      XSIZE=6, $
      YSIZE=1)

;  btn1=['','']
;  roi_intensity_btn = CW_BGROUP(BASE7_4, btn1, /EXCLUSIVE, $
;		/ROW,UVALUE='ROI_INTENSITY_Y')
;  WIDGET_CONTROL,roi_intensity_btn,SET_VALUE=1


  BASE7_5 = WIDGET_BASE(BASE7, $
;      ROW=1, $
      COLUMN=1, $
      MAP=1, FRAME=1, $
      UVALUE='BASE7_5')
  TextVal649 = [ $
    strtrim(x_peak,2) ]
  TEXT12 = WIDGET_TEXT( BASE7_5,VALUE=TextVal649, $
      EDITABLE=1, $
      UVALUE='ROI_CENTROID', $
      XSIZE=15, $
      YSIZE=1)
  SLIDER5 = WIDGET_SLIDER( BASE7_5, $
      MAXIMUM=dim-1, $
      MINIMUM=0, $
      UVALUE='SLIDER_CENTROID', $
;      VALUE=dim-1, $
      XSIZE=80)


;  btn1=['','']
;  roi_centroid_btn = CW_BGROUP(BASE7_5, btn1, /EXCLUSIVE, $
;		/ROW,UVALUE='ROI_CENTROID_Y')
;  WIDGET_CONTROL,roi_centroid_btn,SET_VALUE=1


  BASE7_6 = WIDGET_BASE(BASE7, $
      ROW=1, $
      MAP=1, FRAME=1, $
      UVALUE='BASE7_6')
  TextVal703 = [ $
    strtrim(fwhm,2) ]
  TEXT13 = WIDGET_TEXT( BASE7_6,VALUE=TextVal703, $
      EDITABLE=1, $
      UVALUE='ROI_FWHM', $
      XSIZE=15, $
      YSIZE=1)

;  btn1=['','']
;  roi_fwhm_btn = CW_BGROUP(BASE7_6, btn1, /EXCLUSIVE, $
;		/ROW,UVALUE='ROI_FWHM_Y')
;  WIDGET_CONTROL,roi_fwhm_btn,SET_VALUE=1

  BASE9 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE9')


  value=['N','Y']
  roi_subplot = WIDGET_DROPLIST( BASE9, $
      UVALUE='ROI_SUBPLOT', TITLE='Subplot', $
      VALUE=value)
  WIDGET_CONTROL,roi_subplot,SET_DROPLIST_SELECT=1

  roi_list = WIDGET_BUTTON(BASE9,VALUE='ROIs', $
                UVALUE='ROI_LIST')

  roi_add = WIDGET_BUTTON(BASE9,VALUE='Add', $
                UVALUE='ROI_ADD')

  roi_panel = WIDGET_BUTTON(BASE9,VALUE='Mod', $
                UVALUE='ROI_PANEL')

  roi_clear = WIDGET_BUTTON(BASE9,VALUE='Del', $
                UVALUE='ROI_CLEAR')

  roi_calc = WIDGET_BUTTON(BASE9,VALUE='Calc Fits', $
                UVALUE='ROI_CALC')

  roi_help = WIDGET_BUTTON(BASE9,VALUE='Help', $
                UVALUE='ROI_HELP')

  roi_exit = WIDGET_BUTTON(BASE9,VALUE='Close', $
                UVALUE='ROI_EXIT')

  roi_text = WIDGET_TEXT(BASE2, $
	VALUE='', $
	XSIZE=80, YSIZE=8, /SCROLL,UVALUE='ROI_TEXT')

  widget_ids = { $
        roi_select : roi_select, $
	left_fld: TEXT10, $
	right_fld: TEXT11, $
	centroid:  TEXT12, $
	fwhm:     TEXT13, $
	intensity: TEXT14, $
	text: roi_text, $
	dim : dim, $
	n_roi : n_roi, $
	roi : 0, $
	code : make_array(n_roi,/int), $   ; 1 - parameters defined for the ROI 
	xl_val : make_array(n_roi,value=x(0)), $	; roi xl value
	xr_val : make_array(n_roi,value=x(dim-1)), $	; roi xr value
	A0: make_array(n_roi), $  	; y-Peak
	A1: make_array(n_roi), $  	; x at y-Peak
	A2: make_array(n_roi), $  	; fwhm/2.
	subplot : 1, $      ; 0 - no subplot, 1 - subplot on
	xl : 0, $
	xr : dim-1, $
	X: x, $
	Y: y $
	}

; default starts with 1 lorentzian fit

  widget_ids.code(0) = 1
  widget_ids.A0(0) = y_peak
  widget_ids.A1(0) = x_peak
  widget_ids.A2(0) = fwhm/2.
  WIDGET_CONTROL, ROIFIT2, /REALIZE

  XMANAGER, 'ROIFIT2', ROIFIT2
END
;
; Auto Save File For ez_fit.pro
;
;  Mon Aug 25 13:23:31 CDT 1997
;

;@PS_open.pro
;@u_read.pro
;@xdisplayfile.pro
;@fitting.pro
;@regressfit.pro
;@svdfitgraf.pro
;@lorentzian.pro
;@ezfit_getvector.pro
;@fit_statistic.pro

PRO ezfit_get1DData,filename,y
COMMON EZ_FIT_BLOCK,ezfitData,image
if n_params() eq 0 then begin
	res=widget_message("Usage: ezfit_get1DData,filename,image",/info, $
		title='EZ_FIT Info')
	return
end
u_openr,unit,filename,/XDR 
u_read,unit,x
u_read,unit,y
u_close,unit
	ezfit_init1d,x,y
        return
END

PRO ezfit_init1d,x,y
COMMON EZ_FIT_BLOCK,ezfitData,image
        ezfitData.x = x
        ezfitData.im = y
        ezfitData.y = y(*,0)
        ezfitData.dim = 1
        ezfitData.pick = 0
        ezfitData.width = n_elements(x)
        sz=size(y)
        if sz(0) eq 1 then ezfitData.height=1 else ezfitData.height=sz(2)
        ezfitData.I_index=0
        ezfitData.J_index=0
        image=y
END

PRO ezfit_readImageData,x,y,im,filename=filename

file='fitting.bin'
if keyword_set(filename) then file=strtrim(filename,2)
u_openr,unit,file,/XDR
u_read,unit,x
u_read,unit,y
u_read,unit,im
u_close,unit

END 

PRO ezfit_get2DData,F,image2
COMMON EZ_FIT_BLOCK,ezfitData,image
		ezfit_readImageData,xarray,yarray,image,filename=F 
		ezfitData.im = image
		ezfit_init2D,xarray,yarray,image2
		ezfitData.file = F
END

PRO ezfit_init2D,xarray,yarray,image2
COMMON EZ_FIT_BLOCK,ezfitData,image
		ezfitData.x = xarray
		ezfitData.y = yarray
		ezfitData.width = n_elements(xarray) 
		ezfitData.height = n_elements(yarray) 
		ezfitData.dim = 2
image2=make_array(ezfitData.width,ezfitData.height)
image2(*,*)=ezfitData.im(0:ezfitData.width*ezfitData.height - 1)
END

PRO ezfit_picktype,x,y
COMMON EZ_FIT_BLOCK,ezfitData,image
	CASE ezfitData.pick OF 
	0: BEGIN
	x=ezfitData.x(0:ezfitData.width-1)
	y=ezfitData.y(0:ezfitData.width-1)
	END
	1: BEGIN
	x=ezfitData.x(0:ezfitData.width-1)
	y=ezfitData.zx(0:ezfitData.width-1)
	END
	2: BEGIN
	x=ezfitData.y(0:ezfitData.height-1)
	y=ezfitData.zy(0:ezfitData.height-1)
	END
	ENDCASE
END

PRO EZFIT_PDMENU3_Event, Event
COMMON EZ_FIT_BLOCK,ezfitData,image

r = strpos(Event.Value,'File')
if ezfitData.dim eq 0 and r eq -1 then begin
	str=['You have first to use the File->Open 1D or 2D ...', $
		'to load in the data array.']
	res=widget_message(str,/info,title='EZ_FIT Info')
	return
end

  CASE Event.Value OF 

  'File.Open 1D ...': BEGIN
	F=PICKFILE(/READ,FILTER='*.bin1d',GET_PATH=P,PATH=OP)
	if F eq '' then return
	ezfit_open1d,F,Event
    END
  'File.Open ASCII ...': BEGIN
	w_readascii,GROUP = Event.top
    END
  'File.Open 2D ...': BEGIN
	F=PICKFILE(/READ,FILTER='*.bin',GET_PATH=P,PATH=OP)
	if F eq '' then return
	ezfit_open2d,F,Event
    END
  'File.2D XDR ...': BEGIN
	F=PICKFILE(/READ,FILTER='*.bin.xdr',GET_PATH=P,PATH=OP)
	found = findfile(F)
	if found(0) ne '' then begin
		ezfit_get2DData,F,image2
		if XRegistered('GETVECTOR_MAIN13') then $
		WIDGET_CONTROL,ezfitData.base_getvector,/DESTROY
		ezfit_getvector,image2,GROUP=Event.Top
	endif else begin
		res=widget_message(F+ 'not found',/info,title='EZ_FIT Info', $
			DIALOG_PARENT=Event.id)
		return
	end
    END
  'File.Printer ...': BEGIN
	PS_printer,GROUP=Event.Top
    END
  'File.Quit': BEGIN
    WIDGET_CONTROL,Event.Top,/DESTROY
    END
  'GetData.VectorX': BEGIN
	if XRegistered('GETVECTOR_MAIN13') then begin
		o_win = !D.window
		WSET,ezfitData.image_area
		plot,ezfitData.x(0:ezfitData.width-1), $
		  xtitle='Index', ytitle='X', title='X vs Index'
		WSET,o_win
	end
	ezfitData.pick=0
    END
  'GetData.VectorY': BEGIN
	if XRegistered('GETVECTOR_MAIN13') then begin
		o_win = !D.window
		WSET,ezfitData.image_area
		if ezfitData.dim eq 1 then begin
			ezfitData.y = image(0:ezfitData.width-1, $ 
				ezfitData.J_index)
		end
		plot,ezfitData.x(0:ezfitData.width-1), ezfitData.y, $
		  xtitle='X', ytitle='Y', title='Y vs X'
		WSET,o_win
	end
	ezfitData.pick=0
    END
  'GetData.VectorZy': BEGIN
	if ezfitData.dim eq 2 then begin
		ezfit_get2DData,ezfitData.file,image
		ezfitData.zx = image(*,ezfitData.J_index)
	title='Z vs X @ Y='+strtrim(ezfitData.y(ezfitData.J_index),2)
	ezfitData.pick=1
        if XRegistered('POLYFITW_SETUP') then $
        WIDGET_CONTROL,ezfitData.polyfit_label, $
                SET_VALUE='Number of Elements: '+strtrim(ezfitData.width,2)
        if XRegistered('GETVECTOR_MAIN13') then begin
	o_win = !D.WINDOW
	WSET,ezfitData.image_area
	plot,ezfitData.x(0:ezfitData.width-1), ezfitData.zx, $
		xtitle='X', ytitle='Z', title=title 
	WSET,o_win
        str = '       I    X Values         Zy Values'
        for i=0,ezfitData.width-1 do $
        str = [str,string(i, ezfitData.x(i), ezfitData.zx(i),/print)]
        WIDGET_CONTROL, ezfitData.text_area, SET_VALUE=str
	end
	end
    END
  'GetData.VectorZx': BEGIN
	if ezfitData.dim eq 2 then begin
		ezfit_get2DData,ezfitData.file,image
		ezfitData.zy = image(ezfitData.I_index,*)
	title='Z vs Y @ X='+strtrim(ezfitData.x(ezfitData.I_index),2)
        if XRegistered('POLYFITW_SETUP') then $
        WIDGET_CONTROL,ezfitData.polyfit_label, $
                SET_VALUE='Number of Elements: '+strtrim(ezfitData.height,2)
	ezfitData.pick=2
        if XRegistered('GETVECTOR_MAIN13') then begin
	o_win = !D.WINDOW
	WSET,ezfitData.image_area
	plot,ezfitData.y(0:ezfitData.height-1), ezfitData.zy, $
		xtitle='Y', ytitle='Z', title=title 
	WSET,o_win
        str = '       I    Y Values         Zx Values'
        for i=0,ezfitData.height-1 do $
        str = [str,string(i, ezfitData.y(i), ezfitData.zy(i),/print)]
        WIDGET_CONTROL, ezfitData.text_area, SET_VALUE=str
        end
	end
    END
  'GetData.2DImage': BEGIN
	if XRegistered('GETVECTOR_MAIN13') then begin
		o_win = !D.WINDOW
		WSET,ezfitData.image_area
		TVSCL,image
		WSET,o_win
	endif else begin
		ezfit_getvector,image,GROUP=Event.Top
	end
    END
  'Curve Fit.COMFIT.EXPONENTIAL': BEGIN
 	ezfit_picktype,x,y	
	comfitGraf,x,y,/exponential,/print
    END
  'Curve Fit.COMFIT.GEOMETRIC': BEGIN
 	ezfit_picktype,x,y	
	comfitGraf,x,y,/GEOMETRIC,/print
    END
  'Curve Fit.COMFIT.GOMPERTZ': BEGIN
 	ezfit_picktype,x,y	
	comfitGraf,x,y,/GOMPERTZ,/print
    END
  'Curve Fit.COMFIT.HYPERBOLIC': BEGIN
 	ezfit_picktype,x,y	
	comfitGraf,x,y,/HYPERBOLIC,/print
    END
  'Curve Fit.COMFIT.LOGISTIC': BEGIN
 	ezfit_picktype,x,y	
	comfitGraf,x,y,/LOGISTIC,/print
    END
  'Curve Fit.COMFIT.LOGSQUARE': BEGIN
 	ezfit_picktype,x,y	
	comfitGraf,x,y,/LOGSQUARE,/print
    END
  'Curve Fit.CURVEFIT': BEGIN
	curvefit_setup,GROUP=Event.Top
; 	ezfit_picktype,x,y	
;	curvefitGraf,x,y,/print
    END
  'Curve Fit.GAUSSFIT': BEGIN
 	ezfit_picktype,x,y	
	gaussfitGraf,x,y,/print
    END
  'Curve Fit.LADFIT': BEGIN
 	ezfit_picktype,x,y	
	ladfitGraf,x,y,/print
    END
  'Curve Fit.LINFIT': BEGIN
 	ezfit_picktype,x,y	
	linfitGraf,x,y,/print
    END
  'Curve Fit.LORENTZIAN': BEGIN
 	ezfit_picktype,x,y	
	lorentzfitgraf,x,y,/print
    END
  'Curve Fit.POLYFITWFIT': BEGIN
	ezfitData.polyfit=1
	polyfitwsetup,GROUP=Event.Top
    END
  'Curve Fit.POLY_FIT': BEGIN
	ezfitData.polyfit=0
	polyfitwsetup,GROUP=Event.Top
    END
;  'Curve Fit.REGRESS': BEGIN
;	regressfitGraf,x,y,/print
;    END
  'Curve Fit.SVDFIT': BEGIN
 	ezfit_picktype,x,y	
	svdfitsetup,GROUP=Event.Top
    END
  'Multi Fit.REGRESS': BEGIN
	regressfitGraf,x,y,/print
    END
  'Multi Fit.LORENTZIAN': BEGIN
 	ezfit_picktype,x,y	
	ez_fit2,20,x,y,GROUP=Event.Top ; ROI 20
    END
  'Help.COMFIT': BEGIN
    comfitGraf
    END
  'Help.CURVEFIT': BEGIN
    curvefitGraf 
    END
  'Help.GAUSSFIT': BEGIN
    gaussfitGraf 
    END
  'Help.LADFIT': BEGIN
    ladfitGraf
    END
  'Help.LINFIT': BEGIN
    linfitGraf
    END
  'Help.LMFIT': BEGIN
    res=widget_message(' lmfitGraf not available yet.',title='FITTING Info',/info)
    END
  'Help.LORENTZIAN': BEGIN
    lorentzfitGraf
    END
  'Help.POLYFITW': BEGIN
    polyfitwGraf
    END
  'Help.POLY_FIT': BEGIN
    polyfitGraf
    END
  'Help.REGRESS': BEGIN
	regressfitGraf
    END
  'Help.SVDFIT': BEGIN
	svdfitGraf
    END
  'Help.COMMAND': BEGIN
	str=['Usage: ez_fit [[[,XARRAY=X ,YARRAY=Y] ,IM=image] , GROUP=group]', $
	'', $
	'KEYWORD:', $
	'   XARRAY, YARRAY  - only required if 1D data is entered directly form the', $
	'                     command line.', $
	'  IM               - in addition of XARRAY, YARRAY, IM is required if', $
	'                     2D image data is entered directly form the', $
	'                     command line.', $
	'  GROUP            - specifies the calling parent widget ID' $
	]
	res=widget_message(str,/info,title='EZ_FIT Help Info')
    END
  ENDCASE
END

PRO ezfit_open1d,F,Event
COMMON EZ_FIT_BLOCK,ezfitData,image

        if F eq '' then return
        found = findfile(F)
        if found(0) ne '' then begin
        ezfitData.dim = 1
        ezfitData.file = F
                ezfit_get1DData,F,image2
                if XRegistered('GETVECTOR_MAIN13') then $
                WIDGET_CONTROL,ezfitData.base_getvector,/DESTROY
                ezfit_getvector,image2,GROUP=Event.Top
        endif else begin
                res=widget_message(F+ 'not found',/info,title='EZ_FIT Info', $
                        DIALOG_PARENT=Event.id)
                return
        end

END

PRO ezfit_open2d,F,Event
COMMON EZ_FIT_BLOCK,ezfitData,image
        found = findfile(F)
        if found(0) ne '' then begin
                ezfit_get2DData,F,image2
                if XRegistered('GETVECTOR_MAIN13') then $
                WIDGET_CONTROL,ezfitData.base_getvector,/DESTROY
                ezfit_getvector,image2,GROUP=Event.Top
        endif else begin
                res=widget_message(F+ 'not found',/info,title='EZ_FIT Info', $
                        DIALOG_PARENT=Event.id)
                return
        end
END

PRO EZFIT_MAIN13_Event, Event


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  ; Event for EZFIT_PDMENU3
  'EZFIT_PDMENU3': EZFIT_PDMENU3_Event, Event

  'EZFIT_FIELD3': BEGIN
      WIDGET_CONTROL,Event.Id,GET_VALUE=file
	ezfit_open1d,file(0),Event
      END
  'EZFIT_FIELD4': BEGIN
      WIDGET_CONTROL,Event.Id,GET_VALUE=file
	ezfit_open2d,file(0),Event
      END
  'EZFIT_FIELD5': BEGIN
      WIDGET_CONTROL,Event.Id,GET_VALUE=file
	w_readascii,file(0),GROUP = Event.top
      END

  ENDCASE
END



PRO ez_fit,xarray=xarray,yarray=yarray,im=im, GROUP=Group
;+
; NAME:
;       EZ_FIT
;
; PURPOSE:
;       This routine integrates all the IDL line fitting routines into one 
;       single package. It provides the IDL user with a very easy to use
;       line fitting tool. It is a standalone widget application. It can
;       be easily plug into any other IDL program.
;
;       It accepts 1D or 2D arrays either from the input binary file or 
;       directly from the command line. It also accepts the spreadsheet
;       column type ascii input file. 
;
;       It allows the user to select any vector from the input data and
;       perform any fitting he/she desires. It lets the user easily 
;       get the hardcopy of fitting graph and tabulated data.
;
; CATEGORY:
;	Curve fitting Widgets application.
;
; CALLING SEQUENCE:
;
;       EZ_FIT
;	
; KEYWORD PARAMETERS:
;       XARRAY:	Specifies the independent varialbe  X vector if input to be
;               entered from the command line.
;
;       YARRAY:	Specifies the dependent varialbe Y array if input to be 
;               entered from the command line. It is used by 1D or 2D data.
;
;               For 2D image data, the YARRAY represents the Y vector of the
;               second dimension.
;
;               For 1D data, multiple data vectors can be packed into Y array. 
;               The Y(N,M), the N must have the same dimension as in X vector.
;               the M represents the number of dependent variables in Y array.
;               A user can use the cursor in the image area to select the 
;               dependent variable to be fitted, or use the J text field
;               to specify the dependent variable to be fitted..
;               
;       IM:     Specifies the 2D image array corresponding to XARRAY and 
;               YARRAY. IMAGE(N,M), where dimenstion N is the same as the 
;               number of elements in XARRAY, M is the same as the number 
;               of elements in YARRAY.
;
;       GROUP:  The widget ID of the group leader of the widget. If this
;               keyword is specified, the death of the group leader results 
;               in the death of EZ_FIT.
;
; OUTPUTS:
;       Pops up plot and list window to show fitting results with respect 
;       to the input data.
;
; COMMON BLOCKS:
;       COMMON EZ_FIT_BLOCK,ezfitData,image
;
; SIDE EFFECTS:
;       It uses the PLOT1D and XDISPLAYFILE program to show the fitting results.
;
; RESTRICTIONS:
;       Input binary file must be created by the U_OPERW and U_WRITE rountines.
;       The input data is in platform independent XDR binary format. 
;
;       Default input file filter for 1D data is '*.bin1d', the default 
;       input file filter for 2D data is '*.bin'. User may override this
;       default setting in the file selection dialog.
;
;       Input 1D file should contains only two binary objects XARRAY and 
;       YARRAY.  Input 2D file should contains only 3 binary objects XARRAY 
;       YARRAY and IMAGE.
;    
;       Input ASCII type file should contain columns of input data. It
;       uses the W_READASCII to read in columns of data. The first column
;       will be independent variable, the remaining columns will be dependent
;       variables.
;        
; EXAMPLE:
;       
;       Example 1 - Use the File->Open to load input data
;
;	       EZ_FIT
;
;       Example 2 - Use the X,Y keywords to load 1D data 
;
;              EZ_FIT,XARRAY=X, YARRAY=Y
;
;       Example 3 - Use the keywords to load 2D data 
;
;              EZ_FIT,XARRAY=X, YARRAY=Y, IM=image
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin K. Cha, 09-12-97.
;	02-27-98	Inherit color tables from the calling program
;-

COMMON EZ_FIT_BLOCK,ezfitData,image

if XRegistered('EZFIT_MAIN13') then $
	WIDGET_CONTROL,ezfitData.base,/DESTROY

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

ezfitData = { $
	file : '', $ ;	1D name: *.bin1d or   2D name: *.bin 
	base : 0L, $
	dim : 0, $		; 1 - 1d, 2 - 2d data
	base_getvector:0L, $
	image_area : 0, $       ; drawing are for getvector
	text_area : 0L, $
	I_field: 0L, $
	J_field: 0L, $
	I_index:0, $
	J_index:0, $
	pick:0, $ ; 1D: 0 - Y vs X , 2D: 1 - Zy vs X  , 2 - Zx vs Y 
	polyfit:0, $	; 0 for polyfit, 1 for polyfitw
	polyfit_label:0L, $
	polyfit_ndfield: 0L,$
	polyfit_wffield: 0L,$
	polyfit_ndegree: 4, $
	polyfit_factor: 1., $	; weighting factor
	svdfit_base: 0L, $	; svdfit base setup
	svdfit_fname: 'svdfunct', $	; user supplied fname
	svdfit_nterm: 4, $	; no of terms in curve fitting
	svdfit_factor: 1., $	; weighting factor
	svdfit_legendre: 0, $	; if 1 legendre fit is used
	curvefit_noderiv: 0, $
;	curvefit_itmax: 20, $
;	curvefit_tol:1.e-3, $
;	curvefit_fname: 'funct', $
;	curvefit_A: make_array(20), $
	x : make_array(4000), $
	y : make_array(4000), $
	zx: make_array(4000), $
	zy : make_array(4000), $
	im : make_array(1000,1000), $
	TV_width: 300, $
	TV_height: 300, $
	x_mag: 1., $
	y_mag: 1., $
	width : 0, $
	height : 0 $
	}

if keyword_set(im) then begin
	image = im
	ezfitData.im = im
	sz=size(im)
	if keyword_set(xarray) eq 0 then xarray = indgen(sz(1))
	if keyword_set(yarray) eq 0 then yarray = indgen(sz(2))
	ezfit_init2D,xarray,yarray,image2
endif else begin
	if keyword_set(yarray) then begin
	sz=size(yarray)
	if keyword_set(xarray) eq 0 then xarray = indgen(sz(1))
	ezfit_init1d,xarray,yarray
	end
end

  junk   = { CW_PDMENU_S, flags:0, name:'' }

os_init

  EZFIT_MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      /COLUMN, $
      TITLE='EZ_FIT (R1.0)', $
      MAP=1, $
      UVALUE='EZFIT_MAIN13')
  ezfitData.base = EZFIT_MAIN13

  BASE2 = WIDGET_BASE(EZFIT_MAIN13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(EZFIT_MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE3')

  MenuDesc831 = [ $
      { CW_PDMENU_S,       1, 'File' }, $ ;        0
        { CW_PDMENU_S,       0, 'Open 1D ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Open 2D ...' }, $ ;        2
        { CW_PDMENU_S,       0, 'Open ASCII ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Printer ...' }, $ ;        3
        { CW_PDMENU_S,       2, 'Quit' }, $ ;        4
      { CW_PDMENU_S,       1, 'GetData' }, $ ;        5
        { CW_PDMENU_S,       0, 'VectorX' }, $ ;        6
        { CW_PDMENU_S,       0, 'VectorY' }, $ ;        6
        { CW_PDMENU_S,       0, 'VectorZy' }, $ ;        7
        { CW_PDMENU_S,       0, 'VectorZx' }, $ ;        7
        { CW_PDMENU_S,       2, '2DImage' }, $ ;        8
      { CW_PDMENU_S,       1, 'Curve Fit' }, $ ;        9
        { CW_PDMENU_S,       1, 'COMFIT' }, $ ;       10
          { CW_PDMENU_S,       0, 'EXPONENTIAL' }, $ ;        2
          { CW_PDMENU_S,       0, 'GEOMETRIC' }, $ ;        3
          { CW_PDMENU_S,       0, 'GOMPERTZ' }, $ ;        4
          { CW_PDMENU_S,       0, 'HYPERBOLIC' }, $ ;        5
          { CW_PDMENU_S,       0, 'LOGISTIC' }, $ ;        6
          { CW_PDMENU_S,       2, 'LOGSQUARE'}, $  ;      7
        { CW_PDMENU_S,       0, 'CURVEFIT' }, $ ;       11
        { CW_PDMENU_S,       0, 'GAUSSFIT' }, $ ;       12
        { CW_PDMENU_S,       0, 'LADFIT' }, $ ;       13
        { CW_PDMENU_S,       0, 'LINFIT' }, $ ;       14
        { CW_PDMENU_S,       0, 'POLYFITWFIT' }, $ ;       15
        { CW_PDMENU_S,       0, 'POLY_FIT' }, $ ;       16
          { CW_PDMENU_S,       0, 'LORENTZIAN' }, $ ;        5
;        { CW_PDMENU_S,       0, 'REGRESS' }, $ ;       17
        { CW_PDMENU_S,       2, 'SVDFIT' }, $ ;       18
      { CW_PDMENU_S,       1, 'Multi Fit' }, $ ;        9
        { CW_PDMENU_S,       0, 'REGRESS' }, $ ;       17
          { CW_PDMENU_S,       2, 'LORENTZIAN' }, $ ;        5
      { CW_PDMENU_S,       3, 'Help' }, $ ;       19
        { CW_PDMENU_S,       0, 'COMMAND' }, $ ;       20
        { CW_PDMENU_S,       0, 'COMFIT' }, $ ;       20
        { CW_PDMENU_S,       0, 'CURVEFIT' }, $ ;       21
        { CW_PDMENU_S,       0, 'GAUSSFIT' }, $ ;       22
        { CW_PDMENU_S,       0, 'LADFIT' }, $ ;       23
        { CW_PDMENU_S,       0, 'LINFIT' }, $ ;       24
        { CW_PDMENU_S,       0, 'LMFIT' }, $ ;       25
        { CW_PDMENU_S,       0, 'POLYFITW' }, $ ;       26
        { CW_PDMENU_S,       0, 'POLY_FIT' }, $ ;       27
          { CW_PDMENU_S,       0, 'LORENTZIAN' }, $ ;        5
        { CW_PDMENU_S,       0, 'REGRESS' }, $ ;       28
        { CW_PDMENU_S,       2, 'SVDFIT' } $  ;     29

  ]


  EZFIT_PDMENU3 = CW_PDMENU( BASE2, MenuDesc831, /RETURN_FULL_NAME, $
      UVALUE='EZFIT_PDMENU3')

; 1D data : xarray, yarray
  FieldVal1367 = [ $
    'fitting.bin1d' ]
  EZFIT_FIELD3 = CW_FIELD( BASE3,VALUE=FieldVal1367, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='1D XDR File:', $
      XSIZE=60, $
      UVALUE='EZFIT_FIELD3')

; image data : xarray,yarray,image
  FieldVal1432 = [ $
    'fitting.bin' ]
  EZFIT_FIELD4 = CW_FIELD( BASE3,VALUE=FieldVal1432, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='2D XDR File:', $
      XSIZE=60, $
      UVALUE='EZFIT_FIELD4')

  FieldVal1433 = [ $
    '' ]
  EZFIT_FIELD5 = CW_FIELD( BASE3,VALUE=FieldVal1433, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='ASCII File:', $
      XSIZE=60, $
      UVALUE='EZFIT_FIELD5')

  WIDGET_CONTROL, EZFIT_MAIN13, /REALIZE

  XMANAGER, 'EZFIT_MAIN13', EZFIT_MAIN13
END

