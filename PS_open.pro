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
	!os.printer = printer_info.name
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
