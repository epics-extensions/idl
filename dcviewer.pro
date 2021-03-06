;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
;
; data catcher viewer
;

; @view1d.pro
; @view2d.pro

PRO COMMANDTEXT_Event, Event


  WIDGET_CONTROL,Event.Top,GET_UVALUE=info
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'COMMANDTEXT_FIELD': BEGIN
	WIDGET_CONTROL,info.command_field,GET_VALUE=name
	if strtrim(name(0),2) ne '' then begin
	x = string(name,/print)
; 	r = execute(x)	
	call_procedure,x
	if !err lt 0 then begin
		res = WIDGET_MESSAGE(!err_string,/Error,dialog_parent=Event.top)
		end
	end
      END
  'COMMANDTEXT_BUTTON6': BEGIN
	WIDGET_CONTROL,info.base,/DESTROY
	return
      END
  ENDCASE
END

;
;
PRO commandtext_dialog, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  COMMANDTEXT = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      UVALUE='COMMANDTEXT')

  BASE2 = WIDGET_BASE(COMMANDTEXT, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  COMMANDTEXT_FIELD = CW_FIELD( BASE2,VALUE='', $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='IDL> ', $
      UVALUE='COMMANDTEXT_FIELD', $
      XSIZE=60)

  BASE4 = WIDGET_BASE(BASE2, $
      COLUMN=2, $
      MAP=1, $
      UVALUE='BASE4')

  COMMANDTEXT_BUTTON6 = WIDGET_BUTTON( BASE4, $
      UVALUE='COMMANDTEXT_BUTTON6', $
      VALUE='Close')

  info = {  $
	base : COMMANDTEXT, $
	command_field : COMMANDTEXT_FIELD $
	}

  WIDGET_CONTROL, COMMANDTEXT, SET_UVALUE=info
  WIDGET_CONTROL, COMMANDTEXT, /REALIZE

  XMANAGER, 'COMMANDTEXT', COMMANDTEXT
END

PRO BI2XDR_Event, Event
COMMON BI2XDR_BLOCK, bi2xdr_ids

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 
  'BI2XDR_FILE': begin
	WIDGET_CONTROL,Event.id,GET_VALUE=filename
	bi2xdr_convertData,filename(0)
	end
  'BI2XDR_ACCEPT': begin
	WIDGET_CONTROL,bi2xdr_ids.filename,GET_VALUE=filename
	bi2xdr_convertData,filename(0)
	end
  'BI2XDR_CANCEL': begin
	WIDGET_CONTROL,Event.top,/DESTROY
	end

  ENDCASE
END

PRO bi2xdr_convertData,file
	found = findfile(file)
	if found(0) eq '' then begin
		res=WIDGET_MESSAGE('Error: file not found')
	endif else  u_bi2xdr,file
END


PRO bi2xdr_converter, file=file, GROUP=Group
;+
; NAME:
;       BI2XDR_CONVERTER
;
; PURPOSE:
;       This IDL program converts native binary data into platform-
;       independent XDR binary data. 
;
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;       BI2XDR_CONVERTER [,file=file]  [,GROUP=Group]
;
; INPUTS:
;       None.
;
; KEYWORD PARAMETERS:
;       file:   Specifies the input data file name. The input file should 
;               contain pure binary data objects.
;       GROUP:  The widget ID of the group leader of the widget. If this 
;               keyword is specified, the death of the group leader 
;               results in the death of BI2XDR_CONVERTER.
;
; OUTPUTS:
;       The output filename uses the input filename suffixed with '.xdr'.
;       Output file contains the converted XDR binary data objects.
;
; COMMON BLOCKS:
;       COMMON BI2XDR_BLOCK
;
; RESTRICTIONS:
;       The input data file should contain pure native binary data objects. 
;       The 'os.init' and 'dcviewer.pro' must be loaded into IDL first.
;
; EXAMPLE:
;
;       @os.init
;       .RUN dcviewer
;       BI2XDR_CONVERTER
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 06-01-97.
;
;       xx-xx-xx      iii  comment     
;-
;
COMMON BI2XDR_BLOCK, bi2xdr_ids


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  BI2XDR = WIDGET_BASE(GROUP_LEADER=Group, $
      TITLE='BI2XDR', $
      ROW=1, $
      MAP=1, $
      UVALUE='BI2XDR')

  BASE2 = WIDGET_BASE(BI2XDR, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL4 = WIDGET_LABEL(BASE2, $
      FONT=!os.font, $
      UVALUE='LABEL4', $
      VALUE='BI2XDR Data Converter')

  FieldVal575 = [ $
    'catch1d.trashcan' ]

  if keyword_set(file) then FieldVal575=strtrim(file,2)

  BI2XDR_FILE = CW_FIELD( BASE2,VALUE=FieldVal575, $
      ROW=1, XSIZE=70, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Filename:', $
      UVALUE='BI2XDR_FILE')

  BASE4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  BI2XDR_ACCEPT = WIDGET_BUTTON( BASE4, $
      UVALUE='BI2XDR_ACCEPT', $
      VALUE='Accept')

  BI2XDR_CANCEL = WIDGET_BUTTON( BASE4, $
      UVALUE='BI2XDR_CANCEL', $
      VALUE='Cancel')

  bi2xdr_ids = { $
	filename : BI2XDR_FILE $
	}

  WIDGET_CONTROL, BI2XDR, /REALIZE

  XMANAGER, 'BI2XDR', BI2XDR
END


PRO dcViewer_event, Event
COMMON DCVIEWER_BLOCK,dcviewer_ids

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'PROMPT_IDL': begin
	commandtext_dialog,GROUP=Event.top
	end
  '1D_OVERLAY ...': BEGIN
	view1d_overlay,GROUP=Event.Top 
      END
  'BUTTON2': BEGIN
	dnames = strtrim(indgen(15)+1,2)
	assignname_read,dnames
      END
  'BUTTON3': BEGIN
      Print, 'Event for VIEW1D ...'
	found = findfile(dcviewer_ids.data)
	if found(0) ne '' then begin
		if !d.name eq 'WIN' then begin
		view1d,data=dcviewer_ids.data,/XDR,GROUP=Event.Top
		return
		end
		if dcviewer_ids.xdr eq 1 and dcviewer_ids.datatype eq 0 then $
		view1d,data=dcviewer_ids.data,/XDR,GROUP=Event.Top else $
		view1d,data=dcviewer_ids.data,GROUP=Event.Top
	endif else view1d,GROUP=Event.Top 
      END
  'BUTTON4': BEGIN
      Print, 'Event for VIEW2D ...'
	found = findfile(dcviewer_ids.file)
	if found(0) ne '' then begin
		if dcviewer_ids.xdr eq 1 and dcviewer_ids.filetype eq 0 then $
		view2d,file=dcviewer_ids.file,/XDR,GROUP=Event.Top else $
		view2d,file=dcviewer_ids.file,GROUP=Event.Top
	endif else view2d,GROUP=Event.Top
      END
  'BUTTON5': BEGIN
      Print, 'Event for BI2XDR ...'
	bi2xdr_converter,GROUP=Event.Top
      END
  'BUTTON7': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY	
      END
  'BUTTON6': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY	
	exit
      END
  ENDCASE
END



PRO dcViewer,data=data,file=file, XDR=XDR, GROUP=Group
;+
; NAME:
;	DCVIEWER
;
; PURPOSE:
;       This program integrates the view1D and view2D program into a single 
;       system. It provides 1D and 2D data viewing features for data
;       catcher. It operates on the same set of data files generated by 
;       the data catcher. 
; 
;       It is written in pure IDL language and it is platform-independent.
;       It can read data either in native binary form or XDR binary form. 
;  
;       It can be invoked as an XDR data converter to convert native data
;       into XDR binary form.
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;
;       DCVIEWER [,DATA='1data'] [,FILE='1data.image'] [,/XDR] [,GROUP=Group]
;
; INPUTS:
;       None.
;	
; KEYWORD PARAMETERS:
;       DATA:   Specifies the input filename for the 1D scan data on the 
;               command line. 
;       FILE:   Specifies the input filename for the 2D image data on the 
;               command line. 
;       XDR:    Starts the DCVIEWER as the XDR data convert program.
;       GROUP:  The widget ID of the group leader of the widget. If this 
;               keyword is specified, the death of the group leader results in
;               the death of DCVIEWER.
;
; OUTPUTS:
;       It provides all the viewing and report features available in the 
;       EPICS data catcher except with the channel access functions been
;       removed. 
;
; COMMON BLOCKS:
;       COMMON DCVIEWER_BLOCK
;       COMMON BI2XDR_BLOCK
;
; SIDE EFFECTS:
;       If the data catcher has appended new scan data on the same file,
;       a user has to reload the data file to get the newly added scan 
;       data into the DCVIEWER.
;
; RESTRICTIONS:
;       A complete package of DCVIEWER includes the following files:
;          os.init
;          dcviewer.pro
;          view1d.init
;          view1d.pro
;          view1d_overlay.pro
;          view2d.init
;          view2d.pro
;          plot1d.pro
;       The path to these files must be included in the IDL_PATH. Before
;       invoking DCVIEWER, the file 'os.init' must be loaded into IDL
;       first. 
;
; PROCEDURE:
;       On the UNIX operating system, the script file 'viewer' can be 
;       used to directly invoke the data viewer. The environment, path 
;       settings, and running procedure are automatically taking cared 
;       by the script file 'viewer'.
;
; EXAMPLE:
;       Start the DCVIEWER as scan data display package -
;               @os.init
;               dcviewer
;
;       Start the DCVIEWER as an XDR data converter -
;               @os.init
;               dcviewer,/XDR
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 6-01-97.
;       12-19-97 bkc  - Allows the access of the view1d_overlay program
;                       Add the IDL> prompt dialog which let the user
;                       run any IDL command
;       09-06-02 bkc  - The Close button will close viewer program but stay in
;                       IDL sesseion with all the routines intact
;			The Exit button will exit the IDL session 
;-

COMMON DCVIEWER_BLOCK,dcviewer_ids
COMMON COLORS, R_ORIG, G_ORIG, B_ORIG, R_CURR, G_CURR, B_CURR

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  dcviewer_ids = { $
	data	: '', $
	file 	: '', $
	datatype : 0, $
	filetype : 0, $
	xdr 	: 0 $
	}

  dcViewer = WIDGET_BASE(GROUP_LEADER=Group, $
      TITLE='Data Viewer', $
      COL=1, $
      MAP=1, $
      UVALUE='dcViewer')

  BASE1 = WIDGET_BASE(dcViewer, $
      COL=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL4 = WIDGET_LABEL(BASE1, $
      FONT = !os.font, $
      UVALUE='LABEL4', $
      VALUE='Catcher 1D/2D Data Viewer')

  if keyword_set(data) then begin
     LABEL5 = WIDGET_LABEL(BASE1, $
      	UVALUE='LABEL5', /ALIGN_LEFT, $
      	VALUE='1D DATA  :     '+data)
     dcviewer_ids.data = strtrim(data,2)
  end

  if keyword_set(file) then begin
     LABEL6 = WIDGET_LABEL(BASE1, $
      	UVALUE='LABEL6', /ALIGN_LEFT, $
      	VALUE='2D IMAGE :     '+file)
     dcviewer_ids.file = strtrim(file,2)
  end

  BASE2 = WIDGET_BASE(dcViewer, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BUTTON3 = WIDGET_BUTTON( BASE2, $
      UVALUE='BUTTON3', $
      VALUE='VIEW1D ...')

  BUTTON1 = WIDGET_BUTTON( BASE2, $
      UVALUE='1D_OVERLAY ...',$
      VALUE='1D_OVERLAY ...')

  BUTTON2 = WIDGET_BUTTON( BASE2, $
      UVALUE='BUTTON2', $
      VALUE='Assign Di Names for view2d ...')

  BUTTON4 = WIDGET_BUTTON( BASE2, $
      UVALUE='BUTTON4', $
      VALUE='VIEW2D ...')

  PROMPT_IDL = WIDGET_BUTTON( BASE2, $
      UVALUE='PROMPT_IDL', $
      VALUE='IDL Prompt ...')

 if keyword_set(XDR) then begin
  BUTTON5 = WIDGET_BUTTON( BASE2, $
      UVALUE='BUTTON5', $
      VALUE='BI2XDR ...')
  dcviewer_ids.xdr = 1

end

  BUTTON7 = WIDGET_BUTTON( BASE2, $
      UVALUE='BUTTON7', $
      VALUE='   Close  ')

  BUTTON6 = WIDGET_BUTTON( BASE2, $
      UVALUE='BUTTON6', $
      VALUE='   Exit   ')


  WIDGET_CONTROL, dcViewer, /REALIZE

if dcviewer_ids.xdr eq 1 then begin
  if dcviewer_ids.data ne '' then begin
	parts = str_sep(dcviewer_ids.data,'.')
	id1 = n_elements(parts) - 1
	if strupcase(parts(id1)) ne 'XDR' then begin
	dcviewer_ids.datatype = -1
	res = WIDGET_MESSAGE('Error: XDR type of 1D data required !')
	end
  end

  if dcviewer_ids.file ne '' then begin
	parts = str_sep(dcviewer_ids.file,'.')
	id2 = n_elements(parts) - 1
	if strupcase(parts(id2)) ne 'XDR' then begin
	dcviewer_ids.filetype = -1
	res = WIDGET_MESSAGE('Error: XDR  type of 2D image data required !')
	end
  end

 if dcviewer_ids.file eq '' and  dcviewer_ids.data eq '' then begin
	WIDGET_CONTROL,BUTTON3,SENSITIVE=0
	WIDGET_CONTROL,BUTTON4,SENSITIVE=0
 end
end

  XMANAGER, 'dcViewer', dcViewer
END
