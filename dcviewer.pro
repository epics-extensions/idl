;
; data catcher viewer
;

; @view1d.pro
; @view2d.pro


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

  'BUTTON3': BEGIN
      Print, 'Event for VIEW1D ...'
	found = findfile(dcviewer_ids.data)
	if found(0) ne '' then begin
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
  'BUTTON6': BEGIN
      Print, 'Event for Quit'
	WIDGET_CONTROL,Event.Top,/DESTROY	
      END
  ENDCASE
END



PRO dcViewer,data=data,file=file, XDR=XDR, GROUP=Group
COMMON DCVIEWER_BLOCK,dcviewer_ids

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
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  BUTTON3 = WIDGET_BUTTON( BASE2, $
      UVALUE='BUTTON3', $
      VALUE='VIEW1D ...')

  BUTTON4 = WIDGET_BUTTON( BASE2, $
      UVALUE='BUTTON4', $
      VALUE='VIEW2D ...')

 if keyword_set(XDR) then begin
  BUTTON5 = WIDGET_BUTTON( BASE2, $
      UVALUE='BUTTON5', $
      VALUE='BI2XDR ...')
  dcviewer_ids.xdr = 1

end

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
