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
	'Modify -  Highlight the old detector name', $
	'          then type in user desired new name', $
	'Accept -  Press Accept button to accept all changes.','' $
	]
	r = dialog_message(str,/Info,title='Help on Assign Names')
	return
      END
  'ASSIGN_D1TEXT': BEGIN
	res = dialog_message('Error found in D1:DF',/error) 
	return
      END
  'ASSIGN_D01TEXT': BEGIN
	res = dialog_message('Error found in D01:D20',/error) 
	return
      END
  'ASSIGN_D21TEXT': BEGIN
	res = dialog_message('Error found in D21:D40',/error) 
	return
      END
  'ASSIGN_D41TEXT': BEGIN
	res = dialog_message('Error found in D41:D60',/error) 
	return
      END
  'ASSIGN_D61TEXT': BEGIN
	res = dialog_message('Error found in D61:D70',/error) 
	return
      END
  'ASSIGN_ACCEPT': BEGIN
	WIDGET_CONTROL,assignName_state.textD1Wid,GET_VALUE=d1
	WIDGET_CONTROL,assignName_state.textD01Wid,GET_VALUE=d2
	WIDGET_CONTROL,assignName_state.textD21Wid,GET_VALUE=d3
	WIDGET_CONTROL,assignName_state.textD41Wid,GET_VALUE=d4
	WIDGET_CONTROL,assignName_state.textD61Wid,GET_VALUE=d5
	list = [d1,d2,d3,d4,d5]
	assignName_state.dnames = list
  	WIDGET_CONTROL, Event.Top, SET_UVALUE=assignName_state
	assignName_write,list
	WIDGET_CONTROL,Event.top,/DESTROY
      END
  ENDCASE
END


PRO assignName_init,assignName_state
  d1 = [ 'D1','D2','D3','D4','D5','D6','D7','D8','D9','DA','DB', $
	'DC','DD','DE','DF']
  d2 = [ 'D01','D02','D03','D04','D05','D06','D07','D08','D09', $
	'D'+strtrim(indgen(11)+10,2)]
  d3 = [ 'D'+strtrim(indgen(20)+21,2)]
  d4 = [ 'D'+strtrim(indgen(20)+41,2)]
  d5 = [ 'D'+strtrim(indgen(10)+61,2)]
  dnames =[d1,d2,d3,d4,d5]

  found = findfile('.tmpName',count=ct)
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
	ndet : n_elements(dnames), $
	dnames : dnames $
	}

END


PRO assign_detName, part=part, GROUP=Group
; default is 85 detectors
; if part is specified 15 detectors assumed

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

;  junk   = { CW_PDMENU_S, flags:0, name:'' }

  assignName_init,assignName_state

  ASSIGNDNAME = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, title='RE-ASSIGN DETECTOR NAME', $
      MAP=1, $
      UVALUE='ASSIGNDNAME')

  BASE2 = WIDGET_BASE(ASSIGNDNAME, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_BUTTON( BASE2, $
      UVALUE='ASSIGN_HELP', $
      VALUE='Assign Detector Names...')

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
      XSIZE=6,YSIZE=15)

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
      XSIZE=6,YSIZE=20)

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
      XSIZE=6,YSIZE=20)

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
      XSIZE=6,YSIZE=20)

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
      XSIZE=6,YSIZE=10)

  BUTTON15 = WIDGET_BUTTON( BASE2, $
      UVALUE='ASSIGN_ACCEPT', $
      VALUE='Accept')

  	assignName_state.textD1Wid = TEXT5
	assignName_state.textD01Wid = TEXT6
	assignName_state.textD21Wid = TEXT7
	assignName_state.textD41Wid = TEXT8
	assignName_state.textD61Wid = TEXT9

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


PRO assignName_write,dnames
	xdr_open,unit,/write,'.tmpName'
	xdr_write,unit,dnames
	xdr_close,unit
END

PRO assignName_read,dnames,GROUP=group
; use intermediate filename '.tmpName' for saving dnames
	part = 0
	if n_elements(dnames) eq 15 then part=15
	assign_detName,GROUP=group,part=part
	found = findfile('.tmpName',count=ct)
	if ct then begin
	xdr_open,unit,'.tmpName'
	xdr_read,unit,dnames
	xdr_close,unit
	end
END

