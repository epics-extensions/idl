;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
@scan1d__define.pro
@scan2d__define.pro

PRO calibdrv_cleanup,info
	calib_v2 = info.calib_v2
	ret = obj_valid(calib_v2)
	if ret then calib_v2->delete
	calib_v2=0
;	ret = obj_valid()
;	print,ret
END

PRO calibdrv_init,info

	calibdrv_cleanup,info

if info.dim eq 1 then begin
	calib_v2 = obj_new('scan1d')
	calib_v2->open,info.filename
	calib_v2->scan_read_All,maxno
	info.maxno = maxno
	info.scanno_last = maxno
end
if info.dim eq 2 then begin
	calib_v2 = obj_new('scan2d')
	calib_v2->open,info.filename
	calib_v2->readAll,maxno,scanno_last
	info.maxno = maxno
	info.scanno_last = scanno_last
end

	WIDGET_CONTROL,info.scanno_id,SET_VALUE=1
	WIDGET_CONTROL,info.slider_id,SET_SLIDER_MAX=info.scanno_last
	info.scanno = 1

	info.calib_v2 = calib_v2

	if info.dim eq 2 then $
	info.calib_v2->panImage,info.scanno else $
	info.calib_v2->plot,info.scanno
END


PRO PDMENU4_Event,info, Event


  CASE Event.Value OF 


  'File.Open...': BEGIN
     F = dialog_pickfile(filter='*.image',GET_PATH=p,GROUP=Event.Top,/MUST_EXIST,$
		PATH=info.inpath,TITLE='Select Image File',/READ)
	if F eq '' then return
	info.inpath = p
	info.filename = F
	info.classname = strmid(f,strlen(p),strlen(f)-strlen(p))
	WIDGET_CONTROL,info.filename_id,SET_VALUE=F

	if strpos(info.filename,'.image') gt 0 then info.dim=2 else info.dim=1
	calibdrv_init,info
	WIDGET_CONTROL,info.base,SET_UVALUE=info
    END
  'File.Quit': BEGIN
    WIDGET_CONTROL,Event.top,/DESTROY
    calibdrv_cleanup,info
    END
  'Color...': BEGIN
    xloadct,GROUP=Event.top
    END
  'Help...': BEGIN
    PRINT, 'Event for Help...'
    END
  ENDCASE
END





PRO CALIBDRV_Event, Event

  WIDGET_CONTROL, Event.top, GET_UVALUE=info

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  ; Event for PDMENU4
  'PDMENU4': BEGIN
	PDMENU4_Event,info, Event
	return
     END
  'CALIBDRV_FILENAME': BEGIN
	WIDGET_CONTROL,info.filename_id,GET_VALUE=fn
	F = fn(0)
	info.filename=f
	p=''
	p1 = rstrpos(f,!os.file_sep)
	if p1 ge 0 then p = strmid(f,0,p1+1)
	info.inpath = p
	info.filename = F
	info.classname = strmid(f,strlen(p),strlen(f)-strlen(p))
	if strpos(info.filename,'.image') gt 0 then info.dim=2 else info.dim=1
	calibdrv_init,info
      END
  'CALIBDRV_SEQNO': BEGIN
	WIDGET_CONTROL,info.scanno_id,GET_VALUE=f
	info.scanno=f(0)
	if info.dim eq 2 then $
	info.calib_v2->panImage,info.scanno else $
	info.calib_v2->plot,info.scanno
      END
  'CALIBDRV_FIRST': BEGIN
	WIDGET_CONTROL,info.scanno_id,SET_VALUE=1
	info.scanno=1
	if info.dim eq 2 then $
	info.calib_v2->panImage,info.scanno else $
	info.calib_v2->plot,info.scanno
      END
  'CALIBDRV_NEXT': BEGIN
	if info.scanno lt info.scanno_last then begin
		info.scanno = info.scanno+1
		WIDGET_CONTROL,info.scanno_id,SET_VALUE=info.scanno
	if info.dim eq 2 then $
	info.calib_v2->panImage,info.scanno else $
	info.calib_v2->plot,info.scanno
	endif else res = dialog_message('End of file',/Info)
      END
  'CALIBDRV_PREV': BEGIN
	if info.scanno gt 1 then begin
		info.scanno = info.scanno-1
		WIDGET_CONTROL,info.scanno_id,SET_VALUE=info.scanno
	if info.dim eq 2 then $
	info.calib_v2->panImage,info.scanno else $
	info.calib_v2->plot,info.scanno
	end
      END
  'CALIBDRV_LAST': BEGIN
		info.scanno = info.scanno_last
		WIDGET_CONTROL,info.scanno_id,SET_VALUE=info.scanno
	if info.dim eq 2 then $
	info.calib_v2->panImage,info.scanno else $
	info.calib_v2->plot,info.scanno
      END
  'CALIBDRV_SLIDER': BEGIN
	WIDGET_CONTROL,info.slider_id,GET_VALUE=no
	info.scanno = no
	WIDGET_CONTROL,info.scanno_id,SET_VALUE=no
	if info.dim eq 2 then $
	info.calib_v2->panImage,info.scanno else $
	info.calib_v2->plot,info.scanno
      END
  'CALIBDRV_RUN': BEGIN
	scanno = info.scanno
	title = ':  Scan # '+strtrim(scanno,2)
	info.calib_v2->calibration,scanno,GROUP=Event.top
      END
  ENDCASE


  WIDGET_CONTROL, Event.top, SET_UVALUE=info

END




PRO calibdrv, GROUP=Group
device,retain=2,decomposed=0
loadct,39

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  CALIBDRV = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      UVALUE='CALIBDRV')

  BASE2 = WIDGET_BASE(CALIBDRV, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  MenuDesc695 = [ $
      { CW_PDMENU_S,       1, 'File' }, $ ;        0
        { CW_PDMENU_S,       0, 'Open...' }, $ ;        1
        { CW_PDMENU_S,       2, 'Quit' }, $ ;        2
      { CW_PDMENU_S,       0, 'Color...' }, $ ;        3
      { CW_PDMENU_S,       2, 'Help...' } $  ;      4

  ]


  PDMENU4 = CW_PDMENU( BASE3, MenuDesc695, /RETURN_FULL_NAME, $
      UVALUE='PDMENU4')


  FieldVal746 = [ $
    '' ]
  FIELD6 = CW_FIELD( BASE2,VALUE=FieldVal746, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Catcher Image File:', $
      UVALUE='CALIBDRV_FILENAME', $
      XSIZE=60)

  BASE9 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE9')

  FieldVal1008 = [ $
    '' ]
  FIELD10 = CW_FIELD( BASE9,VALUE=FieldVal1008, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Scan 2D #:', $
      UVALUE='CALIBDRV_SEQNO', $
      XSIZE=4)

  BUTTON11 = WIDGET_BUTTON( BASE9, $
      UVALUE='CALIBDRV_FIRST', $
      VALUE='First')

  BUTTON12 = WIDGET_BUTTON( BASE9, $
      UVALUE='CALIBDRV_NEXT', $
      VALUE='Next')

  BUTTON13 = WIDGET_BUTTON( BASE9, $
      UVALUE='CALIBDRV_PREV', $
      VALUE='Prev')

  BUTTON14 = WIDGET_BUTTON( BASE9, $
      UVALUE='CALIBDRV_LAST', $
      VALUE='Last')

  SLIDER14 = WIDGET_SLIDER( BASE9, $
      MAXIMUM=2, $
      MINIMUM=1, $
      UVALUE='CALIBDRV_SLIDER', $
      VALUE=1)

  BUTTON15 = WIDGET_BUTTON( BASE9, $
      UVALUE='CALIBDRV_RUN', $
      VALUE='Run Calibra...')

info = {  base:CALIBDRV, $
	filename_id:FIELD6, $
	scanno_id:FIELD10, $
	slider_id:SLIDER14, $
	filename:'', $
	inpath:'', $
	classname:'', $
	maxno:0, $
	scanno_last:0, $
	scanno:0, $
	dim: 2, $
	calib_v2: obj_new('scan2d') $
	}
	

  WIDGET_CONTROL, CALIBDRV, /REALIZE
  WIDGET_CONTROL, CALIBDRV, SET_UVALUE=info

  XMANAGER, 'CALIBDRV', CALIBDRV
END
