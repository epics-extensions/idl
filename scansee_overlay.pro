@scanSee__define.pro

PRO DC_view_writeConfig,state
        openw,unit,'scanSee.config',/get_lun
        printf,unit,state.filename
        printf,unit,state.path
        printf,unit,''
        free_lun,unit
END

PRO DC_view_readConfig,filename,path
        filename=''
        path=''
        openr,unit,'scanSee.config',/get_lun
        readf,unit,filename
        readf,unit,path
        free_lun,unit

END

PRO SCANSEEOVL_init,filename,overlay_state,print=print
	ret = obj_valid(overlay_state.v)
	if ret then obj_destroy,overlay_state.v

	overlay_state.maxlineno = 1
	v1 = obj_new('scanSee',file=filename)
	overlay_state.v = v1

	v1->read,dim=dim,num_pts=num_pts,cpt=cpt
	if dim gt 1 then overlay_state.maxlineno = cpt(1)
	WIDGET_CONTROL,overlay_state.lineWid, $
		SET_LIST_SELECT=overlay_state.lineno, $
		SET_VALUE='line # '+strtrim(indgen(overlay_state.maxlineno)+1,2)
END

PRO SCANSEEOVL_getlist,overlay_state

	overlay_state.scanlist = 0
	WIDGET_CONTROL,overlay_state.listfld,GET_VALUE=list
	overlay_state.list = list
	lists = strsplit(list(0),',',/extract)
	overlay_state.nolist = n_elements(lists)
	overlay_state.scanlist = fix(lists)
END


PRO SCANSEEOVL_Event, Event

  WIDGET_CONTROL,Event.top,GET_UVALUE=overlay_state
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'SCANSEE1D_OVDONE': BEGIN
	DC_view_writeConfig,overlay_state
	WIDGET_CONTROL,Event.Top,/DESTROY
      END
  'SCANSEE1D_OVACCEPT': BEGIN
	SCANSEEOVL_getlist,overlay_state
	SCANSEEOVL_init,overlay_state.filename,overlay_state,/print
	v1 = overlay_state.v
	list = overlay_state.scanlist(0:overlay_state.nolist-1)
	detno = overlay_state.detno+1
	lineno = overlay_state.lineno+1
	title =  'SCAN:'+overlay_state.list+ $
		'(Detector:'+overlay_state.dname(overlay_state.detno) + $
		', Line # '+strtrim(lineno,2)+')'
	v1->overlay,list,/plot,det_sel=detno,pick1d=lineno,title=title, $
		group=Event.top
;	return
      END
  'SCANSEE1D_OVFILE': BEGIN
	filename = dialog_pickfile(FILTER='*.sca*',GET_PATH=path,PATH=overlay_state.path,/must_exist)
	if filename eq '' then return
	overlay_state.path = path(0)
	overlay_state.filename = filename
	WIDGET_CONTROL,overlay_state.filefld,SET_VALUE=filename
	WIDGET_CONTROL,overlay_state.base2,SENSITIVE=1
	SCANSEEOVL_init,filename,overlay_state   ;,/print
      END
  'SCANSEE1D_OVNAME': BEGIN
	WIDGET_CONTROL,Event.Id,GET_VALUE=fname
	overlay_state.filename = fname(0)
      Print, fname(0)
      END
  'SCANSEE1D_OVHELP': BEGIN
	str=['The input scan files must be automatically generated by IOC.', $
	'This program overlay 1D plot from a list of scan # with user', $
	'specified detector # and 1D scan line #.', $
		'','User interface is initiated by selecting the File... button.', $
		'', 'The Scan # List entered must be separated by a comma.',$
		'This program overlay the 1D plot from each scan from the', $
		'entered list.']
	res = dialog_message(str,title='ScanSee Overlay Info',/info)
	END
  'SCANSEE1D_OVLIST': BEGIN
	SCANSEEOVL_getlist,overlay_state
      END
  'SCANSEE1D_OVXPICK': BEGIN
	res = WIDGET_INFO(Event.Id,/droplist_select)
	overlay_state.xaxis = res
	END
  'SCANSEE1D_OVDETOR': BEGIN
	res = WIDGET_INFO(Event.Id,/LIST_SELECT)
	overlay_state.detno = res
      Print, 'Event for detector'
      END
  'SCANSEE1D_OVLINEN': BEGIN
	res = WIDGET_INFO(Event.Id,/LIST_SELECT)
	overlay_state.lineno = res
      Print, 'Event for detector'
      END
  ENDCASE

	WIDGET_CONTROL,overlay_state.base,SET_UVALUE=overlay_state,BAD_ID=bad
	
END


; DO NOT REMOVE THIS COMMENT: END SCANSEEOVL
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO scanSee_overlay, MAXLINENO=MAXLINENO, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  li = ['D'+strtrim(indgen(9)+1,2), 'DA','DB','DC','DD','DE','DF']
  li = [li,'D0'+strtrim(indgen(9)+1,2), 'D'+strtrim(indgen(61)+10,2)]
  cd,current=dir
  overlay_state = { $
	base : 0L, $
	base2 : 0L, $
	filefld: 0L, $
	listfld: 0L, $
	lineWid: 0L, $
	path :dir, $
	filename:'', $
	list : '1', $   	; string of scan list entered
	scanlist : intarr(100), $
	nolist : 1, $
	maxlineno: 100, $
	lineno: 0, $
	detno: 15, $
	xaxis: 0, $	
	dname: li, $
	v : obj_new('scanSee') $
	}
  if keyword_set(maxlineno) then overlay_state.maxlineno = maxlineno

  found = findfile('scanSee.config',count=ct)
  if ct gt 0 then begin
  DC_view_readConfig,filename,path
  if filename ne '' then overlay_state.filename = filename
  if path ne '' then overlay_state.path = path
  end


  SCANSEEOVL = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1,  MAP=1, $
	TITLE='ScanSee Overlay 1D Plotter', $
      UVALUE='SCANSEEOVL')

  BASE0 = WIDGET_BASE(SCANSEEOVL, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE0')

  BASE3 = WIDGET_BASE(BASE0, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  BUTTON4 = WIDGET_BUTTON( BASE3, $
      UVALUE='SCANSEE1D_OVFILE', $
      VALUE='File...')
  BUTTON41 = WIDGET_BUTTON( BASE3, $
      UVALUE='SCANSEE1D_OVHELP', $
      VALUE='Help...')
  BUTTON5 = WIDGET_BUTTON( BASE3, $
      UVALUE='SCANSEE1D_OVDONE', $
      VALUE='Done')


  BASE2 = WIDGET_BASE(SCANSEEOVL, $
      COLUMN=1, /FRAME, $
      MAP=1, $
      UVALUE='BASE2')
  overlay_state.base2 = BASE2

  FIELD8 = CW_FIELD( BASE2,VALUE='', $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, XSIZE=60, $
      TITLE='Start File', $
      UVALUE='SCANSEE1D_OVNAME')

  BASE13 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE13')

  FIELD14 = CW_FIELD( BASE13,VALUE=overlay_state.list, $
      ROW=1, XSIZE=40, $
      RETURN_EVENTS=1, $
      TITLE='Scan # List', $
      UVALUE='SCANSEE1D_OVLIST')

  Btns775 = [ 'P1','P2','P3','P4' ]
  xaxis_pick = WIDGET_DROPLIST(BASE13,VALUE=Btns775,title='Xaxis', $
	UVALUE='SCANSEE1D_OVXPICK')
  WIDGET_CONTROL,xaxis_pick,SET_DROPLIST_SELECT=overlay_state.xaxis

  BASE30 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE33')
  BASE31 = WIDGET_BASE(BASE30, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE31')
  BASE32 = WIDGET_BASE(BASE30, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE32')


  label_tit1 = WIDGET_LABEL(BASE31,VALUE='Pick Detector')
  LIST22 = WIDGET_LIST( BASE31,VALUE=overlay_state.dname, $
      UVALUE='SCANSEE1D_OVDETOR', YSIZE=5)
  WIDGET_CONTROL,LIST22,SET_LIST_TOP=overlay_state.detno,SET_LIST_SELECT=overlay_state.detno

  label_tit2 = WIDGET_LABEL(BASE32,VALUE='Pick 1D Line #')
  LIST23 = WIDGET_LIST( BASE32, $
	VALUE='line #'+strtrim(indgen(overlay_state.maxlineno)+1,2), $
      	UVALUE='SCANSEE1D_OVLINEN', YSIZE=5)
  WIDGET_CONTROL,LIST23,SET_LIST_SELECT=overlay_state.lineno

  BUTTON15 = WIDGET_BUTTON( BASE30, $
      UVALUE='SCANSEE1D_OVACCEPT', $
      VALUE='Accept')

  overlay_state.base = SCANSEEOVL
  overlay_state.filefld = FIELD8
  overlay_state.listfld = FIELD14
  overlay_state.lineWid = LIST23


  WIDGET_CONTROL,SCANSEEOVL,SET_UVALUE=overlay_state
  WIDGET_CONTROL,overlay_state.base2,SENSITIVE=0

  WIDGET_CONTROL, SCANSEEOVL, /REALIZE

  XMANAGER, 'SCANSEEOVL', SCANSEEOVL
END
