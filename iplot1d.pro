 


PRO IPLOT1D_DRV_Event, Event

  WIDGET_CONTROL,Event.top,GET_UVALUE= iplot1d_data,/no_copy
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'IPLOT1D_BGROUP': BEGIN
      END
  'IPLOT1D_CLEAR': BEGIN
	widget_control,iplot1d_data.pickwid,get_value=id
	id(*) = 0
	widget_control,iplot1d_data.pickwid,set_value=id
      END
  'IPLOT1D_ALL': BEGIN
	widget_control,iplot1d_data.pickwid,get_value=id
	id(*) = 1
	widget_control,iplot1d_data.pickwid,set_value=id
      END
  'IPLOT1D_DONE': BEGIN
	widget_control,iplot1d_data.pickwid,get_value=id
	pickid = where(id gt 0)
	if pickid(0) ge 0 then begin
	ndim = n_elements(pickid)
	x = *iplot1d_data.xarr
	no = n_elements(x)
	y = make_array(no,ndim)
	dname = iplot1d_data.dname ;strarr(ndim)
	yarr = *iplot1d_data.yarr
	for i=0,ndim-1 do begin
	y(*,i) = yarr(*,pickid(i))
	end
	sdname = dname(pickid)
	iplot1d,x,y,dname=sdname,title=iplot1d_data.title
	end
      END
  'IPLOT1D_CANCEl': BEGIN
	widget_control,Event.top,/destroy
	return
      END
  ENDCASE

  WIDGET_CONTROL,Event.top,SET_UVALUE= iplot1d_data,/no_copy

END



PRO IPLOT1D_DRV,xarr,yarr,detname=detname,sel=sel,title=title,xtitle=xtitle, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  sz = size(yarr)
  if sz(0) lt 1 then return
  ndim = 1
  if sz(0) eq 2 then ndim = sz(2)

  dname = 'D' + [ strtrim(indgen(9)+1,2),'A','B','C','D','E','F', $
	'01','02', '03','04','05','06','07','08','09','10', $
	strtrim(indgen(60)+11,2)]

  dname = dname(0:ndim-1)
  if keyword_set(detname) then dname=detname
  wintitle='IPLOT1D_DRV'
  if keyword_set(title) then wintitle=title
  
  if keyword_set(title) eq 0 then title='IPLOT1D_DRV'
  IPLOT1D_DRV = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, title=wintitle, $
      MAP=1, $
      UVALUE='IPLOT1D_DRV')

  BASE2 = WIDGET_BASE(IPLOT1D_DRV, $
      /column, $
      MAP=1, $
      UVALUE='BASE2')

  BASE2_1 = WIDGET_BASE(BASE2,xsize=600,ROW=6, MAP=1, UVALUE='BASE2_1')
;  Btns3228= strtrim(indgen(ndim)+1,2)
  Btns3228 = dname
  BGROUP3 = CW_BGROUP( BASE2_1, Btns3228, $
      row=5, x_scroll_size=600, $
      NONEXCLUSIVE=1, $
      LABEL_TOP='Select Desired Curve # for IPLOT1D Program', $
      UVALUE='IPLOT1D_BGROUP')
  setv = [1,1]
  if keyword_set(sel) then setv = sel
  if ndim gt 1 then widget_control,BGROUP3,set_value=setv  else $
  widget_control,BGROUP3,set_value=1

  BASE6 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE6')

  BUTTON5 = WIDGET_BUTTON( BASE6, $
      UVALUE='IPLOT1D_CLEAR', $
      VALUE='Clear')

  BUTTON6 = WIDGET_BUTTON( BASE6, $
      UVALUE='IPLOT1D_ALL', $
      VALUE=' All ')

  BUTTON7 = WIDGET_BUTTON( BASE6, $
      UVALUE='IPLOT1D_DONE', $
      VALUE='Accept')

  BUTTON8 = WIDGET_BUTTON( BASE6, $
      UVALUE='IPLOT1D_CANCEl', $
      VALUE='Cancel')

  iplot1d_data = { $
	title: '', $
	dname : dname, $
	xarr : ptr_new(/allocate_heap), $
	yarr : ptr_new(/allocate_heap), $
	pickwid : BGROUP3, $
	pickid : intarr(ndim) }

  if keyword_set(xtitle) then iplot1d_data.title = xtitle

  *iplot1d_data.xarr = xarr
  *iplot1d_data.yarr = yarr

  WIDGET_CONTROL, IPLOT1D_DRV, /REALIZE
  widget_control,IPLOT1D_DRV,set_uvalue=iplot1d_data,/no_copy

  XMANAGER, 'IPLOT1D_DRV', IPLOT1D_DRV
END



PRO iplot1d,x,y,dname=dname,title=title,xtitle=xtitle
 
if keyword_set(title) eq 0 then title='IPLOT_1D'
if keyword_set(xtitle) eq 0 then xtitle = ''
if n_params() eq 0 then return
if n_params() eq 1 then begin
	sz = size(x)
	if sz(0) eq 0 then return
	y = x
	no = sz(1)
	x = indgen(no)
	xtitle='Index #'
end

	no = n_elements(x)
	sym_increment = no/11
	sz = size(y)
	ndim = 1
	if sz(0) eq 2 then ndim = sz(2)
	if keyword_set(dname) eq 0 then begin
	dname = 'D'+strtrim(indgen(ndim)+1,2)
	end

	xmin= min(x(0))
	xmax = max(x(no-1))
	dx = (xmax-xmin)*.1
	if dx eq 0 then dx = .5
	xrange=[xmin-dx,xmax+dx]

	ymin= min(y)
	ymax = max(y)
	dy = (ymax-ymin)*.1
	if dy eq 0 then dy = .5
	yrange=[ymin-dy,ymax+dy]

	tvlct,r,g,b,/get
	iplot,x,y(*,0),identifier=ID1,name=dname(0), $
		title=title,xtitle=xtitle,xrange=xrange,yrange=yrange, $
		sym_index=1,sym_size=.25,sym_increment=sym_increment
	if ndim gt 1 then begin
	for i=1,ndim-1 do begin
	sym = i mod 9 + 1
	iplot,/overplot,x,y(*,i),identifier=ID1,name=dname(i), $
		sym_thick=2, xrange=xrange,yrange=yrange, $
		sym_index=sym,sym_size=.25,sym_increment=sym_increment
	end
	end
END
