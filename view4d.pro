


PRO sscan_read4dPick,ssd,pda4d,idet=idet
; idet - specifys the desired detector 4D array to be read
;	 otherwise all detectors will be read

	nd = ssd.nb_det(0)
	if nd eq 0 then return


if ssd.im_filled[3] eq 0 then begin 
	sscan_read4D,SSD,da2D,da3D,da4D,idet=idet 
	ssd.im_filled[3] = 1    ; indicate 4D data read in
	endif else da4d = *ssd.da[0]

	npt = ssd.npts
	if idet gt nd-1 then idet = nd-1
	pda4d = reform(da4d(*,*,*,*,idet),npt(0),npt(1),npt(2),npt(3))
END


PRO sscan_read4D,SSD,da2D,da3D,da4D,idet=idet,pa4D=pa4D
; idet - specify the seq # of the inner most defined detector for a 4D scan
; read 4D scan
   if SSD.rank lt 4 then return
	ptr4D = *SSD.sub_scan_ptr(0)
	if n_elements(ptr4D) eq 0 then return
	if keyword_set(idet) eq 0 then idet=0
	if idet ge SSD.nb_det(0) then begin
		print,'Valid idet < ',ssd.nb_det(0)-1
		return
 	end
widget_control,/hourglass
t1=systime(1)
	if SSD.nb_det(2) eq 0 then da2d = *SSD.da(2) else $
	da2D = make_array(SSD.npts(2),SSD.npts(3),SSD.nb_det(2)) 
	if SSD.nb_det(1) eq 0 then da3d = *SSD.da(1) else $
	da3D = make_array(SSD.npts(1),SSD.npts(2),SSD.npts(3),SSD.nb_det(1))
	if SSD.nb_det(0) eq 0 then da4d = *SSD.da(0) else $
	da4D = make_array(SSD.npts(0),SSD.npts(1),SSD.npts(2),SSD.npts(3),SSD.nb_det(0))
	cpt = SSD.cpt
	d = make_array(SSD.npts(0))	; alloc data vector
tp = 0L
	for l=0,cpt(3)-1 do begin
	  pos = ptr4D(l)
	  sscan_read1D,SSD,l,level=0,pa=p0,da=d0 
	  if n_elements(d0) gt 1 and SSD.im_filled(1) eq 0 then $
		 da2D(*,l,*) = d0(*,*)
	for k=0,cpt(2)-1 do begin
	    sscan_read1D,SSD,k,level=1,pa=p1,da=d1 
	  if n_elements(d1) gt 1 and SSD.im_filled(1) eq 0 then $
		 da3D(*,k,l,*) = d1(*,*)

	if idet ge 0 and idet lt ssd.nb_det(0) then begin
 	  point_lun,-SSD.lun,tp1
	  for j=0,cpt(1)-1 do begin
		if j eq 0 then begin
		sscan_read1D,SSD,j,level=2,pa=p2,da=d2 
		  point_lun,-SSD.lun,tp
	  	  SSD.lrecl = tp-tp1
		  dl = size(d2)
		  SSD.dlen = dl(dl(0)+2)*4L
		  SSD.offset = SSD.lrecl-SSD.dlen ;+ dl(1)*idet*4L
		endif else begin
		  rpos =tp+SSD.offset 
			point_lun,SSD.lun,rpos
			readu,SSD.lun,d2
		  tp = tp+SSD.lrecl
		end
		if n_elements(d2) gt 1 then $
		da4D(*,j,k,l,*) = d2(*,*)
		if j eq 0 then pa4D = p2
	  end
	end
	end

	end
print,'Read Det=',idet+1,'   Time used=',systime(1)-t1

	if idet ge 0 then *SSD.da[0] = da4D
	
	if SSD.im_filled(1) eq 0 then begin
		*SSD.da[1] = da2D
		scanSee_fillImage,SSD,da2D  ;,/echo
	end
widget_control,/clear_events
	*SSD.da[0] = da4d
	*SSD.da[1] = da3d
	*SSD.da[2] = da2d
END



PRO view4d_Event, Event

  WIDGET_CONTROL,Event.top,GET_UVALUE=view4d_state
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'view4d_AXIS': BEGIN
      if Event.Value ne view4d_state.axis  then begin
      CASE  view4d_state.axis OF
      0: widget_control,view4d_state.XWID,sensitive=0
      1: widget_control,view4d_state.YWID,sensitive=0
      2: widget_control,view4d_state.ZWID,sensitive=0
      3: widget_control,view4d_state.UWID,sensitive=0
      ELSE: Message,'Unknown button pressed'
      ENDCASE

	view4d_state.axis = Event.Value

      CASE Event.Value OF
      0: widget_control,view4d_state.XWID,sensitive=1
      1: widget_control,view4d_state.YWID,sensitive=1
      2: widget_control,view4d_state.ZWID,sensitive=1
      3: widget_control,view4d_state.UWID,sensitive=1
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      end
      END
  'view4d_XI': BEGIN
	widget_control,Event.Id,get_value=xi
	view4d_state.slice = xi
      END
  'view4d_YI': BEGIN
	widget_control,Event.Id,get_value=xi
	view4d_state.slice = xi
      END
  'view4d_ZI': BEGIN
	widget_control,Event.Id,get_value=xi
	view4d_state.slice = xi
      END
  'view4d_UI': BEGIN
	widget_control,Event.Id,get_value=xi
	view4d_state.slice = xi
      END
  'view4d_CLOSE': BEGIN
	widget_control,event.top,/destroy
	return
      END
  'view4d_PANIMAGE': BEGIN
	id = view4d_state.slice
	sz = view4d_state.sz
	case view4d_state.axis of
	0: begin
	   v = view4d_state.data(id,*,*,*) 
	   v = reform(v,sz(2),sz(3),sz(4))
	   widget_control,view4d_state.XWID,get_value=id
	   end
	1: begin
	   v = view4d_state.data(*,id,*,*) 
	   v = reform(v,sz(1),sz(3),sz(4))
	   widget_control,view4d_state.YWID,get_value=id
	   end
	2: begin
	   v = view4d_state.data(*,*,id,*)
	   v = reform(v,sz(1),sz(2),sz(4))
	   widget_control,view4d_state.ZWID,get_value=id
	   end
	3: begin
	   v = view4d_state.data(*,*,*,id)
	   v = reform(v,sz(1),sz(2),sz(3))
	   widget_control,view4d_state.UWID,get_value=id
	   end
	else:
	endcase
	title='AXIS: '+strtrim(view4d_state.axis+1,2) + ', SLICE INDEX: ' + strtrim(id,2)
	panimage,v,title=title
	END
  'view4d_3D': BEGIN
	SSD = view4d_state.SSD
	HD_P = SSD.HD_P
	id = view4d_state.slice
	sz = view4d_state.sz
	rank = view4d_state.axis
	case rank of
	0: begin
	   v = view4d_state.data(id,*,*,*) 
	   v = reform(v,sz(2),sz(3),sz(4))
	   widget_control,view4d_state.XWID,get_value=id
		xd = *SSD.pa(1)
		xv = xd(*,0)
		yd = *SSD.pa(2)
		yv = yd(*,0)
		zd = *SSD.pa(3)
		zv = zd(*,0)
		descs = [HD_P(0,1).PXDS,HD_P(0,2).PXDS,HD_P(0,3).PXDS]
	   end
	1: begin
	   v = view4d_state.data(*,id,*,*) 
	   v = reform(v,sz(1),sz(3),sz(4))
	   widget_control,view4d_state.YWID,get_value=id
		xd = *SSD.pa(0)
		xv = xd(*,0)
		yd = *SSD.pa(2)
		yv = yd(*,0)
		zd = *SSD.pa(3)
		zv = zd(*,0)
		descs = [HD_P(0,0).PXDS,HD_P(0,2).PXDS,HD_P(0,3).PXDS]
	   end
	2: begin
	   v = view4d_state.data(*,*,id,*)
	   v = reform(v,sz(1),sz(2),sz(4))
	   widget_control,view4d_state.ZWID,get_value=id
		xd = *SSD.pa(0)
		xv = xd(*,0)
		yd = *SSD.pa(1)
		yv = yd(*,0)
		zd = *SSD.pa(3)
		zv = zd(*,0)
		descs = [HD_P(0,0).PXDS,HD_P(0,1).PXDS,HD_P(0,3).PXDS]
	   end
	3: begin
	   v = view4d_state.data(*,*,*,id)
	   v = reform(v,sz(1),sz(2),sz(3))
	   widget_control,view4d_state.UWID,get_value=id
		xd = *SSD.pa(0)
		xv = xd(*,0)
		yd = *SSD.pa(1)
		yv = yd(*,0)
		zd = *SSD.pa(2)
		zv = zd(*,0)
		descs = [HD_P(0,0).PXDS,HD_P(0,1).PXDS,HD_P(0,2).PXDS]
	   end
	else:
	endcase
	title=ssd.class + ' '+'AXIS: '+strtrim(view4d_state.axis+1,2) + ', SLICE INDEX: ' + strtrim(id,2)
	view3d_2d,v,0,xv,yv,zv,group=Event.top,title=title,descs=descs
      END
  ENDCASE

    widget_control,Event.top,set_uvalue=view4d_state
END


PRO view4d,data,file=file,idet=idet,GROUP=Group,SSD=SSD,title=title
; INPUT: 
;	data(i,j,k,l)  - input 4D array to be sliced
;
; KEYWORD:
;	file  - specifies the input 4d scan file name if data not defined
;	idet  - specifies the detector seq #, default 0
;	SSD   - specifies the input SSD data struct
;	title - specifiies the window title
;

if n_params() eq 0 then begin
	if keyword_set(file) then begin
	sscan_read,ssd,file=file
	if ssd.rank lt 3 then begin
	print,'No 4D data entered'
	return
	end
	if n_elements(idet) eq 0 then idet=0
	sscan_read4dpick,ssd,data,idet=idet
	end
end
sz = size(data)
if sz(0) ne 4 then begin
	print,'not 4D data entered'
	return
end

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  if keyword_set(title) eq 0 then TITLE="SSCAN 4D SLICER"


  view4d = WIDGET_BASE(GROUP_LEADER=Group, $
      TITLE=title, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='view4d')

  lb0 = widget_label(view4d,value='4D SLICER',font='10x20')

  BASE2 = WIDGET_BASE(view4d, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  Btns234 = [ $
    '1', $
    '2', $
    '3', $
    '4' ]
  BGROUP4 = CW_BGROUP( BASE3, Btns234, $
      COLUMN=1, $
      EXCLUSIVE=1, set_value=3, $
      LABEL_TOP='PICK AXIS SLICE #', $
      UVALUE='view4d_AXIS')

  BUTTON3 = WIDGET_BUTTON( BASE3, $
      UVALUE='view4d_PANIMAGE', $
      VALUE='PanImage ...')

  BASE4 = WIDGET_BASE(BASE3, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE4')

  SLIDER1 = WIDGET_SLIDER( BASE4, $
      MAXIMUM=sz(1)-1, $
      MINIMUM=0, $
      UVALUE='view4d_XI', $
      VALUE=sz(1)-1)

  SLIDER2 = WIDGET_SLIDER( BASE4, $
      MAXIMUM=sz(2)-1, $
      MINIMUM=0, $
      UVALUE='view4d_YI', $
      VALUE=sz(2)-1)

  SLIDER3 = WIDGET_SLIDER( BASE4, $
      MAXIMUM=sz(3)-1, $
      MINIMUM=0, $
      UVALUE='view4d_ZI', $
      VALUE=sz(3)-1)

  SLIDER4 = WIDGET_SLIDER( BASE4, $
      MAXIMUM=sz(4)-1, $
      MINIMUM=0, $
      UVALUE='view4d_UI', $
      VALUE=sz(4)-1)

  widget_control,SLIDER1,sensitive=0
  widget_control,SLIDER2,sensitive=0
  widget_control,SLIDER3,sensitive=0

  lb1 = widget_label(base4, value='Axial SLICE #')

  BASE6 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE6')

  BUTTON8 = WIDGET_BUTTON( BASE6, $
      UVALUE='view4d_3D', $
      VALUE='RUN 3D SLICER ...')

  BUTTON7 = WIDGET_BUTTON( BASE6, $
      UVALUE='view4d_CLOSE', $
      VALUE='Close')


  view4d_state = { SSD: SSD, $
	data:data, $
	XWID: slider1, YWID: slider2, ZWID: slider3, $
	UWID: slider4, $
	sz : sz, $
	axis:3,  slice:sz(4)-1}

  WIDGET_CONTROL, view4d, set_uvalue=view4d_state

  WIDGET_CONTROL, view4d, /REALIZE

  XMANAGER, 'view4d', view4d
END



