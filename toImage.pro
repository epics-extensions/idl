@catcher_v1.pro

PRO catch1d_fill_2D_image
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
  COMMON CATCH1D_2D_COM, data_2d

; assign 2D data array

ip = 0
npts = scanData.act_npts-1
y_seqno = scanData.y_seqno
px = make_array(npts,/float)
for i=0,14 do begin
	if realtime_id.def(4+i) gt 0 then begin
	px = scanData.da(0:npts,i)
	CASE i OF
	0: data_2d.d1(0,0:npts,y_seqno) = px
	1: data_2d.d2(0,0:npts,y_seqno) = px
	2: data_2d.d3(0,0:npts,y_seqno) = px
	3: data_2d.d4(0,0:npts,y_seqno) = px
	4: data_2d.d5(0,0:npts,y_seqno) = px
	5: data_2d.d6(0,0:npts,y_seqno) = px
	6: data_2d.d7(0,0:npts,y_seqno) = px
	7: data_2d.d8(0,0:npts,y_seqno) = px
	8: data_2d.d9(0,0:npts,y_seqno) = px
	9: data_2d.d10(0,0:npts,y_seqno) = px
	10: data_2d.d11(0,0:npts,y_seqno) = px
	11: data_2d.d12(0,0:npts,y_seqno) = px
	12: data_2d.d13(0,0:npts,y_seqno) = px
	13: data_2d.d14(0,0:npts,y_seqno) = px
	14: data_2d.d15(0,0:npts,y_seqno) = px
	ENDCASE
	ip = ip + 1
	end
end

; update the image plot

;loadct, 39

npts = scanData.act_npts-1
if n_params() eq 0 then y_seqno = scanData.y_seqno
if y_seqno lt 0 then return
	width = scanData.image_width * scanData.image
	height = scanData.image_height * scanData.image

	old_win = !D.window
	new_win = old_win - 1 
	if y_seqno eq 0 then begin
		window,new_win, xsize = 8*width, ysize=2*height, title='2D_Images'
		for i=0,14 do begin
		xi=(i mod 8)*width+width/2 - 5
		yi=height/2+(15-i)/8*height
		xyouts, xi,yi,'D'+strtrim(i+1,2),/device
		end
        plots,[0,8*width],[height,height],/device
        for i=1,7 do plots,[i*width,i*width],[0,2*height],/device

	end

; WSET: Window is closed and unavailable.        -324  R4.0.1
; WSET: Window is closed and unavailable.        -367  R5.0
; WSET: Window is closed and unavailable.        -386  R5.1
CATCH,error_status
if error_status lt 0 then begin
;	print,!err_string,!err
	if error_status eq -367 or error_status eq -386 or error_status eq -324 then begin

;print,'name: ',!error_state.name
;print,'code: ',!error_state.code
;print,'msg:  ',!error_state.msg
;print,'sys_msg:  ',!error_state.sys_msg

	window,new_win, xsize = 8*width, ysize=2*height, title='2D_Images'
		for i=0,14 do begin
		xi=(i mod 8)*width+width/2 - 5
		yi=height/2+(15-i)/8*height
		xyouts, xi,yi,'D'+strtrim(i+1,2),/device
		end
        plots,[0,8*width],[height,height],/device
        for i=1,7 do plots,[i*width,i*width],[0,2*height],/device

        end
end

	wset,new_win

;	erase
	for sel=0,14 do begin
	if realtime_id.def(4+sel) gt 0 then begin
	CASE sel OF
	0: data_2d.image = data_2d.d1(0,0:npts,0:y_seqno)
	1: data_2d.image = data_2d.d2(0,0:npts,0:y_seqno)
	2: data_2d.image = data_2d.d3(0,0:npts,0:y_seqno)
	3: data_2d.image = data_2d.d4(0,0:npts,0:y_seqno)
	4: data_2d.image = data_2d.d5(0,0:npts,0:y_seqno)
	5: data_2d.image = data_2d.d6(0,0:npts,0:y_seqno)
	6: data_2d.image = data_2d.d7(0,0:npts,0:y_seqno)
	7: data_2d.image = data_2d.d8(0,0:npts,0:y_seqno)
	8: data_2d.image = data_2d.d9(0,0:npts,0:y_seqno)
	9: data_2d.image = data_2d.d10(0,0:npts,0:y_seqno)
	10: data_2d.image = data_2d.d11(0,0:npts,0:y_seqno)
	11: data_2d.image = data_2d.d12(0,0:npts,0:y_seqno)
	12: data_2d.image = data_2d.d13(0,0:npts,0:y_seqno)
	13: data_2d.image = data_2d.d14(0,0:npts,0:y_seqno)
	14: data_2d.image = data_2d.d15(0,0:npts,0:y_seqno)
	ENDCASE
	temp = congrid(data_2d.image, width, height)
	TVSCL, temp, sel
	end
	end

	wset,old_win
END
;
; extract 2D data from 1D file 
; scan_read_extract,startno=31,endno=109,infile='/home/sricat/CHA/2idd/cancer21.scans',/outfile
;
PRO scan_read_extract,startno=startno,endno=endno,infile=infile,outfile=outfile,view=view,y_pv=y_pv,new=new,XDR=XDR
;+
; NAME:
;       SCAN_READ_EXTRACT
;
; PURPOSE:
;       This routine provides the data catcher user a way of creating 2D 
;       images from the 1D scans.  Normally, 2D image are automatically 
;       saved by the data catcher.  Only if something abnomal happened 
;       during data acqusition and 2D images did not get saved, then
;       this routine can be used to create the images.
; 
;       Normally this routine is automatically invoked by the TOIMAGE 
;       widgets program.
;
; CATEGORY:
;       catcher_v1 utility routine.
;
; CALLING SEQUENCE:
;
;       SCAN_READ_EXTRACT, Startno=startno, Endno=endno, Infile='infile',
;           Outfile=outfile, View=view, New=new, Y_PV='scan2.pvname',/XDR
;
; INPUTS:
;       None.
;
; KEYWORD PARAMETERS:
;       INFILE:     Specifies the file name used by the data catcher 1D scan 
;
;       STARTNO:    Specifies 1D scan # at the beginning of 2D scan
;
;       ENDNO:      Specifies 1D scan # at the end of 2D scan
;	
;       VIEW:       Set this keyword each scan will be sequentially displayed
;                   in the data catcher drawing area.
;
;       OUTFILE:    Set this keyword the extracted images will be either 
;                   appended to the default image file or a new image file.
;
;       NEW:        Set this keyword a new image file will be created.
;
;       XDR:        Specifies the input file is in XDR binary format 
;
;       Y_PV:       Specifies the PV name for the 2D scan outer loop
;
; OUTPUTS:
;       During 2D image construction the images are display on screen.
;
; COMMON BLOCKS:
;       COMMON CATCH1D_COM
;       COMMON realtime_block
;       COMMON w_viewscan_block
;       COMMON CATCH1D_2D_COM
;       COMMON CATCH1D_2D_COMXY
;
; SIDE EFFECTS:
;       The data catcher is automatically loaded into IDL. 
;
; RESTRICTIONS:
;       The data catcher must be loaded into IDL. Make sure the entered
;       startno and endno are consistant with the 1D scan data. 
;       All the 1D scan should have same positioner array in order to
;       have a meaningful 2D image data generated.
;
; PROCEDURE:
;       This routine is automatcally loaded into IDL by loading toImage.pro
;
; EXAMPLE:
;       Extract the scan # 31 to 109 from the file 'cancer21.scans'
;
;       1) Screen preview only
;
;          scan_read_extract,startno=31,endno=109,infile='cancer21.scans'
;
;       2) Append images to default file 'cancer21.scans.image'
;
;          scan_read_extract,startno=31,endno=109,infile='cancer21.scans', $
;                 /OUTFILE
;
;       2) Create new image file 'test.image'
;
;          scan_read_extract,startno=31,endno=109,infile='cancer21.scans', $
;                 outfile='test',/NEW
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, 08-06-97.
;	xx-xx-xx   iii	comment
;
;-
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON CATCH1D_2D_COM, data_2d
COMMON CATCH1D_2D_COMXY, pxarray, pyarray

if keyword_set(infile) and keyword_set(startno) and keyword_set(endno) then begin

if keyword_set(y_pv) then scanData.y_pv = y_pv

; read in  index file

	catch1d_readFileIndex,infile
	if w_viewscan_id.maxno gt 1 then begin

	if startno gt w_viewscan_id.maxno then begin
		st = ['Only '+ string(w_viewscan_id.maxno)+'   1D scans found in ',infile]
		res = widget_message(st ,/Info)
		return
	end
	if endno gt w_viewscan_id.maxno then endno = w_viewscan_id.maxno

	if keyword_set(XDR) then u_openr, unit, infile, /XDR else $
	u_openr, unit, infile
	w_viewscan_id.unit = unit
newpos:
	point_lun,w_viewscan_id.unit, w_viewscan_id.fptr(startno-1)
	scan_read_record,unit,version,pv,num_pts,FA,x,y,n,ze
;	print,'w_plotspec_id.seqno, refno,y_seqno,scanno_2d,y_scan,y_req_npts,y_act_npts,y_value'
;	print,y
	; adjust startno endno
	if (endno - startno) gt y(5) then endno = startno + y(5)-1
	if y(4) eq 0 and y(2) eq 0 then begin
		startno = startno+1 
		goto, newpos
	end
	if y(4) and y(2) gt 0 then begin
		if y(2) lt y(5) then startno = startno - y(2)
		goto, newpos
	end

	width = num_pts(0) + 1
	pxarray = make_array(width)
	pxarray = FA(*,0)
	height = endno - startno + 1
	scanData.y_req_npts = height 
	pyarray = make_array(height)
	
		scanData.req_npts = width
		make_2d_data, data_2d, scanData.x_dpt, width, height 

		for i=0,height-1 do begin
		  id = i + startno
		  point_lun,w_viewscan_id.unit, w_viewscan_id.fptr(id-1)
		  if keyword_set(view) then scan_read, unit, id, id+1 else $
		  scan_read, unit, id, 0 
		  if scanData.y_scan eq 0 and scanData.y_seqno eq 0 then goto,new2dscan 
		  catch1d_fill_2D_image
		  pyarray(i) = scanData.y_value
		end
new2dscan:
		u_close, unit
	endno = startno+i-1

		if keyword_set(outfile) then begin
			if keyword_set(new) then begin
			if keyword_set(XDR) then $
			catch1d_extdata_mode_write_image,outfile , /XDR else $
			catch1d_extdata_mode_write_image,outfile 
			endif else begin 
			if keyword_set(XDR) then $
			catch1d_extdata_mode_write_image,infile, /XDR else $
			catch1d_extdata_mode_write_image,infile
			end
		end

	endif else begin
		res=WIDGET_MESSAGE('Not 2D scan data')
		return
	end	
	 
endif else begin
	print,'Usage: scan_read_extract,startno=startno,endno=endno,infile=infile [,outfile=outfile] [,new=new] [,view=view] [,y_pv=y_pv]
end

	st = [  '2D scan seqno #  : '+string(fix(y(3))), $
		'', $
		'    Start scan # : '+string(fix(startno)), $
		'    End   scan # : '+string(fix(endno)), $
		'    Image Width  : '+string(fix(y(5))), $
		'    Image Height : '+string(fix(endno-startno+1))]
	res = widget_message(st,/info)
END


PRO catch1d_extdata_mode_write_image,infile,xdr=xdr
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON LABEL_BLOCK,x_names,y_names,x_descs,y_descs,x_engus,y_engus
COMMON CATCH1D_2D_COM, data_2d
COMMON CATCH1D_2D_COMXY, pxarray, pyarray

filename = strtrim(infile,2)+'.image'

CATCH,error_status
if error_status eq -171 then begin
	w_warningtext,['scan_mode_write_image',!err_string]
	return
        end

	if keyword_set(XDR) then $
	openw,unit,filename,/GET_LUN,/APPEND, /XDR else $
	openw,unit,filename,/GET_LUN,/APPEND

	npts = scanData.req_npts-1
	y_seqno = scanData.y_seqno

if y_seqno ge 0 then begin

	pv2_desc=scanData.y_pv

for i=0,14 do begin
	if  realtime_id.def(4+i) gt 0 then begin
	CASE i OF
	0: data_2d.image = data_2d.d1(0,0:npts,0:y_seqno)
	1: data_2d.image = data_2d.d2(0,0:npts,0:y_seqno)
	2: data_2d.image = data_2d.d3(0,0:npts,0:y_seqno)
	3: data_2d.image = data_2d.d4(0,0:npts,0:y_seqno)
	4: data_2d.image = data_2d.d5(0,0:npts,0:y_seqno)
	5: data_2d.image = data_2d.d6(0,0:npts,0:y_seqno)
	6: data_2d.image = data_2d.d7(0,0:npts,0:y_seqno)
	7: data_2d.image = data_2d.d8(0,0:npts,0:y_seqno)
	8: data_2d.image = data_2d.d9(0,0:npts,0:y_seqno)
	9: data_2d.image = data_2d.d10(0,0:npts,0:y_seqno)
	10: data_2d.image = data_2d.d11(0,0:npts,0:y_seqno)
	11: data_2d.image = data_2d.d12(0,0:npts,0:y_seqno)
	12: data_2d.image = data_2d.d13(0,0:npts,0:y_seqno)
	13: data_2d.image = data_2d.d14(0,0:npts,0:y_seqno)
	14: data_2d.image = data_2d.d15(0,0:npts,0:y_seqno)
	ENDCASE

	; write the detector  number i, width, height

	pvs = make_array(60,6,/byte)
	pvs(0,0) = byte(scanData.pv)
	pvs(0,1) = byte(scanData.y_pv)
	temp = w_plotspec_array(3)
	if strlen(temp) gt 60 then temp = strmid(temp,0,60)
	pvs(0,2) = byte(temp)

;  3  for scan1  positioner desc
;  4  for scan2  positioner desc
;  5  for detector desc

        len = strlen(x_descs(0))
        if len gt 0 then pvs(0:len-1,3) = byte(x_descs(0)) else $
	pvs(0,3) = byte(x_names(0))
        len = strlen(pv2_desc)
        if len gt 0 then pvs(0:len-1,4) = byte(pv2_desc)
        len = strlen(y_descs(i))
        if len gt 0 then pvs(0:len-1,5) = byte(y_descs(i)) else $
	pvs(0,5) = byte(y_names(i))

	u_write,unit,pvs

if scanData.debug eq 1 then begin
print,'2D Scan # =',scanData.scanno_2d, ' Scan # =',scanData.scanno
print,'    width =',scanData.req_npts, ' height =', $
	scanData.y_seqno, ' Detector # =',i
end

	x = [scanData.scanno, scanData.req_npts, scanData.y_seqno+1, i, $
		scanData.scanno_2d, scanData.y_req_npts]

	u_write,unit,x

	; write x and y positioner array
	
	x = pxarray
	u_write,unit,x

	y = pyarray
	u_write,unit,y

	; write image

	u_write,unit,data_2d.image
	end
end
end
	free_lun,unit
	
END

PRO toImage_open,Event
COMMON TOIMAGE_BLOCK,widget_ids

	F = PICKFILE(TITLE='Open ...',FILE='catch1d.trashcan',/READ, $
		GET_PATH=P)
        IF F eq '' THEN return
	WIDGET_CONTROL,widget_ids.infile,SET_VALUE=F
	catcher_v1,data=F,config='catch1d.config',GROUP=Event.Top
		
END

PRO TOIMAGE_PDMENU3_Event, Event


  CASE Event.Value OF 


  'File.Open ...': BEGIN
    PRINT, 'Event for File.Open ...'
    toImage_open,Event
    END
  'File.Quit': BEGIN
    WIDGET_CONTROL,Event.top,/DESTROY
    END
  ENDCASE
END



PRO MAIN13_Event, Event
COMMON TOIMAGE_BLOCK,widget_ids

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  ; Event for TOIMAGE_PDMENU3
  'TOIMAGE_PDMENU3': TOIMAGE_PDMENU3_Event, Event

    'TOIMAGE_XDR': BEGIN
        widget_ids.XDR = Event.Index
        END

  'TOIMAGE_INFILE': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=infile
	widget_ids.in = infile(0)
	WIDGET_CONTROL,/HOURGLASS
	catcher_v1,data=infile(0),config='catch1d.config', GROUP=Event.top
      END
  'TOIMAGE_OUTFILE': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=outfile
	widget_ids.out = outfile(0)
      END
  'TOIMAGE_Y_PV': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=pv
	widget_ids.y_pv = pv(0) 
      END
  'TOIMAGE_START': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=i
	widget_ids.start = i
      END
  'TOIMAGE_END': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=i
	widget_ids.last = i
      END
  'TOIMAGE_NEW': BEGIN
	widget_ids.new= Event.Index  
      END

  'TOIMAGE_ACCEPT': BEGIN
	WIDGET_CONTROL,widget_ids.infile,GET_VALUE=in
	WIDGET_CONTROL,widget_ids.outfile,GET_VALUE=out
	WIDGET_CONTROL,widget_ids.y_pvname,GET_VALUE=y_pv
	WIDGET_CONTROL,widget_ids.startno,GET_VALUE=is
	WIDGET_CONTROL,widget_ids.endno,GET_VALUE=ie
	widget_ids.in = in(0)
	widget_ids.out = out(0)
	widget_ids.y_pv= y_pv(0)
	widget_ids.start = is
	widget_ids.last = ie

      Print, 'Event for Infile:', widget_ids.in
      Print, 'Event for Outfile:', widget_ids.out
      Print, 'Event for Start #', widget_ids.start
      Print, 'Event for End #', widget_ids.last
      
	if widget_ids.new eq 0 then $
	scan_read_extract,startno=widget_ids.start, endno=widget_ids.last, $
		infile=widget_ids.in, XDR=widget_ids.XDR
       
	if widget_ids.new eq 1 then $
	scan_read_extract,startno=widget_ids.start, endno=widget_ids.last, $
		infile=widget_ids.in, /outfile, XDR=widget_ids.XDR

	if widget_ids.new eq 2 then begin 
	if strlen(strtrim(widget_ids.out , 2)) eq 0 then begin
		res = WIDGET_MESSAGE('Out Image File must be provided !')
		return
		end 
	scan_read_extract,startno=widget_ids.start, endno=widget_ids.last, $
		infile=widget_ids.in, outfile=widget_ids.out, /new, $
		y_pv = widget_ids.y_pv, XDR=widget_ids.XDR

	end
      END
  'TOIMAGE_CANCEL': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END MAIN13
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO toImage, GROUP=Group,viewonly=viewonly
;+
; NAME:
;       TOIMAGE
;
; PURPOSE:
;       This program provides the data catcher user a way of creating 2D 
;       images from the 1D scans.  Normally, 2D image are automatically 
;       saved by the data catcher.  Only if something abnomal happened 
;       during data acqusition and 2D images did not get saved, then
;       this program can be used to create the images.
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;
;       TOIMAGE 
;
; INPUTS:
;       None.
;
; KEYWORD PARAMETERS:
;     GROUP:    The widget ID of the group leader of the widget.  If this 
;               keyword is specified, the death of the group leader results in
;               the death of TOIMAGE.
;     VIEWONLY: To construct and view the 2D images from the 1D scan data 
;
; OUTPUTS:
;       During 2D image construction the images are display on screen.
;
; COMMON BLOCKS:
;       COMMON TOIMAGE_BLOCK,widget_ids
;
; SIDE EFFECTS:
;       The data catcher is automatically loaded into IDL. 
;
; RESTRICTIONS:
;       Same environment and IDL setup as the data catcher is required. 
;       Source the setup_ezcaIDL from the EPICS_EXTENSIONS/bin/$HOST_ARCH.
;
; PROCEDURE:
;       This program is automatcally loaded into IDL by loading toImage.pro
;
; EXAMPLE:
;      
;       .run toImage
;       TOIMAGE
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, 08-06-97.
;	xx-xx-xx   iii	comment
;
;-
COMMON TOIMAGE_BLOCK,widget_ids


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      TITLE='toImage', $
      UVALUE='MAIN13')

  BASE2 = WIDGET_BASE(MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL5 = WIDGET_LABEL( BASE2, $
   FONT='-adobe-helvetica-bold-r-normal--20-140-*', $
      UVALUE='LABEL5', $
      VALUE='1D Scans To 2D Image')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  MenuDesc167 = [ $
      { CW_PDMENU_S,       3, 'File' }, $ ;        0
        { CW_PDMENU_S,       0, 'Open ...' }, $ ;        1
        { CW_PDMENU_S,       2, 'Quit' } $  ;      2

  ]


  TOIMAGE_PDMENU3 = CW_PDMENU( BASE3, MenuDesc167, /RETURN_FULL_NAME, $
      UVALUE='TOIMAGE_PDMENU3')
 
  btn_xdr = ['BIN','XDR']
  toimage_xdr = WIDGET_DROPLIST(BASE3, VALUE=btn_xdr, $
        UVALUE='TOIMAGE_XDR',TITLE='')

  BASE7 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE7')

  FieldVal483 = [ $
    'catch1d.trashcan' ]
  INFILENAME = CW_FIELD( BASE7,VALUE=FieldVal483, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Infile:', $
      UVALUE='TOIMAGE_INFILE', $
      XSIZE=60)

  BASE8 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE8')

  FieldVal610 = [ $
    '' ]
  Y_PVNAME = CW_FIELD( BASE8,VALUE=FieldVal610, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Y-PV', $
      UVALUE='TOIMAGE_Y_PV', $
      XSIZE=20)

  FieldVal613 = [ $
    '' ]
  STARTNO = CW_FIELD( BASE8,VALUE=FieldVal613, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Start #', $
      UVALUE='TOIMAGE_START', $
      XSIZE=4)

  FieldVal678 = [ $
    '' ]
  ENDNO = CW_FIELD( BASE8,VALUE=FieldVal678, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='End #', $
      UVALUE='TOIMAGE_END', $
      XSIZE=4)


if keyword_set(viewonly) then $
BTNS913=['ViewOnly']	else $
BTNS913=['ViewOnly','Append','CreateNew'] 
new_image= WIDGET_DROPLIST(BASE8, VALUE=BTNS913, $
        UVALUE='TOIMAGE_NEW',TITLE='Images :')
  WIDGET_CONTROL,new_image,set_droplist_select = 0


  FieldVal548 = [ $
    '' ]
  OUTFILENAME = CW_FIELD( BASE2,VALUE=FieldVal548, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Out Image File:', $
      UVALUE='TOIMAGE_OUTFILE', $
      XSIZE=60)
  if keyword_set(viewonly) then $
  WIDGET_CONTROL,OUTFILENAME,SENSITIVE= 0

  BASE9 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE9')

  ACCEPT = WIDGET_BUTTON(BASE9,VALUE='Accept',UVALUE='TOIMAGE_ACCEPT')
  CANCEL = WIDGET_BUTTON(BASE9,VALUE='Cancel',UVALUE='TOIMAGE_CANCEL')

  WIDGET_CONTROL,toimage_xdr,set_droplist_select = 1
  widget_ids = { $
	in: '', $
	out: '', $
	y_pv:'', $
	start: 0, $
	last: 0, $
	new: 0, $
	XDR: 1, $
	infile: INFILENAME, $
	outfile: OUTFILENAME, $
	y_pvname: Y_PVNAME, $
	startno: STARTNO, $
	endno: ENDNO $
	}


  WIDGET_CONTROL, MAIN13, /REALIZE

  XMANAGER, 'MAIN13', MAIN13
END
