@catcher_v1.pro
;
; extract 2D data from 1D file 
; scan_read_extract,startno=31,endno=109,infile='/home/sricat/CHA/2idd/cancer21.scans',/outfile
;
PRO scan_read_extract,startno=startno,endno=endno,infile=infile,outfile=outfile,view=view,y_pv=y_pv,new=new
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
;           Outfile=outfile, View=view, New=new, Y_PV='scan2.pvname'
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

	u_openr, unit, infile
	w_viewscan_id.unit = unit
	point_lun,w_viewscan_id.unit, w_viewscan_id.fptr(startno-1)
	scan_read_record,unit,version,pv,num_pts,FA,x,y,n,ze
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
		  catch1d_fill_2D_data
		  pyarray(i) = scanData.y_value
		end
		u_close, unit

		if keyword_set(outfile) then begin
			if keyword_set(new) then begin
			catch1d_extdata_mode_write_image,outfile 
			endif else $
			catch1d_extdata_mode_write_image,infile
		end

	endif else begin
		res=WIDGET_MESSAGE('Not 2D scan data')
		return
	end	
	 
endif else begin
	print,'Usage: scan_read_extract,startno=startno,endno=endno,infile=infile [,outfile=outfile] [,new=new] [,view=view] [,y_pv=y_pv]
end

END


PRO catch1d_extdata_mode_write_image,infile
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

PRO PDMENU3_Event, Event


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

  ; Event for PDMENU3
  'PDMENU3': PDMENU3_Event, Event
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
		infile=widget_ids.in
       
	if widget_ids.new eq 1 then $
	scan_read_extract,startno=widget_ids.start, endno=widget_ids.last, $
		infile=widget_ids.in, /outfile

	if widget_ids.new eq 2 then begin 
	if strlen(strtrim(widget_ids.out , 2)) eq 0 then begin
		res = WIDGET_MESSAGE('Out Image File must be provided !')
		return
		end 
	scan_read_extract,startno=widget_ids.start, endno=widget_ids.last, $
		infile=widget_ids.in, outfile=widget_ids.out, /new, $
		y_pv = widget_ids.y_pv
	end
      END
  'TOIMAGE_CANCEL': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END MAIN13
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO toImage, GROUP=Group
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
      UVALUE='MAIN13')

  BASE2 = WIDGET_BASE(MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL5 = WIDGET_LABEL( BASE2, $
   FONT='-adobe-helvetica-bold-r-normal--20-140-*', $
      UVALUE='LABEL5', $
      VALUE='1D Scans To 2D Image')

  MenuDesc167 = [ $
      { CW_PDMENU_S,       3, 'File' }, $ ;        0
        { CW_PDMENU_S,       0, 'Open ...' }, $ ;        1
        { CW_PDMENU_S,       2, 'Quit' } $  ;      2

  ]


  PDMENU3 = CW_PDMENU( BASE2, MenuDesc167, /RETURN_FULL_NAME, $
      UVALUE='PDMENU3')

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

  BASE9 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE9')

  ACCEPT = WIDGET_BUTTON(BASE9,VALUE='Accept',UVALUE='TOIMAGE_ACCEPT')
  CANCEL = WIDGET_BUTTON(BASE9,VALUE='Cancel',UVALUE='TOIMAGE_CANCEL')

  widget_ids = { $
	in: '', $
	out: '', $
	y_pv:'', $
	start: 0, $
	last: 0, $
	new: 0, $
	infile: INFILENAME, $
	outfile: OUTFILENAME, $
	y_pvname: Y_PVNAME, $
	startno: STARTNO, $
	endno: ENDNO $
	}


  WIDGET_CONTROL, MAIN13, /REALIZE

  XMANAGER, 'MAIN13', MAIN13
END
