@u_read.pro
@colorbar.pro
@defroi_pick.pro

PRO multiroi_help

str = ['DEFROI_PICK allows the user flexiblly redefine the ROIs by', $
	'free hand drawing or toggling the pixel elements.', $ 
	'Element selected is marked with hash line and associated ROI number', $
	'Done        - Close the multi-roi selection program', $
	'Help...     - Show this on-line help', $
	'Color...    - Change the IDL window color map', $
	'Refresh     - Refresh TV image', $
	'ShowAll ROIs - Redraw all the selected elements', $
	'ROI #:      - Show list of existing ROIs defined for the image', $
	'              Re-display the ROI statistic in the scroll area', $
	'Draw PolyROI - Redraw the PolyROI for a selected ROI #', $ 
	'Modify ROI  - Select/unselect pixels for a selected ROI #', $ 
	'Add ROI     - Add an additional ROI to the ROI # list', $
	'Del ROI     - Delete the selected ROI', $
	'Zoom Mod/Add- Zoom a box region, modify and add newly picked pixel elements into the ROI', $
	'Zoom Del    - Zoom a box region and delete picked pixel elements from the ROI', $
	'Query Image - Query image mode, RMB to stop', $
	'Scroll Area - Display the ROI statistics if new ROI# is selected', $
	'Save As ... - Save the scroll window content to a disk file', $
	'Print       - Print the scroll window content to printer', $
	'Clear       - Clear the scroll window', $
	'Offset Val: - Offset value', $
	'charsize:   - Specify the charsize used in marking the pixel' $
	]
	r = dialog_message(str,/info)
END

PRO multiroi_pick_Event, Event


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev
  WIDGET_CONTROL, Event.Top, GET_UVALUE=defroi_pickinfo 

  CASE Ev OF 

  'DEFROIPICK_DRAW': BEGIN
      END
  'DEFROIPICK_DONE': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
	return
      END
  'DEFROIPICK_REFRESH': BEGIN
	defroi_refresh,defroi_pickinfo.im0
      END
  'DEFROIPICK_HELP': BEGIN
	multiroi_help	
      END
  'DEFROIPICK_COLOR': BEGIN
	xloadct,GROUP=Event.top	
      END
  'DEFROIPICK_ALL': BEGIN
	im = defroi_pickinfo.im0
	defroi_listall,im,picke,charsize=defroi_pickinfo.csize
	defroi_pickinfo.picke = picke
      END
  'DEFROIPICK_LISTREGION': BEGIN
	im = defroi_pickinfo.im0
	picke = defroi_pickinfo.picke
	region = defroi_pickinfo.region_id
	defroi_refresh,im
	defroi_listregion,im,picke,region_data,region=region
	END
  'DEFROIPICK_LIST': BEGIN
	r = widget_info(Event.Id,/list_select)
	defroi_pickinfo.region_id = r+1
	im = defroi_pickinfo.im0
	picke = defroi_pickinfo.picke
	region = defroi_pickinfo.region_id
	nelem = total(picke eq region)
	if nelem gt 0 then begin

	if defroi_pickinfo.offset ne 0. then im = im - defroi_pickinfo.offset

	defroi_refresh,im
	defroi_listregion,im,picke,region_data,region=region
	str = [ '===========================', $
		'Report generated at: ' + systime(0), $
		'===========================', $
                'REGION OF INTEREST : '+strtrim(region,2)]
	if n_elements(region_data) eq 0 then $
	str = [str, $
		 "ERROR: no statistic for region " + string(region), $
		"***************************",'']
	if n_elements(region_data) then begin

	str = [str, $
                'MINIMUM:        '+strtrim(region_data.min,2)+'  I='+strtrim(region_data.min_i,2)+'  J='+strtrim(region_data.min_j,2), $
                'MAXIMUM:        '+strtrim(region_data.max,2)+'  I='+strtrim(region_data.max_i,2)+'  J='+strtrim(region_data.max_j,2), $
                'TOTAL:          '+strtrim(region_data.total,2), $
                'AVERAGE:        '+strtrim(region_data.ave,2), $
                'VARIANCE:       '+strtrim(region_data.var,2), $
                'DEVIATION:      '+strtrim(region_data.dev,2), $
                'OFFSET:         '+strtrim(defroi_pickinfo.offset,2), $
                'NELEM:          '+strtrim(region_data.nelem,2), $
                'N           I           J  IM(I,J)-Offset' $
        ]

		sz = size(im)
		for ij=0,region_data.nelem-1 do begin
		i = region_data.el_list(ij) MOD sz(1)	
		j = region_data.el_list(ij) / sz(1)	
		str = [str, strtrim(ij,2)+string(i)+string(j)+string(im(i,j))]
		end
	end
	WIDGET_CONTROL,defroi_pickinfo.text,SET_VALUE=str,/append
        end
      END
  'DEFROIPICK_DEL': BEGIN
	picke = defroi_pickinfo.picke
	region = defroi_pickinfo.region_id
	polyfill,[0,500,500,0],[0,0,20,20],color=0,/device
	xyouts,1,1,'***Del ROI '+strtrim(region,2)+' ***',/device
	im = defroi_pickinfo.im0
	defroi_listall,im,picke,charsize=defroi_pickinfo.csize,minuslist=-region
	defroi_pickinfo.picke = picke
      END
  'DEFROIPICK_ADD': BEGIN
	picke = defroi_pickinfo.picke
	region = max(picke) + 1
	polyfill,[0,500,500,0],[0,0,20,20],color=0,/device
	xyouts,1,1,'***Add ROI '+strtrim(region,2)+': LMB pick element, RMB stop***',/device
	defroi_pickinfo.region_max = region  
	str = string( defroi_pickinfo.list(1:defroi_pickinfo.region_max))
	WIDGET_CONTROL,defroi_pickinfo.listwid,set_value=str
  	WIDGET_CONTROL, defroi_pickinfo.listwid, SET_LIST_SELECT=defroi_pickinfo.region_max-1
	im = defroi_pickinfo.im0
	defroi_listall,im,picke,charsize=defroi_pickinfo.csize
	; add the new region selection
	defroi_pickinfo.region_id = defroi_pickinfo.region_max
	region = defroi_pickinfo.region_id
;	defroi_pick,im,picke,region=region,/modify
	defroi_pick,im,picke,region=region,modify=defroi_pickinfo.class+'roi.pick'
	defroi_pickinfo.picke = picke
	defroi_listall,im,picke,charsize=defroi_pickinfo.csize
	polyfill,[0,500,500,0],[0,0,20,20],color=0,/device
      END
  'DEFROIPICK_QUERY': BEGIN
	if defroi_pickinfo.help then $
	r = dialog_message("Use Right Mouse Button to stop query.",/info)
	polyfill,[0,500,500,0],[0,0,20,20],color=0,/device
	xyouts,1,1,defroi_pickinfo.cursor_val,/device
	cursor,x,y,/device,/change
	while (!mouse.button ne 4) do begin
	x = !mouse.x
	y = !mouse.y
	im = defroi_pickinfo.im0
	sz = size(im)
	zoom = [float(defroi_pickinfo.wd)/sz(1),float(defroi_pickinfo.ht)/sz(2)]
	i = fix((x - defroi_pickinfo.xl)/zoom(0))
	j = fix((y - defroi_pickinfo.yl)/zoom(1))
	if i lt 0 then i = 0
	if j lt 0 then j = 0
	if i ge sz(1) then i = sz(1) - 1
	if j ge sz(2) then j = sz(2) - 1
	str = '***IMAGE('+strtrim(i,2)+','+strtrim(j,2)+')='+strtrim(im(i,j),2)
	cursor,x,y,/device,/change
	polyfill,[0,500,500,0],[0,0,20,20],color=0,/device
	xyouts,1,1,str,/device,charsize=2
	end
      END
  'DEFROIPICK_DRAWROI': BEGIN
        im = defroi_pickinfo.im0
	sz = size(im)
        defroi_refresh,im
        xw = defroi_pickinfo.wd
        yw = defroi_pickinfo.ht
        xl = defroi_pickinfo.xl
        yl = defroi_pickinfo.yl
	zoom = [float(xw)/sz(1),float(yw)/sz(2)]
        r = defroi(xw,yw,xverts,yverts,x0=xl,y0=yl)
	xverts = float(xverts) /zoom(0)
	yverts = float(yverts) /zoom(1)
	xv = fix(xverts+0.5)
	yv = fix(yverts+0.5)
        arr = polyfillv(xv,yv,sz(1),sz(2))
        picke = defroi_pickinfo.picke
 
        defroi_listall,im,picke,addlist=[arr,defroi_pickinfo.region_id],charsize=defroi_pickinfo.csize
        defroi_pickinfo.picke = picke
      END
  'DEFROIPICK_MOD': BEGIN
	region = defroi_pickinfo.region_id
	polyfill,[0,500,500,0],[0,0,20,20],color=0,/device
	xyouts,1,1,'***Modify ROI '+strtrim(region,2)+': LMB pick element, RMB stop***',/device
	im = defroi_pickinfo.im0
	defroi_listall,im,picke,charsize=defroi_pickinfo.csize
;	defroi_pick,im,picke,region=region,/modify
	defroi_pick,im,picke,region=region,modify=defroi_pickinfo.class+'roi.pick'
	defroi_pickinfo.picke = picke
	defroi_listall,im,picke,charsize=defroi_pickinfo.csize
	polyfill,[0,500,500,0],[0,0,20,20],color=0,/device
      END
  'DEFROIPICK_ZOOMDEL': BEGIN
	polyfill,[0,500,500,0],[0,0,20,20],color=0,/device
	xyouts,1,1,'***ZoomDel: LMB pick element, RMB stop***',/device
	if defroi_pickinfo.help then begin
	str =['ZoomDel:  resize zoom box, toggle selection, delete from ROI', $
		'Zoom Box: Drag LMB - Reposition box',$
		'          Drag RMB - Resize box', $
		'          Click RMB - Accept the box region', $ 
		'Pixel select Mode:', $
		'          LMB - toggle the selection', $
		'          RMB - stop the selection mode']
	r=dialog_message(str,/info)
	end
	im = defroi_pickinfo.im0
	xl = defroi_pickinfo.xl
	yl = defroi_pickinfo.yl
	wd = defroi_pickinfo.wd
	ht = defroi_pickinfo.ht
	picke = defroi_pickinfo.picke
	region = defroi_pickinfo.region_id
	defroi_zoombox,im,xl,yl,wd,ht,zoom_box,picke,oldpicke=picke,region=region,/delete
	defroi_pickinfo.picke = picke
	polyfill,[0,500,500,0],[0,0,20,20],color=0,/device
      END
  'DEFROIPICK_ZOOMADD': BEGIN
	region = defroi_pickinfo.region_id
	polyfill,[0,500,500,0],[0,0,20,20],color=0,/device
	xyouts,1,1,'***ZoomMod/Add '+strtrim(region,2)+': LMB pick element, RMB stop***',/device
	if defroi_pickinfo.help then begin
	str =['ZoomAdd:  resize zoom box, toggle selection, add to ROI', $
		'Zoom Box: Drag LMB - Reposition box',$
		'          Drag RMB - Resize box', $
		'          Click RMB - Accept the box region', $ 
		'Pixel select Mode:', $
		'          LMB - toggle the selection', $
		'          RMB - stop the selection mode']
	r=dialog_message(str,/info)
	end
	im = defroi_pickinfo.im0
	xl = defroi_pickinfo.xl
	yl = defroi_pickinfo.yl
	wd = defroi_pickinfo.wd
	ht = defroi_pickinfo.ht
	picke = defroi_pickinfo.picke
	defroi_zoombox,im,xl,yl,wd,ht,zoom_box,picke,oldpicke=picke,region=defroi_pickinfo.region_id
	defroi_pickinfo.picke = picke
	polyfill,[0,500,500,0],[0,0,20,20],color=0,/device
      END
  'DEFROIPICK_TEXT': BEGIN
      Print, 'Event for DEFROIPICK_TEXT'
      END
  'DEFROIPICK_TEXTSAVE': BEGIN
	filename = dialog_pickfile(filter='*rois.rpt*', $
		path=defroi_pickinfo.path, $
		file = defroi_pickinfo.class+'rois.rpt', $
		get_path=gpath,/write,title='Save rois.rpt')
	if filename eq gpath then begin
	r = dialog_message('Error: the file name end with "rois.rpt" is required',/error)
	return
	end
	found = findfile(filename,count=ct)
	if ct gt 0 then begin
		r = dialog_message(['Overwite the existing file:',filename],/question)
		if r eq 'No' then return  
	end
	WIDGET_CONTROL,defroi_pickinfo.text,GET_VALUE=str
	openw,unit,filename,/get_lun
	for i=0,n_elements(str)-1 do printf,unit,str(i)
	close,unit
      END
  'DEFROIPICK_TEXTPRINT': BEGIN
	WIDGET_CONTROL,defroi_pickinfo.text,GET_VALUE=str 
	openw,unit,'rois.rpt',/get_lun
	for i=0,n_elements(str)-1 do printf,unit,str(i)
	close,unit
	PS_print,'rois.rpt'
      END
  'DEFROIPICK_TEXTCLEAR': BEGIN
	WIDGET_CONTROL,defroi_pickinfo.text,SET_VALUE=''
      END
  'DEFROIPICK_OFFSET': BEGIN
	WIDGET_CONTROL,defroi_pickinfo.offwid,GET_VALUE=str
	defroi_pickinfo.offset = str
      END
  'DEFROIPICK_CSIZE': BEGIN
	WIDGET_CONTROL,defroi_pickinfo.csizewid,GET_VALUE=str
	defroi_pickinfo.csize = str
	print,defroi_pickinfo.csize
      END
  ENDCASE

  WIDGET_CONTROL, defroi_pickinfo.base, SET_UVALUE=defroi_pickinfo 

END



PRO multiroi_pick,im, GROUP=Group,CLASS=Class


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  multiroi_pick = WIDGET_BASE(GROUP_LEADER=Group, $
	title='DEFROI_PICK R1.0 ', $
      ROW=1, $
      MAP=1, $
      UVALUE='multiroi_pick')

  BASE2 = WIDGET_BASE(multiroi_pick, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  DRAW23 = WIDGET_DRAW( BASE3, $
      BUTTON_EVENTS=1, $
  ;    MOTION_EVENTS=1, $
      RETAIN=2, $
      UVALUE='DEFROIPICK_DRAW', $
      XSIZE=500, $
      YSIZE=500)


  BASE4 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE4')

  BUTTON25 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_DONE', $
      VALUE='Done')

  BUTTON24 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_HELP', $
      VALUE='Help...')

  BUTTON24 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_COLOR', $
      VALUE='Color...')

  BUTTON26 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_REFRESH', $
      VALUE='Refresh')

  BUTTON27 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_ALL', $
      VALUE='ShowAll ROIs')

;  BUTTON28 = WIDGET_BUTTON( BASE4, $
;      UVALUE='DEFROIPICK_LISTREGION', $
;      VALUE='ShowPick ROI')

  BASE36 = WIDGET_BASE(BASE4, $
      ROW=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE36')

  LABEL37 = WIDGET_LABEL( BASE36, $
      UVALUE='LABEL37', $
      VALUE='ROI # :')

  roilist = indgen(32) 
  str = string(roilist(1))
  LIST41 = WIDGET_LIST( BASE36,VALUE=str, $
      UVALUE='DEFROIPICK_LIST', $
      YSIZE=5)

  BUTTON39 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_DRAWROI', $
      VALUE='Draw PolyROI')

  BUTTON32 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_MOD', $
      VALUE='Modify ROI')

  BUTTON30 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_ADD', $
      VALUE='Add ROI')

  BUTTON35 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_DEL', $
      VALUE='Del ROI')

  BUTTON33 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_ZOOMADD', $
      VALUE='Zoom_Mod/Add')

  BUTTON34 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_ZOOMDEL', $
      VALUE='Zoom_Del')

  BUTTON31 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_QUERY', $
      VALUE='Query Image')

  BASE5 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE5')

  BASE5_0 = WIDGET_BASE(BASE5, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE5_0')

  offset_field = CW_FIELD( BASE5_0,VALUE=0., $
      ROW=1, FLOAT=1, RETURN_EVENTS= 1, $
      TITLE='Offset Val: ', $
      XSIZE=8, $
      UVALUE='DEFROIPICK_OFFSET')

  csize_field = CW_FIELD( BASE5_0,VALUE=1., $
      ROW=1, FLOAT=1, RETURN_EVENTS= 1, $
      TITLE='Charsize: ', $
      XSIZE=8, $
      UVALUE='DEFROIPICK_CSIZE')


  TEXT6 = WIDGET_TEXT( BASE5,VALUE='', $
      EDITABLE=1, /SCROLL, $
      UVALUE='DEFROIPICK_TEXT', $
      XSIZE=30, $
      YSIZE=20)

  BASE7 = WIDGET_BASE(BASE5, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE7')

  BUTTON8 = WIDGET_BUTTON( BASE7, $
      UVALUE='DEFROIPICK_TEXTSAVE', $
      VALUE='Save As...')

  BUTTON9 = WIDGET_BUTTON( BASE7, $
      UVALUE='DEFROIPICK_TEXTPRINT', $
      VALUE='Print')

  BUTTON10 = WIDGET_BUTTON( BASE7, $
      UVALUE='DEFROIPICK_TEXTCLEAR', $
      VALUE='Clear')

  BASE8 = WIDGET_BASE(BASE5, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE8')

  WIDGET_CONTROL, multiroi_pick, /REALIZE

  ; Get drawable window index

  COMMON DRAW23_Comm, DRAW23_Id
  WIDGET_CONTROL, DRAW23, GET_VALUE=DRAW23_Id

	sz = size(im)
  	cursor_val = '*** Query cursor image value, RMB to stop ***'

defroi_pickinfo = { $
	path : 'ROI'+ !os.file_sep, $
	class : 'ROI'+ !os.file_sep, $
	help : 1, $
	base : multiroi_pick, $
	text : TEXT6, $
	wid : DRAW23_Id, $
	listwid : LIST41, $
	list: roilist, $
	csizewid : csize_field, $
	csize : 1., $
	offwid : offset_field, $
	offset : 0., $
	cursor_val : cursor_val, $
	xl : 80, $
	yl : 80, $
	wd : 340, $
	ht : 340, $
	scale : [340/sz(1),340/sz(2)], $
	region_id : 1, $
	region_max : 1, $
	picke: make_array(n_elements(im),/byte), $
	im0 : im $
	}

	if keyword_set(class) then begin
		 defroi_pickinfo.class = class
		 len = strpos(class,!os.file_sep,/reverse_search)
		 if len ge 0 then $
		 defroi_pickinfo.path = strmid(class,0,len+1)
	end

	defroi_listall,im,picke,charsize=defroi_pickinfo.csize
	defroi_pickinfo.picke = picke
	nregion = max(picke)
	if nregion gt 1 then begin
	defroi_pickinfo.region_max =  nregion 
	str = string( defroi_pickinfo.list(1:defroi_pickinfo.region_max))
	WIDGET_CONTROL,defroi_pickinfo.listwid,set_value=str
	end
  	WIDGET_CONTROL, LIST41, SET_LIST_SELECT=0

  WIDGET_CONTROL, multiroi_pick, SET_UVALUE=defroi_pickinfo 
	
  XMANAGER, 'multiroi_pick', multiroi_pick
END
