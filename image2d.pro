;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************

@colorbar.pro
@saveImage.pro
@PS_open.pro

PRO image2d_normalize_accept,pick_i,image2d_state

	view_option = image2d_state.view_option

	view_option.pick_ref = pick_i
	view_option.fullcolor = 2

	image_array = *image2d_state.image_array
	sz = size(image_array)
	save_seqno = image2d_state.detector-1
        if sz(0)  ne 3 then begin
                st = 'You have to load the scan # in first'
		r = dialog_message(st,/info)
                return
                end
        begin_seqno = 0
        end_seqno = sz(3)-1
        seqno = begin_seqno + pick_i - 1

        if seqno le end_seqno and seqno ge begin_seqno then begin
                image2d_state.detector = seqno + 1
		
		; read in ref image
		image_ref = image_array(*,*,seqno)
		image2d_REPLOT,image2d_state

		rmin = MIN(image_ref)
		rmax = MAX(image_ref)
		view_option.r_k_max = rmax
		view_option.r_k_min = rmin

		*image2d_state.image = image_ref

        endif else begin
                st = [ $
                'No more image for SCAN #' + string(image2d_state.scanno_current), $
                'Total number of images for this scan is ' + $
                string(end_seqno - begin_seqno) ]
		r = dialog_message(st,/info)
		return
        end
	image2d_state.view_option = view_option
	image2d_REPLOT,image2d_state
	image2d_state.detector = save_seqno + 1

END

PRO image2d_normalize_Event, Event
COMMON IMAGE2D_NORM_BLOCK, norm_ids

  WIDGET_CONTROL,Event.top,GET_UVALUE=norm_state,/no_copy
  image2d_state = norm_state.image2d_state

  view_option = image2d_state.view_option
  widget_ids = image2d_state.widget_ids

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'NORM_PICKED': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=i
	WIDGET_CONTROL,norm_ids.norm,SET_VALUE=2
	image2d_normalize_accept, i,image2d_state
	image2d_normalize_setvalue,image2d_state
	   image2d_REPLOT,image2d_state
      END
  'NORM_COLOR_SCHEME': BEGIN
	view_option.fullcolor = Event.value
	if Event.value eq 1 then  $
		WIDGET_CONTROL,norm_ids.userBase,SENSITIVE=1 else $
		WIDGET_CONTROL,norm_ids.userBase,SENSITIVE=0 
	if view_option.fullcolor lt 2 then begin
;	   zmin = view_option.k_min
;	   zmax = view_option.k_max
	   if view_option.fullcolor eq 1 then begin
		zmin = view_option.u_k_min
		zmax = view_option.u_k_max
           WIDGET_CONTROL,image2d_state.widget_ids.z_min,SET_VALUE=zmin
           WIDGET_CONTROL,image2d_state.widget_ids.z_max,SET_VALUE=zmax
	   end
        image2d_state.view_option.fullcolor = view_option.fullcolor
	image2d_normalize_setvalue,image2d_state
	   image2d_REPLOT,image2d_state
	endif else begin
   	   WIDGET_CONTROL,norm_ids.pick,GET_VALUE=i
	   if i gt image2d_state.maxno then begin
	   st = ['Error: Invalid Normalize Against Image Seq # '+strtrim(i,2), $
	      '       Valid Max Image Seq # is '+strtrim(image2d_state.maxno,2)]
	   r = dialog_message(st ,/error)
	   endif else begin
	   image2d_normalize_accept, i,image2d_state
 	   image2d_normalize_setvalue,image2d_state
	   if i ne image2d_state.detector then $
	   image2d_REPLOT,image2d_state
	   end
	end
	norm_state.image2d_state = image2d_state
      END
  'NORM_SLIDER1': BEGIN
	WIDGET_CONTROL,Event.Id,GET_VALUE=zmin
	image2d_state.view_option.u_k_min = zmin
	image2d_state.view_option.fullcolor = 1
        WIDGET_CONTROL,image2d_state.widget_ids.z_min,SET_VALUE=zmin
  	image2d_REPLOT,image2d_state
	norm_state.image2d_state = image2d_state
      END
  'NORM_SLIDER2': BEGIN
	WIDGET_CONTROL,Event.Id,GET_VALUE=zmax
	image2d_state.view_option.u_k_max = zmax
	image2d_state.view_option.fullcolor = 1
        WIDGET_CONTROL,image2d_state.widget_ids.z_max,SET_VALUE=zmax
  	image2d_REPLOT,image2d_state
	norm_state.image2d_state = image2d_state
      END
  'NORM_HELP': BEGIN
	st=['Currently, there are three modes of image color scheme available.', $
	'It defaults to the Auto Scaled scheme. ', $
	'', '    Auto_Scaled - Automatically scaled by the z-max, z-min of the image data.', $
	'', '    User_Scaled - User settable interest range of z-min, z-max.', $
	'', '    Normalized - Value normalized against the value of the selected image #.', $
	'', $
	'If the User Scaled color scheme is selected, then a user can directly modify', $
	'the Zmin and Zmax fields of the main window or through the lower/upper', $
	'bound sliders.', '', $ 
	'If the image sequence # field is entered with a <CR>, it automatically turns on the', $
	'normalized color scheme mode. The entered # will be the reference image #.', $
	'The normalized value of the reference image itself will be a uniform 1.', $
	'', $
	'The Done of color scheme dialog resets the image to AutoScaled mode.', '', $
	'NOTE: if problem with Normalization encountered, a user can change to',$
	'      AutoScale or UserScale mode and then re-select the detector from',$
	'      the detector list to clear the error.','' $
	 ] 
	res=dialog_message(st,/info,title='Help on Image Color Scheme')
	END
  'NORM_CANCEL': BEGIN
	WIDGET_CONTROL,norm_ids.base,/DESTROY
  	image2d_state.widget_ids.norm_base = 0L
        image2d_state.view_option.fullcolor = 0
	norm_ids.base = 0L 
	norm_ids.userbase = 0L 
	norm_ids.lb1 = 0L 
	norm_ids.lb2 = 0L 
	norm_ids.lb3 = 0L 
	norm_ids.lb4 = 0L 
	norm_ids.lb5 = 0L 
	norm_ids.lb6 = 0L 
	norm_ids.sldr1 = 0L 
	norm_ids.sldr2 = 0L 
	norm_ids.norm = 0L 
	norm_ids.pick = 0L 
	image2d_REPLOT,image2d_state
	image2d_renew,widget_ids.base
	return
      END
  ENDCASE

  widget_control,widget_ids.base,get_uvalue=temp_state,/no_copy
  temp_state.view_option.fullcolor = image2d_state.view_option.fullcolor 
  widget_control,widget_ids.base,set_uvalue=temp_state,/no_copy
  WIDGET_CONTROL,norm_state.base,SET_UVALUE=norm_state,/no_copy
END

PRO image2d_renew,wid
  widget_control,wid,get_uvalue=image2d_state,/no_copy
  widget_ids = image2d_state.widget_ids
  	image2d_state.widget_ids.norm_base = 0L
        image2d_state.view_option.fullcolor = 0
  xarr = *image2d_state.xarr
  yarr = *image2d_state.yarr
  	WIDGET_CONTROL, widget_ids.x_min, SET_VALUE=0
  	WIDGET_CONTROL, widget_ids.y_min, SET_VALUE=0
  	WIDGET_CONTROL, widget_ids.x_max, SET_VALUE=image2d_state.width-1
  	WIDGET_CONTROL, widget_ids.y_max, SET_VALUE=image2d_state.height-1
	image2d_REPLOT,image2d_state
	; set initial xl,xr,yl,yr
  	WIDGET_CONTROL, widget_ids.x1WID, SET_VALUE=xarr(0)
  	WIDGET_CONTROL, widget_ids.y1WID, SET_VALUE=yarr(0)
  	WIDGET_CONTROL, widget_ids.x2WID, SET_VALUE=xarr(image2d_state.width-1)
  	WIDGET_CONTROL, widget_ids.y2WID, SET_VALUE=yarr(image2d_state.height-1)
  widget_control,wid,set_uvalue=image2d_state,/no_copy
END

PRO image2d_normalize_setvalue,image2d_state
COMMON IMAGE2D_NORM_BLOCK, norm_ids

   view_option = image2d_state.view_option

      mode = 'Normalized Against ...'
   if view_option.fullcolor eq 0 then mode = 'Auto Scaled'
   if view_option.fullcolor eq 1 then mode = 'User Scaled'

      st1='Auto Scaled (Max  Value) :' + string(view_option.z_max)
      st2='Auto Scaled (Min  Value) :' + string(view_option.z_min)
      st3='User Scaled (Upper Bound) :' ; + string(view_option.u_k_max)
      st4='User Scaled (Lower Bound) :' ; + string(view_option.u_k_min)
      st5='Ref Image # '+strtrim(view_option.pick_ref,2) + $
		' (Max Value) :' + string(view_option.r_k_max)
      st6='Ref Image # '+strtrim(view_option.pick_ref,2) + $
		' (Min Value) :' + string(view_option.r_k_min)

	WIDGET_CONTROL,norm_ids.lb1,SET_VALUE=st1
	WIDGET_CONTROL,norm_ids.lb2,SET_VALUE=st2
	WIDGET_CONTROL,norm_ids.lb3,SET_VALUE=st3
	WIDGET_CONTROL,norm_ids.lb4,SET_VALUE=st4
	WIDGET_CONTROL,norm_ids.lb5,SET_VALUE=st5
	WIDGET_CONTROL,norm_ids.lb6,SET_VALUE=st6

END


PRO image2d_normalize, GROUP=Group,image2d_state,title=title
COMMON IMAGE2D_NORM_BLOCK, norm_ids

if n_params() lt 1 then return 
if image2d_state.view_option.user eq 0 then begin
	st = ['"Image Color Scheme" is available only if','the droplist Pixel is set to the "By User" mode']
	r = dialog_message(st,/info)
	return
end
if XRegistered('image2d_normalize') then return 

  view_option = image2d_state.view_option

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  d_title = 'Image Color Scheme'
  if keyword_set(title) then d_title = d_title +' ('+title+')'

  image2d_normalize = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, UNAME='image2d_normalize',$
      TITLE=d_title, $
      UVALUE='image2d_normalize')

  BASE2 = WIDGET_BASE(image2d_normalize, $
      COL=1, $
      MAP=1, $
      UVALUE='BASE2')

  color_schemes = [ $
	'AutoScaled', $
	'UserScaled', $
	'Normalized' $
	]
  NORM_COLOR_SCHEME = CW_BGROUP(BASE2, color_schemes, $
	ROW=1, /EXCLUSIVE, /NO_RELEASE, $
	UVALUE='NORM_COLOR_SCHEME')

  norm_min = WIDGET_LABEL( BASE2, $
      UVALUE='NORM_MIN', /ALIGN_LEFT,$
      VALUE='Auto Scaled (Min Value) :' + string(view_option.z_min))

  norm_max = WIDGET_LABEL( BASE2, $
      UVALUE='NORM_MAX', /ALIGN_LEFT,$
      VALUE='Auto Scaled (Max  value) :' + string(view_option.z_max))

  BASE5 = WIDGET_BASE( BASE2, COL=1)   ;/FRAME)
  BASE5_1 = WIDGET_BASE( BASE5, ROW=1)   ;/FRAME)
  BASE5_2 = WIDGET_BASE( BASE5, ROW=1)   ;/FRAME)
  norm_lower = WIDGET_LABEL( BASE5_1, $
      UVALUE='NORM_LOWER', /ALIGN_LEFT,$
      VALUE='User Scaled (Lower Bound) :') ;+ string(view_option.u_k_min))
  zmax = view_option.z_max
  zmin = view_option.z_min
  if zmax eq zmin then begin 
	zmin = 0
	zmax = 255
  end
;if view_option.z_max gt view_option.z_min then $
  sldr1 = CW_FSLIDER(BASE5_1,MAX=zmax,MIN=zmin, $
	value=view_option.z_min,UVALUE='NORM_SLIDER1')

  norm_upper = WIDGET_LABEL( BASE5_2, $
      UVALUE='NORM_UPPER', /ALIGN_LEFT,$
      VALUE='User Scaled (Upper Bound) :') ;+ string(view_option.u_k_max))
;if view_option.z_max gt view_option.z_min then $
  sldr2 = CW_FSLIDER(BASE5_2,MAX=zmax,MIN=zmin, $
	value=view_option.z_max,UVALUE='NORM_SLIDER2')

  BASE3 = WIDGET_BASE( BASE2, COLUMN=1)  ;/FRAME)
  norm_picked = CW_FIELD( BASE3,VALUE=view_option.pick_ref, $
      ROW=1, $
      INTEGER=1, /return_events, $  
      TITLE='Normalize Against Image (Seq+15) #:', XSIZE=2, $
      UVALUE='NORM_PICKED')

  norm_ref_lower = WIDGET_LABEL( BASE3, $
      UVALUE='NORM_LOWER', /ALIGN_LEFT,$
      VALUE='Reference (Min Value) :' + string(view_option.r_k_min))

  norm_ref_upper = WIDGET_LABEL( BASE3, $
      UVALUE='NORM_UPPER', /ALIGN_LEFT,$
      VALUE='Reference (Max Value)) :' + string(view_option.r_k_max))

  BASE4 = WIDGET_BASE( BASE2, ROW=1)

  NORM_HELP = WIDGET_BUTTON( BASE4, $
      UVALUE='NORM_HELP', $
      VALUE='Help...')

  NORM_CANCEL = WIDGET_BUTTON( BASE4, $
      UVALUE='NORM_CANCEL', $
      VALUE='Done')

  image2d_state.widget_ids.norm_base = image2d_normalize
  image2d_state.view_option = view_option

  norm_ids = { $
	base : image2d_normalize, $
	userBase : BASE5, $		; user scaled WID
	lb1 : norm_max, $		; auto max WID
	lb2 : norm_min, $		; auto min WID
	lb3 : norm_upper, $		; user max WID
	lb4 : norm_lower, $		; user min WID
	lb5 : norm_ref_upper, $		; ref upper WID
	lb6 : norm_ref_lower, $		; ref lower WID
	sldr1: sldr1, $
	sldr2: sldr2, $
	norm : norm_color_scheme, $	; color scheme WID
	pick : norm_picked $              ; norm detector WID
	}

	image2d_state.norm_ids.base = norm_ids.base
	image2d_state.norm_ids.userBase = norm_ids.userBase
	image2d_state.norm_ids.lb1 = norm_ids.lb1
	image2d_state.norm_ids.lb2 = norm_ids.lb2
	image2d_state.norm_ids.lb3 = norm_ids.lb3
	image2d_state.norm_ids.lb4 = norm_ids.lb4
	image2d_state.norm_ids.lb5 = norm_ids.lb5
	image2d_state.norm_ids.lb6 = norm_ids.lb6
	image2d_state.norm_ids.sldr1 = norm_ids.sldr1
	image2d_state.norm_ids.sldr2 = norm_ids.sldr2
	image2d_state.norm_ids.norm = norm_ids.norm
	image2d_state.norm_ids.pick = norm_ids.pick

  WIDGET_CONTROL, norm_ids.norm, SET_VALUE=0
  if view_option.fullcolor eq 1 then $
  	WIDGET_CONTROL, norm_ids.userBase, SENSITIVE=1 else $
  	WIDGET_CONTROL, norm_ids.userBase, SENSITIVE=0

  image2d_normalize_setvalue,image2d_state

  norm_state = { base:image2d_normalize, $
	norm_ids: norm_ids, $ 
	image2d_state:image2d_state}

  WIDGET_CONTROL, image2d_normalize, /REALIZE
  WIDGET_CONTROL, image2d_normalize, SET_UVALUE=norm_state,/no_copy

;  return,image2d_normalize
  XMANAGER, 'image2d_normalize', image2d_normalize , /NO_BLOCK
END


PRO image2d_help,Event
st = ['IMAGE2D -  An image array viewing program which allows the user to load 2D', $
  'iamge_array directly into IDL', $
  '','                       *File  Menu*','',$
  'Save Image for Aim  - save 2D image variables accepted by the AIM program', $
  'Save as PNG         - save TV image as IDL PNG file', $
  'Save as TIFF        - save TV image as IDL TIFF file in reverse order', $
  'Save as XDR         - save 2D image, X,Y,Z ranges in XDR format', $
  'Printer...          - dialog to override the default printer', $
  'Print               - send TV plot to PS plotter device', $
  'PS_close            - close the PS plotter device', $
  'Quit                - quit the IMAGE2D program', $
  '','                       *Color Menu*','',$
  'Save Private Color Table - save current color table as private', $
  'Load Private Color Table - load the saved private color table into IDL', $
  'Image Color Scheme...    - dialog for using user adjustable color scheme', $
  'Change Color Table...    - dialog for selecting various IDL color tables', $
  '','                       *Help*', '',$
  'Help...    - gives this help info page', $
  '','                       *View as Menu*', '',$
  'Log Off/On       - color scale in linear of logarithm', $
  'TV               - display 2D data as scaled TV iamge', $
  'Eq.TV.AspRt      - display 2D data with equal X,Y axis aspect ratio', $
  'LIGHT_SHADE_SURF - display 2D data as light shade surface plot', $
  'CONTOUR          - display 2D data as equal contour line plot', $
  'SHOW3            - show data as image, surface, and equal contour plot', $
  'PLOT2D...        - use PLOT2D program to display 2D image data', $
  'SHADE_SURF       - display 2D data as shade surface plot', $
  '','                       *Pixel Menu*','', $
  'By Image         - display actual 2D data image without scaling', $
  'By User          - pixel automatically re-sized by user data (default)', $
  '','                       *Plot vs Menu*','', $
  'Step #           - step # used in X, Y axis plot', $
  'Values           - real values used in X, Y axis plot (default)', $
  '','                       *ASCII Report*','', $
  'ASCII...         - termial window display ascii image data', $
  'Format           - text field controls the ascii data format', $
  '','                       *Image Selection*','', $
  'ReNew            - Refresh TV image as re-selected the 2D image', $
  'D01-D70 List     - Detector D01-D70 selection list', $
  'D1-DF List       - Detector D1-DF selection list', $
  '','                       *Drawing Area Events*','', $
  'Lelf Mouse Button   - button 1 (LMB) updates cursor X,Y,Z field values', $
  'Middle Mouse Button - button 2 (MMB) pops up profile lines at the cursor position',$
  'Right Mouse Button  - button 3 (RMB) zoomin box and updates XL,XR,YL,YR fields', $
  '','                       *Set New Scan Ranges*','', $
  'Set New 2D Scan Ranges - accepts XL,XR,YL,YR values and calls the caput process ',$
  '                         to set as the new 2D scan ranges', $
  '','                       *PanImages Menu*','', $
  'PanImages...     - pops up PanImage program', $
  'Calibration...   - pops up calibration program', $
  '','                    *Fitting Menu*','', $
  'Ez_Fit...        - pops up EZ_FIT program with TV image data', $
  '2D Binary        - saves TV image as EZ_FIT 2D binary data ', $
  '','                       *2D-ROI Menu*','', $
  'Help...          - pops up help info for 2D-ROI program', $
  'ROI...           - pops up 2D ROI program',$
  'Type subMenu     - sets the type of ROI defined', $
  '    RectROI      - rectangular ROI',$
  '    FilterROI    - filter ROI (lower and upper bound)',$
  '    PolyROI      - polygon ROI',$
  'AppendRpt...     - dialog append ROI report to old report file', $
  'ReplaceRpt...    - dialog replace an old report file', $
  'ViewRpt...       - dialog display the ROI report file', $
  'RenameRpt...     - dialog to rename ROI report', $
  '','                       *Ranges Text Fields*','', $
  'Xmin             - image starting X index', $
  'Xmax             - image ending X index', $
  'Ymin             - image starting Y index', $
  'Ymax             - image ending Y index', $
  'Zmin             - image Z value lower bound', $
  'Zmax             - image Z value upper bound', $
  '','                       *Info Text*','', $
  'Display image summary information' $
        ]
  xdisplayfile,text=st,Group=Event.top
END



PRO image2d_ROIRpt,image2d_state,Ref=Ref,roifile=roifile,rptfile=rptfile,header=header,comment=comment,append=append
COMMON STATISTIC_2DBLOCK, statistic_2dids

print,'statistic_2dids.back=',statistic_2dids.back

seq = image2d_state.scanno_current

if image2d_state.maxno le 0 then begin
	res=WIDGET_MESSAGE('Error: no image file loaded in')
	return
end

	image_array = *image2d_state.image_array
	xarr = *image2d_state.xarr
	yarr = *image2d_state.yarr
	def = image2d_state.id_def
	nodet = n_elements(def)

	xdim = image2d_state.width 
	ydim = image2d_state.height   ;catch2d_file.y_req_npts

	scanno_2d = seq

;	panimage,da2d,def

; pops up roi images

update:
	header_ass = ''
	comment_ass = ''
	if keyword_set(header) then header_ass=header
	if keyword_set(comment) then comment_ass=comment

	pick = 1

	if keyword_set(ref) then pick=ref	
	if pick gt 0 and pick le nodet then im_ref = image_array(*,*,pick-1) else begin
		res = dialog_message('Invalid reference detector # picked',/error)
		return
		end

	reportname =image2d_state.view_option.rptfile;   'tmproi.rpt'
	if keyword_set(rptfile) then reportname=rptfile

	if image2d_state.view_option.roifile eq '' then begin
	f = dialog_pickfile(path=statistic_2dids.roipath,filter='*roi.xdr*',title='Pick ROI definition File',/READ)
	if f eq '' then return
	found = findfile(f)
	if found(0) eq '' then begin
		res = dialog_message(['Filename:',f, 'not found!'],/info)
		return
	end
	image2d_state.view_option.roifile = f
	end
	; read roi

	xrange=[0,xdim-1]
	yrange=[0,ydim-1]
	filename=image2d_state.view_option.roifile ; 'tmproi.xdr'
	if keyword_set(roifile) then filename=roifile
	if statistic_2dids.back eq 2 then filename=filename+'.poly'
	found = findfile(filename)
	if found(0) eq '' then begin
		res = dialog_message(['ROI filename',filename, 'not found.', $
			'','The whole 2D region is assumed'],/info)
	endif else begin

	if statistic_2dids.back eq 0 then begin
	xdr_open,unit,filename 
	xdr_read,unit,x
	xdr_close,unit
	xrange=fix([x(0)/x(4),x(1)/x(4)])
	yrange=fix([x(2)/x(5),x(3)/x(5)])
	end
	if statistic_2dids.back eq 1 then begin
		lower_b = statistic_2dids.backave
		upper_b = statistic_2dids.backave2
	end
	if statistic_2dids.back eq 2 then begin
		statistic_2dReadPolyROI,statistic_2dids,xverts,yverts,xv,yv,arr
	end
	end

	if xrange(1) ge xdim then xrange(1) = xdim -1
	if yrange(1) ge ydim then yrange(1) = ydim -1

	if statistic_2dids.refresh eq 1 then begin
		xrange= statistic_2dids.xrange
		yrange= statistic_2dids.yrange
	end

	if keyword_set(append) then $
        openw,1,reportname,ERROR=err,/append else $
        openw,1,reportname,ERROR=err
        IF (err NE 0) then begin
		PRINTF, -2, !ERR_STRING
		close,1
		return
	end
	printf,1,'===================================================='
	printf,1,'Generated at: ',systime(0)
        printf,1,'Header: ',header_ass
        printf,1,'Comment: ',comment_ass
	if statistic_2dids.refresh eq 0 then $
	printf,1,'ROIfile: ',filename else $
	printf,1,'ROIfile: [Temporary ROI specified]'
	printf,1,''

	for i=0,nodet-1 do begin
	if def(i) then begin
	;
        printf,1,'Detector #: ', i+1
		im = image_array(*,*,i)
		if keyword_set(ref) then im = image_array(*,*,i)/im_ref	
		if statistic_2dids.back eq 2 then begin
			nelem = n_elements(arr)
		if nelem eq 0 then begin
		r = dialog_message('You have to define the POLYGON ROI first',/info)
		return
		end
			temp = make_array(nelem)
			for ij=0,nelem-1 do begin
			j = arr(ij) / xdim
			k = arr(ij) MOD xdim
			temp(ij) = im(k,j)
			end
		endif else begin
		nelem = (xrange(1)-xrange(0)+1)*(yrange(1)-yrange(0)+1)
		temp = im[xrange(0):xrange(1),yrange(0):yrange(1)]
		end
		result = moment(temp,mdev=mdev,sdev=sdev)
		temp_max = max(temp)
		temp_min = min(temp)
		total = total(temp)
		ave = result[0]

	; write  report
	if statistic_2dids.back eq 2 then begin
		printf,1,'ROI defined by polygon'
		printf,1,'Xverts index:',xv
		printf,1,'Yverts index:',yv
	endif else begin
        printf,1,'ROI in index: [',strtrim(xrange(0),2),':', $
                strtrim(xrange(1),2),', ', $
                strtrim(yrange(0),2),':', $
                strtrim(yrange(1),2),'] '
        printf,1,'ROI in values: [',strtrim(xarr(xrange(0)),2),':', $
                strtrim(xarr(xrange(1)),2),', ', $
                strtrim(yarr(yrange(0)),2),':', $
                strtrim(yarr(yrange(1)),2),'] '
	end

        printf,1,'ave = ',ave
        printf,1,'dev = ',sdev
        printf,1,'min = ',temp_min
        printf,1,'max = ',temp_max
        printf,1,'total = ',total
        printf,1,'nelem = ',nelem
        printf,1,''
	end
	end
        close,1

	xdisplayfile,reportname
END




PRO PDMENU99_ROI_event, Event, image2d_state
COMMON STATISTIC_2DBLOCK, statistic_2dids

	view_option = image2d_state.view_option
	widget_ids = image2d_state.widget_ids

	image_array = *image2d_state.image_array
	xarr = *image2d_state.xarr
	yarr = *image2d_state.yarr
	x = xarr(0:image2d_state.width-1)
	y = yarr(0:image2d_state.height-1)
	im=image_array(*,*,image2d_state.detector-1)

	
if n_elements(im) eq 0  then begin
	r = dialog_message(['No image data found','','You have to load the 2D data first'],/info)
	return
end

	header=[ image2d_state.path+!os.file_sep+image2d_state.name, $ 
	       ',  Detector # '+ strtrim(image2d_state.detector,2) + '  ('+ $
		image2d_state.DPVS(image2d_state.detector-1)+')']
	comment=''
	if view_option.fullcolor eq 2 then $
	comment='Normalized against detecor '+strtrim(view_option.pick_ref,2)

	if XRegistered('scan2d_ROI')  then  $
		WIDGET_CONTROL,statistic_2dids.base,/DESTROY,BAD=bad
	mode=0
	if n_elements(statistic_2dids) then begin
	  mode = statistic_2dids.back
	  if image2d_state.view_option.roifile ne statistic_2dids.file then $
	  image2d_state.view_option.roifile = statistic_2dids.file
	  if image2d_state.view_option.rptfile ne statistic_2dids.rpt then $
	  image2d_state.view_option.rptfile = statistic_2dids.rpt
	end

	if image2d_state.view_option.rptfile eq '' then $
		image2d_state.view_option.rptfile = image2d_state.home+!os.file_sep+'ROI'+!os.file_sep+$
			image2d_state.name+'_roi.rpt'
	if image2d_state.view_option.roifile eq '' then $
		image2d_state.view_option.roifile = image2d_state.home+!os.file_sep+'ROI'+!os.file_sep+$
			image2d_state.name+'_roi.xdr'

	if n_elements(statistic_2dids) eq 0 then $
	scan2d_roi,im,x,y,GROUP=Event.Top,header=header,comment=comment, $
		mode=mode,rptfile=image2d_state.view_option.rptfile, $
		roifile=image2d_state.view_option.roifile,roi_data=statistic_2dids


  CASE Event.Value OF
  '2D-ROI.Type.RectROI': BEGIN
	statistic_2dids.back = 0
	return
	END
  '2D-ROI.Type.FilterROI': BEGIN
	statistic_2dids.back = 1
	return
	END
  '2D-ROI.Type.PolyROI': BEGIN
	statistic_2dids.back = 2
	return
	END

  '2D-ROI.AppendRpt...': BEGIN
;	image2d_state.view_option.rptfile = f
	if statistic_2dids.comment ne '' then comment=statistic_2dids.comment
	if image2d_state.view_option.fullcolor eq 2 then $
	image2d_ROIRpt,image2d_state, $
		header=header, comment=statistic_2dids.comment, $
		Ref=image2d_state.view_option.pick_ref, /append else $
	image2d_ROIRpt,image2d_state, $
		header=header, comment=statistic_2dids.comment, /append
	END
  '2D-ROI.ReplaceRpt...': BEGIN
	F = image2d_state.view_option.rptfile
;	f = dialog_pickfile(path=statistic_2dids.rptpath,filter='*rpt*',title='Replace ROI Rpt File',/READ)
	if f eq '' then return
	found = findfile(f)
	if found(0) eq '' then begin
		res = dialog_message(['Filename:',f, 'not found will be created!'],/info)
	endif else begin
	st = ['Are you sure you want to overwrite this file ?', $
		'If you enter Yes, then all the old text contents', $
		'in this file will be lost.', $
		'','Replacing ',F , ' ???']
	res = dialog_message(st,/question)
	if res eq 'No' then return
	end

;	image2d_state.view_option.rptfile = f
	if statistic_2dids.comment ne '' then comment=statistic_2dids.comment
	if image2d_state.view_option.fullcolor eq 2 then $
	image2d_ROIRpt,image2d_state, $
		header=header, comment=statistic_2dids.comment, $
		Ref=image2d_state.view_option.pick_ref else $
	image2d_ROIRpt,image2d_state, $
		header=header, comment=statistic_2dids.comment
	
        END
  '2D-ROI.ViewRpt...': BEGIN
	f = dialog_pickfile(path=statistic_2dids.rptpath,filter='*rpt*',title='View ROI Rpt File',/READ)
	if f eq '' then return
	found = findfile(f)
	if found(0) eq '' then begin
		res = dialog_message(['Filename:',f, 'not found!'],/info)
		return
	end
;	image2d_state.view_option.rptfile = f
	xdisplayfile,f
        END
  '2D-ROI.RenameRpt...': BEGIN
	old = image2d_state.view_option.rptfile
	rename_dialog,image2d_state.home+!os.file_sep+'ROI',old,'',GROUP=Event.top
        END
  '2D-ROI.ROI...': BEGIN
	scan2d_roi,im,x,y,GROUP=Event.Top,header=header,comment=comment, $
		mode=mode,rptfile=image2d_state.view_option.rptfile, $
		roifile=image2d_state.view_option.roifile,roi_data=statistic_2dids
        END
  '2D-ROI.Help...': BEGIN
	st = [ $
		'In general the 2D ROI reports generated by 2D-ROI menu',$
		'in Image2d consist of all detectors defined in a given 2D scan.', $
		'',$
		'           Options of 2D-ROI Menu', '', $
		'Help...           - Show this help info ', $
		'ROI...            - Pops up  2D Statistic ROI program', $
		'Type->RectROI     - Set the type of ROI used in the summary report', $
		'       FilterROI', $
		'        PolyROI', $
		'AppendRpt...      - Append 2D statistic summary report of all', $
		'                       detectors to the report file', $
		'                       for a given 2D scan ROI',$
		'ReplaceRpt...     - Overwrite 2D statistic report file', $
		'                       with the summary of all the detectors', $
		'                       with ROI as show in 2D Statistic ROI window',$
		'ViewRpt...        - Select and view any 2D statistic report', $
		'RenameRpt...      - Rename the rpt file to a new name', '',$
		'',$
		'If the detailed 2D ROI reports for a specified detector, or', $
		'refining of the ROI are desired, a user should run the ',$
		'"2D Statistic ROI" program first which can be brought up by ', $
		'','          2D-ROI->ROI...   ', '', $
		'The AppendRep... button in "2D Statistic ROI" window generates ', $
		'detail report for the displayed image.', '', $
		'For file management simplicity the 2D ROI statistic report ', $
		'should end with roi.rpt and the region of interest file ',$
		'for rectangle or polygon ROI should end with roi.xdr ' $
		]
	xdisplayfile,text=st,title='Image2d Help on 2D-ROI'

	END
  ENDCASE

END


PRO image2d_datatotext,image2d_state
COMMON IMAGE2D_NORM_BLOCK, norm_ids

	widget_ids = image2d_state.widget_ids
	view_option = image2d_state.view_option
	image_array = *image2d_state.image_array
	xarr = *image2d_state.xarr
	yarr = *image2d_state.yarr

	if view_option.user then begin 
	widget_control,widget_ids.x_min,get_value=x1
	widget_control,widget_ids.x_max,get_value=x2
	widget_control,widget_ids.y_min,get_value=y1
	widget_control,widget_ids.y_max,get_value=y2
	image2d_state.view_option.x_min = x1
	image2d_state.view_option.y_min = y1
	image2d_state.view_option.x_max = x2
	image2d_state.view_option.y_max = y2
	  image = image_array(x1:x2,y1:y2,image2d_state.detector-1)
 	  px = xarr(x1:x2)
	  py = yarr(y1:y2)
	endif else begin
	image = image_array(*,*,image2d_state.detector-1)
	  px =xarr
	  py =yarr
	end

	;reset for user range

;norm_ids = image2d_state.norm_ids
if XRegistered('image2d_normalize') then begin
  if norm_ids.norm then begin
  	widget_control,norm_ids.norm,get_value=n1
  	widget_control,norm_ids.pick,get_value=n2
	image2d_state.view_option.fullcolor = n1
	image2d_state.view_option.pick_ref = n2
  endif else begin
	image2d_state.norm_ids.userBase = 0L
	image2d_state.norm_ids.lb1 = 0L
	image2d_state.norm_ids.lb2 = 0L
	image2d_state.norm_ids.lb3 = 0L
	image2d_state.norm_ids.lb4 = 0L
	image2d_state.norm_ids.lb5 = 0L
	image2d_state.norm_ids.lb6 = 0L
	image2d_state.norm_ids.norm = 0L
	image2d_state.norm_ids.pick = 0L
	image2d_state.widget_ids.norm_base = 0L
	image2d_state.view_option.fullcolor = 0
	image2d_state.view_option.pick_ref = 1 
  end
end
;print,'fullcolor=',image2d_state.view_option.fullcolor,'  pick_ref=',image2d_state.view_option.pick_ref

	if image2d_state.view_option.fullcolor eq 2 then begin
		image_ref = *image2d_state.image
	 	image = image/ image_ref(x1:x2,y1:y2)	
	end
		
	sz = size(image)
	image2d_state.width = sz(1)
	image2d_state.height = sz(2)

       t_format = 'G18.8'
        if keyword_set(format) then t_format = format
        fwidth = 'I'+strmid(t_format,1,strpos(t_format,'.')-1)

        dir = image2d_state.outpath+'ASCII'+!os.file_sep
        found = findfile(dir,count=ct)
        if ct lt 1 then spawn,!os.mkdir + ' ' +dir

	detector = image2d_state.detector
        suf0 = '00'
        suf = strtrim(detector,2)
        ln = strlen(suf)
        strput,suf0,suf,2-ln
        file = image2d_state.name +'.im'+suf0
        report = file

        if keyword_set(outfile) then report = strtrim(outfile,2)

        ; check for write permission error
 ;       catch,status_error
 ;       if status_error ne 0 then begin
 ;       lp = strpos(file,!os.file_sep,/reverse_search)
 ;       if lp gt 0 then class = strmid(file,lp+1,strlen(file)-lp-1)
 ;       report = dir+class
 ;       end

        openw,fw,dir+report,/get_lun

        s = size(image)
        dim = s(1:2)
        printf,fw,'; From:',image2d_state.name ,',   Detector = ',image2d_state.DPVS(detector-1) ;strtrim(detector,2)

        printf,fw,';   data('+strtrim(dim(0),2)+','+strtrim(dim(1),2)+')'
        printf,fw,'; -------------------------------'

        f0 = '(";              (yvalues)",'+ '5000('+t_format+',:))'
        if n_elements(py) gt 0 then printf,fw,format=f0,py
        if n_elements(py) gt 0 then begin
                f1 = '('+t_format+',I,'+strtrim(dim(1),2)+'('+t_format+'))'
                f0 = '(";                   \ Y",'+strtrim(dim(1),2)+fwidth+',/,";                  X \",/,";      (xvalues)")'
                endif else begin
                f0 = '(";    \ Y",'+strtrim(dim(1),2)+fwidth+',/,";   X \",/)'
                f1 = '(I,'+strtrim(dim(1),2)+'('+t_format+'))'
                end
        printf,fw,format=f0,indgen(dim(1))
        newdata = transpose(image)
        d1 = dim(1)
        d2 = dim(0)
        temp = make_array(dim(1))
        for j=0,d2-1 do begin

       temp = newdata(0:d1-1,j)
        if n_elements(px) gt 0 then printf,fw,format=f1,px(j),j,temp else $
                printf,fw,format=f1,j,temp
        end
        free_lun,fw

        if keyword_set(nowin) then return
        xdisplayfile,dir+report,group=group
END

PRO show_cross,x,y,d_id,s_id
if n_params() lt 4 then begin
	print,'Usage: show_cross,x,y,d_wid,s_wid
	print,'       x, y - specify cross hair coordinate
	print,'       d_win  - specify tv image window
	print,'       s_win  - saved virtual image window        
	return
	end
CATCH,error_status
if error_status eq -324 then begin
	print,!err_string
	print,'Invalid window id : ', s_id
	return
	end
	WSET,s_id
	width = !d.x_size
	height = !d.y_size
	WSET,d_id
	xa = [0,width-1]
	ya = [y,y]
	plots,xa,ya,/device
	xa = [x,x]
	ya = [0,height-1]
	plots,xa,ya,/device
END

PRO hide_cross,x,y,d_id,s_id
if n_params() lt 4 then begin
	print,'Usage: hide_cros,x,y,d_wid,s_wid
	print,'       x, y - specify cross hair coordinate
	print,'       d_win  - specify tv image window
	print,'       s_win  - saved virtual image window        
	return
	end
CATCH,error_status
if error_status eq -324 then begin
	print,!err_string
	print,'Invalid window id : ', s_id
	return
	end
	WSET,s_id
	width = !d.x_size
	height = !d.y_size
	WSET,d_id
if x ge 0 and x lt width then $
 	device,copy=[x,0,1,height,x,0,s_id]
if y ge 0 and y lt height then $
 	device,copy=[0,y,width,1,0,y,s_id]
END

PRO update_pixmap,wid
	o_wid = !d.window
	if !d.n_colors eq 16777216 then	channel=1 else channel=0
	data = TVRD(TRUE=channel)
	WSET,wid
	TV,data,TRUE=channel
	WSET,o_wid
END

PRO create_pixmap,wid,data=data,xp=xp,yp=yp,width=width,height=height
if n_params() lt 1 then begin
	print,'Usage: create_pixmap,wid 
	print,'       output - wid , saved virtual image window id
	print,'       keyword - xp,yp, width,height
	print,'Save the whole TV window to a new virtual window
	print,'         if keyword is used all four of them must be specified  
	return
	end


	if !d.n_colors eq 16777216 then	data = TVRD(TRUE=1) else $
	data = TVRD(TRUE=0)

	if keyword_set(xp) and keyword_set(yp) and keyword_set(width) $
		 and keyword_set(height) then begin
		if !d.n_colors eq 16777216 then $ 
		newdata = data(0:2, xp:xp+width-1, yp:yp+height-1) else $
		newdata = data(xp:xp+width-1, yp:yp+height-1)
		data = newdata
		end

	ss = size(data)
	if ss(0) eq 2 then begin
		xs = ss(1)
		ys = ss(2)
		channel = 0
		end
	if ss(0) eq 3 and ss(1) eq 3 then begin
		xs = ss(2)
		ys = ss(3)
		channel = 1
		end
	if !d.n_colors eq 16777216 then	$
	print,'CREATE PIXMAP: Array(3,',strtrim(xs,2),',',strtrim(ys,2),')'  else $
	print,'CREATE PIXMAP: Array(',strtrim(xs,2),',',strtrim(ys,2),')'

	window,/free,/pixmap, xsize=xs, ysize=ys
	wid= !d.window
	TV,data,TRUE=channel

END


PRO image2d_REPLOT,image2d_state
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON COLORBAR, colorbar_data
COMMON PRINTER_BLOCK,printer_info
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
COMMON IMAGE2D_NORM_BLOCK, norm_ids

 if n_elements(colorbar_data) eq 0 then colorbar_init,colorbar_data

	widget_ids = image2d_state.widget_ids
	view_option = image2d_state.view_option
	image_array = *image2d_state.image_array
	px = *image2d_state.xarr
	py = *image2d_state.yarr

if XRegistered('image2d_normalize') then begin
  if norm_ids.norm gt 0L then begin
  	widget_control,norm_ids.norm,get_value=n1
  	widget_control,norm_ids.pick,get_value=n2
	image2d_state.view_option.fullcolor = n1
	image2d_state.view_option.pick_ref = n2
  endif else begin
	image2d_state.view_option.fullcolor = 0
	image2d_state.view_option.pick_ref = 16 ;1 
  end
endif else begin
	image2d_state.view_option.fullcolor = 0
	image2d_state.view_option.pick_ref = 16 ;1 
end
;print,'fullcolor=',image2d_state.view_option.fullcolor,'  pick_ref=',image2d_state.view_option.pick_ref

	if view_option.user then begin 
	widget_control,widget_ids.x_min,get_value=x1
	widget_control,widget_ids.x_max,get_value=x2
	widget_control,widget_ids.y_min,get_value=y1
	widget_control,widget_ids.y_max,get_value=y2
	image2d_state.view_option.x_min = x1
	image2d_state.view_option.y_min = y1
	image2d_state.view_option.x_max = x2
	image2d_state.view_option.y_max = y2
	  image = image_array(x1:x2,y1:y2,image2d_state.detector-1)
	  xarr = px(x1:x2) 
	  yarr = py(y1:y2) 
	endif else begin 
	image = image_array(*,*,image2d_state.detector-1)
	  xarr = px
	  yarr = py
	end
	sz = size(image)
	
	widget_control,widget_ids.x1WID,set_value=xarr(0)
	widget_control,widget_ids.x2WID,set_value=xarr(sz(1)-1)
	widget_control,widget_ids.y1WID,set_value=yarr(0)
	widget_control,widget_ids.y2WID,set_value=yarr(sz(2)-1)

	image2d_state.view_option.z_min = min(image,imin)
	image2d_state.view_option.i_min = imin mod sz(1)
	image2d_state.view_option.j_min = imin / sz(1)
	image2d_state.view_option.z_max = max(image,imax)
	image2d_state.view_option.i_max = imax mod sz(1)
	image2d_state.view_option.j_max = imax / sz(1)

if image2d_state.scanno_current lt 0 then return

if !d.name eq 'WIN' or !d.n_colors gt !d.table_size then device,decomposed=0

; update the info block

image2d_state.z_desc = image2d_state.zdescs(image2d_state.detector-1)

xtitle='2D SCAN # '+strtrim(image2d_state.scanno_current,2)+ $
;	',  D'+strtrim(image2d_state.detector,2)+ $
	', '+image2d_state.x_desc +  $
	', FILE:'+image2d_state.name + $ 
	',  USER:'+strupcase(getenv('USER')) 

        ytitle = image2d_state.y_desc

        header_note1='MAX: ' + strtrim(image2d_state.view_option.z_max,2) + ' @ ('+ $
                strtrim(xarr(image2d_state.view_option.i_max),2) + ', ' + $
                strtrim(yarr(image2d_state.view_option.j_max),2) + ')'

        header_note='MIN: ' + strtrim(image2d_state.view_option.z_min,2) + ' @ ('+ $
                strtrim(xarr(image2d_state.view_option.i_min),2) + ', ' + $
                strtrim(yarr(image2d_state.view_option.j_min),2) + ')'

num_images = fix(total(image2d_state.id_def gt 0))
str = ['Selected Image File        : '+ image2d_state.name] 
str = [str,'Total Number of Images     : '+ string(num_images)]
	if image2d_state.scanno_current eq 0 then begin
	str = [ str,'Extracted 2D Array '+ $
		',   DETECTOR='+image2d_state.DPVS(image2d_state.detector-1) + $
	', ('+image2d_state.x_desc +', '+ image2d_state.y_desc+', '+ $
		image2d_state.z_desc + ')']
	endif else begin
	str = [ str,'2D SCAN # ='+strtrim(image2d_state.scanno_current)+ $
		',   DETECTOR='+image2d_state.DPVS(image2d_state.detector-1) + $
	', ('+image2d_state.x_desc +', '+ image2d_state.y_desc+', '+ $
		image2d_state.z_desc + ')']
	end
	str = [str, '1D scan #=(0-'$
		+ strtrim(image2d_state.height-1,2)+ ')'+$
		',   width='+strtrim(image2d_state.width,2)+ $
		',   height='+strtrim(image2d_state.height,2) ] 
	str = [str, 'x_pv = '+image2d_state.x_pv+',   y_pv = '+image2d_state.y_pv]
	WIDGET_CONTROL, widget_ids.info, SET_VALUE= str

	str = 'MIN:'+strtrim(image2d_state.view_option.z_min,2) + ' @ (' + $
		strtrim(image2d_state.view_option.i_min,2) + ',' + $
		strtrim(image2d_state.view_option.j_min,2) + ')'
	WIDGET_CONTROL, widget_ids.zmin, SET_VALUE= str
	str = 'MAX:'+strtrim(image2d_state.view_option.z_max,2) + ' @ (' + $
		strtrim(image2d_state.view_option.i_max,2) + ',' + $
		strtrim(image2d_state.view_option.j_max,2) + ')'
	WIDGET_CONTROL, widget_ids.zmax, SET_VALUE= str

if image2d_state.view_option.fullcolor eq 0 then begin
	WIDGET_CONTROL,widget_ids.z_min,SET_VALUE=strtrim(image2d_state.view_option.z_min,2)
	WIDGET_CONTROL,widget_ids.z_max,SET_VALUE=strtrim(image2d_state.view_option.z_max,2)
end
if image2d_state.view_option.fullcolor eq 1 then begin
	WIDGET_CONTROL,widget_ids.z_min,GET_VALUE=v_min
	WIDGET_CONTROL,widget_ids.z_max,GET_VALUE=v_max
	image2d_state.view_option.u_k_max = v_max
	image2d_state.view_option.u_k_min = v_min
end

; check for user range

		xdim = sz(1)
		ydim = sz(2)
		x_min=0
		x_max=xdim-1
		y_min=0
		y_max=ydim-1
	if view_option.fullcolor eq 2 then begin
	   image_ref = image_array(*,*,image2d_state.view_option.pick_ref-1)
		image_ref = float(image_ref)
		ij = where(image_ref eq 0.)
		if ij(0) ge 0 then begin
		ii = ij mod xdim
		jj = ij / xdim
		image_ref(ii(0),jj(0)) = 1.e-3
		end
	   *image2d_state.image = image_ref
	   image = image/ image_ref(x1:x2,y1:y2)	
;print,max(image_ref),min(image_ref)
;print,max(image),min(image)
	end
		

	if image2d_state.view_option.user eq 1 then begin
		newimage = image(x_min:x_max,y_min:y_max)
	endif else begin
		newimage = image
		end
*image2d_state.image = newimage
;      
; set plot area for 2D view
;

if !d.name eq OS_SYSTEM.device then WSET,widget_ids.plot2d_area

		x = xarr ;(0:image2d_state.width - 1)
		y = yarr ;(0:image2d_state.height - 1)
		ix = n_elements(x)
		iy = n_elements(y)

;  draw 2D data as data image
   if image2d_state.view_option.user eq 0 then begin
	erase 
	; expand data to drawing area
		image2d_state.x_mag = 1
		image2d_state.y_mag = 1
		newimage2 = newimage

	v_max = max(newimage)
	v_min = min(newimage)
	ncolors = image2d_state.view_option.ncolors

	
	if v_max eq v_min then begin       ;(all same value)
;		dv = v_max - v_min
;		if dv eq 0 then fact = ncolors  else fact = ncolors / dv
		fact=ncolors
		TV,newimage2*fact
	endif else begin
		newimage2 = bytscl(newimage2,top=ncolors,min=v_min,max=v_max)
		TV,newimage2
	end
	return
   end

if image2d_state.view_option.z_min ne image2d_state.view_option.z_max then $
   shades = (image-image2d_state.view_option.z_min)/(image2d_state.view_option.z_max-image2d_state.view_option.z_min)*image2d_state.view_option.ncolors else $
   shades = image*image2d_state.view_option.ncolors

   title=image2d_state.DPVS(image2d_state.detector-1)+' - '+image2d_state.z_desc

	CASE image2d_state.view_option.surface OF
	2: begin    ; light shaded
		if image2d_state.view_option.versus then begin
			if x_max lt ix then ix=x_max
			if y_max lt iy then iy=y_max 
			newim = image(x_min:ix,y_min:iy)
			nx=x(x_min:ix)
			ny=y(y_min:iy)
			SHADE_SURF, newim,nx,ny 
		endif else SHADE_SURF, newimage
	   end
	6: begin
		if image2d_state.view_option.versus then begin
			if x_max lt ix then ix=x_max
			if y_max lt iy then iy=y_max 
			newim = image(x_min:ix,y_min:iy)
			nx=x(x_min:ix)
			ny=y(y_min:iy)
			SHADE_SURF, newim,nx,ny ,shades=shades 
		endif else SHADE_SURF, newimage, shades=shades
	   end
	3: begin
			if x_max lt ix then ix=x_max
			if y_max lt iy then iy=y_max 
			newim = image(x_min:ix-1,y_min:iy-1)
			nc = image2d_state.view_option.ncolors
			labels=[1,1,1,1,1,1,1,1,1,1,1]
			zmax = max(newim)
			zmin = min(newim)
			dz= (zmax-zmin)/ 9.
		if dz eq 0. then begin
		erase
		return
		end
			dc = nc / 10 
			colors = nc
			levels = zmin
			for i=1,9 do begin
			levels = [levels, zmin + dz*i]
			colors = [colors,nc -dc*i]
			end
		if !d.n_colors gt !d.table_size then begin
		colors = lonarr(n_elements(labels))
		getLineColors,colors
		end

		if image2d_state.view_option.versus then begin       ; versus values
			nx=x(x_min:ix-1)
			ny=y(y_min:iy-1)
		endif else begin       			; versus step # 
			temp = indgen(ix)
			nx=temp(x_min:ix-1)
			temp = indgen(iy)
			ny=temp(y_min:iy-1)
		end
		CONTOUR, newim,nx,ny, $
			levels = levels, $
			c_colors=reverse(colors), c_labels=labels, $
			 c_charsize=1.5,/follow
	   end
	4: begin
		if image2d_state.view_option.versus then begin
			if x_max lt ix then ix=x_max
			if y_max lt iy then iy=y_max 
			newim = image(x_min:ix-1,y_min:iy-1)
			nx=x(x_min:ix-1)
			ny=y(y_min:iy-1)
			SHOW3, newim , nx, ny
		endif else SHOW3, newimage
;		SHOW3, newimage, sscale=2
	   end
	5: begin
		PLOT2D, newimage,id, $
			xarr=xarr,yarr=yarr, $
                        comment=[header_note1,header_note], $
			wtitle='IMAGE2D(Plot2d)', $
                        xtitle=xtitle, ytitle=ytitle, $
                        title=title
;		XSURFACE, newimage, id
		if n_elements(id) eq 0 then begin
		r = dialog_message(['Error: First close the old XSurface window',$
			'       then select TV before select new image and XSURFACE'], /info)
		
		return
		endif else $
		widget_ids.xsurface = id
	   end
	0: begin
	   erase
		; expand data to drawing area
		image2d_state.x_mag = 1
		image2d_state.y_mag = 1

	 	    xratio = 1.
		    yratio = float(y_max-y_min+1)/(x_max-x_min+1) 
		    if yratio gt 1. then begin
			xratio = 1. / yratio
			yratio = 1.
		    end 
		width = (x_max - x_min + 1) / xratio
		height = (y_max - y_min + 1)/ yratio

		if image2d_state.view_option.user eq 1 and !d.name eq OS_SYSTEM.device then begin
		width = !d.x_size - image2d_state.view_option.margin_l - image2d_state.view_option.margin_r
		height = !d.y_size - image2d_state.view_option.margin_t - image2d_state.view_option.margin_b
		image2d_state.x_mag = float(width)/(x_max-x_min +1)
		image2d_state.y_mag = float(height)/(y_max-y_min +1)
		end
; help,image,newimage,width,height

		newimage2 = CONGRID(newimage,width,height)
		ncolors = image2d_state.view_option.ncolors

	if view_option.fullcolor eq 0 then begin
		v_max = max(newimage2)
		v_min = min(newimage2)
	end
	if view_option.fullcolor eq 1 then begin
		v_max = image2d_state.view_option.u_k_max
		v_min = image2d_state.view_option.u_k_min
	end

	if view_option.fullcolor eq 2 then begin

		v_max = max(newimage2)
		v_min = min(newimage2)
	WIDGET_CONTROL,widget_ids.z_min,SET_VALUE=strtrim(v_min,2)
	WIDGET_CONTROL,widget_ids.z_max,SET_VALUE=strtrim(v_max,2)
	end

;		if !d.name ne OS_SYSTEM.device then ncolors = ncolors - 1

		if v_max eq v_min then begin       ;(all same value)
			dv = v_max - v_min
			if dv eq 0 then fact = ncolors  else fact = ncolors / dv
			newimage2 = newimage2*fact
		endif else begin
		if image2d_state.view_option.fullcolor lt 2 then $
		newimage2 = bytscl(newimage2,top=ncolors,min=v_min,max=v_max) 
		if image2d_state.view_option.fullcolor eq 2 then $
		newimage2 = bytscl(newimage2,top=ncolors,min=v_min,max=v_max)
		end

		xrange = [x_min,x_max]
		yrange = [y_min,y_max]
		title = ' vs X,Y Step #'
		if image2d_state.view_option.versus then begin
		xrange = [ xarr(x_min), xarr(x_max)]
		yrange = [ yarr(y_min), yarr(y_max)]
		title = ' vs X,Y Values'
		end

log = 0
off = 0.
if v_max gt v_min then begin
if image2d_state.LOG then begin
	if v_min lt 0 then begin
	; log scale is not good for v_man < 0
	newimage2 = newimage-v_min
	off = v_min
	v_max = v_max-v_min
	v_min= 0.;.01*v_max
	end
	log = 1
	newimage2 = alog10(newimage2)
	if v_min ge 0 then begin
	v_max = alog10(v_max)
	if v_min eq 0 then begin
		if v_max gt 1. then v_min = .01*v_max else $
		v_min = -2
	endif else v_min = alog10(v_min)
	end
end
end

		; for PS  get aspect ratio, outward tick marks 

	; draw headers

        header_note1='MAX: ' + strtrim(image2d_state.view_option.z_max,2) + ' @ ('+ $
		strtrim(xarr(image2d_state.view_option.i_max),2) + ', ' + $
		strtrim(yarr(image2d_state.view_option.j_max),2) + ')' 

        header_note='MIN: ' + strtrim(image2d_state.view_option.z_min,2) + ' @ ('+ $
		strtrim(xarr(image2d_state.view_option.i_min),2) + ', ' + $
		strtrim(yarr(image2d_state.view_option.j_min),2) + ')' 

	if !d.name ne 'PS' then begin
          xdis = 0.01 * !d.x_size
          ydis = !d.y_size - 1.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note1,/device
          ydis = !d.y_size - 2.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note,/device
	endif else begin
	  if printer_info.reverse then t_color = ncolors-1 else t_color = 0
          xdis = 0.001 * !d.x_size
          ydis = !d.y_size - 1.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note1,/device, color=t_color
          ydis = !d.y_size - 2.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note,/device, color=t_color
	end

	if strtrim(image2d_state.z_desc,2) ne '' then $
	title = image2d_state.z_desc + ' - ' + title else $
	title = image2d_state.DPVS(image2d_state.detector-1) + title

		if !d.name eq 'PS' then begin
		    xo = !d.x_size * image2d_state.view_option.ps_l
		    yo = !d.y_size * image2d_state.view_option.ps_b
		    xw = !d.x_size * (image2d_state.view_option.ps_r - image2d_state.view_option.ps_l)
		    yw = !d.y_size * (image2d_state.view_option.ps_t - image2d_state.view_option.ps_b)

		    pos = [image2d_state.view_option.ps_l, image2d_state.view_option.ps_b, $
			image2d_state.view_option.ps_r, image2d_state.view_option.ps_t]

		    if v_max eq v_min then $
		    TV,newimage2,xo,yo,xsize=xw,ysize=yw,/nan else $
		    TVSCL,newimage2,xo,yo,xsize=xw,ysize=yw,/nan


		    plot,/noerase,/nodata, pos=pos, [-1,-1], $
			xrange=xrange, yrange=yrange, $
			xticklen= -!p.ticklen, yticklen=-!p.ticklen, $
			title=title, xtitle=xtitle, $
			ytitle=ytitle, $
			xstyle = 1, ystyle=1 ,color=t_color

		endif else begin
		if v_max eq v_min then $
		    TV,newimage2, image2d_state.view_option.margin_l, image2d_state.view_option.margin_b,/nan else $
		    TVSCL,newimage2, image2d_state.view_option.margin_l, image2d_state.view_option.margin_b,/nan

		    p1 = [float(image2d_state.view_option.margin_l)/ !d.x_size, $
			float(image2d_state.view_option.margin_b)/!d.y_size, $
			float(!d.x_size - image2d_state.view_option.margin_r) / !d.x_size, $
			float(!d.y_size - image2d_state.view_option.margin_t) / !d.y_size $
			]

		    plot,/noerase,/nodata, pos=p1 ,[-1,-1], $
			xrange=xrange, yrange=yrange, $
			xticklen= -!p.ticklen, yticklen=-!p.ticklen, $
			xtitle= image2d_state.x_desc, $
			ytitle=ytitle, $
			title=title, xstyle = 1, ystyle=1

		end

		if !d.name eq 'PS' then colorbar,[v_min,v_max],y=50,x=390,LOG=LOG,off=off  else $
		colorbar,[v_min,v_max],colorbar_data.width, $
			colorbar_data.height, $
			horizontal=colorbar_data.horiz, $
			x=colorbar_data.x, y=10, $
			reverse=printer_info.reverse, $
			LOG=LOG,off=off, $
			ncap=colorbar_data.nlabel,format=colorbar_data.format

                ; save pixmap
                if !d.name ne OS_SYSTEM.device then return
                image2d_state.view_option.d_wid = !d.window
                if image2d_state.view_option.s_wid ge 0 then begin
                        wid = image2d_state.view_option.s_wid
                        update_pixmap,wid
                endif else begin
                        create_pixmap,wid
                        image2d_state.view_option.s_wid = wid
                end

	   end
	1: begin
	   erase
		; equal aspect ratio  
		image2d_state.x_mag = 1
		image2d_state.y_mag = 1
		width = x_max - x_min + 1
		height = y_max - y_min + 1

		xrange = [ xarr(x_min), xarr(x_max)]
		yrange = [ yarr(y_min), yarr(y_max)]
	 	    xratio = 1.
		    yratio = float(yrange(1)-yrange(0))/(xrange(1)-xrange(0)) 
		    if yratio gt 1. then begin
			xratio = 1. / yratio
			yratio = 1.
		    end 

		if image2d_state.view_option.user eq 1 and !d.name eq OS_SYSTEM.device then begin
		width = !d.x_size - image2d_state.view_option.margin_l - image2d_state.view_option.margin_r
		height = !d.y_size - image2d_state.view_option.margin_t - image2d_state.view_option.margin_b
		image2d_state.x_mag = floor(width/(x_max-x_min + 1))
		image2d_state.y_mag = floor(height/(y_max-y_min + 1))
		width = width * xratio
		height = height * yratio
		end

		newimage2 = CONGRID(newimage,width,height)

		v_max = max(newimage2)
		v_min = min(newimage2)
		ncolors = image2d_state.view_option.ncolors

		if v_max eq v_min then begin       ;(all same value)
			dv = v_max - v_min
			if dv eq 0 then fact = ncolors  else fact = ncolors / dv
			newimage2 = newimage2*fact
		endif else begin
		newimage2 = bytscl(newimage2,top=ncolors,min=v_min,max=v_max)
		end

		xrange = [x_min,x_max]
		yrange = [y_min,y_max]
		title = 'vs X,Y Step #'
		if image2d_state.view_option.versus then begin
		xrange = [ xarr(x_min), xarr(x_max)]
		yrange = [ yarr(y_min), yarr(y_max)]
		title = 'vs X,Y Values'
		end


log = 0
off = 0.
if v_max gt v_min then begin
if image2d_state.LOG then begin
	if v_min lt 0 then begin
	; log scale is not good for v_man < 0
	newimage2 = newimage-v_min
	off = v_min
	v_max = v_max-v_min
	v_min= 0.;.01*v_max
	end
	log = 1
	newimage2 = alog10(newimage2)
	if v_min ge 0 then begin
	v_max = alog10(v_max)
	if v_min eq 0 then begin
		if v_max gt 1. then v_min = .01*v_max else $
		v_min = -2
	endif else v_min = alog10(v_min)
	end
end
end
		; for PS  get aspect ratio, outward tick marks 

	; draw headers

        header_note1='MAX: ' + strtrim(image2d_state.view_option.z_max,2) + ' @ ('+ $
		strtrim(xarr(image2d_state.view_option.i_max),2) + ', ' + $
		strtrim(yarr(image2d_state.view_option.j_max),2) + ')' 

        header_note='MIN: ' + strtrim(image2d_state.view_option.z_min,2) + ' @ ('+ $
		strtrim(xarr(image2d_state.view_option.i_min),2) + ', ' + $
		strtrim(yarr(image2d_state.view_option.j_min),2) + ')' 

	if !d.name ne 'PS' then begin
          xdis = 0.01 * !d.x_size
          ydis = !d.y_size - 1.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note1,/device
          ydis = !d.y_size - 2.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note,/device
	endif else begin
	  if printer_info.reverse then t_color = ncolors-1 else t_color = 0
          xdis = 0.001 * !d.x_size
          ydis = !d.y_size - 1.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note1,/device,color=t_color
          ydis = !d.y_size - 2.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note,/device,color=t_color
	end

	ytitle = image2d_state.y_desc
	if strtrim(image2d_state.z_desc,2) ne '' then $
	title = image2d_state.z_desc + ' - ' + title else $
	title = image2d_state.DPVS(image2d_state.detector-1) +' ' +title

		if !d.name eq 'PS' then begin
		    xo = !d.x_size * image2d_state.view_option.ps_l
		    yo = !d.y_size * image2d_state.view_option.ps_b
		    xw = !d.x_size * xratio *(image2d_state.view_option.ps_r - image2d_state.view_option.ps_l)
		    yw = !d.y_size * yratio *(image2d_state.view_option.ps_t - image2d_state.view_option.ps_b)

		    if v_max eq v_min then $
		    TV,newimage2,xo,yo,xsize=xw,ysize=yw,/nan else $
		    TVSCL,newimage2,xo,yo,xsize=xw,ysize=yw,/nan

		    pos = [image2d_state.view_option.ps_l, image2d_state.view_option.ps_b, $
			xw / !d.x_size + image2d_state.view_option.ps_l,  $
			yw / !d.y_size + image2d_state.view_option.ps_b ]

		    plot,/noerase,/nodata, pos=pos, [-1,-1], $
			xrange=xrange, yrange=yrange, $
			xticklen= -!p.ticklen, yticklen=-!p.ticklen, $
			title=title, xtitle=xtitle, ytitle=ytitle, $
			xstyle = 1, ystyle=1, color=t_color

		endif else begin

		if v_max eq v_min then $
		    TV,newimage2, image2d_state.view_option.margin_l, image2d_state.view_option.margin_b,/nan else $
		    TVSCL,newimage2, image2d_state.view_option.margin_l, image2d_state.view_option.margin_b,/nan
		    p1 = [float(image2d_state.view_option.margin_l)/ !d.x_size, $
			float(image2d_state.view_option.margin_b)/!d.y_size, $
			(float(image2d_state.view_option.margin_l)+width)/!d.x_size, $
			(float(image2d_state.view_option.margin_b)+height)/!d.y_size $
			]

		    plot,/noerase,/nodata, pos=p1 ,[-1,-1], $
			xrange=xrange, yrange=yrange,title=title, $
			xtitle=xtitle, ytitle=ytitle, $
			xstyle = 1, ystyle=1
		end
		if !d.name eq 'PS' then colorbar,[v_min,v_max],y=100,LOG=LOG,off=off else $
		colorbar,[v_min,v_max],colorbar_data.width, $
			colorbar_data.height, $
			horizontal=colorbar_data.horiz, $
			x=colorbar_data.x, y=10, $
			reverse=printer_info.reverse, $
			LOG=LOG,off=off, $
			ncap=colorbar_data.nlabel,format=colorbar_data.format

	   end
	ELSE: print,'Unknow case entered'
	ENDCASE

END



PRO image2d_init,widget_ids,image2d_state=image2d_state,image_array,xarr,yarr,title=title,outpath=outpath,id_def=id_def,scanno=scanno,pv=pv,xdescs=xdescs,ydescs=ydescs,zdescs=zdescs,seqnm=seqnm,VERS=VERS
COMMON IMAGE2D_NORM_BLOCK, norm_ids

@os.init

  sz = size(image_array)

  if sz(0) ne 3 then return

  view_option = { $
        roifile : '', $
        rptfile : '', $
        format : 'G17.7', $  ; ascii report format
        surface : 0, $    ; 0-TV, 1-SURFACE,2-CONTOUR, 3-SHOW3
        ncolors : !d.table_size-1, $   max color index 
        pick_ref : 16, $   ; normalized against detector # 16
        fullcolor : 0, $   0-auto 1-user 2-normalize
        CA : 0, $   ; realtime set this to 1
        user    : 1, $  ; if 1 use user set range
        versus  : 1, $  ; 0 - step # or 1 - values 
        margin_l : 80, $         ; TV left margin
        margin_r : 80, $         ; TV right margin
        margin_t : 50, $         ; TV top margin
        margin_b : 50, $         ; TV bottom margin
        ps_l : 0.1, $           ; PS left 
        ps_b : 0.125, $         ; PS bottom 
        ps_r : 0.85, $          ; PS right 
        ps_t : 0.875, $         ; PS top 
        pickx : 0, $            ; pick P1 as X axis
        width   : sz(1), $        ; default x set range 
        height  : sz(2), $        ; default y set range
        x_min   : 0, $          ; user set xmin
        x_max   : sz(1)-1, $       ; user set xmax
        y_min   : 0, $          ; user set ymin
        y_max   : sz(2)-1, $       ; user set ymax
        d_wid   : -1, $         ; image window id
        s_wid   : -1, $         ; pixmap window id
        x       : 0, $          ; device cursor x
        y       : 0, $          ; device cursor y
        z       : 0., $
        i_min   : 0, $          ; i index for z_min
        j_min   : 0, $          ; j index for z_min
        i_max   : 0, $          ; i index for z_max
        j_max   : 0, $          ; j index for z_max
        k_min   : 0., $         ; k index for z_min
        k_max   : 0., $         ; k index for z_max
        u_k_min : 0., $         ; z_min for user scale
        u_k_max : 255., $       ; z_max for user scale
        r_k_min : 0., $         ; z_min for norm ref
        r_k_max : 255., $       ; z_max for norm ref
        z_min   : 0., $         ; z_min for TV image 
        z_max   : 255. $  ; z_max for TV image 
        }

;  if sz(3) lt 16 then view_option.pick_ref = 1

  ListVal949 = [ 'D1','D2','D3','D4','D5','D6','D7','D8','D9', $
	'DA','DB','DC','DD','DE','DF', $ 
    'D01', 'D02', 'D03', 'D04', 'D05', 'D06', 'D07', 'D08', 'D09', 'D10' ]
	vlist = 'D'+strtrim(indgen(60)+11,2)
	
  if keyword_set(VERS) then ListVal949 = [ListVal949(15:24),vlist] else $
	ListVal949 = [ListVal949,vlist]


	list = indgen(sz(3)) + 1
  if keyword_set(seqnm) then begin
	ListVal949= 'S'+ strtrim(list,2)
	if n_elements(zdescs) gt 1 then ListVal949 = zdescs 
	if n_elements(seqnm) gt 1 then ListVal949 = seqnm

	if sz(3) gt 15 then begin
	widget_control,widget_ids.sel_image,set_value=ListVal949(0:14)
	widget_control,widget_ids.sel_base,set_value=ListVal949(0:sz(3)-1)
;	widget_control,widget_ids.sel_base,set_value=ListVal949(15:sz(3)-1)
	endif else $
	widget_control,widget_ids.sel_image,set_value=ListVal949(0:sz(3)-1)
  end

  norm_ids = { $
	base :0L, $
        userBase : 0L, $             ; user scaled WID
        lb1 : 0L, $               ; auto max WID
        lb2 : 0L, $               ; auto min WID
        lb3 : 0L, $             ; user max WID
        lb4 : 0L, $             ; user min WID
        lb5 : 0L, $         ; ref upper WID
        lb6 : 0L, $         ; ref lower WID
	sldr1 : 0L, $
	sldr2 : 0L, $
       norm : 0L, $     ; color scheme WID
        pick : 0L $              ; norm detector WID
        }

cd,current=p

image2d_state = { $
	widget_ids : widget_ids, $
	view_option : view_option, $
	norm_ids : norm_ids, $
	VERS : VERS, $
	path : p, $
	home : p, $
	outpath : p+!os.file_sep, $
	name : 'image2d', $	; filename
	id_def: make_array(sz(3),/int,value=1), $
	DPVS : ListVal949, $
	x_pv : '', $
	y_pv : '', $
	scanno_current : 0, $
	detector: 16, $
	z_desc : '', $ 
	y_desc : '', $ 
	x_desc : '', $ 
	xdescs : make_array(4,/string), $
	ydescs : make_array(4,/string), $	
	zdescs : make_array(sz(3),/string), $	
	title : '', $
	width : sz(1), $
	height : sz(2), $
	maxno : sz(3), $       ;  85 last detector defined
	start : 0, $           ; panimage start index
	x_mag : 1., $
	y_mag : 1., $
	ID1   : '', $   	; ITOOL identifier
	LOG   : 0, $ 		; LOG scale off/on
	xarr : ptr_new(/allocate_heap), $
	yarr : ptr_new(/allocate_heap), $
	image : ptr_new(/allocate_heap), $    ; ref image
	image_array : ptr_new(/allocate_heap) $
	}


  if sz(3) lt 16 then begin
	image2d_state.detector=1
	view_option.pick_ref = 1
  end
  if n_elements(yarr) lt sz(2) then yarr=yarr(0)+indgen(sz(2))*(yarr(1)-yarr(0))

  *image2d_state.xarr = xarr
  *image2d_state.yarr = yarr
  *image2d_state.image_array = image_array
  *image2d_state.image = image_array(*,*,view_option.pick_ref-1)

  image2d_resetInit,image2d_state

  if keyword_set(title) then begin
	cd,current=p
        lp = strpos(title,!os.file_sep,/reverse_search)
        if lp ge 0 then begin
		class = strmid(title,lp+1,strlen(title)-lp-1)
		path = strmid(title,0,lp)
	endif else begin
		class = title
		path = p 
	end
	image2d_state.name = class
	image2d_state.path = path
  end

  if keyword_set(outpath) then image2d_state.outpath = outpath
  if keyword_set(id_def) then image2d_state.id_def = id_def

  if keyword_set(scanno) then image2d_state.scanno_current = scanno

  if keyword_set(pv) then begin
	image2d_state.x_pv = pv(0)
	image2d_state.y_pv = pv(1)
  end

  if keyword_set(zdescs) then begin
	image2d_state.zdescs = zdescs
	nd = n_elements(zdescs)
	if nd le 15 then image2d_state.detector = nd
	image2d_state.z_desc = zdescs(image2d_state.detector-1)
	if nd gt 15 then begin
	widget_control,widget_ids.sel_image,set_value=image2d_state.DPVS(0:14)
	widget_control,widget_ids.sel_base,set_value=image2d_state.DPVS(15:nd-1)
	endif else begin
	widget_control,widget_ids.sel_image,set_value=image2d_state.DPVS(0:nd-1)
	end
  end

  if keyword_set(ydescs) then begin
	image2d_state.ydescs = ydescs
	image2d_state.y_desc = ydescs(0)
  end
  if keyword_set(xdescs) then begin
	image2d_state.xdescs = xdescs
	image2d_state.x_desc = xdescs(0)
  end
END

PRO image2d_resetInit,image2d_state

  xarr = *image2d_state.xarr
  yarr = *image2d_state.yarr
  image_array = *image2d_state.image_array
  sz = size(image_array)
  image2d_state.view_option.width = sz(1)
  image2d_state.view_option.height = sz(2)
  image2d_state.view_option.x_min = 0
  image2d_state.view_option.x_max = sz(1)-1
  image2d_state.view_option.y_min = 0
  image2d_state.view_option.y_max = sz(2)-1

; set initial xl,xr,yl,yr

  WIDGET_CONTROL, image2d_state.widget_ids.x1WID, SET_VALUE=xarr(0)
  WIDGET_CONTROL, image2d_state.widget_ids.y1WID, SET_VALUE=yarr(0)
  WIDGET_CONTROL, image2d_state.widget_ids.x2WID, SET_VALUE=xarr(image2d_state.width-1)
  WIDGET_CONTROL, image2d_state.widget_ids.y2WID, SET_VALUE=yarr(image2d_state.height-1)

; set initial xmin,xmax,ymin,ymax

  WIDGET_CONTROL, image2d_state.widget_ids.x_min, SET_VALUE= image2d_state.view_option.x_min
  WIDGET_CONTROL, image2d_state.widget_ids.y_min, SET_VALUE= image2d_state.view_option.y_min
  WIDGET_CONTROL, image2d_state.widget_ids.x_max, SET_VALUE= image2d_state.view_option.x_max
  WIDGET_CONTROL, image2d_state.widget_ids.y_max, SET_VALUE= image2d_state.view_option.y_max

END

PRO image2d_toAim,Event
END


PRO PDMENU4_Event, Event, image2d_state
COMMON IMAGE2D_NORM_BLOCK, norm_ids
COMMON COLORBAR, colorbar_data

	ncol = image2d_state.width
	nrow = image2d_state.height
	xarr = *image2d_state.xarr
	yarr = *image2d_state.yarr
	image_array = *image2d_state.image_array
	image = image_array(*,*,image2d_state.detector-1)
	view_option = image2d_state.view_option

  CASE Event.Value OF 

  'File.Open...': BEGIN
;    PRINT, 'Event for File.Open...'
;	r = dialog_pickfile(get_path=p,group=Event.top,/must_exist, $
;		filter='*.mda', $
;		path=image2d_state.path,/read)
;	if r eq '' then return
;	image2d_state.path = p
;	image2d_readParms,image2d_state,filename=r
    END
  'File.Save Image for AIM': BEGIN
	imarr = image
	save,filename='dc2aim.sav',/XDR,ncol,nrow,xarr,yarr,imarr
    END
  'File.Save as JPEG': BEGIN
        win = image2d_state.widget_ids.plot2d_area
	outpath = image2d_state.outpath+'JPEG'+!os.file_sep
	st = strtrim(image2d_state.detector,2)
	if image2d_state.name ne '' then $
	file = image2d_state.name+'_'+st+'.jpg' 
	save_jpg,win=win,file=file,path=outpath
    END
  'File.Save as PNG': BEGIN
        win = image2d_state.widget_ids.plot2d_area
	outpath = image2d_state.outpath+'PNG'+!os.file_sep
	st = strtrim(image2d_state.detector,2)
	if image2d_state.name ne '' then $
	file = image2d_state.name+'_'+st+'.png' 
	save_png,win=win,file=file,path=outpath
    END
  'File.Save as TIFF': BEGIN
        win = image2d_state.widget_ids.plot2d_area
	outpath = image2d_state.outpath+'TIFF'+!os.file_sep
	st = strtrim(image2d_state.detector,2)
	if image2d_state.name ne '' then $
	file = image2d_state.name+'_'+st+'.tiff' 
	save_tiff,win=win,file=file,path=outpath
    END
  'File.Save as XDR': BEGIN
        xmin = xarr(0)
        xmax = xarr(image2d_state.width-1)
        ymin = yarr(0)
        ymax = yarr(image2d_state.height-1)
        ranges = [xmin,xmax,ymin,ymax,view_option.z_min,view_option.z_max]
        xdr_open,unit,'image2d.xdr',/write
        xdr_write,unit,image
        xdr_write,unit,ranges
        xdr_close,unit
        st = image2d_state.DPVS(image2d_state.detector-1)
        outname=image2d_state.name+'_'+st+'.xdr'
        outpath = image2d_state.outpath+'XDR'+!os.file_sep
        rename_dialog,outpath,'image2d.xdr',outname,GROUP=Event.Top
    END
  'File.Printer...': BEGIN
	PS_printer,Group=Event.top
    END
  'File.print': BEGIN
	PS_TVRD,file='image2d.ps',wid=image2d_state.widget_ids.plot2d_area
	return
	wset,image2d_state.widget_ids.plot2d_area
	arr = TVRD()
	sz = size(arr)
	width = 7.7*2.54*sz(1)/700	
	height = 7.7*2.54*sz(2)/700	
	PS_open,'image2d.ps',/TV,xsize=width,ysize=height
;	image2d_REPLOT,image2d_state
	TV,arr
	PS_close
	PS_print,'image2d.ps'
    END
  'File.PS_close': BEGIN
	PS_close
    END
  'File.Quit': BEGIN
	ptr_free,image2d_state.xarr	
	ptr_free,image2d_state.yarr	
	ptr_free,image2d_state.image	
	ptr_free,image2d_state.image_array	
	WIDGET_CONTROL,Event.Top,/DESTROY
	return
    END
  'Color.Save Private Color Table': BEGIN
	TVLCT,red,green,blue,/GET
	save,red,green,blue,file='pvtcolors.dat'
    END
  'Color.Load Private Color Table': BEGIN
	found = findfile('pvtcolors.dat')
	if found(0) eq '' then begin
	str = 'Error: Private color table never been saved before'
	r = dialog_message(str,/error)
	endif else begin
	restore,'pvtcolors.dat'
	TVLCT,red,green,blue
	end
    END
  'Color.ColorBar Config...': BEGIN
        WSET,image2d_state.widget_ids.plot2d_area
        colorbar_config,GROUP=Event.top
    END
  'Color.Color in Log Scale': BEGIN
	image2d_state.LOG = 1
	image2d_REPLOT,image2d_state
	image2d_state.LOG=0
    END
  'Color.Change Color Table...': BEGIN
	XLOADCT,Group=Event.Top
    END
  'Color.Image Color Scheme...': BEGIN
	image2d_normalize,Group=Event.top,image2d_state,title=image2d_state.name
	image2d_state.widget_ids.norm_base = norm_ids.base
    END
  'Help.Help...': BEGIN
	image2d_help,Event
    END
  ENDCASE
END

PRO image2d_killnotify,WID
  WIDGET_CONTROL,WID,GET_UVALUE=image2d_state,/NO_COPY
	ptr_free,image2d_state.xarr	
	ptr_free,image2d_state.yarr	
	ptr_free,image2d_state.image	
	ptr_free,image2d_state.image_array	
  WIDGET_CONTROL,WID,SET_UVALUE=image2d_state,/NO_COPY
END

PRO PDMENU93_Event, Event, image2d_state


   xarr = *image2d_state.xarr
   yarr = *image2d_state.yarr
   image_array = *image2d_state.image_array
   id_def = image2d_state.id_def
   title='Scan # '+ strtrim(image2d_state.scanno_current,2)
	x = xarr(0:image2d_state.width-1)
	y = yarr(0:image2d_state.height-1)
	image = image_array(*,*,image2d_state.detector-1)

  detnm = image2d_state.zdescs
  maxno = image2d_state.maxno

  ; only first 85 images accepted by the panimage program

  sz = size(image_array)

  CASE Event.Value OF 
  'PanImages.PanImage Subsets.First...': BEGIN
	p1 = 0
	p2 = 85
	if maxno lt p2 then p2 = maxno
	id_def = id_def(p1:p2-1)
	title = title + ' ('+detnm(p1)+' : '+detnm(p2-1)+')'
	detnm = detnm(p1:p2-1)
	image_array = image_array(*,*,p1:p2-1)
	image2d_state.start = p2 
	panimage_sel,image_array,id_def,title=title,detnm=detnm,group=Event.top,VERS=image2d_state.VERS
    END
  'PanImages.PanImage Subsets.Next...': BEGIN
	p1 = image2d_state.start
	if maxno gt p1 then begin
	p2 = p1+85
  	if maxno lt p2 then p2 = maxno
	id_def = id_def(p1:p2-1)
	title = title + ' ('+detnm(p1)+' : '+detnm(p2-1)+')'
	detnm = detnm(p1:p2-1)
	image_array = image_array(*,*,p1:p2-1)
	image2d_state.start = p2
	panimage_sel,image_array,id_def,title=title,detnm=detnm,group=Event.top,VERS=image2d_state.VERS
	end
    END
  'PanImages.PanImage Subsets.Prev...': BEGIN
	p2 = image2d_state.start
	p1 = p2-85
	if p1 lt 0 then p1=0
	id_def = id_def(p1:p2-1)
	title = title + ' ('+detnm(p1)+' : '+detnm(p2-1)+')'
	detnm = detnm(p1:p2-1)
	image_array = image_array(*,*,p1:p2-1)
	image2d_state.start = p1
	panimage_sel,image_array,id_def,title=title,detnm=detnm,group=Event.top,VERS=image2d_state.VERS
    END
  'PanImages.PanImage Subsets.Last...': BEGIN
	p2 = maxno
	p1 = p2-85
  	if p1 lt 0 then p1 = 0
	id_def = id_def(p1:p2-1)
	title = title + ' ('+detnm(p1)+' : '+detnm(p2-1)+')'
	detnm = detnm(p1:p2-1)
	image_array = image_array(*,*,p1:p2-1)
	image2d_state.start = p1
	panimage_sel,image_array,id_def,title=title,detnm=detnm,group=Event.top,VERS=image2d_state.VERS
    END
  'PanImages.PanImage Subsets.Mid-point...': BEGIN
	p1 = (image2d_state.start+maxno)/2
	p2 = p1+85
	if maxno lt p2 then p2 = maxno
	id_def = id_def(p1:p2-1)
	title = title + ' ('+detnm(p1)+' : '+detnm(p2-1)+')'
	detnm = detnm(p1:p2-1)
	image_array = image_array(*,*,p1:p2-1)
	image2d_state.start = p1 
	panimage_sel,image_array,id_def,title=title,detnm=detnm,group=Event.top,VERS=image2d_state.VERS
    END
  'PanImages.PanImages...': BEGIN
	detnm = image2d_state.DPVS
	panimage_sel,image_array,id_def,title=title,group=Event.top,detnm=detnm,VERS=image2d_state.VERS
    END
  'PanImages.Calibration...': BEGIN
	calibration_factor,image_array,id_def,xv=x,yv=y,title=title,VERS=image2d_state.VERS
    END
  'Fitting.Ez_Fit...': BEGIN
	ez_fit,xarray=x,yarray=y,im=image,Group=Event.top
    END
  'Fitting.2D Binary': BEGIN
        u_openw,unit,'fitting.bin',/XDR
        u_write,unit,x
        u_write,unit,y
        u_write,unit,image
        u_close,unit
        st = '2D binary data save in "fitting.bin"'
        xdisplayfile,text=st
    END
  ENDCASE
END




PRO IMAGE2D_BASE_Event, Event

  WIDGET_CONTROL,Event.Top,GET_UVALUE=image2d_state,/NO_COPY
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev
if n_elements(image2d_state.view_option) eq 0 then begin
	widget_control,Event.top,/destroy,bad_id=bad
	return
end
  view_option = image2d_state.view_option
  widget_ids = image2d_state.widget_ids
  image_array = *image2d_state.image_array
  image = image_array(*,*,image2d_state.detector-1)
  xarr = *image2d_state.xarr
  yarr = *image2d_state.yarr
  id_def = image2d_state.id_def
  nx = n_elements(xarr) 
  ny = n_elements(yarr) 

;if image2d_state.view_option.fullcolor eq 2 then image = *image2d_state.image

  CASE Ev OF 

;  'IMAGE2D_XAXIS': BEGIN
;	image2d_state.view_option.pickx = Event.index
;print,image2d_state.view_option.pickx
;      END
  'IMAGE2D_DRAW': BEGIN
	wset,widget_ids.plot2d_area

	if Event.Press eq 1 then begin
	cursor,x,y,/device

	if x ge view_option.margin_l and x lt (!d.x_size-view_option.margin_r) and  $
	  y ge view_option.margin_b and y lt (!d.y_size-view_option.margin_t) then begin
	  Ix = fix( float(x-view_option.margin_l) / image2d_state.x_mag)
	  Iy = fix( float(y-view_option.margin_b) / image2d_state.y_mag)
	   if Ix ge 0 and Ix lt nx and ( Ix ge 0 and Iy lt ny) then begin
	  x0 = xarr(Ix)
	  y0 = yarr(Iy)
	  z = image(Ix,Iy)
	  x0 = xarr(Ix+image2d_state.view_option.x_min)
	  y0 = yarr(Iy+image2d_state.view_option.y_min)
	  z = image(Ix+image2d_state.view_option.x_min,Iy+image2d_state.view_option.y_min)
	  WIDGET_CONTROL,widget_ids.x_cursor,SET_VALUE=x0
	  WIDGET_CONTROL,widget_ids.y_cursor,SET_VALUE=y0
	  WIDGET_CONTROL,widget_ids.z_cursor,SET_VALUE='Z: '+string(z) + $
		' @ (' + strtrim(Ix,2) +',' + strtrim(Iy,2) + ')'
	   end
	  end
	end
	if Event.Press eq 2 then begin
	cursor,x,y,/device

	if x ge view_option.margin_l and x lt (!d.x_size-view_option.margin_r) and  $
	  y ge view_option.margin_b and y lt (!d.y_size-view_option.margin_t) then begin
	  Ix = fix( float(x-view_option.margin_l) / image2d_state.x_mag)
	  Iy = fix( float(y-view_option.margin_b) / image2d_state.y_mag)
	  Zy = image(*,Iy)
	  plot1d,xarr,Zy,/data,title='Zx Profile @ Y='+strtrim(yarr(Iy),2), $
		xtitle='X',ytitle='Z', $
		group=Event.top
	  Zx = transpose(image(Ix,*))
	  plot1d,yarr,Zx,/data,title='Zy Profile @ X='+strtrim(xarr(Ix),2), $
		xtitle='Y',ytitle='Z', $
		group=Event.top
	  end
	end

	if Event.Press eq 4 then begin
;	hide_cross,view_option.x,view_option.y,view_option.d_wid,view_option.s_wid
        nx = 50
        ny = 50
        x0 = Event.x - 25
        y0 = Event.y - 25

        my_box_cursor, x0, y0, nx,ny, /INIT


	if x0 lt view_option.margin_l then x0 = view_option.margin_l
	if x0 gt (!d.x_size-view_option.margin_r) then x0 = !d.x_size-view_option.margin_r-nx
	if y0 lt view_option.margin_B THen y0 = view_option.margin_b
	if y0 gt (!d.y_size-view_option.margin_t) then y0 = !d.y_size-view_option.margin_t-ny

        if view_option.user eq 1 then begin
                x_min = float(x0-view_option.margin_l) / $
                        image2d_state.x_mag + view_option.x_min
		if x_min lt 0 then x_min = 0
        x_min = fix(x_min)
                x_max = float(x0+nx-view_option.margin_l) / $
                        image2d_state.x_mag + view_option.x_min
        x_max = ceil(x_max)
                y_min = float(y0-view_option.margin_b) / $
                        image2d_state.y_mag + view_option.y_min
        y_min = fix(y_min)
                y_max = float(y0+ny-view_option.margin_b) / $
                        image2d_state.y_mag + view_option.y_min
        y_max = ceil(y_max)

        if x_max gt view_option.x_max then x_max = view_option.x_max; - 1
        if y_max gt view_option.y_max then y_max = view_option.y_max; - 1
	if y_max eq y_min then begin
		if y_min gt 0 then y_min = y_max-1 else y_max = y_min+1
	end
	if x_max eq x_min then begin
		if x_min gt 0 then x_min = x_max-1 else x_max = x_min+1
	end

	if x_max lt x_min then begin
	temp = x_min
	x_min = x_max
	x_max = temp
	end
	if y_max lt y_min then begin
	temp = y_min
	y_min = y_max
	y_max = temp
	end


openw,1,'box.txt'
printf,1,'lower_left',xarr(x_min),yarr(y_min)
printf,1,'upper_right',xarr(x_max),yarr(y_max)
close,1

; update start/end range
        WIDGET_CONTROL, widget_ids.x1WID, SET_VALUE= xarr(x_min)
        WIDGET_CONTROL, widget_ids.x2WID, SET_VALUE= xarr(x_max)
        WIDGET_CONTROL, widget_ids.y1WID, SET_VALUE= yarr(y_min)
        WIDGET_CONTROL, widget_ids.y2WID, SET_VALUE= yarr(y_max)

        WIDGET_CONTROL,widget_ids.plot_wid,/CLEAR_EVENTS
                WIDGET_CONTROL,widget_ids.x_min, $
                        SET_VALUE = strtrim( fix(x_min),2)
                WIDGET_CONTROL,widget_ids.x_max, $
                        SET_VALUE = strtrim( ceil(x_max),2)
                WIDGET_CONTROL,widget_ids.y_min, $
                        SET_VALUE = strtrim( fix(y_min),2)
                WIDGET_CONTROL,widget_ids.y_max, $
                        SET_VALUE = strtrim( ceil(y_max),2)

                view_option.x_min = fix(x_min)
                view_option.y_min = fix(y_min)
                view_option.x_max = ceil(x_max)
                view_option.y_max = ceil(y_max)
                image2d_REPLOT,image2d_state
        end
	end
      END
  ; Event for PDMENU4
  'PDMENU4': PDMENU4_Event, Event, image2d_state
  'IMAGE2D_LOGCOLOR': BEGIN
	image2d_state.LOG = Event.Index
	image2d_REPLOT,image2d_state
      END
  'IMAGE2D_VIEW': BEGIN
	image2d_state.view_option.surface = Event.Index
	image2d_REPLOT,image2d_state
      END
  'IMAGE2D_PIXEL': BEGIN
       image2d_state.view_option.user = Event.Index
        CASE view_option.user OF
        0: begin
		image2d_REPLOT,image2d_state
        end
        1: begin
                WIDGET_CONTROL,widget_ids.x_min,SET_VALUE=view_option.x_min
                WIDGET_CONTROL,widget_ids.x_max,SET_VALUE=view_option.x_max
                WIDGET_CONTROL,widget_ids.y_min,SET_VALUE=view_option.y_min
                WIDGET_CONTROL,widget_ids.y_max,SET_VALUE=view_option.y_max
		image2d_REPLOT,image2d_state
        end
        ELSE:
        ENDCASE
      END
  'IMAGE2D_PLOTVS': BEGIN
	image2d_state.view_option.versus = Event.Index
        image2d_REPLOT,image2d_state
      END
  'IMAGE2D_DONE': BEGIN
	ptr_free,image2d_state.xarr	
	ptr_free,image2d_state.yarr	
	ptr_free,image2d_state.image	
	ptr_free,image2d_state.image_array	
	WIDGET_CONTROL,Event.Top,/DESTROY
	return
    END
  'IMAGE2D_ASCII': BEGIN
        image2d_datatotext,image2d_state
      END
  'IMAGE2D_FORMAT': BEGIN
        WIDGET_CONTROL,Event.id,Get_VALUE=f
        image2d_state.view_option.format = f(0)
        image2d_datatotext,image2d_state
      END
  'IMAGE2D_ITOOL': BEGIN
	tvlct,r,g,b,/get
	rgb = reform([r,g,b],256,3)
	px = *image2d_state.xarr
	py = *image2d_state.yarr
	if view_option.user then begin 
	widget_control,widget_ids.x_min,get_value=x1
	widget_control,widget_ids.x_max,get_value=x2
	widget_control,widget_ids.y_min,get_value=y1
	widget_control,widget_ids.y_max,get_value=y2
	image2d_state.view_option.x_min = x1
	image2d_state.view_option.y_min = y1
	image2d_state.view_option.x_max = x2
	image2d_state.view_option.y_max = y2
	  image = image_array(x1:x2,y1:y2,image2d_state.detector-1)
	  xarr = px(x1:x2) 
	  yarr = py(y1:y2) 
	endif else begin 
	image = image_array(*,*,image2d_state.detector-1)
	  xarr = px
	  yarr = py
	end
	x = float(xarr)
	y = float(yarr)

	catch,status_error
	if status_error ne 0 then begin
	  WIDGET_CONTROL,Event.Top,SET_UVALUE=image2d_state,/NO_COPY,BAD_ID=bad
	  return
	end
	iImage,image,rgb_table=rgb,GROUP=Event.top, $
	title=image2d_state.name+': ('+image2d_state.DPVS(image2d_state.detector-1)+')', $ 
		view_grid=[2,2], $
		identifier=ID1, $
		name='Detector '+image2d_state.DPVS(image2d_state.detector-1)
	image2d_state.ID1 = ID1

	iSurface,image,rgb_table=rgb,GROUP=Event.top,x,y, $
		identifier=image2d_state.ID1,view_number=2, $
		name='Detector '+image2d_state.DPVS(image2d_state.detector-1)

	iContour,image,rgb_table=rgb,GROUP=Event.top,x,y, $
		identifier=image2d_state.ID1,view_number=3, $
		name='Detector '+image2d_state.DPVS(image2d_state.detector-1)
      END
  'IMAGE2D_RENEW': BEGIN
  	WIDGET_CONTROL, widget_ids.x_min, SET_VALUE=0
  	WIDGET_CONTROL, widget_ids.y_min, SET_VALUE=0
  	WIDGET_CONTROL, widget_ids.x_max, SET_VALUE=image2d_state.width-1
  	WIDGET_CONTROL, widget_ids.y_max, SET_VALUE=image2d_state.height-1
	image2d_REPLOT,image2d_state
	; set initial xl,xr,yl,yr
  	WIDGET_CONTROL, widget_ids.x1WID, SET_VALUE=xarr(0)
  	WIDGET_CONTROL, widget_ids.y1WID, SET_VALUE=yarr(0)
  	WIDGET_CONTROL, widget_ids.x2WID, SET_VALUE=xarr(image2d_state.width-1)
  	WIDGET_CONTROL, widget_ids.y2WID, SET_VALUE=yarr(image2d_state.height-1)
      END
  'IMAGE2D_LIST1': BEGIN
	if Event.Index lt (image2d_state.maxno-15) then begin
	image2d_state.detector = Event.Index + 16
	if id_def(image2d_state.detector-1) then begin
	image2d_REPLOT,image2d_state
	image2d_state.ID1 = ''
	; set initial xl,xr,yl,yr
  	WIDGET_CONTROL, widget_ids.x1WID, SET_VALUE=xarr(0)
  	WIDGET_CONTROL, widget_ids.y1WID, SET_VALUE=yarr(0)
  	WIDGET_CONTROL, widget_ids.x2WID, SET_VALUE=xarr(image2d_state.width-1)
  	WIDGET_CONTROL, widget_ids.y2WID, SET_VALUE=yarr(image2d_state.height-1)
	endif else begin
	r = dialog_message('Detector not defined',/info)
	end
	end
      END
  'IMAGE2D_LIST2': BEGIN
	if id_def(Event.Index) then begin
	image2d_state.detector = Event.Index + 1
	image2d_REPLOT,image2d_state
	image2d_state.ID1 = ''
	; set initial xl,xr,yl,yr
  	WIDGET_CONTROL, widget_ids.x1WID, SET_VALUE=xarr(0)
  	WIDGET_CONTROL, widget_ids.y1WID, SET_VALUE=yarr(0)
  	WIDGET_CONTROL, widget_ids.x2WID, SET_VALUE=xarr(image2d_state.width-1)
  	WIDGET_CONTROL, widget_ids.y2WID, SET_VALUE=yarr(image2d_state.height-1)
	endif else begin
	r = dialog_message('Detector not defined',/info)
	end
      END
  'IMAGE2D_XCURSOR': BEGIN
;      Print, 'Event for Cursor @ X'
      END
  'IMAGE2D_YCURSOR': BEGIN
;      Print, 'Event for Cursor @ Y'
      END
  'IMAGE2D_XL': BEGIN
;      Print, 'Event for XL'
      END
  'IMAGE2D_XR': BEGIN
;      Print, 'Event for XR'
      END
  'IMAGE2D_YL': BEGIN
;      Print, 'Event for YL'
      END
  'IMAGE2D_YR': BEGIN
;      Print, 'Event for YR'
      END
  'IMAGE2D_SETRANGE': BEGIN
        WIDGET_CONTROL, widget_ids.x1WID, GET_VALUE= x1
        WIDGET_CONTROL, widget_ids.x2WID, GET_VALUE= x2
        WIDGET_CONTROL, widget_ids.y1WID, GET_VALUE= y1
        WIDGET_CONTROL, widget_ids.y2WID, GET_VALUE= y2 
	nm = [image2d_state.x_pv+'.P1SP', image2d_state.x_pv+'.P1EP' , $
 	      image2d_state.y_pv+'.P1SP', image2d_state.y_pv+'.P1EP'] 
	listing0=''
	; check scan mode
	sm = 'caget -t '+image2d_state.x_pv+'.P1AR' +','+ $
	  	image2d_state.y_pv+'.P1AR'
	spawn,sm,smode
	if strtrim(smode(0),2) eq 'RELATIVE' then begin
	x0 = 0.5*(double(x1)+double(x2))
	y0 = 0.5*(double(y1)+double(y2))
	xwidth = 0.5*abs(double(x2)-double(x1))
	ywidth = 0.5*abs(double(y2)-double(y1))
	x1=string(-xwidth)
	x2=string(xwidth)
	
	if strtrim(smode(1),2) eq 'RELATIVE' then begin
	y1=string(-ywidth)
	y2=string(ywidth)
	end

	listing0=['RELATIVE SCAN MODE DETECTED','', $
	  'New Center for RELATIVE SCAN:', '  ('+strtrim(x0,2)+','+strtrim(y0,2)+')', $
	  '','New Relative Positions:']
	str =  [ listing0, $
		 'Xmin: '+nm(0)+'='+string(x1) ,'Xmax: '+nm(1)+'='+string(x2), $
		 'Ymin: '+nm(2)+'='+string(y1) ,'Ymax: '+nm(3)+'='+string(y2), $
		'','Set New 2D Scan Center Position (Yes/No)? ','']
	yn = dialog_message(str,title='Set New Center Position',/question)

	; set new center position
	if yn eq 'Yes' then begin
	st='caget -t '+ $
	  image2d_state.x_pv+'.P1PV'+','+image2d_state.y_pv+'.P1PV' 
	spawn,st,motors
	motors = strtrim(motors,2)
	if n_elements(motors) eq 2 then begin
	st='caput '+ motors(0)+','+motors(1)+ ' ' +$
	  strtrim(x0,2)+','+strtrim(y0,2)
	spawn,st,listing
	r = dialog_message(listing,/info,title='New 2D Scan Center')
	end
	end
	end

	str =  ['Set New 2D Scan ranges (Yes/No)? ','', $
		 'Xmin: '+nm(0)+'='+string(x1) ,'Xmax: '+nm(1)+'='+string(x2), $
		 'Ymin: '+nm(2)+'='+string(y1) ,'Ymax: '+nm(3)+'='+string(y2) ]
	yn = dialog_message(str,title='Set New 2D Scan Ranges',/question)

	if yn eq 'Yes' then begin
	st='caput '+nm(0) +','+nm(1)+','+ nm(2) +','+nm(3)+' '+ $
	  strtrim(x1,2)+','+strtrim(x2,2)+','+ strtrim(y1,2)+','+ strtrim(y2,2)
	spawn,st,listing
	r = dialog_message(listing,/info,title='Set New 2D Scan Ranges')
	end
      END
  'IMAGE2D_XMIN': BEGIN
         WIDGET_CONTROL, widget_ids.x_min, GET_VALUE = x
        if fix(x) ge 0 and fix(x) lt view_option.x_max then begin
        	if view_option.user eq 1 then image2d_REPLOT,image2d_state
	endif else begin
         WIDGET_CONTROL, widget_ids.x_min, SET_VALUE =0 
	 image2d_REPLOT,image2d_state
	end
      END
  'IMAGE2D_XMAX': BEGIN
         WIDGET_CONTROL, widget_ids.x_max, GET_VALUE = x
        if fix(x) gt view_option.x_min and $
		 fix(x) le view_option.width then begin
        if view_option.user eq 1 then image2d_REPLOT,image2d_state
	endif else begin
         WIDGET_CONTROL, widget_ids.x_max, SET_VALUE = image2d_state.width-1 
	 image2d_REPLOT,image2d_state
	end
      END
  'IMAGE2D_YMIN': BEGIN
         WIDGET_CONTROL, widget_ids.y_min, GET_VALUE = y
        if fix(y) ge 0 and fix(y) lt view_option.y_max then begin
        if view_option.user eq 1 then image2d_REPLOT,image2d_state
	endif else begin
         WIDGET_CONTROL, widget_ids.y_min, SET_VALUE =0 
	 image2d_REPLOT,image2d_state
	end
      END
  'IMAGE2D_YMAX': BEGIN
         WIDGET_CONTROL, widget_ids.y_max, GET_VALUE = y
        if fix(y) gt view_option.y_min and $
		 fix(y) le view_option.height then begin
        if view_option.user eq 1 then image2d_REPLOT,image2d_state
	endif else begin
         WIDGET_CONTROL, widget_ids.y_max, SET_VALUE = image2d_state.height-1 
	 image2d_REPLOT,image2d_state
	end
      END
  'IMAGE2D_ZMIN': BEGIN
	widget_control,widget_ids.z_min,get_value=z_min
	image2d_state.view_option.u_k_min = z_min
	image2d_state.view_option.k_min = z_min
	if image2d_state.view_option.user then image2d_REPLOT,image2d_state
      END
  'IMAGE2D_ZMAX': BEGIN
	widget_control,widget_ids.z_max,get_value=z_max
	image2d_state.view_option.u_k_max = z_max
	image2d_state.view_option.k_max = z_max
	if image2d_state.view_option.user then image2d_REPLOT,image2d_state
      END
  ; Event for PDMENU93
  'PDMENU93': PDMENU93_Event, Event, image2d_state
  ; Event for PDMENU99
  'PDMENU99': PDMENU99_ROI_Event, Event, image2d_state
  'IMAGE2D_TEXT': BEGIN
      END
  ENDCASE

  WIDGET_CONTROL,Event.Top,SET_UVALUE=image2d_state,/NO_COPY,BAD_ID=bad

END




PRO image2d, image_array,xarr,yarr,GROUP=Group,title=title,outpath=outpath,pv=pv,id_def=id_def,scanno=scanno,xdescs=xdescs,ydescs=ydescs,zdescs=zdescs,seqnm=seqnm,VERS=VERS
;+
; NAME:
;       IMAGE2D
;
; PURPOSE:
;       This program provides a general purpose, flexible generic 2D image
;       array viewer.  It provides 2D TV, SURFACE, CONTOUR, and SHADE_SURF plot.
;       It allows the user to select any 2D slice out of the image_array.
;       It is very simple to use and provides various display features of
;       showing 2D image data.
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
; INPUTS:
;       IMAGE_ARRAY[W,H,N] :   Image data array with N frames.
;       XARR[W]:   Variable to specify the corresponding x vector values.
;       YARR[H]:   Variable to specify the corresponding y vector values.
;
; KEYWORD PARAMETERS:
;
;       GROUP:   Specify the parent widget ID, the destroy of the parent ID
;                results the destroy of this program.
;       TITLE:   Set this keyword to specify the title string of the window.
;       PV:      Specify the scan record names if desired
;       SCANNO:  Specify the 2D scanno if it is known
;       OUTPATH: Specify the current output directory.
;	XDESCS[4]:  String array specify the x axis title
;	YDESCS[4]:  String array specify the y axis title
;	ZDESCS[N]:  String array specify the Z axis title
;	ID_DEF[N]:  2D data defined indicator vector 
;	SEQNM:   if SEQNM=1, detector list will be replaced by the ZDESCS
;		 if SEQNM is a astring array then detector list will be
;                replaced by SEQNM
;	VERS:    if specified implies scan version 5.19 or later assumed
;
; COMMON BLOCKS:
;       IMAGE2D_NORM_BLOCK used by the dialog of setting the Image
;       Color Scheme.
;
; SIDE EFFECTS:
;       Although many copy of IMAGE2D can co-exist at the same time, only
;       one dialog of setting the Image Color Scheme can be accessed.
;
; EXAMPLES:
;    Example 1 - Run IMAGE2D program with image_array(width,height,n),
;       xarr(width), Yarr(height)
;
;            IMAGE2D,image_array,Xarr,Yarr,Title='Window title'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, July 3, 2002.
;	01-07-2003	Release 1.0
;			Show Z value with @ (I,J) index
;			Modify the set new 2D scan ranges to check for relative
;                       mode to reset the new center position and relative new
;                       scan range
;	10-31-2003	Release 1.1
;			Support scan VERS 5.19 name start from D01...
;			
;-

@os.init
PS_init

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  def_title='IMAGE2D R1.1'
  if keyword_set(title) then def_title=title
  IMAGE2D_BASE = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      TITLE=def_title, $
      UVALUE='IMAGE2D_BASE')

  BASE2 = WIDGET_BASE(IMAGE2D_BASE, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      TITLE='IMAGE2D_ROW1', $
      UVALUE='BASE3')

  MenuDesc267 = [ $
      { CW_PDMENU_S,       1, 'File' }, $ ;        0
;        { CW_PDMENU_S,       0, 'Open...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Save Image for AIM' }, $ ;        2
        { CW_PDMENU_S,       0, 'Save as PNG' }, $ ;        3
        { CW_PDMENU_S,       0, 'Save as TIFF' }, $ ;        4
        { CW_PDMENU_S,       0, 'Save as XDR' }, $ ;        5
        { CW_PDMENU_S,       0, 'Printer...' }, $ ;        6
        { CW_PDMENU_S,       0, 'print' }, $ ;        7
        { CW_PDMENU_S,       0, 'PS_close' }, $ ;        8
        { CW_PDMENU_S,       2, 'Quit' }, $ ;        9
      { CW_PDMENU_S,       1, 'Color' }, $ ;       10
        { CW_PDMENU_S,       0, 'Save Private Color Table' }, $ ;       11
        { CW_PDMENU_S,       0, 'Load Private Color Table' }, $ ;       12
        { CW_PDMENU_S,       0, 'Image Color Scheme...' }, $ ;       14
        { CW_PDMENU_S,       0, 'Change Color Table...' }, $ ;       13
        { CW_PDMENU_S,       2, 'ColorBar Config...' }, $ ;       13
      { CW_PDMENU_S,       1, 'Help' }, $ ;       15
        { CW_PDMENU_S,       2, 'Help...' } $  ;     16

  ]


  PDMENU4 = CW_PDMENU( BASE3, MenuDesc267, /RETURN_FULL_NAME, $
      UVALUE='PDMENU4')

  LOGCOLOR = widget_droplist(BASE3,value=['Off','On'],title='Log', $
	UVALUE='IMAGE2D_LOGCOLOR')

  vlist = ['TV','Eq.TV.AspRt','LIGHT_SHADE_SURF','CONTOUR','SHOW3','PLOT2D...','SHADE_SURF']
  viewas = widget_droplist(BASE3,value=vlist, $
	TITLE='View as',UVALUE='IMAGE2D_VIEW')

  pixelby = widget_droplist(BASE3,value=['By Image','By User'], $
	TITLE='Pixel',UVALUE='IMAGE2D_PIXEL')
  widget_control,pixelby,set_droplist_select=1

  plotvs = widget_droplist(BASE3,value=['Step #','Values'], $
	TITLE='Plot vs',UVALUE='IMAGE2D_PLOTVS')
  widget_control,plotvs,set_droplist_select=1

  BUTTON8 = WIDGET_BUTTON( BASE3, $
      UVALUE='IMAGE2D_DONE', $
      VALUE='Done')


  BASE8 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      TITLE='IMAGE2D_ROW2', $
      UVALUE='BASE8')

  BUTTON9 = WIDGET_BUTTON( BASE8, $
      UVALUE='IMAGE2D_ASCII', $
      VALUE='ASCII...')

  FieldVal507 = [ $
    'G17.7' ]
  FIELD10 = CW_FIELD( BASE8,VALUE=FieldVal507, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE=' ', $
      UVALUE='IMAGE2D_FORMAT', $
      XSIZE=6)

  BUTTON14 = WIDGET_BUTTON( BASE8, $
      UVALUE='IMAGE2D_RENEW', $
      VALUE='ReNew')

  LABEL19 = WIDGET_LABEL( BASE8, $
      UVALUE='LABEL19', $
      VALUE='  Det#')

  ListVal1099 = [ $
    'D1  ', $
    'D2', $
    'D3', $
    'D4', $
    'D5', $
    'D6', $
    'D7', $
    'D8', $
    'D9', $
    'DA', $
    'DB', $
    'DC', $
    'DD', $
    'DE', $
    'DF' ]

  if keyword_set(zdescs) then ListVal1099=zdescs
  LIST20 = WIDGET_LIST( BASE8,VALUE=ListVal1099, $
      UVALUE='IMAGE2D_LIST2', $
      XSIZE=20,YSIZE=3)

  LABEL15 = WIDGET_LABEL( BASE8, $
      UVALUE='LABEL15', $
      VALUE='  Det#')

  ListVal949 = [ $
    'D01  ', 'D02', 'D03', 'D04', 'D05', 'D06', 'D07', 'D08', 'D09', 'D10' ]
	vlist = 'D'+strtrim(indgen(60)+11,2)
	ListVal949 = [ListVal949,vlist] +'          '
  if  keyword_set(zdescs) then begin
	no = n_elements(zdescs) 
	if no  gt 15 then ListVal949=zdescs(15:no-1)
  end
  LIST17 = WIDGET_LIST( BASE8,VALUE=ListVal949, $
      UVALUE='IMAGE2D_LIST1', $
      XSIZE=20,YSIZE=3)

  BUTTON18 = WIDGET_DROPLIST( BASE8, value=['iImage','iSurface','iContour'], $
      UVALUE='IMAGE2D_ITOOL', $
      TITLE='ITOOL')

  BASE24 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      TITLE='IMAGE2D_ROW3', $
      UVALUE='BASE24')

  BASE25 = WIDGET_BASE(BASE24, $
      COLUMN=1, $
      MAP=1, $
      TITLE='IMAGE2D_ROW3_1', $
      UVALUE='BASE25')

  LABEL27 = WIDGET_LABEL( BASE25, $
      UVALUE='LABEL27', $
      VALUE='3 Buttons: LMB-Values,MMB-Line plots,RMB-Zoom_in')

  DRAW33 = WIDGET_DRAW( BASE25, $
      RETAIN=2,/BUTTON_EVENTS, $
      UVALUE='IMAGE2D_DRAW', $
      XSIZE=460, $
      YSIZE=400)


  BASE26 = WIDGET_BASE(BASE24, $
      COLUMN=1, $
      MAP=1, $
      TITLE='IMAGE2D_ROW3_2', $
      UVALUE='BASE26')

  LABEL30 = WIDGET_LABEL( BASE26, $
      UVALUE='LABEL30', $
      VALUE='--IMAGE--')

  LABEL31 = WIDGET_LABEL( BASE26, $
      UVALUE='LABEL31',/ALIGN_LEFT, $
      VALUE='MIN Z:                                    ** ')

  LABEL32 = WIDGET_LABEL( BASE26, $
      UVALUE='LABEL32',/ALIGN_LEFT, $
      VALUE='MAX Z:                                    ** ')

  BASE40 = WIDGET_BASE(BASE26, $
      COLUMN=1,  $
      FRAME=1, $
      MAP=1, $
      TITLE='IMAGE2D_ROW3_2_1', $
      UVALUE='BASE40')

  FieldVal2394 = [ $
    '' ]
  FIELD42 = CW_FIELD( BASE40,VALUE=FieldVal2394, $
      COLUMN=1, /frame,$
      STRING=1, /NOEDIT, $
      TITLE='Cursor @ X', $
      UVALUE='IMAGE2D_XCURSOR')

  FieldVal2459 = [ $
    '' ]
  FIELD43 = CW_FIELD( BASE40,VALUE=FieldVal2459, $
      COLUMN=1, /frame,$
      STRING=1, /NOEDIT, $
      TITLE='Cursor @ Y', $
      UVALUE='IMAGE2D_YCURSOR')


  LABEL49 = WIDGET_LABEL( BASE40, $
      UVALUE='LABEL49',/ALIGN_LEFT, $
      VALUE='Z:                                     ')

  BASE50 = WIDGET_BASE(BASE26, $
      COLUMN=1, $
      FRAME=1, $
      MAP=1, $
      TITLE='IMAGE2D_ROW3_2_2', $
      UVALUE='BASE50')

  BASE51 = WIDGET_BASE(BASE50, $
      ROW=1, $
      FRAME=1, $
      MAP=1, $
      TITLE='IMAGE2D_ROW3_2_2_1', $
      UVALUE='BASE51')

  FieldVal2910 = [ $
    '' ]
  FIELD52 = CW_FIELD( BASE51,VALUE=FieldVal2910, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Xmin', $
      UVALUE='IMAGE2D_XL', $
      XSIZE=10)

  FieldVal2975 = [ $
    '' ]
  FIELD53 = CW_FIELD( BASE51,VALUE=FieldVal2975, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Xmax', $
      UVALUE='IMAGE2D_XR', $
      XSIZE=10)


  BASE60 = WIDGET_BASE(BASE50, $
      ROW=1, $
      FRAME=1, $
      MAP=1, $
      TITLE='IMAGE2D_ROW3_2_2_2', $
      UVALUE='BASE60')

  FieldVal3234 = [ $
    '' ]
  FIELD61 = CW_FIELD( BASE60,VALUE=FieldVal3234, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Ymin', $
      UVALUE='IMAGE2D_YL', $
      XSIZE=10)

  FieldVal3299 = [ $
    '' ]
  FIELD62 = CW_FIELD( BASE60,VALUE=FieldVal3299, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Ymax', $
      UVALUE='IMAGE2D_YR', $
      XSIZE=10)


  BUTTON67 = WIDGET_BUTTON( BASE50, $
      UVALUE='IMAGE2D_SETRANGE', $
      VALUE='Set New 2D Scan Ranges')




  BASE69 = WIDGET_BASE(BASE2, $
      ROW=1, $
      FRAME=1, $
      MAP=1, $
      TITLE='IMAGE2D_ROW4', $
      UVALUE='BASE69')

  FieldVal3613 = [ $
    '0' ]
  FIELD70 = CW_FIELD( BASE69,VALUE=FieldVal3613, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Imin', $
      UVALUE='IMAGE2D_XMIN', $
      XSIZE=5)

  FieldVal3751 = [ $
    '100' ]
  FIELD73 = CW_FIELD( BASE69,VALUE=FieldVal3751, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Imax', $
      UVALUE='IMAGE2D_XMAX', $
      XSIZE=5)

  FieldVal3816 = [ $
    '0' ]
  FIELD74 = CW_FIELD( BASE69,VALUE=FieldVal3816, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Jmin', $
      UVALUE='IMAGE2D_YMIN', $
      XSIZE=5)

  FieldVal3881 = [ $
    '100' ]
  FIELD75 = CW_FIELD( BASE69,VALUE=FieldVal3881, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Jmax', $
      UVALUE='IMAGE2D_YMAX', $
      XSIZE=5)

  FieldVal4023 = [ $
    '0.' ]
  FIELD76 = CW_FIELD( BASE69,VALUE=FieldVal4023, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='Zmin', $
      UVALUE='IMAGE2D_ZMIN', $
      XSIZE=10)

  FieldVal4088 = [ $
    '0.' ]
  FIELD77 = CW_FIELD( BASE69,VALUE=FieldVal4088, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='Zmax', $
      UVALUE='IMAGE2D_ZMAX', $
      XSIZE=10)


  BASE80 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      TITLE='IMAGE2D_ROW5', $
      UVALUE='BASE80')

  BASE81 = WIDGET_BASE(BASE80, $
      COLUMN=1, $
      MAP=1, $
      TITLE='IMAGE2D_ROW5_1', $
      UVALUE='BASE81')

  BASE92 = WIDGET_BASE(BASE81, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE92')

  sz = size(image_array)
  if n_elements(xarr) eq 0 then xarr = indgen(sz(1))
  if n_elements(yarr) eq 0 then yarr = indgen(sz(2))
  if sz(3) lt 15 and n_elements(seqnm) eq 0 then seqnm=1
  if sz(3) gt 85 then begin
  MenuDesc4955 = [ $
      { CW_PDMENU_S,       1, 'PanImages' }, $ ;        0
        { CW_PDMENU_S,       0, 'Calibration...' }, $ ;        2
        { CW_PDMENU_S,       3,'PanImage Subsets' }, $ ;        1
        { CW_PDMENU_S,       0,'First...' }, $ ;        1
        { CW_PDMENU_S,       0,'Next...' }, $ ;        1
        { CW_PDMENU_S,       0,'Mid-point...' }, $ ;        1
        { CW_PDMENU_S,       0,'Prev...' }, $ ;        1
        { CW_PDMENU_S,       2,'Last...' }, $ ;        1
      { CW_PDMENU_S,       3, 'Fitting' }, $ ;       14
        { CW_PDMENU_S,       0, 'Ez_Fit...' }, $ ;       15
        { CW_PDMENU_S,       2, '2D Binary' } $  ;     16
  ]
  endif else begin
  MenuDesc4955 = [ $
      { CW_PDMENU_S,       1, 'PanImages' }, $ ;        0
        { CW_PDMENU_S,       0, 'Calibration...' }, $ ;        2
        { CW_PDMENU_S,       2,'PanImages...' }, $ ;        1
      { CW_PDMENU_S,       3, 'Fitting' }, $ ;       14
        { CW_PDMENU_S,       0, 'Ez_Fit...' }, $ ;       15
        { CW_PDMENU_S,       2, '2D Binary' } $  ;     16
  ]
  end

  BASE26_1 = WIDGET_BASE(BASE26, $
      ROW=1, $
      FRAME=1, $
      MAP=1) 
  PDMENU93 = CW_PDMENU( BASE26_1, MenuDesc4955, /RETURN_FULL_NAME, $
      UVALUE='PDMENU93')

  ROIMenuDesc4955 = [ $
      { CW_PDMENU_S,       1, '2D-ROI' }, $ ;        3
        { CW_PDMENU_S,       0, 'Help...' }, $ ;        4
        { CW_PDMENU_S,       0, 'ROI...' }, $ ;        5
        { CW_PDMENU_S,       1, 'Type' }, $ ;        6
          { CW_PDMENU_S,       0, 'RectROI' }, $ ;        7
          { CW_PDMENU_S,       0, 'FilterROI' }, $ ;        8
          { CW_PDMENU_S,       2, 'PolyROI' }, $ ;        9
        { CW_PDMENU_S,       0, 'AppendRpt...' }, $ ;       10
        { CW_PDMENU_S,       0, 'ReplaceRpt...' }, $ ;       11
        { CW_PDMENU_S,       0, 'ViewRpt...' }, $ ;       12
        { CW_PDMENU_S,       2, 'RenameRpt...' } $ ;       13
  ]
  PDMENU99 = CW_PDMENU( BASE26_1, ROIMenuDesc4955, /RETURN_FULL_NAME, $
      UVALUE='PDMENU99')

  BASE82 = WIDGET_BASE(BASE80, $
      ROW=1, $
      FRAME=1, $
      MAP=1, $
      TITLE='IMAGE2D_ROW5_2', $
      UVALUE='BASE82')

  TextVal4525 = [ $
    '' ]
  TEXT86 = WIDGET_TEXT( BASE82,VALUE=TextVal4525, $
      EDITABLE=1,/scroll, $
      UVALUE='IMAGE2D_TEXT', $
      XSIZE=80, $
      YSIZE=5)

  WIDGET_CONTROL, IMAGE2D_BASE, /REALIZE


  ; Get drawable window index

  COMMON DRAW33_Comm, DRAW33_Id
  WIDGET_CONTROL, DRAW33, GET_VALUE=DRAW33_Id

 widget_ids = { base : IMAGE2D_BASE, $
      xsurface    : 0L,  $
      top_base    : 0L,       $
      norm_base   : 0L,       $
      sel_base    : LIST17,       $   D01-D70
      sel_image   : LIST20,       $   D1-DF
      plot_wid    : DRAW33,       $
      plot2d_area   : DRAW33_Id,    $
pixelby : pixelby, $
      zmin      :  LABEL31, $
      zmax      :  LABEL32, $
      x_cursor  :  FIELD42, $
      y_cursor  :  FIELD43, $
      z_cursor  :  LABEL49, $
        x1WID   : FIELD52, $ 
        x2WID   : FIELD53, $
        y1WID   : FIELD61, $
        y2WID   : FIELD62, $
      x_min       : FIELD70,     $
      x_max       : FIELD73,     $
      y_min       : FIELD74,     $
      y_max       : FIELD75,     $
      z_min       : FIELD76,     $
      z_max       : FIELD77,     $
      info        : TEXT86,      $
      textdata    : 0L,   $
        x1      : !x, $
        y1      : !y, $
        x2      : !x, $
        y2      : !y $
        }


if n_elements(image_array) eq 0 then begin
  xarr =  -3. + .06 * indgen(100) ; xarr
  yarr = .1 * indgen(50) ; yarr
  image_array = indgen(100,50,200,/float) ;image_array
end

  if n_elements(VERS) eq 0 then VERS=0
  image2d_init,widget_ids,image2d_state=image2d_state, $
	image_array,xarr,yarr,title=title,outpath=outpath, pv=pv, $
	id_def=id_def,scanno=scanno,xdescs=xdescs,ydescs=ydescs,$
	zdescs=zdescs,seqnm=seqnm,VERS=VERS

  image2d_REPLOT,image2d_state

  WIDGET_CONTROL,IMAGE2D_BASE,SET_UVALUE=image2d_state,/NO_COPY

  XMANAGER, 'IMAGE2D_BASE', IMAGE2D_BASE
END
