;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
;
; panimage.pro
;

PRO panimage_outfiles,win,res_image,tiff=tiff,order=order,png=png,pict=pict,xdr=xdr

	wset, win

catch,error_status
if error_status ne 0 then begin
r = dialog_message([!error_state.MSG,'You need to make sure you have write permission.'],/error)
return
end
	if keyword_set(TIFF) then begin
		tvlct,r,g,b,/get
		tiffname = strtrim(tiff,2)
	 	if keyword_set(order) then $
		write_tiff,tiffname,reverse(TVRD(),2),red=r,green=g,blue=b $
		else write_tiff,tiffname,TVRD(),red=r,green=g,blue=b
	end

	if keyword_set(PNG) then begin
		tvlct,r,g,b,/get
		pngname = strtrim(png,2)
		write_png,pngname,TVRD(),r,g,b
	end

        if keyword_set(PICT) then begin
                tvlct,r,g,b,/get
                gifname = strtrim(pict,2)
                write_pict,gifname,TVRD(),r,g,b
        end

        if keyword_set(XDR) then begin
                xdrname = strtrim(xdr,2)
		xdr_open,unit,xdrname,/write,error=error ; /append
		xdr_write,unit,res_image
		xdr_close,unit
        end

END 


PRO panimage_sel_init,panimageinfo,image_array,det_def,detnm=detnm

  sz = size(image_array)

; select  order in D1, ...,DF, D01...D70

  detname = 'D'+ [ $
		strtrim(indgen(9)+1,2),'A','B','C','D','E','F', $
       		'01','02','03','04','05','06','07','08','09', $
              	strtrim(indgen(61)+10,2) $
		]
  l123 = ['D01-D10','D11-D20','D21-D30','D31-D40','D41-D50','D51-D60','D61-D70','D1-DF']

  if keyword_set(detnm) then detname= detnm

  panimageinfo = {  $
	base : 0L, $
	title : '',$
	sublist : l123, $
	detname : detname, $
	new_win : -1, $
	multiwin : 1, $		  	; multiple window option
	image_array: image_array, $
	width : sz(1), $
	height : sz(2), $
	ndet: sz(3), $
	id_def: intarr(85), $
	outtype : 0, $
	factor : 1, $
	path : '', $
	class : '', $
	tiffname : '', $
	savemode : 0, $
	reverse : 1, $
	labelon : 0, $
	numd : 10, $		
	sel_list: intarr(85), $
	factor_id : 0L, $		;pan_factor, $
	tiff_id : 0L, $			;pan_tiffname, $
	reverse_id: 0L, $		;pan_reverse, $
	list_wid: 0L $ 			;LIST6 $
	}

panimageinfo.id_def = det_def

END

PRO PANIMAGE_ascii,file,ret,panimageinfo
	num = max(where(ret > 0))
	fmt = "(i4,"+ strtrim(panimageinfo.height,2)+"(g17.7))"

	openw,1,file
	for i=0,num do begin
		printf,1,""
		printf,1,""
		printf,1,"DETECTOR #",strtrim(ret(i)+1,2),'  (',panimageinfo.detname(ret(i)),')'
		im = panimageinfo.image_array(*,*,ret(i))
		for j=0,panimageinfo.width-1 do begin
		printf,1,format=fmt,j,im(j,*)
		end
	end
	close,1
END


PRO PANIMAGE_SEL_accept,ret,title,panimageinfo
;  construct a image_subarray according to the selected ret vector
;
;  ret - contains the Di index number
;  title - specifies5 the panwindow title text
;  panimageinfo - structure defined in panimage_sel
;

	sz = size(panimageinfo.image_array)
	if ret(0) gt sz(3) then return  ; outsize range select

	num = n_elements(ret)
	if num gt sz(3) then num = sz(3)
	image_subarray =make_array(panimageinfo.width,panimageinfo.height,num)

	for i=0,num-1 do begin
		if ret(i) lt sz(3) then $
		image_subarray(*,*,i) = panimageinfo.image_array(*,*,ret(i))
	end
	new_win = panimageinfo.new_win

	detname = panimageinfo.detname(ret)

	def = panimageinfo.id_def(ret)
	if panimageinfo.tiffname ne '' and panimageinfo.savemode  then begin
	order = panimageinfo.reverse
	if panimageinfo.outtype eq 0 then TIFF = panimageinfo.tiffname
	if panimageinfo.outtype eq 1 then PNG = panimageinfo.tiffname
	if panimageinfo.outtype eq 2 then PICT = panimageinfo.tiffname
	if panimageinfo.outtype eq 3 then XDR = panimageinfo.tiffname
	end

	panimage_slider,image_subarray,def,panimageinfo.factor,title=title, $
		DETNM=detname, labelon=panimageinfo.labelon, $
		TIFF=TIFF,ORDER=ORDER,PNG=PNG,PICT=PICT,XDR=XDR, $
		group=panimageinfo.base,numd=panimageinfo.numd
END


PRO PANIMAGE_SEL_Event, Event

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev
  WIDGET_CONTROL, Event.top, GET_UVALUE=panimageinfo

  CASE Ev OF 
  'PANIMAGE_SUBLIST': BEGIN
	sublist = WIDGET_INFO(Event.ID,/DROPLIST_SELECT)
	CASE sublist OF
	0: ret = indgen(10) +15 
	1: ret = indgen(10) +25 
	2: ret = indgen(10) +35 
	3: ret = indgen(10) +45
	4: ret = indgen(10) +55 
	5: ret = indgen(10) +65 
	6: ret = indgen(10) +75 
	7: ret = indgen(15)
	ENDCASE
	title = panimageinfo.title + ' : ' + panimageinfo.sublist(sublist)
	PANIMAGE_SEL_accept,ret,title,panimageinfo
	panimageinfo.sel_list = intarr(85) 
	panimageinfo.sel_list = ret 
      END
  'PANIMAGE_ALL': BEGIN
	ret = indgen(85) 
	title = panimageinfo.title + ' : All read detectors'
	ret_conv = ret(0:panimageinfo.ndet-1)
	PANIMAGE_SEL_accept,ret_conv,title,panimageinfo
	panimageinfo.sel_list = ret_conv 
      END
  'PANIMAGE_PSPRINT': BEGIN
		old_win = !d.window
		if panimageinfo.new_win gt 0 then begin
		wset,panimageinfo.new_win
		arr = TVRD()
    		PS_open,'idl.ps',/TV,yoffset=1.
		TV,arr
		PS_close
;		PS_print,'idl.ps'
		wset,old_win
		end
		return
      END
  'PANIMAGE_ACCEPT': BEGIN
	WIDGET_CONTROL,panimageinfo.tiff_id,GET_VALUE=name
	panimageinfo.tiffname = name
      ret = WIDGET_INFO(panimageinfo.list_wid,/LIST_SELECT)
	num = WIDGET_INFO(panimageinfo.list_wid,/LIST_NUMBER)
	if ret(0) lt 0 then begin
		ret = indgen(85) 
		ret_conv = ret(0:panimageinfo.ndet-1)
	endif else begin
	ret_conv = ret
	end
	title = panimageinfo.title
	PANIMAGE_SEL_accept,ret_conv,title,panimageinfo
	panimageinfo.sel_list = intarr(85) 
	panimageinfo.sel_list = ret_conv 
      END
  'PANIMAGE_CANCEL': BEGIN
	WIDGET_CONTROL,Event.top,/DESTROY
	return
      END
  'PANIMAGE_ASCII': BEGIN
	ret_conv = panimageinfo.sel_list
	if total(ret_conv) gt 0 then begin 
	file = DIALOG_PICKFILE(/write,title="PanImage save as ASCII file...", $
		FILE="panimage_data.txt", $
		DIALOG_PARENT=Event.top,FILTER="*txt*")
	if file ne '' then $
	PANIMAGE_ascii,file,ret_conv,panimageinfo
	end
      END
  'PANIMAGE_OUTPUT': BEGIN
	type = WIDGET_INFO(Event.ID,/DROPLIST_SELECT)
	panimageinfo.outtype = type
	if type eq 0 then $
		WIDGET_CONTROL,panimageinfo.reverse_id,SENSITIVE=1 $
	else 	WIDGET_CONTROL,panimageinfo.reverse_id,SENSITIVE=0 
	case type of
	0: newname = panimageinfo.path + panimageinfo.class+'tiff'
	1: newname = panimageinfo.path + panimageinfo.class+'png'
	2: newname = panimageinfo.path + panimageinfo.class+'pict'
	3: newname = panimageinfo.path + panimageinfo.class+'xdr'
	endcase
	WIDGET_CONTROL,panimageinfo.tiff_id,SET_VALUE = newname
	END
  'PANIMAGE_TIFFNAME': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=name
	panimageinfo.tiffname = name(0)
      END
  'PANIMAGE_FACTOR': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=factor
	panimageinfo.factor = factor
      END
  'PANIMAGE_LIST': BEGIN
      END
  'PANIMAGE_SAVEMODE': BEGIN
	WIDGET_CONTROL,panimageinfo.tiff_id,SENSITIVE=Event.Select
        panimageinfo.savemode = Event.Select
      END
  'PANIMAGE_REVERSE': BEGIN
        panimageinfo.reverse = Event.Select
      END
  'PANIMAGE_LABELON': BEGIN
        panimageinfo.labelon = Event.Select
      END
  'PANIMAGE_NUMD': BEGIN
;	if Event.Select then panimageinfo.numd = 5 else panimageinfo.numd = 10 
	numd = WIDGET_INFO(Event.ID,/DROPLIST_SELECT)
	panimageinfo.numd = numd+1
      END
  'PANIMAGE_MULTIWIN': BEGIN
        panimageinfo.multiwin = Event.Select
      END

  ENDCASE

   WIDGET_CONTROL, Event.top, SET_UVALUE=panimageinfo
END




PRO panImage_sel, GROUP=Group,image_array,det_def,title=title,new_win=new_win,panimageinfo,tiff=tiff,path=path,detnm=detnm
;+
; NAME: 
;   panImage_Sel
;
; PURPOSE:
;       This method pops up a PanImage selection dialog for a given 2D scan
;       image_array.
;
; CALLING SEQUENCE:
;       panImage_sel, Image_array, det_def [,TITLE='description'] [,DETNM=detnm]
;
; ARGUMENTS:
;  Image_array:  Image_array(Width,Height,Ndets) specifies the 2D
;                image_array for all detectors, where each image has
;                dimension of WidthxHeight, Ndets is the number of detectors
;  Det_def:      Det_def(Ndets) defines the vector of indicators for detector
;                image presence, 0 not present , 1 present
;
; KEYWORDS:
;     TITLE:     Specifies the title of the panImage window
;     TIFF:      Specifies the output tiff file name 
;     PATH:      Specifies the output data path
;     DETNM:     Specifies the Ndets image ID names
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Mar 29, 2000.
;       04-21-2003 bkc  Add ASCII... report generation button for selected image
;-

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  panimage_sel_init,panimageinfo,image_array,det_def,DETNM=DETNM

  PANIMAGE_SEL = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
	TITLE="PanImage Selection Dialog", $
      UVALUE='PANIMAGE_SEL')

  BASE2 = WIDGET_BASE(PANIMAGE_SEL, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE2_0 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE2_2 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2_2')

  pan_sublist = WIDGET_DROPLIST( BASE2_2,VALUE=panimageinfo.sublist, $
      UVALUE = "PANIMAGE_SUBLIST",TITLE='Select Sublist:')

  pan_factor = CW_FIELD(BASE2_2,/INTEGER,/RETURN_EVENTS, $
	TITLE='Grid Exp Factor:', $
	XSIZE=2, YSIZE=1, VALUE=1, $
	UVALUE = "PANIMAGE_FACTOR")

  BASE2_3 = WIDGET_BASE(BASE2, $
      ROW=1, /FRAME, $
      MAP=1, $
      UVALUE='BASE2_3')

  BASE2_3_1 = WIDGET_BASE(BASE2_3, $
      COLUMN=1, /FRAME, $
      MAP=1, $
      UVALUE='BASE2_3_1')

  lebel2 = WIDGET_LABEL(BASE2_3_1,Value='Select Detectors:')
  pan_labelon = CW_BGROUP( BASE2_3_1, ['Di Show/Hide'], $
      ROW=1, NONEXCLUSIVE=1, UVALUE='PANIMAGE_LABELON')

  pan_numd = WIDGET_DROPLIST( BASE2_3_1,VALUE=['1','2','3','4','5','6','7','8','9','10'], $
      UVALUE = "PANIMAGE_NUMD",TITLE='Di/Row:')
	widget_control,pan_numd,set_droplist_select=panimageinfo.numd-1

  BASE2_20 = WIDGET_BASE(BASE2_3, $
      ROW=1, /FRAME, $
      MAP=1, $
      UVALUE='BASE2_20')

  LIST6 = WIDGET_LIST( BASE2_20,VALUE=panimageinfo.detname, /MULTIPLE,  $
      UVALUE='PANIMAGE_LIST', XSIZE=10, $
      YSIZE=5)
  widget_control,LIST6,set_list_top=15
  pan_accept = WIDGET_BUTTON( BASE2_20,VALUE='Accept', $
      UVALUE = "PANIMAGE_ACCEPT")
  pan_all = WIDGET_BUTTON( BASE2_20,VALUE=" All ", $
      UVALUE = "PANIMAGE_ALL")
  pan_print = WIDGET_BUTTON( BASE2_20,VALUE="Print", $
      UVALUE = "PANIMAGE_PSPRINT")
  pan_cancel = WIDGET_BUTTON( BASE2_20,VALUE=' Close', $
      UVALUE = "PANIMAGE_CANCEL")


  BASE2_12 = WIDGET_BASE(BASE2, $
      COLUMN=1, /FRAME, $
      MAP=1, $
      UVALUE='BASE2_12')

  BASE2_1 = WIDGET_BASE(BASE2_12, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2_1')

  Btns753 = [ $
    'Save PanImage' ]
  pan_savemode = CW_BGROUP( BASE2_1, Btns753, $
      ROW=1, $
      NONEXCLUSIVE=1, $
      UVALUE='PANIMAGE_SAVEMODE')

  pan_output = WIDGET_DROPLIST( BASE2_1,VALUE=['TIFF','PNG','PICT','XDR'], $
      UVALUE = "PANIMAGE_OUTPUT",TITLE='Type')

  pan_reverse = CW_BGROUP( BASE2_1, ['Reverse'], $
      ROW=1, NONEXCLUSIVE=1, UVALUE='PANIMAGE_REVERSE')
  WIDGET_CONTROL,pan_reverse,SET_VALUE=1

  pan_ascii = WIDGET_BUTTON( BASE2_1, VALUE="ASCII...", $
      UVALUE='PANIMAGE_ASCII')

  pan_tiffname = CW_FIELD(BASE2_12, /RETURN_EVENTS, $
	TITLE='Filename:', $
	XSIZE=50, YSIZE=1, VALUE="tiff", $
	UVALUE = "PANIMAGE_TIFFNAME")
  WIDGET_CONTROL,pan_tiffname,SENSITIVE=0

  WIDGET_CONTROL, PANIMAGE_SEL, /REALIZE

  if keyword_set(path) then panimageinfo.path = path
  if keyword_set(tiff) then  begin
	WIDGET_CONTROL,pan_tiffname, SET_VALUE= panimageinfo.path+tiff
  	panimageinfo.tiffname = tiff
	l = strpos(tiff,'.',/reverse_search)
	class = strmid(tiff,0,l+1)
  	panimageinfo.class = class
  end

  panimageinfo.base = PANIMAGE_SEL 
  panimageinfo.factor_id = pan_factor
  panimageinfo.tiff_id = pan_tiffname
  panimageinfo.reverse_id = pan_reverse
  panimageinfo.list_wid  = LIST6

  if n_elements(new_win) then panimageinfo.new_win = new_win
  if n_elements(title) then panimageinfo.title = title

  WIDGET_CONTROL, PANIMAGE_SEL, SET_UVALUE=panimageinfo
  XMANAGER, 'PANIMAGE_SEL', PANIMAGE_SEL
  
END


PRO panImage,image_array,id_def,factor,title=title,new_win=new_win,xpos=xpos,ypos=ypos,tiff=tiff,reverse=reverse,labelon=labelon,png=png,pict=pict,xdr=xdr,error=error,ISEL=ISEL,NUMD=NUMD,DETNM=DETNM
;+
; NAME:
;	panImage
;
; PURPOSE:
;       This method pops up a new PanImage window for a given 2D scan 
;       image_array.
;
; CALLING SEQUENCE:
;       panImage, Image_array [,Id_def] [,Factor=1]  [,TIFF='tifname',/reverse]
;		  [,TITLE='description'] [,DETNM=DetNm] [,ISEL=Isel]
;                 [,NUMD=NumD]
;                 [,GIF='pngname'] [,PICT='pictname'] [,ERROR=error]
;
; ARGUMENTS:
;  Image_array:  Image_array(Width,Height,Ndets) specifies the 2D 
;                image_array for all detectors, where each image has
;                dimension of WidthxHeight, Ndets is the number of detectors
;  id_def:       Id_def(Ndets) defines the vector of indicators for detector 
;                image presence, 0 not present , 1 present 
;  factor:       Specifies the multiplication factor for TV image, default
;                TV image size is 60x60
;
; KEYWORDS:
;     TITLE:   Specifies the title of the panImage window
;     PNG:     Specifies the output png filename. If specified the
;              panImage window will be saved in the png output file.
;     PICT:    Specifies the output pict filename. If specified the
;              panImage window will be saved in the pict output file.
;     TIFF:    Specifies the output tiff filename. If specified the
;              panImage window will be saved in the tiff output file.
;     XDR:     Specifies the output XDR filename. If specified, 
;              an XDR image_array will be saved.
;     REVERSE: Specifies whether the reverse tiff should be saved.
;     LABELON: Specifies whether to label the image.
;     NEW_WIN: Returns the new window number of the panImages
;     ISEL:    Specifies the image_array is an extracted subarray from the
;              original array
;     NUMD:    Specifies the number of image in a row 
;     DETNM:   Override the detname used
;     BID:     Return the top base widget id
;     WID:     Returns the window id number
;
; EXAMPLE:
;     
;     panImage, image_array
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Mar 29, 2000.
;       10-02-2001 bkc  Add window position keywords
;	xx-xx-xxxx bkc  comment
;-

error=0
	if n_elements(image_array) eq 0 then begin
		st = "Usage: panImage,image_array,title='description',tif=..."
		r = dialog_message(st,/Error)
		error = -1
		return
	end

	sz = size(image_array)

	if n_elements(sz) lt 5 then begin
		st = "Dimension error!!  Image_array(W,H,N)"
		r = dialog_message(st,/Error)
		error = -1
		return
	end

	if n_elements(id_def) eq 0 then id_def = make_array(sz(3),value=1)

	detname = 'D'+ [ $
		strtrim(indgen(9)+1,2),'A','B','C','D','E','F', $
       		'01','02','03','04','05','06','07','08','09', $
              	strtrim(indgen(61)+10,2) $
		]
	if keyword_set(DETNM) then detname=DETNM

; pops up pan images

update:

	NC = 8
	if n_elements(numd) then NC = numd
	ND = sz(3)
	if sz(0) eq 2 then ND = 1
	NR = CEIL(float(ND)/NC)
	if ND lt NC then NC = ND

	NL = NR*NC -1

	width = 60
	height = 60
	old_win = !D.window

	if n_elements(factor) then begin
		if factor lt .1 then factor = 1
		width = width * factor
		height = height * factor
	end

	o_win = -1
	if n_elements(new_win) then o_win = new_win
catch,error_status
if error_status ne 0 then begin
help,!error_state,/st
       o_win = -1
      if !error_state.name eq  'IDL_M_CNTOPNFIL' then  begin
        r = dialog_message([!error_state.msg,!error_state.sys_msg,$
                string(!error_state.code)],/error)
	return
	end
end
if o_win ne -1 then wdelete,o_win
o_win = -1
	width1 = width+1
	height1 = height+1
	if o_win lt 0 then begin
	if keyword_set(xpos) then $
		window,/free, xsize = NC*width1-1, ysize=NR*height1-1, $
			xpos=xpos,ypos=ypos,title=title,RETAIN=2 $
	else	window,/free, xsize = NC*width1-1, ysize=NR*height1-1, $
			title=title,RETAIN=2
		if n_elements(isel) gt 0 then begin
		for i=0,ND-1 do begin
		ii = NL-i
		xi=(i mod NC)*width1+width/2 - 5 
		yi=height/2+ii/NC*height1
		xyouts, xi,yi,detname(i),/device
		end
		end
	end

new_win =!d.window 

	wset,new_win
	for sel=0,ND-1 do begin
	if id_def(sel) gt 0 then begin
	xp = (sel mod NC) *width1
	yp = (NR - 1 - sel / NC) *width1
	v_max = max(image_array(*,*,sel),min=v_min)
	if v_max eq v_min then begin
		temp = (!d.n_colors-1) * image_array(*,*,sel) 
		TV,congrid(temp,width,height),xp,yp ;,sel
	endif else begin
		temp = congrid(image_array(*,*,sel), width, height)
		TVSCL, temp,xp,yp ;, sel
	end
	endif else begin
		ii = NL-sel
		xi=(sel mod NC)*width1+width/2 - 5 
		yi=height/2+ii/NC*height1
		xyouts, xi,yi,detname(sel),/device
		end
	end

	;labelon
	if keyword_set(labelon) then begin
		for i=0,ND-1 do begin
		ii = NL-i
		xi=(i mod NC)*width1+5 
		yi=ii/NC*height1+5
		xyouts, xi,yi,detname(i),/device
		end
	end

;	for i=1,NR-1 do plots,[0,NC*width1],[i*height1,i*height1],/device
;	for i=1,NC-1 do plots,[i*width1,i*width1],[0,NR*height1],/device

	panimage_outfiles,new_win,image_array,tiff=tiff,order=reverse,png=png,pict=pict,xdr=xdr

	catch,error_status
	if error_status ne 0 then return
	wset,old_win

END

PRO panimage_slider,image_array,id_def,factor,numd=numd,title=title,Group=Group,bid=base,wid=win,tiff=tiff,order=order,labelon=labelon,png=png,pict=pict,xdr=xdr,error=error,detnm=detnm
;+
; NAME:
;	panImage_slider
;
; PURPOSE:
;       This method pops up a new PanImage window for a given 2D scan 
;       image_array.
;
; CALLING SEQUENCE:
;       panImage, Image_array [,Id_def] [,Factor=1]  [,TIFF='tifname',/reverse]
;		  [,TITLE='description'] [,DETNM=DetNm] [,ISEL=Isel]
;                 [,NUMD=NumD]
;                 [,GIF='pngname'] [,PICT='pictname'] [,ERROR=error]
;
; ARGUMENTS:
;  Image_array:  Image_array(Width,Height,Ndets) specifies the 2D 
;                image_array for all detectors, where each image has
;                dimension of WidthxHeight, Ndets is the number of detectors
;  id_def:       Id_def(Ndets) defines the vector of indicators for detector 
;                image presence, 0 not present , 1 present 
;  factor:       Specifies the multiplication factor for TV image, default
;                TV image size is 60x60
;
; KEYWORDS:
;     TITLE:   Specifies the title of the panImage window
;     PNG:     Specifies the output png filename. If specified the
;              panImage window will be saved in the png output file.
;     PICT:    Specifies the output pict filename. If specified the
;              panImage window will be saved in the pict output file.
;     TIFF:    Specifies the output tiff filename. If specified the
;              panImage window will be saved in the tiff output file.
;     XDR:     Specifies the output XDR filename. If specified, 
;              an XDR image_array will be saved.
;     REVERSE: Specifies whether the reverse tiff should be saved.
;     LABELON: Specifies whether to label the image.
;     NEW_WIN: Returns the new window number of the panImages
;     ISEL:    Specifies the image_array is an extracted subarray from the
;              original array
;     NUMD:    Specifies the number of image in a row 
;     DETNM:   Override the detname used
;     BID:     Return the top base widget id
;     WID:     Returns the window id number
;
; EXAMPLE:
;     
;     panImage_slider, image_array
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Feb 23, 2003.
;	xx-xx-xxxx bkc  comment
;-

error=0
if n_elements(image_array) eq 0 then begin
	st = "Usage: panImage_slider,numd=numd,image_array,id_def,..."
	r = dialog_message(st,/error)
	error=-1
	return
	end

if n_params() then begin
	old_win = !d.window
	sz = size(image_array)
	if sz(0) eq 2 then begin
		width = sz(1)*factor
		height = sz(2)*factor
		if width lt 60 then width = 60
		if height lt 60 then height = 60
		im = image_array
		v_max = max(im)
		v_min = min(im)
		if v_max eq v_min then res_image = (!d.table_size -1 )*im  $
		else res_image = bytscl(im)
		res_image = congrid(res_image,width,height)
		jrow = 1
		xsize = width
		ysize = height
		goto, display_image
	end

	width = 60
	height = 60
	if n_elements(factor) then begin
		width = width * factor
		height = height * factor
	end 

	; image width & height
	if keyword_set(numd) eq 0 then numd = 10
	if sz(3) lt numd then numd = sz(3)
	if n_elements(id_def) eq 0 then id_def = make_array(sz(3),value=1)

	jrow = sz(3)/numd 
	if sz(3) mod numd gt 0 then jrow = jrow+1
	xsize = (width + 1) * numd - 1
	ysize = (height + 1) * jrow - 1
	res_image = bytarr(xsize,ysize)

	; reconstruct composite image
	for k=0,sz(3)-1 do begin
	if id_def(k) then begin
	im = image_array(*,*,k)
	v_max = max(im)
	v_min = min(im)
	if v_max eq v_min then bim = (!d.table_size -1 )*im else bim = bytscl(im)
	bim = congrid(bim,width,height)
	i = k mod numd
	j = jrow-1 - k / numd
	is = i * (width +1)
	js = j * (height +1)
;	print, k,i,j,is,js
        res_image(is:is+width-1,js:js+height-1) = bim(*,*) 
	end
	end
endif else res_image = dist(400,800)

display_image:
	xvisible = xsize
	if xsize gt 800 then xvisible = 800
	yvisible = ysize
	if ysize gt 800 then yvisible = 800
	win=!d.window
	if keyword_set(wid) then win=wid
	slide_image,res_image,top_id=base,show_full=0,title=title, $
		Group=Group,/register, $
		slide_window=win,xvisible=xvisible,yvisible=yvisible
	wid = win
	bid = base

; set detname

        if sz(0) eq 3 then begin
	wset,win
	for k=0,sz(3)-1 do begin
	i = k mod numd
	j = jrow-1 - k / numd
	is = i * (width +1) 
	js = j * (height +1) 
	if id_def(k) eq 0 then begin	
	   xyouts,is+width/2,js+height/2,detnm(k),/device
	endif else begin
	   if keyword_set(labelon) then $ 
	   xyouts,is+5,js+5,detnm(k),/device
	end
	end
	end

	NC = numd
	NR = jrow
	if NC gt 1 then begin
	width1 = width+1
	height1= height+1
	for i=1,NR-1 do plots,[0,NC*width1-1],[i*height1-1,i*height1-1],/device
	for i=1,NC-1 do plots,[i*width1-1,i*width1-1],[0,NR*height1-1],/device
	end

	; save output file
	if keyword_set(tiff) or keyword_set(png) or keyword_set(pict) or keyword_set(xdr) then $
	panimage_outfiles,win,res_image,tiff=tiff,order=order,png=png,pict=pict,xdr=xdr

	wset,old_win
END
