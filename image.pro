;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
FORWARD_FUNCTION REVERSE

function rgbTo16, r, g, b
  nRBits = 5
  nGBits = 6
  nBBits = 5

  cr = ishft(ULONG(r), -(8-nRBits))
  sr = ishft(cr, (nGBits+nBBits))

  cg = ishft(ULONG(g), -(8-nGBits))
  sg = ishft(cg, nBBits)

  cb = ishft(ULONG(b), -(8-nBBits))
  sb = cb

  ccomb = sr+sg+sb

  return, ccomb
end

PRO calc_16bit,im,wd,ht,image=image,vmax=vmax,vmin=vmin,true=true
;    With your original TIFF image data, you can do the following:
        sz = size(im)
        if sz(0) ne 3 then return

  rimg = ULONG(REFORM(im[0,*,*]))
  gimg = ULONG(REFORM(im[1,*,*]))
  bimg = ULONG(REFORM(im[2,*,*]))


        if keyword_set(true) then $
        im2 = rimg + (gimg + bimg *256L) * 256L else begin      ;24bit
                img16 = rgbTo16(rimg,gimg,bimg)         ;16bit
                im2 = img16
        end
help,im2

        cmax = max(im2)
        cmin = min(im2)
print,cmax,cmin
        fmax=.991707
        fmin = -.891682
        if keyword_set(vmax) then fmax=vmax
        if keyword_set(vmin) then fmin=vmin
        val = fmin + (fmax-fmin)*(float(im2)-cmin)/(cmax-cmin)
        image=val
        plot2d,val
END

PRO calc_8bit,im,wd,ht,image=image,vmax=vmax,vmin=vmin
; the im must be obtained from a 8 bit tiff file
; 
        sz = size(im)
        if sz(0) ne 2 then return
        im2 = congrid(im,wd,ht)
        cmax = max(im2)
        cmin = min(im2)
        fmax=.991707
        fmin = -.891682
        if keyword_set(vmax) then fmax=vmax
        if keyword_set(vmin) then fmin=vmin
        val = fmin + (fmax-fmin)*im2/float(cmax-cmin)
        image=val
        plot2d,val
END

pro calc_image,wd,ht,image=image,vmax=vmax,vmin=vmin
; this tvrd() screen and try to calculate the image array
;  this function only works for the 8 bit color image
;  not work for the 16 bit color image

        ; 16 bit color device
        if !d.n_colors gt 256 then begin
        im = tvrd(/true)
        im2 = color_quan(im,1,r,g,b,get_translation=tr)
 print,!d.n_colors
 help,r,g,b,im2,im,tr
 print,im2
        tvlct,r,g,b
        val = make_array(wd,ht)
        for i=0,wd-1 do begin
        for j=0,ht-1 do begin
                c = im2(i,j)
                cf = r(c) + 256L * (g(c) + 256L * b(c))
                val(i,j) = cf
        end
        end
        val = congrid(val,wd,ht)

        ; 8 bit color device
        endif else begin

        im = tvrd()
        im2 = congrid(im,wd,ht)
        val = im2
        end

        cmax = max(val)
        cmin = min(val)

;tvlct,r,g,b,/get
;	cval = intarr(cmax+1)
;	for i=0,cmax do begin
;	cval(i) = rgbTo16(r(i),g(i),b(i))
;	end
;help,cval	
;index = sort(cval)
;print,index 
;print,cval
;print,cval(index)
;cvmin = min(cval)
;cvmax = max(cval)

        fmax=.991707
        fmin = -.891682
        if keyword_set(vmax) then fmax=vmax
        if keyword_set(vmin) then fmin=vmin
        val = fmin + (fmax-fmin)*(float(val)-cmin)/(cmax-cmin)
        image=val
        plot2d,val,ncolors=cmax+1
end



PRO obj_cleanup
        ot = obj_valid()
        if n_elements(ot) gt 0 then begin
        for i=0,n_elements(ot)-1 do obj_destroy, ot(i)
        end
END

PRO curvePlot,window=window,view=view,curve=curve,legend=legend,data

if n_elements(data) eq 0 then data=sin(findgen(100)/10)

        xr = 100
        yr = 2
        view = obj_new('idlgrview',viewplane_rect=[0,-1,xr,yr])
        model = obj_new('idlgrmodel')
        plot = obj_new('idlgrplot',data)
        window = obj_new('idlgrwindow',COLOR_MODEL=1)
        
;       add curve model
        view->add,model
        model->add,plot
;       add legend model2
        legendobj,mylegend
        model2 = obj_new('idlgrmodel')
        view->add,model2
        model2->add,mylegend
        model2->translate,10,0,0

        window->draw,view
        curve = model
        legend = model2
;  obj_destroy,window
; obj_destroy,view
END

PRO model_translation,oModel,dx,dy,dz
        oModel->Translate,dx,dy,dz
        oModel->getProperty,transform=newT
        print,'newT=',newT
END
PRO model_rotation,oModel,Dangle,Iaxis
        axis =[1,0,0]   ; x-axis
        if Iaxis eq 2 then axis = [0,1,0]
        if Iaxis eq 3 then axis = [0,0,1]
        oModel->Rotate,axis,Dangle
        oModel->getProperty,transform=newT
        print,'newT=',newT
END

PRO surfacePlot,data,window=oWin,surface=oSurface,view=oView,model=oModel

        if n_elements(data) eq 0 then data = dist(60)
        zdata = data
        oView = obj_new('idlgrview', eye=6, $
                viewplane_rect=[-1,-1,2,2])
        oModel = obj_new('idlgrmodel')
        oView->add, oModel
        oSurface = obj_new('idlgrsurface',zdata, $
                shading=0, $
                color=[255,255,0])
        oModel->add,oSurface

        oSurface->getProperty,xrange=xrange,yrange=yrange,zrange=zrange
        xs = [-0.5, 1/(xrange[1]-xrange[0])]
        ys = [-0.5, 1/(yrange[1]-yrange[0])]
        zs = [0, 1/(zrange[1]-zrange[0])]
        oSurface->setProperty,xcoord_conv=xs, $
                ycoord_conv=ys,zcoord_conv=zs
        oModel->Rotate,[1,0,0],-90
        oModel->Rotate,[0,1,0],30       
        oModel->Rotate,[1,0,0],30
        oWin = obj_new('idlgrwindow',COLOR_MODEL=1)
        oWin->draw,oView
END

PRO updatePos,newpos,plot=plotobj,window=owindow,view=oview
; newpos [x,y,0]
; x,y,z   specifies the coordinate of the lower left corner of the plot object 
; plotobj : specifies the model (plot object) to be moved
; oview   : specifies the view object which contains the omodel
; owindow : specifies the window object to be redraw
;
        if obj_valid(owindow) then begin
        if n_elements(newpos) eq 2 then newpos=[newpos,0]
        ctm = plotobj->getCTM()
        opos = ctm(3,*)
        dpos = newpos - opos
        plotobj->translate,dpos(0),dpos(1),dpos(2)
        owindow->draw,oview
        endif else begin
                str = ['updatePos, [x,y,0], plot=plotobj, window=owin, view=oview', $
                'where','plotobj, owin, oview must be defined first']
                print,dialog_message(str,/error)
        end
END

PRO legendObj,mylegend,strings=strings
        itemnamearr = ['Cows', 'Weasels']
        if keyword_set(strings) then itemnamearr = strings
        mytitle = obj_new('idlgrtext', 'My Legend')
        mysymbol = obj_new('idlgrsymbol',4)
        mypattern = obj_new('idlgrpattern',1)
        mylegend = obj_new('idlgrlegend',itemnamearr,title=mytitle, $
                border_gap=0.8, gap=0.5, item_type=[0,1], $
                item_object=[mysymbol,mypattern],/show_outline)
END 

PRO legendPlot,window=mywindow,legend=mylegend,view=myview,model=mymodel
        mywindow = obj_new('idlgrwindow')
        myview = obj_new('idlgrview')
        mymodel = obj_new('idlgrmodel')
        myview->add,mymodel

        legendObj,mylegend,strings=['line1','line2','line3']
        mymodel->add,mylegend
        dims = mylegend->computeDimensions(mywindow)
; print,dims
        mymodel->translate,-(dims[0]/2.), -(dims[1]/2.), 0
        mywindow->draw,myview
END

PRO imagePlot,window=owin,view=oview,image=oimage
        file = filepath('rose.jpg',subdir=['examples','data'])
        read_jpeg, file, image

        mywindow = obj_new('idlgrwindow',dimensions=[227,149])
        myview = obj_new('idlgrview',view=[0,0,227,149])
        mymodel = obj_new('idlgrmodel')
        myimage = obj_new('idlgrimage',image,interleave=0)

        myview->add,mymodel
        mymodel->add,myimage
        mywindow->draw,myview
        owin = mywindow
        oview = myview
        oimage = myimage
END

PRO colorbarPlot,title=title,dims=dims,left=left,right=right,xypos=xypos
; default place vertical colorbar object at the center of page
; left   - place colorbar at left hand side
; right  - place colorbar at right hand side
; xypos=[rx,ry]   - specify the normalized x,y position of the colorbar
;                   -1 < rx,ry < 1

        mywindow = obj_new('idlgrwindow')
        myview = obj_new('idlgrview')
        mymodel = obj_new('idlgrmodel')
        myview->add,mymodel

        titlestr = 'My Colorbar'
        if keyword_set(title) then titlestr = title
        mytitle = obj_new('idlgrtext',titlestr)

        ; normalized width and height of bar dimension in xr,yr
        barDims = [0.1,0.4]
        if keyword_set(dims) then barDims = dims

	; get color table

	TVLCT,redValues,greenValues,blueValues,/GET
        mycolorbar = obj_new('idlgrcolorbar',redValues,greenValues, $
                blueValues,TITLE=mytitle,dimensions=barDims, $
        /show_axis,/show_outline)

        mymodel->add,mycolorbar

        barplustextdims = mycolorbar->computeDimensions(mywindow)

	; center
	xpos = -bardims[0]+barplustextdims[0]/2.
	ypos = -bardims[1]+barplustextdims[1]/2.
	; right
	if keyword_set(right) then begin
	xpos = 1.-barplustextdims[0]
	ypos = -barplustextdims[1]/2.
	end
	; legt
	if keyword_set(left) then begin
	xpos = -.9+barplustextdims[0]
	ypos = -barplustextdims[1]/2.
	end
	if keyword_set(xypos) then begin
		xpos = xypos(0)
		ypos = xypos(1)
	end
	
        mymodel->translate,xpos,ypos, barplustextdims[2] 
;        mymodel->scale,2,2,2
        mywindow->draw,myview
END

PRO image_data_aprox,data,xl,xr,yl,yr,v_max,v_min,width=width,height=height,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,real=real,group=group
; extract 2D image area from the 8bit tiff data
;

	im = data(xl:xr-1,yl:yr-1)

	sz = size(im)
	wd = sz(1)
	ht = sz(2)
	if keyword_set(width) then wd = width
	if keyword_set(height) then ht = height
	im2 = congrid(im,wd,ht,/minus_one)

	c_max = max(im2)
	c_min = min(im2)
	dm = c_max-c_min
	n_im = v_min + (v_max-v_min)*(im2 - c_min)/dm

	if keyword_set(real) then begin
	dx = (xmax-xmin)/(wd-1)
	dy = (ymax-ymin)/(ht-1)
		x = xmin + dx * indgen(wd)
		y = ymin + dy * indgen(ht)
		colors = max(im)
		plot2d,n_im,xarr=x,yarr=y ;,group=group
	endif else plot2d,n_im ;,group=group
END

PRO image_read,filename,image=image,ranges=ranges,show=show,red=r,green=g,blue=b,ftype=ftype
; this program can read image data from user specified filename
; it supports tiff, png, jpg, ascii, xdr type files
;
        file = 'myfile.tif'
        if n_elements(filename) then file = filename

        pos = strpos(file,'.',/reverse_search)
        if pos gt 0 then ftype = strmid(file,pos+1,strlen(file)-pos)

	ftype = strlowcase(ftype)

        CASE ftype OF
        'txt': begin
		idata = read_ascii(file,comment_symbol=';')
		nm = tag_names(idata)
		if nm(0) eq 'FIELD01' then im2 = transpose(idata.field01) 
		if nm(0) eq 'FIELD1' then im2 = transpose(idata.field1)
		if nm(0) eq 'FIELD001' then im2 = transpose(idata.field001) 
		loadct, 39
		TVLCT,R,G,B,/GET
               end
        'xdr': begin
		catch,error_status
		if error_status ne 0 then begin
			print,!err_status
			goto,closexdr
		end
		xdr_open,unit,file
		xdr_read,unit,im2
		xdr_read,unit,ranges
	   closexdr:
		xdr_close,unit
		loadct, 39
		TVLCT,R,G,B,/GET
		end
        'tiff': begin
                im2 = read_tiff(file,R,G,B)
		; if 16 bit tiff file r=0,g=0,b=0 is returned
                if n_elements(R) gt 1 then TVLCT,R,G,B 
               end
        'tif': begin
                im2 = read_tiff(file,R,G,B)
		; if 16 bit tiff file r=0,g=0,b=0 is returned
                if n_elements(R) gt 1 then TVLCT,R,G,B
               end
        'png': begin
                ok = QUERY_PNG(file,s)
                IF (ok) THEN BEGIN
                IF (s.HAS_PALETTE) THEN BEGIN
;                        READ_PNG,file,im2,R,G,B
                        im2 =READ_PNG(file,R,G,B)
                        TVLCT,R,G,B
                      ENDIF ELSE BEGIN
;                        READ_PNG,file,im2
                        im2 = READ_PNG(file)
                      ENDELSE
                   ENDIF
                end
        'jpg': begin
                read_jpeg,file,im2
		sz = size(im2)
		if sz(0) eq 2 then begin
		loadct,39
		tvlct,r,g,b,/get
		end
              end
        ELSE: begin
                print,'File type "',ftype, '" not supported'
                return
              end
        ENDCASE

        image = im2
        sz = size(im2)
;        if sz(0) eq 3 then device,decomposed=1 else device,decomposed=0
        if !d.n_colors eq 16777216 then device,decomposed=1 else device,decomposed=0

        if keyword_set(show) then begin
          if sz(0) eq 3 and sz(1) eq 3 then begin
                window,1,xsize=sz(2),ysize=sz(3)
                tv,im2,/true
          end
          if sz(0) eq 2 then begin
                window,1,xsize=sz(1),ysize=sz(2)
                tvlct,R,G,B
                tv,im2
          end
        end
END

PRO image_write,owindow,wid=wid,image=image,filename=filename,show=show,png=png,jpg=jpg,tif=tif,model=model,reverse=reverse
; true color graphic case
; owindow   - is for idlgrwindow object
; wid       - is the window id for direct graphic
; im        - option output image read from IDL window
; model=1   - color table mode, tvrd return 2 dim array
;             tv,image
; model=0   - true color mode, tvrd(ture=1) return 3 dim array
;             tv,image,/true
; 
        TVLCT,R,G,B,/GET
        if !d.n_colors le 256 then begin
        model=1
        end

        if n_params() eq 0 then begin
                ; direct graphic
                id = !d.window
                if keyword_set(wid) then id = wid
                wset,id
                if keyword_set(model) then image=tvrd() else $
                image = tvrd(true=1)
;                if keyword_set(jpg) then image=tvrd(true=1)  ; only true works for jpg
        endif else begin
                ; object graphic
                if obj_valid(owindow) then begin
                myimage = owindow->read()
                myimage->getproperty, data=image
                end
        end

;help,image,model
; print,max(image),min(image)
        sz = size(image)

        if keyword_set(show) then begin
        if sz(0) eq 3 and sz(1) eq 3 then begin
                window,0,xsize=sz(2),ysize=sz(3)
		device,decomposed=1
                tv,image,/true
		device,decomposed=0
                end
        if sz(0) eq 2 then begin
                window,0,xsize=sz(1),ysize=sz(2)
                tvlct,R,G,B
                tv,image
                end
        end

        ; write tif/jpeg/png file


        if keyword_set(jpg) then begin
        file = 'myfile.jpg'
        if keyword_set(filename) then file=filename
        if sz(0) eq 3 then write_jpeg,file,image,/true else $
                write_jpeg,file,image 
        return
        end

        if keyword_set(png) then begin
        file = 'myfile.png'
        if keyword_set(filename) then file=filename
        if sz(0) eq 3 then begin
		device,decomposed=1
		write_png,file,image 
		device,decomposed=0
	endif else $
                write_png,file,image,R,G,B 
        return
        end

        if keyword_set(tif) then begin
        file = 'myfile.tif'
        if keyword_set(filename) then file=filename
	if keyword_set(reverse) then begin
        	if sz(0) eq 3 then $
			write_tiff,file,reverse(image,3) else $
			write_tiff,file,reverse(image,2),red=R,green=G,blue=B
	endif else begin
        if sz(0) eq 3 then begin
		device,decomposed=1
		write_tiff,file,image,1,red=R,green=G,blue=B 
		device,decomposed=0
	endif else $ 
                write_tiff,file,image,red=R,green=G,blue=B ;,1
        end
        end

END

PRO image_write_drv,image,R,G,B,filename=filename,tif=tif,png=png,jpg=jpg
help,filename,image,r,g,b
; help,tif,png,jpeg

        sz = size(image)

        if keyword_set(jpg) then begin
        file = 'myfile.jpg'
        if keyword_set(filename) then file=filename
        if sz(0) eq 3 then write_jpeg,file,image,/true else $
                write_jpeg,file,image 
        return
        end

        if keyword_set(png) then begin
        file = 'myfile.png'
        if keyword_set(filename) then file=filename
        if sz(0) eq 3 then write_png,file,image else $
                write_png,file,image,R,G,B 
        return
        end

        if keyword_set(tif) then begin
        file = 'myfile.tif'
        if keyword_set(filename) then file=filename
        if sz(0) eq 3 then write_tiff,file,reverse(image,2) else $
                write_tiff,file,reverse(image),red=R,green=G,blue=B  ;,1
        end

END

PRO oprinter_setup,myprinter,model=model,units=units,dimensions=dimensions
        index = 0    ; RGB true color
        if keyword_set(model) then index = 1    ; color table
        if obj_valid(myprinter) eq 0 then $
        myprinter = obj_new('idlgrprinter',color_model=model,units=units)
        r = dialog_printersetup(myprinter)
END

;
; currently it can not assign the size of the printer output
;
PRO oprinter_job,myview,myprinter
; send the myview object to myprinter object
;  myview     - required input, view or scene objects
;  myprinter  - printer object
;
        if obj_isa(myview,'IDLgrView') or obj_isa(myview,'IDLgrScene') then begin
        if obj_valid(myprinter) eq 0 then $
        oprinter_setup,myprinter

;       r = dialog_printjob(myprinter) 
;       if r eq 0 then return

        WIDGET_CONTROL,HOURGLASS=1
        myprinter->draw,myview
        myprinter->newdocument
        WIDGET_CONTROL,HOURGLASS=0
        end
END

;
; IDL Event Callback Procedures
; img_eventcb
;
; Generated on: 08/16/2001 11:41.28
;
; @testobj.pro
; @PS_open.pro
; @img_eventcb
; @img
;-----------------------------------------------------------------
pro OnPrinter, Event
	PS_printer
end

;-----------------------------------------------------------------
pro OnPrint, Event
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

	TVLCT,red,green,blue,/get
	r_orig = red
	g_orig = green
	b_orig = blue

           ; Find the draw widget, which is named Draw.
           wDraw = WIDGET_INFO(Event.top, FIND_BY_UNAME='WID_DRAW_0');

           ; Make sure something was found.
           IF(wDraw GT 0)THEN BEGIN
              ; Make the draw widget the current, active window.
              WIDGET_CONTROL, wDraw, GET_VALUE=idDraw
        WSET,idDraw
	arr = TVRD()
	sz = size(arr)
	xs = sz(1)
	ys = sz(2)
	width = float(xs)/40
	height = float(ys)/40
;	PS_open,'idl.ps',/TV
	set_plot,'PS'
	device,filename='idl.ps',/color,bits=8, $
		/Courier,/Bold, scale_factor=1.1, $
		xsize=width,ysize=height
	TV,arr
	PS_close
	PS_print, 'idl.ps'
	END
end

;-----------------------------------------------------------------
pro OnPrinter2, Event
        WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
        model = img_state.model
        myprinter = img_state.printer
        oprinter_setup,myprinter,model=model
        img_state.printer = myprinter
        WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY
end
;-----------------------------------------------------------------
pro OnPrint2, Event,original=original
        WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
        width = img_state.xsize
        height = img_state.ysize
        dimensions=[width,height]
        oprinter = img_state.printer
        if obj_valid(oprinter) eq 0 then begin
        oprinter_setup,oprinter,model=model
        img_state.printer = oprinter
        end
if keyword_set(original) then begin
        data = *img_state.im
        dimensions=[float(img_state.WD)/!d.x_px_cm,float(img_state.HT)/!d.y_px_cm]
        tvlct,img_state.R,img_state.G,img_state.B
endif else begin
        dimensions=[float(width)/!d.x_px_cm,float(height)/!d.y_px_cm]
        data = tvrd(true=1)
end
        WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY

        oview = obj_new('idlgrview', $
                units=2, $ ; centimeters
                dimensions=dimensions, $ 
                viewplane_rect=[0,0,width,height])
        omodel = obj_new('idlgrmodel')
        oimage = obj_new('idlgrimage',data)
        omodel->add,oimage
        oview->add,omodel

        oprinter_job,oview,oprinter

;       WIDGET_CONTROL,hourglass=1
;       oprinter->draw,oview
;       oprinter->newdocument
;       WIDGET_CONTROL,hourglass=0

end
;-----------------------------------------------------------------
pro OnExit, Event
     WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
	vmin = img_state.vmin
	vmax = img_state.vmax
	xmin = img_state.xmin
	xmax = img_state.xmax
	ymin = img_state.ymin
	ymax = img_state.ymax
	path = img_state.path
     WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY

	catch,error_status
	if error_status ne 0 then goto,close_config
	openw,2,'img.config'
	printf,2,vmax,vmin,xmin,xmax,ymin,ymax
	printf,2,path
close_config:
	close,2
        WIDGET_CONTROL, Event.top, /DESTROY
end
;-----------------------------------------------------------------
pro OnColor, Event
	XLOADCT,GROUP=Event.top
end
;
; Empty stub procedure used for autoloading.
;
pro img_eventcb
end
;-----------------------------------------------------------------
PRO OnOverlay, Event
	WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
	path = img_state.path
	WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY
	scan2d_overlay,path=path
END
;-----------------------------------------------------------------
PRO OnDrawCursor, Event
     WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY

	if Event.RELEASE eq 1 then begin
		img_state.xl = Event.X
		img_state.yl = Event.Y
	end
	if Event.RELEASE eq 4 then begin
		xr = Event.X
		yr = Event.Y
	if xr lt img_state.xl then begin
		img_state.xr = img_state.xl
		img_state.xl = xr
	endif else img_state.xr = xr
	if yr lt img_state.yl then begin
		img_state.yr = img_state.yl
		img_state.yl = yr
	endif else img_state.yr = yr
	end

	if XRegistered('image_extract') then begin 
		ml = widget_info(/managed)
		wl = widget_info(ml,FIND_BY_UNAME='IMAGE_EXTRACT')
		id = where(wl gt 0)
		if id(0) ne -1 then begin
		exWID = wl(id(0)) 
		widget_control,exWID,GET_UVALUE=ex_state,/NO_COPY
		widget_control,ex_state.xlWID,set_value=img_state.xl
		widget_control,ex_state.ylWID,set_value=img_state.yl
		widget_control,ex_state.xrWID,set_value=img_state.xr
		widget_control,ex_state.yrWID,set_value=img_state.yr
		widget_control,ex_state.wdWID, $
			set_value=(img_state.xr - img_state.xl)+1
		widget_control,ex_state.htWID, $
			set_value=(img_state.yr - img_state.yl)+1
	   if Event.RELEASE eq 2 then begin
		str = 'Pixels @ Cursor: X='+strtrim(Event.X,2) $
			+',  Y='+strtrim(Event.Y,2)
		widget_control,ex_state.cursor,set_value=str
	   end
		widget_control,exWID,SET_UVALUE=ex_state,/NO_COPY
		end
	end

     WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY

     if Event.RELEASE eq 4 and XRegistered('image_extract') eq 0 then $ 
	OnImageCalc,Event
END
;-----------------------------------------------------------------
PRO OnImageCalc, Event
     WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
	xl = img_state.xl
	yl = img_state.yl
	xr = img_state.xr
	yr = img_state.yr
     WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY

	; extract raw data
	imgReadScreen,im	
	image_extract,im,xl=xl,yl=yl,xr=xr,yr=yr, $
		Group=Event.Top
END
;-----------------------------------------------------------------
PRO OnBaseResize, Event

	if XRegistered('image_extract') then image_extract_destroy 

           ; Find the draw widget, which is named Draw.
           wDraw = WIDGET_INFO(Event.top, FIND_BY_UNAME='WID_DRAW_0');

	imgReadScreen,im	
        WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
if n_elements(*img_state.im) gt 0  then begin
        img_state.xsize = event.X
        img_state.ysize = event.Y

           ; Make sure something was found.
           IF(wDraw GT 0)THEN BEGIN
              ; Make the draw widget the current, active window.
              WIDGET_CONTROL, wDraw, GET_VALUE=idDraw
              WIDGET_CONTROL, wDraw, scr_xsize=event.x,scr_ysize=event.y
              WSET,idDraw

              im = CONGRID(im, event.X, event.Y)
              ; Display the image.
	if img_state.ftype eq 'xdr' or img_state.ftype eq 'txt' then tvscl,im else $
              TV, im
           END
end
      WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY
END
;-----------------------------------------------------------------
pro OnOpen, Event
; If there is a file, draw it to the draw widget.

ftype=''
WIDGET_CONTROL,event.top,GET_UVALUE=img_state,/NO_COPY

reg = ['*.jpg','*.tif*','*.png','*.xdr','*.txt']
catch,error_status
if error_status ne 0 then reg = '*.*'

	sFile = DIALOG_PICKFILE(FILTER=reg, $
		PATH=img_state.path,GET_PATH=path)

        IF sFile eq path THEN GOTO,NOPICKFILE 

           ; Find the draw widget, which is named Draw.
           wDraw = WIDGET_INFO(Event.top, FIND_BY_UNAME='WID_DRAW_0');

           ; Make sure something was found.
           IF(wDraw GT 0)THEN BEGIN
              ; Make the draw widget the current, active window.
              WIDGET_CONTROL, wDraw, GET_VALUE=idDraw
              WSET,idDraw

	WIDGET_CONTROL,/HOURGLASS
		r=0
		b=0
		g=0
              ; Read in the image.
              image_read,sFile,image=im,ranges=ranges,red=r,green=g,blue=b,ftype=ftype  ;,/show
	if img_state.file eq '' then begin
	WIDGET_CONTROL,img_state.menu1WID,SENSITIVE=1
	WIDGET_CONTROL,img_state.menu2WID,SENSITIVE=1
	WIDGET_CONTROL,img_state.menu3WID,SENSITIVE=1
	end

if n_elements(ranges) gt 1 and ftype eq 'xdr' then begin
        img_state.xmin = ranges(0)
        img_state.xmax = ranges(1)
        img_state.ymin = ranges(2)
        img_state.ymax = ranges(3)
        img_state.vmin = ranges(4)
        img_state.vmax = ranges(5)
end
              ; If TrueColor image, quantize image to pseudo-color:

	img_state.ftype = ftype
      sz = size(im)
        img_state.file = sFile
        img_state.path = path
        if sz(0) eq 3 then begin
        img_state.WD = sz(2)
        img_state.HT = sz(3)
        img_state.MODEL = 0
        end
        if sz(0) eq 2 then begin
        img_state.WD = sz(1)
        img_state.HT = sz(2)
        img_state.MODEL = 1
        end
      IF (SIZE(im, /N_DIM) EQ 3) THEN $
              im = COLOR_QUAN(im, 1, r, g, b)
        *img_state.IM = im

if n_elements(r) gt 1 then begin
        img_state.R = R
        img_state.G = G
        img_state.B = B
end
              ; Size the image to fill the draw area.
              im = CONGRID(im, !D.X_SIZE, !D.Y_SIZE)
              ; Handle TrueColor displays:
              DEVICE, DECOMPOSED=0
              ; Load color table, if one exists:
              IF (n_elements(r) GT 0) THEN TVLCT, r, g, b

              ; Display the image.
	if ftype eq 'txt'  or ftype eq 'xdr' then tvscl,im else $
              TV, im

   ENDIF

NOPICKFILE:
; Save the image in the uvalue of the top-level base.
        WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY

	if XRegistered('image_extract') then image_extract_destroy

end

;-----------------------------------------------------------------
pro OnSavePNG, Event, original=original
        WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
	path = img_state.path
        WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY
sFile = DIALOG_PICKFILE(FILTER='*.png',/write,path=path,get_path=p)
	if sFile eq p then return

; Find the draw widget, which is named Draw:
        wDraw = WIDGET_INFO(Event.top, FIND_BY_UNAME='WID_DRAW_0')
        IF(wDraw GT 0) THEN BEGIN
           ; Make the draw widget the current, active window:
           WIDGET_CONTROL, wDraw, GET_VALUE=idDraw

        image_write,wid=idDraw,/png,filename=sFile
        END
end
;-----------------------------------------------------------------

pro OnSaveJPEG, Event, original=original

        WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
	path = img_state.path
        WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY
sFile = DIALOG_PICKFILE(FILTER='*.jpg',/write,path=path,get_path=p)
	if sFile eq p then return

; Find the draw widget, which is named Draw:
        wDraw = WIDGET_INFO(Event.top, FIND_BY_UNAME='WID_DRAW_0')
        IF(wDraw GT 0) THEN BEGIN
           ; Make the draw widget the current, active window:
           WIDGET_CONTROL, wDraw, GET_VALUE=idDraw

        image_write,wid=idDraw,/jpg,filename=sFile
        END
end
;-----------------------------------------------------------------
pro OnSaveTIFF, Event, original=original,reverse=reverse

        WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
	path = img_state.path
        WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY
sFile = DIALOG_PICKFILE(FILTER='*.tif*',/write,path=path,get_path=p)
	if sFile eq p then return

; Find the draw widget, which is named Draw:
        wDraw = WIDGET_INFO(Event.top, FIND_BY_UNAME='WID_DRAW_0')
        IF(wDraw GT 0) THEN BEGIN
           ; Make the draw widget the current, active window:
           WIDGET_CONTROL, wDraw, GET_VALUE=idDraw

        image_write,wid=idDraw,/tif,filename=sFile,reverse=reverse ;,/show
        END
end
;-----------------------------------------------------------------
pro OnSaveXDR, Event, original=original,reverse=reverse
; save raw image only
        WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
	path = img_state.path
        WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY
sFile = DIALOG_PICKFILE(FILTER='*.xdr*',/write,path=path,get_path=p)
	if sFile eq p then return

; Find the draw widget, which is named Draw:
        wDraw = WIDGET_INFO(Event.top, FIND_BY_UNAME='WID_DRAW_0')
        IF(wDraw GT 0) THEN BEGIN
           ; Make the draw widget the current, active window:
           WIDGET_CONTROL, wDraw, GET_VALUE=idDraw

        WIDGET_CONTROL,Event.Top,GET_UVALUE=img_state,/NO_COPY
	ranges=[img_state.xmin,img_state.xmax,img_state.ymin,img_state.ymax, $
		img_state.vmin,img_state.vmax]
	im = *img_state.im
        WIDGET_CONTROL,Event.Top,SET_UVALUE=img_state,/NO_COPY
	xdr_open,unit,sFile,/write
	xdr_write,unit,im
	xdr_write,unit,ranges
	xdr_close,unit
        END
end
;-----------------------------------------------------------------
PRO OnInputData, Event
        WIDGET_CONTROL,Event.Top,GET_UVALUE=img_state,/NO_COPY
        img_state.original = 1
        WIDGET_CONTROL,Event.Top,SET_UVALUE=img_state,/NO_COPY
END
;-----------------------------------------------------------------
PRO OnReadTVScreen, Event
        WIDGET_CONTROL,Event.Top,GET_UVALUE=img_state,/NO_COPY
        img_state.original = 0
        WIDGET_CONTROL,Event.Top,SET_UVALUE=img_state,/NO_COPY
END
;-----------------------------------------------------------------
pro OnExportJPG, Event
; there is problem with write_jpeg on 2D image data
; only works true image byte data array
        WIDGET_CONTROL,Event.Top,GET_UVALUE=img_state,/NO_COPY
        image = *img_state.im
        R = img_state.R
        G = img_state.G
        B = img_state.B
	path = img_state.path
	MODEL = img_state.MODEL
        WIDGET_CONTROL,Event.Top,SET_UVALUE=img_state,/NO_COPY
        sz = size(image)
	if MODEL eq 0 then begin
	ret = dialog_message('Error: SaveRawData in JPEG only works for 2D data',/error)
	return
	end
        sFile = DIALOG_PICKFILE(FILTER='*.jpg',/write,path=path,get_path=p)
	if sFile eq p  then return

	; scale the 2D data with color table
	type = sz[n_elements(sz)-2]
	if sz(0) eq 2 and type ne 1 then begin
	vmin = min(image)
	vmax = max(image)
	image = byte((!d.table_size-1)*(image-vmin)/(vmax-vmin))
	end

        if strpos(sFile,'.jpg') gt 0 then begin
        if sz(0) eq 3 then write_jpeg,sFile,image,/true else $
        write_jpeg,sFile,image, /progressive 
        end

end
;-----------------------------------------------------------------
pro OnExportPNG, Event
        WIDGET_CONTROL,Event.Top,GET_UVALUE=img_state,/NO_COPY
        image = *img_state.im
        R = img_state.R
        G = img_state.G
        B = img_state.B
	path = img_state.path
        WIDGET_CONTROL,Event.Top,SET_UVALUE=img_state,/NO_COPY
        sz = size(image)
        sFile = DIALOG_PICKFILE(FILTER='*.png',/write,path=path,get_path=p)
	if sFile eq p  then return

	; scale the 2D data with color table
	type = sz[n_elements(sz)-2]
	if sz(0) eq 2 and type ne 1 then begin
	vmin = min(image)
	vmax = max(image)
	image = ceil((!d.table_size-1)*(image-vmin)/(vmax-vmin))
	end

        if strpos(sFile,'.png') gt 0 then begin
        if sz(0) eq 3 then write_png,sFile,image else $
                write_png,sFile,image,R,G,B
        end

end
;-----------------------------------------------------------------
pro OnExportTIF, Event, reverse=reverse
; export raw object image
;
        WIDGET_CONTROL,Event.Top,GET_UVALUE=img_state,/NO_COPY
        image = *img_state.im
        R = img_state.R
        G = img_state.G
        B = img_state.B
	path = img_state.path
        WIDGET_CONTROL,Event.Top,SET_UVALUE=img_state,/NO_COPY
        sz = size(image)
        sFile = DIALOG_PICKFILE(FILTER='*.tif*',/write,path=path,get_path=p)
	if sFile eq p  then return
        if strpos(sFile,'.tif') gt 0 then begin

	; scale the 2D data with color table
	type = sz[n_elements(sz)-2]
	if sz(0) eq 2 and type ne 1 then begin
	vmin = min(image)
	vmax = max(image)
	image = ceil((!d.table_size-1)*(image-vmin)/(vmax-vmin))
	end

	if keyword_set(reverse) then begin
	  if sz(0) eq 3 then write_tiff,sFile,reverse(image,3) else $
                write_tiff,sFile,reverse(image,2),red=R,green=G,blue=B  
	endif else begin
          if sz(0) eq 3 then begin
		write_tiff,sFile,image,1 
		device,decompose=0
		endif else $
                write_tiff,sFile,image,red=R,green=G,blue=B  
	end
	end

end
;-----------------------------------------------------------------
pro OnTVImageROI,Event
	if !d.n_colors gt 256 then begin
		OnScaleTVImage,Event
	return
	end

	imgReadScreen,im	
	sz = size(im)
     WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
	ftype = img_state.ftype
	xl = img_state.xl
	yl = img_state.yl
	xr = img_state.xr
	yr = img_state.yr
        wd = img_state.xsize
        ht = img_state.ysize
	wid = img_state.WID
        original = img_state.original
     WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY

	wd = xr-xl+1
	ht = yr-yl+1
	if wd lt 2 or ht lt 2 then return
	if xr gt sz(1) then xr = sz(1)-1
	if yr gt sz(2) then yr = sz(2)-1
	wDraw = WIDGET_INFO(Event.top,FIND_BY_UNAME='WID_DRAW_0')
	widget_control,wDraw,scr_xsize=wd,scr_ysize=ht
	WSET,wid
	erase
	im = im(xl:xr,yl:yr)
	im = congrid(im,wd,ht)
	if ftype eq 'xdr' or ftype eq 'txt' then tvscl,im else $
	TV,im
	if XRegistered('image_extract') then image_extract_destroy 
end
;-----------------------------------------------------------------
pro OnRestoreOldCLT, Event
     WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
	R = img_state.R
	G = img_state.G
	B = img_state.B
     WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY
	TVLCT,R,G,B
end
;-----------------------------------------------------------------
pro OnRawTVImage, Event
     WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
        im = *img_state.im

	img_state.original = 1
	sz = size(im)
	wDraw = WIDGET_INFO(Event.top, FIND_BY_UNAME='WID_DRAW_0');
	WIDGET_CONTROL,wDraw,SCR_XSIZE=sz(1),SCR_YSIZE=sz(2)
	r = img_state.R
	g = img_state.G
	b = img_state.B
	wid = img_state.WID
	ftype = img_state.ftype
     WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY

	WSET,wid 
	xl = 80 ;img_state.xl
	yl = 50 ;img_state.yl
	xr = 380 ;img_state.xr
	yr = 350 ;img_state.yr
      IF (SIZE(im, /N_DIM) EQ 3) THEN $
              im = COLOR_QUAN(im, 1, r, g, b)
              ; Size the image to fill the draw area.
             ; im = CONGRID(im, wd, ht)
              ; Handle TrueColor displays:
              DEVICE, DECOMPOSED=0
        erase
              ; Load color table, if one exists:
              IF (N_ELEMENTS(r) GT 0) THEN TVLCT, r, g, b
	if ftype eq 'xdr' or ftype eq 'txt' then tvscl,im else $
              TV, im

; extract raw data

	sz = size(im)
	if sz(1) lt xr or sz(2) lt yl then begin
		xl=-1
		xr = sz(1)
		yl=-1
		yr = sz(2)
	end
	image_extract,im,xl=xl,yl=yl,xr=xr,yr=yr,Group=Event.Top

end
;-----------------------------------------------------------------
pro OnScaleTVImage, Event
     WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
        im = *img_state.im
        wd = img_state.xsize
        ht = img_state.ysize
	ftype = img_state.ftype
	WID = img_state.WID
	img_state.original = 0
     WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY
wDraw = WIDGET_INFO(Event.top, FIND_BY_UNAME='WID_DRAW_0')
WIDGET_CONTROL,wDraw,SCR_XSIZE=wd,SCR_YSIZE=ht
WSET,WID
      IF (SIZE(im, /N_DIM) EQ 3) THEN $
              im = COLOR_QUAN(im, 1, r, g, b)
              ; Size the image to fill the draw area.
              im = CONGRID(im, wd, ht)
              ; Handle TrueColor displays:
              DEVICE, DECOMPOSED=0
              ; Load color table, if one exists:
              IF (N_ELEMENTS(r) GT 0) THEN TVLCT, r, g, b
              ; Display the image.
	if ftype eq 'xdr' or ftype eq 'txt' then tvscl,im else $
              TV, im

	image_extract,im, Group=Event.Top
end
;-----------------------------------------------------------------
pro OnBottomUpTVImage, Event
	imgReadScreen,im
        TV, reverse(im,2)
end
;-----------------------------------------------------------------
pro OnMirrorTVImage, Event
	imgReadScreen,im
        TV, reverse(im)
end
;-----------------------------------------------------------------
pro OnTransposeTVImage, Event
	imgReadScreen,im
	sz = size(im)
	wd = sz(1)
	ht = sz(2)
	wDraw = WIDGET_INFO(Event.top, FIND_BY_UNAME='WID_DRAW_0')
	WIDGET_CONTROL,wDraw,SCR_XSIZE=ht,SCR_YSIZE=wd
     WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
        img_state.xsize = ht
        img_state.ysize = wd
	wid = img_state.wid
     WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY
	im1 = transpose(im)
	WSET,WID
        TV, im1
end
;-----------------------------------------------------------------
pro imgReadScreen,im,r,g,b
	if !d.n_colors eq 16777216 then begin
              ; Handle TrueColor displays:
;              DEVICE, DECOMPOSED=0
		im0 = tvrd(/true)
		im = color_quan(im0,1,r,g,b)
		tvlct,r,g,b
	endif else im = tvrd()
end
;-----------------------------------------------------------------
pro OnReduceTVImage0, Event
	imgReadScreen,im
	sz = size(im)
	wd = sz(1)/4 * 3
	ht = sz(2)/4 * 3
	wDraw = WIDGET_INFO(Event.top, FIND_BY_UNAME='WID_DRAW_0')
	WIDGET_CONTROL,wDraw,SCR_XSIZE=wd,SCR_YSIZE=ht
     WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
        img_state.xsize = wd
        img_state.ysize = ht
	ftype = img_state.ftype
	WSET,img_state.WID
     WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY

              ; Size the image to fill the draw area.
              im = CONGRID(im, wd, ht)
              ; Load color table, if one exists:
	erase
	if ftype eq 'xdr' or ftype eq 'txt' then tvscl,im else $
              TV, im
end
;-----------------------------------------------------------------
pro OnReduceTVImage, Event
	imgReadScreen,im
	sz = size(im)
	wd = sz(1)/2
	ht = sz(2)/2
	wDraw = WIDGET_INFO(Event.top, FIND_BY_UNAME='WID_DRAW_0')
	WIDGET_CONTROL,wDraw,SCR_XSIZE=wd,SCR_YSIZE=ht
     WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
        img_state.xsize = wd
        img_state.ysize = ht
	ftype = img_state.ftype
	WSET,img_state.WID
     WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY

              ; Size the image to fill the draw area.
              im = CONGRID(im, wd, ht)
              ; Load color table, if one exists:
	erase
	if ftype eq 'xdr' or ftype eq 'txt' then tvscl,im else $
              TV, im
end
;-----------------------------------------------------------------
pro OnExpandTVImage, Event
	imgReadScreen,im
	sz = size(im)
	wd = sz(1) * 2
	ht = sz(2) * 2
	wDraw = WIDGET_INFO(Event.top, FIND_BY_UNAME='WID_DRAW_0')
;	WIDGET_CONTROL,wDraw,DRAW_XSIZE=wd,DRAW_YSIZE=ht
	WIDGET_CONTROL,wDraw,SCR_XSIZE=wd,SCR_YSIZE=ht
     WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
	img_state.xsize = wd
	img_state.ysize = ht 
	ftype = img_state.ftype
	WSET,img_state.WID
     WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY
              ; Size the image to fill the draw area.
              im = CONGRID(im, wd, ht)
              ; Display the image.
	if ftype eq 'xdr' or ftype eq 'txt' then tvscl,im else $
              TV, im
end
;-----------------------------------------------------------------
pro OnExpandTVImage0, Event
	imgReadScreen,im
	sz = size(im)
	wd = sz(1) * 5/4 
	ht = sz(2) * 5/4 
	wDraw = WIDGET_INFO(Event.top, FIND_BY_UNAME='WID_DRAW_0')
;	WIDGET_CONTROL,wDraw,DRAW_XSIZE=wd,DRAW_YSIZE=ht
	WIDGET_CONTROL,wDraw,SCR_XSIZE=wd,SCR_YSIZE=ht
     WIDGET_CONTROL, event.top, GET_UVALUE=img_state, /NO_COPY
	img_state.xsize = wd
	img_state.ysize = ht 
	ftype = img_state.ftype
	WSET,img_state.WID
     WIDGET_CONTROL, event.top, SET_UVALUE=img_state, /NO_COPY
              ; Size the image to fill the draw area.
              im = CONGRID(im, wd, ht)
              ; Display the image.
	if ftype eq 'xdr' or ftype eq 'txt' then tvscl,im else $
              TV, im
end
;-----------------------------------------------------------------
pro OnHelpExtract, Event
  str = [ ' HELP on Extract 2D ASCII Data','', $
	'There are two methods of extracting 2D image data from the', $
	'drawing area:', '',$
	'Method 1 - Views->Extract Image (Original)', '',$
	'Method 2 - ', $
	'     Click Left Mouse Button to pick lower left corner', $
	'     Click Right Mouse Button to pick upper right corner ', $
	'', $
	'    (Both methods pops up the Image Calculation dialog)', $
	'Steps for extracting image data from the drawing area:','',$
	'* Enter XL,XR,YL,YR pixel ranges for 2D image', $
	'* Enter real data dimensions: Width, Height ', $
	'* Enter the real known data range: MAX, MIN values',$
	'* Click the Accept... button on the Image Calcution dialog', $
	'* Click the Data... button on the PLOT2D program','', $
	'    (2D image data is saved in the default file plot2d.txt,', $
	'     which is renamable by the user)','', $
	'    (The calculated result is an approximation of the original data', $
	'     based on the MAX, MIN and image information and may be ',$
	'     not exact same as the origianl data)']
  xdisplayfile,text=str,title='HELP ON EXTRACT 2D DATA'
end
;-----------------------------------------------------------------
pro OnHelpImg, Event
  str = [ ' HELP on IMG - Image Process program', $
	'','File Munu', $
	'  Open...    - Read in  any .tif, .png, .jpg type files', $
	'  Printer... - Set up PostScript printer for IDL', $
	'  Print      - Send image to printer', $
	'  Exit       - Close IMG program', $
	'','Views Menu',$
	'  Bottom-Top Mirror Image  - Flip TV image bottom to top', $
	'  Left-Right Mirror Image  - Flip TV image Left to Right ', $
	'  Reduce Image by Half     - Image Shrink by half ', $
	'  Expand Image by Two      - Image Expand by two ', $
	'  Extract Image (Scaled)    - Original image data scaled to window size', $
	'  Extract Image (Original)  - Original read in raw data image', $
	'  Show Extracted ROI Only   - Display extracted ROI only', $
	'                       (Before select this item, the ', $
	'                        ROI must be first defined by:', $
	'                        Use LMB to pick lower left corner',$
	'                        Use RMB to pick upper right corner )',$
	'  Color Table...      - Call IDL xloadct program', $
	'','SaveTVRD    (Note: resize the image saved)',$
	'  PNG    - Read current image display and write in PNG format', $
	'  TIFF   - Read current image display and write in TIFF format', $
	'  TIFF-R - Read current image display and write TIFF in reverse order', $
	'  JPEG   - Read current image display and write in JPEG format', $
	'','SaveRawData',$
	'  PNG    - Write raw image data in PNG format', $
	'  TIFF   - Write raw image data in TIFF format', $
	'  TIFF-R - Write raw image data in TIFF format in reverse order', $
	'  JPEG   - Write raw image data in JPEG format', $
	'','HELP',$
	'  HELP...  - Pops up this on-line help', $
	'']
;	r = dialog_message(str,/info,title='Help IMG')
	xdisplayfile,text=str,title='HELP ON IMG'
end

;-----------------------------------------------------------------
pro OnHelpFileName, Event
  WIDGET_CONTROL,Event.Top,GET_UVALUE=img_state,/NO_COPY
        filename = img_state.file
  WIDGET_CONTROL,Event.Top,SET_UVALUE=img_state,/NO_COPY
  str = ['', 'Current File Opened : ','    '+ filename,'', $
	'This program can read following types of image file:','', $
	'*.txt          - ASCII 2D image data', $
	'*.xdr          - XDR 2D image data', $
	'*.tiff	        - TIFF file of 8 bit or 24 bit', $
	'*.jpg          - JPG graphic file', $
	'*.png          - PNG graphic file', $
	'', $
	'For an Image(W,H) array the ASCII file should contains the following ', $
	'       Comment line should start with a ; at column one', $
	'       Each line should contain H number of Y values and', $	
	'       Each line is terminated by a carriage return', $
	'       The file itself should contain a total of W lines','', $
	'For XDR data file it should be preprared by the xdr_open program', $
	'       For examaple save the Image(W,H) array in plot2d.xdr file ',$
	'       xdr_open,unit,"plot2d.xdr"', $
	'       xdr_write,unit,Image', $
	'       xdr_write,unit,[Xmin,Xmax,Ymin,Ymax,Vmin,Vmax]', $
	'       xdr_close,unit''']
	xdisplayfile,text=str,title='HELP ON File Name'
end
;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.




PRO image_extract_Event, Event

  WIDGET_CONTROL, Event.top, GET_UVALUE=approxim_state,/NO_COPY

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'APPROXM_XL': BEGIN
      END
  'APPROXM_XR': BEGIN
      END
  'APPROXM_YL': BEGIN
      END
  'APPROXM_YR': BEGIN
      END
  'APPROXM_XNPTS': BEGIN
      END
  'APPROXM_YNPTS': BEGIN
      END
  'APPROXM_MAX': BEGIN
      END
  'APPROXM_MIN': BEGIN
      END
  'APPROXM_XMIN': BEGIN
      END
  'APPROXM_XMAX': BEGIN
      END
  'APPROXM_YMIN': BEGIN
      END
  'APPROXM_YMAX': BEGIN
      END
  'APPROXIM_ACCEPT': BEGIN
	WIDGET_CONTROL,approxim_state.xlWID,GET_VALUE=xl
	approxim_state.xl = xl 
	WIDGET_CONTROL,approxim_state.xrWID,GET_VALUE=xr
	approxim_state.xr = xr
	WIDGET_CONTROL,approxim_state.ylWID,GET_VALUE=yl
	approxim_state.yl = yl
	WIDGET_CONTROL,approxim_state.yrWID,GET_VALUE=yr
	approxim_state.yr = yr
	WIDGET_CONTROL,approxim_state.wdWID,GET_VALUE=wd
	approxim_state.width = wd
	WIDGET_CONTROL,approxim_state.htWID,GET_VALUE=ht
	approxim_state.height = ht
	WIDGET_CONTROL,approxim_state.maxWID,GET_VALUE=vmax
	approxim_state.max = vmax
	WIDGET_CONTROL,approxim_state.minWID,GET_VALUE=vmin
	approxim_state.min = vmin
	WIDGET_CONTROL,approxim_state.xminWID,GET_VALUE=xmin
	approxim_state.xmin = xmin
	WIDGET_CONTROL,approxim_state.xmaxWID,GET_VALUE=xmax
	approxim_state.xmax = xmax
	WIDGET_CONTROL,approxim_state.yminWID,GET_VALUE=ymin
	approxim_state.ymin = ymin
	WIDGET_CONTROL,approxim_state.ymaxWID,GET_VALUE=ymax
	approxim_state.ymax = ymax

	image2 = approxim_state.image
	sz = size(image2)
	if xr gt sz(1) or yr gt sz(2) or (xr-xl) gt sz(1) or (yr-yl) gt sz(2) then begin
		str = ['Please redefine the Image Region Of Interest','', $
			'LMB -  Left Mouse Button to pick lower left corner', $
			'RMB -  Right Mouse Button to pick upper right corner']
		r = dialog_message(str,/Error)
  		WIDGET_CONTROL, Event.top, SET_UVALUE=approxim_state,/NO_COPY
		return
	end

	widget_control,approxim_state.parent,GET_UVALUE=img_state,/no_copy
	model=img_state.model
	if model then $
	image_data_aprox,image2,xl,xr, yl,yr, vmax,vmin, $
		width=wd,height=ht, xmin=xmin,xmax=xmax, $
		ymin=ymin,ymax=ymax, /real, $
		group=Event.top $
	else begin
;	calc_image,wd,ht,image=image,vmax=vmax,vmin=vmin	
	r = dialog_message('2D Data Generation only available for 8 bit image ',/Error)

	end

	img_state.vmax = vmax
	img_state.vmin = vmin
	img_state.xmax = xmax
	img_state.xmin = xmin
	img_state.ymax = ymax
	img_state.ymin = ymin
	widget_control,approxim_state.parent,SET_UVALUE=img_state,/no_copy

      END

  'APPROXIM_HELP': BEGIN
	str = [ 'This dialog let the user to reconstruct the original 2D image data', $
	'from a TV image with user modifyable: XL,XR,YL,YR,Width,Height,Max,Min .', $
	'', $
	'Drawing Area Event:', $
	'            Left Mouse Button   - define start bound box', $
	'            Right Mouse Button  - define close bound box', $
	'            Middle Mouse Button - query the cursor pixel location', $
	'Pixels @ Cursor - label to show X,Y pixel at cursor ',$
	'XL, XR    - Specify the range of X pixels of the TV image', $
	'YL, YR    - Specify the range of Y pixels of the TV image', $
	'            (Define the TV image pixel ranges to be extracted)', $
	'WIDTH     - Specify the resized X dimension of the extracted',$
	'            image data', $
	'HEIGHT    - Specify the resized Y dimension of the extracted',$
	'            image data', $
	'Max Value - Specify the maximum value for the extract image', $  	
	'Min Value - Specify the minimum value for the extract image', $  	
	'Xmin      - Specify X axis begin value', $
	'Xmax      - Specify X axis end value', $
	'Ymin      - Specify Y axis begin value', $
	'Ymax      - Specify Y axis end value', $
	'Accept... - Calculated and plotted the extracted 2D image data', $
	'            (PLOT2D provides the DATA... and Pick1D... buttons', $
	'             to show 2D image data)', $
	'Help...   - Show this help page', $
	'Cancel    - Close this dialog']
	xdisplayfile,text=str,Group=Event.top
      END

  'APPROXIM_CANCEL': BEGIN
	WIDGET_CONTROL,Event.top,/DESTROY
	return
      END
  ENDCASE

  WIDGET_CONTROL, Event.top, SET_UVALUE=approxim_state,/NO_COPY

END

PRO image_extract_destroy
	ml = widget_info(/managed)
	wl = widget_info(ml,FIND_BY_UNAME='IMAGE_EXTRACT')
	id = where(wl gt 0)
	if id(0) ne -1 then begin
	widget_control,wl(id(0)),/destroy
	end
END

PRO image_extract,image2,xl=xl,yl=yl,xr=xr,yr=yr,xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,vmin=vmin,vmax=vmax,width=width,height=height,GROUP=Group

if XRegistered('image_extract') then image_extract_destroy

                ml = widget_info(/managed)
                wl = widget_info(ml,FIND_BY_UNAME='WID_BASE_0')
		id = where(wl gt 0)
                if id(0) ne -1 then begin
		wid = wl(id(0))
                widget_control,wid,GET_UVALUE=img_state,/no_copy
                endif else return

  parent = wid 
  sz = size(image2)
  xl = 0 	;img_state.xl
  xr = sz(1) 	;img_state.xr
  yl = 0 	;img_state.yl
  yr = sz(2) 	;img_state.yr
  vmin = img_state.vmin
  vmax = img_state.vmax
  x1 = img_state.xmin
  x2 = img_state.xmax
  y1 = img_state.ymin
  y2 = img_state.ymax
                widget_control,wid,SET_UVALUE=img_state,/no_copy

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  image_extract = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, UNAME='IMAGE_EXTRACT', $
      MAP=1, TITLE=' IMAGE CALCULATION', $
      UVALUE='image_extract')

  BASE2 = WIDGET_BASE(image_extract, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL3', $
      VALUE='EXTRACT TV IMAGE CALCULATION')

  LABEL3_1 = WIDGET_LABEL( BASE2, $
	/ALIGN_LEFT, VALUE='Query Pixels @ cursor use Middle Mouse Button:')

  BASE7 = WIDGET_BASE(BASE2, $
      ROW=1, /frame, $
      MAP=1, $
      UVALUE='BASE7')

  LABEL4 = WIDGET_LABEL( BASE7, $
      UVALUE='LABEL4', $
      VALUE='Pixel Ranges for Image: ')

  pix_xl = CW_FIELD( BASE7,VALUE=xl, $
      ROW=1, $
      INTEGER=1, $
      TITLE='XL', $
      UVALUE='APPROXM_XL', $
      XSIZE=4)

  pix_xr = CW_FIELD( BASE7,VALUE=xr, $
      ROW=1, $
      INTEGER=1, $
      TITLE='XR', $
      UVALUE='APPROXM_XR', $
      XSIZE=4)

  pix_yl = CW_FIELD( BASE7,VALUE=yl, $
      ROW=1, $
      INTEGER=1, $
      TITLE='YL', $
      UVALUE='APPROXM_YL', $
      XSIZE=4)

  pix_yr = CW_FIELD( BASE7,VALUE=yr, $
      ROW=1, $
      INTEGER=1, $
      TITLE='YR', $
      UVALUE='APPROXM_YR', $
      XSIZE=4)


  BASE20 = WIDGET_BASE(BASE2, $
      ROW=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE20')

  LABEL21 = WIDGET_LABEL( BASE20, $
      UVALUE='LABEL21', $
      VALUE='Image Data Resize Dimensions : ')

  wd = xr-xl
  if keyword_set(width) then wd=width
  NPTS_X = CW_FIELD( BASE20,VALUE=wd, $
      ROW=1, $
      INTEGER=1, $
      TITLE='WIDTH', $
      UVALUE='APPROXM_XNPTS', $
      XSIZE=4)

  ht = yr-yl
  if keyword_set(height) then ht=height
  NPTS_Y = CW_FIELD( BASE20,VALUE=ht, $
      ROW=1, $
      INTEGER=1, $
      TITLE='HEIGHT', $
      UVALUE='APPROXM_YNPTS', $
      XSIZE=4)


  BASE29 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE29')

  value_max = CW_FIELD( BASE29,VALUE=vmax, $
      ROW=1, $
      FLOAT=1, $
      TITLE='Image MAX Value', $
      UVALUE='APPROXM_MAX', $
      XSIZE=20)

  value_min = CW_FIELD( BASE29,VALUE=vmin, $
      ROW=1, $
      FLOAT=1, $
      TITLE='Image MIN Value', $
      UVALUE='APPROXM_MIN', $
      XSIZE=20)


  BASE49 = WIDGET_BASE(BASE2, $
      ROW=1, MAP=1, /frame, $
      UVALUE='BASE49')
  xmin = CW_FIELD( BASE49,VALUE=x1, $
      ROW=1, FLOAT=1, TITLE='AXIS: Xmin', $
      UVALUE='APPROXM_XMIN', XSIZE=10)
  xmax = CW_FIELD( BASE49,VALUE=x2, $
      ROW=1, FLOAT=1, TITLE='Xmax', $
      UVALUE='APPROXM_XMAX', XSIZE=10)
  ymin = CW_FIELD( BASE49,VALUE=y1, $
      ROW=1, FLOAT=1, TITLE='Ymin', $
      UVALUE='APPROXM_YMIN', XSIZE=10)
  ymax = CW_FIELD( BASE49,VALUE=y2, $
      ROW=1, FLOAT=1, TITLE='Ymax', $
      UVALUE='APPROXM_YMAX', XSIZE=10)


  BASE36 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE36')

  BUTTON37 = WIDGET_BUTTON( BASE36, $
      UVALUE='APPROXIM_ACCEPT', $
      VALUE='Accept & Plot2D...')

  BUTTON35 = WIDGET_BUTTON( BASE36, $
      UVALUE='APPROXIM_HELP', $
      VALUE='Help...')

  BUTTON38 = WIDGET_BUTTON( BASE36, $
      UVALUE='APPROXIM_CANCEL', $
      VALUE='Cancel')

  approxim_state = { $
	parent: parent, $
	base : image_extract, $ 
	cursor: LABEL3_1, $
	xlWID : pix_xl, $
	xrWID : pix_xr, $
	ylWID : pix_yl, $
	yrWID : pix_yr, $
	wdWID : NPTS_X, $
	htWID : NPTS_Y, $
	maxWID : value_max, $
	minWID : value_min, $
	xminWID : xmin, $
	xmaxWID : xmax, $
	yminWID : ymin, $
	ymaxWID : ymax, $
	xmin : x1, $
	xmax : x2, $
	ymin : y1, $
	ymax : y2, $
	xl : xl, $
	xr : xr, $
	yl : yl, $
	yr : yr, $
	width : wd, $
	height : ht, $
	max   : vmax, $
	min   : vmin, $
	image : image2 $
	}


  WIDGET_CONTROL, image_extract, /REALIZE
  WIDGET_CONTROL, image_extract, SET_UVALUE=approxim_state,/NO_COPY

  XMANAGER, 'image_extract', image_extract
END
;
;
pro WID_BASE_0_event, Event

;  WIDGET_CONTROL, Event.top, GET_UVALUE=img_state,/NO_COPY
;  wid = img_state.WID
;  wset,wid
;  WIDGET_CONTROL, Event.top, SET_UVALUE=img_state,/NO_COPY
  wWidget =  Event.top

  case Event.id of

    Widget_Info(wWidget, FIND_BY_UNAME='WID_BASE_0'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BASE' )then $
        OnBaseResize, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_10'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnOpen, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_1'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnPrinter, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_2'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnPrint, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_6'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnExit, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_8'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnColor, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_3'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnBottomUpTVImage, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_4'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnMirrorTVImage, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_4_1'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnTransposeTVImage, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_30'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnReduceTVImage0, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_31'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnReduceTVImage, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_32'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnExpandTVImage, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_33'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnExpandTVImage0, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_35'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnTVImageROI, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_34'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnRestoreOldCLT, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_5'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnScaleTVImage, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_9'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnRawTVImage, Event
    end
;    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_11'): begin
;      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
;        OnImageCalc, Event
;    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_15'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnSavePNG, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_16'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnSaveJPEG, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_17'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnSaveTIFF, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_18'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnSaveTIFF, Event, /reverse
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_19'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnSaveXDR, Event, /reverse
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_41'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnExportJPG, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_42'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnExportPNG, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_43'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnExportTIF, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_44'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnExportTIF, Event,/reverse
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_61'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnOverlay, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_51'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnHelpIMG, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_52'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnHelpExtract, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_53'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        OnHelpFileName, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_DRAW_0'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_DRAW' )then $
        OnDrawCursor, Event
    end
    else:
  endcase

end

pro WID_BASE_0, GROUP_LEADER=wGroup, xsize=xsize,ysize=ysize,config=config,_EXTRA=_VWBExtra_

  Resolve_Routine, 'img_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  WID_BASE_0 = Widget_Base( GROUP_LEADER=wGroup, UNAME='WID_BASE_0'  $
      ,XOFFSET=5 ,YOFFSET=5 ,TITLE='IMG - Image Process Program' , $
	SPACE=3 ,XPAD=3 ,YPAD=3  $
        ,/TLB_SIZE_EVENTS $
      ,COLUMN=1 ,MBAR=WID_BASE_0_MBAR)


  W_MENU_0 = Widget_Button(WID_BASE_0_MBAR, UNAME='W_MENU_0' ,/MENU  $
      ,VALUE='File')


  W_MENU_10 = Widget_Button(W_MENU_0, UNAME='W_MENU_10'  $
      ,VALUE='Open...')


  W_MENU_1 = Widget_Button(W_MENU_0, UNAME='W_MENU_1' ,/SEPARATOR  $
      ,VALUE='Printer...')


  W_MENU_2 = Widget_Button(W_MENU_0, UNAME='W_MENU_2' ,VALUE='Print')

  W_MENU_6 = Widget_Button(W_MENU_0, UNAME='W_MENU_6' ,/SEPARATOR  $
      ,VALUE='Exit')


  W_MENU_7 = Widget_Button(WID_BASE_0_MBAR, UNAME='W_MENU_7' ,/MENU  $
      ,VALUE='Views')

  W_MENU_3 = Widget_Button(W_MENU_7, UNAME='W_MENU_3'  $
      ,VALUE='Bottom-Up Mirror Image')

  W_MENU_4 = Widget_Button(W_MENU_7, UNAME='W_MENU_4'  $
      ,VALUE='Left-Right Mirror Image')

  W_MENU_4_1 = Widget_Button(W_MENU_7, UNAME='W_MENU_4_1'  $
      ,VALUE='Transpose Image')

  W_MENU_30 = Widget_Button(W_MENU_7, UNAME='W_MENU_30',/SEPARATOR  $
      ,VALUE='Reduce Image by 1/4')

  W_MENU_31 = Widget_Button(W_MENU_7, UNAME='W_MENU_31'  $
      ,VALUE='Reduce Image by half')

  W_MENU_32 = Widget_Button(W_MENU_7, UNAME='W_MENU_32'  $
      ,VALUE='Expand Image by two')

  W_MENU_33 = Widget_Button(W_MENU_7, UNAME='W_MENU_33'  $
      ,VALUE='Expand Image by 1/4')

  W_MENU_5 = Widget_Button(W_MENU_7, UNAME='W_MENU_5',/SEPARATOR  $
      ,VALUE='Extract Image (Scaled)...')

  W_MENU_9 = Widget_Button(W_MENU_7, UNAME='W_MENU_9'  $
      ,VALUE='Extract Image (Original)...')

  W_MENU_35 = Widget_Button(W_MENU_7, UNAME='W_MENU_35'  $
      ,VALUE='Extracted ROI Only')

;  W_MENU_11 = Widget_Button(W_MENU_7, UNAME='W_MENU_11'  $
;      ,VALUE='Extract Image Data')

  W_MENU_8 = Widget_Button(W_MENU_7, UNAME='W_MENU_8', /SEPARATOR $
	,VALUE='Color Table...')

  W_MENU_34 = Widget_Button(W_MENU_7, UNAME='W_MENU_34'  $
	,VALUE='Restore Input Color Table')


  W_MENU_14 = Widget_Button(WID_BASE_0_MBAR, UNAME='W_MENU_14' ,/MENU  $
      ,VALUE='SaveTVRD')

  W_MENU_15 = Widget_Button(W_MENU_14, UNAME='W_MENU_15' ,VALUE='PNG')

  W_MENU_17 = Widget_Button(W_MENU_14, UNAME='W_MENU_17'  $
      ,VALUE='TIFF')

  W_MENU_18 = Widget_Button(W_MENU_14, UNAME='W_MENU_18'  $
      ,VALUE='TIFF-R')

  W_MENU_16 = Widget_Button(W_MENU_14, UNAME='W_MENU_16'  $
      ,VALUE='JPEG')

  W_MENU_40 = Widget_Button(WID_BASE_0_MBAR, UNAME='W_MENU_40' ,/MENU  $
      ,VALUE='SaveRawData')
  
  W_MENU_42 = Widget_Button(W_MENU_40, UNAME='W_MENU_42' ,VALUE='PNG')
  
  W_MENU_43 = Widget_Button(W_MENU_40, UNAME='W_MENU_43'  $
      ,VALUE='TIFF')

  W_MENU_44 = Widget_Button(W_MENU_40, UNAME='W_MENU_44'  $
      ,VALUE='TIFF-R')

  W_MENU_41 = Widget_Button(W_MENU_40, UNAME='W_MENU_41'  $
      ,VALUE='JPEG')

  W_MENU_19 = Widget_Button(W_MENU_40, UNAME='W_MENU_19'  $
      ,VALUE='XDR')

  W_MENU_60 = Widget_Button(WID_BASE_0_MBAR, UNAME='W_MENU_60' ,/MENU  $
      ,VALUE='Tools')
  W_MENU_61 = Widget_Button(W_MENU_60, UNAME='W_MENU_61'  $
      ,VALUE='Overlay2DImages...')
  
  W_MENU_50 = Widget_Button(WID_BASE_0_MBAR, UNAME='W_MENU_50' ,/MENU  $
      ,VALUE='Help')
  
  W_MENU_51 = Widget_Button(W_MENU_50, UNAME='W_MENU_51'  $
      ,VALUE='Help...')

  W_MENU_52 = Widget_Button(W_MENU_50, UNAME='W_MENU_52'  $
      ,VALUE='Extract 2D Data...')

  W_MENU_53 = Widget_Button(W_MENU_50, UNAME='W_MENU_53'  $
      ,VALUE='File Name...')

  WD = 400
  HT = 300
  if keyword_set(xsize) then WD=xsize
  if keyword_set(ysize) then HT=ysize

  WID_DRAW_0 = Widget_Draw(WID_BASE_0, UNAME='WID_DRAW_0' ,XOFFSET=3  $
	,/BUTTON_EVENTS,UVALUE='WID_DRAW_0' $
	,YOFFSET=6 ,SCR_XSIZE=WD ,SCR_YSIZE=HT)

  img_state = { base :WID_BASE_0, $
	menu1WID: W_MENU_7,$
	menu2WID: W_MENU_14,$
	menu3WID: W_MENU_40,$
        file    : '', $
	path    : '', $
	ftype	: '', $
        im      : ptr_new(/ALLOCATE_HEAP),  $
	color_low : 0, $
	color_high : !d.n_colors, $
        original : 0, $    ;  1 - actual read in size , 0 - current screen size
	xl	: 80, $
	xr	: 380, $
	yl	: 50, $
	yr	: 350, $
	xmin:0., $
	xmax:1., $
	ymin:0., $
	ymax:1., $
	vmax:256., $
	vmin:0., $
        WD      : WD, $
        HT      : HT, $
        R       : intarr(256), $
        G       : intarr(256), $
        B       : intarr(256), $
	table_size : 256, $
        WID     : -1, $
        XSIZE   : WD, $
        YSIZE   : HT, $
        printer : obj_new(), $
        MODEL   : 0  $  ; true color
        }

  fconfig = 'img.config'
  if keyword_set(config) then fconfig = config
  fn = findfile(fconfig,count=ct)
  if ct eq 1 then begin
  p=''
	catch,error_status
	if error_status ne 0 then goto,close_config
	openr,1,fconfig
	readf,1,vmax,vmin,xmin,xmax,ymin,ymax
	readf,1,p
close_config:
	close,1
	img_state.vmax = vmax
	img_state.vmin = vmin
	img_state.xmax = xmax
	img_state.xmin = xmin
	img_state.ymax = ymax
	img_state.ymin = ymin
	img_state.path = p
  end

  Widget_Control, W_MENU_7, SENSITIVE=0 
  Widget_Control, W_MENU_14, SENSITIVE=0 
  Widget_Control, W_MENU_40, SENSITIVE=0 
  Widget_Control, /REALIZE, WID_BASE_0

  WIDGET_CONTROL,WID_DRAW_0,GET_VALUE=iDraw
  wset,iDraw
  img_state.WID = !d.window
  img_state.xsize = !d.x_size
  img_state.ysize = !d.y_size
  Widget_Control, WID_BASE_0, SET_UVALUE=img_state,/NO_COPY
  XManager, 'WID_BASE_0', WID_BASE_0, /NO_BLOCK

end
;
; Empty stub procedure used for autoloading.
;
pro img, GROUP_LEADER=wGroup, xsize=xsize,ysize=ysize,_EXTRA=_VWBExtra_
;+
; NAME:
;	IMG
; 
; PURPOSE:
;	This program provides a simple IDL image processing program.
;	It accepts any 8 bits or 24 bits TIF, JPG, or PNG files as input.
;	The image displayed is automatically reajusted to fit  the
;	window size.
;
; 	The window size of this program is adjustable by the window
;	manager.
; 
;	Depress the 'Print' button will generate a postscript copy of 
;	of the graph.
;
;	This program also support the XDR 2D image files generated by
;	any of the following IDL programs: view2d, vw2d, image2d, plot2d.
;
; CATEGORY:
;	Widgets.
;
; CALLING SEQUENCE:
;
;	IMG  [,GROUP_LEADER=wGroup] [,XSIZE=Xsize] [,YSIZE=Ysize]
;
; KEYWORD PARAMETERS:
;	GROUP_LEADER:   If specified, sets the parent widget ID
;
;	XSIZE:          If specified, sets the window width pixel size
;                       default 400
;
;	YSIZE:          If specified, sets the window height pixel size
;                       default 300
;
; RESTRICTION:
;     This program is developed with IDL 5.5, it supports TIF, JPG and PNG 
;     image file. For IDL 5.5 no PNG file allowed.
;     
; EXAMPLE:
;
;	IMG
;-
  WID_BASE_0, GROUP_LEADER=wGroup, xsize=xsize,ysize=ysize,_EXTRA=_VWBExtra_
end
