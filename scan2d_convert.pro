;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
PRO scan2d::Read_TIFF,no,im,TIFF=TIFF
;+
; NAME:
;       scan2d::READ_TIFF
;
; PURPOSE:
;       This method allows the user to read a TIFF image file based on
;       eishter the image seq no or the user specified tiff name.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]READ_TIFF [,NO] [,TIFF=tiff]
;
; ARGUMENTS:
;    NO:       a int variable to specify the input image sequence number.
;    IM:       returns the im(500,500) array
;
; KEYWORDS:
;    TIFF:     specifiies the output TIFF file name to be used. If not given
;              the tiff file is automatically generated from the image filename.
;
; EXAMPLE:
;    Save images 8 from the 'junk2.image' as a TIFF file.
;    The junk2.image8.tiff will be saved by this command.
;    The object v2 need to be defined only if it is not yet defined.
;
;         v2 = obj_new('scan2d',file='junk2.image')
;         v2->read_tiff,8
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, May 4, 1999.
;       xx-xx-xxxx      comment
;-

	if keyword_set(TIFF) then filename=TIFF else begin
	if n_elements(no) eq 0 then no=self.seqno
	if no gt 0 and no le self.maxno then  $
	filename=self.name+strtrim(no,2)+'.tiff'
	end

	found=findfile(filename)
	if found(0) ne '' then begin
	im = read_tiff(filename)
	tv,im
	endif else begin
        res=dialog_message(['File not found:', $
		filename],/error)
        end
END

PRO scan2d::Write_TIFF,no,TIFF=TIFF,top2bottom=top2bottom,type=type
;+
; NAME:
;       scan2d::WRITE_TIFF
;
; PURPOSE:
;       This method allows the user to create a TIFF image file with a 
;       user requested image seq no. The default file name created will
;       be ended with '.tiff' suffix. 
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]WRITE_TIFT,no 
;
; ARGUMENTS:
;    NO:       a int variable to specify the input image sequence number.
;
; KEYWORDS:
;    TIFF:     specifiies the output TIFF file name to be used. If not given
;              the tiff file is automatically generated from the image filename.
;  TOP2BOTTOM: TIFF to be saved in reverse order, ie from top to bottom row
;    TYPE:     Specify plot type, can be contour,surface,or shade_surf
;
; EXAMPLE:
;    Save images 8 from the 'junk2.image' as a TIFF file.
;    The junk2.image8.tiff will be saved by this command.
;    The object v2 need to be defined only if it is not yet defined.
;
;         v2 = obj_new('scan2d',file='junk2.image')
;         v2->write_tiff,8
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, May 4, 1999.
;       xx-xx-xxxx      comment
;-

tvlct,R,G,B,/get

	if n_elements(no) eq 0 then no=self.seqno
	if no gt 0 and no le self.maxno then begin
	filename=self.name+strtrim(no,2)+'.tiff'
	if keyword_set(TIFF) then filename=TIFF

	self->point_lun,no-1
	if keyword_set(type) then self->view,type=type,/noplot else $
	self->view,/noplot
	if keyword_set(top2bottom) then $
		write_tiff,filename,reverse(TVRD(),2),1,red=r,green=g,blue=b $
		else write_tiff,filename,TVRD(),red=r,green=g,blue=b
	endif else begin
	res=dialog_message(['Invalid image no :'+string(no),$
		'Max valid no='+string(self.maxno)],/error)
	end
END

PRO scan2d::Write_GIF,List,gif=gif,all=all,type=type
;+
; NAME:
;       scan2d::WRITE_GIF
;
; PURPOSE:
;       This method allows the user to create a list of GIF image files
;       based on the user specified list of image numbers. The name convention
;       of each GIF file will be the image file suffixed with its image number 
;       plus the '.gif'.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]WRITE_GIF, LIST
;
; ARGUMENTS:
;    LIST:     a string variable to specify the input image list
;              each image will be saved as a separate gif file.
;
; KEYWORDS:
;    GIF:      specifiies the output GIF file name to be used. If not given
;              the gif file is automatically generated from the image filename.
;    ALL:      If specified, then all the gif images of the 2D object will be 
;              saved in the same output gif file.
;    TYPE:     specifies the 2D view plot type
;
; EXAMPLE:
;    Save images 1, 4 to 8 from the 'junk2.image' as separate GIF files.
;    At the completion of write_gif, the following gif files will be
;    created:
;	   junk2.image1.gif
;	   junk2.image4.gif
;	   junk2.image5.gif
;	   junk2.image6.gif
;	   junk2.image7.gif
;	   junk2.image8.gif
;    The object v2 need to be defined only if it is not yet defined.
;
;         v2 = obj_new('scan2d',file='junk2.image')
;         v2->write_gif,'1,4:8'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, May 4, 1999.
;       xx-xx-xxxx      comment
;-

	im = make_array(500,500,/byte)
	tvlct,R,G,B,/get

if n_elements(list) gt 0 then begin
	list=strtrim(list,2)

; parse the string list

	sz = size(list)
	if sz(n_elements(sz)-2) eq 7 then begin
	sep1=','
	if strpos(list,sep1) lt 0 then sep1=' '
	if strpos(list,':') gt 0 then parse_num,string(list),res,sep1=sep1,sep2=':' else $
	parse_num,string(list),res,sep1=sep1
;	print,'scan:',res
	list = res
	end

        for i=0,n_elements(list)-1 do begin
	pick=list(i)-1
	filename = self.name+strtrim(list(i),2)+'.gif'       ; 'test.gif'
	if keyword_set(gif) then filename=gif
        self->point_lun,pick
	if keyword_set(type) then self->view,/noplot,type=type else $
        self->view,/noplot
	write_gif,filename,tvrd(),R,G,B
	write_gif,filename,/close
	end

	return
end

	if keyword_set(all) then begin   ; save all 
	filename = self.name+'.gif'       
        if keyword_set(gif) then filename=gif
	self->point_lun,0
	if keyword_set(type) then self->view,/noplot,type=type else $
	self->view,/noplot

	write_gif,filename,tvrd(),R,G,B,/multiple

	for i=1,self.maxno-1 do begin
	if keyword_set(type) then self->view,/noplot,type=type else $
	self->view,/noplot
	write_gif,filename,tvrd(),/multiple
	end
	write_gif,filename,/close
	return
	end

	res=dialog_message("Usage: obj->write_gif,'list of image nubmers'",/Info)	
END

PRO scan2d::Read_GIF,gif=gif,start_no,end_no,imArray=imArray,noview=noview
;+
; NAME:
;       scan2d::READ_GIF
;
; PURPOSE:
;       This method allows the user to read the GIF image from an GIF image
;       file.  The GIF file must be created by the IDL WRITE_GIF routine.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]READ_GIF, START_NO [,END_NO,GIF=gif] 
;                 [,IMARRAY=imArray [,/NOVIEW]]
;
; ARGUMENTS:
;    START_NO: the desired GIF image number selected  
;    END_NO:   the terminated GIF image number from a multiple GIF image file 
;
; KEYWORDS:
;    GIF:      specifiies the GIF file name to be used. If not given
;              it is figured out from the START_NO and 2D image filename 
;    IMARRAY:  return the 3D image array(500,500,end_no-start_no+1)
;    NOVIEW:   if specified, no gif image verification is desired 
;              
;
; EXAMPLE:
;    Example 1 create and display the 20 gif image for the 'junk2.image' file. 
;
;         v2=obj_new('scan2d',file='junk2.image')
;	  v2->write_gif,20
;         v2->read_gif,20
;
;    Example 2 read images 4 to 8 from the gif file previously created by
;    the obj->write_gif,/all method and return the extracted gif images
;    in IMARRAY varible.
;
;         v2=obj_new('scan2d',file='junk2.image')
;         v2->write_gif,/all
;         v2->read_gif,4,8,gif='junk2.image.gif',imarray=imarray,/noview
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, May 4, 1999.
;       xx-xx-xxxx      comment
;-

	n1=self.seqno
	filename = self.name+strtrim(n1,2)+'.gif'   

	if n_elements(start_no) then begin
	n1= start_no
	if n1 lt 1 then n1=1
	if n1 gt self.maxno then n1=self.maxno
	filename = self.name+strtrim(n1,2)+'.gif'
	end

	if keyword_set(gif) then filename=gif

	found = findfile(filename)
	if found(0) eq '' then begin
		res = dialog_message(['GIF file:',filename +' not found'],/error)
		return
	end

	read_gif,filename,im,/multiple,R,G,B
	TVLCT,R,G,B
	read_gif,filename,/close
	tv,im
	imArray = im

	; more than 1 gif image 
	n2=n1
	if n_elements(end_no) then n2=end_no

	if keyword_set(gif) eq 0 then return
	sz=size(im)
	im_array = make_array(sz(1),sz(2),n2,/byte)

	for i=0,n2-1 do begin
	read_gif,filename,im,/multiple
	im_array(0,0,i) = im(*,*)
	end
	read_gif,filename,/close

	imArray = im_array(*,*,n1-1:n2-1)
	if keyword_set(noview) then return
	for i=n1,n2 do begin
	im = im_array(*,*,i-1)
	tv,im
	end

END
