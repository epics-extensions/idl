;
; sscan__define.pro
;
;+
; 1) Object method allows the user to access 1D/2D/3D data from the sscan 
;     object
;
;	@os.init
;	.run sscan__define.pro
; 	v = obj_new('sscan',file='/home/beams/CHA/data/xxx/cha_0001.mda')
;
;    Pick mda file from the specified input directory
;
; 	v = obj_new('sscan',path='/home/beams/CHA/data/xxx')
;
;	v->get,da1d=da1d,da2d=da2d,....
;		extract data array and descs info
;
;	v->plot1d,scan=id
;               where id zero based line seq #
;
;	v->plot2d,id
;               where id zero based image seq #
;
;	v->image2d
;		pass 2D image array to image2d program
;	v->panimage
;		pass 2D image array to panimage program
; 	v->view3d_2d
;		pass 3D data array to 3D slicer view3d_2d program
;	v->pickfile
;		use file selection to pick another mda file
;
; 2) The ILD widget program sscan allows the user view data through 
;    packaged idl program 
;   
;       sscan 
;-

@sscan.pro

PRO sscan::Delete
        obj_destroy,self
heap_gc
;print,ptr_valid()
END

PRO sscan::pickfile
	path = self.path
	self->delete
	self = obj_new('sscan',path=path)
;	sscan_read,path=path,/echo
END

PRO sscan::det,id_def,ip_def,rank=rank
;+
; NAME:
; 	sscan::det
;
; PURPOSE:
;	This method returns the defind detects for the specified rank.

; OUTPUT:
;	ID_DEF:	returns the indicator vector of defined detectors
;	IP_DEF:	returns the indicator vector of defined positioners
;	
; KEYWORDS:
;	RANK:	specify the axis rank, default is same as the scan dimension
;
; EXAMPLE:
;	This example returns the indicater vector of defined vector of the rank 2
;	v->det,id_def,rank=2
;-
	dim = self.rank
	ID = dim-1
	if rank lt dim and rank gt 0 then ID = rank -1
	id_def = self.id_def(4:4+self.DETMAX(ID)-1,ID)
	ip_def = self.id_def(0:3,ID)
END

PRO sscan::get,da1d=da1d,da2d=da2d,da3d=da3d,pa1d=pa1d,pa2d=pa2d,pa3d=pa3d,labels=labels,id_def=id_def,rank=rank,xdesc=xdesc,ydesc=ydesc,zdesc=zdesc
;+
; NAME:
; 	sscan::get
;
; PURPOSE:
;	This method allows the user to extract scan data info of an opened mda 
;	scan file through using the keyword specifications.
;
; CALLLING SEQUENCE:
;	Obj->[sscan::]get,DA1D=da1d,DA2D=da2d,...
;
; KEYWORD:
;  PA1D	    -	extract positioner vector array of outer most scan 
;  PA2D	    -	extract positioner vector array of inner scaan 
;  PA3D	    -	extract positioner vector array of inner most scan
;  DA1D	    -	extract detector 1D vector array
;  DA2D	    -	extract detector 2D image array
;  DA3D	    -	extract detector 3D volume array
;  LABELS   -	extract name,desc,units labels[89*3,3] of 4PIs+85DIs 
;  ID_DEF   -	extract indicator id_def[89,3] of defined 4PIs+85DIs 
;  RANK     -   return dimension or rank of scan
;  XDESC    -	extract X positioner description 
;  YDESC    -	extract Y positioner description
;  ZDESC    -	extract Z detector names description
;
; EXAMPLE:
;	In this example extracts the 8th image from the 2D data array from the 
;	/home/beams/CHA/Yorick/data/2idd_0002.mda file, then displays the 
;	extracted image by the plot2d program. 
;
;	v = obj_new('sscan',file='/home/beams/CHA/Yorick/data/2idd_0002.mda')
;	v->get,da2d=da2d
;	im = da2d(*,*,7)
;	plot2d,im
;	
;-
	rank=self.rank
	labels = self.labels
	id_def = self.id_def
	is = 89
	ie = is+89-1
	if self.rank eq 1 then begin
	da1d = *self.da(0)
	pa1d = *self.pa(0)
	xdesc = labels(is:ie,0)
	end
	if self.rank eq 2 then begin
	da2d = *self.da(0)
	pa2d = *self.pa(0)
	da1d = *self.da(1)
	pa1d = *self.pa(1)
	xdesc = labels(is:ie,0)
	ydesc = labels(is:ie,1)
	end
	if self.rank eq 3 then begin
	da3d = *self.da(0)
	pa3d = *self.pa(0)
	da2d = *self.da(1)
	pa2d = *self.pa(1)
	da1d = *self.da(2)
	pa1d = *self.pa(2)
	xdesc = labels(is:ie,0)
	ydesc = labels(is:ie,1)
	zdesc = labels(is:ie,2)
	end
END

PRO sscan::plot1d,scan=scan
;+
; NAME:
;	sscan::plot1d
;
; PURPOSE:
;	This method allows the user to use plot1d to view the extracted
;	1D array from any 1D/2D/3D scan mda file. By default, all the 
;	detectors defined will be plotted by plot1d. A user has great
; 	flexibility in plot re-configuration by pressing the PlotOptions...
; 	dialog.
;
; CALLING SEQUENCE:
;	Obj->[sscan::]plot1d [,SCAN=scan]
;
; KEYWORD:
; 	SCAN:	specify the 2D/3D zero based scan index number (applicable
;		to 2D/3D scan only)
;		If specified, then the 1D array extracted from the 
;		da2D array is plotted.
;		If not specified, by default the da1D array is plotted.
;
; EXAMPLE:
;	Following example extracts all the detectors from the da1D data array 
;	from the  given scan mda file  
;
;	  v = obj_new('sscan',file='/home/beams/CHA/Yorick/data/2idd_0002.mda')
; 	  v->plot1d
;
;	Following example extracts all detectors from the da2D data array for 
;	3D scan # 10.
;
;	  v = obj_new('sscan',file='/home/beams/CHA/Yorick/data/2idd_0002.mda')
; 	  v->plot1d,scan=10
;
;-
	if n_elements(id) eq 0 then id=0
	dim = self.rank
    IF  n_elements(scan) EQ 0 THEN BEGIN
	if self.DETMAX(dim-1) gt 0 then begin
	yarr = *self.DA(dim-1)
	id_def = self.id_def(4:4+self.DETMAX(dim-1)-1,dim-1)
	zdescs = strtrim(indgen(n_elements(id_def)),2) + ': '
	zdescs=zdescs + self.labels(4:4+self.DETMAX(dim-1)-1,dim-1)
	xarr = *self.pa(dim-1)
	xdescs=self.labels(0,dim-1)
	title=''
	end
    ENDIF ELSE BEGIN
	if dim eq 1 then return
	if self.DETMAX(dim-2) gt 0 then begin
	data = *self.DA(dim-2)
	sz = size(data)
	if scan ge sz(2) then scan= sz(2)-1
	yarr = reform(data(*,scan,*),sz(1),sz(3))
	id_def = self.id_def(4:4+self.DETMAX(dim-2)-1,dim-2)
	zdescs = strtrim(indgen(n_elements(id_def)),2) + ': '
	zdescs= zdescs + self.labels(4:4+self.DETMAX(dim-2)-1,dim-2)
	xarr = *self.pa(dim-2)
	xdescs=self.labels(0,dim-2)
	title= strtrim(dim,2)+'D-Scan #: '+strtrim(scan,2)
	end
    END

	plot1d,xarr,yarr,legend=zdescs,xtitle=xdescs,ytitle=ydescs, $
		title=title, wTitle=self.file,/data
END

PRO sscan::plot2d,id
;+
; NAME:
;	sscan::plot2d
;
; PURPOSE:
;	This method allows the user to use mouse-drive plot2d program to 
;	analyze a selected 2D image from 2D array extracted from a 2D/3D 
;	scan mda file. User can easily access PICK1D, ROI, Calibration 
;	subprograms.
;
; CALLING SEQUENCE:
;	Obj->[sscan::]plot2d,ID
;
; INPUT: 
;	ID   -  zero based image index number, default to 0
;
; EXAMPLE:
;	Use plot2d to extract the 16th 2D image from the 2D image array.
;
;	v = obj_new('sscan',file='/home/beams/CHA/Yorick/data/2idd_0002.mda')
; 	v->plot2d,15
;
;-
	if n_elements(id) eq 0 then id=0
	dim = self.rank
	if dim lt 2 then return
	if self.DETMAX(dim-2) gt 0 then begin
	im_array = *self.DA(dim-2)
	id_def = self.id_def(4:4+self.DETMAX(dim-2)-1,dim-2)
	xarr = *self.pa(dim-2)
	yarr = *self.pa(dim-1)
	xdescs=self.labels(0,dim-2)
	ydescs=self.labels(0,dim-1)
	zdescs=self.labels(4:4+self.DETMAX(dim-2)-1,dim-2)
	data = im_array(*,*,id)
	plot2d,data,xarr,yarr,title=zdescs(id),xtitle=xdescs,ytitle=ydescs, $
		wTitle=self.file
	end
END

PRO sscan::image2d
;+
; NAME:
;	sscan::image2d
;
; PURPOSE:
;	This method allows the user to use mouse-drive image2d program to 
;	analyze any 2D image arrray extracted from a 2D/3D scan mda file.
;	User can easily access ROI, Calibration, panimage subprograms.
;
; CALLING SEQUENCE:
;	Obj->[sscan::]image2d
;
; EXAMPLE:
;	v = obj_new('sscan',file='/home/beams/CHA/Yorick/data/2idd_0002.mda')
; 	v->image2d
;
;-
	dim = self.rank
	if dim lt 2 then return
	if self.DETMAX(dim-2) gt 0 then begin
	im_array = *self.DA(dim-2)
	id_def = self.id_def(4:4+self.DETMAX(dim-2)-1,dim-2)
	xarr = *self.pa(dim-2)
	yarr = *self.pa(dim-1)
	title=self.file
xdescs=self.labels(0,dim-2)
ydescs=self.labels(0,dim-1)
zdescs=self.labels(4:4+self.DETMAX(dim-2)-1,dim-2)
	image2d,im_array,xarr,yarr,id_def=id_def,title=title,scanno=self.scanno,xdescs=xdescs,ydescs=ydescs,zdescs=zdescs
	end
END

PRO sscan::panimage
;+
; NAME:
;	sscan::panimage
;
; PURPOSE:
;	This method allows the user flexiblely to display the panimage of the 
; 	2D data array extracted from a 2D/3D scan mda file. A user can easily 
;	generate the panimage output in various combination and format.
;
; CALLING SEQUENCE:
;	Obj->[sscan::]panimage
;
; EXAMPLE:
;	v = obj_new('sscan',file='/home/beams/CHA/Yorick/data/2idd_0002.mda')
; 	v->panimage
;
;-
	dim = self.rank
	if dim lt 2 then return
	if self.DETMAX(dim-2) gt 0 then begin
	im_array = *self.DA(dim-2)
	id_def = self.id_def(4:4+self.DETMAX(dim-2)-1,dim-2)
	end
	title='2D'
	if dim eq 3 then title = '3D'
	title = title+' Scan #' + strtrim(self.scanno,2)
	panimage_sel,im_array,id_def,title=title
;	panimage,im_array,id_def,numd=10,title=title
END

PRO sscan::view3d_2d,id
;+
; NAME:
;	sscan::view3d_2d 
;
; PURPOSE:
;	This method allows the user to display 3D data array from a 3D scan. 
;	It calls view3d_2d program which allows the user to slice any cross
;	section from any axis. It calls the sum image program and allows the 
;	user flexible to re-define the range of index includeed in sum image 
;	calculation. Various image subprograms are vailable in veiwe3d_2d.
;
; CALL SEQUENCE:
;	v->view3d_2d,ID
;
; INPUT:
;	ID:	Specify the zero based seq # , default to 0
;		Integer ID can not exceed number of DIs defined in the inner 
;		most scan record for a 3D scan
; 
; EXAMPLE:
;	Following example get the second 3D array from the 3D scan file.
;
;	v = obj_new('sscan',file='/home/beams/CHA/Yorick/data/2xfm_0020.mda')
; 	v->view3d_2d,1
;
;-
	dim = self.rank
	if dim lt 3 then return
	if n_elements(id) eq 0 then id=0
	nd = self.DETMAX(0)
	da3d = *self.DA(0)
	sz = size(da3d)
	if sz(0) eq 4 then begin
		if id ge sz(4) then id=sz(4)-1
		da = da3d(*,*,*,id) 
	endif else da = da3D(*,*,*)
	xv = *self.pa(0)
	yv = *self.pa(1)
	zv = *self.pa(2)
	di = where(self.id_def(4:4+nd,0))
	if di(0) eq -1 then return
	if n_elements(di) gt 1 then dii = di(id) else dii=di
	title=self.labels(4+dii,0)
	descs = [self.labels(0,0),self.labels(0,1),self.labels(0,2)]
	if strtrim(descs(0),2) eq '' then descs(0) = 'Index' 
	view3d_2d,da,0,xv,yv,zv,title=title,descs=descs
END

PRO sscan::overlay
	scan2d_overlay,path=self.path
END

PRO sscan::print
;+
; NAME:
;	sscan::print
;
; PURPOSE:
;	This method print out the brief info about the extracted sscan object 
;	of the user specified mda file.
;
; CALLING SEQUENCE:
;	Obj->[sscan::]print
;
; EXAMPLE:
;	v = obj_new('sscan',file='/home/beams/CHA/data/xxx/cha_0001.mda')
; 	v->print
;
;-
	print,'FILE:',self.FILE
	print,'ID_DEF:',self.ID_DEF
	print,'RANK:',self.RANK
	print,'NPTS:',self.npts
	print,'CPT:',self.CPT
	print,'NB_POS:',self.NB_POS
	print,'NB_DET:',self.NB_DET
	print,'NB_TRG:',self.NB_TRG
	print,'DETMAX:',self.DETMAX
	print,'TS1:',self.TS1
	print,'TS2:',self.TS2
help,*self.PA(0),*self.PA(1),*self.PA(2)
help,*self.DA(0),*self.DA(1),*self.DA(2)
END

FUNCTION sscan::Init,file=file,path=path,HD_Pos=HD_Pos,HD_Det=HD_Det,HD_Trg=HD_Trg
;+
; NAME:
;	sscan::Init
;
; PURPOSE:
;	This function create a sscan object from a selected mda file. If
;	file name keyword is not specified then the mda file selection 
;	dialog will be used to select the desired mda file.
;	The object created can be used by various methods defined in the
;	program.
;
; CALLING SEQUENCE:
;	Obj = obj_new('sscan' [,FILE=file ,PATH=path,...])
;
; KEYWORD:
;   file  - specifies input mda file used
;   path  - dialog_pickfile will be used to select the mda file with
;           specified path directory
; HD_pos  - returns array of positioner info
; HD_det  - returns array of detector info 
; HD_trg  - returns array of trigger info 
;
; EXAMPLE:
; 	This example creates a sscan object v with known mda file:
;
;	   v = obj_new('sscan',file='/home/beams/CHA/data/xxx/cha_0001.mda')
;
;	This example first uses the file selection dialog to pick a mda file 
;	from the specified path directory, then creates a sscan object for 
;	the selected file:
;
;	   v = obj_new('sscan',path='/home/beams/CHA/data/xxx')
;- 
;loadct,39
	if keyword_set(file) then sscan_read,SSD,file=file,/echo else $
	sscan_read,SSD,path=path,/echo
	ty = size(SSD,/type)
	if ty ne 8 then return,0

	self.file = SSD.file
	self.path = SSD.path
	self.rank = SSD.rank
	self.scanno = SSD.scanno
	self.npts = SSD.npts
	self.cpt = SSD.cpt
	self.nb_pos = SSD.nb_pos
	self.nb_det = SSD.nb_det
	self.nb_trg = SSD.nb_trg
	self.id_def = SSD.id_def 
	self.labels = SSD.labels 
	self.detMax = SSD.detMax 
	self.pv = SSD.pv 
	self.ts1 = SSD.ts1 
	self.ts2 = SSD.ts2 
	self.im_filled = SSD.im_filled 
	self.da = SSD.da 
	self.pa = SSD.pa 
	self.labels = SSD.labels 

	self->axis,SSD,HD_Pos,HD_Det,HD_Trg
	return,1
END

PRO sscan::Axis,SSD,H_P,H_D,H_T
;  positioner,detector, trigger info structure defined in mad files
; 
  pos_info= { $
    pxpv:'', $
    pxds:'', $
    pxsm:'', $
    pxeu:'', $
    rxpv:'', $
    rxds:'', $
    rxeu:'' }
  det_info= { $
    dxpv:'', $
    dxds:'', $
    dxeu:'' }
  trg_info= { $
    txpv:'', $
    txcd: 1.0 }

HD_P = make_array(4,3,value=pos_info)
HD_D = make_array(85,3,value=det_info)
HD_T = make_array(4,3,value=trg_info)

    H_P = SSD.HD_P
    H_D = SSD.HD_D
    H_T = SSD.HD_T
END

PRO sscan__define

ntot = 85+4

  DetMax = [0,0,0]
  id_def = intarr(ntot,3)
  cd,current=p

  struct = { sscan, $
  	file  : '', $
        path : p, $
        lun : -1, $
        rank  : 0, $
        scanno : 0, $
        npts    : [0,0,0], $
        cpt     : [0,0,0], $
        nb_pos  : [0,0,0],$
        nb_det  : [0,0,0],$
        nb_trg  : [0,0,0],$
        id_def : id_def, $
; HD_P:HD_P,;
; HD_D:HD_D,;
; HD_T:HD_T,;
        labels  : strarr(3*ntot,3), $
        detMax  : [0,0,0], $
        pv      : ['','',''], $
        ts1     : '', $
        ts2     : '', $
        im_filled : [0,0,0], $
        noenv  : 0, $
        envPtr : 0L, $
        EH      : ptr_new(/allocate_heap), $
        da      : ptrarr(3,/allocate_heap), $
        pa      : ptrarr(3,/allocate_heap), $
        sub_scan_ptr : ptrarr(2,/aLLOCATE_HEAP) $
        }

END

