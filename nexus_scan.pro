;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
@NX__define.pro
@panimage.pro
@scan2d_overlay.pro
@fit_statistic.pro


;  .run NX__define
;  v = obj_new('NX',file='cha_0049.nexus')


PRO nexus_scan_getScanH,seq,group=group
COMMON NEXUS_BLOCK,nexus_state

	V = nexus_state.obj

	nexus_scan_getScanDataH,V,seq,seqs=seqs,data=data,xa=xa,ya=ya,za=za,name=name
;help,data,xa,ya,za
;print,seqs
;	title = 'SCAN_'+strtrim(nexus_state.scanno(nexus_state.seqno-1),2)+' '+ $
	title =	 nexus_state.title + $
		' scahH:'+name 
	sz = size(data)
	if sz(0) eq 3 then begin
;		if nexus_state.axis eq 0 then view3d_2d,data,title=title else $
		view3d_2d,data,0,xa,ya,za,title=title ,Group=group
	end
	if sz(0) eq 2 then begin
;		if nexus_state.axis eq 0 then  plot2d,data,xarr=xa,yarr=ya,title=title else $
		plot2d,data,xarr=xa,yarr=ya,title=title ,Group=group
	end
	if sz(0) eq 1 then begin
;		if nexus_state.axis eq 0 then  plot1d,data,title=title else $
		plot1d,xa,data,title=title ,Group=group
	end

END

PRO nexus_scan_getScanDataH,V,seq,gid=gid,seqs=seqs,data=data,xa=xa,ya=ya,za=za,name=name,only=only
COMMON NEXUS_BLOCK,nexus_state
; this routine returns data,xa,ya for a given group seq # of
;   scanH detector 'scanH_det_'
;   undefined returned with !values.d_nan 
; INPUT:
;  Seq   - speicify group sequence # , zero based
;
; KEYWORD:
;  Gid   - specify group gid #, if gid specified overrides the seq param
;  ONLY  - if specified only the data is returned
;
;	v = nexus_state.obj

	if keyword_set(gid) then v->getVG,gid,tags,refs else $
	v->VG,seq,tags,refs

	data = 0   ;!values.f_nan
nent = n_elements(refs)
	v->sdsData,refs(0),name,data,type,ndims,dims,seq=seq,/nowin

	if keyword_set(only) then begin
		seqs=seq
		return
	end

	sz = size(data)
	
	xa = indgen(sz(1))	;!values.d_nan
	if sz(0) ge 2 then ya = indgen(sz(2))	;!values.d_nan
	if sz(0) eq 3 then za = indgen(sz(3))	;!values.d_nan

	r_seqs = intarr(nent)  
	r_seqs(0) = seq


	if nent gt 1 then begin
	for i=1,nent-1 do begin
	v->sdsData,refs(i),nm,da,ty,nd,di,seq=seq,/nowin

		r_seqs(i) = seq
		if nm eq 'axis3_01' then begin
			za = da
		end
		if nm eq 'axis2_01' then begin
			ya = da
		end
		if nm eq 'axis1_01' then begin
			xa = da
		end
	end
	end
seqs = r_seqs
END

PRO NX::findVGnames,gid,names
; this function returns the group name with known GIDs
;
	ng = n_elements(gid)
	names = strarr(ng)

	fid = HDF_OPEN(self.file)

	for i=0,ng-1 do begin
	vg_hdl = HDF_VG_ATTACH(fid,gid(i))
	if vg_hdl gt 0 then begin
        HDF_VG_GETINFO,vg_hdl,name=name,class=class
	names(i) = name
        HDF_VG_DETACH,vg_hdl
	end
	end

	HDF_CLOSE,fid

; print,names
END

PRO nexus_scan_getScanno,scanno
COMMON NEXUS_BLOCK,nexus_state

	V = nexus_state.obj
	V->findVG,seq,gid,name='SCAN_'   ;class='NXentry'
	V->findVG,dseq,gid1,name='fakeDim0',seq_start=seq(nexus_state.Nscan-1) 
	seq = [seq,dseq]

	nexus_state.g_seq = seq
;print,'SCAN_ group seq id',seq 
	V->findVGnames,gid,names
	nexus_state.scanno=0
	scanno = fix(strmid(names,5,4))
	nexus_state.scanno = scanno
	
	; get the scanH_det_  ref for the scans

	v->findVG,name='scanH_det_',hseq,hgid,/nowin
;print,'scanH_det_ group seq id',hseq  ;,hgid
	
	if hseq(0) ge 0 then begin
	nexus_state.scanh_gseq=0
	nexus_state.scanh_n=0
	
	ik = 1
	for i=0,n_elements(hseq)-1 do begin
	for k=ik,nexus_state.Nscan do begin
		if seq(k) gt hseq(i) then begin
		nexus_state.scanh_gseq(k-1) = hseq(i)
		nexus_state.scanh_n(k-1) = nexus_state.scanh_n(k-1)+1
		ik = k-1
;print,i,k,hseq(i),nexus_state.scanh_gseq(k-1),nexus_state.scanh_n(k-1)
		goto,nexti
		end
	end
nexti:
	end
	end

;print,'nexus_state.scanh_gseq',nexus_state.scanh_gseq(0:nexus_state.Nscan)

END

PRO nexus_scan_help
    str = [ $, 
	'NEXUS_SCAN is a simple scan browser for nexus files which must be', $
	'created by the "scan2nexus" command. Each file should contain multiple ', $
	'records of 1D/2D/3D scans. The 1D/2D/3D scans can be appended into file ', $
	'whith any order.  Currently, it can not handle 3D scan yet.','', $
	'It allows the user flexiblely to access any scan record from the', $
	'nexus file.   Each scan record owns its own displaying window by', $
	'clicking the desired scan # from the scan list.', $
	'', 'The user interface is given below:', $
	'',$
	'File         - Use file selection dialog to select a *.nexus file',$
	'Name         - Enter input nexus file directly from keyboard ', $
	'Scan list    - Show the list of scan # found in the file', $
	'               (select scan # from the list for desired scan)', $
	'Overlay1D... - Run 1D overlay plot for selected detector', $
	'ListScan#... - Display the scan list info in the nexus file', $
	'Help...      - Display this help page', $
	'Close        - Close the nexus_scan program','', $
	'PLOT1D program is used to display 1D scan, a user can use', $
	'various buttons to tailor curves, legends, labels, etc', $
	'for the 1D plot desired. For aborted 1D scan data a user can use ', $
	'the NPT slider to select the last data point to be plotted.', $
	'', 'A simple 2D interface allows the user to access the PANIMAGE,', $
	'CALIBRATION, OVERLAY2D programs, and a scroll list allows the', $
	'user to use PLOT2D program to tailor the seleced 2D image plot.' $
	]
	r = dialog_message(str,/info,title='NEXUS_SCAN_HELP')

END

PRO nexus_1d_select_help,group=group
    str=[ $
	'Help...        - Display this help page', $
	'Scroll List    - Multiple slection list of detectors', $
	'        Click any item of list selecting the highlighted item', $
	'        CNTL+LMB add the picked item to the selected list', $
	'        SHIFT+LMB add all the items between the last two clicks', $
	'PLOT1D...      - Use PLOT1D to display all the selected detectors', $
	'FITTING...     - Call EZ_FIT to access various curve fitting methods', $
	'STATISTIC...   - Display the FWHM and statistics for the selected curves', $
	'FWHM...        - Plot the selected curves with FWHM if it is found', $ 
	'                 (each curve has its own window)', $
	'FWHM DY...     - Plot the derivate of the selected curves with FWHM if', $
	'                 it is found (each curve has its own window)', $
	'Done           - Close the dialog' $
	]
	xdisplayfile,text=str,group=group,title='Help on NEXUS_1D_SELECT'
END

PRO nexus_scan_2DHelp
	str = ['For the picked 2D scan user can access the various 2D programs', $
	'User interface is given below:','', $
	'PanImage...   - Access various panImage plot options for the scan', $
	'2D Calibration... - Access the calibration program for the selected',$
	'                    2D scan', $
	'Overlay2D Init... - Select the images to be overlaid ( at most 15 can',$
	'                    be selected from the 85)', $
	'Overlay2D...      - Run 2D image overlay program', $
	'Help...           - Show this help page', $
	'List Box          - Select and plot the detector image from the list', $
	'Close             - Close the nexus 2D selection program', $
	'']
	r = dialog_message(str,/info,title='NEXUS_SCAN_2DHELP')
ENd



PRO nexus_scan_list,str
COMMON NEXUS_BLOCK,nexus_state

	nscan = nexus_state.Nscan
	ndims = nexus_state.ndims(0:nscan-1)

	for i=0,nscan-1 do begin
	if i eq 0 then str = '     SEQ#   SCAN#    DIM#    B-SDS#  L-SDS#  Haxis_SEQH'
	str = [str,string(i)+string(nexus_state.scanno(i)) +string(ndims(i)) $
		+string(nexus_state.bgnSDS(i)) $
		+string(nexus_state.endSDS(i)) $
		+string(nexus_state.scanh_gseq(i))]
	end

END




PRO nexus_scan_overlay,id=id,line=line,npt=npt,pick1d=pick1d
COMMON NEXUS_BLOCK,nexus_state
;   Automatically exclude all 2D scans from the 1D overlay line plot
;
; KEYWORD:
;    NPT  - specify the number of data points to be plotted, default from the
;           1st scan
;    ID   - sepecify the detector #  of the line
;    LINE - specidify the line # from the 2D scan, default to 1st line 
;           if line exceeds the available lines, the last line will be
;           assumed
;    PICK1D - Pick lines from 1D scans only for the overlay plot
;

	if keyword_set(id) eq 0 then id = 16

	
	dname = ['det_0'+strtrim(indgen(9)+1,2),'det_'+strtrim(indgen(76)+10,2)]
	dn = dname[id-1]

	dname = ['D'+strtrim(indgen(9)+1,2),'DA','DB','DC','DD','DE','DF', $
		'D0'+strtrim(indgen(9)+1,2), 'D'+strtrim(indgen(61)+10,2)]
	seqs = nexus_state.endSDS(0:nexus_state.Nscan-1)
	seq_start = nexus_state.bgnSDS(0:nexus_state.Nscan-1)

	V = nexus_state.obj
	V->findSDS,'pos_01',seq_p1,ref,p1,dims=dims,/nowin,start=seq_start(pick1d(0))
	if seq_p1 gt seqs(pick1d(0)) then begin
		if n_elements(warning_str) eq 0 then $
		warning_str = ['No pos_1 found for', $
			 'SCAN #'+strtrim(nexus_state.scanno(pick1d(0)),2) ]
	end
	if keyword_set(npt) eq 0 then npt=dims(0)
	
	;  fliter out scans has the picked detector 

	for i=0,nexus_state.Nscan-1 do begin
	; extract 1D scan only
	if pick1d(i) gt 0 and nexus_state.ndims(i) eq 1  then begin
		bgn_seq = seq_start(i)
		V->findSDS,dn,seq_d1,ref,d1,ndims=ndims,dims=dims, $
			/nowin,start=bgn_seq
	if n_elements(d1) eq 0 then begin
	   	r = dialog_message('Detector '+dname[id-1]+' not defined for scan # '+ $
			strtrim(nexus_state.scanno(i),2),/error)
		end

	;   check for missing data for the scan

	if seq_d1 lt seqs(i) then begin
		if n_elements(dpick) eq 0 then dpick=i else dpick=[dpick,i]
		end
	end
	end

endloop:
	nscan = n_elements(dpick)
	if nscan lt 1 then begin
	   	r = dialog_message('Not 1D scan for specified detector found ',/error)
   		return
	end

	XA = make_array(npt,nscan,/double)
	YA = make_array(npt,nscan)
	NPTA = intarr(nscan)
	scanno = nexus_state.scanno(dpick)

	is1d = intarr(nscan) 	;nexus_state.Nscan)
	
	seqs = nexus_state.endSDS(dpick)
	seq_start = nexus_state.bgnSDS(dpick) ; 0


	for i=0,nscan-1 do begin
	d1=0
		V->findSDS,dn,seq_d1,ref,d1,ndims=ndims,dims=dims, $
			/nowin,start=seq_start(i)

	if n_elements(d1) gt 0 then begin

		if ndims eq 2 and keyword_set(pick1d) then goto,bypass2 
		V->findSDS,'pos_01',seq_p1,ref,p1,/nowin,start=seq_start(i)
		if ndims eq 2 then $
		V->findSDS,'pos_01',seq_p2,ref,p2,/nowin,start=seq_p1+1
		NPTA(i) = n_elements(p1) 
		n_pts = n_elements(p1)-1
		if n_pts gt npt then n_pts = npt-1
		if ndims eq 1 then begin
		    XA(0:n_pts,i) = p1(0:n_pts)
		    YA(0:n_pts,i) = d1(0:n_pts)
		    is1d(i) = 1
		end
		; if 2D to be included for future expansion
		if ndims eq 2 then begin
		    XA(0:n_pts,i) = p2(0:n_pts,0)
		    if keyword_set(line) then begin
		      if line gt 0 and line le dims(1) then $
		      YA(0:n_pts,i) = d1(0:n_pts,line-1) else $ 
		      YA(0:n_pts,i) = d1(0:n_pts,dims(1)-1)
		    endif else $
		    YA(0:n_pts,i) = d1(0:n_pts,0)
		end
	bypass2:
	end
	end


	TITLE = 'Overlay Plot - ' + dname[id-1] + '('+dn+')'
	legend = 'Scan # ' + strtrim(scanno,2)
	comment='***Line 1 selected from each scan'
	if keyword_set(line) then $
	comment=[comment,'   Plus line '+strtrim(line,2)+' selected from 2D scans' ]
	plot1d,xa,ya,title=title,legend=legend,comment=comment,/data, $
		width=500

	if n_elements(warning_str) gt 0 then $
	r=dialog_message(warning_str,/info,title='List of 1D Scan found')
;	xdisplayfile,text=warning_str,title='List of 1D SCAN found',width=30,height=15
END




PRO nexus_scan_overlay2D
COMMON NEXUS_BLOCK,nexus_state

	V = nexus_state.obj

	seq_start = nexus_state.endSDS(nexus_state.seqno-1)
	V->findSDS,'det_',seq_d1,ref,d1,ndims=ndims,dims=dims, $
		/nowin,start=seq_start

	if ndims ne 2 then return
	
	V->findSDS,'scan_time',seq_time,ref,timeStamp,/nowin,start=seq_d1
	ND = seq_time - seq_d1
	
	da2D = make_array(dims(0),dims(1),ND)
	nexus_da2D,V,seq_d1,ND,da2D,detrs,names,descs,units


;    at most 15 detector can be selected need to check for 
; initial selection

        detname =  'D'+ [strtrim(indgen(9)+1,2),'A','B','C','D','E','F' , $
                '01','02','03','04','05','06','07','08','09', $
                strtrim(indgen(61)+10,2)]

	fdname = detname(detrs-1)

; read the selects.dat file

	found = findfile('selects.dat',count=ct)
	if ct then begin
	xdr_open,unit,'selects.dat'
	xdr_read,unit,init_selects
	xdr_close,unit
	if n_elements(init_slects) gt 15 then init_selects = init_selects(0:14)
	endif else begin
		init_selects = indgen(15) + 15
	end

	fdname = fdname(init_selects)
	ND = n_elements(init_selects)
	selects = indgen(ND)+1

;print,fdname
;print,init_selects

	def = intarr(15)
	vmin = make_array(15)
	vmax = make_array(15)
	image_array = make_array(dims(0),dims(1),15)
	for i=0,ND-1 do begin
		ip = init_selects(i)
		def(i)=1
		image_array(*,*,i) = da2D(*,*,ip)
		vmin(i) = min(da2d(*,*,ip))
		vmax(i) = max(da2d(*,*,ip))
	end

	overlayInitState,overlay_state,image_array,def,vmax,vmin, $
		col=2,row=2,pixels=2,selects=selects,fdname=fdname,/discrete

	scan2d_overlayimage,overlay_state

END


PRO nexus_scan_overlay2D_init
COMMON NEXUS_BLOCK,nexus_state

	V = nexus_state.obj

	seq_start = nexus_state.endSDS(nexus_state.seqno-1)
	V->findSDS,'det_',seq_d1,ref,d1,ndims=ndims,dims=dims, $
		/nowin,start=seq_start

	if ndims ne 2 then return
	
	V->findSDS,'scan_time',seq_time,ref,timeStamp,/nowin,start=seq_d1
	ND = seq_time - seq_d1
	
	da2D = make_array(dims(0),dims(1),ND)
	nexus_da2D,V,seq_d1,ND,da2D,detrs,names,descs,units


;    at most 15 detector can be selected need to check for 
; initial selection

        detname =  'D'+ [strtrim(indgen(9)+1,2),'A','B','C','D','E','F' , $
                '01','02','03','04','05','06','07','08','09', $
                strtrim(indgen(61)+10,2)]

	fdname = detname(detrs-1)
;print,fdname
	overlayInitSelect,fdname


END


PRO nexus_scan_readConfig,nexus_scan_state
	filename=''
catch,error_status
if error_status ne 0 then return
	openr,unit,'NX.config',/get_lun
	readf,unit,filename
	free_lun,unit
	nexus_scan_startup,filename,nexus_scan_state
END

PRO nexus_scan_writeConfig,nexus_scan_state
if n_elements(nexus_scan_state) then begin
	openw,unit,'NX.config',/get_lun
	printf,unit,nexus_scan_state.file
	free_lun,unit
end
END

PRO nexus_scan_open,V,file=file

	if obj_valid(V) then obj_destroy,V

	filename='/home/beams/CHA/data/xxx/cha_0049.nexus'
	if keyword_set(file) then filename = file
	V = obj_new('NX',file=filename)
END

PRO nexus_scan_init,V,seqs
COMMON NEXUS_BLOCK,nexus_state

	if n_elements(V) eq 0 then begin
		r = dialog_message('NX object "V" must be defined first.',/error)
		return
	end

	if n_elements(nexus_state) eq 0 then $
	nexus_state = { $
		obj: V, $
		Nscan : 0, $
		scanno : make_array(1000,value=-1), $
		ndims : intarr(1000), $
		scanh_gseq : intarr(1000), $ ;last g_seq of scanH for each scan 
		scanh_n : intarr(1000), $    ; num of scanH found for each scan 
		bgnSDS : intarr(1000), $     ; scan begin SDS in file
		endSDS : intarr(1000), $     ; scan end SDS in file
		g_seq : intarr(1000), $      ; g_seq for begin of SCAN in file
		title : '',$
		g_seq1 : intarr(85), $	;  Di in scan1 found
		g_seq2 : intarr(85), $	;  Di in scan2 found
		g_seqH : intarr(85), $	;  Di in scanH found
		seqno: 0 $
	} else begin

	nexus_state.scanno=-1
	nexus_state.ndims=1
	nexus_state.scanh_gseq = 0
	nexus_state.scanh_n = 0
	nexus_state.bgnSDS = 0
	nexus_state.endSDS = 0
	nexus_state.g_seq = 0
	nexus_state.title = '' 
	nexus_state.g_seq1 = 0 
	nexus_state.g_seqH = 0 
	nexus_state.seqno = 0
	end

	V->findALL,'scan_time',seqs,/nowin
	nexus_state.Nscan = n_elements(seqs)
	nexus_state.endSDS = seqs
	nexus_state.obj = V
; print,'end SDS ',seqs

	bgn = intarr(nexus_state.Nscan)
	for i=1,n_elements(seqs)-1 do begin
	bgn(i) = seqs(i-1)+1
	end
	nexus_state.bgnSDS = bgn
;print,'bgn SDS',bgn

	nexus_scan_getScanno,scanno

	nscan = nexus_state.Nscan

	ndims = make_array(nscan,value=1)    ; default 1D
	for i=0,nscan-1 do begin
	seq1 = nexus_state.bgnSDS(i)
	seq2 = nexus_state.endSDS(i)
	V->findALL,'axis3_01',s3,seq_start=seq1,seq_end=seq2,/nowin
	V->findALL,'axis2_01',s2,seq_start=seq1,seq_end=seq2,/nowin
	if s2(0) ne -1 then ndims(i) = 2
	if s3(0) ne -1 then ndims(i) = 3
	end
	nexus_state.ndims = ndims

	nexus_scan_list
	
END




PRO  nexus_getStatisticDeviation_1d,id1,y,mean,sdev,mdev,st
        mean=0.
        sdev=0.
        mdev=0.
        no = n_elements(y)
        if no eq 0 then return
        mean = total(y)/no
        if no eq 1 then return
        index = where(y gt mean, count)      ; check for constant function
        mean = [mean,0.,0.,0.]
        if count gt 0 then mean = MOMENT(y,mdev=mdev,sdev=sdev)

st = id1
st= [st+' ']
        st = [st, '   Mean         = '+string(mean(0))]
        st = [st, '   Standard Dev = '+string(sdev)]
        st = [st, '   Mean Abs Dev = '+string(mdev)]
        st = [st, '   Variance     = '+string(mean(1))]
        st = [st, '   Skewness     = '+string(mean(2))]
        st = [st, '   Kurtosis     = '+string(mean(3))]
END


PRO  nexus_getStatistic_1d,id1,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,st

; call statistic_1d

        statistic_1d,p1,d1,c_mass,x_peak,y_peak,y_hpeak,FWHM

st = id1
st= [st+' ']
        st = [st, '   Peak  X='+strtrim(x_peak,1)+'  Y='+strtrim(y_peak,1)]
;       st = [st, '   H-Peak  Y='+strtrim(y_hpeak)]
        st = [st, '   Centroid  '+ strtrim(c_mass,1)]
        st = [st, '   FWHM      '+strtrim(FWHM,1)]

if n_elements(x_peak) gt 0 then begin
        largest = max(y_peak)
        i_largest = 0
        for i=0,n_elements(x_peak)-1 do begin
                if y_peak(i) ge largest then begin
                i_largest = i
                goto, write_peak
                end
                end
        write_peak:
        xpeak = x_peak(i_largest)
        ypeak = y_peak(i_largest)
        end

END


PRO NEXUS_1D_SELECT_subarray,list,ya
	n = n_elements(list)
	sz = size(ya)
	da = make_array(sz(1),n)
	for i=0,n-1 do begin
	da(*,i) = ya(*,list(i))
	end
	ya = da
END



PRO NEXUS_1D_SELECT_Event, Event

  WIDGET_CONTROL, Event.top, GET_UVALUE=nexus_1d_state,/no_copy 
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  state = nexus_1d_state.drv

  CASE Ev OF 

  'NEXUS_1DSELECT_LIST': BEGIN
	list = widget_info(Event.id,/list_select)
      END
  'NEXUS_1DSELECT_PLOT1D': BEGIN
	x = state.xa(*,0)
	ya = state.ya
	el = widget_info(nexus_1d_state.listWID,/list_select)
	nexus_1d_select_subarray,el,ya
	y = ya
	legend=state.legend(el)
	title=state.title
	xtitle=state.xtitle
	ytitle=state.ytitle
	comment=state.comment
	stamp=state.timeStamp
	plot1d,x,y,title=title,xtitle=xtitle,ytitle=ytitle,stamp=stamp, $
		comment=comment,legend=legend,/data,group=Event.top	
      END
  'NEXUX_1DSELECT_FITTING': BEGIN
	x = state.xa(*,0)
	ya = state.ya
	u_openw,unit,'fitting.bin1d',/XDR
	u_write,unit,x
	u_write,unit,ya
	u_close,unit	
	el = widget_info(nexus_1d_state.listWID,/list_select)
	ez_fit,x=x,y=ya,GROUP=Event.top,jpick=el(0)
      END
  'NEXUS_1DSELECT_FWHM': BEGIN
	vx = state.xa(*,0)
	el = widget_info(nexus_1d_state.listWID,/list_select)
	for i=0,n_elements(el)-1 do begin
	  vy = state.ya(*,el(i))
	  title=state.title +': FWHM of '+state.y_names(el(i))
	  statistic_1d,VX,VY,c_mass,x_peak,y_peak,y_hpeak,fwhm,xl,xr,/plot, $
		report='fwhm.rpt',title=title,group=Event.top
	end
      END
  'NEXUS_1DSELECT_STATISTIC': BEGIN
	vx = state.xa(*,0)
	el = widget_info(nexus_1d_state.listWID,/list_select)
	for i=0,n_elements(el)-1 do begin
	  vy = state.ya(*,el(i))
	  id1 = state.legend(el(i))
	  nexus_getStatistic_1d,id1,vx,vy,c_mass,xpeak,ypeak,y_hpeak,FWHM,st
	  if n_elements(str) eq 0 then str = st else str=[str,st]
	  nexus_getStatisticDeviation_1d,id1,vy,mean,sdev,mdev,st
	  str = [str,st,'']
	end
	xdisplayfile,text=str
		
      END
  'NEXUS_1DSELECT_DYDX': BEGIN
	vx = state.xa(*,0)
	el = widget_info(nexus_1d_state.listWID,/list_select)
	for i=0,n_elements(el)-1 do begin
	  vy = state.ya(*,el(i))
	  vy = slope(vx,vy)
	  title=state.title +': FWHM of DY/DX '+state.y_names(el(i))
	  statistic_1d,VX,VY,c_mass,x_peak,y_peak,y_hpeak,fwhm,xl,xr,/plot, $
		report='fwhm.rpt',title=title,group=Event.top
	end
      END
  'NEXUS_1DSELECT_Done': BEGIN
	widget_control,Event.top,/DESTROY
	return
      END
  'NEXUS_1DSELECT_HELP': BEGIN
	nexus_1d_select_help,group=Event.top
      END
  ENDCASE

  WIDGET_CONTROL, Event.top, SET_UVALUE=nexus_1d_state,/no_copy 

END




PRO NEXUS_1D_SELECT, state, legend=legend, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  NEXUS_1D_SELECT = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, title=state.title+': Nexus_1D_Select',$
      UVALUE='NEXUS_1D_SELECT')

  BASE2 = WIDGET_BASE(NEXUS_1D_SELECT, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

	list_1d = 'Curve '+strtrim(indgen(85)+1,2)
	if keyword_set(legend) then list_1d = legend 
	if n_elements(state) then list_1d = state.legend
	list_11 = widget_list(BASE3,value=list_1d,/multiple, $
	ysize=10,UVALUE='NEXUS_1DSELECT_LIST')
	WIDGET_CONTROL,list_11,SET_LIST_SELECT=indgen(n_elements(list_1d))


  BASE8 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE8')

  BUTTON22 = WIDGET_BUTTON( BASE8, $
      UVALUE='NEXUS_1DSELECT_HELP', $
      VALUE='Help...')

  BUTTON9 = WIDGET_BUTTON( BASE8, $
      UVALUE='NEXUS_1DSELECT_PLOT1D', $
      VALUE='PLOT1D...')

  BUTTON10 = WIDGET_BUTTON( BASE8, $
      UVALUE='NEXUX_1DSELECT_FITTING', $
      VALUE='FITTING...')

  BUTTON12 = WIDGET_BUTTON( BASE8, $
      UVALUE='NEXUS_1DSELECT_STATISTIC', $
      VALUE='STATISTIC...')

  BUTTON11 = WIDGET_BUTTON( BASE8, $
      UVALUE='NEXUS_1DSELECT_FWHM', $
      VALUE='FWHM...')

  BUTTON13 = WIDGET_BUTTON( BASE8, $
      UVALUE='NEXUS_1DSELECT_DYDX', $
      VALUE='FWHM DY...')

  BUTTON14 = WIDGET_BUTTON( BASE8, $
      UVALUE='NEXUS_1DSELECT_Done', $
      VALUE='Done')


  nexus_1d_state = { drv: state, $
	listWID: list_11 }

  WIDGET_CONTROL, NEXUS_1D_SELECT, /REALIZE
  WIDGET_CONTROL, NEXUS_1D_SELECT, SET_UVALUE=nexus_1d_state,/no_copy 

  XMANAGER, 'NEXUS_1D_SELECT', NEXUS_1D_SELECT
END





PRO nexus_scan_getScanArray,V,seqs,ND,scanno=scanno,title=title,data=data,xa=xa,ya=ya,za=za,group=group
COMMON NEXUS_BLOCK,nexus_state
; seq_start  - start SDS seq# for array
; ND         - Numb of detecors of SDS found in scan1/scan2



	seq_start = seqs(0)
	sz = size(data)

	seq_time = nexus_state.endSDS(nexus_state.seqno-1)
	V->findSDS,'scan_time',seq_end,ref,timeStamp,start=seq_time

; get X & Y position names & descs

	NP = n_elements(seqs)-1
	pa = make_array(sz(1),4,/double)   ;,value=!values.d_nan)
	if NP gt 0 then begin
	axis_nm = strarr(NP)
	detrs = strarr(NP)
	names = strarr(NP)
	descs = strarr(NP)
	units = strarr(NP)
	for i=1,NP do begin
		v->SDS,seqs(i),p1,name=name,type=type, $
			natts=natts,ndims=ndims,dims=dims, $
			attrnms=attrnms,attrdas=attrdas,/nowin

		axis_nm(i-1)=name
		for j=0,natts-1 do begin
		if strpos(attrnms(j),'_num') ge 0 then detrs(i-1)=fix(attrdas(j))
		if strpos(attrnms(j),'_pv') ge 0 then names(i-1) = attrdas(j)
		if strpos(attrnms(j),'_desc') ge 0 then descs(i-1) = attrdas(j)
		if strpos(attrnms(j),'_unit') ge 0 then units(i-1) = attrdas(j)
		end
; print,seqs(i),name,detrs(i-1),names(i-1),descs(i-1),units(i-1)
		if name eq 'axis2_01' then begin
			ya = p1
			y_sel = detrs(i-1)
			y_names = names(i-1)
			y_descs = descs(i-1)
			y_units = units(i-1)
		end
		if strpos(name,'axis1_') ge 0 then begin
			x_sel = detrs(i-1)
			if x_sel eq 1 then begin
			  xa = p1
			  x_names = names(i-1)
			  x_descs = descs(i-1)
			  x_units = units(i-1)
			end
			pa(*,x_sel-1) = p1(*)
		end
	end
	endif else begin
		xa = indgen(sz(1))
		pa(*,0:3) = xa(*)
		x_names = ''
		x_descs = 'Step #'
		x_units = ''
	end


; 1D array from the scan1 record
	if sz(0) eq 1 then begin
	DA = make_array(sz(1),ND)
	d_seqs = indgen(ND)+seq_start
	nexus_DA,V,d_seqs,ND,sz(1),DA,dets,y_names,y_descs,y_units
	
        lastd = max(dets)
        def = intarr(lastd)
        for i=0,ND-1 do begin
                def(dets(i)-1) = 1
        end

	x = pa(*,0)    ; xa
	ya = da
	
	xtitle = x_descs(0)
	if x_units(0) ne '' then xtitle = xtitle+' ('+x_units(0) +')'
	ytitle =''
	stamp = timeStamp
	legend = 'Det '+strtrim(dets,2)+' '+y_descs
;	plot1d,x,ya,title=title,xtitle=xtitle,ytitle=ytitle, $
;		legend=legend,comment=comment,stamp=stamp,/data
	end

; 2D array from the scan1 record

	if sz(0) eq 2 then begin
	da2D = make_array(sz(1),sz(2),ND)
	
	detrs = make_array(ND,/int)
	names = make_array(ND,/string)
	descs = make_array(ND,/string)
	units = make_array(ND,/string)

	for i=0,ND-1 do begin
	seq = seq_start+i
	V->sds,seq,data,name=name,type=type,natts=natts,ndims=ndims,dims=dims, $
		attrnms=attrnms,attrdas=attrdas,/nowin
	
		for j=0,natts-1 do begin
		if strpos(attrnms(j),'_num') ge 0 then detrs(i) = fix(attrdas(j))
		if strpos(attrnms(j),'_pv') ge 0 then names(i) = attrdas(j)
		if strpos(attrnms(j),'_desc') ge 0 then descs(i) = attrdas(j)
		if strpos(attrnms(j),'_unit') ge 0 then units(i) = attrdas(j)
		end
	da2D(*,*,i)=data(*,*)
	end

	legend = 'Det '+strtrim(detrs,2)+' '+descs

	lastd = max(detrs)
	def = intarr(lastd)
	DA = make_array(sz(1),sz(2),lastd)   ;value=!values.f_nan)

	for i=0,ND-1 do begin
		def(detrs(i)-1) = 1
		DA(*,*,detrs(i)-1) = da2D(*,*,i)
	end
	YTITLE= y_descs(0)+ " ("+y_units(0)+")"
	end

; if 3D scan 
	if sz(0) eq 3 then begin
	da3D = make_array(sz(1),sz(2),sz(3),ND)
	detrs = make_array(ND,/int)
	names = make_array(ND,/string)
	descs = make_array(ND,/string)
	units = make_array(ND,/string)

	for i=0,ND-1 do begin
	seq = seq_start+i
	V->sds,seq,data,name=name,type=type,natts=natts,ndims=ndims,dims=dims, $
		attrnms=attrnms,attrdas=attrdas,/nowin
	
		for j=0,natts-1 do begin
		if strpos(attrnms(j),'_num') ge 0 then detrs(i) = fix(attrdas(j))
		if strpos(attrnms(j),'_pv') ge 0 then names(i) = attrdas(j)
		if strpos(attrnms(j),'_desc') ge 0 then descs(i) = attrdas(j)
		if strpos(attrnms(j),'_unit') ge 0 then units(i) = attrdas(j)
		end
	da3D(*,*,*,i)=data(*,*,*)
	end

	legend = 'Det '+strtrim(detrs,2)+' '+descs

	lastd = max(detrs)
	def = intarr(lastd)
	DA = make_array(sz(1),sz(2),sz(3),lastd)    ;,value=!values.f_nan)

	for i=0,ND-1 do begin
		def(detrs(i)-1) = 1
		DA(*,*,*,detrs(i)-1) = da3D(*,*,*,i)
	end
	YTITLE= y_descs(0)+ " ("+y_units(0)+")"
	end


	XTITLE= x_descs(0)+ " ("+x_units(0)+")"
	title = nexus_state.title
	if keyword_set(title) then title = title
	COMMENT = 'SCAN # '+strtrim(scanno,2)
	if n_elements(za) eq 0 then za=0.D

	state_2d = { da:da, $
		pa:pa, $
		xa:xa, $
		ya:ya, $
		za:za, $
		title:title, $
		xtitle:xtitle, $
		ytitle:ytitle, $
		timeStamp: timeStamp, $
		comment: comment, $
		legend: legend, $
		detrs: detrs, $
		x_names: x_names, $	
		x_descs: x_descs, $	
		x_units: x_units, $	
		y_names: y_names, $	
		y_descs: y_descs, $	
		y_units: y_units, $	

		def : def, $
		sel_id: 15, $
		scanno:scanno $
	}

	if sz(0) eq 1 then nexus_1D_select,state_2d,group=group
	if sz(0) eq 2 then nexus_2D_select,state_2d,group=group
	if sz(0) eq 3 then nexus_3D_select,state_2d,group=group

END






PRO NEXUS_3D_SELECT_Event, Event

  WIDGET_CONTROL,EVENT.TOP,GET_UVALUE=state3d,/NO_COPY
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'NEXUS_SCAN_3DLIST': BEGIN
	state3d.pick = event.index
      END
  'NEXUS_SCAN_3DAXIS': BEGIN
	state3d.axis = Event.Value
      END
  'NEXUS_SCAN_3DSLICE': BEGIN
      Print, 'Event for 3D Slicer...'
	state = state3d.state
	xa = state.xa
	ya = state.ya
	za = state.za
        title = state.title + ' ('+ state.legend(state3d.pick)+')'
	rank=0
	i = state.detrs(state3d.pick)
 	data = state.da(*,*,*,i)
	if state3d.axis eq 0 then view3d_2d,data,title=title,group=Event.top else $
	view3d_2d,data,rank,xa,ya,za,title=title,group=Event.top
      END
  'NEXUS_SCAN_3DDONE': BEGIN
	widget_control,Event.top,/destroy
	return
      END
 
  ENDCASE

  WIDGET_CONTROL,EVENT.TOP,SET_UVALUE=state3d,/NO_COPY

END



PRO NEXUS_3D_SELECT, state, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  title = state.title+' - 3D Selector'

  NEXUS_3D_SELECT = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, TITLE=title, $
      UVALUE='NEXUS_3D_SELECT')

  BASE2 = WIDGET_BASE(NEXUS_3D_SELECT, $
      COLUMN=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL3', $
      VALUE='Pick Detector #')

  ListVal271 = [ 'D'+strtrim(indgen(9)+1,2),'DA','DB','DC','DD','DE','DF', $
                'D0'+strtrim(indgen(9)+1,2), 'D'+strtrim(indgen(61)+10,2)]
  if n_elements(state) then ListVal271 = state.legend
  LIST4 = WIDGET_LIST( BASE2,VALUE=ListVal271, $
      UVALUE='NEXUS_SCAN_3DLIST', $
      YSIZE=10)


  BASE8 = WIDGET_BASE(NEXUS_3D_SELECT, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE8')

  BASE9 = WIDGET_BASE(BASE8, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE9')

  Btns460 = [ $
    'Index' , $
    'Values' ]
  BGROUP10 = CW_BGROUP( BASE9, Btns460, $
      ROW=1, $
      FRAME=1, /exclusive, $
      LABEL_TOP='Axial Values', $
      UVALUE='NEXUS_SCAN_3DAXIS')

  BUTTON11 = WIDGET_BUTTON( BASE9, $
      UVALUE='NEXUS_SCAN_3DSLICE', $
      VALUE='3D Slicer...')

  BUTTON11 = WIDGET_BUTTON( BASE9, $
      UVALUE='NEXUS_SCAN_3DDONE', $
      VALUE='Done')

  WIDGET_CONTROL,LIST4,SET_LIST_SELECT=15
  WIDGET_CONTROL,BGROUP10,SET_VALUE=1
  state3d = {  $ 
	state:state, $
	listWID: LIST4, $
	pick: 15, $
	axis: 1 $
	}	

  WIDGET_CONTROL, NEXUS_3D_SELECT, /REALIZE
  WIDGET_CONTROL,NEXUS_3D_SELECT,SET_UVALUE=state3d,/NO_COPY


  XMANAGER, 'NEXUS_3D_SELECT', NEXUS_3D_SELECT
END


PRO nexus_scan_first,group=group
COMMON NEXUS_BLOCK,nexus_state
	nexus_state.seqno = 1
	nexus_scan_current,group=group
END

PRO nexus_scan_last,group=group
COMMON NEXUS_BLOCK,nexus_state
	nexus_state.seqno = nexus_state.Nscan 
	nexus_scan_current,group=group
END

PRO nexus_scan_prev,group=group
COMMON NEXUS_BLOCK,nexus_state
	seqno = nexus_state.seqno - 1
	if seqno lt 1 then begin
		r = dialog_message('Begin of file reached!!',/info)
		return
	end
	nexus_state.seqno = seqno
	nexus_scan_current,group=group
END

PRO nexus_scan_next,group=group
COMMON NEXUS_BLOCK,nexus_state
	seqno = nexus_state.seqno + 1
	if seqno gt nexus_state.Nscan then begin
		r = dialog_message('End of file reached!!!',/info)
		return
	end
	nexus_scan_current,group=group
END

PRO nexus_scan_current,group=group
COMMON NEXUS_BLOCK,nexus_state
	seqno = nexus_state.seqno - 1
	scanno = nexus_state.scanno(seqno)
	nexus_state.title = 'SCAN_'+strtrim(scanno,2)

	v = nexus_state.obj

	nexus_state.g_seq1 = 0
	v->findVG,seq1,gid1,name='scan1_det_', $
		seq_start=nexus_state.g_seq(seqno), $
		seq_end=nexus_state.g_seq(seqno+1) 
	if seq1(0) gt 0 then begin
		nexus_state.g_seq1 = seq1
		ND = n_elements(seq1)
		nexus_scan_getScanDataH,V,seq1(0),seqs=seqs,data=data, $
			xa=xa,ya=ya,za=za,name=name
		seq_start = seqs(0)
		nexus_scan_getScanArray,V,seqs,ND,scanno=scanno, $
			group=group, $
			title=nexus_state.title,data=data,xa=xa,ya=ya,za=za
	end

	nexus_state.g_seq2 = 0
	v->findVG,seq2,gid2,name='scan2_det_', $
		seq_start=nexus_state.g_seq(seqno), $
		seq_end=nexus_state.g_seq(seqno+1) 
	if seq2(0) gt 0 then begin
		nexus_state.g_seq2 = seq2
		ND = n_elements(seq2)
		nexus_scan_getScanDataH,V,seq2(0),seqs=seqs,data=data, $
			xa=xa,ya=ya,za=za,name=name
		seq_start = seqs(0)
		nexus_scan_getScanArray,V,seqs,ND,scanno=scanno, $
			group=group, $
			title=nexus_state.title,data=data,xa=xa,ya=ya,za=za
	end


	if nexus_state.scanh_gseq(seqno) gt 0 then begin
		nexus_scanh,nexus_state.scanh_n(seqno), $
			title=nexus_state.title,Group=group
        end

END



PRO nexus_DA,V,seqs,ND,NPT,da,detrs,names,descs,units,debug=debug
; read 1D data array (works for both positioner and detectors)
; seqs  - specify a set of SDS seq # to be read
; ND    - Number of seqs in seqs to be read
; da    - return the data array
; detrs - returns the detector number
; names - returns the detector names
; descs - returns the detector descs
; units - returns the detector units
	
 	detrs= make_array(ND,/int)
	names = make_array(ND,/string)
	descs = make_array(ND,/string)
	units = make_array(ND,/string)

	for i =0,ND-1 do begin
	seq = seqs(i)
	V->sds,seq,data,name=name,type=type,natts=natts, $
		dims=dims,ndims=ndims, $
		attrnms=attrnms,attrdas=attrdas,/nowin
	if ndims eq 1 and dims(0) le NPT then begin
		for j=0,natts-1 do begin
		if strpos(attrnms(j),'_num') ge 0 then detrs(i) = fix(attrdas(j))
		if strpos(attrnms(j),'_pv') ge 0 then names(i) = attrdas(j)
		if strpos(attrnms(j),'_desc') ge 0 then descs(i) = attrdas(j)
		if strpos(attrnms(j),'_unit') ge 0 then units(i) = attrdas(j)
		end
	DA(0:NPT-1,i) = data(0:NPT-1,0)
	end
	end

endread:

if keyword_set(debug) then begin
print,detrs
print,names
print,descs
print,units
end

END


PRO nexus_da2D,V,seq_d1,ND,da2D,detrs,names,descs,units,debug=debug
; read 2D data array (for detectors)
; seq_d1- specify the start SDS seq # for the scan 
; ND    - total number of continuous detectors detected 
; da    - return the data array
; detrs - returns the detector number
; names - returns the detector names
; descs - returns the detector descs
; units - returns the detector units
	
 	detrs= make_array(ND,/int)
	names = make_array(ND,/string)
	descs = make_array(ND,/string)
	units = make_array(ND,/string)

	for i =0,ND-1 do begin
	seq = seq_d1+i
	V->sds,seq,data,name=name,type=type,natts=natts,ndims=ndims,dims=dims, $
		attrnms=attrnms,attrdas=attrdas,/nowin
		
		for j=0,natts-1 do begin
		if strpos(attrnms(j),'_num') ge 0 then detrs(i) = fix(attrdas(j))
		if strpos(attrnms(j),'_pv') ge 0 then names(i) = attrdas(j)
		if strpos(attrnms(j),'_desc') ge 0 then descs(i) = attrdas(j)
		if strpos(attrnms(j),'_unit') ge 0 then units(i) = attrdas(j)
		end
	dA2D(*,*,i) = data(*,*)
	end
if keyword_set(debug) then begin
print,detrs
print,names
print,descs
print,units
end

END


PRO NEXUS_SCAN_PICK1D_Event, Event

  widget_control,Event.top,get_uvalue=state,/no_copy
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 
  'NEXUS_SEL_DET': BEGIN
	state.sel_id = event.index
	widget_control,state.nptWID,get_value=npt
	state.npt = npt
	widget_control,/hourglass
	nexus_scan_overlay,id=state.sel_id+1,npt=npt,pick1d=state.sel_scanno
      END
  'NEXUS_SEL_ACCEPT': BEGIN
	widget_control,state.nptWID,get_value=npt
	state.npt = npt
	widget_control,/hourglass
	nexus_scan_overlay,id=state.sel_id+1,npt=npt,pick1d=state.sel_scanno
      END
  'NEXUS_SEL_SCANNO': BEGIN
	state.sel_scanno = 0
	sel_scanno = widget_info(Event.id,/list_select)
	state.sel_scanno(sel_scanno) = 1
;	print,,state.sel_scanno
      END
  'NEXUS_SEL_NPTS': BEGIN
	widget_control,Event.ID,get_value=npt
	state.npt = npt
;	print,npt
      END
  'NEXUS_SEL_CLOSE': BEGIN
	widget_control,Event.top,/destroy
	return
      END
  ENDCASE
  widget_control,Event.top,set_uvalue=state,/no_copy
END





PRO nexus_scan_pick1d, GROUP=Group
COMMON NEXUS_BLOCK,nexus_state

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  scanno = nexus_state.scanno(0:nexus_state.Nscan-1)
  sel_scanno = make_array(nexus_state.Nscan,value=1)

  state = { sel_id : 15, npt: 0, nptWID:0L, $
	scanno: scanno, sel_scanno:sel_scanno}

  NEXUS_SCAN_PICK1D = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, title='Overlay 1D...', $
      MAP=1, $
      UVALUE='NEXUS_SCAN_PICK1D')

  label0 = widget_label(NEXUS_SCAN_PICK1D,value='Select SCAN # & DIs')

  BASE2 = WIDGET_BASE(NEXUS_SCAN_PICK1D, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  listscan = 'SCAN_'+strtrim(scanno,2)
  LIST2 = WIDGET_LIST( BASE2,VALUE=listscan, $
      UVALUE='NEXUS_SEL_SCANNO', /multiple, $
      YSIZE=10)
  widget_control,LIST2,set_list_select=indgen(nexus_state.Nscan)
  if nexus_state.Nscan gt 10 then begin
	state.sel_scanno = 0
	state.sel_scanno(0:10) = 1	
	widget_control,LIST2,set_list_select=indgen(11)
  end

  ListVal251 =  'D'+ [strtrim(indgen(9)+1,2),'A','B','C','D','E','F' , $
                '01','02','03','04','05','06','07','08','09', $
                strtrim(indgen(61)+10,2)] +'    '

  LIST3 = WIDGET_LIST( BASE2,VALUE=ListVal251, $
      UVALUE='NEXUS_SEL_DET', $
      YSIZE=10)
  widget_control,LIST3,set_list_select=state.sel_id

  BASE2_1 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2_1')

  field5 = CW_FIELD(BASE2_1,value=state.npt,/integer,/return_events,$
	title='NPTS:',Xsize=4,UVALUE='NEXUS_SEL_NPTS')
  state.nptWID = field5

  accept = Widget_button(BASE2_1,value='Accept', UVALUE='NEXUS_SEL_ACCEPT')
  close = Widget_button(BASE2_1,value='Close', UVALUE='NEXUS_SEL_CLOSE')

  widget_control,NEXUS_SCAN_PICK1D,set_uvalue=state,/no_copy
  WIDGET_CONTROL, NEXUS_SCAN_PICK1D, /REALIZE

  XMANAGER, 'NEXUS_SCAN_PICK1D', NEXUS_SCAN_PICK1D
END

PRO NEXUS_2DSELECT_Event, Event

  widget_control,Event.top,get_uvalue=state,/no_copy
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 
  'NEXUS_SEL_PANIMAGE': BEGIN
	da2d = state.da
	def = state.def
	panimage_sel,da2d,def,title=state.title,tiff='NX.tiff',group=Event.top
      END
  'NEXUS_SEL_OVERLAY2D_INIT': BEGIN
	nexus_scan_overlay2D_init
      END
  'NEXUS_SEL_OVERLAY2D': BEGIN
	nexus_scan_overlay2D
      END
  'NEXUS_SEL_IMAGE2D': BEGIN
	da2d = state.da
	def = state.def
	x = state.xa
	y = state.ya
	xdescs = state.x_descs
	ydescs = state.y_descs
	title = state.title
	xtitle = state.xtitle
	ytitle = state.ytitle
	scanno = state.scanno
	image2d,da2d,x,y,Group=Event.top,title=title,id_def=def,scanno=scanno,$
		xdescs= xdescs,ydescs=ydescs
      END
  'NEXUS_SEL_2DHELP': BEGIN
	nexus_scan_2DHelp
      END
  'NEXUS_SEL_CALIB': BEGIN
	da2d = state.da
	def = state.def
	x = state.xa
	y = state.ya
	calibration_factor,da2d,def,title=state.title,xv=x,yv=y,group=Event.top
      END
  'NEXUS_SEL_DET': BEGIN
	dname = ['D'+strtrim(indgen(9)+1,2),'DA','DB','DC','DD','DE','DF', $
		'D0'+strtrim(indgen(9)+1,2), 'D'+strtrim(indgen(61)+10,2)]
	sz = size(state.da)
	if event.index lt sz(3) then begin
	state.sel_id = event.index
	title = state.title+ ' (' + dname(event.index) + ')'
	im = state.da(*,*,event.index)
	plot2d,im,xarr=state.xa,yarr=state.ya,title=title, $
		xtitle=state.xtitle,ytitle=state.ytitle, $
		stamp=state.timestamp
	endif else begin
		r=dialog_message('Detector not defined!!',/error)
	end
      END
  'NEXUS_SEL_CLOSE': BEGIN
	widget_control,Event.top,/destroy
	return
      END
  ENDCASE
  widget_control,Event.top,set_uvalue=state,/no_copy
END




PRO nexus_2D_select, state, GROUP=Group

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  NEXUS_2DSELECT = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, title=state.title +' - 2D Selector', $
      MAP=1, $
      UVALUE='NEXUS_2DSELECT')

;  label0 = widget_label(NEXUS_2DSELECT,value='Select Detector')

  BASE2 = WIDGET_BASE(NEXUS_2DSELECT, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE2_1 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2_1')

  panimagebtn = Widget_button(BASE2_1,value='PanImage...', UVALUE='NEXUS_SEL_PANIMAGE')
  Calibtn = Widget_button(BASE2_1,value='2D Calibration...', UVALUE='NEXUS_SEL_CALIB')
  BASE2_2 = WIDGET_BASE(BASE2_1, $
      COLUMN=1, frame=2, $
      MAP=1, $
      UVALUE='BASE2_1')

  overlayinitbtn = Widget_button(BASE2_2,value='Overlay2D Init...', UVALUE='NEXUS_SEL_OVERLAY2D_INIT')
  overlaybtn = Widget_button(BASE2_2,value='Overlay2D...', UVALUE='NEXUS_SEL_OVERLAY2D')

  BMP749 = [ $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 23b, 228b, 56b, 111b ], $
    [ 18b, 164b, 108b, 247b ], $
    [ 50b, 166b, 68b, 145b ], $
    [ 82b, 165b, 4b, 17b ], $
    [ 82b, 181b, 5b, 49b ], $
    [ 82b, 21b, 245b, 47b ], $
    [ 210b, 245b, 37b, 193b ], $
    [ 146b, 20b, 53b, 129b ], $
    [ 146b, 20b, 61b, 145b ], $
    [ 18b, 20b, 33b, 247b ], $
    [ 23b, 20b, 33b, 111b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 30b, 31b, 0b, 0b ], $
    [ 62b, 63b, 0b, 0b ], $
    [ 48b, 99b, 0b, 0b ], $
    [ 48b, 99b, 0b, 0b ], $
    [ 48b, 99b, 0b, 0b ], $
    [ 24b, 99b, 0b, 0b ], $
    [ 12b, 99b, 102b, 6b ], $
    [ 6b, 115b, 102b, 6b ], $
    [ 62b, 63b, 0b, 0b ], $
    [ 62b, 31b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ]  $
  ]
  image2dBtn = WIDGET_BUTTON( BASE2_1, $
      UVALUE='NEXUS_SEL_IMAGE2D', $
      VALUE=BMP749,/BITMAP)

 help = Widget_button(BASE2_1,value='Help...', UVALUE='NEXUS_SEL_2DHELP')

  ListVal251 =  'D'+ [strtrim(indgen(9)+1,2),'A','B','C','D','E','F' , $
                '01','02','03','04','05','06','07','08','09', $
                strtrim(indgen(61)+10,2)] +'    '

  LIST3 = WIDGET_LIST( BASE2,VALUE=ListVal251, $
      UVALUE='NEXUS_SEL_DET', $
      YSIZE=10)
  widget_control,LIST3,set_list_select=15

  close = Widget_button(BASE2,value='Close', UVALUE='NEXUS_SEL_CLOSE')

  widget_control,NEXUS_2DSELECT,set_uvalue=state,/no_copy
  WIDGET_CONTROL, NEXUS_2DSELECT, /REALIZE

  XMANAGER, 'NEXUS_2DSELECT', NEXUS_2DSELECT
END


PRO nexus_scan_startup,file,nexus_scan_state
COMMON NEXUS_BLOCK,nexus_state

	if HDF_ISHDF(file) eq 0 then begin
		 r = dialog_message([file,'is not a nexus file !!!'],/error)
		 return
	end
	if n_elements(nexus_state) then V = nexus_state.obj
	nexus_scan_open,V,file=file
widget_control,/HOURGLASS
	v->print,/nowin
	nexus_scan_init,V,seqs

	st = 'Nexus Scan Seq # [1-' + strtrim(nexus_state.Nscan,2) + ']'
	lp = strpos(file,!os.file_sep,/reverse_search)
	nexus_scan_state.datapath = strmid(file,0,lp)
	nexus_scan_state.file = file

  if n_elements(nexus_state) then begin
  	scanno = nexus_state.scanno(0:nexus_state.Nscan-1)
  	ndims = nexus_state.ndims(0:nexus_state.Nscan-1)
  	listscan = 'SCAN_'+strtrim(scanno,2) + ' : ( ndims='+strtrim(ndims,2)+' )'
	ih = where(nexus_state.scanh_gseq gt 0)
	for i=0,n_elements(ih)-1 do begin
	if ih(0) ge 0 then begin
	listscan(ih(i)) = listscan(ih(i)) + '   scanH_axis gid='+ $
		 strtrim(nexus_state.scanh_gseq(ih(i)),2)
	end
	end

	widget_control,nexus_scan_state.fileWID,set_value=file
	widget_control,nexus_scan_state.listWID,set_value=listscan
  end
END




PRO NEXUS_SCANH_Event, Event
COMMON NEXUS_BLOCK,nexus_state

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'NEXUS_SCANH_RECORD': BEGIN
	seq = nexus_state.scanh_gseq(nexus_state.seqno-1) $
		- nexus_state.scanh_n(nexus_state.seqno-1) + 1 + event.index
	nexus_scan_getScanH,seq,group=Event.top
      END
;  'NEXUS_SCANH_AXIS': BEGIN
;	nexus_state.axis = Event.value
;      END
  'NEXUS_SCANH_CLOSE': BEGIN
	widget_control,Event.top,/destroy
      END
  ENDCASE
END



PRO nexus_scanh, nseq, title=title, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  tit='SCANH'
  if keyword_set(title) then tit=title

  NEXUS_SCANH = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, title=tit, $
      UVALUE= 'NEXUS_SCANH')

  BASE2 = WIDGET_BASE(NEXUS_SCANH, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  listseq = title+ ' : ScanH Group #'+string(indgen(nseq))
  LIST3 = WIDGET_LIST( BASE2,VALUE=listseq, $
      UVALUE='NEXUS_SCANH_RECORD', $
      YSIZE=5)

  BASE4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

;  BGROUP3 = CW_BGROUP( BASE4, ['Index','Values'], $
;      ROW=1, $
;      EXCLUSIVE=1, $
;      LABEL_LEFT='AXIAL', $
;      UVALUE='NEXUS_SCANH_AXIS')

  BUTTON5 = WIDGET_BUTTON( BASE4, $
      UVALUE='NEXUS_SCANH_CLOSE', $
      VALUE='Close')



  WIDGET_CONTROL, NEXUS_SCANH, /REALIZE

  XMANAGER, 'NEXUS_SCANH', NEXUS_SCANH
END

PRO NEXUS_SCAN_Event, Event
COMMON NEXUS_BLOCK,nexus_state

  WIDGET_CONTROL, Event.top, GET_UVALUE=nexus_scan_state,/no_copy
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'NEXUS_SCAN_FILE': BEGIN
	datapath = nexus_scan_state.datapath
	file = dialog_pickfile(filter='*.nexus',get_path=p,group=event.top, $
		/must_exist,path=datapath,/read,title='Select Nexus Scan File')
	if file ne '' then begin
	nexus_scan_startup,file,nexus_scan_state
	nexus_scan_writeConfig,nexus_scan_state
	end
      END
  'NEXUS_SCAN_FILENAME': BEGIN
	file = Event.Value
	if file ne '' then $
	nexus_scan_startup,file,nexus_scan_state
	nexus_scan_writeConfig,nexus_scan_state
      END
  'NEXUS_SCAN_SCANNO': BEGIN
	widget_control,/hourglass
	sel_scanno = Event.index ;widget_info(Event.id,/list_select)
	nexus_state.seqno = sel_scanno + 1
	nexus_scan_current,group=Event.top
;	if nexus_state.scanh_gseq(sel_scanno) gt 0 then begin
;		nexus_scanh,nexus_state.scanh_n(sel_scanno),title=nexus_state.title,Group=Event.top
;		end
      END
  'NEXUS_SCAN_HELP': BEGIN
	nexus_scan_help
      END
  'NEXUS_SCAN_OVERLAY1D': BEGIN
	if nexus_state.Nscan gt 1 then $ 
	nexus_scan_pick1d,group=Event.top
      END
  'NEXUS_SCAN_LISTING': BEGIN
	nexus_scan_list,str
	xdisplayfile,text=str,width=55,group=Event.top
;	r = dialog_message(str,/info,title='LIST of NEXUS SCANS')
      END
  'NEXUS_SCAN_CLOSE': BEGIN
	nexus_scan_writeConfig,nexus_scan_state
	widget_control,Event.top,/destroy
;	exit
	return
      END
  ENDCASE

  WIDGET_CONTROL, Event.top, SET_UVALUE=nexus_scan_state,/no_copy
END



PRO NEXUS_SCAN, file=file, GROUP=Group
COMMON NEXUS_BLOCK,nexus_state


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  NEXUS_SCAN = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, title='NEXUS_SCAN', $
      UVALUE='NEXUS_SCAN')

  BASE2 = WIDGET_BASE(NEXUS_SCAN, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  BUTTON4 = WIDGET_BUTTON( BASE3, $
      UVALUE='NEXUS_SCAN_FILE', $
      VALUE='File')

  filename=''
  FIELD5 = CW_FIELD( BASE3,VALUE=filename, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Name', Xsize=55, $
      UVALUE='NEXUS_SCAN_FILENAME')

  listscan = ['','','','']
  LIST2 = WIDGET_LIST( BASE2,VALUE=listscan, $
      UVALUE='NEXUS_SCAN_SCANNO',  $
      YSIZE=10)

  BASE13 = WIDGET_BASE(NEXUS_SCAN, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE13')

  BUTTON30 = WIDGET_BUTTON( BASE13, $
      UVALUE='NEXUS_SCAN_OVERLAY1D', $
      VALUE='Overlay1D...')

  BUTTON31 = WIDGET_BUTTON( BASE13, $
      UVALUE='NEXUS_SCAN_LISTING', $
      VALUE='ListScan#...')


  BUTTON28 = WIDGET_BUTTON( BASE13, $
      UVALUE='NEXUS_SCAN_HELP', $
      VALUE='Help...')

  BUTTON29 = WIDGET_BUTTON( BASE13, $
      UVALUE='NEXUS_SCAN_CLOSE', $
      VALUE='Close')

  nexus_scan_state = { base: NEXUS_SCAN, $
	listWID: LIST2, $
	fileWID  : FIELD5, $
	file     : '', $
	datapath : '' $
	}

  fd = findfile('NX.config',count=ct)
  if ct then nexus_scan_readConfig,nexus_scan_state
  if keyword_set(file) then begin
	filename = file
	if HDF_ISHDF(filename) then nexus_scan_startup,filename,nexus_scan_state
  if n_elements(nexus_scan_state) then $
  widget_control,nexus_scan_state.fileWID,set_value=nexus_scan_state.file
  end

  WIDGET_CONTROL, NEXUS_SCAN, SET_UVALUE=nexus_scan_state,/no_copy
  WIDGET_CONTROL, NEXUS_SCAN, /REALIZE


  XMANAGER, 'NEXUS_SCAN', NEXUS_SCAN
END
