;
; sscan__define.pro
;
; 1) Object method allows the user to excess 1D/2D/3D data from the sscan 
;     object
;
; 	v = obj_new('sscan',file='/home/beams/CHA/data/xxx/cha_0001.mda')
;
;    Pick mda file from the specified input directory
;
; 	v = obj_new('sscan',path='/home/beams/CHA/data/xxx')
;
;	v->pickfile
;	v->read,da1d=da1d,da2d=da2d,....
;
; 2) The ILD widget program sscan allows the user view data through 
;    packaged idl program 
;   
;       sscan 
;

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

PRO sscan::get,da1d=da1d,da2d=da2d,da3d=da3d,pa1d=pa1d,pa2d=pa2d,pa3d=pa3d,labels=labels,id_def=id_def
	labels = self.labels
	id_def = self.id_def
	if self.rank eq 1 then begin
	da1d = *self.da(0)
	pa1d = *self.pa(0)
	end
	if self.rank eq 2 then begin
	da2d = *self.da(0)
	pa2d = *self.pa(0)
	da1d = *self.da(1)
	pa1d = *self.pa(1)
	end
	if self.rank eq 3 then begin
	da3d = *self.da(0)
	pa3d = *self.pa(0)
	da2d = *self.da(1)
	pa2d = *self.pa(1)
	da1d = *self.da(2)
	pa1d = *self.pa(2)
	end
END

PRO sscan::print
	print,'FILE:',self.FILE
	print,'RANK:',self.RANK
	print,'NPTS:',self.npts
	print,'CPT:',self.CPT
	print,'NB_POS:',self.NB_POS
	print,'NB_DET:',self.NB_DET
	print,'NB_TRG:',self.NB_TRG
	print,'ID_DEF:',self.ID_DEF
	print,'DETMAX:',self.DETMAX
	print,'TS1:',self.TS1
	print,'TS2:',self.TS2
help,*self.PA(0),*self.PA(1),*self.PA(2)
help,*self.DA(0),*self.DA(1),*self.DA(2)
END

FUNCTION sscan::Init,file=file,path=path,HD_Pos=HD_Pos,HD_Det=HD_Det,HD_Trg=HD_Trg
; KEYWORD:
;   file  - specifies input mda file used
;   path  - dialog_pickfile will be used to select the mda file with
;           specified path directory
;
; HD_pos  - returns positoner info array
; HD_det  - returns detector info array
; HD_trg  - returns trig info array
; 
loadct,39
	if keyword_set(file) then sscan_read,SSD,file=file,/echo else $
	sscan_read,SSD,path=path,/echo
	if n_elements(SSD) eq 0 then return,0

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
	self.labels = SSD.labels 
	self.labels = SSD.labels 
	self.labels = SSD.labels 
	self.labels = SSD.labels 
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


