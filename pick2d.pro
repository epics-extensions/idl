PRO ASCII2D_REP,da2d,x,y,dir,outname,Event

        nx = n_elements(x)

        t_format = 'G18.8'
        if keyword_set(format) then t_format = format
        fwidth = 'I'+ strmid(t_format,1,strpos(t_format,'.')-1)
        col = strmid(t_format,1,strpos(t_format,'.')-1)

                s = size(da2d)
                dim = s(1:2)
                f0 = '(";    \ Z",10X,'+strtrim(dim(1),2)+fwidth+',/,";   Y \",/"; ")'
                f1 = '('+t_format+',I,'+strtrim(dim(1),2)+'('+t_format+'))'

                f2 = '(";        ",'+col+'X,'+strtrim(dim(1),2)+'('+t_format+'))'
        openw,1,'plot2d.txt'
        printf,1,'; Dest Path: ',dir
        printf,1,'; Dest File: ',outname
        printf,1,';   data(',strtrim(dim(0),2),',',strtrim(dim(1),2),')'

                printf,1,format=f2,y
                printf,1,format=f0,indgen(dim(1))

          for i=0,nx-1 do begin
          lineA = reform(da2d(i,*))
          printf,1,format=f1,x(i),i,lineA
          end
        close,1
        xdisplayfile,'plot2d.txt',Group=Event.top
        rename_dialog,dir,'plot2d.txt',outname,GROUP=Event.top

END



PRO PICK2D_Event, Event

  WIDGET_CONTROL,Event.top,GET_UVALUE=data_info
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  title=data_info.title +data_info.class+'-'+ data_info.detname(data_info.idet)
  data = data_info.im_array(*,*,data_info.idet)

  CASE Ev OF 

  '3D_2DMENU_PLOT2D': BEGIN
	plot2d,data,xarr=data_info.x,yarr=data_info.y,title=title
      END
  '3D_2DMENU_ASCII2D': BEGIN
	dir = data_info.path
	outname=data_info.class + '.im'+data_info.detname(data_info.idet)
	ASCII2D_REP,data,data_info.x,data_info.y,dir,outname,Event
      END
  '3D_2DMENU_PICK1D': BEGIN
	calibra_pick1d,data,xa=data_info.x,ya=data_info.y, $
		title =title ,Group=Event.top	
      END
  '3D_2DMENU_CLOSE': BEGIN
	WIDGET_CONTROL,Event.top,/DESTROY
	return
      END
  '3D_2DMENU_LIST': BEGIN
	id = widget_info(Event.id,/LIST_SELECT)
	if id ge data_info.ndet then begin
		r = dialog_message('Detector not used!',/info)
		return
	end
	if data_info.id_def(id) eq 0 then begin
		r = dialog_message('Detector not used!',/info)
		return
	end
	data_info.idet = id
  	data = data_info.im_array(*,*,data_info.idet)
	plot2d,data,xarr=data_info.x,yarr=data_info.y
      END
  ENDCASE

  	WIDGET_CONTROL,Event.top,SET_UVALUE=data_info
	
END




PRO PICK2D, data,x,y, def=def, title=title, GROUP=Group,Path=Path,Class=Class
;+
; NAME:
;   PICK2D
;
; PURPOSE:
;       This dialog allows the user dynamically picks the desired
;       detector numbers out of a 2D data array. A user can freely
;       view the 2D data of the selected detector by using PLOT2D,
;       ASCII2D, and 2D_PICK1D sub-programs.
;
; CALLING SEQUENCE:
;       pick2d, data, x, y [,Def=def] [,Title=title] [,Path=path]
;                               [,Class=class] [,Group=group]
;
; INPUT:
;     Data:    Input multiple 2D data array Data(WIDTH,HEIGHT,NDET)
;		WIDTH - X dim size
;		HEIGHT - Y dim size
;		NDET  - last # of detector defined
;        X:    Vector of X positioner values, default index vector
;        Y:    Vector of Y positioner values, default index vector
;
; KEYWORDS:
;  Def:        The detector defined indicater vector of NDET values, 0 
;               indicates not defined, 1 indicates defined, default all 1
;  Title:      Optional prefix title, default ''
;  Path:       Full path specifies the starting file directory where the scan
;              files are stored.
;  Group:      Specifies the parent widget ID. If specified, the destroy of
;              parent window resulted the destroy of Pick3d window.

;  Class:      Specifies the title class, e.g. filename where data is extracted 
;
; RESTRICTIONS:
;    Number of detector NDET must be greater than 15  and less than 85 in the
;    data array.
;
; EXAMPLE:
;    PICK2D,data
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, March 1, 2001.
;-


if n_params() eq 0 then return

sz = size(data)
if sz(0) lt 3 then return

ndet = sz(3)
if n_elements(x) eq 0 then x = indgen(sz(1))
if n_elements(y) eq 0 then y = indgen(sz(2))
id_def = make_array(sz(3),/int,value=1)
detname = ['D'+ strtrim(indgen(9)+1,2), 'DA','DB','DC','DD','DE','DF']
detname = [ detname, 'D0'+strtrim(indgen(9)+1,2)]
detname = [ detname, 'D'+strtrim(indgen(61)+10,2)]

cd,current=dir
data_info = { x : x, $
	y : y, $
	im_array : data, $
	path : dir, $
	class : '', $
	id_def : id_def, $
	width: sz(1), $
	height: sz(2), $
	detname: detname, $
	listWid: 0L, $
	title :'', $
	ndet: ndet, $
	idet: 0 $
	}

if keyword_set(path) then data_info.path= path 
if keyword_set(class) then data_info.class= class 
if keyword_set(title) then data_info.title= title 
if keyword_set(def) then begin
	data_info.id_def = def
	data_info.ndet = n_elements(def)
end

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  PICK2D = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
	title='PICK2D', $
      UVALUE='PICK2D')

  BASE2 = WIDGET_BASE(PICK2D, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  BASE3 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE3')

  BUTTON5 = WIDGET_BUTTON( BASE3, $
      UVALUE='3D_2DMENU_PLOT2D', $
      VALUE='PLOT2D')

  BUTTON6 = WIDGET_BUTTON( BASE3, $
      UVALUE='3D_2DMENU_ASCII2D', $
      VALUE='ASCII2D')

  BUTTON7 = WIDGET_BUTTON( BASE3, $
      UVALUE='3D_2DMENU_PICK1D', $
      VALUE='2D_PICK1D...')

  BUTTON9 = WIDGET_BUTTON( BASE3, $
      UVALUE='3D_2DMENU_CLOSE', $
      VALUE='Done')


  LIST8 = WIDGET_LIST( BASE4,VALUE=data_info.detname, $
      UVALUE='3D_2DMENU_LIST', $
      YSIZE=10)
  WIDGET_CONTROL,LIST8,SET_LIST_SELECT=15
  data_info.idet = 15

  WIDGET_CONTROL, PICK2D, SET_UVALUE= data_info

  WIDGET_CONTROL, PICK2D, /REALIZE

  XMANAGER, 'PICK2D', PICK2D
END
