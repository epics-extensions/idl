@PS_open.pro
@saveImage.pro

PRO img_replot,img_state

	data = img_state.data

	flip = img_state.flip
	if flip eq 2 then data = reverse(data,2)
	if flip eq 1 then data = reverse(data,1)
  sz = size(data)
  if sz(0) eq 2 then begin
  if !d.name ne 'PS' then begin
	 device,decomposed=0
  	wset,img_state.win
	tdata = congrid(data,img_state.width,img_state.height)
  end
  
	palette = img_state.palette
	r = palette(0,*)
	g = palette(1,*)
	b = palette(2,*)
	tvlct,r,g,b
  
  tv,tdata

  end

  img_state.data = data
  img_state.flip = 0
END

PRO PDMENU3_Event, Event,img_state

  CASE Event.Value OF

  'Export.TIFF': BEGIN
	save_tiff,win=img_state.win
    END
  'Export.PNG': BEGIN
	save_png,win=img_state.win
    END
  'Export.PICT': BEGIN
	save_pict,win=img_state.win
    END
  ENDCASE
END

PRO IMG13_Event, Event
  widget_control,Event.top,get_uvalue=img_state

IF (Event.id EQ Event.top) THEN BEGIN
        WIDGET_CONTROL,img_state.id_draw, SCR_XSIZE=Event.x, SCR_YSIZE=Event.y
	img_state.width = Event.x
	img_state.height = Event.y
print,Event.x,Event.y
	img_replot,img_state
  	widget_control,Event.top,set_uvalue=img_state
	return
ENDIF

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  ; Event for PDMENU3
  'PDMENU3': PDMENU3_Event, Event,img_state

  'IMG_UPDOWN': BEGIN
	img_state.flip = 2
      END
  'IMG_LEFTRIGHT': BEGIN
	img_state.flip = 1
      END
  'IMG_PRINT': BEGIN
	PS_TVRD,wid=img_state.win
      END
  'IMG_PRINTER': BEGIN
	PS_printer
      END
  'IMG_DONE': BEGIN
	widget_control,Event.top,/destroy
	return
      END
  'IMG_DRAW': BEGIN
      Print, 'Event for IMG_DRAW'
      END
  ENDCASE

	img_replot,img_state

  widget_control,Event.top,set_uvalue=img_state
END



PRO plot2d_image, GROUP=Group,data,palette=palette,wtitle=wtitle
; plot real image data with real palette
;           DATA - 2D image byte array to be plotted
; PALETTE(3,256) - color palette asscociated with the image data
;                  if not specified the current color table is used
; WTITLE         - string to specify the window title
;


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  TITLE='IMG with Palette'
  if keyword_set(wtitle) then title=wtitle

  IMG13 = WIDGET_BASE(GROUP_LEADER=Group, $
 	/TLB_SIZE_EVENTS, $       ; resizable window
      ROW=1, $
      MAP=1, $
      TITLE=title, $
      UVALUE='IMG13')

  BASE2 = WIDGET_BASE(IMG13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  BUTTON4 = WIDGET_BUTTON( BASE3, $
      UVALUE='IMG_UPDOWN', $
      VALUE='Flip Vertical')

  BUTTON5 = WIDGET_BUTTON( BASE3, $
      UVALUE='IMG_LEFTRIGHT', $
      VALUE='Flip Horizontal')

  MenuDesc1144 = [ $
      { CW_PDMENU_S,       3, 'Export' }, $ ;        0
        { CW_PDMENU_S,       0, 'TIFF' }, $ ;        1
        { CW_PDMENU_S,       0, 'PNG' }, $ ;        2
        { CW_PDMENU_S,       2, 'PICT' } $  ;      3
  ]

  PDMENU3 = CW_PDMENU( BASE3, MenuDesc1144, /RETURN_FULL_NAME, $
      UVALUE='PDMENU3')

  BUTTON9 = WIDGET_BUTTON( BASE3, $
      UVALUE='IMG_PRINT', $
      VALUE='Print')

  BUTTON10 = WIDGET_BUTTON( BASE3, $
      UVALUE='IMG_PRINTER', $
      VALUE='Printer...')

  BUTTON11 = WIDGET_BUTTON( BASE3, $
      UVALUE='IMG_DONE', $
      VALUE='Done')

  width =500
  height=500
  sz = size(data)
  width=sz(1)
  height=sz(2)

  DRAW12 = WIDGET_DRAW( BASE2, $
;      BUTTON_EVENTS=1, $
      RETAIN=2, $
      UVALUE='IMG_DRAW', $
      XSIZE=width, $
      YSIZE=height)

  WIDGET_CONTROL, IMG13, /REALIZE

  ; Get drawable window index
  WIDGET_CONTROL, DRAW12, GET_VALUE=drawWin

	tvlct,r,g,b,/get
	opalette = reform([r,g,b],3,256)
  if n_elements(palette) eq 0 then begin
	palette = opalette
  end

  img_state = { $
	id_draw : DRAW12, $
	win : drawWin, $
	width : width, $
	height : height, $
	flip: 0., $
	opalette: opalette, $
	palette: palette, $
	data : data $
	}
  widget_control,IMG13,set_uvalue=img_state

  img_replot,img_state

  XMANAGER, 'IMG13', IMG13
END
