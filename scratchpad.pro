

PRO SCRATCHPAD_MAIN13_Event, Event
COMMON SCRATCHPAD_BLOCK, scratchpad_id

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'SCRATCHPAD_TEXT': BEGIN
      END
  'BUTTON5': BEGIN
	WIDGET_CONTROL,scratchpad_id.base,/DESTROY
      END
  'BUTTON6': BEGIN
	WIDGET_CONTROL,scratchpad_id.textID,GET_VALUE=st
	file = dialog_pickfile(/write,filter='*',get_path=newp,group=event.top,$
		path=scratchpad_id.path,title='ScratchPad save ...')
	if strtrim(file,2) ne '' then begin
	scratchpad_id.no = n_elements(st)
	openw,unit,file,error=err,/get_lun
	if err ne 0 then begin
		res = dialog_message(/Error,'Failed to write the file')
		return
		end
	for i=0,scratchpad_id.no-1 do begin
		scratchpad_id.text(i) = st(i) 
		printf,unit,st(i)
	end
	close,unit
	scratchpad_id.file = file
	scratchpad_id.path = newp
	endif else begin
		res = dialog_message(/Info,'File name needed in order to save', $
			dialog_parent=scratchpad_id.base)
	end
      END
  'BUTTON7': BEGIN
	file = dialog_pickfile(/read,filter='*',get_path=newp,group=event.top,$
		path=scratchpad_id.path,title='ScratchPad Open ...')
	if strtrim(file,2) ne '' then begin
	scratchpad_id.file = file
	scratchpad_id.path = newp
	openr,unit,file,/get_lun,error=err
	st = fstat(unit)
	buff = make_array(st.size,/byte)
	readu,unit,buff
	close,unit
	WIDGET_CONTROL,scratchpad_id.textID,SET_VALUE=string(buff)
	end
      END
  ENDCASE


END


; DO NOT REMOVE THIS COMMENT: END SCRATCHPAD_MAIN13
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO scratchpad, GROUP=Group
COMMON SCRATCHPAD_BLOCK, scratchpad_id

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  SCRATCHPAD_MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, TITLE='ScratchPad', $
      MAP=1, $
      UVALUE='SCRATCHPAD_MAIN13')

  BASE2 = WIDGET_BASE(SCRATCHPAD_MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  TextVal406 = [ $
    '' ]
  TEXT3 = WIDGET_TEXT( BASE2,VALUE=TextVal406, $
      ALL_EVENTS=1, $
      EDITABLE=1, /SCROLL, $
      NO_NEWLINE=1, $
      UVALUE='SCRATCHPAD_TEXT', $
      XSIZE=40, $
      YSIZE=20)

  BASE4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  BUTTON7 = WIDGET_BUTTON( BASE4, $
      UVALUE='BUTTON7', $
      VALUE='Open ...')

  BUTTON6 = WIDGET_BUTTON( BASE4, $
      UVALUE='BUTTON6', $
      VALUE='Save ...')

  BUTTON5 = WIDGET_BUTTON( BASE4, $
      UVALUE='BUTTON5', $
      VALUE='Done')



  WIDGET_CONTROL, SCRATCHPAD_MAIN13, /REALIZE

  CD,CURRENT=p
  scratchpad_id = { $
		base: SCRATCHPAD_MAIN13, $
		textID: TEXT3, $
		no : 0, $
		text: make_array(1000,/string), $
		path : p, $
		file: '' $
		}
		 

  XMANAGER, 'SCRATCHPAD_MAIN13', SCRATCHPAD_MAIN13
END
