
PRO RENAME_DIALOG_Event, Event
COMMON RENAME_BLOCK,rename_ids


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'RENAME_DIALOGACCEPT': BEGIN
      Print, 'Event for Accept'
	WIDGET_CONTROL,rename_ids.path_id,GET_VALUE=pathdir
	WIDGET_CONTROL,rename_ids.old_id,GET_VALUE=file1
	WIDGET_CONTROL,rename_ids.new_id,GET_VALUE=file2
	if strtrim(file2(0),2) eq '' then return
	len = strlen(pathdir(0))
	if strmid(pathdir(0),len-1,1) ne !os.file_sep then $
		pathdir = pathdir(0)+!os.file_sep
	oldname = strtrim(file1(0),2)
	newname = pathdir+strtrim(file2(0),2)
	spawn,!os.mv + ' '+ oldname + ' '+newname +' &',res
	WIDGET_CONTROL,Event.Top,/DESTROY
      END
  'RENAME_DIALOGCANCEL': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END RENAME_DIALOG
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO rename_dialog, pathdir,oldname,newname, GROUP=Group
COMMON RENAME_BLOCK,rename_ids

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  RENAME_DIALOG = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
	TITLE='Rename File ...', $
      UVALUE='RENAME_DIALOG')

  BASE2 = WIDGET_BASE(RENAME_DIALOG, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  RENAME_OLD = CW_FIELD( BASE2,VALUE=oldname, $
      ROW=1, $
      STRING=1, $
  ;    RETURN_EVENTS=1, $
      TITLE='Old Filename:', $
      UVALUE='RENAME_OLD', $
      XSIZE=60)

  RENAME_PATH = CW_FIELD( BASE2,VALUE=pathdir, $
      ROW=1, $
      STRING=1, $
  ;    RETURN_EVENTS=1, $
      TITLE='Dest Path:', $
      UVALUE='RENAME_PATH', $
      XSIZE=60)

  new=''
  if n_elements(newname) then new=newname
  RENAME_NEW = CW_FIELD( BASE2,VALUE=new, $
      ROW=1, $
      STRING=1, $
  ;    RETURN_EVENTS=1, $
      TITLE='New Filename:', $
      UVALUE='RENAME_NEW', $
      XSIZE=60)

  BASE5 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE5')

  BUTTON6 = WIDGET_BUTTON( BASE5, $
      UVALUE='RENAME_DIALOGACCEPT', $
      VALUE='Accept')

  BUTTON7 = WIDGET_BUTTON( BASE5, $
      UVALUE='RENAME_DIALOGCANCEL', $
      VALUE='Cancel')

  rename_ids = { $
	path: pathdir, $
	oldname: oldname, $
	newname: oldname, $
	path_id: RENAME_PATH, $
	old_id: RENAME_OLD, $
	new_id: RENAME_NEW $
	}

  WIDGET_CONTROL, RENAME_DIALOG, /REALIZE

  XMANAGER, 'RENAME_DIALOG', RENAME_DIALOG
END