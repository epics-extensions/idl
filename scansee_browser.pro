;
; scanee_browser.pro
;
@readScan.pro
@read_scan.pro.R2

PRO scansee_browser_Event, Event


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'ASSIGN_NAMES': BEGIN
	assignname_read,dnames,GROUP=Event.top
      END
  'ASSIGN_OVERLAY': BEGIN
	scanSee_overlay,GROUP=Event.top
      END
  'ASSIGN_SCANSEE': BEGIN
	spawn,'scanSee -D &'
      END
  'ASSIGN_VW2D': BEGIN
	spawn,'vw2d &'
      END
  'ASSIGN_PICK3d': BEGIN
      spawn,'pick3d &'
      END
  'ASSIGN_SB': BEGIN
      spawn,'SB &'
      END
  'ASSIGN_DONE': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
	exit
      END
  'ASSIGN_HELP1': BEGIN
      str = [ $
		'ScanSee Browser allows the user to access following IDL', $
		'programs. ','', $
;		'Detector Names... - run the assign_detName program', $
		'Run scanSee...     - run scanSee program', $
		'Assign DetNames & Run VW2D... - run vw2d program with new detector names', $
		'Run Pick3d...     - run pick3d program', $
		'Run 1D/2D/3D SB... - run 1D/2D/3D scan browser', $
		'Run 1D Overlay... - run scanSee_overlay program' $
	]
	r = dialog_message(str,/info,title='ScanSee Browser Info') 
      END
  ENDCASE
END





PRO scansee_browser, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  scansee_browser = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      TITLE='ScanSee Browsers', $
      UVALUE='scansee_browser')

  BASE2 = WIDGET_BASE(scansee_browser, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BUTTON22 = WIDGET_BUTTON( BASE2, $
      UVALUE='ASSIGN_HELP1', $
      VALUE='Help...')

;  BUTTON3 = WIDGET_BUTTON( BASE2, $
;      UVALUE='ASSIGN_NAMES', $
;      VALUE='Redefine Detector Names...')

  BUTTON2 = WIDGET_BUTTON( BASE2, $
      UVALUE='ASSIGN_SCANSEE', $
      VALUE='Run SCANSEE...')

  BUTTON4 = WIDGET_BUTTON( BASE2, $
      UVALUE='ASSIGN_VW2D', $
      VALUE='Assign DetNames & Run VW2D...')

  BUTTON6 = WIDGET_BUTTON( BASE2, $
      UVALUE='ASSIGN_PICK3d', $
      VALUE='Run PICK3D...')

  BUTTON7 = WIDGET_BUTTON( BASE2, $
      UVALUE='ASSIGN_SB', $
      VALUE='Run 1D/2D/3D SB...')

  BUTTON5 = WIDGET_BUTTON( BASE2, $
      UVALUE='ASSIGN_OVERLAY', $
      VALUE='Run 1D OVERLAY...')

  BUTTON13 = WIDGET_BUTTON( BASE2, $
      UVALUE='ASSIGN_DONE', $
      VALUE='Done')


  WIDGET_CONTROL, scansee_browser, /REALIZE

  XMANAGER, 'scansee_browser', scansee_browser
END
