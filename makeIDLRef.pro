@mk_html_help.pro

PRO makeCurvFit
files = [ $
	'/usr/local/rsi/idl_5/lib/comfit.pro', $
	'/usr/local/rsi/idl_5/lib/curvefit.pro', $
	'/usr/local/rsi/idl_5/lib/gaussfit.pro', $
	'/usr/local/rsi/idl_5/lib/ladfit.pro', $
	'/usr/local/rsi/idl_5/lib/linfit.pro', $
	'/usr/local/rsi/idl_5/lib/lmfit.pro', $
	'/usr/local/rsi/idl_5/lib/poly_fit.pro', $
	'/usr/local/rsi/idl_5/lib/polyfitw.pro', $
	'/usr/local/rsi/idl_5/lib/regress.pro', $
;	'/usr/local/rsi/idl_5/lib/sfit.pro', $
	'/usr/local/rsi/idl_5/lib/svdfit.pro' $
	]

descript=[ '<H1>IDL Curve Fitting Routines</H1> ', $
'<PRE>', $
'<BR><BR><I><B>For detail information please refer to the IDL reference manual.</B></I>', $
'<BR>COMFIT      -  Six common types of gradient-expansion least-square fit', $
'<BR>		EXPONENTIAL       Y = a0  * a1^x + a2 ', $
'<BR>		GEOMETRIC	  Y = a0 * x^a1 + a2 ', $
'<BR>		GOMPERTZ	  Y = a0 * a1^(a2*x) + a3 ', $
'<BR>		HYPERBOLIC	  Y = 1./(a0 + a1*x) ', $
'<BR>		LOGISTIC	  Y = 1./(a0 * a1^x + a2) ', $
'<BR>		LOGSQUARE	  Y = a0 + a1*alog10(x) + a2 * alog10(x)^2 ', $
'<BR>CURVEFIT    -  Least-square fit to an arbitrary non-linear function', $
'<BR>GAUSSFIT    -  Least-square fit to ', $
'<BR>                  F(x) = A0*EXP(-((x-A1)/A2)^2/2) + A3 + A4*x + A5*x^2  ',  $
'<BR>LADFIT      -  Least-absolute-deviation fit to        Y = A + Bx ', $
'<BR>LINFIT      -  Minimize-chi-square error fit to       Y = A + Bx ', $
'<BR>LMFIT       -  Non-linear least-square fit to  n arbitrary non-linear function       ', $
'<BR>POLYFITW    -  Weighted least-square polynomial fit', $
'<BR>POLY_FIT    -  Least-square polynomial fit', $
'<BR>REGRESS     -  Multiple linear regression fit', $
'<BR>SVDFIT      -  Least-square fit to user-supplied/built-in/legendre polynomial ', $
'</PRE>' $
	]
mk_html_help,files,'html/CurvFitRef.html', descript=descript
END


PRO catcherRef
descript=[ '<H1>catcher_v1.pro</H1> ', $
'At APS this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This program normally is automatically invoked by the script command <B>catcher</B>.' $
]
mk_html_help,'catcher_v1.pro','html/catcherRef.html', $
	descript=descript
END

PRO v1dOvlRef
descript=[ '<H1>view1d_overlay.pro</H1> ', $
'At APS this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This program allows the user to overlay few different 1D scan data on the same plot.', $
'This program normally is automatically invoked by the script command <B>viewer</B>.' $
]
mk_html_help,'view1d_overlay.pro','html/v1dOvlRef.html', $
	descript=descript
END

PRO dcviewerRef
descript=[ '<H1>dcviewer.pro</H1> ', $
'At APS this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This program integrates the <B>view1d</B>, <B>view2d</B>, <B>bi2xdr_converter</B> programs into a single system', $
'This program normally is automatically invoked by the script command <B>viewer</B>.' $
]
mk_html_help,'dcviewer.pro','html/dcviewerRef.html', $
	descript=descript
END

PRO view1dRef
descript=[ '<H1>view1d.pro</H1> ', $
'At APS this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This program allows the user to view 1D scan data.', $
'This program can be directly accessed by the <B>viewer</B>.' $
]
mk_html_help,'view1d.pro','html/view1dRef.html', $
	descript=descript
END


PRO view2dRef
descript=[ '<H1>view2d.pro</H1> ', $
'At APS this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This program allows the user to view 2D image data.', $
'This program can be directly accessed by the <B>catcher</B> or <B>viewer</B>.' $
]
mk_html_help,'view2d.pro','html/view2dRef.html', $
	descript=descript
END

PRO plot1dRef
descript=[ '<H1>plot1d.pro</H1> ', $
'At APS this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This is a general purpose of 1D multi-line plot package.' $
]
mk_html_help,'plot1d.pro','html/plot1dRef.html', $
	descript=descript

END

PRO plot2dRef
descript=[ '<H1>plot2d.pro</H1> ', $
'At APS this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This is a general purpose of 2D plot package.' $
]
mk_html_help,'plot2d.pro','html/plot2dRef.html', $
	descript=descript

END

PRO u_readRef
descript=[ '<H1>u_read.pro</H1> ', $
'At APS this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This is a utility program for unformatted binary I/O. It supports both native and platform-independent XDR binary.' $
]
mk_html_help,'u_read.pro','html/u_readRef.html', $
	descript=descript
END

PRO PS_openRef
descript=[ '<H1>PS_open.pro</H1> ', $
'At APS this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This is a utility program for supporting PostScript plot.' $
]
mk_html_help,'PS_open.pro','html/PS_openRef.html', $
	descript=descript

END

PRO scan2dOverlayRef
descript=[ '<H1>scan2d_overlay.pro</H1> ', $
'At APS this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This is a flexible 2D image displaying widget program.',$
'This program dynamically constructs the 2D composite image from the selected images for a given 2D scan and pops up new composite image window. ', $
'<P>This program is a sub-program of <I>scan2d__define.pro</I> and is dynamically loaded into <I>scan2d__define</I> at the run-time.' $
]
mk_html_help,'scan2d_overlay.pro','html/scan2d_OverlayRef.html', $
	descript=descript

END

PRO cw_termRef
descript=[ '<H1>cw_term.pro</H1> ', $
'At APS this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This is a utility program for generating window display terminal. It also provides a button of printing the window contents.' $
]
mk_html_help,'cw_term.pro','html/cw_termRef.html', $
	descript=descript

END

PRO toImageRef
;
descript=[ '<H1>toImage.pro</H1> ', $
'At APS this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This is a utility program for extracting 1D scans and created 2D images.', $
'This program normally is invoked by <B>toimage</B> script.' $ 
]
mk_html_help,'toImage.pro','html/toImageRef.html', $
	descript=descript
END


PRO ez_fitRef
;
descript=[ '<H1>ez_fit.pro</H1> ', $
'At present this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This is a general purpose curve fitting widget program.', $
'It integrates all the line fitting routines into a single system.', $
'It allows the IDL user easily to access various form of curve fitting.', $
'It is internally available to the data catcher R2.1 and viewer R2.1 or later release.', $
'<P>From the unix operating system to access <B>ez_fit</B> as a stand alone program  by entering :', $
'<PRE>     <B>ezfit</B></PRE>', $
'<P>In order to access <B>ez_fit</B> from other IDL program, a user has to make',$
'sure that the following environment variable is set before invoking IDL :', $
'<PRE><B>    setenv EPICS_EXTENSIONS  /usr/local/epics/extensions </B></PRE>', $
'<P>and make sure including the directory <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B>',$
'in his/her IDL search path.' $
]
mk_html_help,'ez_fit.pro','html/ez_fitRef.html', $
	descript=descript
END

PRO objCleanRef
descript=[ '<H1>obj_clean.pro</H1> ', $
'At present this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This program provides the utility routine OBJ_CLEAN to easily clean up various', $
'objects from the IDL heap.']

mk_html_help,'obj_clean.pro','html/objCleanRef.html', $
	descript=descript
END

PRO scan1d_ObjectRef
;
descript=[ '<H1>scan1d__define.pro</H1> ', $
'At present this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This program is written in IDL 5.0 object programming language .', $
'It allows the IDL user easily to access 1D scan data captured by the data catcher', $
'without bringing up data catcher or viewer.', $
'<P>For proper operation of the <B>scan1d</B> object, a user first has to make',$
'sure that the IDL 5.0 or later is used by his environment settings.', $
'<P>In order to access <B>scan1d</B> object from other IDL program, a user has to make',$
'sure that the following environment variables are set before invoking IDL :', $
'<PRE>     <B>setenv EPICS_EXTENSIONS  /usr/local/epics/extensions </B></PRE>', $
'<PRE><B>     setenv IDL_STARTUP $EPICS_EXTENSIONS/bin/$HOST_ARCH/viewer_startup.pro </B></PRE>', $
'and make sure including the directory <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B>',$
'in his/her IDL search path.', $
'<P>From the unix operating system to access IDL <I><B>scan1d</B></I> object class by entering :', $
'<PRE>     <B>idl  go_scan1d</B></PRE>', $
'<P>The files <B>u_read.pro</B>, <B>plot1d.pro</B>, and <B>scan1d__define.pro</B> are automatically loaded into IDL 5.0 for user.', $
"</B><P>For an example to create a <I><B>'scan1d'</B></I> object with variable name as <B>v1d</B>, ", $
"where the 1D catcher data is saved in the file <B><I>'junk2'</I></B>:", $

"<PRE><B>     v1d = obj_new('scan1d',file=<I>'junk2'</I>)</B></PRE>", $
"<P>To get the positioner array PA and detector array DA for scan number 10, plus plotting and listing windows:", $
'<PRE><B>     v1d->read,10,DA=da,PA=pa,/plot,/list </B></PRE>' $
]
mk_html_help,'scan1d__define.pro','html/scan1d_ObjectRef.html', $
	descript=descript
END

PRO scan2d_ObjectRef
;
descript=[ '<H1>scan2d__define.pro</H1> ', $
'At present this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This program is written in IDL 5.0 object programming language .', $
'It allows the IDL user easily to access 2D scan data captured by the data catcher.', $
'<P>In order to access <B>scan2d</B> object from other IDL program, a user has to make',$
'sure that the following environment variable is set before invoking IDL :', $
'<PRE>     <B>setenv EPICS_EXTENSIONS  /usr/local/epics/extensions </B></PRE>', $
'<PRE><B>     setenv IDL_STARTUP /usr/local/epics/extensions/bin/$HOST_ARCH/viewer_startup.pro </B></PRE>', $
'<P>and make sure including the directory <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B>',$
'in his/her IDL search path.', $
'<P>From the unix operating system to access IDL <I><B>scan2d</B></I> object class by entering :', $
'<PRE>     <B>idl  go_scan2d</B></PRE>', $
'<P>The files <B>u_read.pro</B>, and <B>scan2d__define.pro</B> are automatically loaded into IDL 5.0 for user.', $
"</B><P>For an example to create a <I><B>'scan2d'</B></I> object with variable name as <B>v2</B>, ", $
"where the 2D catcher data is saved in the file <B><I>'junk2.image'</I></B>:", $

"<PRE><B>     v2 = obj_new('scan2d',file=<I>'junk2.image'</I>)</B></PRE>", $

"<P>To view the image number 135 from the file:", $
'<PRE><B>     v2->view,135 </B></PRE>' $
]
mk_html_help,'scan2d__define.pro','html/scan2d_ObjectRef.html', $
	descript=descript
END

PRO NX_ObjectRef
;
descript=[ '<H1>NX__define.pro</H1> ', $
'At present this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This program is written in IDL 5.1 object programming language .', $
'It provides the IDL user conveninent methods of accessing NX HDF data set.', $
'<P>In order to access <B>NX</B> object from other IDL program, a user has to make',$
'sure that the following environment variable is set before invoking IDL :', $
'<PRE>     <B>setenv EPICS_EXTENSIONS  /usr/local/epics/extensions </B></PRE>', $
'<PRE><B>     setenv IDL_STARTUP /usr/local/epics/extensions/bin/$HOST_ARCH/viewer_startup.pro </B></PRE>', $
'<P>and make sure including the directory <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B>',$
'in his/her IDL search path.', $
'<P>During an IDL session to access <I><B>NX</B></I> object class by entering :', $
'<PRE>     <B>.run NX__define</B></PRE>', $
"</B><P>For an example to create a <I><B>'NX'</B></I> object with variable name as <B>v</B>, ", $
"where the NX HDF data is saved in the file <B><I>'1.hdf'</I></B>:", $

"<PRE><B>     v = obj_new('NX',file=<I>'1.hdf'</I>)</B></PRE>", $

"<P>To view the HDF file summary:", $
'<PRE><B>     v->print </B></PRE>' $
]
mk_html_help,'NX__define.pro','html/NX_ObjectRef.html', $
	descript=descript
END


PRO readScanRef
;
descript=[ '<H1>readScan.pro</H1> ', $
'At present this program is installed in the <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B> directory.', $
'This program is written in IDL 5.1 programming language .', $
'It provides the IDL user conveninent methods of accessing scan data set ',$
'automatically saved by the IOC savedata software.',$
'<P>In order to access <B>scanSee</B> data from other IDL program, a user has to make',$
'sure that the following environment variable is set before invoking IDL :', $
'<PRE>     <B>setenv EPICS_EXTENSIONS  /usr/local/epics/extensions </B></PRE>', $
'<PRE><B>     setenv IDL_STARTUP /usr/local/epics/extensions/bin/$HOST_ARCH/viewer_startup.pro </B></PRE>', $
'<P>and make sure including the directory <B>/usr/local/epics/extensions/bin/$HOST_ARCH</B>',$
'in his/her IDL search path.', $
'<P>During an IDL session to access <I><B>scanSee</B></I> data by entering :', $
'<PRE>     <B>.run readScan</B></PRE>', $
"</B><P>For an example to access the scan data with the scan data structure pointer <B>gD</B>, ", $
"where the scan data is saved in the file <B><I>'rix:._0003.scan'</I></B>:", $

"<PRE><B>     readScanFile,<I>'rix:._0003.scan'</I>,gD</B></PRE>", $

"<P>To view the scan data summary:", $
'<PRE><B>     scanImage_print,gD </B></PRE>' $
]
mk_html_help,'readScan.pro','html/readScan.html', $
	descript=descript
END


PRO makeIDLRef
; This program builds the catcher.html from the source files
; files = ['catcher_v1.pro','view2d.pro','plot1d.pro','view1d.pro','dcviewer.pro']
; mk_html_help, files, 'html/catcher.html'

; catcherRef
	catcherRef

; v1dOvlRef
	v1dOvlRef

; dcviewerRef
	dcviewerRef


; view1dRef
	view1dRef

; view2dRef
	view2dRef

; plot1dRef
	plot1dRef

; plot2dRef
	plot2dRef

; u_readRef
	u_readRef

; PS_openRef
	PS_openRef

; CW_TermRef
	CW_TermRef

;  toImageRef

	toImageRef

; ez_fitRef
	ez_fitRef

; scan1d_ObjectRef
	scan1d_ObjectRef

; scan2d_ObjectRef
	scan2d_ObjectRef

; objCleanRef
	objCleanRef

; NX_ObjectRef
	NX_ObjectRef

; readScanRef
	readScanRef

END
