#*************************************************************************
# Copyright (c) 2002 The University of Chicago, as Operator of Argonne
# National Laboratory.
# Copyright (c) 2002 The Regents of the University of California, as
# Operator of Los Alamos National Laboratory.
# This file is distributed subject to a Software License Agreement found
# in the file LICENSE that is included with this distribution. 
#*************************************************************************
#
# $Id: Makefile,v 1.8 2014/02/14 15:50:28 jba Exp $
#
TOP = ../..
include $(TOP)/configure/CONFIG

DOCS = catcher.README catcher.html  catcher_help.txt view2d_help.txt \
	DC_help.txt \
	scanSee.README vw2d_help.txt pick3d_help.txt

IDLS = ezcaidl_startup.pro my_box_cursor.pro \
	os.init PS_open.pro u_read.pro cw_term.pro plot1d.pro plot2d.pro \
	scan_colorbar.pro rename_dialog.pro scan2d_roi.pro xdisplayfile.pro \
	fit_statistic.pro calibration_factor.pro calibra_pick1d.pro \
	calibdrv.pro iplot1d.pro overlay_1d.pro \
	h5b.pro plot2d_image.pro saveImage.pro \
 	dc2ins_2d dc2ins_1d wd_readascii.pro \
	go_scan1d  go_scan2d \
	dc2tiff.pro scan2d_convert.pro \
	ez_fit.pro catch1d.tbl fixIndexFile.pro \
	catcher_v1.pro catcher_v1.init toImage.pro \
	scan1d__define.pro obj_clean.pro \
	scan2d__define.pro scan2d_overlay.pro \
	dcviewer.pro viewer_startup.pro \
	view1d.pro view1d.init view1d_overlay.pro \
	view2d.pro view2d.init cursor62_caput.pro makeIDLRef.pro \
	dc2hdf.pro img.pro image2d.pro \
	hdfb.pro NX__define.pro nexus_scan.pro \
	DC.init DC.pro vw2d.bm view3d_slicer.pro \
	sscan.pro sscan__define.pro user_scan.pro wc.pro \
	sb2rpt.pro view4d.pro \
	multiroi_pick.pro xdr_open.pro \
	pick3d.pro view3d_2dsum.pro pick2d.pro \
 	view3d_2d.pro panimage.pro slide_image.pro \
	mda2vol.pro xrf_peak_library.txt \
	memtest.pro scansee_overlay.pro assignname_read.pro scansee_browser.pro
#	volviewer.pro volviewer_eventcb.pro volume_animator.pro \

SCRIPTS_DEFAULT = ezcaidl_setup idluserdir_setup \
	calibra scanBrowser \
	idlvm \
	dc2tiff \
	ezfit \
	catcher toimage \
	viewer mda2vol \
	dc2hdf img \
	hdfb NX h5b \
	scanSee sscan \
	SB \
	pick3d 
#	volviewer \

#SCRIPTS_WIND32 = -nil-
SCRIPTS_WIN32 = $(IDLS) \
		go_DC go_NX go_SB go_img go_pick3d \
		go_scanBrowser go_vw2d \
		ezcaidl_startup.win viewer_startup.win

include $(TOP)/configure/RULES

