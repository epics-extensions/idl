;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
;device,pseudo=8
;device,decomposed=0
device,retain=2

product = getenv('EPICS_EXTENSIONS')

arch = getenv('EPICS_HOST_ARCH')
if arch eq '' then arch = getenv('HOST_ARCH')
if arch ne '' then $
!path=  product +'/bin/'+arch+':'+!path
!path= product + '/idllib:'+!path

private = getenv('EPICS_EXTENSIONS_PVT')
print,'private ',private
if private ne '' then $
!path= private + '/idllib:' +private+'/bin/'+arch+':'+ !path

private = getenv('EPICS_EXT_PVT')
if private ne '' then $
!path= private + '/idllib:' + !path

print,!path
;
!QUIET=1

@os.init

print,'Run ezcaIDL ...'
.run ezcaIDL
print,'Run ezcaIDLWidgets ...'
.run ezcaIDLWidgets
caInit
caSetTimeout,0.001
caPendIO,time=0.01,list_time=3.
caPendEvent,time=0.000001
print,caVersion()

; add periodic ca_pend_event event
add_caPendEvent,timer=5.
print,' add_caPendEvent,timer=.1'
