arch = getenv('HOST_ARCH')
private = getenv('EPICS_EXTENSIONS_PVT')
print,'arch ',arch
print,'private ',private

!path= '/usr/local/epics/extensions/bin/'+arch+':'+!path
if private ne '' then $
!path= private + '/bin/'+arch + ':' + !path

private = getenv('EPICS_EXT_PVT')
if private ne '' then $
!path= private + '/bin/'+arch + '/idl:' + !path

print,!path
;
!QUIET=1

@os.init

;  assign color
window,colors=32
wdelete

device,retain=2

print,'Run ezcaIDL ...'
.run ezcaIDL
print,'Run ezcaIDLWidgets ...'
.run ezcaIDLWidgets
caInit
caSetTimeout,0.001
print,caVersion()
