
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
device,bypass_translation=0         ;  required for 24 bit device
