pro memtest
  i=2047L
  catch,error
  if (error ne 0) then begin
    i=i-1
  endif
  a=ptr_new(bytarr(i*2L^20L,/nozero),/no_copy)
  print, 'Maximum size array that IDL can allocate: ',i,' Mb'
  ptr_free, a
end
