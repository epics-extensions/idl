PRO WC,filename,nline,ncol
; simulate unix command WC to read an ASCII file
; nline - return the total # of lines in file
; ncol  - returns the totol # of data elements in a line
        openr,1,filename
        line=''
        nline=0
        while NOT EOF(1) do begin
        on_ioerror,close1
        readf,1,line
        nline=nline+1
        if nline eq 1 then begin
                y = strsplit(line,' ',/extract)
                ncol = n_elements(y)
        end
        end
close1:
        close,1
END

