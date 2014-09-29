; Reorganize Wire files on mons machine at USAFA

nmax = 200000L
b = strarr(nmax)
cnt = 0L

for i=2,4 do begin
 for j=0,9 do begin
  print,i,j,'x',format='(I2, I2, A1, $)'
  for k=0,9 do begin
   for l=0,9 do begin

    spawnrob,'ls -1 /data1/bruntt/wire/dl/tape1/5star.'+$
     string(i,format='(I1)')+ $
     string(j,format='(I1)') + $
     string(k,format='(I1)') + $
     string(l,format='(I1)')+ '*/*',a
    na = n_elements(a)
    
    ; print,i,j,k,l,na

    if a(0) ne '' then begin
     b(cnt:cnt+na-1) = a
     cnt = cnt + na
    endif

  endfor
 endfor
endfor
endfor

b = b(0:cnt-1)
help,b

end
