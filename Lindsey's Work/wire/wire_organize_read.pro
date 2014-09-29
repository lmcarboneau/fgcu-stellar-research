; Reorganize Wire files on mons machine at USAFA

nmax = 200000L

; c = strarr(nmax)
; c(0:cnt-1) = b
; b = c

 b = strarr(nmax)
 cnt = 0L

goto,here



pp = strarr(4)
pp(0) = 'log'
pp(1) = 'data'
pp(2) = 'ps'
pp(3) = 'time_series'
npp = n_elements(pp)

for i=1,1 do begin
 for j=0,9 do begin
  for k=0,9 do begin
   for l=0,9 do begin
    for p=0,npp-1 do begin
      cont = pp(p)

    spawnrob,'ls -1 /data2/bruntt/dl/tape2/'+$
     string(i,format='(I1)')+ $
     string(j,format='(I1)') + $
     string(k,format='(I1)') + $
     string(l,format='(I1)')+ '*/*'+cont+'*',a 
    
    na = n_elements(a)
    
    if a(0) ne '' then begin
     b(cnt:cnt+na-1) = a
     cnt = cnt + na
    endif

    endfor
   endfor
  endfor
 endfor
endfor





for i=0,5,5 do begin
 for j=0,9 do begin
  print,i,j,'x',format='(I2, I2, A1, $)'
  for k=0,9 do begin
   for l=0,9 do begin
    for p=0,npp-1 do begin
      cont = pp(p)

  if i ne 0 then $
    spawnrob,'ls -1 /data2/bruntt/dl/tape2/5star.'+$
     string(i,format='(I1)')+ $
     string(j,format='(I1)') + $
     string(k,format='(I1)') + $
     string(l,format='(I1)')+ '*/*'+cont+'*',a else $
    spawnrob,'ls -1 /data2/bruntt/dl/tape2/5star.'+$
     string(j,format='(I1)') + $
     string(k,format='(I1)') + $
     string(l,format='(I1)')+ '*/*'+cont+'*',a
    


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
endfor




for i=1,1 do begin
 for j=0,9 do begin
  for k=0,9 do begin
   for l=0,9 do begin
    for p=0,npp-1 do begin
      cont = pp(p)

    spawnrob,'ls -1 /data2/bruntt/dl/tape2/testB.'+$
     string(i,format='(I1)')+ $
     string(j,format='(I1)') + $
     string(k,format='(I1)') + $
     string(l,format='(I1)')+ '*/*'+cont+'*',a
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
endfor


for i=1,2 do begin
 for j=0,9 do begin
  for k=0,9 do begin
   for l=0,9 do begin
    for p=0,npp-1 do begin
      cont = pp(p)

    spawnrob,'ls -1 /data2/bruntt/dl/tape2/testC.'+$
     string(i,format='(I1)')+ $
     string(j,format='(I1)') + $
     string(k,format='(I1)') + $
     string(l,format='(I1)')+ '*/*'+cont+'*',a
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
endfor

here:

; =====================
;       TAPE 1
; =====================

for i=2,4 do begin
 for j=0,9 do begin
  for k=0,9 do begin
   for l=0,9 do begin
    for p=0,npp-1 do begin
      cont = pp(p)

    spawnrob,'ls -1 /data1/bruntt/wire/dl/tape1/5star.'+$
     string(i,format='(I1)')+ $
     string(j,format='(I1)') + $
     string(k,format='(I1)') + $
     string(l,format='(I1)')+ '*/*'+cont+'*',a
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
endfor



b = b(0:cnt-1)

; sort by filenames:
gg = sort(b)
b = b(gg)

help,b

save,filename='~/wire_files.idl',b,cnt

end
