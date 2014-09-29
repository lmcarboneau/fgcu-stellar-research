; Compare WIRE HD numbers for B-type stars with lists
; from Peter de Cat's list at http://www.ster.kuleuven.ac.be/staff/peter/Bstars/

close,1

m4_get_basedir, base

if n_elements(wire) eq 0 then $
 restore,base + 'wire_process/reduced_B2.idl'

temp=''
cnt = 0

nmax = 300
peter = replicate({hd:0L, spec1:'', mass:0., emass:0., logg:0., elogg:0., teff:0., eteff:0.,$
 vsini:-9., list:'', pdc:0B}, nmax)
peter.pdc = -1

dirr = base + 'papers/wire/bstars/'
suffix = '.txt'
files = ['SPB_confirmed','SPB_candidate','bcep_field','bcep_susphot','bcep_susspec']
nf = n_elements(files)

;; $head SPB_confirmed.txt SPB_candidate.txt bcep_field.txt bcep_susphot.txt bcep_susspec.txt               

for k=0,nf-1 do begin

filename = dirr + files(k) + suffix
print,' %%% Reading HD numbers from file: ' + filename

openr,1,filename
readf,1,temp ; read comment
readf,1,temp

while not eof(1) do begin
 readf,1,temp
 a = strsplit(temp,' ',/extract)
 hdtemp = a(0)
 x = strsplit(hdtemp,'D',/extract)
 hdtemp = x(1)
 peter(cnt).hd = long(hdtemp)
 peter(cnt).list = files(k)
 peter(cnt).pdc = k
 cnt = cnt + 1
endwhile

close,1
endfor

peter = peter(0:cnt-1)
close,1

help,peter 

; Cross check with WIRE targets:
nw = n_elements(wire)
pdc = fltarr(nw) & pdc(*) = -1

for i=0,nw-1 do begin
 w = where(peter.hd eq wire(i).hd,c)
 if c ge 1 then begin
   print,' %%% WIRE star found in Peter de Cat list: ',wire(i).hd, ' ---> ', peter(w).list
   pdc(i) = peter(w).pdc ; pdc entry
 endif else begin
   print,' %%% WIRE star not found in Peter de Cat list: ',wire(i).hd 
 endelse
endfor

outfile = '~/papers/wire/bstars/pdc.idl'
save,filename=outfile, pdc

END
