PRO wire_all, dirr, target=target, prefix=prefix, startfile=startfile,cnt2=cnt2

; Read wire data from a certain directory -- H. Bruntt 2nd of May 2003
; This is the first prg. you run ... then wire_pos
; Example:
; New format: wire_all,'/data2/bruntt/wire/newdat/',target='alphaCen'
; wire_all,'/ai43/bruntt/wire/altair/'
; wire_all,'/data2/bruntt/wire/dat/alphaCen/data/'
; wire_all,'/data1/bruntt/wire/oct04/polaris/',target='AlphaUMi'

; =============================================================
; Optional input parameters:
; These may only be set if wire_all.pro crashes:
; =============================================================
; startfile: set to the last "# files" on your screen, ie
;            just before the last "wire file saved" 
; cnt2: also, set this to the last value in the suffix 
;       following "wire_files_saved", e.g: set keywords: startfile=119, cnt2=46
; # files:  119/ 135
; In:  /mnt/sdb6/wire/rawdata/nov2005/rfd/BetaHyi//rfd39pb2005-271-1644.tlm
; Out: /mnt/sdb6/wire/rawdata/nov2005/rfd/BetaHyi//rfd39pb2005-271-1644.idl
; *** WARNING: wire array is too small! I will save... and go to next file!
; *** wire file saved: /mnt/sdb6/wire/rawdata/nov2005/rfd/BetaHyi//BetaHyi_wire_BetaHyi_046.idl
; =============================================================



default9, prefix, ''
default9, startfile, 0L

; =============================================================
; Extract data if packed!
; =============================================================
if n_elements(target) eq 0 then target = ''

if prefix eq '' then begin
 if target eq '' then begin ; old wire format
  spawnrob,'gzip -df '+dirr+'/' + '*.data.gz',aa
  spawnrob,'ls -1 '+dirr+'/' + '*.data',aa
 endif else begin
  spawnrob,'ls -1 '+dirr+'/rfd39*',aa
 ; spawnrob,'ls -1 '+dirr+'/wire_200?_d???_t??_*',aa
  w = where(strmatch(aa,'*.gz') eq 1,c)

  for i=0,c-1 do begin
   spawnrob,'gzip -d ' + aa(w(i)), com
  endfor

    spawnrob,'ls -1 '+dirr+'/rfd39*.tlm',aa
  ; spawnrob,'ls -1 '+dirr+'/wire_200?_d???_t??_*',aa

  if startfile ne 0 then aa = aa(startfile:*) ; if startfile was set

  endelse
endif else begin
  spawnrob,'ls -1 '+dirr+'/' + prefix + '*',aa
  print,' %%% I will read these data: '
  naa = n_elements(aa)
  if naa eq 0 then begin
    print,' *** No data with that prefix in that dir: ' + dirr + ' --- ' + prefix
    stop
  endif
  for j=0,naa-1 do print,aa(j)
  hitme,spectrum_forfan
endelse

print,' %%% Files that will be imported: '
print,aa

; =============================================================

; =============================================================
; Any .gz files that could not be unpacked?
; =============================================================
spawnrob,'ls -1 ' +dirr+'/*.gz', bad
nbad = n_elements(bad)
if nbad ge 1 and bad(0) ne '' then begin
 print,''
 print,' *** There was ' + strcompress(nbad,/remove_all) + $
  ' corrupt .gz files: '
 for jk=0,nbad-1 do print,bad(jk)

 if nbad le 2 then begin
   print,' %%% Since there are less than 3 bad files I will continue!'
   print,''
 endif else begin
   print,''
   print,' *** Hit x to stop ... '
    s9 = get_kbrd(1)
    if s9 eq 'x' then stop
 endelse
 
endif
; =============================================================

; =============================================================
na = n_elements(aa)  
if na le 0 or aa(0) eq '' then begin
 print,' *** Could not find any wire files!'
 help,bad
 stop
endif
; =============================================================

; =============================================================
nslot = 5 ; assume that five stars are read out
; =============================================================

; if strmatch(dirr,'*beta_UMi*') eq 1 then nslot=1 ; NO!!!

; =============================================================
; You may change con / cnt2 if the program crashes ... (i.e. a currupt input file)
con  =  0L ; loop counter
default9,cnt2,  0L ; wire file counter

npt_big = 35000. * 3. * 1.1 ; < memory problem ... must be < 1e6 !
; npt_big: There is typically 35000 data points pr. star in one file.
; I multiply by three (== three files) and add 10% (*1.1).

; =============================================================

; =============================================================
; Read the rest of the data?
; =============================================================
 read_again:
; ----------

; =============================================================
; Set up the big wire file!
; =============================================================

wire = replicate({x:intarr(nslot),y:intarr(nslot),hjd:dblarr(nslot),$
                  stamp:strarr(nslot), $
                  d:lonarr(nslot,8,8)},npt_big)
cnt = lonarr(nslot)

; =============================================================
for i=con,na-1 do begin ; for each .data file:
; =============================================================

; =============================================================
; Read next file ; read each file into the temp structure
; There are two programs for reading the raw data:
; wire_read.pro: old format, data taken before Late-2003
; wire_read_newformat.pro: Data taken from Late-2003 and onward ...
; If the variable "target" is set to '' wire_read.pro is used!
; =============================================================

; =============================================================
if target eq '' then wire_read,aa(i),0,temp,npt else $
 wire_read_newformat,aa(i),target,0,temp,npt,addt=0.5
; addt: October 2004, set to +0.5
; In Autumn 2005 I found the correct time correction: addt should be
; -1.0 instead, but in order for ALL wire data to have the same
; error, I did not change this. 
; Thus: all WIRE data have erroneous times: "subtract 1.5 days"
; =============================================================

; plot,temp.hjd(0),temp.d(0,3,3),psym=3,ysty=3

if ( max(cnt) + max(npt) ) ge (npt_big*0.7) then begin
 if total(cnt) eq 0 then stop
 print,'*** WARNING: wire array is too small! I will save... and go to next file!'
 break ; exit loop right now!
endif 

; =============================================================
for jj=0,nslot-1 do begin
; =============================================================
if npt(jj) ge 1 then begin
    ; Store the data from file
     wire(cnt(jj):cnt(jj)+npt(jj)-1).x(jj) = temp(0:npt(jj)-1).x(jj)
     wire(cnt(jj):cnt(jj)+npt(jj)-1).y(jj) = temp(0:npt(jj)-1).y(jj)
     wire(cnt(jj):cnt(jj)+npt(jj)-1).stamp(jj) = temp(0:npt(jj)-1).stamp(jj) ;unique timestamp
     wire(cnt(jj):cnt(jj)+npt(jj)-1).hjd(jj) = temp(0:npt(jj)-1).hjd(jj)
     wire(cnt(jj):cnt(jj)+npt(jj)-1).d(jj,*,*) = temp(0:npt(jj)-1).d(jj,*,*)
    ; increase wire array counter
     cnt(jj) = cnt(jj) + npt(jj)
 endif ;else begin
     ; print,' %%% No data for star: '+string(jj,format='(I2)')+' -- file: '+aa(i)
 ;endelse
endfor ; next slot!
; =============================================================

    ; Loop info
     nm = strsplit(aa(i),'/',/extract) & nn = n_elements(nm)
     print,nm(nn-1),': '+strcompress(string(cnt))
     print,' # files: '+strcompress(string(i+1))+'/'+strcompress(string(na))    

; if cnt(1) gt 100 
; plot,wire(0:2000).hjd(0)-median(wire(0:2000).hjd(0)),wire(0:2000).d(0,3,3),psym=3,xr=[-.1,.1]
; plot,wire(0:2000).hjd(1)-median(wire(0:2000).hjd(1)),wire(0:2000).d(1,3,3),psym=3,xr=[-.1,.1]
; plot,wire(0:2000).hjd(4)-median(wire(0:2000).hjd(4)),wire(0:2000).d(4,3,3),psym=3,xr=[-.1,.1]

; increase file name counter
 con = con + 1

endfor ; next data file
; =============================================================

; =============================================================
cnt_max = max(cnt)
if cnt_max le 0 then goto,fail_data

wire = wire(0:cnt_max-1) ; remove unused data space

ss = sort(wire.hjd(0)) ; sort by increasing time!
wire = wire(ss)        ; reshuffle by increasing time!
; =============================================================

; =============================================================
; Save data file ...
; =============================================================
cnt2 = cnt2 + 1
nam1 = strsplit(dirr,'/',/extract) & n_nam1 = n_elements(nam1)
nam2 = nam1(n_nam1-1)
 if cnt2 le   9 then                suffix = '00'+strcompress(string(cnt2),/remove_all)
 if cnt2 ge  10 and cnt2 le 99 then suffix = '0' +strcompress(string(cnt2),/remove_all)
 if cnt2 gt  99 then                suffix = ''  +strcompress(string(cnt2),/remove_all)

outname = dirr+'/'+nam2+'_wire_'+target + '_' + suffix+'.idl'
save, filename = outname,wire,/compress ; save final wire file
print,'*** wire file saved: '+outname
; =============================================================

; =============================================================
fail_data:
; =============================================================

if con lt na then goto,read_again ; still more data to be read
; =============================================================



spawnrob,"ls -1 " + dirr + nam2 + "_wire_" + target + "_*.idl",ff
nf = n_elements(ff)
nf2 = round(nf * 0.5)
add2 = ceil(nf * 0.12)
if nf le 5 then add2 = 0

; =============================================================
print,''
print,' ===================================================='
print,' %%% Disentangle data (two primary objects):
print,' ===================================================='
print,'     spawnrob,"ls -1 ' + dirr + nam2 + '_wire_' + target + '_???.idl",ff'
print,'     wire_newformat_sep2, ff
print,''
print,' ===================================================='
print,' %%% Then launch wire_pos.pro for each object:'
print,' ===================================================='
print,'   spawnrob,"ls -1 ' + dirr + nam2 + '_wire_' + target + '_*obj1.idlr",ff'
print,'   nf = '+strcompress(nf) + ' & nf2 = '+strcompress(nf2)+$
          ' & add2 = '+strcompress(add2)
print,'   choose_ref = [ff(add2),ff(nf2),ff(nf-1-add2)]'
print,'   wire_pos,choose_ref(1),1,"","'+target+'"'
print,''
print,'   spawnrob,"ls -1 ' + dirr + nam2 + '_wire_' + target + '_*obj2.idlr",ff2'
print,'   nf = '+strcompress(nf) + ' & nf2 = '+strcompress(nf2)+$
          ' & add2 = '+strcompress(add2)
print,'   choose_ref2 = [ff2(add2),ff2(nf2),ff2(nf-1-add2)]'
print,'   wire_pos,choose_ref2(1),1,"","'+target+'"'
print,''


; =============================================================

; =============================================================
; Compress data !!
; =============================================================
spawnrob,'nice +14 gzip -f '+dirr+'/*.tlm &',aa
spawnrob,'nice +14 gzip -f '+dirr+'/wire_2*.dat &',aa
; =============================================================


end
