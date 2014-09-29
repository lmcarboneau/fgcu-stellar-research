
print,' %%% Use another program for this. ;
print,' %%% It is better to keep the times as JD and then change'
print.' %%% them at the end of the reduction!'
stop

; October 2004

PRO wire_corr_hjd, files, target, addt=addt

; ============================================================
nf = n_elements(files)
; ============================================================

; ============================================================
if n_elements(addt) eq 0 then addt = 0.5 ; comp. w/ Uytterhoeven (Lam Sco)
; ============================================================

; ============================================================
; Get the RA/DEC for this object (needed for HJD calculation)
; ============================================================
wire_red_setup, position_file, target_file
wire_target_info, target_file, info
ninfo= n_elements(info)
winfo = where(strmatch(info.object,target) eq 1,cinfo)
if cinfo ne 1 then begin
 print,' %%% Target not found in wire_corr_hjd.pro: ' + target
 stop
endif

ra = info(winfo).ra2 & dec = info(winfo).dec2
; ============================================================

; ============================================================
; For each input file, restore the wire structure:
; ============================================================
for f=0,nf-1 do begin
; ============================================================

; ============================================================
restore,files(f)
n = n_elements(wire)
wire.hjd = -1.0 ; reset all times
; ============================================================


for s=0,4 do begin ; each slot
for p=0L,n-1 do begin

  dateall = wire(p).stamp(s)

  g = strsplit(dateall,'-',/extract)
  if n_elements(g) le 2 or $
     strmatch(dateall,'*:*') ne 1 or $
     strmatch(dateall,'*A*') eq 1 or $
     strmatch(dateall,'*c*') eq 1 then goto,skip_pt

 ; Repaired .gz files may have currupt time information:
  kolon = strsplit(dateall,':',/extract)
  if n_elements(kolon) ge 4 then goto,skip_pt


; "?" in front of year ?
  if strlen(strcompress(g(0),/remove_all)) ge 3 then goto,skip_pt 

  year = float(g(0))
  day  = float(g(1))
  time = g(2)
  g2 = strsplit(time,':',/extract)
  hour = float(g2(0)) 
  min  = double(g2(1)) + ( double(g2(2)) / 60. )

   year = year + 2000D
   multbad = 1.0
   if (year ge 2009) or (year le 2002) then begin
     print,' % Something is wrong with the times ... year is: ',year
     multbad = -1.
   endif

   juldate, [year, 1, 1, hour, min], jd_temp ; Julian Date
   jd_temp = jd_temp + day 

   hjd_temp = helio_jd( jd_temp, ra, dec)   ; Heliocentric Julian Date

   wire(p).hjd(s) = hjd_temp * multbad ; + addt ; why add 0.5 days again? it's already done in wire_all !!

skip_pt:

  endfor ; next data point
 endfor ; next slot in wire structure

outfile = files(f)
save,filename = outfile, wire
print,' %%% Saved wire structure: ' + outfile

endfor ; next file


END
