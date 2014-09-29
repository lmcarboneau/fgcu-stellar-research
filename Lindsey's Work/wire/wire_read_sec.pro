; Read info on secondary stars: HD number, Parallax, vsini + Stromgren
; photometry

infile = '~bruntt/wire/wire_process/wire_secondary2.txt'

get_lun,u
openr,u,infile

datsec = replicate( {simbadname:'', wirename:'', slot:0B, hd:0L, pax:-1., e_pax:-1., $
                     vsini:0., e_vsini:0., hbeta:0., uvby:fltarr(4), comment:'', $
                     b:0., v:0., bv:0., ra:0., dec:0., extra: fltarr(20), $
                     obscount:0., calccount:0., spec:''}, 500)

datsec.bv = -99
datsec.b = -29.
datsec.v = -59.

cnt = 0L

while not eof(u) do begin

simbadname='' & wirename='' & slot=0B & hd=0L & pax=-1. & e_pax=-1.
vsini=0. & e_vsini=0. & hbeta=0. & uvby=fltarr(4) & comment=''

readf,u,$
simbadname, wirename, slot, hd, pax,  e_pax, $
 vsini, e_vsini, hbeta, uvby, comment, $
 format='(A15,A15,I3,F7, F9.2, F5.2, I4, I3, F7.3, F8.3, F8.3, F6.3,I3, A20)'

simbad2 = strcompress(simbadname)
if strmid(simbad2,0,1) eq ' ' then simbad2 = strmid(simbad2,1,200)
g = strlen(simbad2)
if strmid(simbad2,g-1,1) eq ' ' then simbad2 = strmid(simbad2,0,g-1)

datsec(cnt).simbadname = simbad2
datsec(cnt).wirename = wirename
datsec(cnt).slot = slot
datsec(cnt).hd = hd
datsec(cnt).pax = pax
datsec(cnt).e_pax = e_pax
datsec(cnt).vsini = vsini
datsec(cnt).e_vsini = e_vsini
datsec(cnt).hbeta = hbeta
datsec(cnt).uvby = uvby
datsec(cnt).comment = comment

print,simbadname, wirename, slot, hd, pax,  e_pax, $
 vsini, e_vsini, hbeta, uvby, comment, $
 format='(A15,A15,I3, F10.0, F9.2, F5.2, I4, I3, F7.3, F8.3, F8.3, F6.3,I3,X, A20)'

cnt = cnt + 1

endwhile

close,u
free_lun,u

datsec = datsec(0:cnt-1)



; NOW GET INPUT ... RA/DEC + B,V 
wire_red_setup, position_file, target_file
restore,position_file

ninfo = n_elements(xyinfo)

for i=0,cnt-1 do begin
 oo = datsec(i).simbadname

 print,oo

 for j=0,ninfo-1 do begin


 w = where(strmatch(xyinfo(j).object,'*' + oo + '*') eq 1,c)
 if c eq 1 then begin ; object found in info structure

  datsec(i).v         = xyinfo(j).extra(0,w)
  datsec(i).bv        = xyinfo(j).extra(1,w)
  datsec(i).b         = datsec(i).bv + datsec(i).v  
  datsec(i).calccount = xyinfo(j).extra(2,w)
  datsec(i).obscount  = xyinfo(j).extra(3,w)
  datsec(i).ra        = xyinfo(j).extra(4,w)
  datsec(i).dec       = xyinfo(j).extra(5,w)

  spec = strsplit(xyinfo(j).object(w), '&', /extract)
  datsec(i).spec = strcompress(spec(1),/remove_all)

; FROM wire_auto_field.pro:
;      xyinfo(wtarg_pos).extra(0,starnumber(k)) = vv(wmat(xx)) ; V mag, simbad
;      xyinfo(wtarg_pos).extra(1,starnumber(k)) = bv(wmat(xx)) ; B mag, simbad
;      xyinfo(wtarg_pos).extra(2,starnumber(k)) = cr(wmat(xx)) ; c.r. from B-V
;      xyinfo(wtarg_pos).extra(3,starnumber(k)) = ct0(k)
;      xyinfo(wtarg_pos).extra(4,starnumber(k)) =  ra2(wmat(xx))
;      xyinfo(wtarg_pos).extra(5,starnumber(k)) = dec2(wmat(xx))

 endif

endfor
endfor

 plot,datsec.bv,datsec.v,psym=2,xtit='B-V',ytit='V',$
  yr=[10,0],xr=[-2,5],xsty=1,ysty=1

 outfile = '~bruntt/wire/wire_process/wire_secondary_info.idl' 
 save,filename=outfile, datsec, /compress
 print,' %%% Saved file as: ' + outfile


END
