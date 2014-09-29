; Correct times in sub file

infile = '~/Documents/various/oz2fin.txt'
addtime1 = 5.0 ; minutes
addtime2 = 0.0 ; seconds

infile = '~/Documents/various/oz2c.txt'
addtime1 = 10.0 ; minutes
addtime2 =  6.0 ; seconds


openr,1,infile

sub = replicate( {id:0, timetag:'', txt:'', $
                  time1:0., time2:0., time3:0., time4:0., time5:0., time6:0.}, 80)
cnt = 0
temp = ''

while not eof(1) do begin

readf,1,temp
x = strcompress(temp,/remove_all)
l = strlen(x)

if l eq 0 then begin
 cnt = cnt + 1 ; new line
endif

if l ge 1 and l le 2 then begin
 sub(cnt).id = long(x)
 goto,newline
endif

if strmatch(x,'*:*') eq 1 and strmatch(x,'*00*') eq 1 then sub(cnt).timetag = x else $
 sub(cnt).txt = sub(cnt).txt + temp + ' '

newline:
endwhile
close,1


; Remove unused entries
sub = sub(0:cnt-1)
sub.id = indgen(cnt) + 1

print,' %%% Imported ' + string(cnt+1) + ' subtitles'

for i=0,cnt-1 do begin
 time = sub(i).timetag
 x = strsplit(time,':',/extract)
 sub(i).time1 = x(1)
 y = strsplit(x(2),'-',/extract)
 z = strsplit(y(0),',',/extract)
 sub(i).time2 = z(0)
 sub(i).time3 = float(z(1)) 

 sub(i).time4 = x(3)
 z = strsplit(x(4),',',/extract)
 sub(i).time5 = z(0)
 sub(i).time6 = float(z(1)) 

endfor

n = 10 & print,sub(n).timetag, sub(n).time1, sub(n).time2, sub(n).time3, sub(n).time4, sub(n).time5, sub(n).time6


outfile = infile + '.out'
openw,1,outfile

for j=0,cnt-1 do begin

printf,1,strcompress(sub(j).id,/remove_all)
printf,1,'00:' + string(sub(j).time1+addtime1,format='(I2)') + ':' + $
                 string(sub(j).time2+addtime2,format='(I2)') + ',' + $
                 string(sub(j).time3,format='(I3)') + '--->00:' + $
                 string(sub(j).time4+addtime1,format='(I2)') + ':' + $
                 string(sub(j).time5+addtime2,format='(I2)') + ',' + $
                 string(sub(j).time6,format='(I3)')
printf,1,sub(j).txt
printf,1,''

endfor

close,1

print,' %%% Wrote new file: ' + outfile

END
