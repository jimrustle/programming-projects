
fid = fopen("/tmp/mpd.fifo", 'rb');
while 1
    vals = fread(fid, 1024, 'int16')/256 + 256;
    plot(vals);
    drawnow;
end
fclose(fid);
