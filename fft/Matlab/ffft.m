%graphics_toolkit("gnuplot");
fid = fopen("/tmp/mpd.fifo", 'rb');
while 1
    vals = fread(fid, 1024, 'int16')/256 + 256;
    %plot(vals);
    ys = abs(fft(vals)(3:128));
    plot(ys/sum(ys), "color", 'r', "linewidth", 1);
    axis([0, 128-3, 0, 0.2]);
    drawnow;
end
fclose(fid);
