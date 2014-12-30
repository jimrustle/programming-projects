#!/usr/bin/ruby

def print_help ()
    puts "Available flag options: \n\
    flag | intention       | command \n\
    ------------------------------------------------------------------ \n\
    -nv  | 'no video'      | /usr/local/bin/mpv --no-video \n\
    -c   | 'check'         | youtube-dl --list-formats --no-playlist \n\
    -md  | 'music download | youtube-dl -x --audio-format best -f best \n\
    -q   | 'query'         | /usr/local/bin/mpv --ytdl-format= \n\
    -d   | 'download'      | youtube-dl --no-playlist \n\
    none | 'default'       | /usr/local/bin/mpv --ytdl-format=18"
end

link = `xclip -out`

if ARGV[0] == "-h"
    print_help()
elsif (link =~ /youtube/)
    puts "#{link} contains yt"
    if ARGV[0] == "-nv"
        puts "now playing: no video option"
        system "/usr/local/bin/mpv --no-video \"#{link}\""
    elsif ARGV[0] == "-c"
        puts "now checking formats"
        system "youtube-dl --list-formats --no-playlist \"#{link}\""
    elsif ARGV[0] == "-md"
        puts "now downloading (music only)"
        system "youtube-dl -x --audio-format best -f best \"#{link}\""
    elsif ARGV[0] == "-q"
        puts "now playing with chosen quality of " + ARGV[1]
        system "/usr/local/bin/mpv --ytdl-format=" +
            ARGV[1] + " \"#{link}\""
    elsif ARGV[0] == "-d"
        puts "now downloading (video)"
        system "youtube-dl --no-playlist \"#{link}\""
    else
        puts "now playing: low quality"
        system "/usr/local/bin/mpv --ytdl-format=18 \"#{link}\""
    end
else
    puts "#{link} is not a youtube link"
    print_help()
end

puts link
