#!/bin/bash

# script to download data from NOAA/NASA GOES archive
# the script will also convert the downloaded .gifs into a webm
# requirements: wget, `convert' from ImageMagick, ffmpeg or avconv

# note: goes-arch only stores up to 21 days' worth of images

# url format: east continental infrared year nDay
# example   : east continental infrared 2015 178th
url="http://www.goes-arch.noaa.gov/ECIR15"
site=""

mkdir -p conus
cd conus

# calculate your days and fill them in
#for day in 176; do
for day in {165..176}; do
    # leave the hour and minutes the same
    for hour in {00..23}; do
        for minute in 15 45; do
            site=$url$day$hour$minute".GIF"

            #echo $site

            wget $site
            # to prevent geting scrape-banned
            sleep 1
        done
    done
done

# convert all downloaded .gifs to .pngs
for file in *.GIF; do
    convert $file $file".png"
done

cd ..

# ffmpeg or avconv, hasn't been tested with ffmpeg yet
cat conus/*.png | avconv -f image2pipe -r 60 -vcodec png -i - -c:v libvpx -crf 4 -b:v 2M out.webm

#rm -rf conus

