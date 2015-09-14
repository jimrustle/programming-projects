
# vc0706 image grabber tool

```
perl -e 'print "\x56\x00\x11\x00";' > /dev/ttyACM0
od -x < /dev/ttyACM0
```

only grabs images from the camera, doesn't attempt to mess around
with baud or color settings

Running `vc0706_test [your serial line]` without additional
arguments will just test the connection and print the version number
of your camera.

Run `./vc0706_test [serial line] --snap_image` to snap an image to `out.jpg`, or
use an additional argument (`./vc0706_test  [serial line] --snap_image hello.jpg`)
for a filename output.

# serial table

make sure you check your camera's serial line first via `dmesg` or
`journalctl -f`

(following table is probably incorrect)
(I use an Arduino Uno as my UART interface, so I usually choose
/dev/ttyUSB0)

device | name
- | -
usb-to-uart | /dev/ttyUSB0
uart device | /dev/ttyS0
I have no idea | /dev/ACM0

