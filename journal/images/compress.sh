#!/bin/sh

# Compresses the resized images to the format that will be served. Because the
# effect of jpeg compression depends a lot on the image, the compression
# settings have been tweaked for every image individually.

# I have a few images where jpeg performs poorly and png performs really well.
# Sometimes the jpeg is slightly smaller than the png at a quality setting
# where the artifacts become unnoticeable, sometimes png actually outperforms
# jpeg.

# Mozjpeg insists on conflicting with libjpeg-turbo, but many packages depend on
# libjpeg-turbo and I don't want to replace it system-wide. Fortunately there
# is an aur package that installs mozjpeg to /opt.

if [[ "$OSTYPE" == "darwin"* ]]; then
  mozjpeg='/usr/local/cellar/mozjpeg/3.1_1/bin/cjpeg -quality'
else
  mozjpeg='/opt/mozjpeg/bin/cjpeg -quality'
fi


mkdir -p compressed

$mozjpeg 88.0 resized/elustblacknew.jpg > compressed/elustblacknew.jpg
$mozjpeg 88.0 resized/Holden-and-Camille-Header.jpg > compressed/Holden-and-Camille-Header.jpg
$mozjpeg 88.0 resized/HEADER-teachers.jpg > compressed/HEADER-teachers.jpg
$mozjpeg 88.0 resized/2015-11-15-sinful-sunday.jpg > compressed/2015-11-15-sinful-sunday.jpg
$mozjpeg 88.0 resized/Sex-is-my-new-hobby-HEADER.jpg > compressed/Sex-is-my-new-hobby-HEADER.jpg
$mozjpeg 88.0 resized/2017-03-01-wagon.jpg > compressed/2017-03-01-wagon.jpg
$mozjpeg 88.0 resized/Steeled-Snake-HEader-300x225.jpg > compressed/Steeled-Snake-HEader-300x225.jpg
