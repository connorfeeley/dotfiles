#! /usr/bin/env perl

use warnings;
use strict;

use Image::Xpm;
use POSIX;

my $iwidth = 8;
my $iheight = 24;
my $pixels_per = 2;
my $color_bg = "white";
# my $color_fg = "darkseagreen";
my $color_fg = "grey90";

$color_bg = "#2B2B2B";
$color_bg = "#1f1f1f"; # zenburn
$color_bg = "#22242b"; # doom
$color_fg = "#999999";
$color_bg = "#eeeeee";


for ( my $file_num = 0; $file_num <= 8; $file_num++ ) {
    my $fname = "load_$file_num.xpm";
    unlink $fname;
    my $i = Image::Xpm->new(-file => $fname, -width => $iwidth, -height => $iheight);

    my $h = $file_num * $pixels_per;

    for ( my $x = 0; $x < $iwidth; $x++ ) {
        for ( my $y = 0; $y < $iheight; $y++ ) {
            $i->xy($x, $y, $color_bg);
        }
    }

    for ( my $x = 0; $x < $iwidth; $x++ ) {
        for ( my $y = ($iheight - 6); $y >= ($iheight - 6 - $h); $y-- ) {
            $i->xy($x, $y, $color_fg);
        }
    }

    $i->save;
}
