#!/usr/bin/perl
# Dupechecker.pl
# Checks for duplicate files in a dir, non-recursively.
# by justin
#
# TODO: More functionality?????

use strict;
use warnings;
use v5.12;
use Digest::MD5::File qw(file_md5_hex);

# my $dir_path = $ARGV[0] || './';

sub dupe_check {
	my ($dir_path) = @_;
	opendir my ($dir), $dir_path or die "Error opening: ${dir_path}";
	my @files = readdir $dir;
	closedir $dir;

	my %hashes = ();
	foreach (@files) {
		next if -d "$dir_path/$_";
		my $md5 = file_md5_hex("$dir_path/$_");
		# say "$_ has MD5 hash: $md5.";
		$hashes{$_} = $md5;
	}

	foreach my $fname1 (keys(%hashes)) {
		foreach my $fname2 (keys(%hashes)) {
			if ($fname1 eq $fname2) {
				next;
			}
			if ($hashes{$fname1} eq $hashes{$fname2}) {
				say "$fname1 and $fname2 are the same file " .
					"(hash: $hashes{$fname1}).";
				last;
			}
		}
	}
}

if (@ARGV) {
	for (my $i = 0; $i < scalar @ARGV; $i++) {
		dupe_check($ARGV[$i]);
	}
} else {
	dupe_check('./');
}
