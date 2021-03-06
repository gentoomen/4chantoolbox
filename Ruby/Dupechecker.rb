#!/usr/bin/ruby
# dupechecker
# Finds duplicate files based on md5 and filesize
# Created by DukeDerpington for the gentoomen 4chantoolbox project
#             (i also created the scraper :P just sayin')

require "getoptlong"
require "digest/md5"

## Defaults
@recursive   = false       #defaults to not recursive
@directory   = "./"        #default is current dir
@ask         = true        #defaults to requiring confirmation
@dummy       = false

# All important list of files
@filestats   = []

class Fileblock
    attr_reader :wholepath, :size, :md5
    def initialize inpath
        @wholepath = inpath
        @size = File.size(inpath)
        @md5 = ""
    end
    def get_md5
        @md5 = Digest::MD5.hexdigest(IO.read(@wholepath))
    end
end

def add_dir(dirpath)
    puts "Scanning directory " + dirpath
    (Dir.entries(dirpath) - ". ..".split).each do |tempfile|
        if File.directory? dirpath + tempfile and @recursive then
            add_dir(dirpath + tempfile + "/")
        elsif File.file? dirpath + tempfile then
            @filestats << Fileblock.new(dirpath + tempfile)
        end
    end
end

# get user input to determine if it's deleteion time or not
def get_choice
    choose = ""
    validOptions = ['y', 'n', 'o', 'b', 'yes', 'no', 'other', 'both']
    while not validOptions.include? choose do
        print ('(y)es / (n)o / the (o)ther file / (b)oth ? ')
        choose = gets.chomp
    end
    return choose
end

def pretty_size size
    suffixes = { "B" => 2**10, "K" => 2**20, "M" => 2**30, "G" => 2**40, "T" => 2**50 }
    suffixes.each do |suf, lim|
        if size > lim then
            next
        else
            return (size/lim/2**10.0).to_i.to_s + suf
        end
    end
end

def remfile infile
    if File.file?(infile.wholepath) then
        File.delete(infile.wholepath)
        puts infile.wholepath + " deleted."
    end
end

def showmatch filea, fileb
    puts "=========================== Size Match ==========================="
    puts "File:    " + filea.wholepath
    puts "Matches: " + fileb.wholepath
    puts "Size:    " + pretty_size( filea.size )
end

def usage
    puts "=================================================================="
    puts "Usage: Dupechecker-Ruby [OPTION] <directory>"
    puts "Script to remove duplicate files"
    puts " "
    puts " -d/--dummy        No deletion. Output findings without prompting"
    puts " -r/--recursive    Recurse through directories"
    puts " -y/--yes-to-all   Remove duplicates without prompting"
    puts " -h/--help         This help text"
    puts "=================================================================="
    exit
end

## Rip out any genuine paths in the args
ARGV.each do |arg|
    if File.directory? arg then
        @directory = arg
    end
end

## Handle Args
opts = GetoptLong.new(
    [ '-d', "--dummy", GetoptLong::NO_ARGUMENT ],
    [ '--help', "-h", GetoptLong::NO_ARGUMENT ],
    [ '--recursive', "-r", GetoptLong::NO_ARGUMENT ],
    [ '--yes-to-all', "-y", GetoptLong::NO_ARGUMENT ]
)

opts.each do |opt, arg|
    if ['-d', '--dummy'].include? opt then
        @dummy = true
    elsif ['-h', '--help'].include? opt then
        usage()
    elsif ['-r', '--recursive'].include? opt then
        @recursive = true
    elsif ['-y', '--yes-to-all'].include? opt then
        @ask = false
    end
end

# Spit out a warnign if we're using the current dir
if @directory == "./" then
    puts "No valid directory given. Using current directory."
end

# If the dir doesn't end with a "/" we need to add one
if not @directory[ - 1, 1 ] == "/" then
    @directory += "/"
end

## Main part
#Get a list of files only (no dirs)
add_dir(@directory)

#to allow us to catch exceptions
begin
    # The real meat of it -- compare listed files.
    @filestats.length.times do |i|
        startfile = @filestats[i]
        # be careful to check we haven't deleted either the file in a previous action
        if File.file?(startfile.wholepath) then

            (i+1).upto @filestats.length-1 do |e|
                checkagainst = @filestats[e]
                # be careful to check we haven't deleted either the file in a previous action
                if not File.file?(checkagainst.wholepath) then
                    break
                end

                if startfile.size == checkagainst.size then
                    showmatch(startfile, checkagainst)

                    # Generate md5s if necessary
                    [startfile, checkagainst].each do |f|
                        f.get_md5() if f.md5 == ""
                    end
                
                    if @dummy then
                        puts "=== Full md5 match found ==="
                    else
                        if startfile.md5 == checkagainst.md5 then
                            puts "Match found! Would you like to delete duplicate file " + startfile.wholepath.inspect + ""
    
                            # Allowance for the automatic overwrite flag
                            choice = get_choice() if @ask

                            # In instructed to, delete the file    
                            if ['o', 'other', 'b', 'both'].include? choice then
                                remfile(checkagainst)
                            end

                            if ['y', 'yes', 'b', 'both'].include? choice or not @ask then
                                remfile(startfile)
                                break
                            end

                        else
                            puts "No md5 match."
                        end
                    end
                end
            end
        end
    end
rescue Interrupt, SystemExit
    puts "Interrupted. Excited cleanly."
    exit
end
