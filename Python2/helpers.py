#!/usr/bin/python3.2
"""
Created on 20 Aug 2011

@author: shana
Contact: ShanaLover on irc.rizon.net
This module contains helper functions usable with Python3 interpreters.
"""
import shutil, hashlib, platform, urllib.request


def downloadFile(linkToFile, saveDestination, maxTries = 5):
    """
    This function takes a link to a file and a save destination to a file.
    It will download and save the file and return the path of the saved file.
    Will use regular urllib.request download if platform is not Linux.
    
    arguments: (linkToFile, saveDestination) -- link to a file and path to a directory
    keyword arguments: (maxTries = 5) -- number of tries before ditching the download. Default unlimited.
    
    Returns: string - path to the saved file
    """
    if platform.system() == 'Linux':
        shutil.os.system('wget -nv -nc -t {0} -P {1} {2}'.format(maxTries, saveDestination, linkToFile))
        return shutil.os.path.join(saveDestination, shutil.os.path.split(linkToFile)[1])
    #Must be on non-Linux system therefore no wget
    else:
        #Navigate to specified directory
        if not shutil.os.path.isdir(saveDestination):
            shutil.os.mkdir(saveDestination)
        originalDirectory = shutil.os.path.abspath(shutil.os.curdir)
        shutil.os.chdir(saveDestination)
        
        #Append http:// if necessary
        if not linkToFile.find('http://') == 0:
            linkToFile = 'http://' + linkToFile
        tried = 0
        while maxTries > tried:
            #Download the file
            try:
                #Download the data and file size information
                fileData = urllib.request.urlopen(linkToFile)
                fileSize = int(fileData.headers['Content-Length'])
                fileData = fileData.read()
                
                #Obtain file name
                saveFileTo = shutil.os.path.split(linkToFile)[1]
                
                #Save the file
                fileSaveObject = open(saveFileTo, 'wb')
                fileSaveObject.write(fileData)
                fileSaveObject.close()
                
                #Check that the file size matches
                if not shutil.os.path.getsize(saveFileTo) == fileSize:
                    tried += 1
                    continue 
            except:
                tried += 1
        #Go back to the initial directory
        shutil.os.chdir(originalDirectory)
        
        return shutil.os.path.join(saveDestination, shutil.os.path.split(linkToFile)[1])


def fileMD5(filePath):
    """
    Returns hexdigested md5 for a file provided as an argument.
    
    Returns: string - hexdigested MD5 of a file
    """
    md5 = hashlib.md5()
    file = open(filePath, 'rb')
    while True:
        data = file.read(128)
        if not data:
            break
        md5.update(data)
    return md5.hexdigest()


def removeFileDuplicates(directoryWithFiles, askUser = False):
    """
    Non recursive file duplicate removal function.
    Will perform initial check based on file size and then on md5 if necessary.
    Removes files with 0 as their size without asking.
    arguments - (directoryWithFiles)
    keyword arguments (askUser = False) -- decides whether it should ask the user for input before deletion
    
    
    Return: integer - number of removed files
    """
    #Make a check
    if not shutil.os.path.isdir(directoryWithFiles):
        print('{0} is not a directory or doesn\'t exist.'.format(shutil.os.path.abspath(directoryWithFiles)))
        return -1
    
    #Remember current directory and change current directory to the one with files
    oldDirectory = shutil.os.path.abspath(shutil.os.curdir)
    shutil.os.chdir(directoryWithFiles)
    
    #Create a list of all the existing files within the directory
    originalFileList = [file for file in shutil.os.listdir() if shutil.os.path.isfile(file)]
    
    #Get size of every file and put it in a dictionary based on that size
    fileDictionary = {}
    for file in originalFileList:
        currentFileSize = shutil.os.path.getsize(file)
        if fileDictionary.get(currentFileSize, -1) == -1:
            fileDictionary[currentFileSize] = [file]
        else:
            fileDictionary[currentFileSize].append(file)
    
    #Remove all files with 0B in size
    if not fileDictionary.get(0, -1) == -1:
        for file in fileDictionary[0]:
            shutil.os.remove(file)
        del fileDictionary[0]
    
    #Remove keys with only one value (only one image with that file size)
    temporaryDictionary = fileDictionary.copy()
    for key in temporaryDictionary.keys():
        if not len(fileDictionary[key]) > 1:
            del fileDictionary[key]
    del temporaryDictionary
    
    #Now start the removal.
    MD5Dictionary = {}
    for key in fileDictionary.keys():
        for file in fileDictionary[key]:
            currentMD5 = fileMD5(file)
            if MD5Dictionary.get(currentMD5, -1) == -1:
                MD5Dictionary[currentMD5] = file
            elif askUser == True:
                answer = ''
                while not answer == 'y' or not answer == 'n' or not answer == 'Y' or not answer == 'N':
                    print('Found a duplicate file. Do you want to delete {0} in favour of {1}?'.format(file, MD5Dictionary[currentMD5]))
                    answer = input('(y/n): ')
                if answer == 'Y' or answer == 'y':
                    shutil.os.remove(file)
                else:
                    continue
            else:
                shutil.os.remove(file)
                
    currentFileList = [file for file in shutil.os.listdir() if shutil.os.path.isfile(file)]
    shutil.os.chdir(oldDirectory)
    
    return len(originalFileList) - len(currentFileList)
                

def renameFiles(
                directoryWithFiles, 
                nameLength = 7, 
                fixExtensions = False, 
                quiet = False, 
                extensionList = ['.jpg', '.png', '.gif', '.jpeg', '.bmp'], 
                safetyPass = True
                ):
    """
    A function that takes a directory and renames every file in that directory. Non-recursive.
    The files will be renamed in numerical order (1.jpg, 2.png, 3.jpg) and will be padded with zeros.
    Length of the padding will depend on the length argument.
    Arguments - (directoryWithFiles) -- directory with the files user wants to rename.
    Keyword arguments - (nameLength = 7, fixExtensions = False, quiet = False, extensionList = ['.jpg', '.png', '.gif', '.jpeg', '.bmp'], safetyPass = True) 
                                                            -- total length of a file name after padding it with zeros, use nameLength = 0 for no padding
                                                            -- decides whether the function will attempt to fix/guess broken file extensions.
                                                            -- decides whether the function will provide any output
                                                            -- decides what extensions we want to rename (default extensions are the only fixable ones)
                                                            -- decides whether do the first safety rename to avoid over-writing over files. Recommended.
    Returns: integer - number of renamed files
    """
    #Check if directory exists
    if not shutil.os.path.isdir(directoryWithFiles):
        print('No such directory.')
        return 0
    
    #Change directory to the one with files
    oldDirectory = shutil.os.path.abspath(shutil.os.curdir)
    shutil.os.chdir(directoryWithFiles)
    
    #Create a list of all the files within a directory
    fileList = [file for file in shutil.os.listdir() if shutil.os.path.isfile(file)]
    
    #Split file names and extensions into a tuple
    fileList = [shutil.os.path.splitext(file) for file in fileList if True]
    
    #Possibly attempt to fix extensions
    eList = []
    fixed = 0
    for file in fileList:
        if fixExtensions:
            if file[1] == '' or file[1] == '.' or file[1] == '.j' or file[1] == '.jp':
                eList.append((file[0], '.jpg'))
                fixed += 1
                if not quiet:
                    print('Changed {0} to {1}'.format(file[1], '.jpg'))
            elif file[1] == '.jpe' and not file[1] == '.jpeg' :
                eList.append((file[0], '.jpeg'))
                fixed += 1
                if not quiet:
                    print('Changed {0} to {1}'.format(file[1], '.jpeg'))
            elif 'n' in file[1] and not 'm' in file[1] and not file[1] == '.png':
                eList.append((file[0], '.png'))
                fixed += 1
                if not quiet:
                    print('Changed {0} to {1}'.format(file[1], '.png'))
            elif 'f' in file[1] and not file[1] == '.gif':
                eList.append((file[0], '.gif'))
                fixed += 1
                if not quiet:
                    print('Changed {0} to {1}'.format(file[1], '.gif'))
            elif 'b' in file[1] and not file[1] == '.bmp':
                eList.append((file[0], '.bmp'))
                fixed += 1
                if not quiet:
                    print('Changed {0} to {1}'.format(file[1], '.bmp'))
            else:
                eList.append(file)
        else:
            eList.append(file)
        
    if not quiet and fixExtensions:
        print('Fixed {0} extensions.'.format(fixed), end = '\r')
        
    #Filter out only files with desired extensions
    fileList = [file for file in eList if file[1] in extensionList]
    
    #Start the renaming
    totalToRename = len(fileList)
    if not quiet:
        print('There are {0} files to rename.'.format(totalToRename))
    
    #First rename for safety
    if safetyPass:
        preFileList = []
        if not quiet:
            print('Renaming every existing filename prevent over-writing.')
        for index, file in enumerate(fileList):
            #First, add a prefix and suffix to all the existing file names. This prevents over-writing existing files with already existing name pattern.
            #Prefix in form of current index, pr, and old file name (e.g. asdf.jpg might become 12prasdf.jpg
            prefixedName = str(index) + 'pr' + file[1]
            
            #Keep changing prefixedName until not unique
            while shutil.os.path.exists(prefixedName + file[1]):
                prefixedName += str(index)
            shutil.os.rename(file[0] + file[1], prefixedName + file[1])
            preFileList.append((prefixedName, file[1]))
    
            if not quiet:
                print('Prefixed and renamed image {0} out of {1}'.format(index + 1, totalToRename), end = '\r')
    else:
        preFileList = fileList[:]
        
    #Finished prefixing images, do final renaming now
    renamedFiles = 0
    for index, file in enumerate(preFileList):
        #Pad new name with zeros
        numberOfZeros = nameLength - len(str(index + 1))
        if numberOfZeros < 0:
            numberOfZeros = 0
        
        newName = (numberOfZeros * '0') + str(index + 1) + file[1]
        if not shutil.os.path.exists(newName):
            shutil.os.rename(file[0] + file[1], newName)
            renamedFiles += 1
            if not quiet:
                print('Renamed {0} out of {1}.      '.format(renamedFiles, totalToRename), end = '\r')
        else:
            print('\nWas about to overwrite {0} with a different file. Something went wrong. Skipping.'.format(newName))
            continue
        
    #Done renaming.
    if not quiet:
        print('Done. Renamed total of {0} out of {1} files.'.format(renamedFiles, totalToRename))
    
    shutil.os.chdir(oldDirectory)
    return renamedFiles
        
        