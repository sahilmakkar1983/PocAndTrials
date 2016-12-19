
# coding: utf-8

# In[20]:

import os
from pydub import AudioSegment
from os.path import basename
import argparse

parser = argparse.ArgumentParser(description='Process mp3dir and convert all files in dir to wav')
parser.add_argument('--mp3dir', required=True,
                   help='mp3 directory absolute path where all mp3 to converted to wav are present')
parser.add_argument('--wavdir', required=True,
                   help='absolute dir-path where converted wavs would be stored')

args = parser.parse_args()

print ("1");
for filename in os.listdir(args.mp3dir):
   
    print ("2")
    print (args.mp3dir, filename)
    mp3FilePath = "".join([args.mp3dir, "/",filename])
    sound = AudioSegment.from_mp3(mp3FilePath)
    print("Hi:{} ".format(mp3FilePath))
    mp3FileName = basename(mp3FilePath)
    
    mp3FileNameWithoutExt = os.path.splitext(mp3FileName)[0]
    wavFilePath = "".join([args.wavdir,"/",mp3FileNameWithoutExt,'.wav'])
    print (wavFilePath)
    sound.export(wavFilePath, format="wav")
