import wave, array, math, time, argparse, sys
import numpy, pywt
from scipy import signal
import pdb
import matplotlib.pyplot as plt
import pandas as pd
from scipy.io import wavfile 
from pydub import AudioSegment
import pywt

import pandas as pnd
from nltk.tokenize import RegexpTokenizer
from stop_words import get_stop_words
from nltk.stem.porter import PorterStemmer
from sklearn.feature_extraction.text import CountVectorizer

import os
from os.path import basename
import argparse

from sklearn.metrics.pairwise import linear_kernel,cosine_similarity
from mutagen.mp3 import MP3
from mutagen.easyid3 import EasyID3
import mutagen.id3
import requests
import magic




class bpmExtractor():
    
    def __init__(self):
        self.bpmsDF = pd.DataFrame(columns=['audioFile','bpms','title','genre','composer','album'])
        #pass
    
    def read_wav(self,filename):
        return wavfile.read(filename)

    def read_wav_mono(self,filename):

        #open file, get metadata for audio
        try:
            wf = wave.open(filename,'rb')
        except IOError as e:
            print (e)
            return

        # typ = choose_type( wf.getsampwidth() ) #TODO: implement choose_type
        nsamps = wf.getnframes();
        assert(nsamps > 0);
        #print ("nsamps: {}".format(nsamps))

        fs = wf.getframerate()
        assert(fs > 0)
        #print ("fs: {}".format(fs))

        #tmprate, tmpdata = wavfile.read(filename)
        #print("shape tmdata: {} tmprate: {}".format(tmpdata.shape,tmprate))

        # read entire file and make into an array
        samps = list(array.array('i',wf.readframes(nsamps)))
        #print ('Read', nsamps,'samples from', filename)
        try:
            assert(nsamps == len(samps))
        except AssertionError as e:
            print  (nsamps, "not equal to", len(samps))
        #print ("shape samps: {}".format(len(samps)))
        return samps, fs


    def ab_adjuster(self,value, granualityRoundOff):
        #granualityRoundOff means : if granualityRoundOff is 5, then 12.4 would be rounded to 10, 
        # 12.6 would be rounded to 15
        quotient,remainder = divmod(value,granualityRoundOff)
        #remainder = value%granualityRoundOff
        #print (quotient)
        if ( float(remainder) < float(granualityRoundOff)/2):
            #print(quotient*granualityRoundOff)
            return quotient*granualityRoundOff
        else:
            #print((quotient+1)*granualityRoundOff)
            return ((quotient+1)*granualityRoundOff)


    # print an error when no data can be found
    def no_audio_data(self):
        print ("No audio data for sample, skipping...")
        return None, None

    # simple peak detection
    def peak_detect(self,data):
        max_val = numpy.amax(abs(data)) 
        peak_ndx = numpy.where(data==max_val)
        if len(peak_ndx[0]) == 0: #if nothing found then the max must be negative
            peak_ndx = numpy.where(data==-max_val)
        return peak_ndx

   
    
    def bpm_detector(self,data,fs):
        #print("")
        #print ("+"*80)
        cA = [] 
        cD = []
        correl = []
        cD_sum = []
        levels = 4
        max_decimation = 2**(levels-1);
        min_ndx = 60./ 220 * (fs/max_decimation)
        max_ndx = 60./ 40 * (fs/max_decimation)

        #print("type(Data): {} shape(Data): {}".format(type(data),len(data)))
        #data = data.tolist()
        for loop in range(0,levels):
            cD = []
            # 1) DWT
            if loop == 0:
                [cA,cD] = pywt.dwt(data,'db4');
                #cA : approximation coeff
                #cD : Detailed Coeff
                cD_minlen = len(cD)/max_decimation+1;
                cD_sum = numpy.zeros(cD_minlen);
            else:
                [cA,cD] = pywt.dwt(cA,'db4');

            #print("cA : {}, cD: {}".format(cA,cD))

            #print ("DWT : type(cA) : {} type(cD): {} len(cA) {}, len(cD) {}".format(type(cA), type(cD), cA.shape, cD.shape))
            # 2) Filter
            cD = signal.lfilter([0.01],[1 -0.99],cD);
            #print("Filtered cD")
            #print(cD)

            # 4) Subtractargs.filename out the mean.

            # 5) Decimate for reconstruction later.
            cD = abs(cD[::(2**(levels-loop-1))]);
            #print("Cd raise to level-inverse{}",format(cD))
            #print("cD shape: {}".format(cD.shape))
            cD = cD - numpy.mean(cD);
            #print("cD - mean(cD)")
            #print(cD)
            # 6) Recombine the signal before ACF
            #    essentially, each level I concatenate 
            #    the detail coefs (i.e. the HPF values)
            #    to the beginning of the array
            cD_sum = cD[0:cD_minlen] + cD_sum;
            #print ("cD_sum")
            #print(cD_sum)

        #print ("Loop over")
        if [b for b in cA if b != 0.0] == []:
            return no_audio_data()
        # adding in the approximate data as well...    
        cA = signal.lfilter([0.01],[1 -0.99],cA);
        cA = abs(cA);
        cA = cA - numpy.mean(cA);
        #print("cA - mean(cA){}".format(cA))

        #print ("Filtered cA{}".format(cA))
        cD_sum = cA[0:cD_minlen] + cD_sum;
        #print ("cD_sum_with-approx-cA")
        #print(cD_sum)

        # ACF
        correl = numpy.correlate(cD_sum,cD_sum,'full') 
        #print("sizeof(cD_sum_with-approx-cA){}, sizeof(ACF){}".format(len(cD_sum),len(correl)))
        #print ('ACF on cD_sum_with-approx-cA')
        #print (correl)

        midpoint = len(correl) / 2
        correl_midpoint_tmp = correl[midpoint:]
        #print ('correl_midpoint_tmp')
        #print (correl_midpoint_tmp)
        #print ("min_ndx{} max_ndx{}".format(min_ndx,max_ndx))
        peak_ndx = self.peak_detect(correl_midpoint_tmp[min_ndx:max_ndx]);
        if len(peak_ndx) > 1:
            return no_audio_data()

        #print ('peak_ndx')
        #print (peak_ndx)
        peak_ndx_adjusted = peak_ndx[0]+min_ndx;
        #print ('peak_ndx_adjusted,max_decimation')
        #print (peak_ndx_adjusted,max_decimation)
        bpm = 60./ peak_ndx_adjusted * (fs/max_decimation)
        #print (bpm)
        return bpm,correl


    def mp3ToWav(self,mp3dir,mp3FilePath):
        #mp3FilePath = "".join([mp3dir, "/",filename])
        if os.path.exists(mp3FilePath) :#and self.isMp3Valid(mp3FilePath):
            audioMime = magic.from_file(mp3FilePath,mime=True)
            #print(audioMime)
            if 'application/octet-stream' not in audioMime and 'audio/mpeg' not in audioMime:
                print("Skipping {}".format(audioMime))
                return None,mp3FilePath
                    
            sound = AudioSegment.from_mp3(mp3FilePath)
            mp3FileName = basename(mp3FilePath)

            mp3FileNameWithoutExt = os.path.splitext(mp3FileName)[0]
            wavFilePath = "".join([mp3dir,"/",mp3FileNameWithoutExt,'.wav'])
            #print (wavFilePath)
            sound.export(wavFilePath, format="wav")
            return (wavFilePath,mp3FilePath)
            
        return None,mp3FilePath
    

    def extractBPMtoDF(self,filename,mp3filePath,audioWindowinSec):
        #filename = "/home/tivo/Desktop/songs/wav/AYTH.wav"
        #window = 5
        window = audioWindowinSec
        #global bpmsDF

        fs,samps = self.read_wav(filename)

        data = []
        correl=[]
        bpm = 0
        n=0;
        nsamps = samps.shape[0]
        #print("samp shape: {}, row: {}, col: {}".format(samps.shape,samps.shape[0],samps.shape[1]))
        window_samps = int(window*fs)         
        samps_ndx = 0;  #first sample in window_ndx 
        max_window_ndx = nsamps / window_samps;
        bpms = numpy.zeros(max_window_ndx)
        adjusted_bpms = []

        #print ("fs {}, nsamps {}, window_samps {},max_window_ndx {}, int(max_window_ndx) {}"\
        #       .format(fs, nsamps, window_samps,max_window_ndx, int(max_window_ndx)))
        #iterate through all windows
        for channelIdx in range(0,samps.shape[1]):
            print()
            sampList = list(samps[:,channelIdx])
            for window_ndx in range(0,int(max_window_ndx)):

                #get a new set of samples
                #print n,":",len(bpms),":",max_window_ndx,":",fs,":",nsamps,":",samps_ndx
                #print("channelIdx: {}",format(channelIdx))
                print(len(sampList))
                #data = list(samps[channelIdx:,].tolist()[samps_ndx:samps_ndx+window_samps])
                data = sampList[samps_ndx:samps_ndx+window_samps]
                #print("type(Data): {}".format(type(data)))
                print("Data-size: {}".format(len(data)))
                if not ((len(data) % window_samps) == 0):
                    print("Skipping channelIdx :{}".format(channelIdx))
                    continue
                    #raise AssertionError( str(len(data) ) ) 

                bpm, correl_temp = self.bpm_detector(data,fs)

                if bpm is None:
                    continue

                adjusted_bpm = self.ab_adjuster(bpm,5) # 5 second is round off granuality
                bpms[window_ndx] = bpm
                adjusted_bpms.append(str(int(adjusted_bpm)))
                correl = correl_temp

                #iterate at the end of the loop
                samps_ndx = samps_ndx+window_samps;
                n=n+1; #counter for debug...
            
            mp3H = Mp3Header(filename)
            self.bpmsDF = self.bpmsDF.append(pd.DataFrame({'audioFile': filename,'bpms': ' '.join(adjusted_bpms),
                                                'title': mp3H.getTitle(),'genre': mp3H.getGenre(),
                                                'composer': mp3H.getComposer(),'album': mp3H.getAlbum()}, index=[0]))
            bpm = numpy.median(bpms)
            #print ('Completed.  Estimated Beats Per Minute:', bpm)

#n = range(0,len(correl))
#plt.plot(n,abs(correl)); 
#plt.show(False); #plot non-blocking
#time.sleep(10);
#plt.close();


    def extractBPMtoDFExisting(self,filename,mp3filePath,audioWindowinSec,bpmGranuality):
        #filename = "/home/tivo/Desktop/songs/wav/AYTH.wav"
        #window = 5
        window = audioWindowinSec
        #global bpmsDF

        samps,fs = self.read_wav_mono(filename)

        data = []
        correl=[]
        bpm = 0
        n=0;
        nsamps = len(samps)
        window_samps = int(window*fs)         
        samps_ndx = 0;  #first sample in window_ndx 
        max_window_ndx = nsamps / window_samps;
        bpms = numpy.zeros(max_window_ndx)
        adjusted_bpms = []

        #print ("fs {}, nsamps {}, window_samps {},max_window_ndx {}, int(max_window_ndx) {}"\
        #       .format(fs, nsamps, window_samps,max_window_ndx, int(max_window_ndx)))
        #iterate through all windows
        for window_ndx in range(0,int(max_window_ndx)):

            #get a new set of samples
            #print n,":",len(bpms),":",max_window_ndx,":",fs,":",nsamps,":",samps_ndx
            data = samps[samps_ndx:samps_ndx+window_samps]
            #print("type(Data): {}".format(type(data)))
            #print("Data-size: {}".format(len(data)))
            if not ((len(data) % window_samps) == 0):
                raise AssertionError( str(len(data) ) ) 

            bpm, correl_temp = self.bpm_detector(data,fs)

            if bpm is None:
                continue

            adjusted_bpm = self.ab_adjuster(bpm,bpmGranuality) # 5 second is round off granuality
            bpms[window_ndx] = bpm
            adjusted_bpms.append(str(int(adjusted_bpm)))
            correl = correl_temp

            #iterate at the end of the loop
            samps_ndx = samps_ndx+window_samps;
            n=n+1; #counter for debug...

        mp3H = Mp3Header(mp3FilePath)
        self.bpmsDF = self.bpmsDF.append(pd.DataFrame({'audioFile': mp3FilePath,'bpms': ' '.join(adjusted_bpms),
                                                'title': mp3H.getTitle(),'genre': mp3H.getGenre(),
                                                'composer': mp3H.getComposer(),'album': mp3H.getAlbum()}, index=[0]))
        bpm = numpy.median(bpms)
        #print ('Completed.  Estimated Beats Per Minute:', bpm)

#n = range(0,len(correl))
#        plt.plot(n,abs(correl)); 
#        plt.show(False); #plot non-blocking
#        time.sleep(10);
#        plt.close();
#
class similarSongs():
    def __init__(self,dataset):
        tokenizer = RegexpTokenizer(r'\w+')
        p_stemmer = PorterStemmer()
        self.wCModel = CountVectorizer(min_df=1,tokenizer=None,stop_words="english")
        self.dataset = dataset

    def fit_transform(self,datasetFeatureCol):
        print (self.dataset[datasetFeatureCol].head(5))
        self.inX = self.wCModel.fit_transform(self.dataset[datasetFeatureCol])
        self.datasetFeatureCol = datasetFeatureCol

    def findKSimilarSongs(self,song,k):
        #print (song)
        #print (self.dataset[self.dataset['audioFile'] == song])
        #print (self.dataset[self.dataset['audioFile'] == song].index)
        songIndex = self.dataset[self.dataset['audioFile'] == song].index.tolist()[0]
        #print (songIndex)
        cosine_similarities = cosine_similarity(self.inX.A, self.inX.A[songIndex], dense_output=True).flatten()
        #print (cosine_similarities)

        #related_docs_indices = cosine_similarities.argsort()[:-5:-1]
        related_docs_indices = cosine_similarities.argsort()[:-k-2:-1]
        return (related_docs_indices[1:])
        #return (related_docs_indices)

class Mp3Header():
    def __init__(self, mp3FilePath):
        f = open(mp3FilePath, "rb")
        self.mp3file = MP3(mp3FilePath, ID3=EasyID3)
        self.fileStatus = True
        #print (str(f.read(3).decode("utf-8")))
        #if f.read(3).decode("utf-8") == "ID3":
        #    print("1")
        #    self.mp3file = MP3(mp3FilePath, ID3=EasyID3)
        #    self.fileStatus = True
        #else:
        #    print(0)
        #    self.fileStatus = False
        
    def getGenre(self):
        if self.fileStatus == True:
            return self.mp3file.get("genre")
        else:
            return ""
  
    def getAlbum(self):
        if self.fileStatus == True:
            return self.mp3file.get("album")
        else:
            return ""

    def getArtist(self):
        if self.fileStatus == True:
            return self.mp3file.get("artist")
        else:
            return ""

    def getComposer(self):
        if self.fileStatus == True:
            return self.mp3file.get("composer")
        else:
            return ""
    
    def getTitle(self):
        if self.fileStatus == True:
            return self.mp3file.get("title")
        else:
            return ""


print (__name__)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Process mp3dir and convert all files in dir to wav')
    parser.add_argument('-m', '--mp3dir',
                       help='mp3Dir directory absolute path where all mp3 for BPM are kept')
    parser.add_argument('-c','--csvFilePath', required=True,
                       help='mp3FileName(column1),bpms(column2) space seprated would be saved here')
    parser.add_argument('--regenerateCSV',
                       help='mp3FileName(column1),bpms(column2) space seprated would be saved here')
    parser.add_argument('-s', '--songToSearch',
                       help='Song for which you need recommendations')

    args = parser.parse_args()
    csvFilePath = args.csvFilePath
    #songToSearch = args.songToSearch
    if (args.regenerateCSV):

        #wavDir = '/home/tivo/Desktop/songs/wav/test'
        #mp3Dir = '/home/tivo/Desktop/songs/mp3'
        #csvFilePath = '/home/tivo/Desktop/bpms.csv'

        if(args.mp3dir is None):
            print ("Error: --mp3dir or -m: where all mp3 are kept is required for regenerate case")
            exit()

        mp3Dir = args.mp3dir
        bpmE = bpmExtractor()

        for dirpath, dirnames, filenames in os.walk(mp3Dir):
            for filename in [f for f in filenames if f.endswith(".mp3")]:
                filepath = os.path.join(dirpath, filename)

                #print (mp3Dir, filename)
                #mp3FilePath = "".join([mp3Dir, "/",filename])
                wavFilePath,mp3FilePath = bpmE.mp3ToWav(mp3Dir,filepath)
                #print("FileName: {} ".format(wavFilePath))
                if wavFilePath is not None and os.path.exists(wavFilePath):
                    bpmE.extractBPMtoDFExisting(wavFilePath,mp3FilePath,5,5)
                    #Remove wavFile after use
                    os.remove(wavFilePath)
                #print("=="*80)
                #extractBPMtoDF(wavFilePath,10)

        bpmE.bpmsDF.to_csv(csvFilePath)

    print ("*"*80)
    print ("CSV Generated")
    #bpmsDF = pnd.read_csv('/home/tivo/Desktop/bpms_mono_way.csv')
    if(args.songToSearch is None):
        print ("Error: -s or --songToSearch : aka song for which you need recommendations is required for regenerate case")
        exit()

    songToSearch = args.songToSearch
    bpmsDF = pnd.read_csv(csvFilePath)

    sSongsObj = similarSongs(bpmsDF)
    sSongsObj.fit_transform('bpms')

    #songToSearch = '/home/tivo/Desktop/songs/wav/XKPJ.wav'
    similarSongList = sSongsObj.findKSimilarSongs(songToSearch, 5)

    print ("Song requested bpm is {}".format(bpmsDF[bpmsDF['audioFile'] == songToSearch]['bpms']))
    for count,j in enumerate(similarSongList):
        print("Song-{} is: {}".format(count,bpmsDF.loc[j]['audioFile']))
        print("\t bpms is : {}".format(bpmsDF.loc[j]['bpms']))
        print ()

