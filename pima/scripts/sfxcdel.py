# -*- coding: utf-8 -*-
"""
Created on Wed Apr 22 15:01:35 2015

@author: Dmitry Duev
"""

import os, struct
import numpy as np
import argparse

class bindel(object):
    '''
    Parse binary delay files in SFXC format
    '''
    def __init__(self, fname, fdir='.'):
        self.fname = fname
        self.fdir = fdir
        
        self.parse()

    def parse(self):
        self.scans = {}
        with open(os.path.join(self.fdir, self.fname),'rb') as f_del:

            # binary header (header_size, sta_name):
            self.header_size = struct.unpack('<i', f_del.read(4))[0] 
            # (unpack always returns a tuple)
            self.sta = struct.unpack('<2sx', f_del.read(self.header_size))[0]
            
            # unpack scan by scan
            sn = 1 # scan number
            while True:
                try:
                    source = struct.unpack('<80sx', f_del.read(81))[0]
                    mjd = struct.unpack('<i', f_del.read(4))[0]
                    # init dict for scan
                    self.scans[sn] = {'source':None, 'mjd':None, 'time':None, \
                                      'uvw':None, 'delay':None, \
                                      'phase':None, 'amp':None}
                    self.scans[sn]['source'] = source
                    self.scans[sn]['mjd'] = mjd
                    
                    time, uvw, delay, phase, amp = [], [], [], [], []
                    
                    while True:                
                        t,u,v,w,d,p,a = struct.unpack('<7d', f_del.read(8*7))
                        if t==0 and d==0:
                            break
                        else:
                            time.append(t)
                            uvw.append((u,v,w))
                            delay.append(d)
                            phase.append(p)
                            amp.append(a)

                    self.scans[sn]['time'] = np.array(time)
                    self.scans[sn]['uvw'] = np.array(uvw)
                    self.scans[sn]['delay'] = np.array(delay)
                    self.scans[sn]['phase'] = np.array(phase)
                    self.scans[sn]['amp'] = np.array(amp)
                    
                    sn += 1

                except:
                    break
                
    def getSources(self):
        '''
        Return list of sources
        '''
        return list(set([self.scans[sn]['source'].strip() \
                         for sn in self.scans.keys()]))
                
    def dump(self, binary=False, txt=False, out_name=None, out_dir=None):
        '''
        Dump parsed (and processed) data back to a binary .del-file
        '''
        if out_name==None:
            dot = self.fname.index('.')
            out_name = self.fname[:dot] + 'i.del'
#            out_name = self.fname
        if txt:
            dot = self.fname.index('.')
            out_name_txt = self.fname[:dot] + '.txt'
        if out_dir==None:
            out_dir = self.fdir
        # create output dir if it doesn't exist:
        if not os.path.isdir(out_dir):
            os.mkdir(out_dir)
        
        if binary:
            with open(os.path.join(out_dir, out_name),'wb') as f_del:
                # binary header (header_size, sta_name):
                line = struct.pack('<i2sx', self.header_size, self.sta)
                f_del.write(line)
                
                for sn in self.scans.keys():
                    # source name and mjd
                    line = struct.pack('<80sxi', self.scans[sn]['source'], \
                                                 self.scans[sn]['mjd'])
                    f_del.write(line)
                    for t, (u,v,w), d, p, a in zip(self.scans[sn]['time'], \
                          self.scans[sn]['uvw'], self.scans[sn]['delay'], \
                          -self.scans[sn]['phase'], self.scans[sn]['amp']):
    #                      np.zeros_like(self.scans[sn]['delay']), self.scans[sn]['amp']):
                        line = struct.pack('<7d', t,u,v,w,d,p,a)
                        f_del.write(line)
                    # trailing zeros at scan end:
                    line = struct.pack('<7d', *list(np.zeros(7)))
                    f_del.write(line)

        # dump txt:
        if txt:
            with open(os.path.join(out_dir, out_name_txt),'w') as f_txt:
                # binary header (header_size, sta_name):
                line = '{:s}\n'.format(self.sta)
                f_txt.write(line)
                
                for sn in self.scans.keys():
                    # source name and mjd
                    line = '{:s} {:.0f}\n'.format(self.scans[sn]['source'].strip(), \
                                                 self.scans[sn]['mjd'])
                    f_txt.write(line)
                    for t, (u,v,w), d, p, a in zip(self.scans[sn]['time'], \
                          self.scans[sn]['uvw'], self.scans[sn]['delay'], \
                          -self.scans[sn]['phase'], self.scans[sn]['amp']):
    #                      np.zeros_like(self.scans[sn]['delay']), self.scans[sn]['amp']):
                        line = '{:f} {:f} {:f} {:f} {:.15e} {:.15e} {:.15e}\n'.format(t,\
                                                                   u,v,w,d,p,a)
                        f_txt.write(line)
                        

#%%
if __name__ == '__main__':
    '''
        If not imported, but run from the command line
    '''
    # create parser
    parser = argparse.ArgumentParser(prog='python sfxcdel.py', \
                formatter_class=argparse.RawDescriptionHelpFormatter,\
                description='Parse.')
    # optional arguments
#    parser.add_argument('-p', '--parallel', action='store_true',
#                        help='run computation in parallel mode')

    # positional argument
    parser.add_argument('delfiles', type=str, help='input del-file(s)', nargs='*')    
    args = parser.parse_args()
    
    for delfile in args.delfiles:
        bd = bindel(delfile)
        bd.dump(txt=True)
