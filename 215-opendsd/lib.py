"""
Python library file for the OpenDSD job.
"""
import gzip
  
def download_ambry_db(url,name):  
    """Download an Ambry database file from the repository. """
    import gzip
    import sys
    import os
    
    if not os.path.exists(name+'.gz'):
        print 'Downloading:', url
        sys.stdout.flush()
        import urllib
        urllib.urlretrieve (url, name+'.gz')
    else:
        print 'Already downloaded:', url
        sys.stdout.flush()

    if not os.path.exists(name):
        print 'Extracting to:',name
        sys.stdout.flush()
        with open(name,'wb') as out_f, gzip.open(name+'.gz', 'rb') as in_f:
            out_f.write(in_f.read())   
    else:
        print 'Already extracted:', name
        sys.stdout.flush()
