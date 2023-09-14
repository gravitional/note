import zipfile as zpf
import pathlib as ptl

fs_lst = [
    'ext.pl',
    'pkg_test.py',
    'test.f90'
]


with zpf.ZipFile('my_zip_test.7z', mode='w', compression=zpf.ZIP_LZMA,
                 allowZip64=True, compresslevel=None, strict_timestamps=False) as myzip:
    idir = ptl.Path('my_zip')
    for name in fs_lst:
        p_file = idir/name
        myzip.write(name, arcname=str(p_file))
