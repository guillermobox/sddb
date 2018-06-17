#!/usr/bin/python3
import h5py
import sys

f = h5py.File(sys.argv[1])

def get_group(**kwargs):
    def filtergroup(group):
        for (k,v) in kwargs.items():
            if not k in group.attrs:
                return False
            if group.attrs[k] != v:
                return False
        return True
    return filter(filtergroup, f.values())

print(list(get_group(temperature=200)))
