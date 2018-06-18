#!/usr/bin/python3
import h5py
import sys

class sddb(object):
    def __init__(self, path):
        self.file = h5py.File(path)

    def filter(self, **kwargs):
        def filtergroup(group):
            link = self.file.get(group.name, getclass=True, getlink=True)
            if link == h5py.SoftLink:
                return False
            for (k,v) in kwargs.items():
                if not k in group.attrs:
                    return False
                if group.attrs[k] != v:
                    return False
            return True
        return filter(filtergroup, self.file.values())

    def show_group(self, group):
        print(group.name)
        for attr in group.attrs:
            print("  ", attr, group.attrs[attr])
        for dataset in group:
            self.show_dataset(group.get(dataset))

    def label_from_dataset(self, dset):
        fullname = dset.attrs.get('fullname')
        if fullname:
            label = fullname.decode('ascii')
        else:
            label = dset.name.split('/')[-1]

        units = dset.attrs.get('units')

        if units:
            label = label + ' (' + units.decode('ascii') + ')'
        return label

    def show_dataset(self, dset):
        print("    {0} \"{1}\" {2} items".format(dset.name, self.label_from_dataset(dset), dset.shape[0]))
