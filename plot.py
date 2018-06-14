import h5py
import matplotlib.pyplot as plt

def label_from_dataset(dset):
    label = '{0} ({1})'.format(dset.attrs['fullname'][0].decode('utf8'), dset.attrs['units'][0].decode('utf8'))
    return label
filename = 'data.h5'
f = h5py.File(filename)

group = f['/last']
li = f.get('/last', getlink=True)
groupname = li.path

current = group['current']
voltage = group['voltage']

title = "Plotting {0}/{1}\n".format(filename,groupname)

for at in group.attrs:
    title = title + at + " = " + str(group.attrs[at]) + '\n'

plt.plot(voltage.value, current.value, 'k-')
plt.title(title.strip())

plt.xlabel(label_from_dataset(voltage))
plt.ylabel(label_from_dataset(current))

plt.show()