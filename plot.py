# pylint: disable=no-member
import h5py
import matplotlib

matplotlib.use('Qt5Agg')

import matplotlib.pyplot as plt

plt.style.use('/home/indalecio/Desktop/TU/guille.mplstyle')

matplotlib.rcParams['figure.figsize'] = (3.5, 2.7)
matplotlib.rcParams['savefig.dpi'] = 300

def label_from_dataset(dset):
    fullname = dset.attrs.get('fullname')
    if fullname:
        label = fullname.decode('ascii')
    else:
        label = dset.name.split('/')[-1]

    units = dset.attrs.get('units')

    if units:
        label = label + ' (' + units.decode('ascii') + ')'
    return label

filename = 'data.h5'
f = h5py.File(filename)

group = f['/last']
li = f.get('/last', getlink=True)
groupname = li.path

current = group['current']
voltage = group['voltage']

filename = "{0}/{1}".format(filename,groupname)
plt.annotate(filename, (0.02, 0.98),
    xycoords='figure fraction',
    verticalalignment='top',
    horizontalalignment='left',
    family='monospace',
)

if 'creation_time' in group.attrs:
    time = group.attrs['creation_time'].decode('ascii')
    plt.annotate(time, (0.98, 0.98),
        xycoords='figure fraction',
        verticalalignment='top',
        horizontalalignment='right',
        family='monospace',
    )

title = "$\\it{0}$\n".format(filename)

for at in group.attrs:
    if at == 'creation_time':
        continue
    title = title + at + " = " + str(group.attrs[at]) + "\n"

plt.gca().annotate(title.strip(),
            xy=(0.05, 0.95), xycoords='axes fraction',
            horizontalalignment='left', verticalalignment='top',
            bbox=dict(boxstyle="square,pad=.5", fc=(.9, .9, .9), lw=1),
)

plt.plot(voltage.value, current.value,
    color='black',
    linestyle='solid',
    linewidth=1
)

plt.xlabel(label_from_dataset(voltage))
plt.ylabel(label_from_dataset(current))
plt.gcf().tight_layout()
plt.subplots_adjust(top=0.9)

plt.show()
