# sddb (Simple Data Data Base)

## Introduction

The idea is to be able to export/import data easily, without
having to explicitly recurring to use complicated file names
as is common practice is science.

From Fortran the interface should be something like this:

```fortran
CALL sddbNew()
CALL sddbConfigure('temperature', 77)
CALL sddbConfigure('ees', .FALSE.)
CALL sddbSave('energy distribution', mydata)
```

Now, from Python I'd like to do:

```python
import sddb

data = sddb.get('energy distribution', temperature=77, ees=False)
```

And go on with postprocessing. All the data is suposed to live in the current folder, but it can be changed. A file should have all the metadata information required, like *metadata.sddb* or similar. Then, the contents could be stored in several different files, in a folder *data.sddb*. If metadata dissapears, I don't want the files to be useless, for for each file also all the metadata should be expressed in the top, inside some comments.

The contents of *metadata.sddb* should also be accesible to read and understand, so something similar to this, if yaml:

```yaml
datasets:
 - path: '76d80224611fc919a5d54f0ff9fba446'
   date: 2018-06-02 13:30:00
   author: Guillermo
   set: energy distribution
   temperature: 77
   ees: false
   type: ascii
   dimensions: 2 850
 - path: <more like the previous one>
```
The path is a md5sum of the contents, or other type of hash just to make them easy to differentiate. In general, the path should not be defined by the metadata, but I would like to export the data to path-like database, to the previous data would live instead in:

```
temperature-77/ees-false/2018-06-03-13:30:00-energy_distribution.csv
```      

Depending on the ammount of data, I could decide how many folders to create and in which order.

Or maybe a plain structure with all the information in the file name:
```
temperature-77-ees-false-2018-06-03-13:30:00-energy_distribution.csv
```