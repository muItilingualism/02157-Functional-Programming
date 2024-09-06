# Running .fsx files

Either run the `run.sh` file with your `.fsx` file as argument.

```bash 
./run.sh "script.fsx"
```

Or, manually enter interactive environemnt:

```bash
dotnet fsi
``` 

Load the .fsx

```bash
#load "script.fsx"
open Script
```

---

Now you can utilize the functions within.

```bash
dostuff 5
```
