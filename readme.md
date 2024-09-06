# Running .fsx files

Either run the `run.sh` script with your `.fsx` file as argument.

```bash 
./run.sh "script.fsx"
```

---

Or, manually enter interactive environment

```bash
dotnet fsi
``` 

Load the .fsx

```bash
#load "script.fsx"
open Script
```

---

In both cases you can now, utilize the functions within `script.fsx` directly

```bash
dostuff 5
```
