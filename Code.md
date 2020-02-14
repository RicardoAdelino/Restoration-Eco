It is recommended that some packeges shoulb be installed directly from the github repository. Specifically the package factoextra.
The following code allows to install the factoextra package using github repository as source.

```
if(!require(devtools)) install.packages("devtools")
devtools::install_url("https://github.com/wilkelab/cowplot/archive/0.6.3.zip")
devtools::install_github("kassambara/factoextra") 
```

#Packages
```
library(factoextra)
library(tidyr)
library(cluster)
library(randomcoloR)
library(ggrepel)
```

