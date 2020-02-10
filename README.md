**\*currently under development\***

This package contains functions for:
- Importing flat txt files containing CAGED microdata (available [here](http://pdet.mte.gov.br/microdados-rais-e-caged));
- Performing data cleaning and tidying on such data;
- Writting rds files with the clean, preprocessed dataframes;
- Importing multiple of these rds files at once, in a single dataframe;
- Performing data analysis and visualization of employment data, using tools from the `tidyverse` and the `tmap` package.

As of now, the package focuses on investigating employment level and trends in the State of Sao Paulo and its 43 Government Regions. For this reason, some features are specific to such state (_e.g._, plotting maps). In addition, the package comes with several dataframes containing data from Sao Paulo.

While the package and this ReadMe are not ready, you may want to visit my other repository, [`caged_data_analysis`](https://github.com/andremenegatti/caged_data_analysis), where you can find some scripts that use this package to actually do stuff.
