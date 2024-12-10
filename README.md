# AdvancedR3 project:

This project is about creating automated and reproducible analysis
pipelines as a part of course organized by Danish Diabetes and Endocrine
Academy.

# Brief description of folder and file contents

The following folders contain:

-   `data/`: preprocessed datasets ready for statitical analysis;
-   `doc/`: manuscripts and files with analysis decription;
-   `R/`: files with R scripts and codes for anlysis pipelines.

# Installing project R package dependencies

If dependencies have been managed by using
`usethis::use_package("packagename")` through the `DESCRIPTION` file,
installing dependencies is as easy as opening the `AdvancedR3.Rproj`
file and running this command in the console:

```         
# install.packages("remotes")
remotes::install_deps()
```

You'll need to have remotes installed for this to work.

# Resource

For more information on this folder and file workflow and setup, check
out the [prodigenr](https://rostools.github.io/prodigenr) online
documentation.
