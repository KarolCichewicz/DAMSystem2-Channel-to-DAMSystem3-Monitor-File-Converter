# DAMSystem2-Channel-to-DAMSystem3-Monitor-File-Converter

### Authors
Karol Cichewicz, Jay Hirsh, University of Virginia, Charlottesville, VA

### Short description
This is a Shiny app for converting TriKinetics channel files to monitor files. This converter was developed to provide compatibility with our ShinyR-DAM app for users still using the legacy DAMSystem2 data acquisition software. Out file converter operates in the cloud, and can be accessed via this link: https://karolcichewicz.shinyapps.io/DAM2_to_DAM3_converter/ , or deployed locally using RStudio. For optimization and grant writing purposes we track the usage of our app using google analytics java script, included in this repository. We do not collect any information about the data processed by our program.

### Testing
For testing we provide 32 channel files in this repository. We would like to thank Dr. Herman Wijnen from the University of Southampton, UK, for providing these test files.

### Abstract
We developed a web application ShinyR-DAM for analyzing Drosophila locomotor activity, sleep, and circadian rhythms recorded by the Drosophila Activity Monitor (DAM) system (TriKinetics, Waltham, MA). The DAM system measures locomotor activity as infrared beam break counts of flies walking in glass tubes. It allows long-term recording of behavior, making it particularly suitable for circadian biology studies. Comparing with the existing programs for DAM data analysis, ShinyR-DAM substantially decreases the complexity and time required to analyze the data, producing aesthetically pleasing plots, summary tables, and data files for further statistical analysis. Our program has an intuitive graphical user interface that enables novice users to quickly learn the analyses. It runs in a web browser and requires no installation on a local computer, nor programming skills.

### License
DAMSystem2-Channel-to-DAMSystem3-Monitor-File-Converter source code is provided under the GPLv3 free software license.
