# F1 Practice Lap Times

Lap times from Free Practices scraped from FIA site's PDFs.

[Example here](https://www.fia.com/sites/default/files/2020_08_ita_f1_p1_timing_firstpracticesessionlaptimes_v01.pdf).

Doesn't detect deleted laptimes (~~strikethrough~~ in pdf). Retains features of original tables - e.g. lap 1 is always a time of day rather than laptime.

Used rvest, tabulizer and tidyverse packages.
