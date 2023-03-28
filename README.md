# Scraping German News Websites

Scripts to scrape large German news websites, used for my project on automating news media bias detection. To get the code, simply run 

```
git clone https://github.com/kssrr/german-media-scrape
```

If you are unfamiliar with git, you can copy-paste & run the `setup.R`-script, which will also install the dependencies for you. 

## Getting the data

[Direct download (compressed .tar.gz)](https://archive.org/download/german-media-scrape_202302/data.tar.gz)

We assembled a demo-dataset that includes all articles between January 1st 2020 and December 31st 2022 from the media outlets taz, Zeit, Süddeutsche, Spiegel & Welt. The data set includes a little over one million German-language news articles (uncompressed ~3.5 GB) of varying length. Article titles are missing for some sites due to an earlier problem with the scrapes; we plan to add them in later versions. The data is hosted [here](https://archive.org/details/german-media-scrape_202302).

The data set includes broad coverage of various impactful events that could be fruitfully analysed, like the German federal election 2021, COVID-19, the 2022 Soccer World Cup, and of course the Russian invasion of Ukraine in early 2022. 

Theoretically, the scripts could also be used to scrape data going back as far as the newspapers' archives allow; simply change the corresponding code early on in the scripts where the dates (years) to scrape are specified.


## Collaboration

If you decide to use the data or the code, please contact us so we can boast about it here :)

## Example usage

You can do a lot of interesting basic exploratory analysis with this kind of data, for example examine reporting on political parties:

![Reporting on political parties on two German news websites.](https://user-images.githubusercontent.com/121236725/210736182-01f7a3f2-3f72-420c-b03e-8cc252426dba.png)

You could also look at the salience of particular topics:

![Media Attention on Ukraine after the 2022 invasion.](https://user-images.githubusercontent.com/121236725/210806381-59ed1d41-fac2-4b1f-a99d-748e30a428ef.png)

![World Cup 2022](https://user-images.githubusercontent.com/121236725/210814418-e05d6aaf-5976-454f-89af-d5e9998476df.png)

Or investigate pairwise correlation clusters of keywords (click to enlarge; see [here](https://www.tidytextmining.com/ngrams.html#counting-and-correlating-pairs-of-words-with-the-widyr-package) for the methodology):

![Network](https://user-images.githubusercontent.com/121236725/212491454-24f43fbc-788f-4496-878f-c5068d68f89a.png)

![welt_network](https://user-images.githubusercontent.com/121236725/212496053-41494d26-1043-477c-b23a-3d29fe73613a.png)

*Special thanks to the [University of Münster](https://www.uni-muenster.de/de/) for providing us with additional computational resources for this project.*

